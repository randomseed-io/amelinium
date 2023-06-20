(ns

    ^{:doc    "amelinium service, common API controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.api.controller

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces                :as               p]
            [tick.core                          :as               t]
            [clojure.string                     :as             str]
            [amelinium.logging                  :as             log]
            [amelinium.common                   :as          common]
            [amelinium.common.controller        :as           super]
            [io.randomseed.utils.map            :as             map]
            [io.randomseed.utils.map            :refer     [qassoc]]
            [io.randomseed.utils                :refer         :all]
            [amelinium.i18n                     :as            i18n]
            [amelinium.api                      :as             api]
            [amelinium.auth                     :as            auth]
            [amelinium.http                     :as            http]
            [amelinium.http.middleware.session  :as         session]
            [amelinium.http.middleware.coercion :as        coercion]
            [amelinium.types.session            :refer         :all]
            [amelinium]
            [puget.printer                      :refer     [cprint]])

  (:import (amelinium Session)))

;; Helpers

(defn remove-login-data
  "Removes login data from the form params and body part of a request map."
  [req]
  (-> req
      (common/remove-form-params :password :repeated-password :user/password :user/repeated-password :user/new-password :new-password)
      (common/remove-params :body-params :body false :password :user/password :repeated-password :user/repeated-password :user/new-password :new-password)))

(defn cleanup-req
  "Takes a request map `req` and an authentication state, 2-element vector
  `auth-state`. Removes login information from form and body data if its second
  element does not have a truthy value (meaning that we are NOT on an authentication
  page which is allowed to process passwords)."
  [req auth-state]
  (if (nth auth-state 1 false) req (remove-login-data req)))

(defn login-data?
  "Returns `true` if `:body` map of a request contains login data."
  [req]
  (if-some [bparams (get req :body-params)]
    (and (or (contains? bparams :user/password) (contains? bparams :password))
         (or (contains? bparams :user/login)    (contains? bparams :login)))))

;; Request preparation handler

(defn prep-request!
  "Prepares a request before any controller is called. Checks if parameters are
  valid (if validators are configured). If there is a session present, checks for its
  validity and tests if an account is locked."
  ([req]
   (prep-request! req nil))
  ([req session-key]
   (let [^Session sess (session/of req session-key)
         auth-state    (delay (common/login-auth-state req :login-page? :auth-page?))
         auth?         (delay (nth @auth-state 1 false))
         login-data?   (delay (login-data? req))
         auth-db       (delay (auth/db req))
         session-error (session/error sess)
         authorized?   (get req :user/authorized?)]

     (cond

       ;; Authorization failed.

       (not (or session-error authorized?))
       (-> req (cleanup-req @auth-state) (api/render-error :auth/access-denied))

       ;; There is no session. Short-circuit.

       (session/empty? sess)
       (-> req (cleanup-req @auth-state))

       ;; Account is manually hard-locked.

       (super/account-locked? req sess @auth-db)
       (let [user-id   (session/id         sess)
             email     (session/user-email sess)
             ip-addr   (get req :remote-ip/str)
             for-user  (log/for-user user-id email ip-addr)
             for-mail  (log/for-user nil email ip-addr)
             translate (common/translator req)]
         (log/wrn "Hard-locked account access attempt" for-user)
         (api/oplog req
                    :user-id user-id
                    :op      :access-denied
                    :level   :warning
                    :msg     (str "Permanent lock " for-mail))
         (api/render-error req :auth/locked))

       ;; Session is not valid.

       (and (not (session/valid? sess)) (not (and @auth? @login-data?)))
       (let [req           (cleanup-req req @auth-state)
             expired?      (session/expired?   sess)
             user-id       (session/user-id    sess)
             email         (session/user-email sess)
             error-id      (get session-error :id)
             error-cause   (get session-error :cause)
             ip-addr       (:remote-ip/str req)
             for-user      (log/for-user user-id email ip-addr)
             for-mail      (log/for-user nil email ip-addr)
             translate-sub (common/translator-sub req)]

         ;; Log the event.

         (if expired?
           ;; Session expired.
           (do (log/msg "Session expired" for-user)
               (api/oplog req
                          :user-id user-id
                          :op      :session
                          :ok?     false
                          :msg     (str "Expired " for-mail)))
           ;; Session invalid in another way.
           (when (some? error-id)
             (api/oplog req
                        :user-id user-id
                        :op      :session
                        :ok?     false
                        :level   (:severity session-error)
                        :msg     error-cause)
             (log/log (:severity session-error :warn) error-cause)))

         ;; Generate a response describing an invalid session.
         (-> req
             (api/add-missing-sub-status error-id :session-status :response/body translate-sub)
             (api/render-error :auth/session-error)))

       ;; Authorization failed but session error was not handled for some strange reason.

       (not authorized?)
       (-> req (cleanup-req @auth-state) (api/render-error :auth/access-denied))

       :----pass

       ;; We have a valid session and authorization.
       ;;
       ;; Remove login data from the request if we are not authenticating a user.
       ;; Take care about broken go-to (move to a login page in such case).

       (cleanup-req req [nil @auth?])))))

;; Response rendering handlers

(defn render!
  "Renders a response by calling `render-ok` on a `req` request map. If
  `:app/status` key is present in `req` and is not `nil`, it will call
  `render-status` instead with `req` and a value associated with this key (which
  should be a keyword). If `:response/fn` key is present in `req` and it is not
  `nil`, it should be a function which will be called with `req` argument."
  ([req]
   (if-some [st (get req :app/status)]
     (api/render-status req st)
     (if-some [f (get req :response/fn)]
       (f req)
       (api/render-ok req))))
  ([req status-or-fn]
   (if (ident? status-or-fn)
     (api/render-status req status-or-fn)
     (if (fn? status-or-fn)
       (status-or-fn req)
       (api/render-ok req)))))

(defn not-found!
  "Calls `render-not-found` on `req`."
  [req]
  (api/render-not-found req))

;; Coercion error handler

(defn handle-coercion-error
  "Called when coercion exception is thrown by the handler executed earlier in a
  middleware chain. Takes exception object `e`, response wrapper `respond` and
  `raise` function.

  When a coercion error is detected during **request processing**, it creates a sequence
  of maps (by calling `amelinium.http.middleware.coercion/explain-errors-simple`) where
  each contains the following keys:

  - `:parameter/id`,
  - `:parameter/src`,
  - `:parameter/path`,
  - `:parametery/type`,
  - `:error/summary`,
  - `:error/description`.

  The sequence is then stored in a map identified with the `:response/body` key of a
  request map, under the key `:parameters/errors`. Additionally, the following keys
  are added to the response body:

  - `:lang` (current language),
  - `:status` (always set to `:error/bad-parameters`),
  - `:status/title` (a result of translation of the `:error/bad-parameters` key),
  - `:status/description` (a result of translation of the `:error/bad-parameters.full` key).

  When a coercion error is detected during **response processing**, it creates a 500 status
  response with the following body:

  - `:lang` (current language),
  - `:status` (always set to `:server-error/internal`),
  - `:status/title` (a result of translation of the `:server-error/internal` key),
  - `:status/description` (a result of translation of the `:server-error/internal.full` key),
  - `:sub-status` (always set to `:output/error`),
  - `:sub-status/title` (a result of translation of the `:output/error` key),
  - `:sub-status/description` (a result of translation of the `:output/error.full` key)."
  [e respond raise]
  (let [data  (ex-data e)
        req   (get data :request)
        ctype (get data :type)
        data  (dissoc data :request)]
    (case ctype

      :reitit.coercion/request-coercion
      (let [tr-sub (i18n/no-default (common/translator-sub req))
            errors (coercion/explain-errors-simple data tr-sub)]
        (-> (api/assoc-body req :parameters/errors errors)
            (api/render-bad-params)
            (respond)))

      :reitit.coercion/response-coercion
      (let [data       (dissoc data :response)
            error-list (coercion/list-errors-simple data)]
        (log/err "Response coercion error:" (coercion/join-errors-with-values error-list))
        (respond (api/render-error req :output/error)))

      (raise e))))

;; Handler for OPTIONS method

(defn handle-options
  "Default handler for the OPTIONS method. Adds `Access-Control-Allow-Methods` header
  with supported methods listed (separated by commas and space characters)."
  [req]
  (api/render-ok
   (api/add-header req :Access-Control-Allow-Methods
                   (->> (-> req (get :reitit.core/match) (get :result))
                        (filter second) keys (map name)
                        (str/join ", ") str/upper-case))))
