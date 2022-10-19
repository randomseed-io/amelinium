(ns

    ^{:doc    "amelinium service, common API controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.api.controller

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces                :as             p]
            [tick.core                          :as             t]
            [clojure.string                     :as           str]
            [amelinium.logging                  :as           log]
            [amelinium.common                   :as        common]
            [amelinium.common.controller        :as         super]
            [io.randomseed.utils.map            :as           map]
            [io.randomseed.utils                :refer       :all]
            [amelinium.i18n                     :as          i18n]
            [amelinium.api                      :as           api]
            [amelinium.http                     :as          http]
            [amelinium.http.middleware.language :as      language]
            [amelinium.http.middleware.coercion :as      coercion]))

(p/import-vars [amelinium.common.controller
                check-password lock-remaining-mins
                account-locked? prolongation? prolongation-auth?
                regular-auth? hard-expiry? keywordize-params? kw-form-data])

;; Helpers

(defn remove-login-data
  "Removes login data from the form params and body part of a request map."
  [req]
  (-> req
      (map/update-existing :form-params dissoc "password")
      (map/update-existing :params      dissoc :password)
      (update              :body-params dissoc :password)))

(defn cleanup-req
  [req auth-state]
  (if (nth auth-state 1 false) req (remove-login-data req)))

(defn login-data?
  "Returns true if :body map of a request contains login data."
  [req]
  (if-some [bparams (get req :body-params)]
    (and (contains? bparams :password)
         (contains? bparams :login))))

(defn add-session-status
  [req smap translate-sub]
  (if (= (get req :response/status) :error/session)
    (api/add-missing-sub-status req (api/session-status (or smap (common/session req)))
                                :session-status :response/body translate-sub)
    (api/body-add-session-id req)))

(defn auth-user-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may render a response."
  [req user-email password sess route-data lang]
  (let [req (super/auth-user-with-password! req user-email password sess route-data)]
    (if (api/response? req)
      req
      (let [lang (or lang (common/pick-language req))]
        (-> req
            (language/force lang)
            (add-session-status sess (common/translator-sub req lang)))))))

;; Controllers

(defn authenticate!
  "Logs user in when user e-mail and password are given, or checks if the session is
  valid to serve a current page.

  Takes a request map and obtains database connection, client IP address and
  authentication configuration from it. Also gets a user e-mail and a password from a
  map associated with the `:body-params` key of the `req`. Calls
  `auth-user-with-password!` to get a result or a redirect if authentication was not
  successful.

  If there is no e-mail nor password given (the value is `nil`, `false` or an empty
  string) then authentication is not performed but instead the validity of a session
  is tested. If the session is invalid then a redirect to a login page is
  performed. Its destination URL is obtained via a route name taken from the
  `:auth/info` key of a route data, or from the `:auth/info` route identifier (as a
  default fallback).

  If the session is valid then the given request map is returned as is."
  [req]
  (let [body-params (get req :body-params)
        user-email  (some-str (get body-params :login))
        password    (if user-email (some-str (get body-params :password)))
        sess        (common/session req)
        route-data  (http/get-route-data req)]
    (cond
      password           (auth-user-with-password! req user-email password sess route-data nil)
      (get sess :valid?) req
      :invalid!          (api/move-to req (get route-data :auth/info :auth/info)))))

(defn info!
  "Returns login information."
  [req]
  (let [sess-opts  (get req :session/config)
        sess-key   (or (get sess-opts :session-key) :session)
        sess       (get req sess-key)
        prolonged? (delay (some? (and (get sess :expired?) (get req :goto-uri))))]
    (-> req
        (assoc sess-key
               (delay (if @prolonged?
                        (assoc sess :id (or (get sess :id) (get sess :err/id)) :prolonged? true)
                        (assoc sess :prolonged? false))))
        (assoc-in [:response/body :lock-remains]
                  (lock-remaining-mins req
                                       (api/auth-db req)
                                       (if @prolonged? sess)
                                       t/now)))))
;; Request preparation handler

(defn prep-request!
  "Prepares a request before any controller is called. Checks if parameters are
  valid (if validators are configured). If there is a session present, checks for its
  validity and tests if an account is locked."
  [req]
  (let [sess           (common/session req)
        auth-state     (delay (common/login-auth-state req :login-page? :auth-page?))
        auth?          (delay (nth @auth-state 1 false))
        login-data?    (delay (login-data? req))
        auth-db        (delay (api/auth-db req))
        valid-session? (delay (get sess :valid?))]

    (cond

      ;; Request is invalid.

      (not (get req :validators/params-valid?))
      (api/render-error req :parameters/error)

      ;; There is no session. Short-circuit.

      (not (and sess (or (get sess :id) (get sess :err/id))))
      (-> req (cleanup-req @auth-state))

      ;; Account is manually hard-locked.

      (account-locked? req sess @auth-db)
      (let [user-id   (:user/id      sess)
            email     (:user/email   sess)
            ip-addr   (:remote-ip/str req)
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

      (and (not @valid-session?) (not (and @auth? @login-data?)))
      (let [req           (cleanup-req req @auth-state)
            expired?      (get sess :expired?)
            user-id       (get sess :user/id)
            email         (get sess :user/email)
            reason        (get (get sess :error) :reason)
            ip-addr       (:remote-ip/str req)
            for-user      (log/for-user user-id email ip-addr)
            for-mail      (log/for-user nil email ip-addr)
            translate-sub (common/translator-sub req)]

        ;; Log the event.

        (if expired?
          ;; Session expired.
          (do (log/msg "Session expired" for-user)
              (api/oplog req
                         :user-id (:user/id sess)
                         :op      :session
                         :ok?     false
                         :msg     (str "Expired " for-mail)))
          ;; Session invalid in another way.
          (when (some? reason)
            (api/oplog req
                       :user-id (:user/id sess)
                       :op      :session
                       :ok?     false
                       :level   (:error sess)
                       :msg     reason)
            (log/log (:severity (:error sess) :warn) reason)))

        ;; Generate a response describing an invalid session.

        (-> req
            (api/add-missing-sub-status reason :session-error :response/body translate-sub)
            (api/render-error :auth/session-error)))

      :----pass

      ;; We have a valid session.
      ;;
      ;; Remove login data from the request if we are not authenticating a user.
      ;; Take care about broken go-to (move to a login page in such case).

      (cleanup-req req [nil @auth?]))))

;; Response rendering handlers

(defn render!
  "Renders a response by calling `render-ok` on a `req` request map. If
  `:response/status` key is present in `req` and is not `nil`, it will call
  `render-status` instead with `req` and a value associated with this key (which
  should be a keyword). If `:response/fn` key is present in `req` and it is not
  `nil`, it should be a function which will be called with `req` argument."
  [req]
  (if-some [st (get req :response/status)]
    (api/render-status req st)
    (if-some [f (get req :response/fn)]
      (f req)
      (api/render-ok req))))

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
  of maps (by calling `amelinium.http.middleware.coercion/explain-errors`) where each
  contains the following keys:

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
      (let [tr-sub (common/translator-sub req)
            errors (coercion/explain-errors data tr-sub)]
        (-> req
            (update :response/body assoc :parameters/errors errors)
            (api/render-bad-params)
            (respond)))

      :reitit.coercion/response-coercion
      (let [data       (dissoc data :response)
            error-list (coercion/list-errors data)]
        (log/err "Response coercion error:" (coercion/join-errors-with-values error-list))
        (respond (api/render-error req :output/error)))

      (raise e))))

;; Handler for OPTIONS method

(defn handle-options
  "Default handler for the OPTIONS method. Adds `Access-Control-Allow-Methods` header
  with supported methods listed (separated by commas and space characters)."
  [req]
  (render!
   (assoc req :response/headers
          {"Access-Control-Allow-Methods"
           (->> (-> req (get :reitit.core/match) (get :result))
                (filter second) keys (map name)
                (str/join ", ") str/upper-case)})))
