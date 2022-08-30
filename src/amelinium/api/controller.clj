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
            [amelinium.common.controller        :as    controller]
            [io.randomseed.utils.map            :as           map]
            [io.randomseed.utils                :refer       :all]
            [amelinium.i18n                     :as          i18n]
            [amelinium.api                      :as           api]
            [amelinium.http                     :as          http]
            [amelinium.http.middleware.language :as      language]
            [amelinium.http.middleware.coercion :as      coercion]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Authentication

(p/import-vars [amelinium.common.controller
                check-password lock-remaining-mins
                account-locked? prolongation? prolongation-auth?
                regular-auth? hard-expiry? keywordize-params? kw-form-data])

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

(defn auth-user-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may render a response."
  [req user-email password sess route-data lang]
  (let [req (controller/auth-user-with-password! req user-email password sess route-data)]
    (if (api/response? req)
      req
      (let [lang          (or lang (api/pick-language req))
            smap          (common/session req)
            astatus       (get req :auth/status)
            status        (controller/auth-status-to-status astatus)
            resp-fn       (controller/auth-status-to-resp   astatus)
            translate-sub (common/translator-sub req lang)
            amessage      (translate-sub :auth  astatus)
            message       (translate-sub :error status)
            sess-err?     (= status :error/session)
            substatus     (if sess-err? :session/status  :auth/status)
            submessage    (if sess-err? :session/message :auth/message)
            req           (-> req
                              (assoc :response/body {:status       status
                                                     :message      message
                                                     :status/sub   substatus
                                                     :message/sub  submessage
                                                     :auth/status  astatus
                                                     :auth/message amessage})
                              (language/force lang)
                              (api/body-add-lang lang))]
        (api/render-response resp-fn (if sess-err?
                                       (api/body-add-session-errors req smap translate-sub lang)
                                       (api/body-add-session-id req smap)))))))

(defn authenticate!
  "Logs user in when user e-mail and password are given, or checks if the session is
  valid to serve a current page.

  Takes a request map and obtains database connection, client IP address and
  authentication configuration from it. Also gets a user e-mail and a password from a
  map associated with the `:body-params` key of the `req`. Calls `auth-user-with-password!`
  to get a result or a redirect if authentication was not successful.

  If there is no e-mail nor password given (the value is `nil`, `false` or an empty
  string) then authentication is not performed but instead validity of a session is
  tested. If the session is invalid then redirect to a login page is performed. The
  destination URL is obtained via the route name taken from the `:auth/info` key of
  a route data, or from the `:auth/info` route identifier (as a default).

  If the session is valid then the given request map is returned as is."
  [req]
  (let [body-params (get req :body-params)
        user-email  (some-str (get body-params :login))
        password    (if user-email (some-str (get body-params :password)))
        sess        (common/session req)
        route-data  (http/get-route-data req)]
    (cond
      password           (controller/auth-user-with-password! req user-email password sess route-data)
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

(defn prep-request!
  "Prepares a request before any controller is called."
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
      (let [lang      (common/lang-id    req)
            translate (common/translator req)]
        (-> req
            (assoc :response/body
                   {:status        :error/bad-parameters
                    :message       (translate :error/bad-parameters)
                    :status/sub    :params/errors
                    :params/errors (get req :validators/reasons)})
            (api/body-add-lang lang)
            api/render-bad-params))

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
            lang      (common/lang-id req)
            translate (common/translator req)]
        (log/wrn "Hard-locked account access attempt" for-user)
        (api/oplog req
                   :user-id user-id
                   :op      :access-denied
                   :level   :warning
                   :msg     (str "Permanent lock " for-mail))
        (-> req
            (assoc :response/body
                   {:status       :error/authorization
                    :message      (translate :error/authorization)
                    :status/sub   :auth/status
                    :message/sub  :auth/message
                    :auth/status  :locked
                    :auth/message (translate :auth/locked)})
            (api/body-add-lang lang)
            api/render-unauthorized))

      ;; Session is not valid.

      (and (not @valid-session?) (not (and @auth? @login-data?)))
      (let [req           (cleanup-req req @auth-state)
            expired?      (get sess :expired?)
            user-id       (:user/id      sess)
            email         (:user/email   sess)
            ip-addr       (:remote-ip/str req)
            for-user      (log/for-user user-id email ip-addr)
            for-mail      (log/for-user nil email ip-addr)
            lang          (common/lang-id req)
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
          (when-some [reason (:reason (:error sess))]
            (api/oplog req
                       :user-id (:user/id sess)
                       :op      :session
                       :ok?     false
                       :level   (:error sess)
                       :msg     reason)
            (log/log (:severity (:error sess) :warn) reason)))

        ;; Generate a response describing an invalid session.

        (-> req
            (assoc :response/body {:status      :error/session
                                   :status/sub  :session/status
                                   :message/sub :session/message
                                   :message     (translate-sub :error/session)})
            (api/body-add-lang lang)
            (api/body-add-session-errors sess translate-sub lang)
            api/render-forbidden))

      :----pass

      ;; We have a valid session.
      ;;
      ;; Remove login data from the request if we are not authenticating a user.
      ;; Take care about broken go-to (move to a login page in such case).

      (cleanup-req req [nil @auth?]))))

(defn render!
  [req]
  (if (contains? req :response/fn)
    ((get req :response/fn) req)
    (api/render-ok req)))

(defn not-found!
  [req]
  (api/render-not-found req))

;; Coercion error handler

(defn handle-coercion-error
  "Called when coercion exception is thrown by the handler executed earlier in a
  middleware chain. Takes exception object `e`, response wrapper `respond` and
  `raise` function.

  When a coercion error is detected during request processing, it creates a sequence
  of maps (by calling `amelinium.http.middleware.coercion/explain-errors`) where each
  contains the following keys:

  - `:parameter/id`,
  - `:parameter/src`,
  - `:parameter/path`,
  - `:parametery/type`,
  - `:error/summary`,
  - `:error/description`.

  The sequence is then stored in a map identified with the `:response/body` key of a
  request map, under the key `:error/parameters`. Additionally, the following keys
  are added to the response body:

  - `:lang` (current language),
  - `:status` (always set to `:error/parameters`),
  - `:status/message` (a result of translation of the `:error/parameters` key),
  - `:status/sub` (always set to `:error/parameters`)."
  [e respond raise]
  (let [data  (ex-data e)
        req   (get data :request)
        ctype (get data :type)
        data  (dissoc data :request)]
    (case ctype

      :reitit.coercion/request-coercion
      (respond
       (let [lang          (common/lang-id req)
             translate-sub (common/translator-sub req)]
         (-> req
             (assoc :response/body {:lang             lang
                                    :status           :error/parameters
                                    :status/message   (translate-sub :error/parameters)
                                    :status/sub       :error/parameters
                                    :error/parameters (coercion/explain-errors data translate-sub)})
             api/render-bad-params)))

      :reitit.coercion/response-coercion
      (respond
       (let [data       (dissoc data :response)
             lang       (common/lang-id req)
             translate  (common/translator req)
             error-list (coercion/list-errors data)]
         (log/err "Response coercion error:" (coercion/join-errors error-list))
         (-> req
             (assoc :response/body {:lang           lang
                                    :status         :error/internal
                                    :status/message (translate :error/internal)
                                    :status/sub     :error/parameters})
             api/render-internal-server-error)))

      (raise e))))

;; Handle options method

(defn handle-options
  "Default handler for OPTIONS method."
  [req]
  (render!
   (assoc req :response/headers
          {"Access-Control-Allow-Methods"
           (->> (-> req (get :reitit.core/match) (get :result))
                (filter second) keys (map name)
                (str/join ", ") str/upper-case)})))
