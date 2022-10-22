(ns

    ^{:doc    "amelinium service, common controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.common.controller

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces                :as          p]
            [reitit.ring                        :as       ring]
            [reitit.coercion                    :as   coercion]
            [ring.middleware.keyword-params     :as    ring-kw]
            [ring.util.http-response            :as       resp]
            [ring.util.request                  :as        req]
            [ring.util.codec                    :as      codec]
            [reitit.core                        :as          r]
            [reitit.ring                        :as       ring]
            [selmer.filters                     :as    filters]
            [selmer.parser                      :as       tmpl]
            [lazy-map.core                      :as   lazy-map]
            [tick.core                          :as          t]
            [amelinium.i18n                     :as       i18n]
            [amelinium.i18n                     :refer    [tr]]
            [amelinium.logging                  :as        log]
            [amelinium.model.user               :as       user]
            [amelinium.common                   :as     common]
            [io.randomseed.utils.time           :as       time]
            [io.randomseed.utils.var            :as        var]
            [io.randomseed.utils.map            :as        map]
            [io.randomseed.utils                :refer    :all]
            [amelinium.auth                     :as       auth]
            [amelinium.http                     :as       http]
            [amelinium.http.middleware.language :as   language]))

(def ^:const keywordize-params? false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations

(defn kw-form-data
  "Changes form data keys into keywords by calling
  `ring.middleware.keyword-params/keyword-params-request` on a specially crafted
  map."
  ([form-data]
   (kw-form-data form-data {}))
  ([form-data opts]
   (if (and keywordize-params? form-data)
     (-> (array-map :params form-data)
         (ring-kw/keyword-params-request opts)
         :params)
     form-data)))

(defn check-password
  [user password auth-config]
  (if (and user password)
    (auth/check-password-json password
                              (get user :shared)
                              (get user :intrinsic)
                              auth-config)))

(defn account-locked?
  "Returns true if an account associated with the session is hard-locked.
  Uses cached property."
  ([req session]
   (if-some [db (common/auth-db req)]
     (account-locked? req session db)))
  ([req session db]
   (some? (some->> session :user/id (user/prop-get-locked db)))))

(defn lock-remaining-mins
  "Returns the time of the remaining minutes of a soft account lock when the visited
  page ID is `:login/account-soft-locked`. Otherwise it returns nil. Uses cached user
  properties."
  ([req auth-db smap time-fn]
   (lock-remaining-mins req auth-db smap time-fn "login"))
  ([req auth-db smap time-fn id-form-field]
   (if auth-db
     (if-some [user (or (user/props-by-session auth-db smap)
                          (user/props-by-email auth-db (get (get req :form-params) id-form-field)))]
       (if-some [auth-config (common/auth-config req (get user :account-type))]
         (if-some [mins (time/minutes (common/soft-lock-remains user auth-config (time-fn)))]
           (if (zero? mins) 1 mins)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

(defn prolongation?
  "Returns true if session is expired (but not hard expired) and a user is not logged
  in and there is no login data present and we are not authenticating user. In other
  words: this returns true when we are good with redirecting user to a session
  prolongation page."
  [sess [login? auth?] login-data?]
  (or (and (get sess :expired?) (not (get sess :hard-expired?))
           (not login?)
           (or (not auth?) (not login-data?)))
      false))

(defn prolongation-auth?
  "Returns true if user is being authenticated to prolongate the soft-expired session."
  [sess login? auth? login-data?]
  (or (and login-data? auth? (not login?) (:expired? sess)) false))

(defn regular-auth?
  "Returns true if user is being authenticated."
  [sess login? auth? login-data?]
  (or (and auth? login-data? (not login?) (not sess)) false))

(defn hard-expiry?
  "Returns true if the session is hard-expired and we are not on the hard-expired login
  page. Uses the given, previously collected session data, does not connect to a
  database."
  [req sess route-data]
  (or (and (get sess :hard-expired?)
           (not (common/on-page? req (get route-data :auth/session-expired :login/session-expired))))
      false))

(defn auth-user-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may emit a redirect or set the `:response/status`. The last
  `auth-only-mode` argument, when set to `true` (default is `false` when not given)
  causes session creation and prolongation to be skipped if the authentication is
  successful."
  ([req user-email password sess route-data]
   (auth-user-with-password! req user-email password sess route-data false))
  ([req user-email password sess route-data auth-only-mode]
   (let [ipaddr       (get req :remote-ip)
         ipplain      (get req :remote-ip/str)
         auth-db      (common/auth-db req)
         user         (user/get-login-data auth-db user-email)
         user-id      (get user :id)
         pwd-suites   (select-keys user [:intrinsic :shared])
         auth-config  (common/auth-config req (get user :account-type))
         auth-db      (get auth-config :db)
         for-user     (log/for-user user-id user-email ipplain)
         for-mail     (log/for-user nil user-email ipplain)
         opname       (if auth-only-mode :auth :login)
         oplog-fn     (common/oplog-logger-populated req route-data)
         oplog        (fn [ok? l m a] (oplog-fn :ok? ok? :user-id user-id :opname opname
                                                :level l :msg (str m " " a)))
         hard-locked? (fn [] (common/hard-locked? user))
         soft-locked? (fn [] (common/soft-locked? user auth-config (t/now)))
         invalid-pwd? (fn [] (not (check-password user password auth-config)))]

     (cond

       (hard-locked?) (do (log/wrn "Account locked permanently" for-user)
                          (oplog false :info "Permanent lock" for-mail)
                          (assoc req :auth/ok? false :response/status :auth/locked))

       (soft-locked?) (do (log/msg "Account locked temporarily" for-user)
                          (oplog false :info "Temporary lock" for-mail)
                          (assoc req :auth/ok? false :response/status :auth/soft-locked))

       (invalid-pwd?) (do (log/wrn "Incorrect password or user not found" for-user)
                          (when user-id
                            (oplog false :warn "Bad password" for-mail)
                            (user/update-login-failed auth-db user-id ipaddr
                                                      (get auth-config :locking/max-attempts)
                                                      (get auth-config :locking/fail-expires)))
                          (assoc req :auth/ok? false :response/status :auth/bad-password))

       auth-only-mode (do (log/msg "Authentication successful" for-user)
                          (oplog true :info "Authentication OK" for-mail)
                          (user/update-login-ok auth-db user-id ipaddr)
                          (assoc req :auth/ok? true :response/status :auth/ok))

       :authenticate! (do (log/msg "Login successful" for-user)
                          (oplog true :info "Login OK" for-mail)
                          (user/update-login-ok auth-db user-id ipaddr)
                          (let [goto-uri  (if (get sess :expired?) (get req :goto-uri))
                                sess-opts (get req :session/config)
                                sess      (if goto-uri
                                            (user/prolong-session sess-opts sess ipaddr)
                                            (user/create-session  sess-opts user-id user-email ipaddr))]

                            (if-not (get sess :valid?)

                              (let [e (get sess :error)
                                    r (:reason   e)
                                    s (:severity e)]
                                (when r
                                  (log/log (or s :warn) r)
                                  (oplog-fn :level s :user-id user-id :op :session :ok? false :msg r))
                                (assoc req :auth/ok? false :response/status :auth/session-error))

                              (if goto-uri
                                (resp/temporary-redirect goto-uri)
                                (-> req
                                    (assoc (or (get sess-opts :session-key) :session) sess
                                           :auth/ok? true :response/status :auth/ok)
                                    ((get (get req :roles/config) :handler identity)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special actions (controller handlers)

(defn authenticate!
  "Logs user in when user e-mail and password are given, or checks if the session is
  valid to serve a current page.

  Takes a request map and obtains database connection, client IP address and
  authentication configuration from it. Also `login` and `password` strings. Calls
  `auth-user-with-password!` to get a result or a redirect if authentication was not
  successful.

  If there is no e-mail or password given (the value is `nil`, `false` or an empty
  string) then authentication is not performed but instead validity of a session is
  tested. If the session is invalid redirect to a login page is performed. The
  destination URL is obtained via the route name taken from the `:auth/info` key of
  a route data, or from `:login` route identifier as default. If the session is valid
  then the given request map is returned as is."
  [req user-email user-password]
  (let [user-email     (some-str user-email)
        user-password  (if user-email (some-str user-password))
        sess           (common/session req)
        valid-session? (get sess :valid?)
        route-data     (http/get-route-data req)]

    (if user-password

      ;; Authenticate using email and password.
      (auth-user-with-password! req user-email user-password sess route-data)

      ;; Check session.
      (if-not valid-session?

        ;; Invalid session causes a redirect to a login page.
        (common/move-to req (get route-data :auth/info :auth/info))

        ;; Valid session causes page to be served.
        req))))

;; Coercion error handler

(defn handle-coercion-error
  [e respond raise]
  (let [data  (ex-data e)
        ctype (get data :type)]
    (if-let [status (case ctype
                      ::coercion/request-coercion  422
                      ::coercion/response-coercion 500
                      nil)]
      (respond
       {:status status
        :body   (coercion/encode-error data)})
      (raise e))))
