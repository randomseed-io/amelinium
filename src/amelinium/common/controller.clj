(ns

    ^{:doc    "amelinium service, common controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.common.controller

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces                :as            p]
            [reitit.coercion                    :as     coercion]
            [ring.middleware.keyword-params     :as      ring-kw]
            [amelinium.http.response            :as         resp]
            [tick.core                          :as            t]
            [amelinium.logging                  :as          log]
            [amelinium.identity                 :as     identity]
            [amelinium.model.user               :as         user]
            [amelinium.model.confirmation       :as confirmation]
            [amelinium.common                   :as       common]
            [io.randomseed.utils.time           :as         time]
            [io.randomseed.utils.map            :refer  [qassoc]]
            [io.randomseed.utils                :refer      :all]
            [amelinium.auth                     :as         auth]
            [amelinium.http                     :as         http]
            [amelinium.http.middleware.session  :as      session]
            [amelinium.types.session            :refer      :all]
            [puget.printer                      :refer  [cprint]])

  (:import (amelinium Session)))

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
  "Check password using authentication configuration."
  [user password auth-config]
  (if (and user password)
    (auth/check-password-json password
                              (get user :shared)
                              (get user :intrinsic)
                              auth-config)))

(defn account-locked?
  "Returns true if an account associated with the session is hard-locked.
  Uses cached property."
  (^Boolean [req session]
   (if-some [db (auth/db req)]
     (account-locked? req session db)))
  (^Boolean [req session db]
   (some? (some->> session :user/id (user/prop-get-locked db)))))

(defn lock-remaining-mins
  "Returns the time of the remaining minutes of a soft account lock when the visited
  page ID is `:login/account-soft-locked`. Otherwise it returns `nil`. Uses cached
  user properties."
  ([req auth-db smap time-fn]
   (lock-remaining-mins req auth-db smap time-fn :user/login))
  ([req auth-db smap time-fn id-field]
   (if auth-db
     (let [user (and smap (user/props-of :session auth-db smap))
           user (or user (user/props-of :email auth-db (get-in req [:parameters :form id-field])))]
       (if (some? user)
         (if-some [auth-config (auth/config req (get user :account-type))]
           (if-some [mins (time/minutes (common/soft-lock-remains user auth-config (time-fn)))]
             (if (zero? mins) 1 mins))))))))

(defn prolongation?
  "Returns `true` if the given session `sess` is expired (but not hard expired),
  current user is not logged in, there is no login data present, and we are not
  authenticating any user. In other words: returns `true` when we are good with
  redirecting a user to a session prolongation page (a form of login page)."
  ^Boolean [^Session sess [login? auth?] login-data?]
  (or (and (not login?)
           (or (not auth?) (not login-data?))
           (session/soft-expired? sess))
      false))

(defn prolongation-auth?
  "Returns `true` if user visited the prolongation page after submitting credentials to
  the authentication page is being authenticated to prolongate the soft-expired
  session."
  ^Boolean [^Session sess login? auth? login-data?]
  (or (and login-data?
           auth?
           (not login?)
           (session/expired? sess))
      false))

(defn regular-auth?
  "Returns `true` if user is being authenticated."
  ^Boolean [^Session sess login? auth? login-data?]
  (or (and auth?
           login-data?
           (not login?)
           (or (not sess) (session/empty? sess)))
      false))

(defn hard-expiry?
  "Returns `true` if the session is hard-expired and we are not on the hard-expired
  login page. Uses the given, previously collected session data. Does not connect to
  a database."
  ^Boolean [req ^Session sess route-data]
  (or (and (session/hard-expired? sess)
           (not (common/on-page?
                 req
                 (or (get-in route-data [:auth-error/destinations :auth/session-expired] :login/session-expired)))))
      false))

(defn get-goto-uri
  "Obtains go-to URI from `req` if `am/goto` form parameter is present and session is
  soft-expired. Used to get the destination URI from a session variable when user is
  authenticated to be redirected into a page where session expiration has been
  encountered a moment ago."
  [req ^Session sess]
  (if (and (session/soft-expired? sess)
           (contains? (get req :form-params) "am/goto"))
    (if-some [g (session/get-var sess :goto)]
      (if (session/get-variable-failed? g)
        false
        (:uri g)))))

(defn get-goto
  "Gets go-to map from a session variable even if the session is not valid."
  [^Session smap]
  (session/get-var smap :goto))

(defn get-goto-for-valid
  "Gets go-to map from session variable if the session is valid (and not expired)."
  [^Session smap]
  (if (and smap (session/valid? smap))
    (get-goto smap)))

(defn hx-prolong?
  "Returns `true` if authentication is about prolonging session with HTMX
  request.

  Requires session `sess` to be soft-expired and `HX-Target` request header taken
  from `req` (under `:headers` key) to indicate the same target as configured
  `:auth/prolongate` target value for `:status/targets` map of a route
  data (given as `route-data`)."
  [req route-data sess]
  (if (and sess (session/soft-expired? sess))
    (if-some [hx-target (get (get req :headers) "hx-target")]
      (if-some [t (get (get route-data :status/targets) :auth/prolongate)]
        (or (= t (strb "#" hx-target)) (= t hx-target))
        false)
      false)
    false))

(defn hx-prolong?
  "Returns `true` if authentication is about prolonging session with HTMX
  request.

  Requires session `sess` to be soft-expired and `HX-Target` request header taken
  from `req` (under `:headers` key) to indicate the same target as configured
  `:auth/prolongate` target value for `:status/targets` map of a route
  data (given as `route-data`)."
  [req route-data sess]
  (and sess (session/soft-expired? sess)
       (common/use-hx? req route-data :error/use-htmx?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

(defn verify-request-id-update
  "Default confirmation request ID field updater for asynchronous identity
  confirmation."
  [db id-type id code token response]
  (if-some [headers (:headers response)]
    (if-some [req-id (if (map? headers)
                       (or (get headers "twilio-request-id")
                           (get headers "x-message-id")))]
      (confirmation/update-request-id db id code token req-id))))

(defn verify-process-error
  "Default error processor for asynchronous e-mail or SMS sending."
  [db id-type id code token exception]
  (cprint exception))

(defn invalidate-user-sessions!
  "Invalidates user sessions if `id-type` is an e-mail."
  ([req route-data id-type id user-id]
   (invalidate-user-sessions! req route-data id-type id user-id nil))
  ([req route-data id-type id user-id session-key]
   (if (or (identical? :email id-type) (identical? :user/email id-type))
     (let [route-data  (or route-data (http/get-route-data req))
           session-key (or session-key (get route-data :session-key))]
       (session/delete-all! req session-key user-id)))))

(defn auth-user-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may emit a redirect (if a go-to was detected) or set the
  `:app/status` in the returned request map (but not in the response map!).

  In case of successful authentication it will set `:auth/ok` to `true` and
  `:response-status` to `:auth/ok`.

  In case of session prolongation and successful authentication it will set
  `:auth/ok?` to `true` and `:app/status` to `:auth/prolonged-ok`.

  The last, `auth-only-mode` argument, when set to `true` (default is `false` when
  not given) causes session creation and prolongation to be skipped if the
  authentication is successful."
  ([req user-email password]
   (auth-user-with-password! req user-email password nil nil false nil))
  ([req user-email password sess]
   (auth-user-with-password! req user-email password sess nil false nil))
  ([req user-email password sess route-data]
   (auth-user-with-password! req user-email password sess route-data false nil))
  ([req user-email password sess route-data auth-only-mode]
   (auth-user-with-password! req user-email password sess route-data auth-only-mode nil))
  ([req user-email password sess route-data auth-only-mode session-key]
   (let [route-data    (or route-data (http/get-route-data req))
         ipaddr        (get req :remote-ip)
         ipplain       (get req :remote-ip/str)
         auth-settings (or (get route-data :auth/setup) (auth/settings req))
         auth-db       (auth/db auth-settings)
         user          (user/get-login-data auth-db user-email)
         user-id       (get user :id)
         ac-type       (get user :account-type)
         pwd-suites    (select-keys user [:intrinsic :shared])
         auth-config   (delay (auth/config auth-settings ac-type))
         email-str     (identity/->str :email user-email)
         for-user      (log/for-user user-id email-str ipplain)
         for-mail      (log/for-user nil email-str ipplain)
         opname        (if auth-only-mode :auth :login)
         oplog-fn      (common/oplog-logger-populated req route-data)
         oplog         (fn [ok? l m a] (oplog-fn :ok? ok? :user-id user-id :opname opname
                                                 :level l :msg (str m " " a)))
         hard-locked?  (fn [] (common/hard-locked? user))
         soft-locked?  (fn [] (common/soft-locked? user @auth-config (t/now)))
         invalid-pwd?  (fn [] (not (check-password user password @auth-config)))]

     (cond

       (hard-locked?) (do (log/wrn "Account locked permanently" for-user)
                          (oplog false :info "Permanent lock" for-mail)
                          (qassoc req :user/authorized? false :user/authenticated? false
                                  :user/id user-id :user/account-type ac-type
                                  :auth/ok? false :app/status :auth/locked))

       (soft-locked?) (do (log/msg "Account locked temporarily" for-user)
                          (oplog false :info "Temporary lock" for-mail)
                          (qassoc req :user/authenticated? false :user/authorized? false
                                  :user/id user-id :user/account-type ac-type
                                  :auth/ok? false :app/status :auth/soft-locked))

       (invalid-pwd?) (do (log/wrn "Incorrect password or user not found" for-user)
                          (when user-id
                            (oplog false :warn "Bad password" for-mail)
                            (user/update-login-failed @auth-config user-id ipaddr))
                          (qassoc req :user/authorized? false :user/authenticated? false
                                  :user/id user-id :user/account-type ac-type
                                  :auth/ok? false :app/status :auth/bad-password))

       auth-only-mode (do (log/msg "Authentication successful" for-user)
                          (oplog true :info "Authentication OK" for-mail)
                          (user/update-login-ok auth-db user-id ipaddr)
                          (qassoc req :auth/ok? true :app/status :auth/ok
                                  :user/id user-id :user/account-type ac-type
                                  :user/authenticated? true))

       :authenticate! (do (log/msg "Login successful" for-user)
                          (oplog true :info "Login OK" for-mail)
                          (user/update-login-ok auth-db user-id ipaddr)
                          (let [^Session sess (or sess
                                                  (session/of req (or session-key
                                                                      (get route-data :session-key))))
                                hx?           (and (common/use-hx? req route-data :error/use-htmx?))
                                hx-pl?        (and hx? sess (session/soft-expired? sess))
                                goto-uri      (if-not hx-pl? (get-goto-uri req sess))
                                goto?         (boolean goto-uri)
                                prolonged?    (or hx-pl? goto?)
                                ^Session sess (if prolonged?
                                                (session/prolong sess ipaddr)
                                                (session/create  sess user-id email-str ipaddr))]

                            (if-not (session/valid? sess)

                              (let [e (session/error sess)
                                    c (:cause    e)
                                    s (:severity e)]
                                (when c
                                  (log/log (or s :warn) c)
                                  (oplog-fn :level s :user-id user-id :op :session :ok? false :msg c))
                                (qassoc req
                                        :auth/ok?            false
                                        :app/status          :auth/session-error
                                        :user/authenticated? false
                                        :user/authorized?    false
                                        :user/id             user-id
                                        :user/account-type   ac-type))

                              (-> req
                                  (qassoc
                                   :auth/ok?          true
                                   :app/status        (if prolonged? :auth/prolonged-ok :auth/ok)
                                   :auth/goto         goto-uri
                                   :auth/htmx?        hx?
                                   :auth/use-htmx?    hx?
                                   :user/id           user-id
                                   :user/account-type ac-type)
                                  (session/inject sess)
                                  (common/replace-session-id-header sess)
                                  (common/roles-refresh)))))))))

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
  tested. If the session is invalid, the redirect to a login page is performed. The
  destination URL is obtained via a route name taken from the `:auth/info` key of a
  route data, or from `:login` route identifier (as default). If the session is valid
  then the given request map is returned as-is."
  ([req user-email user-password]
   (authenticate! req user-email user-password nil))
  ([req user-email user-password session-key]
   (let [route-data             (delay (http/get-route-data req))
         ^String  user-password (if user-email (some-str user-password))
         ^Session sess          (session/of req (or session-key (get @route-data :session-key)))]

     (if user-password

       ;; Authenticate using email and password.
       (auth-user-with-password! req user-email user-password sess @route-data)

       ;; Check session.
       (if (session/invalid? sess)

         ;; Invalid session causes a redirect to a login page.
         (common/move-to req (get @route-data :auth/info :auth/info))

         ;; Valid session causes page to be served.
         req)))))

(defn set-password!
  "Sets user password for the given user ID. Returns `:pwd/created` if operation
  succeeded. May return error statuses: `:pwd/bad-user`, `:pwd/db-error`,
  `:pwd/bad-password`. User must have account type assigned for this operation to
  succeed."
  [req user-id password]
  (qassoc
   req :app/status
   (let [auth-settings (auth/settings req)
         auth-db       (auth/db auth-settings)]
     (if-some [ac-type (user/account-type auth-db :id user-id)]
       (if-some [auth-config (auth/config auth-settings ac-type)]
         (let [pwd-data (user/make-user-password auth-config password)]
           (if-some [pwd-suite-id (get pwd-data :suite-id)]
             (if-some [result (user/update-password (auth/db auth-config)
                                                    user-id
                                                    pwd-suite-id
                                                    (get pwd-data :intrinsic))]
               (if (pos-int? result)
                 :pwd/created
                 :pwd/bad-user)
               :pwd/db-error)
             :pwd/bad-password))
         :pwd/bad-user)
       :pwd/bad-user))))

;; Coercion error handler

(defn handle-coercion-error
  "Generic coercion error handler. Takes an exception object `e` and two functions:
  `respond` which will receive the response and should pre-process it (defaults to
  `clojure.core/identity` if falsy), `raise` which should handle the exception if its
  type is unusual and (possibly) re-throw it (defaults to `throw`)."
  [e respond raise]
  (let [data  (ex-data e)
        ctype (get data :type)]
    (if-let [status (case ctype
                      ::coercion/request-coercion  422
                      ::coercion/response-coercion 500
                      nil)]
      (if respond
        (respond {:status status :body (coercion/encode-error data)})
        {:status status :body (coercion/encode-error data)})
      (if raise
        (raise e)
        (throw e)))))

(defn throw-bad-param
  "Generates bad parameter exception which should trigger coercion error.
  The value of `param-type` must be a valid schema."
  [req param value param-type]
  (if-some [param (some-keyword param)]
    (let [param-type (some-keyword param-type)]
      (throw
       (ex-info
        "Parameter error"
        {:type        :reitit.coercion/request-coercion
         :request     req
         :response    nil
         :coercion    (http/get-route-data req :coercion)
         :in          [:request :form-params]
         :schema      [:map {:closed true} [param param-type]]
         :errors      (list {:path [param] :in [param] :schema param-type :value nil})
         :value       {param value}
         :transformed {param nil}})))))
