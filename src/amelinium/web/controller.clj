(ns

    ^{:doc    "amelinium service, common controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.web.controller

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [reitit.ring                        :as       ring]
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
            [amelinium.web.model.user           :as       user]
            [io.randomseed.utils.time           :as       time]
            [io.randomseed.utils.var            :as        var]
            [io.randomseed.utils.map            :as        map]
            [io.randomseed.utils                :refer    :all]
            [amelinium.web                      :as        web]
            [amelinium.auth                     :as       auth]
            [amelinium.http                     :as       http]
            [amelinium.http.middleware.language :as   language]))

(def ^:const keywordize-params? false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data population

(defn route-data+
  "Injects route data directly into a request map."
  [req _]
  (get (get req ::r/match) :data))

(defn auth-db+
  "Injects authorization data source directly into a request map."
  [req _]
  (get (get (get req :route/data) :auth/config) :db))

(defn oplog-logger+
  "Injects operations logger function into a request map."
  [req _]
  (delay (web/oplog-logger req)))

(defn user-lang+
  "Injects user's preferred language into a request map."
  [req _]
  (delay
    (when-some [db (web/auth-db req)]
      (when-some [user-id (get (get req :session) :user/id)]
        (when-some [supported (get (get req :language/settings) :supported)]
          (supported (user/setting db user-id :language)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations

(defn- kw-form-data
  "Changes form data keys into keywords by calling
  ring.middleware.keyword-params/keyword-params-request on a crafted map."
  ([form-data]
   (kw-form-data form-data {}))
  ([form-data opts]
   (if (and keywordize-params? form-data)
     (-> (array-map :params form-data)
         (ring-kw/keyword-params-request opts)
         :params)
     form-data)))

(defn- extract-form-data
  "Gets go-to data from for a valid (and not expired) session. Returns form data as a
  map. The resulting map has session-id entry removed (if found)."
  ([req gmap]
   (extract-form-data req gmap nil))
  ([req gmap smap]
   (when (and gmap (= (get req :uri) (get gmap :uri)))
     (when-some [form-data (get gmap :form-data)]
       (when (and (map? form-data) (pos-int? (count form-data)))
         (let [smap    (or smap (get req :session))
               opts    (get req :session/config)
               sid-key (web/session-key smap opts)]
           (dissoc form-data sid-key)))))))

(defn- remove-login-data
  "Removes login data from the form params part of a request map."
  [req]
  (-> req
      (update :form-params dissoc "password")
      (update :params dissoc :password)))

(defn- cleanup-req
  [req [_ auth?]]
  (if auth? req (remove-login-data req)))

(defn- inject-goto
  "Injects go-to data into a request. Form data is merged only if go-to URI matches
  current page URI and session ID matches. Go-to URI is always injected. When the
  given gmap is broken it will set :goto-injected? to true but :goto-uri and :goto to
  false."
  ([req gmap]
   (inject-goto req gmap nil))
  ([req gmap smap]
   (if-not gmap
     req
     (if (web/session-variable-get-failed? gmap)
       (assoc req :goto-injected? true :goto-uri false :goto false)
       (let [req (assoc req :goto-injected? true :goto-uri (get gmap :uri))]
         (if-some [form-data (extract-form-data req gmap smap)]
           (-> req
               (update :form-params #(delay (merge form-data %)))
               (update :params      #(delay (merge (kw-form-data form-data) %))))
           req))))))

(defn check-password
  [user password auth-config]
  (when (and user password)
    (auth/check-password-json password
                              (get user :shared)
                              (get user :intrinsic)
                              auth-config)))

(defn login-data?
  "Returns true if :form-params map of a request contains login data."
  [req]
  (when-some [fparams (get req :form-params)]
    (and (contains? fparams "password")
         (contains? fparams "login"))))

(defn account-locked?
  "Returns true if an account associated with the session is hard-locked.
  Uses cached property."
  ([req session]
   (when-some [db (web/auth-db req)]
     (account-locked? req session db)))
  ([req session db]
   (some?
    (some->> session :user/id (user/prop-get-locked db)))))

(defn lock-remaining-mins
  "Returns the time of the remaining minutes of a soft account lock when the visited
  page ID is :login/account-soft-locked. Otherwise it returns nil. Uses cached user
  properties."
  ([req auth-db smap time-fn]
   (lock-remaining-mins req auth-db smap time-fn "login"))
  ([req auth-db smap time-fn id-form-field]
   (when auth-db
     (when-some [user (or (user/props-by-session auth-db smap)
                          (user/props-by-email auth-db (get (get req :form-params) id-form-field)))]
       (when-some [auth-config (http/get-route-data req :auth/config)]
         (when-some [mins (time/minutes (web/soft-lock-remains user auth-config (time-fn)))]
           (if (zero? mins) 1 mins)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

(defn- get-goto+
  "Gets go-to map from a session variable even if the session expired."
  [smap opts]
  (user/get-session-var (web/allow-soft-expired smap) opts :goto))

(defn- get-goto-for-valid+
  "Gets go-to map from session variable if the session is valid (and not expired)."
  [smap opts]
  (when (and smap (get smap :valid?))
    (get-goto+ smap opts)))

(defn- populate-goto+
  "Gets go-to data from session variable if it does not yet exist in req
  structure. Works also for expired session and only if go-to URI (:uri key of a map)
  is the same as currently visited page. Uses inject-goto to inject goto data from a
  session setting."
  ([req smap]
   (populate-goto+ req smap (get req :session/config)))
  ([req smap opts]
   (if (or (get req :goto-injected?) (not smap)
           (not (get (web/allow-soft-expired smap) :id)))
     req
     (inject-goto req (get-goto+ smap opts) smap))))

(defn- prolongation?
  "Returns true if session is expired (but not hard expired) and a user is not logged
  in and there is no login data present and we are not authenticating user. In other
  words: this returns true when we are good with redirecting user to a session
  prolongation page."
  [sess [login? auth?] login-data?]
  (or (and (get sess :expired?) (not (get sess :hard-expired?))
           (not login?)
           (or (not auth?) (not login-data?)))
      false))

(defn- prolongation-auth?
  "Returns true if user is being authenticated to prolongate the soft-expired session."
  [sess login? auth? login-data?]
  (or (and login-data? auth? (not login?) (:expired? sess)) false))

(defn- regular-auth?
  "Returns true if user is being authenticated."
  [sess login? auth? login-data?]
  (or (and auth? login-data? (not login?) (not sess)) false))

(defn- hard-expiry?
  "Returns true if the session is hard-expired and we are not on the hard-expired login
  page. Uses the given, previously collected session data, does not connect to a
  database."
  [req sess]
  (or (and (get sess :hard-expired?)
           (not (web/on-page? req :login/session-expired)))
      false))

(defn auth-user-with-password!
  "Authentication helper. Expects username and password to be present in form
  parameters. Used by other controllers. Short-circuits on certain conditions and may
  emit a redirect or render a response."
  ([req user-email password sess route-data lang]
   (let [ipaddr       (get req :remote-ip)
         ipplain      (get req :remote-ip/str)
         oplog        (or (get req :oplog/logger) (web/oplog-logger req route-data))
         auth-config  (or (get route-data :auth/config) (get req :auth/config))
         auth-db      (web/auth-db req auth-config)
         user         (user/get-login-data auth-db user-email auth-config)
         user-id      (get user :id)
         pwd-suites   (select-keys user [:intrinsic :shared])
         for-user     (log/for-user user-id user-email ipplain)
         for-mail     (log/for-user nil user-email ipplain)
         hard-locked? (fn [] (web/hard-locked? user))
         soft-locked? (fn [] (web/soft-locked? user auth-config (t/now)))
         invalid-pwd? (fn [] (not (check-password user password auth-config)))]

     (cond

       (hard-locked?) (do (log/wrn "Account locked permanently" for-user)
                          (oplog :user-id user-id :op :login :ok? false :msg (str "Permanent lock " for-mail))
                          (web/move-to req (get route-data :auth/locked :login/account-locked)))

       (soft-locked?) (do (log/msg "Account locked temporarily" for-user)
                          (oplog :user-id user-id :op :login :ok? false :msg (str "Temporary lock " for-mail))
                          (web/move-to req (get route-data :auth/soft-locked :login/account-soft-locked)))

       (invalid-pwd?) (do (log/wrn "Incorrect password or user not found" for-user)
                          (when user-id
                            (oplog :level :warning :user-id user-id :op :login :ok? false :msg (str "Bad password " for-mail))
                            (user/update-login-failed auth-db user-id ipaddr
                                                      (get auth-config :locking/max-attempts)
                                                      (get auth-config :locking/fail-expires)))
                          (web/move-to req (get route-data :auth/bad-password :login/bad-password)))

       (do (log/msg "Authentication successful" for-user)
           (web/oplog req :user-id user-id :op :login :message (str "Login OK " for-mail))
           (user/update-login-ok auth-db user-id ipaddr)
           :authenticated!)

       (let [goto-uri (when (get sess :expired?) (get req :goto-uri))
             opts     (get req :session/config)
             sess     (if goto-uri
                        (user/prolong-session sess opts ipaddr)
                        (user/create-session opts user-id user-email ipaddr))]

         (if-not (get sess :valid?)

           (let [e      (get sess :error)
                 r      (:reason e)
                 action (if goto-uri "prolongation" "creation")]

             (log/wrn "Session invalid after" action (log/for-user user-id user-email))
             (when r
               (log/log (:severity e :warn) r)
               (oplog :level e :user-id user-id :op :session :ok? false :msg r))
             (web/go-to req (get route-data :auth/session-error :login/session-error)))

           (if goto-uri
             (resp/temporary-redirect goto-uri)
             (-> req
                 (assoc :session sess)
                 (language/force (or lang (web/pick-language-str req)))
                 ((get (get req :roles/config) :handler identity))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special actions (controller handlers)

(defn authenticate!
  "Logs user in. Takes a request and obtains database connection, client IP address and
  authentication configuration from it. Also takes login and password fields from
  form parameters map (POST method) of a request. Calls check-password helper which
  uses database (user/get-password function) to obtain shared and intrinsic chain of
  password settings in a JSON format. Next step is to pass plain password, IP address
  and these chains (expressing vectors of maps) to a checker function referred in
  authentication settings (under the key :check-fn). When the result is a truthy
  value then user/update-login-ok is called, otherwise user/update-login-failed is
  called. If the login was successful, session is created (with user/create-session)
  and welcome page is rendered. If the login was not successful, user is redirected
  to a page explaining the reason (bad password or username, bad session or exceeded
  attempts)."
  [req]
  (let [form-params    (get req :form-params)
        user-email     (some-str (get form-params "login"))
        password       (when user-email (some-str (get form-params "password")))
        sess           (get req  :session)
        lang-settings  (get req :language/settings)
        valid-session? (get sess :valid?)
        ring-match     (ring/get-match req)
        route-data     (http/get-route-data req)
        lang           (web/pick-language-str req web/language-pickers-logged-in)]

    (if password
      ;; Authenticate using email and password.
      (auth-user-with-password! req user-email password sess route-data lang)

      ;; Check session.
      (if-not valid-session?

        ;; Invalid session causes a redirect to a login page.
        (web/move-to req (get route-data :auth/login :manager/login) lang)

        ;; Valid session causes page to be served.
        (if (some? (language/path-lang-id req lang-settings ring-match))
          ;; Render the contents in a language specified by the current path.
          req
          ;; Redirect to a proper language version of this very page.
          (web/move-to req (or (get route-data :name) (get req :uri)) lang))))))

(defn login!
  "Prepares response data to display a login page."
  [req]
  (let [sess       (get req :session)
        prolonged? (delay (some? (and (get sess :expired?) (get req :goto-uri))))]
    (-> req
        (assoc :session
               (delay (if @prolonged?
                        (assoc sess :id (or (get sess :id) (get sess :err/id)) :prolonged? true)
                        (assoc sess :prolonged? false))))
        (assoc-in [:app/data :lock-remains]
                  (delay (lock-remaining-mins req
                                              (web/auth-db req)
                                              (when @prolonged? sess)
                                              t/now))))))

(defn prep-request!
  "Prepares a request before any controller is called."
  [req]
  (let [req         (assoc req :app/data-required [] :app/data web/empty-lazy-map)
        sess        (get req :session)
        auth-state  (delay (web/login-auth-state req :login-page? :auth-page?))
        login-data? (delay (login-data? req))
        auth-db     (delay (web/auth-db req))]

    (cond

      ;; Request is invalid.

      (not (get req :validators/params-valid?))
      (-> req web/no-app-data web/render-bad-params)

      ;; There is no session. Short-circuit.

      (not (and sess (or (get sess :id) (get sess :err/id))))
      (-> req (cleanup-req @auth-state))

      ;; Account is manually hard-locked.

      (account-locked? req sess @auth-db)
      (let [user-id  (:user/id      sess)
            email    (:user/email   sess)
            ip-addr  (:remote-ip/str req)
            for-user (log/for-user user-id email ip-addr)
            for-mail (log/for-user nil email ip-addr)]
        (log/wrn "Hard-locked account access attempt" for-user)
        (web/oplog req
                   :user-id user-id
                   :op      :access-denied
                   :level   :warning
                   :msg     (str "Permanent lock " for-mail))
        (web/go-to req (or (http/get-route-data req :auth/account-locked)
                           :login/account-locked)))

      ;; Session expired and the time for prolongation has passed.

      (hard-expiry? req sess)
      (let [user-id  (:user/id      sess)
            email    (:user/email   sess)
            ip-addr  (:remote-ip/str req)
            for-user (log/for-user user-id email ip-addr)
            for-mail (log/for-user nil email ip-addr)]
        (log/msg "Session expired (hard)" for-user)
        (web/oplog req
                   :user-id user-id
                   :op      :session
                   :ok?     false
                   :msg     (str "Hard-expired " for-mail))
        (web/go-to req (or (http/get-route-data req :auth/session-expired)
                           :login/session-expired)))

      ;; Session expired and we are not reaching an authentication page nor a login page.
      ;; User can re-validate session using a login page.
      ;; We have to preserve form data and original, destination URI in a session variable.

      (prolongation? sess @auth-state @login-data?)
      (do (user/put-session-var (web/allow-soft-expired sess) (get req :session/config)
                                :goto {:uri       (get req :uri)
                                       :form-data (get (cleanup-req req @auth-state) :form-params)})
          (web/move-to req (or (http/get-route-data req :auth/prolongate)
                               :login/prolongate)))

      :----pass

      (let [valid-session? (get sess :valid?)
            req            (-> req
                               (populate-goto+ sess (get req :session/config))
                               (cleanup-req @auth-state))
            [_ auth?]      @auth-state
            goto?          (get req :goto-injected?)
            goto-uri       (and goto?  (get req :goto-uri))
            goto-failed?   (and goto?  (false? goto-uri))
            goto-unwanted? (and (some? (get req :goto-uri)) (not auth?))]

        ;; Session is invalid (or just expired without prolongation).
        ;; Notice the fact and go with displaying content.
        ;; Checking for a valid session is the responsibility of each controller.

        (and (not valid-session?) (not (and auth? @login-data?))
             (if (get sess :expired?)
               (let [user-id  (:user/id      sess)
                     email    (:user/email   sess)
                     ip-addr  (:remote-ip/str req)
                     for-user (log/for-user user-id email ip-addr)
                     for-mail (log/for-user nil email ip-addr)]
                 (log/msg "Session expired" for-user)
                 (web/oplog req
                            :user-id (:user/id sess)
                            :op      :session
                            :ok?     false
                            :msg     (str "Expired " for-mail)))
               (when-some [reason (:reason (:error sess))]
                 (web/oplog req
                            :user-id (:user/id sess)
                            :op      :session
                            :ok?     false
                            :level   (:error sess)
                            :msg     reason)
                 (log/log (:severity (:error sess) :warn) reason))))

        ;; Remove goto session variable as we already injected it into a response.
        ;; Remove goto session variable if it seems broken.
        ;; Condition: not applicable in prolonging mode.

        (and valid-session?
             (or goto-failed? goto-unwanted?)
             (user/del-session-var sess (get req :session/config) :goto))

        ;; Remove login data from the request if we are not authenticating a user.
        ;; Take care about broken go-to (move to a login page in such case).

        (if goto-failed?
          (web/move-to req (or (http/get-route-data :auth/session-error) :login/session-error))
          (cleanup-req req [nil auth?]))))))

(defn render-page!
  "Renders page after a specific controller was called. The :app/view and :app/layout
  keys are added to the request data by controllers to indicate which view and layout
  file should be used. Data passed to the template system is populated with common
  keys which should be present in :app/data."
  [req]
  (web/render-ok req))

(defn default
  [req]
  (assoc-in req [:vars :message]
            (str "amelinium 1.0.0")))
