(ns

    ^{:doc    "amelinium service, common web controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.web.controller

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces                :as          p]
            [reitit.ring                        :as       ring]
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
            [amelinium.common.controller        :as     common]
            [io.randomseed.utils.time           :as       time]
            [io.randomseed.utils.var            :as        var]
            [io.randomseed.utils.map            :as        map]
            [io.randomseed.utils                :refer    :all]
            [amelinium.web                      :as        web]
            [amelinium.api                      :as        api]
            [amelinium.auth                     :as       auth]
            [amelinium.http                     :as       http]
            [amelinium.http.middleware.language :as   language]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data population

(p/import-vars [amelinium.common.controller
                route-data+ auth-db+ oplog-logger+ user-lang+])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Authentication

(p/import-vars [amelinium.common.controller
                check-password lock-remaining-mins
                account-locked? prolongation? prolongation-auth?
                regular-auth? hard-expiry?
                keywordize-params? kw-form-data])

(defn extract-form-data
  "Gets go-to data from for a valid (and not expired) session. Returns form data as a
  map. The resulting map has session-id entry removed (if found)."
  ([req gmap]
   (extract-form-data req gmap nil))
  ([req gmap smap]
   (when (and gmap (= (get req :uri) (get gmap :uri)))
     (when-some [form-data (get gmap :form-data)]
       (when (and (map? form-data) (pos-int? (count form-data)))
         (let [smap      (or smap (get req :session))
               sess-opts (get req :session/config)
               sid-key   (web/session-key smap sess-opts :session :session/config)]
           (dissoc form-data sid-key)))))))

(defn remove-login-data
  "Removes login data from the form params part of a request map."
  [req]
  (-> req
      (update :form-params dissoc "password")
      (update :params dissoc :password)))

(defn cleanup-req
  [req [_ auth?]]
  (if auth? req (remove-login-data req)))

(defn inject-goto
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

(defn login-data?
  "Returns true if :form-params map of a request contains login data."
  [req]
  (when-some [fparams (get req :form-params)]
    (and (contains? fparams "password")
         (contains? fparams "login"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

(defn get-goto+
  "Gets go-to map from a session variable even if the session expired."
  [smap sess-opts]
  (user/get-session-var sess-opts (web/allow-soft-expired smap) :goto))

(defn get-goto-for-valid+
  "Gets go-to map from session variable if the session is valid (and not expired)."
  [smap sess-opts]
  (when (and smap (get smap :valid?))
    (get-goto+ smap sess-opts)))

(defn populate-goto+
  "Gets go-to data from session variable if it does not yet exist in req
  structure. Works also for expired session and only if go-to URI (:uri key of a map)
  is the same as currently visited page. Uses inject-goto to inject goto data from a
  session setting."
  ([req smap]
   (populate-goto+ req smap (get req :session/config)))
  ([req smap sess-opts]
   (if (or (get req :goto-injected?) (not smap)
           (not (get (web/allow-soft-expired smap) :id)))
     req
     (inject-goto req (get-goto+ smap sess-opts) smap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special actions (controller handlers)

(defn auth-user-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may emit a redirect or render a response."
  [req user-email password sess route-data lang]
  (let [req (common/auth-user-with-password! req user-email password sess route-data)]
    (if (resp/response? req)
      req
      (language/force req (or lang (web/pick-language-str req))))))

(defn authenticate!
  "Logs user in when user e-mail and password are given, or checks if the session is
  valid to serve a current page.

  Takes a request map and obtains database connection, client IP address and
  authentication configuration from it. Also gets a user e-mail and a password from a
  map associated with the `:form-params` key of the `req`. Calls
  `auth-user-with-password!` to get a result or a redirect if authentication was not
  successful.

  If there is no e-mail or password given (the value is `nil`, `false` or an empty
  string) then authentication is not performed but instead validity of a session is
  tested. If the session is invalid redirect to a login page is performed. The
  destination URL is obtained via the route name taken from the `:auth/login` key of
  a route data, or from `:login` route identifier as default. If the destination path
  is parameterized with a language the redirect will set this path parameter to a
  value obtained by calling the `web/pick-language-str` using language detection
  chain identified by the `:user` key. The same language will be
  passed to `auth-user-with-password!`.

  If the session is valid then the given request map is returned with the
  `:authenticated!` key set to `true`."
  [req]
  (let [form-params    (get req :form-params)
        user-email     (some-str (get form-params "login"))
        password       (when user-email (some-str (get form-params "password")))
        sess           (get req :session)
        lang           (web/pick-language-str req :user)
        valid-session? (get sess :valid?)
        ring-match     (get req ::r/match)
        route-data     (http/get-route-data ring-match)]
    (cond
      password          (auth-user-with-password! req user-email password sess route-data lang)
      valid-session?    (if (some? (language/from-path req))
                          ;; Render the contents in a language specified by the current path.
                          req
                          ;; Redirect to a proper language version of this very page.
                          (web/move-to req (or (get route-data :name) (get req :uri)) lang))
      :invalid-session! (web/move-to req (get route-data :auth/login :login) lang))))

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
  "Prepares a request before any web controller is called."
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
      (do (user/put-session-var (get req :session/config)
                                (web/allow-soft-expired sess)
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
            goto-unwanted? (and (not auth?) (some? (get req :goto-uri)))]

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
             (user/del-session-var (get req :session/config) sess :goto))

        ;; Remove login data from the request if we are not authenticating a user.
        ;; Take care about broken go-to (move to a login page in such case).

        (if goto-failed?
          (web/move-to req (or (http/get-route-data :auth/session-error) :login/session-error))
          (cleanup-req req [nil auth?]))))))

(defn render!
  "Renders page after a specific web controller was called. The `:app/view` and
  `:app/layout` keys are added to the request data by controllers to indicate which
  view and layout file should be used. Data passed to the template system is
  populated with common keys which should be present in `:app/data`."
  [req]
  (web/render-ok req))

(defn default
  [req]
  (assoc-in req [:vars :message]
            (str "amelinium 1.0.0")))
