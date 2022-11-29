(ns

    ^{:doc    "amelinium service, common web controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.controller

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [tick.core                          :as               t]
            [reitit.core                        :as               r]
            [ring.util.http-response            :as            resp]
            [clojure.string                     :as             str]
            [amelinium.types.session            :refer         :all]
            [amelinium.logging                  :as             log]
            [amelinium.model.user               :as            user]
            [amelinium.common                   :as          common]
            [amelinium.common.controller        :as           super]
            [amelinium.i18n                     :as            i18n]
            [amelinium.web                      :as             web]
            [amelinium.auth                     :as            auth]
            [amelinium.http                     :as            http]
            [amelinium.http.middleware.session  :as         session]
            [amelinium.http.middleware.language :as        language]
            [amelinium.http.middleware.coercion :as        coercion]
            [io.randomseed.utils.map            :as             map]
            [io.randomseed.utils.map            :refer     [qassoc
                                                            qupdate]]
            [io.randomseed.utils                :refer         :all]
            [potpuri.core                       :refer [deep-merge]])

  (:import [amelinium Session]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Authentication

(defn saved-params
  "Gets go-to data for a valid (and not expired) session. Returns data as a map. The
  resulting map has, among form data, `:session-id` entry removed (if found) but
  `:ref-uri`, `:page` and other entries kept."
  ([req gmap]
   (saved-params req gmap nil))
  ([req gmap ^Session smap]
   (if (and gmap (= (get req :uri) (get gmap :uri)))
     (if-some [gmap (valuable (select-keys gmap [:form-params :query-params
                                                 :session-id :ref-uri :page]))]
       (let [^Session smap     (or (session/of smap) (session/of req))
             ^String sid-field (or (session/id-field smap) "session-id")]
         (if (and (= (session/db-id smap) (get gmap :session-id false)))
           (-> gmap
               (common/remove-form-params sid-field)
               (dissoc :session-id))))))))

(defn remove-login-data
  "Removes login data from the form params part of a request map."
  [req]
  (common/remove-form-params req :password))

(defn cleanup-req
  "Removes login data from `req` if we are on authentication page."
  [req [_ auth?]]
  (if auth? req (remove-login-data req)))

(defn inject-goto
  "Injects go-to data (`gmap`) into a request map `req` (affected, form-related keys
  are: `:form-params`, `:query-params` and `:params`. Form data is merged only if a
  go-to URI (`:uri` key of `gmap`) matches the URI of a current page. Go-to URI is
  always injected. When the given `gmap` is broken, it will set `:goto-injected?` to
  `true` but `:goto-uri` and `:goto` to `false`."
  ([req gmap]
   (inject-goto req gmap nil))
  ([req gmap ^Session smap]
   (if-not gmap
     req
     (if (or (common/session-variable-get-failed? gmap)
             (not (get gmap :uri)))
       (qassoc req
               :goto-injected? true
               :goto-uri      false
               :goto          false)
       (let [dest-uri (get gmap :uri)
             req      (qassoc req :goto gmap :goto-injected? true :goto-uri dest-uri)
             {:keys [form-params query-params parameters ref-uri page]}
             (saved-params req gmap smap)]
         (if (or form-params ref-uri page query-params parameters)
           (let [headers          (get req :headers)
                 req-form-params  (get req :form-params)
                 req-query-params (get req :query-params)
                 req-params       (get req :params)
                 req              (qassoc req :headers (qassoc headers "referer" ref-uri))
                 req              (if (nil? req-form-params) req
                                      (qassoc req :form-params
                                              (delay
                                                (conj (or form-params {})
                                                      (dissoc req-form-params "am/goto")))))
                 req              (if (nil? req-query-params) req
                                      (qassoc req :query-params
                                              (delay
                                                (conj (or query-params {}) req-query-params))))
                 req              (if (nil? req-params) req
                                      (qassoc req :params
                                              (delay
                                                (conj (or req-params {})
                                                      (dissoc req-params "am/goto" :am/goto)))))]
             req)
           req))))))

(defn login-data?
  "Returns true if `:form-params` map of a request map `req` contains login data."
  [req]
  (if-some [fparams (get req :form-params)]
    (and (contains? fparams "password")
         (contains? fparams "login"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

(defn get-goto
  "Gets go-to map from a session variable even if the session is not valid."
  [^Session smap]
  (session/get-var smap :goto))

(defn get-goto-for-valid
  "Gets go-to map from session variable if the session is valid (and not expired)."
  [^Session smap]
  (if (and smap (session/valid? smap))
    (get-goto smap)))

(defn populate-goto
  "Gets a go-to data from a session variable if it does not yet exist in the `req`
  context. Works also for the expired session and only if a go-to URI (`:uri` key of
  a map) is the same as the URI of a currently visited page. Uses `inject-goto` to
  inject the data.

  Go-to data will not be populated (or even tried) if there is no session, or is
  there is already a value associated with the `:goto-injected` key, or form params
  does not contain `am/goto` parameter of any value (even an empty string or `nil`)."
  [req ^Session smap]
  (if (and smap
           (contains? (get req :form-params) "am/goto")
           (not (get req :goto-injected?)))
    (let [smap-ok (session/allow-soft-expired smap)]
      (if (nil? (session/id smap-ok))
        req
        (inject-goto req (get-goto smap-ok) smap)))
    req))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special actions (controller handlers)

(defn auth-user-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may emit a redirect or render a response."
  [req user-email password sess route-data lang]
  (let [req (super/auth-user-with-password! req user-email password sess route-data)]
    (if (web/response? req)
      req
      (case (get req :response/status)
        :auth/ok            (language/force req (or lang (web/pick-language-str req)))
        :auth/locked        (common/move-to req (get route-data :auth/locked        :login/account-locked))
        :auth/soft-locked   (common/move-to req (get route-data :auth/soft-locked   :login/account-soft-locked))
        :auth/bad-password  (common/move-to req (get route-data :auth/bad-password  :login/bad-password))
        :auth/session-error (common/go-to   req (get route-data :auth/session-error :login/session-error))
        (common/go-to req (get route-data :auth/error :login/error))))))

(defn authenticate!
  "Logs user in when user e-mail and password are given, or checks if the session is
  valid to serve a current page.

  Takes a request map and obtains a database connection, a client IP address and an
  authentication configuration from it. Also gets user's e-mail and a password from a
  map associated with the `:form-params` key of the `req`. Calls
  `auth-user-with-password!` to get the result or perform a redirect if the
  authentication was not successful.

  If there is no e-mail nor password given (the value is `nil`, `false` or an empty
  string) then the authentication is not performed but instead the validity of a
  session is tested. If the session is invalid a redirect to the login page is
  performed; the destination URL is obtained by looking up the route data key
  `:auth/login` and taking a route name associated with it, or by visiting an URL
  associated with the `:login` route name (as default, when the previous lookup was
  not successful). If the destination path is parameterized with a language, the
  redirect will set this path parameter to a value obtained by calling the
  `web/pick-language-str`, using language detection chain identified by the `:user`
  key. The same language will be passed to the `auth-user-with-password!` call.

  If the session is valid then the given request map is returned with the
  `:authenticated!` key set to `true`."
  [req]
  (let [form-params        (get req :form-params)
        ^String user-email (some-str (get form-params "login"))
        ^String password   (if user-email (some-str (get form-params "password")))
        ^Session sess      (session/of req)
        lang               (delay (web/pick-language-str req :user))
        route-data         (delay (http/get-route-data req))
        valid-session?     (delay (session/valid? sess))]
    (cond
      password          (auth-user-with-password! req user-email password sess @route-data @lang)
      @valid-session?   (if (some? (language/from-path req))
                          ;; Render the contents in a language specified by the current path.
                          req
                          ;; Redirect to a proper language version of this very page.
                          (web/move-to req (or (get @route-data :name) (get req :uri)) @lang))
      :invalid-session! (web/move-to req (or (get @route-data :auth/login) :auth/login) @lang))))

(defn login!
  "Prepares response data to be displayed on a login page."
  [req]
  (let [^Session sess (session/of req)
        app-data      (get req :app/data web/empty-lazy-map)
        rem-mins      (delay (super/lock-remaining-mins req (auth/db req) sess t/now))]
    (qassoc req :app/data (qassoc app-data :lock-remains rem-mins))))

(defn prolong!
  "Prepares response data to be displayed on a prolongation page."
  [req]
  (let [^Session sess (session/of req)]
    (cond

      (and (session/soft-expired? sess) (some? (get-goto sess)))
      (let [app-data      (get req :app/data web/empty-lazy-map)
            sess-key      (or (session/session-key sess) :session)
            ^Session sess (session/allow-soft-expired sess)
            rem-mins      (delay (super/lock-remaining-mins req (auth/db req) sess t/now))]
        (qassoc req
                sess-key  (qassoc sess :prolonged? true)
                :app/data (qassoc app-data :lock-remains rem-mins)))

      (and sess (session/hard-expired? sess))
      (web/move-to req (or (http/get-route-data req :auth/session-expired)
                           :login/session-expired))

      :bad-prolongation
      (web/move-to req (or (http/get-route-data req :auth/session-error)
                           :login/session-error)))))

(defn prep-request!
  "Prepares a request before any web controller is called."
  [req]
  (let [req           (qassoc req :app/data-required [] :app/data web/empty-lazy-map)
        ^Session sess (session/not-empty-of req)
        route-data    (http/get-route-data req)
        auth-state    (delay (common/login-auth-state req :login-page? :auth-page?))
        login-data?   (delay (login-data? req))
        auth-db       (delay (auth/db req))]

    (cond

      ;; Request is invalid.

      (not (get req :validators/params-valid?))
      (web/render-bad-params req nil ["error" "bad-params"] "error")

      ;; There is no real session. Short-circuit.

      (not sess)
      (-> req (cleanup-req @auth-state))

      ;; Account is manually hard-locked.

      (super/account-locked? req sess @auth-db)
      (let [user-id  (session/user-id    sess)
            email    (session/user-email sess)
            ip-addr  (get req :remote-ip/str)
            for-user (log/for-user user-id email ip-addr)
            for-mail (log/for-user nil email ip-addr)]
        (log/wrn "Hard-locked account access attempt" for-user)
        (common/oplog req
                      :user-id user-id
                      :op      :access-denied
                      :level   :warning
                      :msg     (str "Permanent lock " for-mail))
        (web/go-to req (or (get route-data :auth/account-locked) :login/account-locked)))

      ;; Session expired and the time for prolongation has passed.

      (super/hard-expiry? req sess route-data)
      (let [user-id  (session/user-id    sess)
            email    (session/user-email sess)
            ip-addr  (get req :remote-ip/str)
            for-user (log/for-user user-id email ip-addr)
            for-mail (log/for-user nil email ip-addr)]
        (log/msg "Session expired (hard)" for-user)
        (common/oplog req
                      :user-id user-id
                      :op      :session
                      :ok?     false
                      :msg     (str "Hard-expired " for-mail))
        (web/go-to req (or (get route-data :auth/session-expired) :login/session-expired)))

      ;; Session expired and we are not reaching an authentication page nor a login page.
      ;; User can re-validate session using a login page.
      ;; We have to preserve form data and original, destination URI in a session variable.

      (super/prolongation? sess @auth-state @login-data?)
      (let [req           (cleanup-req req @auth-state)
            ^Session sess (session/allow-soft-expired sess)
            session-field (or (session/id-field sess) "session-id")
            req-to-save   (common/remove-form-params req session-field)]
        (session/put-var! sess
                          :goto {:ts           (t/now)
                                 :uri          (get req :uri)
                                 :ref-uri      (some-str (get (get req :headers) "referer"))
                                 :page         (get route-data :name)
                                 :session-id   (session/db-id sess)
                                 :form-params  (get req-to-save :form-params)
                                 :query-params (get req-to-save :query-params)
                                 :params       (get req-to-save :params)})
        (web/move-to req (or (get route-data :auth/prolongate) :login/prolongate)))

      :----pass

      (let [valid-session? (session/valid? sess)
            auth?          (nth @auth-state 1 false)
            req            (if auth? req (populate-goto req sess))
            goto-uri       (if auth? nil (get req :goto-uri))
            req            (cleanup-req req @auth-state)]

        ;; Session is invalid (or just expired without prolongation).
        ;; Notice the fact and go with rendering the content.
        ;; Checking for a valid session is the responsibility of each controller.

        (if (and (not valid-session?) (not (and auth? @login-data?)))
          (if (session/expired? sess)
            (let [user-id  (session/user-id sess)
                  email    (session/user-email sess)
                  ip-addr  (:remote-ip/str req)
                  for-user (log/for-user user-id email ip-addr)
                  for-mail (log/for-user nil email ip-addr)]
              (log/msg "Session expired" for-user)
              (common/oplog req
                            :user-id user-id
                            :op      :session
                            :ok?     false
                            :msg     (str "Expired " for-mail)))
            (when-some [sess-err (session/error sess)]
              (common/oplog req
                            :user-id (session/user-id sess)
                            :op      :session
                            :ok?     false
                            :level   (:severity sess-err)
                            :msg     (:cause    sess-err "Unknown session error"))
              (log/log (:severity sess-err :warn) (:cause sess-err "Unknown session error")))))

        ;; Remove goto session variable as we already injected it into a response.
        ;; Remove goto session variable if it seems broken.
        ;; Condition: not applicable in prolonging mode.

        (if goto-uri
          (if (false? goto-uri)
            ;; Take care of broken go-to.
            (do (session/del-var! sess :goto)
                (web/move-to req (or (get route-data :auth/session-error) :login/session-error)))
            ;; Remove utilized go-to.
            (if valid-session?
              (do (session/del-var! sess :goto)
                  req)
              req))
          req)))))

(defn render!
  "Renders page after a specific web controller was called. The `:app/view` and
  `:app/layout` keys are added to the request data by controllers to indicate which
  view and layout file should be used. Data passed to the template system is
  populated with common keys which should be present in `:app/data`. If the
  `:response/fn` is set then it will be used instead of `web/render-ok` to render an
  HTML response."
  ([req]
   (if-some [st (get req :response/status)]
     (web/render-status req st)
     (if-some [f (get req :response/fn)]
       (f req)
       (web/render-ok req))))
  ([req status-or-fn]
   (if (ident? status-or-fn)
     (web/render-status req status-or-fn)
     (if (fn? status-or-fn)
       (status-or-fn req)
       (web/render-ok req)))))

(defn default
  [req]
  (assoc-in req [:vars :message]
            (str "amelinium 1.0.0")))

;; Coercion

(defn handle-coercion-error
  "Called when coercion exception is thrown by the handler executed earlier in a
  middleware chain. Takes an exception object `e`, response wrapper `respond`, and
  a `raise` function.

  When coercion error is detected during request processing, it creates a map (by
  calling the `amelinium.http.middleware.coercion/map-errors`) containing parameter
  identifiers associated with parameter types (or with `nil` values if type
  information is not available).

  If there is a session then this map is stored in a session variable `:form-errors`
  under the `:errors` key (additionally, there is a `:dest` key identifying a path of
  the current page).

  If there is no valid session or a session variable cannot be stored, the result is
  serialized as a query string parameter `form-errors` with erroneous fields
  separated by commas.

  If type name is available then a string in a form of `parameter:type` is
  generated.

  If type name is not available, a simple parameter name is generated. So the example
  value (before encoding) may look like `email,secret:password` (`email` parameter
  without type information, `secret` parameter with type named `password`).

  Next, the originating URI is obtained from the `Referer` header and a temporary
  redirect (with HTTP code 307) is generated with this path and a query string
  containing `form-errors` parameter. The value of the parameter is empty if form
  errors were saved in a session variable.

  The destination of the redirect can be overriden by the `:form-errors/page`
  configuration option associated with HTTP route data.

  If the destination URI cannot be established, or if a coercion error happened during
  handling some previous coercion error (so current page is where the browser had
  been redirected to), then instead of generating a redirect, a regular page is
  rendered with HTTP code of 422. The `:app/data` key of a request map is updated
  with:

  - `:title` (translated message of `:parameters/error`),
  - `:form/errors` (a map with the result of calling
                    `amelinium.http.middleware.coercion/map-errors`
                    under `:errors` and ),
  - `:coercion/errors` (result of `amelinium.http.middleware.coercion/explain-errors`).

  When a coercion error is detected during response processing, a web page of HTTP
  code 500 is rendered. The `:app/data` key of a request map is updated with the:

  - `:title` (translated message of `:output/error`),
  - `:form/errors` (result of `amelinium.http.middleware.coercion/explain-errors`)."
  [e respond raise]
  (let [data  (ex-data e)
        req   (get data :request)
        ctype (get data :type)
        data  (dissoc data :request :response)]
    (case ctype

      :reitit.coercion/request-coercion
      (let [goto-page              (if-let [goto (get req :goto)] (get goto :page))
            orig-from-goto         (if goto-page (http/route-data-param req goto-page))
            orig-page              (or orig-from-goto (http/get-route-data req :form-errors/page))
            referer                (if (nil? orig-page) (some-str (get (get req :headers) "referer")))
            [orig-uri orig-params] (if referer (common/url->uri+params req referer))
            handling-previous?     (contains? (get req :query-params) "form-errors")]
        (respond
         (if (and (or (valuable? orig-page) (valuable? orig-uri) referer)
                  (not handling-previous?))
           ;; redirect to a form-submission page allowing user to correct errors
           ;; transfer form errors using query params or form params (if a session is present)
           (let [errors        (coercion/map-errors data)
                 orig-uri      (if orig-uri (some-str orig-uri))
                 orig-params   (if orig-uri orig-params)
                 destination   (or orig-page orig-uri)
                 dest-uri      (if (keyword? destination) (common/page req destination) destination)
                 dest-uri      (some-str dest-uri)
                 ^Session smap (session/not-empty-of req)
                 stored?       (if (and smap (session/valid? smap))
                                 (session/put-var!
                                  smap :form-errors
                                  {:dest   dest-uri
                                   :errors errors
                                   :params (->> (keys (get data :transformed))
                                                (select-keys (get data :value)))}))
                 error-params  (if stored? "" (coercion/join-errors errors))
                 joint-params  (qassoc orig-params "form-errors" error-params)]
             (if dest-uri
               (common/temporary-redirect req dest-uri nil joint-params)
               (resp/temporary-redirect
                (str referer (if (str/includes? referer "?") "&" "?")
                     (common/query-string-encode joint-params)))))
           ;; render a separate page describing invalid parameters
           ;; instead of current page
           (let [translate-sub (i18n/no-default (common/translator-sub req))]
             (-> req
                 (map/assoc-missing :app/data common/empty-lazy-map)
                 (qassoc
                  :app/data
                  (-> (get req :app/data web/empty-lazy-map)
                      (qassoc :title           (delay (translate-sub :parameters/error))
                              :coercion/errors (delay (coercion/explain-errors data translate-sub))
                              :form/errors     (delay {:errors (coercion/map-errors data)
                                                       :dest   (:uri req)
                                                       :params (->> (keys (get data :transformed))
                                                                    (select-keys (get data :value)))}))))
                 web/render-bad-params)))))

      :reitit.coercion/response-coercion
      (let [error-list (coercion/list-errors data)]
        (log/err "Response coercion error:" (coercion/join-errors-with-values error-list))
        (respond (web/render-error req :output/error)))

      (raise e))))
