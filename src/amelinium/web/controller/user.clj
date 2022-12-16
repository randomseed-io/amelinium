(ns

    ^{:doc    "amelinium service, web user controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.controller.user

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [tick.core                          :as               t]
            [clojure.string                     :as             str]
            [amelinium.types.session            :refer         :all]
            [amelinium.logging                  :as             log]
            [amelinium.common                   :as          common]
            [amelinium.common.controller        :as           super]
            [amelinium.web                      :as             web]
            [amelinium.auth                     :as            auth]
            [amelinium.http                     :as            http]
            [amelinium.http.middleware.session  :as         session]
            [amelinium.http.middleware.language :as        language]
            [amelinium.model.user               :as            user]
            [amelinium.model.confirmation       :as    confirmation]
            [io.randomseed.utils.map            :as             map]
            [io.randomseed.utils.map            :refer     [qassoc]]
            [io.randomseed.utils                :refer         :all])

  (:import [amelinium Session]))

(def one-minute (t/new-duration 1 :minutes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special actions (controller handlers)

(defn auth-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may emit a redirect or render a response."
  ([req user-email password]
   (auth-with-password! req user-email password nil nil nil nil))
  ([req user-email password sess]
   (auth-with-password! req user-email password sess nil nil nil))
  ([req user-email password sess route-data]
   (auth-with-password! req user-email password sess route-data nil nil))
  ([req user-email password sess route-data lang]
   (auth-with-password! req user-email password sess route-data lang nil))
  ([req user-email password sess route-data lang session-key]
   (let [req (super/auth-user-with-password! req user-email password sess route-data false session-key)]
     (if (web/response? req)
       req
       (case (get req :response/status)
         :auth/ok            (language/force req (or lang (web/pick-language-str req)))
         :auth/locked        (common/move-to req (get route-data :auth/locked        :login/account-locked))
         :auth/soft-locked   (common/move-to req (get route-data :auth/soft-locked   :login/account-soft-locked))
         :auth/bad-password  (common/move-to req (get route-data :auth/bad-password  :login/bad-password))
         :auth/session-error (common/go-to   req (get route-data :auth/session-error :login/session-error))
         (common/go-to req (get route-data :auth/error :login/error)))))))

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
  ([req]
   (authenticate! req nil))
  ([req session-key]
   (let [form-params        (get req :form-params)
         route-data         (delay (http/get-route-data req))
         ^String user-email (some-str (get form-params "login"))
         ^String password   (if user-email (some-str (get form-params "password")))
         ^Session sess      (session/of req (or session-key (get @route-data :session-key)))
         lang               (delay (web/pick-language-str req :user))
         valid-session?     (delay (session/valid? sess))]
     (cond
       password          (auth-with-password! req user-email password sess @route-data @lang session-key)
       @valid-session?   (if (some? (language/from-path req))
                           ;; Render the contents in a language specified by the current path.
                           req
                           ;; Redirect to a proper language version of this very page.
                           (web/move-to req (or (get @route-data :name) (get req :uri)) @lang))
       :invalid-session! (web/move-to req (or (get @route-data :auth/login) :auth/login) @lang)))))

(defn login!
  "Prepares response data to be displayed on a login page."
  ([req] (login! req nil))
  ([req session-key]
   (let [^Session sess (session/of req (or session-key (http/get-route-data req :session-key)))
         app-data      (get req :app/data web/empty-lazy-map)
         rem-mins      (delay (super/lock-remaining-mins req (auth/db req) sess t/now))]
     (qassoc req :app/data (qassoc app-data :lock-remains rem-mins)))))

(defn prolong!
  "Prepares response data to be displayed on a prolongation page."
  ([req]
   (prolong! req nil))
  ([req session-key]
   (let [route-data    (http/get-route-data req)
         ^Session sess (session/of req (or session-key (get route-data :session-key)))]
     (cond

       (and (session/soft-expired? sess) (some? (super/get-goto sess)))
       (let [app-data      (get req :app/data web/empty-lazy-map)
             sess-key      (or (session/session-key sess) :session)
             ^Session sess (session/allow-soft-expired sess)
             rem-mins      (delay (super/lock-remaining-mins req (auth/db req) sess t/now))]
         (qassoc req
                 sess-key  (qassoc sess :prolonged? true)
                 :app/data (qassoc app-data :lock-remains rem-mins)))

       (and sess (session/hard-expired? sess))
       (web/move-to req (or (get route-data :auth/session-expired) :login/session-expired))

       :bad-prolongation
       (web/move-to req (or (get route-data :auth/session-error) :login/session-error))))))

(defn create!
  "Verifies confirmation token against a database and if it matches creates the
  account."
  [req]
  (let [auth-config  (auth/config req)
        db           (auth/db auth-config)
        all-params   (get req :parameters)
        token        (get (get all-params :path) :token)
        params       (get all-params :form)
        code         (get params :code)
        login        (get params :login)
        confirmation (confirmation/establish db login code token one-minute "creation")
        confirmed?   (get confirmation :confirmed?)
        creation     (if confirmed? (user/create-with-token-or-code db login token code))
        created?     (if creation (get creation :created?))
        bad-result?  (or (nil? confirmation) (and confirmed? (nil? creation)))]
    (cond
      bad-result?      (web/render-error req :verify/bad-result)
      created?         (let [mobile-agent? (common/mobile-agent? req)
                             app-uri       (http/get-route-data req :url/login)
                             login         (or (some-str login) (get creation :email))
                             qs            (common/query-string-encode {"login" login})
                             destination   (str app-uri "?" qs)]
                         (confirmation/delete db login)
                         (web/render-status req :verify/done nil
                                            {:app.url            app-uri
                                             :app.url/login      destination
                                             :confirmation/token token
                                             :confirmation/code  code
                                             :user/login         login
                                             :agent/mobile?      mobile-agent?}))
      (not confirmed?) (web/render-error req (:errors confirmation))
      (not created?)   (web/render-error req (:errors creation))
      :error!          (web/render-error req (or (not-empty (:errors confirmation))
                                                 (not-empty (:errors creation)))))))

(defn invalidate-user-sessions!
  ([req route-data id-type id user-id]
   (invalidate-user-sessions! req route-data id-type id user-id nil))
  ([req route-data id-type id user-id session-key]
   (if (= :email id-type)
     (session/del-user-vars! req (or session-key (get route-data :session-key))))))

(defn id-update!
  "Verifies confirmation token against a database and if it matches, updates the
  identity."
  ([req]
   (id-update! req invalidate-user-sessions!))
  ([req session-invalidator]
   (let [auth-config  (auth/config req)
         db           (auth/db auth-config)
         all-params   (get req :parameters)
         path-params  (get all-params  :path)
         form-params  (get all-params  :form)
         token        (get path-params :token)
         id-type      (or (some-keyword (get path-params :id-type)) :email)
         id           (get form-params id-type)
         code         (get form-params :code)
         confirmation (confirmation/establish db id code token one-minute "change")
         confirmed?   (get confirmation :confirmed?)
         updated      (if confirmed? (user/update-identity-with-token-or-code id-type db id token code))
         updated?     (:updated? updated)
         bad-result?  (or (nil? confirmation) (and confirmed? (nil? updated)))]
     (cond
       bad-result?      (web/render-error req :verify/bad-result)
       updated?         (let [user-id       (get updated :id)
                              login         (get updated :email)
                              phone         (get updated :phone)
                              id            (get updated :identity)
                              mobile-agent? (common/mobile-agent? req)
                              route-data    (http/get-route-data req)
                              app-uri       (get route-data :url/login)
                              qs            (common/query-string-encode {"login" login})
                              destination   (str app-uri "?" qs)]
                          (if session-invalidator (session-invalidator req route-data id-type id user-id))
                          (confirmation/delete db id "change")
                          (web/render-status req :verify/done nil
                                             {:app.url            app-uri
                                              :app.url/login      destination
                                              :confirmation/token token
                                              :confirmation/code  code
                                              :identity/type      id-type
                                              :user/identity      id
                                              :user/login         login
                                              :user/email         login
                                              :user/phone         phone
                                              :agent/mobile?      mobile-agent?}))
       (not confirmed?) (web/render-error req (:errors confirmation))
       (not updated?)   (web/render-error req (:errors updated))
       :error!          (web/render-error req (or (not-empty (:errors confirmation))
                                                  (not-empty (:errors updated))))))))
