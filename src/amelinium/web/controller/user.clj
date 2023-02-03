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
            [amelinium.db                       :as              db]
            [amelinium.i18n                     :as            i18n]
            [amelinium.common                   :as          common]
            [amelinium.common.controller        :as           super]
            [amelinium.web                      :as             web]
            [amelinium.auth                     :as            auth]
            [amelinium.http                     :as            http]
            [amelinium.http.middleware.session  :as         session]
            [amelinium.http.middleware.language :as        language]
            [amelinium.model.user               :as            user]
            [amelinium.model.confirmation       :as    confirmation]
            [amelinium.http.client.twilio       :as          twilio]
            [io.randomseed.utils.map            :as             map]
            [io.randomseed.utils.map            :refer     [qassoc]]
            [io.randomseed.utils                :refer         :all])

  (:import [amelinium Session AuthSettings AuthConfig AuthConfirmation]
           [amelinium UserData Suites SuitesJSON]))

(def one-minute (t/new-duration 1 :minutes))

(defn retry-after
  "Returns an expiration date and time formatted according to the RFC 1123."
  [expires]
  (common/rfc1123-date-time expires))

(defn verify!
  "Performs the identity verification by sending an e-mail or SMS with a URL to
  complete confirmation."
  [req {:keys [no-data result reason db id id-type lang translator route-data]
        :as   opts}]
  (let [lang              (or lang       (common/pick-language req :registration) (common/lang-id req))
        tr                (or translator (i18n/no-default (common/translator req lang)))
        {:keys [confirmed?
                errors
                attempts
                expires]} result
        id-str            (db/identity->str id)
        id-type           (or id-type (get result :id-type) :email)
        errors?           (some? (seq errors))
        attempts?         (and (not errors?) (int? attempts))
        attempts-left     (if attempts? (if (neg? attempts) 0 attempts))
        max-attempts?     (if attempts? (zero? attempts-left))
        bad-result?       (not (or errors? attempts?))
        retry-dur         (delay (common/duration-nanos expires))
        retry-in          (delay (common/retry-in-mins @retry-dur))
        in-mins           (delay (tr :in-mins @retry-in))
        retry-in-mins     (delay (tr :try-in-mins @retry-in))
        mins-left         (delay (tr :mins-left @retry-in))
        attempts-left-w   (delay (tr :attempts-left attempts-left))]
    (cond
      bad-result?   (web/render-error  req (or no-data :verify/bad-result))
      errors?       (web/render-error  req errors)
      confirmed?    (web/render-status req :verify/confirmed)
      max-attempts? (-> req
                        (web/assoc-app-data :verify/retry-in           retry-in
                                            :verify/in-mins            retry-in-mins
                                            :verify/retry-unit         :minutes
                                            :verify/retry-dur          retry-dur
                                            :verify/mins-left          mins-left
                                            :verify/attempts-left      attempts-left
                                            :verify/attempts-left-word attempts-left-w
                                            :sub-status/description    retry-in-mins)
                        (web/add-header :Retry-After (retry-after expires))
                        (web/add-status :verify/max-attempts))
      :send!        (let [{:keys [token code
                                  exists?]} result
                          lang-str          (some-str lang)
                          remote-ip         (get req :remote-ip/str)
                          rdata             (or route-data (http/get-route-data req))
                          existing-uid      (if exists? (some-str (get result :existing-user/uid)))
                          existing-user-id  (if exists? (get result :existing-user/id))
                          lang-qs           (common/query-string-encode {"lang" lang-str})
                          url-type          (common/id-type->url-type id-type reason)
                          verify-link       (str (get rdata url-type) token "/?" lang-qs)
                          recovery-link     (if existing-uid (str (get rdata :url/recover) existing-uid "/?" lang-qs))
                          req-updater       (get opts :async/responder super/verify-request-id-update)
                          exc-handler       (get opts :async/raiser super/verify-process-error)
                          req-updater       #(req-updater db id-type id code token %)
                          exc-handler       #(exc-handler db id-type id code token %)
                          email?            (= :email id-type)
                          phone?            (and (not email?) (= :phone id-type))
                          user-login        (if email? id-str (if existing-user-id (delay (user/id-to-email db existing-user-id))))
                          template-params   (delay {:serviceName      (tr :verify/app-name)
                                                    :expiresInMinutes @in-mins
                                                    :remoteAddress    remote-ip
                                                    :verifyCode       (str code)
                                                    :verifyLink       verify-link
                                                    :recoveryLink     recovery-link})]
                      (case id-type
                        :email (if-some [template (get opts (if exists?
                                                              :tpl/email-exists
                                                              :tpl/email-verify))]
                                 (twilio/sendmail-l10n-template-async
                                  (get rdata :twilio/email)
                                  req-updater exc-handler
                                  lang id
                                  template
                                  @template-params))
                        :phone (if-some [sms-tr-key (get opts (if exists?
                                                                :tpl/phone-exists
                                                                :tpl/phone-verify))]
                                 (twilio/sendsms-async
                                  (get rdata :twilio/sms)
                                  req-updater exc-handler
                                  id (tr sms-tr-key @template-params))))
                      (-> req
                          (web/add-status :verify/sent)
                          (web/assoc-app-data
                           :user/identity             id-str
                           :user/phone                (if phone? id-str)
                           :user/email                (if email? id-str)
                           :user/login                user-login
                           :identity/phone?           phone?
                           :identity/email?           email?
                           :identity/type             id-type
                           :verify/retry-in           retry-in
                           :verify/in-mins            in-mins
                           :verify/retry-in-mins      retry-in-mins
                           :verify/retry-unit         :minutes
                           :verify/retry-dur          retry-dur
                           :verify/mins-left          mins-left
                           :verify/attempts-left      attempts-left
                           :verify/attempts-left-word attempts-left-w))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special actions (controller handlers)

(defn auth-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may emit a redirect or render a response."
  ([req user-email password]
   (auth-with-password! req user-email password nil nil nil false nil))
  ([req user-email password sess]
   (auth-with-password! req user-email password sess nil nil false nil))
  ([req user-email password sess route-data]
   (auth-with-password! req user-email password sess route-data nil false nil))
  ([req user-email password sess route-data lang]
   (auth-with-password! req user-email password sess route-data lang false nil))
  ([req user-email password sess route-data lang auth-only?]
   (auth-with-password! req user-email password sess route-data lang auth-only? nil))
  ([req user-email password sess route-data lang auth-only? session-key]
   (let [route-data (or route-data (http/get-route-data req))
         req        (super/auth-user-with-password! req user-email password sess route-data auth-only? session-key)]
     (if (web/response? req)
       req
       (case (get req :response/status)
         :auth/ok            (if auth-only? req (language/force req (or lang (web/pick-language-str req))))
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
       password          (auth-with-password! req user-email password sess @route-data @lang false session-key)
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
         rem-mins      (delay (super/lock-remaining-mins req (auth/db req) sess t/now))]
     (web/assoc-app-data req :lock-remains rem-mins))))

(defn prolong!
  "Prepares response data to be displayed on a prolongation page."
  ([req]
   (prolong! req nil))
  ([req session-key]
   (let [route-data    (http/get-route-data req)
         ^Session sess (session/of req (or session-key (get route-data :session-key)))]
     (cond

       (and (session/soft-expired? sess) (some? (super/get-goto sess)))
       (let [sess-key      (or (session/session-key sess) :session)
             ^Session sess (session/allow-soft-expired sess)
             rem-mins      (delay (super/lock-remaining-mins req (auth/db req) sess t/now))]
         (-> req
             (qassoc sess-key (qassoc sess :prolonged? true))
             (web/assoc-app-data :lock-remains rem-mins)))

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
                             app-uri       (http/get-route-data req :app.url/login)
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

(defn identity-create!
  "Verifies confirmation token against a database and if it matches, updates the
  identity (phone or e-mail)."
  ([req]
   (identity-create! req super/invalidate-user-sessions!))
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
                              app-uri       (get route-data :app.url/login)
                              qs            (common/query-string-encode {"login" login})
                              destination   (str app-uri "?" qs)]
                          (if session-invalidator (session-invalidator req route-data id-type id user-id))
                          (confirmation/delete db id "change")
                          (web/render-status req :identity/created nil
                                             {:app.url            app-uri
                                              :app.url/login      destination
                                              :confirmation/token token
                                              :confirmation/code  code
                                              :identity/email?    (= id-type :email)
                                              :identity/phone?    (= id-type :phone)
                                              :identity/type      id-type
                                              :user/identity      (db/identity->str id)
                                              :user/login         login
                                              :user/email         (db/identity->str login)
                                              :user/phone         (db/identity->str phone)
                                              :agent/mobile?      mobile-agent?}))
       (not confirmed?) (web/render-error req (:errors confirmation))
       (not updated?)   (web/render-error req (:errors updated))
       :error!          (web/render-error req (or (not-empty (:errors confirmation))
                                                  (not-empty (:errors updated))))))))

;; Password setting

(defn- pwd-status
  ([req]
   (pwd-status req (http/get-route-data req)))
  ([req route-data]
   (case (get req :response/status)
     :pwd/created      req
     :pwd/updated      req
     :pwd/bad-password (common/move-to req (get route-data :auth/bad-password :login/bad-password))
     :pwd/bad-user     (common/move-to req (get route-data :auth/bad-password :login/bad-password))
     (common/go-to req (get route-data :auth/error :login/error)))))

(defn password-change!
  "Changes password for the user authenticated with an old password and e-mail or sets
  the password for the given `user-id`."
  [req]
  (let [form-params  (get (get req :parameters) :form)
        user-email   (some-str (or (get form-params :username) (get form-params :login)))
        new-password (get form-params :new-password)
        new-repeated (get form-params :repeated-password)
        old-password (if user-email (some-str (get form-params :password)))
        route-data   (http/get-route-data req)
        req          (auth-with-password! req user-email old-password nil route-data nil true nil)]
    (if (web/response? req)
      req
      (if (= :auth/ok (get req :response/status))
        (if-not (= new-password new-repeated)
          (web/form-params-error! req {:repeated-password :passwords-no-match})
          (super/set-password!
           req
           (or (get req :user/id) (user/email-to-id (auth/db req) user-email))
           new-password))
        req))))

(defn password-recover!
  "Displays password recovery form and initiates password recovery by sending an e-mail
  or SMS message with a verification code or token."
  [req]
  (let [params (get req :parameters)]
    (if-some [id-type (some-keyword (get (get params :path) :id-type))]
      (let [phone? (or (= :phone id-type) (= :user/phone id-type))
            email? (not phone?)
            req    (web/assoc-app-data req :identity/type id-type :phone? phone? :email? email?)]
        (if-some [id (get (get params :form) id-type)]

          ;; initiate recovery
          ;; by generating a verification code and token
          ;; associated with the given identity
          ;; and sending e-mail or SMS message

          (let [auth-settings               (auth/settings req)
                auth-db                     (auth/db auth-settings)
                [props id-type]             (if phone?
                                              [(user/props-by-phone auth-db id) :phone]
                                              [(user/props-by-email auth-db id) :email])
                user-id                     (get props :id)
                ^AuthConfig auth-config     (auth/config auth-settings (get props :account-type))
                ^AuthConfig auth-config     (or auth-config (auth/config auth-settings))
                ^AuthConfirmation auth-cfrm (if auth-config (.confirmation auth-config))
                auth-db                     (if auth-config (.db auth-config) auth-db)
                attempts                    (if auth-cfrm   (.max-attempts auth-cfrm))
                exp                         (if auth-cfrm   (.expires      auth-cfrm))
                result                      (confirmation/create-for-recovery auth-db id user-id exp attempts id-type)
                req                         (verify! req {:id               id
                                                          :db               auth-db
                                                          :lang             (common/lang-id req)
                                                          :id-type          id-type
                                                          :reason           "recovery"
                                                          :result           result
                                                          :tpl/phone-exists :verify/sms-recovery
                                                          :tpl/email-exists :recovery/verify})]
            (web/response
             req
             (qassoc req :app/view :user/password-recover-sent)))

          ;; display initial password recovery form

          req))
      req)))

(defn password-update!
  "Displays password setting form and changes password for a user authenticated with a
  token or code."
  [req]
  (let [params        (get req :parameters)
        form-params   (get params :form)
        query-params  (get params :query)
        path-params   (get params :path)
        token         (or (get path-params :token) (get form-params :token))
        code          (get form-params :code)
        email         (get form-params :email)
        login         (or (get form-params :username) (get form-params :login) email)
        phone         (if-not email (get form-params :phone))
        id-type       (if email :email (if phone :phone))
        id            (or email phone)
        password      (some-str (get form-params :new-password))
        password-2    (some-str (get form-params :repeated-password))
        set-password? (some? (or password password-2))]
    (cond

      ;; password present, token or code received
      ;; validates confirmation and creates a new password

      (and set-password? (some? (or token (code id))))
      (if-not (= password password-2)
        (web/form-params-error! req {:repeated-password :passwords-no-match})
        (let [db   (auth/db req)
              cfrm (confirmation/establish db id code token one-minute "recovery")]
          (if (get cfrm :confirmed?)
            (do
              (confirmation/delete db id "recovery")
              (super/set-password! req (get cfrm :user/id) password))
            (web/render-error req (or (:errors cfrm) :verify/bad-result)))))

      ;; password not present, token or code received
      ;;
      ;; validates confirmation and displays a form for creating new password;
      ;; token is extracted from a database response to use it later instead of a code
      ;;
      ;; if mobile is detected and mobile application URL is defined,
      ;; renders a link or button to go back to the app and enter password there

      (and (not set-password?) (or token (and code id)))
      (let [db   (auth/db req)
            cfrm (confirmation/establish db id code token one-minute "recovery")]
        (if-not (get cfrm :confirmed?)
          (web/render-error req (or (:errors cfrm) :verify/bad-result))
          (let [id         (get cfrm :identity)
                id-type    (get cfrm :id-type)
                user-id    (get cfrm :user/id)
                id-str     (db/identity->str id)
                token      (some-str (or token (get cfrm :token)))
                user-email (db/identity->str (user/id-to-email db user-id))
                user-phone (delay (db/identity->str (user/id-to-phone db user-id)))
                phone?     (= id-type :phone)
                email?     (and (not phone?) (= id-type :email))
                mobile?    (delay (common/mobile-agent? req))
                app-url    (delay (if @mobile? (http/get-route-data req :app.url/recover)))
                app-link   (delay (if @app-url (str app-url "?"
                                                    (common/query-string-encode
                                                     {"login" user-email
                                                      "token" token}))))]
            (web/assoc-app-data
             req
             :user/login              user-email
             :user/email              user-email
             :user/phone              user-phone
             :user/identity           id-str
             :identity/type           id-type
             :identity/email?         email?
             :identity/phone?         phone?
             :confirmation/token      token
             :confirmation/code       code
             :confirmation/confirmed? true
             :agent/mobile?           mobile?
             :app.url/recover         app-link))))

      ;; no token nor code
      :else req)))
