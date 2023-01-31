(ns

    ^{:doc    "amelinium service, API user controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.api.controller.user

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [tick.core                          :as               t]
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
            [amelinium.http.middleware.language :as        language]
            [amelinium.http.client.twilio       :as          twilio]
            [amelinium.model.confirmation       :as    confirmation]
            [amelinium.model.user               :as            user]
            [amelinium.types.auth               :refer         :all]
            [amelinium.types.session            :refer         :all]
            [amelinium                          :refer         :all]
            [puget.printer                      :refer     [cprint]])

  (:import [amelinium Session AuthSettings AuthConfig AuthConfirmation]
           [amelinium UserData Suites SuitesJSON]))

(def one-minute   (t/new-duration 1 :minutes))
(def five-minutes (t/new-duration 5 :minutes))

;; Helpers

(defn- try-lower-case
  [v]
  (if (string? v) (str/lower-case v) v))

(defn retry-after
  "Returns an expiration date and time formatted according to the RFC 1123."
  [expires]
  (common/rfc1123-date-time expires))

(defn auth-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may render a response.

  Initial session `sess` will serve as a configuration source to create a new session
  and inject it into a request map `req` under configured session key.

  Session status will be added to a response body (under the `:response/body` key of
  the processed request map) and the detected or given language will be set in using
  `amelinium.http.middleware.language/force` on the returned request map."
  ([req user-email password]
   (auth-with-password! req user-email password nil nil nil false))
  ([req user-email password ^Session sess]
   (auth-with-password! req user-email password sess nil nil false))
  ([req user-email password ^Session sess route-data]
   (auth-with-password! req user-email password sess route-data nil false))
  ([req user-email password ^Session sess route-data lang]
   (auth-with-password! req user-email password sess route-data lang false))
  ([req user-email password ^Session sess route-data lang auth-only-mode]
   (auth-with-password! req user-email password sess route-data lang auth-only-mode nil))
  ([req user-email password ^Session sess route-data lang auth-only-mode session-key]
   (let [sk  (or session-key (session/session-key sess) (get route-data :session-key))
         req (super/auth-user-with-password! req user-email password sess route-data auth-only-mode session-key)]
     (if (api/response? req)
       req
       (let [lang   (or lang (common/pick-language req))
             tr-sub (i18n/no-default (common/translator-sub req lang))]
         (do
           (-> req
               (language/force lang)
               (api/body-add-session-status session-key tr-sub))))))))

;; Controllers

(defn authenticate!
  "Logs user in when user e-mail and password are given, or checks if the session is
  valid to serve a current page.

  Takes a request map and obtains database connection, client IP address and
  authentication configuration from it. Also gets a user e-mail and a password from a
  map associated with the `:parameters` key and then with `:form` key of the
  `req`. Calls `auth-with-password!` to get the result or a redirect if
  authentication was not successful.

  If there is no e-mail nor password given (the value is `nil`, `false` or an empty
  string) then the password authentication is not performed but instead the validity
  of a session is tested. If the detected session is valid then the given request map
  is returned as is.

  If there was no login nor password given and the session is invalid then a redirect
  to a login page is performed. Its destination URL is obtained via a route name
  taken from the `:auth/info` key of a route data, or from the `:auth/info` route
  identifier (as a default fallback)."
  ([req]
   (authenticate! req nil))
  ([req session-key]
   (api/response
    req
    (let [form-params    (get (get req :parameters) :form)
          user-email     (some-str (get form-params :login))
          password       (if user-email (some-str (get form-params :password)))
          route-data     (delay (http/get-route-data req))
          session-key    (or session-key (get @route-data :session-key))
          sess           (session/of req session-key)
          lang           (delay (common/pick-language req :user))
          valid-session? (delay (session/valid? sess))]
      (cond
        password          (auth-with-password! req user-email password sess @route-data @lang false session-key)
        @valid-session?   req
        :invalid-session! (api/move-to req (or (get @route-data :auth/info) :auth/info) @lang))))))

(defn authenticate-only!
  "Logs user in when user e-mail and password are given.

  Takes a request map and obtains a database connection, client IP address and
  authentication configuration from it. Also gets user's e-mail and a password from a
  map associated with the `:parameters` key and then `:form` key of the `req`. Calls
  `amelinium.common.controller/auth-user-with-password!` to get the authentication result
  with `auth-only-mode` argument set to `true`.

  If there is no e-mail nor password given (the value is `nil`, `false` or an empty
  string) then authentication is not performed.

  Contrary to the `authenticate!`, session information is not checked nor injected
  into a prepared response body and a language is not forced."
  [req]
  (api/response
   req
   (let [form-params (get (get req :parameters) :form)
         user-email  (some-str (get form-params :login))
         password    (if user-email (some-str (get form-params :password)))]
     (super/auth-user-with-password! req user-email password nil nil true nil))))

(defn info!
  "Returns login information."
  ([req] (info! req nil))
  ([req session-key]
   (let [auth-db       (auth/db req)
         session-key   (or session-key (http/get-route-data req :session-key))
         ^Session sess (session/of req session-key)
         prolonged?    (some? (and (session/expired? sess) (get req :goto-uri)))
         remaining     (super/lock-remaining-mins req auth-db (if prolonged? sess) t/now)
         body          (qassoc (get req :response/body) :lock-remains remaining)]
     (qassoc req
             :response/body body
             session-key    (delay
                              (if @prolonged?
                                (qassoc sess :id (or (session/id sess) (session/err-id sess)) :prolonged? true)
                                (qassoc sess :prolonged? false)))))))

;; Identity confirmation

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
        id-type           (or id-type (get :id-type result) :email)
        errors?           (some? (seq errors))
        attempts?         (and (not errors?) (int? attempts))
        attempts-left     (if attempts? (if (neg? attempts) 0 attempts))
        max-attempts?     (if attempts? (zero? attempts-left))
        bad-result?       (not (or errors? attempts?))
        retry-dur         (delay (common/duration-nanos expires))
        retry-in          (delay (common/retry-in-mins @retry-dur))]
    (cond
      bad-result?   (api/render-error  req (or no-data :verify/bad-result))
      errors?       (api/render-error  req errors)
      confirmed?    (api/render-status req :verify/confirmed)
      max-attempts? (-> req
                        (api/add-header :Retry-After (retry-after expires))
                        (api/add-status :verify/max-attempts)
                        (api/assoc-body :verify/retry-in        @retry-in
                                        :verify/retry-unit      :minutes
                                        :verify/retry-dur       @retry-dur
                                        :verify/attempts-left   attempts-left
                                        :sub-status/description (tr :try-in-mins @retry-in)))
      :send!        (let [{:keys [token code
                                  exists?]} result
                          lang-str          (some-str lang)
                          remote-ip         (get req :remote-ip/str)
                          rdata             (or route-data (http/get-route-data req))
                          existing-uid      (if exists? (some-str (get result :existing-user/uid)))
                          lang-qs           (common/query-string-encode {"lang" lang-str})
                          url-type          (common/id-type->url-type id-type reason)
                          verify-link       (str (get rdata url-type) token "/?" lang-qs)
                          recovery-link     (if existing-uid (str (get rdata :url/recover) existing-uid "/?" lang-qs))
                          req-updater       (get opts :async/responder super/verify-request-id-update)
                          exc-handler       (get opts :async/raiser super/verify-process-error)
                          req-updater       #(req-updater db id-type id code token %)
                          exc-handler       #(exc-handler db id-type id code token %)
                          add-retry-fields  (fn [req]
                                              (api/assoc-body
                                               req
                                               :verify/retry-in      @retry-in
                                               :verify/retry-unit    :minutes
                                               :verify/retry-dur     @retry-dur
                                               :verify/attempts-left attempts-left))
                          template-params   (delay {:serviceName      (tr :verify/app-name)
                                                    :expiresInMinutes (tr :in-mins @retry-in)
                                                    :remoteAddress    remote-ip
                                                    :verifyCode       (str code)
                                                    :verifyLink       verify-link
                                                    :recoveryLink     recovery-link})]
                      (case id-type
                        :user/email (if-some [template (get opts (if exists?
                                                                   :tpl/email-exists
                                                                   :tpl/email-verify))]
                                      (twilio/sendmail-l10n-template-async
                                       (get rdata :twilio/email)
                                       req-updater exc-handler
                                       lang id
                                       template
                                       @template-params))
                        :user/phone (if-some [sms-tr-key (get opts (if exists?
                                                                     :tpl/phone-exists
                                                                     :tpl/phone-verify))]
                                      (twilio/sendsms-async
                                       (get rdata :twilio/sms)
                                       req-updater exc-handler
                                       id (tr sms-tr-key @template-params))))
                      (-> req
                          (api/add-status :verify/sent)
                          (add-retry-fields))))))

(defn resend!
  "Re-sends verification e-mail or SMS to confirm the given identity when creating new
  account."
  [req reason]
  (api/response
   req
   (let [auth-settings   (auth/settings req)
         params          (get (get req :parameters) :form)
         ^UserData udata (user/make-user-data-simple auth-settings params)
         phone           (.phone udata)
         email           (.email udata)]
     (if (and phone email)
       (api/render-error req :verify/multiple-ids)
       (let [db          (if udata (.db udata))
             reason      (or (some-str reason) "creation")
             [id id-type
              no-data f] (if phone
                           [phone :phone :verify/bad-phone confirmation/retry-phone]
                           [email :email :verify/bad-email confirmation/retry-email])
             result      (f db id reason)]
         (verify! req {:db               db
                       :id               id
                       :lang             (common/lang-id req)
                       :id-type          id-type
                       :no-data          no-data
                       :reason           reason
                       :result           result
                       :tpl/email-exists :registration/exists
                       :tpl/email-verify :registration/verify
                       :tpl/phone-exists :verify/sms-exists
                       :tpl/phone-verify :verify/sms}))))))

;; Registration

(defn register!
  [req]
  (api/response
   req
   (let [params                      (get (get req :parameters) :form)
         ^AuthSettings auth-settings (auth/settings req)
         ^UserData     udata         (user/make-user-data auth-settings params)
         db                          (if udata (.db udata))
         result                      (confirmation/create-for-registration db udata)]
     (verify! req {:db               db
                   :id               (if udata (.email udata))
                   :id-type          :email
                   :result           result
                   :tpl/email-exists :registration/exists
                   :tpl/email-verify :registration/verify
                   :tpl/phone-exists :verify/sms-exists
                   :tpl/phone-verify :verify/sms}))))

;; Profile editing

(defn edit!
  ([req] (edit! req nil))
  ([req session-key]
   (api/response
    req
    (let [smap        (session/of req (or session-key (http/get-route-data req :session-key)))
          auth-db     (auth/db req)
          user-id     (session/user-id    smap)
          user-email  (session/user-email smap)
          form-params (get (get req :parameters) :form)
          to-change   (-> (common/pick-params form-params [:user/first-name
                                                           :user/last-name
                                                           :user/middle-name])
                          (map/update-existing :user/middle-name some-str))
          change?     (pos-int? (count to-change))
          result      (if change? (user/props-set auth-db user-id to-change))
          bad-result? (not (or change? (pos-int? (:next.jdbc/update-count result))))]
      (if bad-result?
        (api/render-error req :profile/update-error)
        (-> req
            (api/add-status (if change? :profile/updated :profile/not-updated))
            (api/add-body (common/pick-params (user/props-by-session auth-db smap)
                                              :user [:uid :first-name :last-name :middle-name]))))))))

;; Identity management

(defn identity!
  "Gets user's identities."
  ([req] (identity! req nil))
  ([req session-key]
   (api/response
    req
    (let [smap    (session/not-empty-of req (or session-key (http/get-route-data req :session-key)))
          auth-db (auth/db req)
          props   (user/props-by-session auth-db smap)]
      (if (:uid props)
        (api/add-body req (common/pick-params props :user [:uid :email :phone]))
        (api/render-error req :verify/bad-result))))))

(defn identity-edit!
  "Initiates process of changing user's identity (e-mail or phone)."
  ([req]
   (identity-edit! req nil))
  ([req session-key]
   (api/response
    req
    (let [form-params (get (get req :parameters) :form)
          to-change   (select-keys form-params [:user/email :user/phone])
          phone       (get to-change :user/phone)
          email       (get to-change :user/email)]
      (if (and phone email)
        (api/render-error req :verify/multiple-ids)
        (let [route-data    (http/get-route-data req)
              smap          (session/of req (or session-key (get route-data :session-key)))
              user-email    (session/user-email smap)
              password      (if user-email (some-str (get form-params :password)))
              form-params   (dissoc form-params :password)
              auth-result   (super/auth-user-with-password! req user-email password smap route-data true nil)
              auth-bad?     (not= :auth/ok (:response/status auth-result))
              auth-settings (if-not auth-bad? (auth/settings req))
              auth-db       (auth/db auth-settings)
              auth-failed?  (or auth-bad? (not auth-db))]
          (if auth-failed?
            (if auth-bad? auth-result (api/render-error req :verify/bad-result))
            (let [user-id      (session/user-id smap)
                  props        (user/props-by-id auth-db user-id)
                  [id-type id] (first to-change)
                  id-type      (if (= id-type :user/email) :email (if (= id-type :user/phone) :phone id-type))
                  to-change?   (some? id)
                  body         (common/pick-params props :user [:uid :email :phone])
                  req          (api/add-body req body)]
              (if (or (not to-change?)
                      (= (some-> body (get id-type) try-lower-case)
                         (some-> id try-lower-case)))
                (api/render-status req :identity/not-updated)
                (let [lang                        (common/lang-id req)
                      ^AuthConfig auth-config     (auth/config auth-settings (get props :account-type))
                      ^AuthConfirmation auth-cfrm (if auth-config (.confirmation auth-config))
                      auth-db                     (if auth-config (.db auth-config) auth-db)
                      attempts                    (if auth-cfrm   (.max-attempts auth-cfrm))
                      exp                         (if auth-cfrm   (.expires      auth-cfrm))
                      result                      (confirmation/create-for-change auth-db id user-id exp attempts id-type)]
                  (verify! req {:id               id
                                :db               auth-db
                                :lang             lang
                                :id-type          id-type
                                :reason           "change"
                                :result           result
                                :tpl/phone-exists :verify/sms-exists
                                :tpl/phone-verify :verify/sms
                                :tpl/email-exists :update/exists
                                :tpl/email-verify :update/verify})))))))))))

(defn guess-identity-type
  [db-result]
  (if db-result
    (or (get db-result :id-type)
        (common/guess-identity-type (get db-result :identity)))))

(defn identity-create!
  "Verifies confirmation code or token against a database and if it matches, creates
  new identity."
  ([req]
   (identity-create! req super/invalidate-user-sessions!))
  ([req session-invalidator]
   (api/response
    req
    (let [form-params  (get (get req :parameters) :form)
          to-change    (select-keys form-params [:user/email :user/phone])
          token        (get form-params :token)
          code         (get form-params :code)
          [id-type id] (first to-change)]
      (if-not (or token (and code id))
        (api/render-error req :parameters/error)
        (if (> (count to-change) 1)
          (api/render-error req :verify/multiple-ids)
          (let [db           (auth/db req)
                confirmation (confirmation/establish db id code token one-minute "change")
                confirmed?   (get confirmation :confirmed?)
                id-type      (or id-type (guess-identity-type confirmation))
                updated      (if confirmed? (user/update-identity-with-token-or-code id-type db id token code))
                updated?     (:updated? updated)
                bad-result?  (or (nil? confirmation) (and confirmed? (nil? updated)))]
            (cond
              bad-result?      (api/render-error req :verify/bad-result)
              updated?         (let [user-id    (get updated :id)
                                     id         (get updated :identity)
                                     route-data (http/get-route-data req)]
                                 (if session-invalidator (session-invalidator req route-data id-type id user-id))
                                 (confirmation/delete db id "change")
                                 (-> req
                                     (api/add-body {:user/uid (get updated :uid) id-type id})
                                     (api/render-status :identity/created)))
              (not confirmed?) (api/render-error req (:errors confirmation))
              (not updated?)   (api/render-error req (:errors updated))
              :error!          (api/render-error req (or (not-empty (:errors confirmation))
                                                         (not-empty (:errors updated))))))))))))

;; User creation

(defn create!
  "Verifies confirmation code or token against a database, and if it matches, creates a
  new user."
  [req]
  (api/response
   req
   (let [db         (auth/db req)
         all-params (get req :parameters)
         params     (get all-params :form)
         code       (get params :code)
         token      (get params :token)
         login      (or (get params :user/email) (get params :login) (get params :email))]
     (if-not (or token (and code login))
       (api/render-error req :parameters/error)
       (let [confirmation (confirmation/establish db login code token one-minute "creation")
             confirmed?   (get confirmation :confirmed?)
             creation     (if confirmed? (user/create-with-token-or-code db login token code))
             created?     (if creation (get creation :created?))
             bad-result?  (or (nil? confirmation) (and confirmed? (nil? creation)))]
         (cond
           bad-result?      (api/render-error req :verify/bad-result)
           created?         (let [login (or (some-str login) (get creation :email))
                                  uid   (get creation :uid)]
                              (confirmation/delete db login)
                              (-> req
                                  (api/add-body {:user/email login :user/uid uid})
                                  (api/add-status :user/created)))
           (not confirmed?) (api/render-error req (:errors confirmation))
           (not created?)   (api/render-error req (:errors creation))
           :error!          (api/render-error req (or (not-empty (:errors confirmation))
                                                      (not-empty (:errors creation))))))))))

;; Password setting

(defn password-change!
  "Changes a password for the user authenticated with an old password and a session
  token."
  ([req]
   (password-change! req nil super/invalidate-user-sessions!))
  ([req session-key]
   (password-change! req session-key super/invalidate-user-sessions!))
  ([req session-key session-invalidator]
   (api/response
    req
    (let [form-params  (get (get req :parameters) :form)
          old-password (some-str (get form-params :password))
          new-password (get form-params :user/password)
          route-data   (http/get-route-data req)
          session-key  (or session-key (get route-data :session-key))
          session      (session/valid-of req session-key)
          user-email   (session/user-email session)
          req          (super/auth-user-with-password! req user-email old-password nil route-data true nil)]
      (if (= :auth/ok (get req :response/status))
        (let [user-id (or (get req :user/id)
                          (session/user-email session)
                          (user/email-to-id (auth/db req) user-email))
              req     (super/set-password! req user-id new-password)]
          (if (and session-invalidator (= :pwd/created (get req :response/status)))
            (session-invalidator req nil :user/email user-email user-id))
          req)
        req)))))

(defn password-recover!
  "Sets password for a user authenticated with the given recovery key or code and
  identity (an e-mail or a phone number)."
  ([req]
   (password-recover! req super/invalidate-user-sessions!))
  ([req session-invalidator]
   (api/response
    req
    (let [form-params  (get (get req :parameters) :form)
          to-change    (select-keys form-params [:user/email :user/phone])
          new-password (get form-params :user/password)
          token        (get form-params :token)
          code         (get form-params :code)
          [id-type id] (first to-change)
          id-type      (if (= id-type :user/email) :email (if (= id-type :user/phone) :phone id-type))]
      (if (and token code)
        (api/render-error req :parameters/error)
        (let [db           (auth/db req)
              confirmation (confirmation/establish db id code token one-minute "recovery")
              confirmed?   (boolean (if confirmation (get confirmation :confirmed?)))
              user-id      (if confirmation (get confirmation :user/id))
              req          (if confirmed? (super/set-password! req user-id new-password) req)
              updated?     (if confirmed? (= (get req :response/status) :pwd/created))]
          (cond
            (nil? confirmation) (api/render-error req :verify/bad-result)
            updated?            (do (if session-invalidator
                                      (session-invalidator req nil :user/email (user/id-to-email db user-id) user-id))
                                    (confirmation/delete db id "recovery")
                                    req)
            (not confirmed?)    (api/render-error req (:errors confirmation))
            (not updated?)      req
            :error!             (api/render-error req (not-empty (:errors confirmation))))))))))

(defn recovery-create!
  [req]
  "Generates password recovery link and code, and sends it to a user via e-mail or
  SMS."
  (api/response
   req
   (let [form-params (get (get req :parameters) :form)
         password    (some-str (get form-params :password))
         channel     (select-keys form-params [:user/email :user/phone])
         phone       (get channel :user/phone)
         email       (get channel :user/email)
         phone?      (some? phone)]
     (if (and email phone?)
       (api/render-error req :verify/multiple-ids)
       (let [auth-settings               (auth/settings req)
             auth-db                     (auth/db auth-settings)
             [id props id-type]          (if phone?
                                           [phone (user/props-by-phone auth-db phone) :phone]
                                           [email (user/props-by-email auth-db email) :email])
             user-id                     (get props :id)
             ^AuthConfig auth-config     (auth/config auth-settings (get props :account-type))
             ^AuthConfig auth-config     (or auth-config (auth/config auth-settings))
             ^AuthConfirmation auth-cfrm (if auth-config (.confirmation auth-config))
             auth-db                     (if auth-config (.db auth-config) auth-db)
             attempts                    (if auth-cfrm   (.max-attempts auth-cfrm))
             exp                         (if auth-cfrm   (.expires      auth-cfrm))
             result                      (confirmation/create-for-recovery auth-db id user-id exp attempts id-type)]
         (verify! req {:id               id
                       :db               auth-db
                       :lang             (common/lang-id req)
                       :id-type          id-type
                       :reason           "recovery"
                       :result           result
                       :tpl/phone-exists :verify/sms-recovery
                       :tpl/email-exists :recovery/verify}))))))

(defn password-create!
  "Sets new user password using a valid session and a current password OR recovery
  token or code and identity."
  ([req]
   (password-create! req nil super/invalidate-user-sessions!))
  ([req session-key]
   (password-create! req session-key super/invalidate-user-sessions!))
  ([req session-key session-invalidator]
   (let [form-params (get (get req :parameters) :form)]
     (if (or (some? (get form-params :token))
             (some? (get form-params :code)))
       (password-recover! req session-invalidator)
       (password-change!  req session-key session-invalidator)))))
