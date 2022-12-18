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

(def one-minute (t/new-duration 1 :minutes))

;; Helpers

(defn- try-lower-case
  [v]
  (if (string? v) (str/lower-case v) v))

(defn retry-after
  "Returns an expiration date and time formatted according to the RFC 1123."
  [expires]
  (common/rfc1123-date-time expires))

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
   (let [req (super/auth-user-with-password! req user-email password sess route-data auth-only-mode)]
     (if (api/response? req)
       req
       (let [lang        (or lang (common/pick-language req))
             tr-sub      (i18n/no-default (common/translator-sub req lang))
             session-key (session/session-key sess)]
         (-> req
             (language/force lang)
             (api/body-add-session-status session-key tr-sub)))))))

;; Controllers

(defn authenticate!
  "Logs user in when user e-mail and password are given, or checks if the session is
  valid to serve a current page.

  Takes a request map and obtains database connection, client IP address and
  authentication configuration from it. Also gets a user e-mail and a password from a
  map associated with the `:form-params` key of the `req`. Calls
  `auth-with-password!` to get the result or a redirect if authentication was not
  successful.

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
    (let [form-params    (get req :form-params)
          user-email     (some-str (get form-params "login"))
          password       (if user-email (some-str (get form-params "password")))
          route-data     (delay (http/get-route-data req))
          session-key    (or session-key (get @route-data :session-key))
          sess           (session/of req session-key)
          lang           (delay (common/pick-language req :user))
          valid-session? (delay (session/valid? sess))]
      (cond
        password          (auth-with-password! req user-email password sess @route-data @lang false)
        @valid-session?   req
        :invalid-session! (api/move-to req (or (get @route-data :auth/info) :auth/info) @lang))))))

(defn authenticate-only!
  "Logs user in when user e-mail and password are given.

  Takes a request map and obtains a database connection, client IP address and
  authentication configuration from it. Also gets user's e-mail and a password from a
  map associated with the `:form-params` key of the `req`. Calls
  `amelinium.common.controller/auth-with-password!` to get the authentication result
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

(defmacro id-type->url-type
  [id-type reason]
  `(case ~id-type
     :user/email (case ~reason ("change" :change) :url/update-email :url/create)
     :user/phone :url/update-phone
     :url/update-email))

(defn verify!
  "Performs the identity confirmation verification by sending an e-mail or SMS with a
  URL to complete account registration or identity change."
  [req {:keys [no-data result reason db id id-type lang translator route-data]
        :as   opts}]
  (let [lang              (or lang       (common/pick-language req :registration) (common/lang-id req))
        tr                (or translator (i18n/no-default (common/translator req lang)))
        id-type           (or id-type    :user/email)
        {:keys [confirmed?
                errors
                attempts
                expires]} result
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
                          existing-uid      (get result :user/uid)
                          uid               (if exists? (some-str existing-uid))
                          lang-qs           (common/query-string-encode {"lang" lang-str})
                          url-type          (id-type->url-type id-type reason)
                          verify-link       (str (get rdata url-type) token "/?" lang-qs)
                          recovery-link     (if uid (str (get rdata :url/recover) uid "/?" lang-qs))
                          req-updater       (get opts :async/responder verify-request-id-update)
                          exc-handler       (get opts :async/raiser verify-process-error)
                          req-updater       #(req-updater db id-type id code token %)
                          exc-handler       #(exc-handler db id-type id code token %)
                          add-retry-fields  (fn [req]
                                              (api/assoc-body
                                               req
                                               :verify/retry-in      @retry-in
                                               :verify/retry-unit    :minutes
                                               :verify/retry-dur     @retry-dur
                                               :verify/attempts-left attempts-left))
                          template-params   {:serviceName      (tr :verify/app-name)
                                             :expiresInMinutes (tr :in-mins @retry-in)
                                             :remoteAddress    remote-ip
                                             :verifyCode       (str code)
                                             :verifyLink       verify-link
                                             :recoveryLink     recovery-link}]
                      (case id-type
                        :user/email (twilio/sendmail-l10n-template-async
                                     (get rdata :twilio/email)
                                     req-updater exc-handler
                                     lang id
                                     (if exists?
                                       (get opts :tpl/email-exists :registration/exists)
                                       (get opts :tpl/email-verify :registration/verify))
                                     template-params)
                        :user/phone (let [smst (if exists?
                                                 (get opts :tpl/phone-exists :verify/sms-exists)
                                                 (get opts :tpl/phone-verify :verify/sms))
                                          smsb (tr smst template-params)]
                                      (twilio/sendsms-async
                                       (get rdata :twilio/sms)
                                       req-updater exc-handler
                                       id smsb)))
                      (-> req
                          (api/add-status :verify/sent)
                          (add-retry-fields))))))

(defn resend!
  "Re-sends verification e-mail or SMS to confirm the given identity."
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
       (let [db          (.db udata)
             reason      (or (some-str reason) "creation")
             [id id-type
              no-data f] (if phone
                           [phone :user/phone :verify/bad-phone confirmation/retry-phone]
                           [email :user/email :verify/bad-email confirmation/retry-email])
             result      (f db id reason)]
         (verify! req {:db      db
                       :id      id
                       :lang    (common/lang-id req)
                       :id-type id-type
                       :no-data no-data
                       :reason  reason
                       :result  result}))))))

;; Registration

(defn register!
  [req]
  (api/response
   req
   (let [params                      (get (get req :parameters) :form)
         ^AuthSettings auth-settings (auth/settings req)
         ^UserData     udata         (user/make-user-data auth-settings params)
         result                      (confirmation/create-for-registration (.db udata) udata)]
     (verify! req {:id      (.email udata)
                   :id-type :user/email
                   :db      (.db udata)
                   :result  result}))))

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
              auth-result   (super/auth-user-with-password! req user-email password smap route-data nil true)
              auth-bad?     (not= :auth/ok (:response/status auth-result))
              auth-settings (if-not auth-bad? (auth/settings req))
              auth-db       (auth/db auth-settings)
              auth-failed?  (or auth-bad? (not auth-db))]
          (if auth-failed?
            (if auth-bad? auth-result (api/render-error req :verify/bad-result))
            (let [user-id      (session/user-id smap)
                  props        (user/props-by-id auth-db user-id)
                  [id-type id] (first to-change)
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
          (let [auth-config  (auth/config req)
                db           (auth/db auth-config)
                confirmation (confirmation/establish db id code token one-minute "change")
                confirmed?   (get confirmation :confirmed?)
                id-type      (or id-type (guess-identity-type confirmation))
                updated      (if confirmed? (user/update-identity-with-token-or-code id-type db id token code))
                updated?     (:updated? updated)
                bad-result?  (or (nil? confirmation) (and confirmed? (nil? updated)))]
            (println "updated" updated)
            (println "id-type" id-type)
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
   (let [auth-config (auth/config req)
         db          (auth/db auth-config)
         all-params  (get req :parameters)
         params      (get all-params :form)
         code        (get params :code)
         token       (get params :token)
         login       (or (get params :user/email) (get params :login) (get params :email))]
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
