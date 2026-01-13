(ns

    ^{:doc    "amelinium service, web user controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.controller.user

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [tick.core                          :as                      t]
            [clojure.string                     :as                    str]
            [amelinium.http.response            :as                   resp]
            [amelinium.logging                  :as                    log]
            [amelinium.i18n                     :as                   i18n]
            [amelinium.common                   :as                 common]
            [amelinium.common.controller        :as                  super]
            [amelinium.web                      :as                    web]
            [amelinium.web.htmx                 :as                   htmx]
            [amelinium.web.app-data             :as               app-data]
            [amelinium.auth                     :as                   auth]
            [amelinium.http                     :as                   http]
            [amelinium.identity                 :as               identity]
            [amelinium.messaging                :as                    msg]
            [amelinium.http.middleware.session  :as                session]
            [amelinium.http.middleware.language :as               language]
            [amelinium.model.user               :as                   user]
            [amelinium.model.confirmation       :as           confirmation]
            [amelinium.types.session]
            [amelinium.utils                    :refer [simple-duration
                                                        timeout?
                                                        retry-in-mins
                                                        rfc1123-date-time]]
            [io.randomseed.utils.map            :refer [qassoc]]
            [io.randomseed.utils                :refer [qsome
                                                        some-str
                                                        some-keyword]]
            [zprint.core                        :refer [czprint]])

  (:import (amelinium Session
                      UserData)))

(def one-minute   (t/new-duration 1 :minutes))
(def five-minutes (t/new-duration 5 :minutes))

(defn retry-after
  "Returns an expiration date and time formatted according to the RFC 1123."
  [expires]
  (rfc1123-date-time expires))

(defn verify!
  "Performs the identity verification by sending an e-mail or SMS with a URL to
  complete confirmation. Takes a request map `req` and configuration options as a
  map with the following keys:

  `:db`              - database connection handler or a data source object,
  `:id`              - identity used for verification (e-mail address or a phone number),
  `:id-type`         - identity type (`:email` or `:phone`),
  `:lang`            - language (defaults to a result of `common/pick-language`),
  `:reason`          – verification reason (`:creation`, `:recovery`, `:change`, `:unlock`),
  `:no-data`         - status to be set when there is no result from a database (default: `:verify/bad-result`),
  `:result`          - a result of calling confirmation function from a model,
  `:translator`      - translation function (defaults to a result of `amelinium.common/translator`),
  `:route/data`      - route data (defaults to a result of calling `amelinium.http/get-route-data`),
  `:confirm-once?`   - if truthy (default) then existing confirmation will cause error response."
  [req {:keys      [no-data result reason db id id-type confirm-once?
                    lang translator]
        route-data :route/data
        :or        {confirm-once? true}
        :as        opts}]
  (let [lang              (or lang       (common/pick-language req :registration) (common/lang-id req))
        tr                (or translator (i18n/no-default (common/translator req lang)))
        {:keys [confirmed?
                errors
                attempts
                expires]} result
        id-type           (common/guess-identity-type result id id-type)
        errors?           (some? (seq errors))
        attempts?         (and (not errors?) (int? attempts))
        attempts-left     (when attempts? (if (neg? attempts) 0 attempts))
        max-attempts?     (when attempts? (zero? attempts-left))
        bad-result?       (not (or errors? attempts?))
        retry-dur         (delay (simple-duration expires))
        expired?          (delay (timeout? @retry-dur))
        retry-in          (delay (retry-in-mins @retry-dur))
        in-mins           (delay (tr :in-mins     @retry-in))
        retry-in-mins     (delay (tr :try-in-mins @retry-in))
        mins-left         (delay (tr :mins-left   @retry-in))
        attempts-left-w   (delay (tr :attempts-left attempts-left))
        confirmed?        (and confirmed? (boolean confirm-once?))]
    (cond
      bad-result?   (web/render-error  req (or no-data :verify/bad-result))
      errors?       (web/render-error  req errors)
      confirmed?    (web/render-status req :verify/confirmed)
      max-attempts? (-> req
                        (app-data/assoc :verify/retry-in           retry-in
                                        :verify/in-mins            retry-in-mins
                                        :verify/retry-unit         :minutes
                                        :verify/retry-dur          retry-dur
                                        :verify/mins-left          mins-left
                                        :verify/max-attempts?      max-attempts?
                                        :verify/expired?           expired?
                                        :verify/attempts-left      attempts-left
                                        :verify/attempts-left-word attempts-left-w
                                        :sub-status/description    retry-in-mins)
                        (web/add-header :Retry-After (retry-after expires))
                        (web/add-status :verify/max-attempts))
      :else         (let [{:keys [token code
                                  exists?]} result
                          lang-str          (some-str lang)
                          remote-ip         (get req :remote-ip/str)
                          rdata             (or route-data (http/get-route-data req))
                          existing-uid      (when exists? (some-str (get result :existing-user/uid)))
                          existing-user-id  (when exists? (get result :existing-user/id))
                          lang-qs           (common/query-string-encode req {"lang" lang-str})
                          url-type          (common/id-type->url-type id-type reason)
                          verify-link       (str (get rdata url-type) token "/?" lang-qs)
                          recovery-link     (when existing-uid (str (get rdata :url/recover) existing-uid "/?" lang-qs))
                          id-str            (identity/->str id-type id)
                          async-opts        {:on-ok  #(confirmation/update-request-id db id-str code token (:provider/msg-id %))
                                             :on-err #(czprint (:provider/exception %))} ;; fixme: stub
                          email?            (identical? :email id-type)
                          phone?            (and (not email?) (identical? :phone id-type))
                          user-login        (if email? id-str (when existing-user-id (delay (user/email db :id existing-user-id))))
                          qtoken            (delay (confirmation/make-qtoken-all id-str token))
                          template-params   {:serviceName      (tr :verify/app-name)
                                             :expiresInMinutes @in-mins
                                             :remoteAddress    remote-ip
                                             :verifyCode       (str code)
                                             :verifyLink       verify-link
                                             :recoveryLink     recovery-link}]
                      (condp identical? id-type

                        :email
                        (if-some [tkey ((if exists? :tpl/email-exists :tpl/email-verify) opts)]
                          (msg/send-async! (get rdata :amelinium.messaging/email)
                                           {:to            id-str
                                            :lang          lang
                                            :template/key  tkey
                                            :template/data template-params}
                                           async-opts)
                          (log/web-wrn req "No template, not sending e-mail to:" id-str))

                        :phone
                        (if-some [tkey ((if exists? :tpl/phone-exists :tpl/phone-verify) opts)]
                          (msg/send-async! (get rdata :amelinium.messaging/sms)
                                           {:to              id-str
                                            :lang            lang
                                            :translation/key tkey
                                            :body            (tr tkey template-params)}
                                           async-opts)
                          (log/web-wrn req "No template, not sending SMS to:" id-str))

                        (log/web-wrn req "Unknown identity type:" id-type))
                      (-> req
                          (web/add-status :verify/sent)
                          (app-data/assoc
                           :user/identity             id-str
                           :user/phone                (when phone? id-str)
                           :user/email                (when email? id-str)
                           :user/login                user-login
                           :identity/phone?           phone?
                           :identity/email?           email?
                           :identity/type             id-type
                           :verify/expired?           expired?
                           :verify/retry-in           retry-in
                           :verify/in-mins            in-mins
                           :verify/retry-in-mins      retry-in-mins
                           :verify/retry-unit         :minutes
                           :verify/retry-dur          retry-dur
                           :verify/mins-left          mins-left
                           :verify/max-attempts?      max-attempts?
                           :verify/attempts-left      attempts-left
                           :verify/attempts-left-word attempts-left-w
                           :verify/qtoken             qtoken))))))

(defn- auth-ok
  [req _route-data lang]
  (if-some [goto-uri (get req :auth/goto)]
    (resp/temporary-redirect goto-uri)
    (language/force req (or lang (web/pick-language-str req)))))

(defn- auth-prolonged-ok
  [req route-data lang]
  (if (htmx/use? req route-data :auth/use-htmx?)
    (htmx/go-to-with-status req route-data :auth/prolonged-ok :login/prolonged)
    (auth-ok req route-data lang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special actions (controller handlers)

(defn auth-with-password!
  "Authentication helper. Used by other controllers. Short-circuits on certain
  conditions and may emit a redirect or render a response. Uses
  `amelinium.web/handle-error`."
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
         req        (super/auth-user-with-password! req user-email password sess route-data auth-only? session-key)
         status     (get req :app/status)]
     (if (resp/response? req)
       req
       (condp identical? status
         :auth/ok            (if auth-only? req (auth-ok req route-data lang))
         :auth/prolonged-ok  (if auth-only? req (auth-prolonged-ok req route-data lang))
         :auth/locked        (web/handle-error req route-data status :login/account-locked      :Authentication-Error)
         :auth/soft-locked   (web/handle-error req route-data status :login/account-soft-locked :Authentication-Error)
         :auth/bad-password  (web/handle-error req route-data status :login/bad-password        :Authentication-Error)
         :auth/session-error (web/handle-error req route-data status :login/session-error       :Authentication-Error)
         (web/handle-error req route-data (or status :auth/error)    :login/error               :Authentication-Error))))))

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
   (let [form-params        (common/get-form-params req)
         route-data         (delay (http/get-route-data req))
         ^String user-email (some-str (or (get form-params :user/login) (get form-params :login)))
         ^String password   (when user-email (some-str (or (get form-params :user/password) (get form-params :password))))
         ^Session sess      (session/of req (or session-key (get @route-data :session-key)))
         lang               (delay (web/pick-language-str req :user))]
     (cond
       password              (auth-with-password! req user-email password sess @route-data @lang false session-key)
       (session/valid? sess) (if (some? (language/from-path req))
                               ;; Render the contents in a language specified by the current path.
                               req
                               ;; Redirect to a proper language version of this very page.
                               (web/move-to req (or (get @route-data :name) (get req :uri)) @lang))
       :else                 (web/move-to req (or (get @route-data :auth/login) :auth/login) @lang)))))

(defn login!
  "Prepares response data to be displayed on a login page."
  ([req] (login! req nil))
  ([req session-key]
   (let [^Session sess (session/of req session-key)
         rem-mins      (delay (super/lock-remaining-mins req (auth/db req) sess t/now))
         user-login    (delay (some-str (common/get-form-params req :user/login)))]
     (app-data/assoc req :lock-remains rem-mins :user/login user-login))))

(defn logout!
  "Logs user out."
  ([req]
   (logout! req))
  ([req session-key]
   (let [route-data  (http/get-route-data req)
         session-key (or session-key (get route-data :session-key))
         sess        (session/of req session-key)
         req         (session/empty-session-id-header req sess)]
     (session/delete! sess)
     (if-some [dst (get route-data :destination)]
       (web/go-to req dst)
       req))))

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
             (app-data/assoc :lock-remains rem-mins)))

       (and sess (session/hard-expired? sess))
       (web/move-to req (get-in route-data [:auth-error/destinations :auth/session-expired] :login/session-expired))

       :else
       (web/move-to req (get-in route-data
                                [:auth-error/destinations :auth/session-error]
                                :login/session-error))))))

(defn confirmation-status!
  "Renders status of identity confirmation on a basis of user's identity and quick
  token obtained from form params. Identity type must belong to a group
  `:amelinium.identity/public`."
  ([req]             (confirmation-status! req nil nil))
  ([req session-key] (confirmation-status! req session-key nil))
  ([req _session-key reason]
   (if-some [params (not-empty (common/get-form-params req))]
     (let [id     (common/get-identity-param params)
           qtoken (->> [:verify/qtoken :confirmation/qtoken :qtoken] (qsome params))]
       (if (and id qtoken)
         (if-some [r (confirmation/status (auth/db req) id qtoken reason)]
           (let [lang           (or (common/pick-language req :registration) (common/lang-id req))
                 tr             (i18n/no-default (common/translator req lang))
                 retry-dur      (simple-duration (get r :expires))
                 attempts       (get r :attempts)
                 attempts?      (int? attempts)
                 attempts-left  (when attempts? (if (neg? attempts) 0 attempts))
                 max-attempts?  (when attempts? (zero? attempts-left))
                 expired?       (timeout? retry-dur)
                 reason         (get r :reason)
                 reason-str     (delay (some->> reason name))
                 start-over-url (delay (when reason (common/route-data-page-in req [:reason/returns reason])))
                 retry-in       (delay (retry-in-mins retry-dur))
                 id-type        (delay (common/guess-identity-type r id nil nil))
                 start-over?    (or expired? false)
                 req            (app-data/assoc
                                 req
                                 :url/start-over            (when start-over? start-over-url)
                                 :user/id-type              id-type
                                 :user/identity             (delay (identity/->str @id-type id))
                                 :verify/confirmed?         (delay (get r :confirmed?))
                                 :verify/in-mins            (delay (tr :in-mins     @retry-in))
                                 :verify/retry-in-mins      (delay (tr :try-in-mins @retry-in))
                                 :verify/mins-left          (delay (tr :mins-left   @retry-in))
                                 :verify/attempts-left-word (delay (tr :attempts-left attempts-left))
                                 :verify/attempts-left      attempts-left
                                 :verify/retry-in           retry-in
                                 :verify/retry-dur          retry-dur
                                 :verify/max-attempts?      max-attempts?
                                 :verify/expired?           expired?
                                 :verify/qtoken             qtoken
                                 :verify/retry-unit         :minutes)]
             (cond
               expired? (web/handle-error req (when @reason-str (keyword "timeout" @reason-str)))
               :else    req))
           req)
         req))
     req)))

(defn register!
  "Initiates user registration process by receiving e-mail, password and name."
  ([req] (register! req nil))
  ([req _session-key]
   (let [params       (not-empty (common/get-form-params req))
         new-password (when params (get params :user/new-password))]
     (if new-password
       (if (not= (:user/new-password params) (:user/repeated-password params))
         (web/form-params-error! req {:user/repeated-password :repeated-password})
         (let [route-data          (http/get-route-data req)
               [fname sname]       (str/split (or (get params :user/first-name) "") #"\\s+")
               params              (if sname (qassoc params
                                                     :user/first-name  fname
                                                     :user/second-name sname) params)
               auth-settings       (auth/settings req)
               ^UserData     udata (user/make-user-data auth-settings params)
               db                  (when udata (.db udata))
               result              (confirmation/create-for-registration db udata)
               req                 (verify! req {:db               db
                                                 :id               (when udata (.email udata))
                                                 :id-type          :email
                                                 :result           result
                                                 :route/data       route-data
                                                 :tpl/email-exists :registration/exists
                                                 :tpl/email-verify :registration/verify
                                                 :tpl/phone-exists :verify/sms-exists
                                                 :tpl/phone-verify :verify/sms})]
           (web/response
            ;; verify! returned error
            req
            ;; verify! performed registration initiation
            (qassoc req :app/view (get route-data :register-sent/view :user/register-sent)))))
       req))))

(defn create!
  "Verifies confirmation token or code against a database and if it matches creates an
  account."
  [req]
  (let [auth-config  (auth/config req)
        db           (auth/db auth-config)
        all-params   (common/get-params req)
        path-params  (get all-params :path)
        params       (get all-params :form)
        token        (get path-params :token)
        code         (or (get params :confirmation/code) (get params :code))
        login        (or (get params :user/login) (get params :login))
        confirmation (confirmation/establish db login code token one-minute "creation")
        confirmed?   (get confirmation :confirmed?)
        creation     (when confirmed? (user/create db login token code))
        created?     (when creation (get creation :created?))
        bad-result?  (or (nil? confirmation) (and confirmed? (nil? creation)))]
    (cond
      bad-result?      (web/render-error req :verify/bad-result)
      created?         (let [mobile-agent? (common/mobile-agent? req)
                             app-uri       (http/get-route-data req :app.url/login)
                             login         (or (some-str login) (get creation :email))
                             qs            (common/query-string-encode req {"login" login})
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
      :else            (web/render-error req (or (not-empty (:errors confirmation))
                                                 (not-empty (:errors creation)))))))

;; TODO: invalidate all user's sessions after password recovery

(defn identity-update!
  "Initiates identity change for logged-in user by displaying a form and handles
  new identity confirmation with a code or token."
  ([req]
   (identity-update! req nil))
  ([req session-key]
   (let [route-data (delay (http/get-route-data req))
         sess       (session/of req (or session-key (get @route-data :session-key)))
         params     (common/get-form-params req)
         id         (common/get-identity-param params)
         id-type    (identity/type id)
         req        (app-data/assoc req :user/identity id :identity/type id-type)]
     (if id-type

       ;; initiate change
       ;; by generating a verification code and token
       ;; associated with the given identity
       ;; and sending e-mail or SMS message

       (let [user-auth (user/auth-config req id id-type)
             user-id   (when user-auth (get (get user-auth :user/properties) :id))
             auth-cfrm (when user-auth (get user-auth :confirmation))
             auth-db   (when user-auth (get user-auth :db))
             attempts  (when auth-cfrm (get auth-cfrm :max-attempts))
             exp       (when auth-cfrm (get auth-cfrm :expires))
             result    (confirmation/create-for-recovery auth-db id user-id exp attempts id-type)
             req       (verify! req {:id               id
                                     :db               auth-db
                                     :lang             (common/lang-id req)
                                     :id-type          id-type
                                     :reason           "recovery"
                                     :result           result
                                     :confirm-once?    false
                                     :tpl/phone-exists :verify/sms-recovery
                                     :tpl/email-exists :recovery/verify})]
         (web/response
          ;; verify! returned an error response, short-circuit
          req

          ;; display a form for entering verification code
          (qassoc req :app/view :user/password-recover-sent)))

       ;; display initial identity update form
       req))))

(defn identity-create!
  "Verifies confirmation token against a database and if it matches, updates the
  identity (phone or e-mail)."
  ([req]
   (identity-create! req super/invalidate-user-sessions!))
  ([req session-invalidator]
   (let [auth-config  (auth/config req)
         db           (auth/db auth-config)
         all-params   (common/get-params req)
         path-params  (get all-params  :path)
         form-params  (get all-params  :form)
         code         (or (get form-params :confirmation/code)  (get form-params :code))
         token        (some-str     (get path-params :token))
         id-type      (some-keyword (get path-params :id-type))
         id           (get form-params id-type)
         [id id-type] (common/identity-and-type id id-type)
         confirmation (confirmation/establish db id code token one-minute "change")
         confirmed?   (get confirmation :confirmed?)
         updated      (when confirmed? (user/update-identity id-type db token code id))
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
                              qs            (common/query-string-encode req {"login" login})
                              destination   (str app-uri "?" qs)]
                          (when session-invalidator (session-invalidator req route-data id-type id user-id))
                          (confirmation/delete db id "change")
                          (web/render-status req :identity/created nil
                                             {:app.url            app-uri
                                              :app.url/login      destination
                                              :confirmation/token token
                                              :confirmation/code  code
                                              :identity/email?    (identical? id-type :email)
                                              :identity/phone?    (identical? id-type :phone)
                                              :identity/type      id-type
                                              :user/identity      (identity/->str id-type id)
                                              :user/login         login
                                              :user/email         (identity/->str :email login)
                                              :user/phone         (identity/->str :phone phone)
                                              :agent/mobile?      mobile-agent?}))
       (not confirmed?) (web/render-error req (:errors confirmation))
       (not updated?)   (web/render-error req (:errors updated))
       :else            (web/render-error req (or (not-empty (:errors confirmation))
                                                  (not-empty (:errors updated))))))))

;; Password setting

(defn- pwd-status
  ([req]
   (pwd-status req (http/get-route-data req)))
  ([req route-data]
   (condp identical? (get req :app/status)
     :pwd/created      req
     :pwd/updated      req
     :pwd/bad-password (common/move-to req (get-in route-data [:error/destinations :auth/bad-password] :login/bad-password))
     :pwd/bad-user     (common/move-to req (get-in route-data [:error/destinations :auth/bad-password] :login/bad-password))
     (web/go-to req (get-in route-data [:error/destinations :auth/error] :login/error)))))

(defn password-change!
  "Changes password for the user authenticated with an old password and e-mail or sets
  the password for the given `user-id`."
  ([req]
   (password-change! req super/invalidate-user-sessions!))
  ([req session-invalidator]
   (let [form-params  (common/get-form-params req)
         user-email   (some-str (or (get form-params :user/login)   (get form-params :login) (get form-params :username)))
         new-password (or (get form-params :user/new-password)      (get form-params :new-password))
         new-repeated (or (get form-params :user/repeated-password) (get form-params :repeated-password))
         old-password (when user-email (some-str (or (get form-params :user/password) (get form-params :password))))
         route-data   (http/get-route-data req)
         req          (auth-with-password! req user-email old-password nil route-data nil true nil)]
     (if (resp/response? req)
       req
       (if (identical? :auth/ok (get req :app/status))
         (if-not (= new-password new-repeated)
           (web/form-params-error! req {:user/repeated-password :repeated-password})
           (let [user-id (or (get req :user/id) (user/id (auth/db req) :email user-email))
                 req     (super/set-password! req user-id new-password)]
             (if (and session-invalidator (identical? :pwd/created (get req :app/status)))
               (session-invalidator req route-data :email user-email user-id)
               req)))
         req)))))

(defn password-recover!
  "Initiates password recovery by sending an e-mail or SMS message with a verification
  code or token."
  [req]
  (let [params  (common/get-form-params req)
        id      (common/get-identity-param params)
        id-type (identity/type id)
        req     (app-data/assoc req :user/identity id :identity/type id-type)]
    (if id-type

      ;; initiate recovery
      ;; by generating a verification code and token
      ;; associated with the given identity
      ;; and sending e-mail or SMS message

      (let [user-auth (user/auth-config req id id-type)
            user-id   (when user-auth (get (get user-auth :user/properties) :id))
            auth-cfrm (when user-auth (get user-auth :confirmation))
            auth-db   (when user-auth (get user-auth :db))
            attempts  (when auth-cfrm (get auth-cfrm :max-attempts))
            exp       (when auth-cfrm (get auth-cfrm :expires))
            result    (confirmation/create-for-recovery auth-db id user-id exp attempts id-type)
            req       (verify! req {:id               id
                                    :db               auth-db
                                    :lang             (common/lang-id req)
                                    :id-type          id-type
                                    :reason           "recovery"
                                    :result           result
                                    :confirm-once?    false
                                    :tpl/phone-exists :verify/sms-recovery
                                    :tpl/email-exists :recovery/verify})]
        (web/response
         ;; verify! returned an error response, short-circuit
         req

         ;; display a form for entering verification code
         (qassoc req :app/view :user/password-recover-sent)))

      ;; display initial password recovery form
      req)))

(defn password-update!
  "Displays password setting form for a user authenticated with token or code."
  [req]
  (let [params        (common/get-params req)
        form-params   (get params :form)
        query-params  (get params :query)
        path-params   (get params :path)
        token         (or (get path-params :token)
                          (get form-params :confirmation/token)
                          (get form-params :token))
        code          (or (get form-params :confirmation/code)
                          (get form-params :code))
        id            (common/get-identity-param form-params)
        id-type       (identity/type id)
        password      (some-str (or (get form-params :user/new-password)
                                    (get form-params :new-password)))
        password-2    (some-str (or (get form-params :user/repeated-password)
                                    (get form-params :repeated-password)))
        set-password? (some? (or password password-2))]
    (cond

      ;; password present, token or code received
      ;; validates confirmation and creates a new password

      (and set-password? (some? (or token (code id))))
      (if-not (= password password-2)
        (web/form-params-error! req {:user/repeated-password :repeated-password})
        (let [db   (auth/db req)
              cfrm (confirmation/establish db id code token one-minute "recovery")]
          (if (get cfrm :confirmed?)
            (let [req (super/set-password! req (get cfrm :user/id) password)]
              (when (resp/app-status? req :pwd/created)
                (log/web-dbg req "Removing confirmation entry for updated password")
                (confirmation/delete db (or cfrm id) "recovery"))
              req)
            (web/render-error req (or (:errors cfrm) :verify/bad-result)))))

      ;; password not present, token or code received
      ;;
      ;; Validates confirmation and displays a form for creating new password; token
      ;; is extracted from a database response to use it later instead of a code.
      ;;
      ;; If mobile is detected and mobile application URL is defined,
      ;; renders a link or button to go back to the app and enter password there

      (and (not set-password?) (or token (and code id)))
      (let [db   (auth/db req)
            cfrm (confirmation/establish db id code token five-minutes "recovery")]
        (if-not (get cfrm :confirmed?)
          (web/handle-error req (or (:errors cfrm) :verify/bad-result))
          (let [id         (identity/of-type ::identity/public (get cfrm :identity))
                id-type    (identity/type id)
                phone?     (identical? id-type :phone)
                email?     (identical? id-type :email)
                user-id    (get cfrm :user/id)
                token      (some-str (or token (get cfrm :token)))
                user-email (delay (some-str (user/email db :id user-id)))
                user-phone (delay (identity/->str (user/phone db :id user-id)))
                mobile?    (delay (common/mobile-agent? req))
                app-url    (delay (when @mobile? (http/get-route-data req :app.url/recover)))
                app-link   (delay (when @app-url (str app-url "?"
                                                      (common/query-string-encode
                                                       req
                                                       {"login" user-email
                                                        "token" token}))))]
            (app-data/assoc
             req
             :user/login              user-email
             :user/email              user-email
             :user/phone              user-phone
             :user/identity           (identity/->str id-type id)
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

(defn password-create!
  "Creates a password on a basis of a token, or on a basis of a code and identity."
  [req]
  (let [params       (common/get-params req)
        form-params  (get params :form)
        query-params (get params :query)
        path-params  (get params :path)
        token        (or (get form-params :confirmation/token)
                         (get path-params :token)
                         (get form-params :token))
        code         (or (get form-params :confirmation/code)
                         (get form-params :code))
        password     (some-str (or (get form-params :user/new-password)
                                   (get form-params :new-password)))
        password-2   (some-str (or (get form-params :user/repeated-password)
                                   (get form-params :repeated-password)))
        id           (common/get-identity-param form-params)
        id-type      (identity/type id)
        login        (delay (or (get form-params :user/login)
                                (get form-params :login)
                                (get form-params :username)))]
    (if (not= password password-2)
      (-> req
          (app-data/assoc :user/login         login
                          :confirmation/token token
                          :confirmation/code  code)
          (web/form-params-error! {:user/repeated-password :repeated-password}))
      (let [db    (auth/db req)
            cfrm  (confirmation/establish db id code token one-minute "recovery")
            login (delay (or (some->> cfrm :user/id (user/email db :id)) @login))
            req   (app-data/assoc req
                                  :user/login         login
                                  :confirmation/token token
                                  :confirmation/code  code)]
        (if-not (get cfrm :confirmed?)
          (web/handle-error req (or (:errors cfrm) :verify/bad-result))
          (let [req (super/set-password! req (get cfrm :user/id) password)]
            (when (resp/app-status? req :pwd/created)
              (log/web-dbg req "Removing confirmation entry for created password")
              (confirmation/delete db (or cfrm id) "recovery"))
            req))))))
