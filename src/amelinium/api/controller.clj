(ns

    ^{:doc    "amelinium service, common API controller functions."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.api.controller

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces         :as          p]
            [tick.core                   :as          t]
            [amelinium.logging           :as        log]
            [amelinium.common.controller :as     common]
            [io.randomseed.utils.map     :as        map]
            [io.randomseed.utils         :refer    :all]
            [amelinium.api               :as        api]
            [amelinium.http              :as       http]))

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
                auth-user-with-password!
                keywordize-params? kw-form-data])

(defn remove-login-data
  "Removes login data from the form params and body part of a request map."
  [req]
  (-> req
      (map/update-existing :form-params dissoc "password")
      (map/update-existing :params      dissoc :password)
      (update              :body        dissoc :password)))

(defn cleanup-req
  [req [_ auth?]]
  (if auth? req (remove-login-data req)))

(defn login-data?
  "Returns true if :body map of a request contains login data."
  [req]
  (when-some [bparams (get req :body)]
    (and (contains? bparams :password)
         (contains? bparams :login))))

(defn authenticate!
  "Logs user in when user e-mail and password are given, or checks if the session is
  valid to serve a current page.

  Takes a request map and obtains database connection, client IP address and
  authentication configuration from it. Also gets a user e-mail and a password from a
  map associated with the `:body` key of the `req`. Calls `auth-user-with-password!`
  to get a result or a redirect if authentication was not successful.

  If there is no e-mail nor password given (the value is `nil`, `false` or an empty
  string) then authentication is not performed but instead validity of a session is
  tested. If the session is invalid then redirect to a login page is performed. The
  destination URL is obtained via the route name taken from the `:auth/login` key of
  a route data, or from the `:login` route identifier (as a default).

  If the session is valid then the given request map is returned as is."
  [req]
  (let [body-params    (get req :body)
        user-email     (some-str (get body-params :login))
        password       (when user-email (some-str (get body-params :password)))
        sess           (get req :session)
        valid-session? (get sess :valid?)
        route-data     (http/get-route-data req)]
    (cond
      password          (auth-user-with-password! req user-email password sess route-data)
      valid-session?    req
      :invalid-session! (api/move-to req (get route-data :auth/login :login)))))

(defn login!
  "Returns login information."
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
                                              (api/auth-db req)
                                              (when @prolonged? sess)
                                              t/now))))))

(defn prep-request!
  "Prepares a request before any controller is called."
  [req]
  (let [sess        (get req :session)
        auth-state  (delay (api/login-auth-state req :login-page? :auth-page?))
        login-data? (delay (login-data? req))
        auth-db     (delay (api/auth-db req))]

    (cond

      ;; Request is invalid.

      (not (get req :validators/params-valid?))
      (-> req api/render-bad-params)

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
        (api/oplog req
                   :user-id user-id
                   :op      :access-denied
                   :level   :warning
                   :msg     (str "Permanent lock " for-mail))
        (api/go-to req (or (http/get-route-data req :auth/account-locked)
                           :login/account-locked)))

      :----pass

      (let [valid-session? (get sess :valid?)
            auth-state     @auth-state
            [_ auth?]      auth-state
            req            (cleanup-req req auth-state)]

        ;; Session is invalid (or just expired).
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
                 (api/oplog req
                            :user-id (:user/id sess)
                            :op      :session
                            :ok?     false
                            :msg     (str "Expired " for-mail)))
               (when-some [reason (:reason (:error sess))]
                 (api/oplog req
                            :user-id (:user/id sess)
                            :op      :session
                            :ok?     false
                            :level   (:error sess)
                            :msg     reason)
                 (log/log (:severity (:error sess) :warn) reason))))

        ;; Remove login data from the request if we are not authenticating a user.
        ;; Take care about broken go-to (move to a login page in such case).

        (cleanup-req req [nil auth?])))))

(defn render!
  [req]
  (api/render-ok req))

(defn not-found!
  [req]
  (api/render-not-found req))
