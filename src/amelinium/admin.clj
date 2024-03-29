(ns

    ^{:doc    "amelinium service, administrative procedures."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.admin

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.tools.namespace.repl     :refer [refresh
                                                      refresh-all]]
            [next.jdbc                        :as             jdbc]
            [next.jdbc.sql                    :as              sql]
            [next.jdbc.result-set             :as       result-set]
            [next.jdbc.sql.builder            :as          builder]
            [clj-uuid                         :as             uuid]
            [taoensso.nippy                   :as            nippy]
            [alphabase.core                   :as        alphabase]
            [clojurewerkz.balagan.core        :as                b]
            [ragtime.repl                     :as          ragtime]
            [reitit.core                      :as                r]
            [amelinium.logging                :as              log]
            [amelinium.app                    :as              app]
            [amelinium.system                 :as           system]
            [amelinium.web                    :as              web]
            [amelinium.model.user             :as             user]
            [amelinium.http                   :as             http]
            [amelinium.http.middleware        :as       middleware]
            [amelinium.http.router            :as           router]
            [amelinium.http.handler           :as          handler]
            [amelinium.auth                   :as             auth]
            [amelinium.auth.pwd               :as              pwd]
            [io.randomseed.utils.bus          :as              bus]
            [io.randomseed.utils.fs           :as               fs]
            [io.randomseed.utils.ip           :as               ip]
            [io.randomseed.utils.map          :as              map]
            [io.randomseed.utils.crypto       :as           crypto]
            [io.randomseed.utils              :refer    [some-str]]
            [io.randomseed.utils              :as            utils]
            [amelinium.spec                   :as             spec]
            [amelinium.db                     :as               db]

            [clojure.core.async               :as             async]
            [clojure.core.async               :refer  [<! <!! >! >!!
                                                       alts! alts!!
                                                       chan close!
                                                       go go-loop
                                                       put! thread]]

            [tick.core                         :as                t]
            [puget.printer                     :as            puget]
            [puget.printer                     :refer      [cprint]]))

;; Printing and reflection warnings

(set! *warn-on-reflection* true)

;; Users and passwords

(defn set-password
  "Sets a user password in a database for the given user-spec (a user map, user ID or
  UID). It will use the password configuration obtained from authentication
  configuration associated with user's account type."
  ([user-spec]
   (set-password nil user-spec nil))
  ([user-spec plain-password]
   (set-password nil user-spec plain-password))
  ([auth-settings user-spec plain-password]
   (if-some [auth-settings (or auth-settings (::auth/setup app/state) auth/setup)]
     (if-some [db (:db auth-settings)]
       (if-some [user-id (user/find-id db user-spec)]
         (if-some [user (user/props db user-id)]
           (let [atype       (:account-type user)
                 atexp       (if-not atype " (default)")
                 atype       (or atype (:default-type auth-settings))
                 auth-config (auth/config-by-type auth-settings atype)
                 db          (or (:db auth-config) db)
                 auth-model  (some-str (:id auth-config))]
             (println (str "Changing password for user " user-id "." \newline
                           "Account type is " atype atexp
                           ", chosen auth model is " auth-model "." \newline))
             (if-some [plain-password (or (some-str plain-password) (crypto/ask-pass))]
               (if-some [chains (auth/make-password-json plain-password auth-config)]
                 (let [ret (or (user/update-password db user-id chains) 0)]
                   (if (pos-int? ret) (println "Password changed successfully."))
                   (println (str "Updated rows: " ret)))
                 (println "Authentication engine could not produce password chains."))
               (println "Password is empty or blank.")))
           (println "Cannot retrieve user properties."))
         (println "Cannot find user in a database."))
       (println "Authentication database is not set in authentication configuration."))
     (println "Authentication configuration is not available. Run the application?"))))

(defn prop-do-account
  ([f user-spec]
   (prop-do-account nil f user-spec))
  ([auth-settings f user-spec & args]
   (if-some [auth-settings (or auth-settings (::auth/setup app/state) auth/setup)]
     (if-some [db (:db auth-settings)]
       (if-some [user-id (user/find-id db user-spec)]
         (if-some [user (user/props db user-id)]
           (let [user        (user/props db user-id)
                 atype       (:account-type user)
                 atype       (or atype (:default-type auth-settings))
                 auth-config (auth/config-by-type auth-settings atype)
                 db          (or (:db auth-config) db)]
             (apply f db user-id args))
           (println "Cannot retrieve user properties."))
         (println "Cannot find user in a database."))
       (println "Authentication database is not set in authentication configuration."))
     (println "Authentication configuration is not available. Run the application?"))))

(defn lock-account
  ([user-spec]
   (prop-do-account user/prop-set user-spec))
  ([auth-settings user-spec]
   (prop-do-account auth-settings user/prop-set user-spec :locked (t/now))))

(defn unlock-account
  ([user-spec]
   (prop-do-account user/prop-set user-spec))
  ([auth-settings user-spec]
   (prop-do-account auth-settings user/prop-del user-spec :locked)))
