(ns

    ^{:doc    "amelinium service, authentication."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.auth

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [amelinium.db             :as        db]
            [amelinium.logging        :as       log]
            [amelinium.auth.pwd       :as       pwd]
            [amelinium.system         :as    system]
            [io.randomseed.utils      :refer   :all]
            [io.randomseed.utils.time :as      time]
            [io.randomseed.utils.var  :as       var]
            [io.randomseed.utils.map  :as       map])

  (:import [javax.sql DataSource]
           [java.time Duration]))

(defonce config nil)

(defrecord AccountTypes [^String                        sql
                         ^clojure.lang.PersistentVector ids
                         ^clojure.lang.PersistentVector names
                         ^clojure.lang.Keyword          default
                         ^String                        default-name])

(defrecord Locking      [^Long     max-attempts
                         ^Duration lock-wait
                         ^Duration fail-expires])

(defrecord Confirmation [^Long max-attempts
                         ^Duration expires])

(defrecord Registration [^Duration expires])

(defrecord Passwords    [^clojure.lang.Keyword id
                         ^clojure.lang.ISeq    suite
                         ^clojure.lang.Fn      check
                         ^clojure.lang.Fn      check-json
                         ^clojure.lang.Fn      encrypt
                         ^clojure.lang.Fn      encrypt-json
                         ^clojure.lang.Fn      wait])

(defrecord Config       [^clojure.lang.Keyword id
                         ^DataSource           db
                         ^AccountTypes         account-types
                         ^Registration         registration
                         ^Confirmation         confirmation
                         ^Locking              locking
                         ^Passwords            passwords])

(defrecord Settings     [^DataSource           db
                         ^clojure.lang.Keyword default-type
                         ^Config               default
                         ^AccountTypes         types])

;; Password authentication

(defn check-password
  "Checks password for a user against an encrypted password given in password
  suites. Specific authentication configuration map must be given."
  ([password pwd-suites auth-config]
   (if (and password pwd-suites auth-config)
     (if-some [checker (.check ^Passwords (.passwords ^Config auth-config))]
       (if (map? pwd-suites)
         (checker password pwd-suites)
         (checker password nil pwd-suites)))))
  ([password pwd-shared-suite pwd-user-suite auth-config]
   (if (and password pwd-shared-suite pwd-user-suite auth-config)
     (if-some [checker (.check ^Passwords (.passwords ^Config auth-config))]
       (checker password pwd-shared-suite pwd-user-suite)))))

(defn check-password-json
  "Checks password for a user against a JSON-encoded password suites. Specific
  authentication configuration map must be given."
  ([password json-pwd-suites auth-config]
   (if (and password json-pwd-suites auth-config)
     (if-some [checker (.check-json ^Passwords (.passwords ^Config auth-config))]
       (if (map? json-pwd-suites)
         (checker password json-pwd-suites)
         (checker password nil json-pwd-suites)))))
  ([password json-pwd-shared-suite json-pwd-user-suite auth-config]
   (if (and password json-pwd-shared-suite json-pwd-user-suite auth-config)
     (if-some [checker (.check-json ^Passwords (.passwords ^Config auth-config))]
       (checker password json-pwd-shared-suite json-pwd-user-suite)))))

(defn make-password
  "Creates new password for a user. Specific authentication configuration map must be
  given."
  [password auth-config]
  (if (and password auth-config)
    (if-some [encryptor (.encrypt ^Passwords (.passwords ^Config auth-config))]
      (encryptor password))))

(defn make-password-json
  "Creates new password for a user in JSON format. Specific authentication
  configuration map must be given."
  [password auth-config]
  (if (and password auth-config)
    (if-some [encryptor (.encrypt-json ^Passwords (.passwords ^Config auth-config))]
      (encryptor password))))

;; Settings initialization

(defn make-passwords
  [m]
  (apply ->Passwords
         (map (:passwords m)
              [:id :suite :check-fn :check-json-fn :encrypt-fn :encrypt-json-fn :wait-fn])))

(defn parse-account-types
  ([v]
   (parse-account-types some-keyword-simple v))
  ([f v]
   (if v
     (some->> (if (coll? v) (seq (if (map? v) (keys v) v)) (cons v nil))
              seq (filter #(and % (valuable? %)))
              seq (map f) (filter keyword?) seq))))

(defn make-account-types
  [m]
  (let [ids (some->> [:account-types/ids :account-types :account-types/names :account-type]
                     (map (comp parse-account-types m))
                     (filter identity)
                     (apply concat)
                     distinct seq vec)
        nms (if ids (mapv name ids))
        sql (if ids (if (= 1 (count nms)) " = ?" (str " IN " (db/braced-join-? nms))))
        dfl (or (some-keyword-simple (or (:account-types/default m)
                                         (:account-types/default-name m)))
                (first ids))
        dfn (if dfl (name dfl))
        ids (set ids)]
    (->AccountTypes sql ids nms dfl dfn)))

(defn make-registration
  [m]
  (->Registration
   ((fnil time/parse-duration [10 :minutes]) (:registration/expires m))))

(defn make-confirmation
  [m]
  (->Confirmation
   (safe-parse-long (:confirmation/max-attempts m) 3)
   ((fnil time/parse-duration [1 :minutes]) (:confirmation/expires m))))

(defn make-locking
  [m]
  (->Locking
   (safe-parse-long (:locking/max-attempts m) 10)
   ((fnil time/parse-duration [10 :minutes]) (:locking/lock-wait    m))
   ((fnil time/parse-duration [ 1 :minutes]) (:locking/fail-expires m))))

(defn init-auth
  "Authentication configurator."
  [k config]
  (log/msg "Configuring auth engine" k
           (str "(attempts: "  (:locking/max-attempts config)
                ", lock wait: "    (time/seconds  (:locking/lock-wait    config)) " s"
                ", lock expires: " (time/seconds  (:locking/fail-expires config)) " s)"))
  (map->Config {:id            (keyword k)
                :db            (db/ds         (:db config))
                :passwords     (make-passwords     config)
                :account-types (make-account-types config)
                :locking       (make-locking       config)
                :confirmation  (make-confirmation  config)
                :registration  (make-registration  config)}))

(defn config-by-type
  "Returns authentication configuration for the given account type using an
  authentication configuration map."
  [auth-settings account-type]
  (if-some [types-map (.types ^Settings auth-settings)]
    (get types-map (some-keyword-simple account-type))))

(defn config-by-type-with-var
  "Returns authentication configuration for the given account type using an
  authentication settings map stored in a Var of the given (fully-qualified)
  name."
  [var-name account-type]
  (config-by-type (var/deref var-name) account-type))

;; Mapping an account type to its preferred authentication configuration

(defn init-by-type
  "Prepares static authentication preference map."
  [config db]
  (->> config
       (map/map-keys some-keyword-simple)
       map/remove-empty-values
       (map/map-vals #(update % :db (db/ds (fnil identity db))))))

(defn init-config
  "Prepares authentication settings."
  [config]
  (let [config (map/update-existing config :db db/ds)
        config (map/update-existing config :types init-by-type (:db config))]
    (-> config
        (assoc :default (get (:types config) (:default-type config)))
        map->Settings)))

(system/add-init  ::auth [k config] (init-auth k config))
(system/add-halt! ::auth [_ config] nil)

(system/add-init  ::settings [k config] (var/make k (init-config config)))
(system/add-halt! ::settings [k config] (var/make k nil))

(derive ::strong ::auth)
(derive ::simple ::auth)
