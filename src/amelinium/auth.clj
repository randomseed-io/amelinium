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
            [io.randomseed.utils.map  :as       map]
            [tick.core                :as         t])

  (:import [javax.sql DataSource]
           [java.time Duration]))

(defonce config nil)

(def confirmation-expires-default (t/new-duration 10 :minutes))

(defrecord AccountTypes     [^String                        sql
                             ^clojure.lang.PersistentVector ids
                             ^clojure.lang.PersistentVector names
                             ^clojure.lang.Keyword          default
                             ^String                        default-name])

(defrecord AuthLocking      [^Long                max-attempts
                             ^Duration            lock-wait
                             ^Duration            fail-expires])

(defrecord AuthConfirmation [^Long                max-attempts
                             ^Duration            expires])

(defrecord AuthPasswords    [^clojure.lang.Keyword id
                             ^clojure.lang.ISeq    suite
                             ^clojure.lang.Fn      check
                             ^clojure.lang.Fn      check-json
                             ^clojure.lang.Fn      encrypt
                             ^clojure.lang.Fn      encrypt-json
                             ^clojure.lang.Fn      wait])

(defrecord AuthConfig       [^clojure.lang.Keyword id
                             ^DataSource           db
                             ^AccountTypes         account-types
                             ^AccountTypes         parent-account-types
                             ^AuthConfirmation     confirmation
                             ^AuthLocking          locking
                             ^AuthPasswords        passwords])

(defrecord AuthSettings     [^DataSource           db
                             ^clojure.lang.Keyword default-type
                             ^AuthConfig           default
                             ^AccountTypes         types])

;; Password authentication

(defn check-password
  "Checks password for a user against an encrypted password given in password
  suites. Specific authentication configuration map must be given."
  ([password pwd-suites auth-config]
   (if (and password pwd-suites auth-config)
     (if-some [checker (.check ^AuthPasswords (.passwords ^AuthConfig auth-config))]
       (if (map? pwd-suites)
         (checker password pwd-suites)
         (checker password nil pwd-suites)))))
  ([password pwd-shared-suite pwd-user-suite auth-config]
   (if (and password pwd-shared-suite pwd-user-suite auth-config)
     (if-some [checker (.check ^AuthPasswords (.passwords ^AuthConfig auth-config))]
       (checker password pwd-shared-suite pwd-user-suite)))))

(defn check-password-json
  "Checks password for a user against a JSON-encoded password suites. Specific
  authentication configuration map must be given."
  ([password json-pwd-suites auth-config]
   (if (and password json-pwd-suites auth-config)
     (if-some [checker (.check-json ^AuthPasswords (.passwords ^AuthConfig auth-config))]
       (if (map? json-pwd-suites)
         (checker password json-pwd-suites)
         (checker password nil json-pwd-suites)))))
  ([password json-pwd-shared-suite json-pwd-user-suite auth-config]
   (if (and password json-pwd-shared-suite json-pwd-user-suite auth-config)
     (if-some [checker (.check-json ^AuthPasswords (.passwords ^AuthConfig auth-config))]
       (checker password json-pwd-shared-suite json-pwd-user-suite)))))

(defn make-password
  "Creates new password for a user. Specific authentication configuration map must be
  given."
  [password auth-config]
  (if (and password auth-config)
    (if-some [encryptor (.encrypt ^AuthPasswords (.passwords ^AuthConfig auth-config))]
      (encryptor password))))

(defn make-password-json
  "Creates new password for a user in JSON format. Specific authentication
  configuration map must be given."
  [password auth-config]
  (if (and password auth-config)
    (if-some [encryptor (.encrypt-json ^AuthPasswords (.passwords ^AuthConfig auth-config))]
      (encryptor password))))

;; Settings initialization

(defn make-passwords
  [m]
  (if (instance? AuthPasswords m) m
      (apply ->AuthPasswords
             (map (:passwords m)
                  [:id :suite :check-fn :check-json-fn :encrypt-fn :encrypt-json-fn :wait-fn]))))

(defn parse-account-ids
  ([v]
   (parse-account-ids some-keyword-simple v))
  ([f v]
   (if v
     (some->> (if (coll? v) (if (map? v) (keys v) v) (cons v nil))
              seq (filter valuable?) (map f) (filter keyword?) seq))))

(defn new-account-types
  ([ids]
   (new-account-types ids nil))
  ([ids default-id]
   (let [ids (some->> ids parse-account-ids (filter identity) distinct seq)
         dfl (or (some-keyword-simple default-id) (first ids))
         dfn (if dfl (name dfl))
         ids (if dfl (conj ids dfl))
         ids (if ids (set ids))
         nms (if ids (mapv name ids))
         sql (if ids (if (= 1 (count nms)) " = ?" (str " IN " (db/braced-join-? nms))))]
     (->AccountTypes sql ids nms dfl dfn))))

(defn make-account-types
  [m]
  (if (instance? AccountTypes m) m
      (let [act (:account-types m)
            act (if (instance? AccountTypes act) (:ids act) act)
            act (if act (parse-account-ids act))
            ids (some->> [:account-types/ids :account-types/names]
                         (map (partial get m))
                         (apply concat act))]
        (new-account-types ids (or (:account-types/default m)
                                   (:account-types/default-name m))))))

(defn make-confirmation
  [m]
  (if (instance? AuthConfirmation m) m
      (->AuthConfirmation
       (safe-parse-long (:confirmation/max-attempts m) 3)
       ((fnil time/parse-duration [1 :minutes]) (:confirmation/expires m)))))

(defn make-locking
  [m]
  (if (instance? AuthLocking m) m
      (->AuthLocking
       (safe-parse-long (:locking/max-attempts m) 10)
       ((fnil time/parse-duration [10 :minutes]) (:locking/lock-wait    m))
       ((fnil time/parse-duration [ 1 :minutes]) (:locking/fail-expires m)))))

(defn make-auth
  ([m]
   (make-auth nil m))
  ([k m]
   (if (instance? AuthConfig m) m
       (map->AuthConfig {:id            (keyword (or (:id m) k))
                         :db            (db/ds          (:db m))
                         :passwords     (make-passwords      m)
                         :account-types (make-account-types  m)
                         :locking       (make-locking        m)
                         :confirmation  (make-confirmation   m)}))))

(defn init-auth
  "Authentication configurator."
  [k config]
  (log/msg "Configuring auth engine" k
           (str "(attempts: "  (:locking/max-attempts config)
                ", lock wait: "    (time/seconds  (:locking/lock-wait    config)) " s"
                ", lock expires: " (time/seconds  (:locking/fail-expires config)) " s)"))
  (make-auth k config))

(defn config-by-type
  "Returns authentication configuration for the given account type using an
  authentication configuration map."
  [auth-settings account-type]
  (if-some [types-map (.types ^AuthSettings auth-settings)]
    (get types-map (some-keyword-simple account-type))))

(defn config-by-type-with-var
  "Returns authentication configuration for the given account type using an
  authentication settings map stored in a Var of the given (fully-qualified)
  name."
  [var-name account-type]
  (config-by-type (var/deref var-name) account-type))

(defn index-by-type
  "Prepares static authentication preference map by mapping a copy of each
  authentication configuration to any account type identifier found within it. So,
  `[{:account-types {:ids [:a :b]}}]` becomes:
  `{:a {:account-types {:ids [:a :b]}}, :b {:account-types {:ids [:a :b]}}`.

  Additionally, it sets `:db` from global settings and updates `:account-types` field
  to have current account type set as default (including SQL query). Original account
  types is preserved under `:parent-account-types`. Each authentication configuration
  will be initialized if it isn't already."
  [coll db]
  (->> coll
       (filter map?)
       (map #(update % :db (db/ds db)))
       (map #(assoc  % :account-types (make-account-types %)))
       (mapcat #(map list (map keyword (:ids (:account-types %))) (repeat %)))
       (filter #(and (coll? %) (keyword? (first %)) (map? (second %))))
       (map (fn [[id auth-config]]
              (if-some [id (some-keyword-simple id)]
                (vector
                 id
                 (make-auth (or (:id auth-config) id)
                            (assoc auth-config
                                   :parent-account-types (:account-types auth-config)
                                   :account-types (new-account-types id)))))))
       (filter identity)
       (into {})))

(defn init-config
  "Prepares authentication settings."
  [config]
  (let [config (map/update-existing config :db db/ds)
        config (update config :types index-by-type (:db config))]
    (-> config
        (assoc :default (get (:types config) (:default-type config)))
        map->AuthSettings)))

(system/add-init  ::auth [k config] (init-auth k config))
(system/add-halt! ::auth [_ config] nil)

(system/add-init  ::settings [k config] (var/make k (init-config config)))
(system/add-halt! ::settings [k config] (var/make k nil))

(derive ::strong ::auth)
(derive ::simple ::auth)
