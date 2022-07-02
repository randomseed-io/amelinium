(ns

    ^{:doc    "amelinium service, authentication."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.auth

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [amelinium.db               :as        db]
            [amelinium.logging          :as       log]
            [amelinium.auth.pwd         :as       pwd]
            [amelinium.system           :as    system]
            [io.randomseed.utils        :refer   :all]
            [io.randomseed.utils.time   :as      time]
            [io.randomseed.utils.var    :as       var]
            [io.randomseed.utils.map    :as       map]))

(defonce config nil)

;; Password authentication

(defn check-password
  "Checks password for a user against the encrypted password given in password
  suites. Specific authentication configuration map must be given."
  ([password pwd-suites auth-config]
   (when (and password pwd-suites auth-config)
     (when-some [checker (get auth-config :passwords/check-fn)]
       (if (map? pwd-suites)
         (checker password pwd-suites)
         (checker password nil pwd-suites)))))
  ([password pwd-shared-suite pwd-user-suite auth-config]
   (when (and password pwd-shared-suite pwd-user-suite auth-config)
     (when-some [checker (get auth-config :passwords/check-fn)]
       (checker password pwd-shared-suite pwd-user-suite)))))

(defn check-password-json
  "Checks password for a user against JSON-encoded password suites. Specific
  authentication configuration map must be given."
  ([password json-pwd-suites auth-config]
   (when (and password json-pwd-suites auth-config)
     (when-some [checker (get auth-config :passwords/check-json-fn)]
       (if (map? json-pwd-suites)
         (checker password json-pwd-suites)
         (checker password nil json-pwd-suites)))))
  ([password json-pwd-shared-suite json-pwd-user-suite auth-config]
   (when (and password json-pwd-shared-suite json-pwd-user-suite auth-config)
     (when-some [checker (get auth-config :passwords/check-json-fn)]
       (checker password json-pwd-shared-suite json-pwd-user-suite)))))

(defn make-password
  "Creates new password for a user. Specific authentication configuration map must be
  given."
  [password auth-config]
  (when (and password auth-config)
    (when-some [encryptor (get auth-config :passwords/encrypt-fn)]
      (encryptor password))))

(defn make-password-json
  "Creates new password for a user in JSON format. Specific authentication
  configuration map must be given."
  [password auth-config]
  (when (and password auth-config)
    (when-some [encryptor (get auth-config :passwords/encrypt-json-fn)]
      (encryptor password))))

;; Settings initialization

(defn prep-passwords
  [m]
  (let [passwords (:passwords m)]
    (assoc (dissoc m :passwords)
           :passwords/id              (:id              passwords)
           :passwords/wait-fn         (:wait-fn         passwords)
           :passwords/check-fn        (:check-fn        passwords)
           :passwords/check-json-fn   (:check-json-fn   passwords)
           :passwords/encrypt-fn      (:encrypt-fn      passwords)
           :passwords/encrypt-json-fn (:encrypt-json-fn passwords))))

(defn parse-account-types
  ([v]
   (parse-account-types some-keyword-simple v))
  ([f v]
   (when v
     (some->> (if (coll? v) (seq (if (map? v) (keys v) v)) (cons v nil))
              seq (filter #(and % (valuable? %)))
              seq (map f) (filter keyword?) seq))))

(defn prep-account-types
  [m]
  (let [ids (some->> [:account-types/ids :account-types :account-types/names :account-type]
                     (map (comp parse-account-types m))
                     (filter identity)
                     (apply concat)
                     distinct seq vec)
        nms (when ids (mapv name ids))
        sql (when ids (if (= 1 (count nms)) " = ?" (str " IN " (db/braced-join-? nms))))
        dfl (or (some-keyword-simple (or (:account-types/default m)
                                         (:account-types/default-name m)))
                (first ids))
        dfn (when dfl (name dfl))]
    (assoc (dissoc m :account-types)
           :account-types/sql          sql
           :account-types/ids          ids
           :account-types/names        nms
           :account-types/default      dfl
           :account-types/default-name dfn)))

(defn wrap-auth
  "Authentication configurator."
  [k settings]
  (let [s (-> settings
              prep-passwords
              prep-account-types
              (assoc  :id k)
              (update :db db/ds)
              (update :locking/max-attempts safe-parse-long 10)
              (update :locking/lock-wait    (fnil time/parse-duration [10 :minutes]))
              (update :locking/fail-expires (fnil time/parse-duration [1  :minutes])))]
    (log/msg "Configuring auth engine" k
             (str "(attempts: "  (:locking/max-attempts s)
                  ", lock wait: "    (time/seconds  (:locking/lock-wait    s)) " s"
                  ", lock expires: " (time/seconds  (:locking/fail-expires s)) " s)"))
    s))

(defn config-by-type
  "Returns authentication configuration for the given account type using an
  authentication configuration map."
  [config account-type]
  (when-some [types-map (:types config)]
    (or (get types-map (some-keyword-simple account-type))
        (get types-map :default))))

(defn config-by-type-with-var
  "Returns authentication configuration for the given account type using an
  authentication configuration map stored in a Var of the given (fully-qualified)
  name."
  [var-name account-type]
  (config-by-type (var/deref var-name) account-type))

;; Mapping of account type to preferred authentication configuration

(defn init-by-type
  "Prepares static authentication preference map."
  [config db]
  (->> config
       (map/map-keys some-keyword-simple)
       map/remove-empty-values
       (map/map-vals #(map/assoc-missing % :db db))))

(defn init-config
  "Initializes authentication configuration."
  [config]
  (let [config (map/update-existing config :db db/ds)
        db     (get config :db)
        dtype  (get config :default-type)
        config (map/update-existing config :types init-by-type db)]
    (assoc config :default (get (get config :types) dtype))))

(system/add-init  ::auth [k config] (wrap-auth k config))
(system/add-halt! ::auth [_ config] nil)

(system/add-init  ::config [k config] (var/make k (init-config config)))
(system/add-halt! ::config [k config] (var/make k nil))

(derive ::strong ::auth)
(derive ::simple ::auth)
