(ns

    ^{:doc    "amelinium service, user model."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.model.user

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string                    :as           str]
            [clojure.core.cache.wrapped        :as           cwr]
            [next.jdbc                         :as          jdbc]
            [next.jdbc.sql                     :as           sql]
            [next.jdbc.types                   :refer [as-other]]
            [clj-uuid                          :as          uuid]
            [buddy.core.hash                   :as          hash]
            [buddy.core.codecs                 :as        codecs]
            [tick.core                         :as             t]
            [amelinium.db                      :as            db]
            [amelinium.auth.pwd                :as           pwd]
            [amelinium.http.middleware.session :as       session]
            [amelinium.http.middleware.roles   :as         roles]
            [io.randomseed.utils.time          :as          time]
            [io.randomseed.utils.map           :as           map]
            [io.randomseed.utils.ip            :as            ip]
            [io.randomseed.utils               :refer       :all]))

(defonce props-cache    (atom nil))
(defonce settings-cache (atom nil))
(defonce ids-cache      (atom nil))

;; Users

(defn get-user-by-id
  "Given a user ID, return the user record."
  [db id]
  (when-some [id (parse-long id)]
    (sql/get-by-id db :users id db/opts-simple-map)))

(defn get-user-by-email
  "Given an email, return the user record."
  [db email]
  (when-some [email (some-str email)]
    (sql/get-by-id db :users email :email db/opts-simple-map)))

(defn get-user-by-uuid
  "Given an UID, return the user record."
  [db uid]
  (when-some [uid (db/some-uuid-str)]
    (sql/get-by-id db :users uid :uid db/opts-simple-map)))

(def ^:const create-minimal-sql
  (str-spc
   "INSERT IGNORE INTO users (email,account_type)"))

(defn create-minimal
  "Creates a new user identified by the given e-mail with generated ID, UID and without
  setting any additional information."
  ([db email account-type]
   (jdbc/execute-one! db [create-minimal-sql (some-str email) (some-str account-type)])))

;; Passwords

(def ^:const password-query
  (str-spc "SELECT password AS intrinsic, suite AS shared FROM users, password_suites"
           "WHERE users.email = ? AND password_suites.id = users.password_suite_id"))

(def ^:const password-query-atypes-pre
  (str-spc "SELECT password AS intrinsic, suite AS shared FROM users, password_suites"
           "WHERE users.email = ? AND users.account_type"))

(def ^:const password-query-atypes-post
  " AND password_suites.id = users.password_suite_id")

(defn get-password-suites
  "Gets intrinsic and shared password suites for the given user, identified by an
  e-mail address."
  ([db email]
   (when (and db email)
     (jdbc/execute-one! db [password-query email] db/opts-simple-map)))
  ([db email account-types-config]
   (if-some [ac-types (get account-types-config :account-types/names)]
     (when (and db email)
       (let [ac-sql (get account-types-config :account-types/sql)
             query  (str password-query-atypes-pre
                         (if ac-sql ac-sql (db/braced-join-? ac-types))
                         password-query-atypes-post)]
         (jdbc/execute-one! db (cons query (cons email ac-types)) db/opts-simple-map)))
     (get-password-suites db email))))

(def ^:const login-query
  (str-spc "SELECT password AS intrinsic, suite AS shared, users.id AS id, soft_locked, locked, account_type"
           "FROM users, password_suites"
           "WHERE users.email = ? AND password_suites.id = users.password_suite_id"))

(def ^:const login-query-atypes-pre
  (str-spc "SELECT password AS intrinsic, suite AS shared, users.id AS id, soft_locked, locked, account_type"
           "FROM users, password_suites"
           "WHERE users.email = ? AND users.account_type"))

(def ^:const login-query-atypes-post
  " AND password_suites.id = users.password_suite_id")

(defn get-login-data
  "Gets data required for user to be authenticated, including intrinsic and shared
  password suites."
  ([db email]
   (when (and db email)
     (jdbc/execute-one! db [login-query email] db/opts-simple-map)))
  ([db email auth-config]
   (if-some [ac-types (get auth-config :account-types/names)]
     (let [db (or db (get auth-config :db))]
       (when (and db email)
         (let [ac-sql (get auth-config :account-types/sql)
               query  (str login-query-atypes-pre
                           (if ac-sql ac-sql (str "IN " (db/braced-join-? ac-types)))
                           login-query-atypes-post)]
           (jdbc/execute-one! db (cons query (cons email ac-types)) db/opts-simple-map))))
     (get-login-data db email))))

(def ^:const insert-shared-suite-query
  "INSERT IGNORE INTO password_suites(suite) VALUES(?) RETURNING id")

(def ^:const shared-suite-query
  "SELECT id FROM password_suites WHERE suite = ?")

(defn create-or-get-shared-suite-id
  "Gets shared suite ID on a basis of its JSON content. If it does not exist, it is
  created."
  [db suite]
  (when (and db suite)
    (first
     (or (jdbc/execute-one! db [insert-shared-suite-query suite] db/opts-simple-vec)
         (jdbc/execute-one! db [shared-suite-query suite]        db/opts-simple-vec)))))

(defn update-password
  "Updates password information for the given user by updating suite ID and intrinsic
  password in an authorization database. Additionally last_attempt and last_failed_ip
  properties are deleted and login_attempts is set to 0."
  ([db id suites]
   (when suites
     (update-password db id (get suites :shared) (get suites :intrinsic))))
  ([db id shared-suite user-suite]
   (when (and db id shared-suite user-suite)
     (when-some [shared-id (create-or-get-shared-suite-id db shared-suite)]
       (sql/update! db :users
                    {:password_suite_id shared-id
                     :password          user-suite
                     :last_attempt      nil
                     :last_failed_ip    nil
                     :login_attempts    0}
                    {:id id})))))

(defn update-login-ok
  [db id ip]
  (when (and db id)
    (let [login-time (t/now)]
      (sql/update! db :users {:login_attempts 1
                              :soft_locked    nil
                              :last_attempt   login-time
                              :last_login     login-time
                              :last_ok_ip     (ip/to-str-v6 ip)} {:id id}))))

(def ^:const login-failed-update-query
  (str-spc
   "UPDATE users"
   "SET last_failed_ip = ?,"
   " login_attempts = 1 + GREATEST(login_attempts -"
   "  FLOOR(TIME_TO_SEC(TIMEDIFF(NOW(), last_attempt)) / ?),"
   "  0),"
   " last_attempt = NOW()"
   "WHERE id = ?"))

(def ^:const soft-lock-update-query
  (str-spc
   "UPDATE users"
   "SET soft_locked = NOW()"
   "WHERE id = ? AND login_attempts > ?"))

(defn update-login-failed
  [db user-id ip-address max-attempts attempt-expires-after-secs]
  (when (and db user-id)
    (jdbc/execute-one! db [login-failed-update-query
                           (ip/to-str-v6 ip-address)
                           (time/seconds attempt-expires-after-secs 1)
                           user-id])
    (jdbc/execute-one! db [soft-lock-update-query
                           user-id
                           (or max-attempts 1)])))

;; Sessions

(defn create-session
  "Creates a session for user of the given ID and IP address."
  ([req opts-or-config-key user-id user-email ip-address]
   (session/create req opts-or-config-key user-id user-email ip-address))
  ([opts user-id user-email ip-address]
   ((get opts :fn/create) user-id user-email ip-address))
  ([opts user ip-address]
   ((get opts :fn/create) (get user :id) (get user :email) ip-address)))

(defn prolong-session
  ([opts smap ip-address]
   ((get opts :fn/prolong) smap ip-address))
  ([req opts-or-config-key smap ip-address]
   (session/prolong req opts-or-config-key smap ip-address)))

(defn get-session-var
  [opts smap var-name & more]
  (if more
    (apply session/get-var opts smap var-name more)
    (session/get-var opts smap var-name)))

(defn put-session-var
  [opts smap var-name v & more]
  (if more
    (apply session/put-var! opts smap var-name v more)
    (session/put-var! opts smap var-name v)))

(def set-session-var put-session-var)

(defn del-session-var
  [opts smap var-name & more]
  (if more
    (apply session/del-var! opts smap var-name more)
    (session/del-var! opts smap var-name)))

;; Roles

(defn prop-get-roles
  ([smap-or-user-id opts context]
   (roles/filter-in-context context (prop-get-roles smap-or-user-id opts) opts))
  ([smap-or-user-id opts]
   (if (map? smap-or-user-id)
     (roles/get-roles-from-session opts smap-or-user-id)
     (roles/get-roles-for-user-id  opts smap-or-user-id))))

;; Settings (not cached)

(def ^{:arglists '([db user-id setting-id])}
  get-setting
  "Gets user setting and de-serializes it to a Clojure data structure."
  (db/make-setting-getter :user-settings :user-id))

(def ^{:arglists '([db user-id setting-id value]
                   [db user-id setting-id value & pairs])}
  put-setting!
  "Stores one or more settings of the given user in a database. Maximum object size is
  32 KB."
  (db/make-setting-setter :user-settings :user-id))

(def ^{:arglists '([db user-id]
                   [db user-id setting-id]
                   [db user-id setting-id & setting-ids])}
  del-setting!
  "Deletes one or more settings for a given user from a database."
  (db/make-setting-deleter :user-settings :user-id))

;; Settings (cached)

(defn setting
  [db user-id setting-id]
  (db/cached-setting-get settings-cache get-setting db user-id setting-id))

(defn setting-set
  ([db user-id setting-id value]
   (db/cached-setting-set settings-cache put-setting! db user-id setting-id value))
  ([db user-id setting-id value & pairs]
   (apply db/cached-setting-set settings-cache put-setting!
          db user-id setting-id value pairs)))

(defn setting-del
  ([db user-id]
   (db/cached-setting-del settings-cache del-setting! db user-id))
  ([db user-id setting-id]
   (db/cached-setting-del settings-cache del-setting! db user-id setting-id))
  ([db user-id setting-id & more]
   (apply db/cached-setting-del settings-cache del-setting!
          db user-id setting-id more)))

;; Properties (cached)

(def ^:const info-cols
  [:id :uid :email :account_type
   :first_name :last_name :middle_name :phone
   :login_attempts :last_ok_ip :last_failed_ip
   :last_attempt :last_login :created :created_by
   :soft_locked :locked])

(defn info-coercer-coll
  [coll]
  (map/map-vals #(db/key-as-uuid % :uid) coll))

(defn info-coercer
  [m]
  (db/key-as-uuid m :uid))

(def ^{:arglists '([db ids])}
  info-getter-coll
  (comp info-coercer-coll (db/make-getter-coll :users :id info-cols)))

(def ^{:arglists '([db id] [db id & more])}
  info-getter-core
  (db/make-getter :users :id info-cols info-getter-coll))

(defn info-getter
  ([db id]
   (info-coercer (info-getter-core db id)))
  ([db _ id]
   (info-coercer (info-getter-core db nil id)))
  ([db _ id & more]
   (info-getter-coll db (cons id more))))

(def ^{:arglists '([db id keys-vals])}
  info-setter
  (db/make-setter :users :id))

(def ^{:arglists '([db id])}
  info-deleter
  (db/make-deleter :users :id))

(defn props-set
  "Sets properties of a user with the given ID."
  [db id keys-vals]
  (let [r (info-setter db id keys-vals)]
    (db/cache-evict! props-cache (long id)) r))

(defn props-del
  "Deletes all properties of a user with the given ID."
  [db id]
  (let [r (info-deleter db id)]
    (db/cache-evict! props-cache (long id)) r))

(defn prop-set
  "Sets property k of a user with the given ID to value v."
  [db id k v]
  (let [r (info-setter db id {k v})]
    (db/cache-evict! props-cache (long id)) r))

(defn prop-del
  "Deletes property of a user with the given ID by setting it to nil."
  [db id k]
  (prop-set db id k nil))

(defn props
  ([db id]
   (db/get-cached props-cache info-getter db id))
  ([db id & ids]
   (db/get-cached-coll props-cache info-getter-coll db (cons id ids))))

(defn props-multi
  [db ids]
  (db/get-cached-coll props-cache info-getter-coll db ids))

(defn prop
  ([db prop id]
   (db/get-cached-prop props-cache info-getter db prop id))
  ([db prop id & ids]
   (db/get-cached-coll-prop props-cache info-getter-coll db prop (cons id ids))))

(defn prop-or-default
  ([db prop default id]
   (db/get-cached-prop-or-default props-cache info-getter db prop default id))
  ([db prop default id & ids]
   (apply db/get-cached-prop-or-default props-cache info-getter-coll
          db prop default id ids)))

;; Getting user properties by...

(defn props-by-id
  [db user-id]
  (when (some? user-id) (props db user-id)))

(defn props-by-session
  [db smap]
  (when-some [user-id (get smap :user/id)] (props db user-id)))

(defn props-by-session-or-id
  [db smap user-id]
  (when db (or (props-by-session db smap) (props-by-id db user-id))))

;; Email to ID mapping (cached)

(def ^:const email-id-query
  "SELECT id FROM users WHERE email = ?")

(def ^:const emails-ids-query
  "SELECT email, id FROM users WHERE email IN")

(defn get-user-id-by-email
  ([db email]
   (db/get-id-by-email db email-id-query email))
  ([db _ email]
   (db/get-id-by-email db email-id-query email)))

(defn get-user-ids-by-emails
  ([db emails]
   (db/get-ids-by-emails db emails-ids-query emails))
  ([db _ emails]
   (db/get-ids-by-emails db emails-ids-query emails)))

(defn email-to-id
  [db email]
  (db/email-to-id db ids-cache get-user-id-by-email email))

(defn emails-to-ids
  [db emails]
  (db/emails-to-ids db ids-cache get-user-ids-by-emails emails))

(defn props-by-email
  [db email]
  (props db (email-to-id db email)))

(defn id-to-email
  ([db id]
   (prop db :email id))
  ([db id & ids]
   (apply prop db :email id ids)))

(defn ids-to-emails
  ([db ids]
   (db/get-cached-coll-prop props-cache info-getter-coll db :email ids)))

;; UID to ID mapping (cached)

(def ^:const uid-id-query
  "SELECT id FROM users WHERE uid = ?")

(def ^:const uids-ids-query
  "SELECT uid, id FROM users WHERE uid IN")

(defn get-user-id-by-uid
  ([db uid]
   (db/get-id-by-uid db uid-id-query uid))
  ([db _ uid]
   (db/get-id-by-uid db uid-id-query uid)))

(defn get-user-ids-by-uids
  ([db uids]
   (db/get-ids-by-uids db uids-ids-query uids))
  ([db _ uids]
   (db/get-ids-by-uids db uids-ids-query uids)))

(defn uid-to-id
  [db uid]
  (db/uid-to-id db ids-cache get-user-id-by-uid uid))

(defn uids-to-ids
  [db uids]
  (db/uids-to-ids db ids-cache get-user-ids-by-uids uids))

(defn props-by-uid
  [db uid]
  (props db (uid-to-id db uid)))

(defn id-to-uid
  ([db id]
   (prop db :uid id))
  ([db id & ids]
   (apply prop db :uid id ids)))

(defn ids-to-uids
  ([db ids]
   (db/get-cached-coll-prop props-cache info-getter-coll db :uid ids)))

;; Existence testing (not cached)

(def ^:const id-exists-query
  "SELECT 1 FROM users WHERE id = ?")

(defn get-user-id-exists?
  [db id]
  (when (and db id)
    (some? (jdbc/execute-one! db [id-exists-query (db/id-to-db id)] db/opts-simple-map))))

(defn get-user-uid-exists?
  [db uid]
  (some? (get-user-id-by-uid db uid)))

(defn get-user-email-exists?
  [db email]
  (some? (get-user-id-by-email db email)))

;; Existence testing (cached)

(defn id-exists?
  [db id]
  (some? (id-to-uid db id)))

(defn uid-exists?
  [db uid]
  (some? (uid-to-id db uid)))

(defn email-exists?
  [db email]
  (some? (email-to-id db email)))

(defn some-id
  [db id]
  (when (and id (id-exists? db id))
    id))

(defn some-uid
  [db uid]
  (when (and uid (uid-exists? db uid))
    uid))

(defn some-email
  [db email]
  (when (and email (email-exists? db email))
    email))

;; Other

(defn prop-get-locked
  "Returns hard-lock status for the user account. Uses cached property."
  ([db id]
   (prop db :locked id))
  ([db id & ids]
   (apply prop db :locked id ids)))

(defn- emailable?
  [v]
  (and (valuable? v)
       (or (and (string? v) (nat-int? (str/index-of v \@)))
           (ident? v) (nat-int? (str/index-of (str (symbol v)) \@)))))

(defn find-id
  "Gets user ID on a basis of a map with :id key, on a basis of a map with :uid key,
  on a basis of a number, a string or a keyword being ID, email or UID. User must
  exist in a database. Uses cached properties if possible."
  [db user-spec]
  (when (and db user-spec)
    (if (map? user-spec)
      (let [id    (delay (get user-spec :id))
            uid   (delay (get user-spec :uid))
            email (delay (get user-spec :email))]
        (cond
          (and @id    (number?        @id))    (some-id     db @id)
          (and @uid   (uuid/uuidable? @uid))   (uid-to-id   db @uid)
          (and @email (emailable?     @email)) (email-to-id db @email)))
      (cond
        (number?        user-spec) (some-id     db user-spec)
        (uuid/uuidable? user-spec) (uid-to-id   db user-spec)
        (emailable?     user-spec) (email-to-id db user-spec)))))
