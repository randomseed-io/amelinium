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
            [phone-number.core                 :as         phone]
            [amelinium.db                      :as            db]
            [amelinium.auth                    :as          auth]
            [amelinium.auth.pwd                :as           pwd]
            [amelinium.http.middleware.session :as       session]
            [amelinium.http.middleware.roles   :as         roles]
            [amelinium.model.confirmation      :as  confirmation]
            [io.randomseed.utils.time          :as          time]
            [io.randomseed.utils.ip            :as            ip]
            [io.randomseed.utils.map           :as           map]
            [io.randomseed.utils.map           :refer   [qassoc]]
            [io.randomseed.utils               :refer       :all])

  (:import [javax.sql          DataSource]
           [java.time          Duration]
           [phone_number.core  Phoneable]
           [amelinium.auth.pwd Suites SuitesJSON]
           [amelinium.auth     AuthConfig AuthSettings AuthLocking AuthConfirmation AccountTypes]))

(defonce props-cache    (atom nil))
(defonce settings-cache (atom nil))
(defonce ids-cache      (atom nil))

;; DBPassword record is to pass intrinsic suite in JSON format and shared suite ID.
;; It's used to pass data around.

(defrecord DBPassword [^Long   password_suite_id
                       ^String password])

;; UserData record is to pass user data between models and controllers
;; in a bit faster way than with regular maps.

(defrecord UserData   [^String               email
                       ^Phoneable            phone
                       ^clojure.lang.Keyword account-type
                       ^AuthConfig           auth-config
                       ^DataSource           db
                       ^String               password
                       ^String               password-shared
                       ^Long                 password-suite-id
                       ^String               first-name
                       ^String               middle-name
                       ^String               last-name
                       ^Duration             expires-in
                       ^Long                 max-attempts])

;; AuthQueries record is used to pass a set of SQL queries in some structured form.

(defrecord AuthQueries [^String generic
                        ^String pre
                        ^String post
                        ^String single])

;; Authorizable protocol is to support class-based, single method dispatch, when
;; dealing with authorization and authentication data. The auth-source can be a data
;; source, an AuthConfig record or a global AuthSettings record.

(defprotocol Authorizable
  (get-user-auth-data
    [auth-source email queries]
    [auth-source email account-type queries]))

;; User data initialization

(declare create-or-get-shared-suite-id)

(defn make-user-data
  "Creates user data record by getting values from the given authentication settings
  and parameters map. If `:password` parameter is present it will make JSON password
  suite."
  [auth-settings params]
  (let [email         (some-str (or (get params :user/email) (get params :login) (get params :email)))
        phone         (or (get params :user/phone) (get params :phone))
        password      (some-str (or (get params :user/password) (get params :password)))
        account-type  (or (get params :user/account-type) (get params :account-type))
        account-type  (if (keyword? account-type) account-type (some-keyword account-type))
        account-type  (or account-type (.default-type ^AuthSettings auth-settings))
        auth-config   (or (get (.types ^AuthSettings auth-settings) account-type)
                          (.default ^AuthSettings auth-settings))
        auth-cfrm     (.confirmation ^AuthConfig auth-config)
        db            (or (.db ^AuthConfig auth-config) (.db ^AuthSettings auth-settings))
        pwd-chains    (if password (auth/make-password-json password auth-config))
        pwd-intrinsic (if pwd-chains (.intrinsic ^SuitesJSON pwd-chains))
        pwd-shared    (if pwd-chains (.shared    ^SuitesJSON pwd-chains))
        pwd-shared-id (if (and db pwd-shared) (create-or-get-shared-suite-id db pwd-shared))]
    (->UserData email phone (name account-type) auth-config db
                pwd-intrinsic pwd-shared pwd-shared-id
                (some-str (or (get params :user/first-name)  (get params :first-name)))
                (some-str (or (get params :user/middle-name) (get params :middle-name)))
                (some-str (or (get params :user/last-name)   (get params :last-name)))
                (or (.expires      ^AuthConfirmation auth-cfrm) auth/confirmation-expires-default)
                (or (.max-attempts ^AuthConfirmation auth-cfrm) 3))))

(defn make-user-data-simple
  "Creates simple user data record (only db, phone and email) by getting values from
  the given authentication settings and parameters map."
  [auth-settings params]
  (let [email (some-str (or (get params :login) (get params :email)))
        phone (get params :phone)
        db    (.db ^AuthSettings auth-settings)]
    (->UserData email phone nil nil db nil nil nil nil nil nil nil nil)))

;; Users

(defn get-user-by-id
  "Given a user ID, return the user record."
  [db id]
  (if-some [id (parse-long id)]
    (sql/get-by-id db :users id db/opts-simple-map)))

(defn get-user-by-email
  "Given an email, return the user record."
  [db email]
  (if-some [email (some-str email)]
    (sql/get-by-id db :users email :email db/opts-simple-map)))

(defn get-user-by-uuid
  "Given an UID, return the user record."
  [db uid]
  (if-some [uid (db/some-uuid-str)]
    (sql/get-by-id db :users uid :uid db/opts-simple-map)))

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

(defn fetch-session-var
  [opts smap var-name & more]
  (if more
    (apply session/fetch-var! opts smap var-name more)
    (session/fetch-var! opts smap var-name)))

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

(defn info-coercer
  [m]
  (-> m
      (db/key-as-uuid    :uid)
      (db/key-as-keyword :account-type)
      (db/key-as-ip      :last-ok-ip)
      (db/key-as-ip      :last-failed-ip)
      (db/key-as-phone   :phone)))

(defn info-coercer-coll
  [coll]
  (map/map-vals info-coercer coll))

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
  "Returns user properties for the given user ID (cached)."
  ([db id]
   (db/get-cached props-cache info-getter db id))
  ([db id & ids]
   (db/get-cached-coll props-cache info-getter-coll db (cons id ids))))

(defn props-multi
  "Returns user properties for each of the given user IDs (cached)."
  [db ids]
  (db/get-cached-coll props-cache info-getter-coll db ids))

(defn prop
  "Returns user property for the given user ID or a map of user property keyed with its
  ID if multiple IDs are given (cached)."
  ([db prop id]
   (db/get-cached-prop props-cache info-getter db prop id))
  ([db prop id & ids]
   (db/get-cached-coll-prop props-cache info-getter-coll db prop (cons id ids))))

(defn prop-or-default
  "Returns user property for the given user ID or a map of user property keyed with its
  ID if multiple IDs are given (cached)."
  ([db prop default id]
   (db/get-cached-prop-or-default props-cache info-getter db prop default id))
  ([db prop default id & ids]
   (apply db/get-cached-prop-or-default props-cache info-getter-coll
          db prop default id ids)))

;; Getting user properties by...

(defn props-by-id
  "Returns properties of the given user ID (cached)."
  [db user-id]
  (if (some? user-id) (props db user-id)))

(defn props-by-session
  "Returns properties of the given user session (cached)."
  [db smap]
  (if-some [user-id (get smap :user/id)] (props db user-id)))

(defn props-by-session-or-id
  "Returns properties of the given user session or ID (cached)."
  [db smap user-id]
  (if db (or (props-by-session db smap) (props-by-id db user-id))))

;; E-mail to ID mapping

(def ^:const email-id-query
  "SELECT id FROM users WHERE email = ?")

(def ^:const emails-ids-query
  "SELECT email, id FROM users WHERE email IN")

(defn get-user-id-by-email
  "Returns user ID for the given e-mail (not cached)."
  ([db email]
   (db/get-id-by-email db email-id-query email))
  ([db _ email]
   (db/get-id-by-email db email-id-query email)))

(defn get-user-ids-by-emails
  "Returns user IDs for the given e-mails (not cached)."
  ([db emails]
   (db/get-ids-by-emails db emails-ids-query emails))
  ([db _ emails]
   (db/get-ids-by-emails db emails-ids-query emails)))

(defn email-to-id
  "Returns user ID for the given e-mail (cached)."
  [db email]
  (db/email-to-id db ids-cache get-user-id-by-email email))

(defn emails-to-ids
  "Returns user IDs for the given e-mails (cached)."
  [db emails]
  (db/emails-to-ids db ids-cache get-user-ids-by-emails emails))

(defn props-by-email
  "Returns user properties for the given e-mail (cached)."
  [db email]
  (props db (email-to-id db email)))

(defn prop-by-email
  "Returns user property identified by `prop-id` for the given e-mail (cached)."
  ([db prop-id email]
   (prop db prop-id (email-to-id db email)))
  ([db prop-id email & emails]
   (apply prop db prop-id (emails-to-ids db (cons email emails)))))

(defn id-to-email
  "Returns user e-mail for the given user ID (cached)."
  ([db id]
   (prop db :email id))
  ([db id & ids]
   (apply prop db :email id ids)))

(defn ids-to-emails
  "Returns user e-mails for the given user IDs (cached)."
  ([db ids]
   (db/get-cached-coll-prop props-cache info-getter-coll db :email ids)))

;; UID to ID mapping

(def ^:const uid-id-query
  "SELECT id FROM users WHERE uid = ?")

(def ^:const uids-ids-query
  "SELECT uid, id FROM users WHERE uid IN")

(defn get-user-id-by-uid
  "Returns user ID for the given user UID (not cached)."
  ([db uid]
   (db/get-id-by-uid db uid-id-query uid))
  ([db _ uid]
   (db/get-id-by-uid db uid-id-query uid)))

(defn get-user-ids-by-uids
  "Returns user IDs for the given user UIDs (not cached)."
  ([db uids]
   (db/get-ids-by-uids db uids-ids-query uids))
  ([db _ uids]
   (db/get-ids-by-uids db uids-ids-query uids)))

(defn uid-to-id
  "Returns user ID for the given user UID (cached)."
  [db uid]
  (db/uid-to-id db ids-cache get-user-id-by-uid uid))

(defn uids-to-ids
  "Returns user IDs for the given user UIDs (cached)."
  [db uids]
  (db/uids-to-ids db ids-cache get-user-ids-by-uids uids))

(defn props-by-uid
  "Returns user properties for the given user UID (cached)."
  [db uid]
  (props db (uid-to-id db uid)))

(defn id-to-uid
  "Returns user UID for the given user ID (cached)."
  ([db id]
   (prop db :uid id))
  ([db id & ids]
   (apply prop db :uid id ids)))

(defn ids-to-uids
  "Returns user UIDs for the given user IDs (cached)."
  ([db ids]
   (db/get-cached-coll-prop props-cache info-getter-coll db :uid ids)))

;; Existence testing (not cached)

(def ^:const id-exists-query
  "SELECT 1 FROM users WHERE id = ?")

(defn get-user-id-exists?
  [db id]
  (if (and db id)
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
  (if (and id (id-exists? db id)) id))

(defn some-uid
  [db uid]
  (if (and uid (uid-exists? db uid)) uid))

(defn some-email
  [db email]
  (if (and email (email-exists? db email)) email))

;; Creation

(def ^:const create-with-token-query
  (str-squeeze-spc
   "INSERT IGNORE INTO users(email,uid,account_type,first_name,middle_name,last_name,"
   "                         password,password_suite_id)"
   "SELECT id,UUID(),account_type,first_name,middle_name,last_name,"
   "       password,password_suite_id FROM confirmations"
   "WHERE token = ? AND confirmed = TRUE AND password IS NOT NULL"
   "                AND password_suite_id IS NOT NULL"
   "                AND reason = 'creation' AND expires >= NOW()"
   "RETURNING id,uid,email"))

(defn create-with-token
  [db token]
  (let [token (some-str token)]
    (if token
      (if-some [r (jdbc/execute-one! db [create-with-token-query token] db/opts-simple-map)]
        (qassoc r :created? true :uid (db/as-uuid (get r :uid)))
        (let [errs (confirmation/report-errors db token "creation" true)]
          {:created? false
           :errors   errs})))))

(def ^:const create-with-code-query
  (str-squeeze-spc
   "INSERT IGNORE INTO users(email,uid,account_type,first_name,middle_name,last_name,"
   "                         password,password_suite_id)"
   "SELECT id,UUID(),account_type,first_name,middle_name,last_name,"
   "       password,password_suite_id FROM confirmations"
   "WHERE code = ? AND id = ? AND confirmed = TRUE AND password IS NOT NULL"
   "      AND password_suite_id IS NOT NULL"
   "      AND reason = 'creation' AND  expires >= NOW()"
   "RETURNING id,uid,email"))

(defn create-with-code
  [db email code]
  (let [code  (some-str code)
        email (some-str email)]
    (if (and code email)
      (if-some [r (jdbc/execute-one! db [create-with-code-query code email] db/opts-simple-map)]
        (qassoc r :created? true :uid (db/as-uuid (get r :uid)))
        (let [errs (confirmation/report-errors db email code "creation" true)]
          {:created? false
           :errors   errs})))))

(defn create-with-token-or-code
  [db email token code]
  (if-some [token (some-str token)]
    (create-with-token db token)
    (create-with-code  db email code)))

;; Passwords and login data

(def ^:const password-query
  (str-spc "SELECT password AS intrinsic, suite AS shared FROM users, password_suites"
           "WHERE users.email = ? AND password_suites.id = users.password_suite_id"))

(def ^:const password-query-atypes-pre
  (str-spc "SELECT password AS intrinsic, suite AS shared FROM users, password_suites"
           "WHERE users.email = ? AND users.account_type"))

(def ^:const password-query-atypes-post
  " AND password_suites.id = users.password_suite_id")

(def ^:const password-query-atypes-single
  (str-spc "SELECT password AS intrinsic, suite AS shared FROM users, password_suites"
           "WHERE users.email = ? AND users.account_type = ?"
           "AND password_suites.id = users.password_suite_id"))

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

(def ^:const login-query-atypes-single
  (str-spc "SELECT password AS intrinsic, suite AS shared, users.id AS id, soft_locked, locked, account_type"
           "FROM users, password_suites"
           "WHERE users.email = ? AND users.account_type = ?"
           "AND password_suites.id = users.password_suite_id"))

(def ^:const ^AuthQueries login-data-queries
  (->AuthQueries login-query
                 login-query-atypes-pre
                 login-query-atypes-post
                 login-query-atypes-single))

(def ^:const ^AuthQueries password-data-queries
  (->AuthQueries password-query
                 password-query-atypes-pre
                 password-query-atypes-post
                 password-query-atypes-single))

(extend-protocol Authorizable

  AuthConfig
  (get-user-auth-data
    ([^AuthConfig src email ^AuthQueries queries]
     (if email
       (if-some [ac-types (.account-types ^AuthConfig src)]
         (if-some [db (.db ^AuthConfig src)]
           (let [ac-names (.names ^AccountTypes ac-types)
                 ac-sql   (.sql   ^AccountTypes ac-types)
                 ac-sql   (if ac-sql ac-sql (str " IN " (db/braced-join-? ac-names)))
                 query    (str (.pre ^AuthQueries queries) ac-sql (.post ^AuthQueries queries))]
             (jdbc/execute-one! db (cons query (cons email ac-names)) db/opts-simple-map))))))
    ([^AuthConfig src email ac-type ^AuthQueries queries]
     (if-some [ac-type (if (keyword? ac-type) ac-type (some-keyword ac-type))]
       (if email
         (if-some [db (.db ^AuthConfig src)]
           (if-some [ac-ids (.ids ^AccountTypes (.account-types ^AuthConfig src))]
             (if (contains? ac-ids ac-type)
               (jdbc/execute-one!
                db
                (cons (.single ^AuthQueries queries) (cons email (cons (name ac-type) nil)))
                db/opts-simple-map)))))
       (get-user-auth-data src email queries))))

  AuthSettings
  (get-user-auth-data
    ([^AuthSettings src email ^AuthQueries queries]
     (if email
       (let [db (.db ^AuthSettings src)]
         (if-some [ac-type (keyword (prop-by-email db :account-type email))]
           (get-user-auth-data (get (.types ^AuthSettings src) ac-type) email ac-type queries)))))
    ([^AuthSettings src email ac-type ^AuthQueries queries]
     (if-some [ac-type (if (keyword? ac-type) ac-type (some-keyword ac-type))]
       (if email
         (if-some [auth-config (get (.types ^AuthSettings src) ac-type)]
           (get-user-auth-data auth-config email ac-type queries)))
       (get-user-auth-data src email queries))))

  DataSource
  (get-user-auth-data
    ([^DataSource src email ^AuthQueries queries]
     (if email
       (jdbc/execute-one! src
                          (cons (.generic ^AuthQueries queries) (cons email nil))
                          db/opts-simple-map)))
    ([^DataSource src email ac-type ^AuthQueries queries]
     (if-some [ac-type (some-str ac-type)]
       (if email
         (jdbc/execute-one! src
                            (cons (.single ^AuthQueries queries) (cons email (cons ac-type nil)))
                            db/opts-simple-map))
       (get-user-auth-data src email queries)))))

(defn get-login-data
  "Returns data required for user to log in, including password information."
  ([^amelinium.model.user.Authorizable auth-source email]
   (get-user-auth-data auth-source email login-data-queries))
  ([^amelinium.model.user.Authorizable auth-source email account-type]
   (get-user-auth-data auth-source email account-type login-data-queries)))

(defn get-password-suites
  "Returns password information."
  ([^amelinium.model.user.Authorizable auth-source email]
   (get-user-auth-data auth-source email password-data-queries))
  ([^amelinium.model.user.Authorizable auth-source email account-type]
   (get-user-auth-data auth-source email account-type password-data-queries)))

(def ^:const insert-shared-suite-query
  (str-spc
   "INSERT INTO password_suites(suite) VALUES(?)"
   "ON DUPLICATE KEY UPDATE id=id"
   "RETURNING id"))

(def ^:const shared-suite-query
  "SELECT id FROM password_suites WHERE suite = ?")

(def ^:const shared-suite-by-id-query
  "SELECT suite FROM password_suites WHERE id = ?")

(defn create-or-get-shared-suite-id
  "Gets shared suite ID on a basis of its JSON content. If it does not exist, it is
  created. If the value of `suite` is a fixed-precision integer, it is returned."
  [db suite]
  (if (and db suite)
    (if (int? suite)
      suite
      (first
       (jdbc/execute-one! db [insert-shared-suite-query suite] db/opts-simple-vec)))))

(defn get-shared-suite-id
  "Gets shared suite ID on a basis of its JSON content. If the value of `suite` is a
  fixed-precision integer, it is returned."
  [db suite]
  (if (and db suite)
    (if (int? suite)
      suite
      (first
       (jdbc/execute-one! db [shared-suite-query suite] db/opts-simple-vec)))))

(defn get-shared-suite
  "Gets shared suite by its ID as a JSON string."
  [db suite-id]
  (if (and db suite-id)
    (first
     (jdbc/execute-one! db [shared-suite-by-id-query suite-id] db/opts-simple-vec))))

(defn prepare-password-suites
  "Creates a password suites without saving it into a database. Uses database to store
  the given, shared password suite if it does not exist yet. Returns a map with two
  keys: `:password` (JSON-encoded password ready to be saved into a database which
  should be given as an argument) and `:password-suite-id` (integer identifier of a
  shared suite ID which exists on a database)."
  ([db ^Suites suites]
   (if suites
     (prepare-password-suites db (.shared ^Suites suites) (.intrinsic ^Suites suites))))
  ([db shared-suite user-suite]
   (if (and db shared-suite user-suite)
     (if-some [shared-id (create-or-get-shared-suite-id db shared-suite)]
       (->DBPassword shared-id user-suite)))))

(defn generate-password
  "Creates a password for the given authentication config. Returns a map of shared part
  ID and an intrinsic part as two keys: `password-suite-id` and `:password`."
  [auth-config password]
  (if-some [db (.db ^AuthConfig auth-config)]
    (if-some [suites (auth/make-password-json password auth-config)]
      (let [shared-suite    (.shared    ^SuitesJSON suites)
            intrinsic-suite (.intrinsic ^SuitesJSON suites)]
        (if-some [shared-id (create-or-get-shared-suite-id db shared-suite)]
          (->DBPassword shared-id intrinsic-suite))))))

(defn update-password
  "Updates password information for the given user by updating suite ID and intrinsic
  password in an authorization database. Additionally last_attempt and last_failed_ip
  properties are deleted and login_attempts is set to 0."
  ([db id ^Suites suites]
   (if suites
     (update-password db id (.shared ^Suites suites) (.intrinsic ^Suites suites))))
  ([db id shared-suite user-suite]
   (if (and db id shared-suite user-suite)
     (if-some [shared-id (create-or-get-shared-suite-id db shared-suite)]
       (sql/update! db :users
                    {:password_suite_id shared-id
                     :password          user-suite
                     :last_attempt      nil
                     :last_failed_ip    nil
                     :login_attempts    0}
                    {:id id})))))

(defn update-login-ok
  [db id ip]
  (if (and db id)
    (let [login-time (t/now)]
      (sql/update! db :users {:login_attempts 1
                              :soft_locked    nil
                              :last_attempt   login-time
                              :last_login     login-time
                              :last_ok_ip     ip}
                   {:id id}))))

(def ^:const login-failed-update-query
  (str-spc
   "UPDATE users"
   "SET"
   "last_failed_ip = ?,"
   "login_attempts = 1 + GREATEST(login_attempts -"
   "  FLOOR(TIME_TO_SEC(TIMEDIFF(NOW(), last_attempt)) / ?),"
   "  0),"
   "last_attempt = NOW(),"
   "soft_locked = IF(login_attempts > ?, NOW(), soft_locked)"
   "WHERE id = ?"))

(def ^:const soft-lock-update-query
  (str-spc
   "UPDATE users"
   "SET soft_locked = NOW()"
   "WHERE id = ? AND login_attempts > ?"))

(defn update-login-failed
  "Updates `users` table with failed login data (attempts, IP address) according to
  authentication configuration and sets a soft lock if a number of attempts
  exceeded the configured value."
  ([auth-config user-id ip-address]
   (if auth-config
     (if-some [db (.db ^AuthConfig auth-config)]
       (if-some [locking (.locking ^AuthConfig auth-config)]
         (update-login-failed db user-id ip-address
                              (.max-attempts ^AuthLocking locking)
                              (.fail-expires ^AuthLocking locking))
         (update-login-failed db user-id ip-address nil nil)))))
  ([db user-id ip-address max-attempts attempt-expires-after-secs]
   (when (and db user-id)
     (jdbc/execute-one! db [login-failed-update-query
                            ip-address
                            (time/seconds attempt-expires-after-secs 10)
                            (or max-attempts 3)
                            user-id]))))

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
  "Gets user ID on a basis of a map with `:id` key or on a basis of a map with `:uid`
  key or on a basis of a number, a string or a keyword being ID, email or UID. User
  must exist in a database. Uses cached properties if possible."
  [db user-spec]
  (if (and db user-spec)
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
