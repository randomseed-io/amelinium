(ns

    ^{:doc    "amelinium service, user model."
      :author "Paweł Wilk"
      :added  "1.0.0"}

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
            [amelinium.auth                    :as          auth]
            [amelinium.auth.pwd                :as           pwd]
            [amelinium.identity                :as      identity]
            [amelinium.common                  :as        common]
            [amelinium.proto.auth              :as         pauth]
            [amelinium.proto.identity          :as           pid]
            [amelinium.proto.session           :as           sid]
            [amelinium.http.middleware.session :as       session]
            [amelinium.http.middleware.roles   :as         roles]
            [amelinium.model.confirmation      :as  confirmation]
            [amelinium                         :refer       :all]
            [amelinium.types.auth              :refer       :all]
            [amelinium.types.identity          :refer       :all]
            [amelinium.types.session           :refer       :all]
            [phone-number.core                 :as         phone]
            [io.randomseed.utils.time          :as          time]
            [io.randomseed.utils.ip            :as            ip]
            [io.randomseed.utils.map           :as           map]
            [io.randomseed.utils.map           :refer   [qassoc]]
            [io.randomseed.utils               :refer       :all])

  (:import [clojure.lang             Keyword]
           [phone_number.core        Phoneable]
           [amelinium.proto.auth     Authorizable]
           [amelinium.proto.identity Identifiable]
           [amelinium.proto.session  Sessionable]
           [amelinium                Suites SuitesJSON PasswordData]
           [amelinium                Identity Session UserData AuthQueries DBPassword]
           [amelinium                AuthConfig AuthSettings AuthLocking AuthConfirmation AccountTypes]
           [javax.sql                DataSource]
           [java.time                Duration]))

(defonce props-cache    (atom nil))
(defonce settings-cache (atom nil))
(defonce identity-cache (atom nil))

;; User data initialization

(declare create-or-get-shared-suite-id)

(defn make-user-password
  "Creates user password (of type `PasswordData`) on a basis of authentication source
  `auth-src`, a plain password string `password` and optional account type
  `account-type`.

  Authentication source can be a request map, `AuthSettings` record or `AuthConfig`
  record. If this is a request map then authentication settings are looked up under
  the `:auth/setup` key.

  The `account-type` argument is used to select authentication configuration (of type
  `AuthConfig`) specific to a user from the given or obtained authentication
  settings.

  If `account-type` argument is not given and the given authentication source is not
  of type `AuthConfig` then the default authentication configuration associated with
  detected authentication settings is used.

  If `account-type` argument is not given or is set to `nil` or `false`, and the
  given authentication source is already of type `AuthConfig` then it has no impact
  on the authentication configuration being used.

  If `account-type` argument is given and is not `nil` or `false`, and the given
  authentication source is already of type `AuthConfig` then it will be used only if
  it is configured to handle the given account type. If it is does not, password data
  record with `nil` values associated with each field is returned."
  (^PasswordData [auth-src ^String password]
   (make-user-password (auth/config auth-src) password nil))
  (^PasswordData [auth-src ^String password account-type]
   (let [^AuthConfig auth-config (auth/config auth-src account-type)
         ^SuitesJSON pwd-chains  (if password (auth/make-password-json password auth-config))
         ^String     pwd-shared  (if pwd-chains (.shared pwd-chains))
         ^DataSource db          (if auth-config (.db auth-config))
         ^Long       suite-id    (if (and db pwd-shared) (create-or-get-shared-suite-id db pwd-shared))]
     (if suite-id
       (->PasswordData pwd-shared (.intrinsic pwd-chains) suite-id)
       (->PasswordData nil nil nil)))))

(defn make-user-data
  "Creates user data record by getting values from the given authentication settings
  and parameters map. If `:password` parameter is present it will make JSON password
  suite."
  [^AuthSettings auth-settings params]
  (let [^String           email       (some-str (or (get params :user/email) (get params :email) (get params :login)))
        ^Phoneable        phone       (or (get params :user/phone) (get params :phone))
        ^String           password    (some-str     (or (get params :user/password) (get params :password)))
        account-type                  (some-keyword (or (get params :user/account-type) (get params :account-type)))
        account-type                  (or account-type (.default-type auth-settings))
        ^AuthConfig       auth-config (or (get (.types auth-settings) account-type) (.default auth-settings))
        ^AuthConfirmation auth-cfrm   (.confirmation auth-config)
        ^DataSource       db          (or (.db auth-config) (.db auth-settings))
        ^PasswordData     pwd-data    (make-user-password auth-config password account-type)]
    (->UserData email phone (name account-type) auth-config db
                (.intrinsic pwd-data) (.shared pwd-data) (.suite-id pwd-data)
                (some-str (or (get params :user/first-name)  (get params :first-name)))
                (some-str (or (get params :user/middle-name) (get params :middle-name)))
                (some-str (or (get params :user/last-name)   (get params :last-name)))
                (or (.expires      auth-cfrm) auth/confirmation-expires-default)
                (or (.max-attempts auth-cfrm) 3))))

(defn make-user-data-simple
  "Creates simple user data record (only db, phone and email) by getting values from
  the given authentication settings and parameters map."
  [auth-settings params]
  (let [email (some-str (or (get params :user/email) (get params :email) (get params :login)))
        phone (or (get params :user/phone) (get params :phone))
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

(defn get-user-by-uid
  "Given an UID, return the user record."
  [db uid]
  (if (pos-int? uid)
    (sql/get-by-id db :users (long uid) :uid db/opts-simple-map)))

;; Roles

(defn prop-get-roles
  ([smap-or-user-id opts context]
   (roles/filter-in-context context (prop-get-roles smap-or-user-id opts) opts))
  ([smap-or-user-id opts]
   (if (session/session? smap-or-user-id)
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
  [db ^Long user-id setting-id]
  (db/cached-setting-get settings-cache get-setting db user-id setting-id))

(defn setting-set
  ([db ^Long user-id setting-id value]
   (db/cached-setting-set settings-cache put-setting! db user-id setting-id value))
  ([db ^Long user-id setting-id value & pairs]
   (apply db/cached-setting-set settings-cache put-setting!
          db user-id setting-id value pairs)))

(defn setting-del
  ([db ^Long user-id]
   (db/cached-setting-del settings-cache del-setting! db user-id))
  ([db ^Long user-id setting-id]
   (db/cached-setting-del settings-cache del-setting! db user-id setting-id))
  ([db ^Long user-id setting-id & more]
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
  ([db id]          (info-coercer (info-getter-core db id)))
  ([db _ id]        (info-coercer (info-getter-core db nil id)))
  ([db _ id & more] (info-getter-coll db (cons id more))))

(def ^{:arglists '([db id keys-vals])}
  info-setter
  (db/make-setter :users :id))

(def ^{:arglists '([db id])}
  info-deleter
  (db/make-deleter :users :id))

(defn props-set
  "Sets properties of a user with the given ID."
  [db ^Long user-id keys-vals]
  (let [r (info-setter db user-id keys-vals)]
    (db/cache-evict! props-cache (long user-id)) r))

(defn props-del
  "Deletes all properties of a user with the given ID."
  [db ^Long user-id]
  (let [r (info-deleter db user-id)]
    (db/cache-evict! props-cache (long user-id)) r))

(defn prop-set
  "Sets property k of a user with the given ID to value v."
  [db ^Long user-id ^Keyword k v]
  (let [r (info-setter db user-id {k v})]
    (db/cache-evict! props-cache (long user-id)) r))

(defn prop-del
  "Deletes property of a user with the given ID by setting it to nil."
  [db ^Long user-id ^Keyword k]
  (prop-set db user-id k nil))

;; Getting user properties by...

(defn prop-by-id
  "Returns user property for the given user ID or a map of user properties keyed with
  their IDs if multiple IDs are given (cached)."
  ([db ^Keyword prop-id ^Long user-id]
   (if (and prop-id user-id)
     (db/get-cached-prop props-cache info-getter db prop-id user-id)))
  ([db ^Keyword prop-id ^Long user-id & ids]
   (if (and prop-id user-id)
     (db/get-cached-coll-prop props-cache info-getter-coll db prop-id (cons user-id ids)))))

(defn seq-prop-by-id
  "Returns user properties for the given user IDs (cached)."
  [db ^Keyword prop-id user-ids]
  (if (and prop-id user-ids)
    (db/get-cached-coll-prop props-cache info-getter-coll db prop-id user-ids)))

(defn prop-by-ids
  "Returns property for the given user IDs (cached)."
  [db prop-id ids]
  (seq-prop-by-id db prop-id ids))

(defn prop-by-id-or-default
  "Returns user property for the given user ID or a map of user property keyed with its
  ID if multiple IDs are given (cached). If the property is not found the default tag
  is returned instead of `nil`."
  ([db ^Keyword prop default ^Long id]
   (db/get-cached-prop-or-default props-cache info-getter db prop default id))
  ([db ^Keyword prop default ^Long id & ids]
   (apply db/get-cached-prop-or-default props-cache info-getter-coll
          db prop default id ids)))

(defn props-by-id
  "Returns user properties for the given user ID (cached)."
  ([db ^Long user-id]
   (if user-id (db/get-cached props-cache info-getter db user-id)))
  ([db ^Long user-id & ids]
   (db/get-cached-coll props-cache info-getter-coll db (cons user-id ids))))

(defn seq-props-by-id
  "Returns user properties for each of the given user IDs (cached)."
  [db ids]
  (db/get-cached-coll props-cache info-getter-coll db ids))

(defn props-by-ids
  "Returns user properties for each of the given user IDs (cached)."
  [db ids]
  (seq-props-by-id db ids))

;; Getting attribute by identity

(defn- ids-updater
  ([f]
   (fn [m ^Keyword id-type ids]
     (if id-type
       (or (some->> (not-empty ids) (f id-type) (into m)) m) m)))
  ([f a]
   (fn [m ^Keyword id-type ids]
     (if id-type
       (or (some->> (not-empty ids) (f a id-type) (into m)) m) m)))
  ([f a b]
   (fn [m ^Keyword id-type ids]
     (if id-type
       (or (some->> (not-empty ids) (f a b id-type) (into m)) m) m)))
  ([f a b c]
   (fn [m ^Keyword id-type ids]
     (if id-type
       (or (some->> (not-empty ids) (f a b c id-type) (into m)) m) m)))
  ([f a b c & more]
   (let [fargs (apply vector a b c more)]
     (fn [m ^Keyword id-type ids]
       (if id-type
         (or (some->> (not-empty ids) (conj fargs id-type) (apply f) (into m)) m) m)))))

(defn- some-identities
  ([user-identities]
   (->> (identity/of-seq user-identities) (filter identity) seq))
  ([^Keyword identity-type user-identities]
   (->> (identity/of-seq identity-type user-identities) (filter identity) seq)))

(defn- cache-lookup-user-id
  "Performs a cache lookup of user identity `user-identity` using a cache object
  `cache`. Returns a value being a result of a cache lookup and, if the entry
  identified by `user-identity` is not in a cache, returns `:amelinium.db/not-found`.

  Does not perform any pre-processing or post-processing of the `user-identity`."
  [cache ^Identity user-identity]
  (if user-identity
    (cwr/lookup cache user-identity ::db/not-found)))

(defn- cache-lookup-user-ids
  "Takes a cache object `cache` and a sequence of identities `identities`, and returns
  a map with keys and values found in the cache, plus a special key
  `:amelinium.db/not-found` with a list of keys which were not found associated to
  it.

  Does not perform any pre-processing or post-processing of the `user-identity`."
  [cache identities]
  (if (seq identities)
    (reduce (fn [m ^Identity user-identity]
              (let [id (cwr/lookup cache user-identity ::db/not-found)]
                (if (= ::db/not-found id)
                  (qassoc m ::db/not-found (conj (get m ::db/not-found) user-identity))
                  (qassoc m user-identity id))))
            {} identities)))

(defmulti query-id
  "Performs an ID-getting query for the given identity and identity type."
  {:arglists '([db ^Keyword identity-type user-identity])}
  (fn ^Keyword [db ^Keyword identity-type user-identity] (identity/check-type identity-type))
  :hierarchy #'pid/type-hierarchy)

(defmulti query-ids
  "Performs a multiple IDs-getting SQL query for the given identity and identity type."
  {:arglists '([db ^Keyword identity-type user-identities])}
  (fn ^Keyword [db ^Keyword identity-type user-identities] (identity/check-type identity-type))
  :hierarchy #'pid/type-hierarchy)

(defn query-id-std
  [db query user-identity]
  (if-some [dbs (identity/->db user-identity)]
    (first (jdbc/execute-one! db [query dbs] db/opts-simple-vec))))

(defn get-id
  "Takes a user identity and a database connectable object and returns a numerical user
  ID (not cached). Optional identity type will constrain the identity to be treated
  as it will be of certain type."
  ([db ^Identifiable user-identity]
   (if db
     (if-some [^Identity user-identity (identity/of user-identity)]
       (query-id db (identity/type user-identity) user-identity))))
  ([db ^Keyword identity-type ^Identifiable user-identity]
   (if-some [user-identity (identity/of-type identity-type user-identity)]
     (get-id db user-identity))))

(defn query-ids-std
  [db query user-identities]
  (let [db-ids (map identity/->db user-identities)]
    (->> db/opts-simple-vec
         (sql/query db (cons (str query " " (db/braced-join-? db-ids)) db-ids))
         next)))

(defn get-ids
  "Takes user identities and a database connectable object and returns a numerical user
  IDs (not cached). Optional identity type will constrain the identity to be treated
  as it will be of certain type. Returns a map with `amelinium.Identity` objects as
  keys and numerical identifiers as values."
  ([db user-identities]
   (if db
     (->> (some-identities user-identities)
          (group-by identity/type)
          (reduce-kv (ids-updater get-ids db) {}))))
  ([db ^Keyword identity-type user-identities]
   (if db
     (if-some [user-ids (some-identities identity-type user-identities)]
       (->> (query-ids db identity-type user-ids)
            (map #(vector (identity/of-type identity-type (nth % 0)) (nth % 1)))
            (into {}))))))

(defn- id-core
  ([^Boolean trust? db ^Identifiable user-identity]
   (if db
     (if-some [^Identity user-identity (identity/of user-identity)]
       (if (and trust? (= :id (.id-type user-identity)))
         (identity/value user-identity)
         (cwr/lookup-or-miss identity-cache user-identity #(get-id db %))))))
  ([^Boolean trust? db ^Keyword identity-type ^Identifiable user-identity]
   (if db
     (if-some [^Identity user-identity (identity/of-type identity-type user-identity)]
       (if (and trust? (= :id identity-type))
         (identity/value user-identity)
         (cwr/lookup-or-miss identity-cache user-identity #(get-id db identity-type %)))))))

(defn- ids-core
  ([^Boolean trust? db user-identities]
   (if db
     (->> (some-identities user-identities)
          (group-by identity/type)
          (reduce-kv (ids-updater ids-core trust? db) {}))))
  ([^Boolean trust? db ^Keyword identity-type user-identities]
   (if db
     (if-some [user-ids (some-identities identity-type user-identities)]
       (if (and trust? (= :id identity-type))
         (->> user-ids (map (juxt identity identity/value)) (into {}))
         (let [looked-up (cache-lookup-user-ids identity-cache user-ids)
               missing   (seq (get looked-up ::db/not-found))]
           (if-not missing
             looked-up
             (let [db-ids  (get-ids db identity-type user-ids)
                   present (dissoc looked-up ::db/not-found)]
               (reduce #(qassoc %1 %2 (cwr/lookup-or-miss identity-cache %2 db-ids))
                       present missing)))))))))

(defn id
  "Takes a user identity, optional identity type and a database connectable object, and
  returns a numerical user ID (cached).

  Optional identity type will constrain the identity to be treated as it will be of
  certain type."
  (^Long [db ^Identifiable user-identity]
   (id-core false db user-identity))
  (^Long [db ^Keyword identity-type ^Identifiable user-identity]
   (id-core false db identity-type user-identity)))

(defn id-of
  "Like `id` but `identity-type` is a first argument."
  ^Long [^Keyword identity-type db ^Identifiable user-identity]
  (id db identity-type user-identity))

(defn trusted-id
  "Takes a user identity, optional identity type and a database connectable object, and
  returns a numerical user ID (cached).

  When the given identity is of type `:id` (a numerical identifier), it will NOT
  interact with a database but simply trust that the ID exists and will simply return
  it.

  Optional identity type will constrain the identity to be treated as it will be of
  certain type."
  (^Long [db ^Identifiable user-identity]
   (id-core true db user-identity))
  (^Long [db ^Keyword identity-type ^Identifiable user-identity]
   (id-core true db identity-type user-identity)))

(defn trusted-id-of
  "Like `trusted-id` but `identity-type` is a first argument."
  ^Long [^Keyword identity-type db ^Identifiable user-identity]
  (trusted-id db identity-type user-identity))

(defn ids
  "Takes user identities, optional identity type and a database connectable object, and
  returns a map with `amelinium.Identity` keys and numerical user IDs values (cached).

  Optional identity type will constrain the identities to be treated as they will be
  of certain type."
  ([db user-identities]
   (ids-core false db user-identities))
  ([db ^Keyword identity-type user-identities]
   (ids-core false db identity-type user-identities)))

(defn ids-of
  "Like `ids` but `identity-type` is a first argument."
  ^Long [^Keyword identity-type db user-identities]
  (ids db identity-type user-identities))

(defn trusted-ids
  "Takes user identities, optional identity type and a database connectable object, and
  returns a map with `amelinium.Identity` keys and numerical user IDs values (cached).

  When any of the given identities is of type `:id` (a numerical identifier), it will
  NOT interact with a database to get the ID but simply trust that this ID exists and
  will simply put it into a map.

  Optional identity type will constrain the identities to be treated as they will be
  of certain type."
  ([db user-identities]
   (ids-core true db user-identities))
  ([db ^Keyword identity-type user-identities]
   (ids-core true db identity-type user-identities)))

(defn trusted-ids-of
  "Like `trusted-ids` but `identity-type` is a first argument."
  ^Long [^Keyword identity-type db user-identities]
  (trusted-ids db identity-type user-identities))

;; Existence testing (cached)

(defn exists?
  ([db ^Identifiable user-identity]
   (some? (id db user-identity)))
  ([db ^Keyword identity-type ^Identifiable user-identity]
   (some? (id identity-type user-identity))))

(defn existing
  ([db  ^Identifiable user-identity]
   (if-some [user-identity (identity/of user-identity)]
     (if (exists? user-identity) user-identity)))
  ([db ^Keyword identity-type  ^Identifiable user-identity]
   (if-some [user-identity (identity/of-type identity-type user-identity)]
     (if (exists? user-identity) user-identity))))

;; Generic identity mapping

(defn seq-props
  ([db ^Keyword identity-type ^Identifiable user-identities]
   (if-some [by-identity (trusted-ids db identity-type user-identities)]
     (if-some [by-id (seq-props-by-id db (vals by-identity))]
       (reduce-kv (fn [m user-identity user-id]
                    (if-some [p (get by-id user-id)]
                      (qassoc m user-identity p)
                      (dissoc m user-identity)))
                  by-identity by-identity))))
  ([db ^Identifiable user-identities]
   (if-some [by-identity (trusted-ids db user-identities)]
     (if-some [by-id (seq-props-by-id db (vals by-identity))]
       (reduce-kv (fn [m user-identity user-id]
                    (if-some [p (get by-id user-id)]
                      (qassoc m user-identity p)
                      (dissoc m user-identity)))
                  by-identity by-identity)))))

(defn props
  ([db ^Identifiable user-identity]
   (some->> (trusted-id db user-identity) (props-by-id db)))
  ([db ^Identifiable user-identity & identities]
   (seq-props db (cons user-identity identities))))

(defn props-of
  ([^Keyword identity-type db ^Identifiable user-identity]
   (some->> (trusted-id db identity-type user-identity) (props-by-id db)))
  ([^Keyword identity-type db ^Identifiable user-identity & identities]
   (seq-props db identity-type (cons user-identity identities))))

(defn seq-prop
  ([db ^Keyword prop-id user-identities]
   (if-some [prop-id (some-keyword prop-id)]
     (->> (seq-props db user-identities)
          (map/map-vals #(get % prop-id)))))
  ([db ^Keyword prop-id ^Keyword identity-type user-identities]
   (if-some [prop-id (some-keyword prop-id)]
     (->> (seq-props db identity-type user-identities)
          (map/map-vals #(get % prop-id))))))

(defn seq-prop-of
  [^Keyword identity-type db ^Keyword prop-id user-identities]
  (seq-prop db prop-id identity-type user-identities))

(defn prop
  ([db ^Keyword prop-id ^Identifiable user-identity]
   (some->> (some-keyword prop-id) (get (props db user-identity))))
  ([db ^Keyword prop-id ^Identifiable user-identity & identities]
   (seq-prop db prop-id (cons user-identity identities))))

(defn prop-of
  ([^Keyword identity-type db ^Keyword prop-id ^Identifiable user-identity]
   (some->> (some-keyword prop-id) (get (props-of identity-type db user-identity))))
  ([^Keyword identity-type db ^Keyword prop-id ^Identifiable user-identity & identities]
   (seq-prop db prop-id identity-type (cons user-identity identities))))

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
        (qassoc r :created? true :uid (identity/parse-uid (get r :uid)) :identity (get r :email))
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
        (qassoc r :created? true :uid (identity/parse-uid (get r :uid)) :identity (get r :email))
        (let [errs (confirmation/report-errors db email code "creation" true)]
          {:created? false
           :errors   errs})))))

(defn create-with-token-or-code
  [db email token code]
  (if-some [token (some-str token)]
    (create-with-token db token)
    (create-with-code  db email code)))

;; Identity management

(def ^:const update-email-with-token-query
  (str-squeeze-spc
   "INSERT IGNORE INTO users (id,email)"
   "SELECT requester_id,id FROM confirmations"
   "WHERE token = ? AND confirmed = TRUE AND user_id IS NULL AND user_uid IS NULL"
   "                AND reason = 'change' AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE email = VALUE(email)"
   "RETURNING id,uid,email,phone"))

(def ^:const update-phone-with-token-query
  (str-squeeze-spc
   "INSERT IGNORE INTO users (id,phone)"
   "SELECT requester_id,id FROM confirmations"
   "WHERE token = ? AND confirmed = TRUE AND user_id IS NULL AND user_uid IS NULL"
   "                AND reason = 'change' AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE phone = VALUE(phone)"
   "RETURNING id,uid,email,phone"))

(defn update-email-with-token
  [db token]
  (let [token (some-str token)]
    (if token
      (if-some [r (jdbc/execute-one! db [update-email-with-token-query token] db/opts-simple-map)]
        (qassoc r :updated? true :uid (identity/parse-uid (get r :uid)) :identity (get r :email))
        (let [errs (confirmation/report-errors db token "change" true)]
          {:updated? false
           :errors   errs})))))

(defn update-phone-with-token
  [db token]
  (let [token (some-str token)]
    (if token
      (if-some [r (jdbc/execute-one! db [update-phone-with-token-query token] db/opts-simple-map)]
        (qassoc r :updated? true :uid (identity/parse-uid (get r :uid)) :identity (get r :phone))
        (let [errs (confirmation/report-errors db token "change" true)]
          {:updated? false
           :errors   errs})))))

(def ^:const update-email-with-code-query
  (str-squeeze-spc
   "INSERT IGNORE INTO users (id,email)"
   "SELECT requester_id,id FROM confirmations"
   "WHERE code = ? AND id = ?"
   "               AND confirmed = TRUE AND user_id IS NULL AND user_uid IS NULL"
   "               AND reason = 'change' AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE email = VALUE(email)"
   "RETURNING id,uid,email,phone"))

(def ^:const update-phone-with-code-query
  (str-squeeze-spc
   "INSERT IGNORE INTO users (id,phone)"
   "SELECT requester_id,id FROM confirmations"
   "WHERE code = ? AND id = ?"
   "               AND confirmed = TRUE AND user_id IS NULL AND user_uid IS NULL"
   "               AND reason = 'change' AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE phone = VALUE(phone)"
   "RETURNING id,uid,email,phone"))

(defn update-email-with-code
  [db email code]
  (let [code  (some-str code)
        email (some-str email)]
    (if (and code email)
      (if-some [r (jdbc/execute-one! db [update-email-with-code-query code email] db/opts-simple-map)]
        (qassoc r :updated? true :uid (identity/parse-uid (get r :uid)) :identity (get r :email))
        (let [errs (confirmation/report-errors db email code "change" true)]
          {:updated? false
           :errors   errs})))))

(defn update-phone-with-code
  [db phone code]
  (let [code  (some-str code)
        phone (identity/->db phone)]
    (if (and code phone)
      (if-some [r (jdbc/execute-one! db [update-phone-with-code-query code phone] db/opts-simple-map)]
        (qassoc r :updated? true :uid (identity/parse-uid (get r :uid)) :identity (get r :phone))
        (let [errs (confirmation/report-errors db phone code "change" true)]
          {:updated? false
           :errors   errs})))))

(defn update-email-with-token-or-code
  [db email token code]
  (if-some [token (some-str token)]
    (update-email-with-token db token)
    (update-email-with-code  db email code)))

(defn update-phone-with-token-or-code
  [db phone token code]
  (if-some [token (some-str token)]
    (update-phone-with-token db token)
    (update-phone-with-code  db phone code)))

(defn update-identity-with-token
  [id-type db id token]
  (case id-type
    :email      (update-email-with-token db token)
    :phone      (update-phone-with-token db token)
    :user/email (update-email-with-token db token)
    :user/phone (update-phone-with-token db token)
    (update-email-with-token db token)))

(defn update-identity-with-code
  [id-type db id code]
  (case id-type
    :email      (update-email-with-code db id code)
    :phone      (update-phone-with-code db id code)
    :user/email (update-email-with-code db id code)
    :user/phone (update-phone-with-code db id code)
    (update-email-with-code db id code)))

(defn update-identity-with-token-or-code
  [id-type db id token code]
  (case id-type
    :email      (update-email-with-token-or-code db id token code)
    :phone      (update-phone-with-token-or-code db id token code)
    :user/email (update-email-with-token-or-code db id token code)
    :user/phone (update-phone-with-token-or-code db id token code)
    (update-email-with-token-or-code db id token code)))

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

(extend-protocol pauth/Authorizable

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
       (pauth/get-user-auth-data src email queries))))

  AuthSettings

  (get-user-auth-data
    ([^AuthSettings src email ^AuthQueries queries]
     (if email
       (let [db (.db ^AuthSettings src)]
         (if-some [ac-type (keyword (prop-of :email db :account-type email))]
           (pauth/get-user-auth-data (get (.types ^AuthSettings src) ac-type) email ac-type queries)))))
    ([^AuthSettings src email ac-type ^AuthQueries queries]
     (if-some [ac-type (if (keyword? ac-type) ac-type (some-keyword ac-type))]
       (if email
         (if-some [auth-config (get (.types ^AuthSettings src) ac-type)]
           (pauth/get-user-auth-data auth-config email ac-type queries)))
       (pauth/get-user-auth-data src email queries))))

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
       (pauth/get-user-auth-data src email queries)))))

(defn get-login-data
  "Returns data required for user to log in, including password information."
  ([^Authorizable auth-source email]
   (pauth/get-user-auth-data auth-source email login-data-queries))
  ([^Authorizable auth-source email account-type]
   (pauth/get-user-auth-data auth-source email account-type login-data-queries)))

(defn get-password-suites
  "Returns password information."
  ([^Authorizable auth-source email]
   (pauth/get-user-auth-data auth-source email password-data-queries))
  ([^Authorizable auth-source email account-type]
   (pauth/get-user-auth-data auth-source email account-type password-data-queries)))

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
  [^AuthConfig auth-config password]
  (if-some [db (.db auth-config)]
    (if-some [^SuitesJSON suites (auth/make-password-json password auth-config)]
      (let [shared-suite    (.shared    suites)
            intrinsic-suite (.intrinsic suites)]
        (if-some [shared-id (create-or-get-shared-suite-id db shared-suite)]
          (->DBPassword shared-id intrinsic-suite))))))

(defn update-password
  "Updates password information for the given user by updating suite ID and intrinsic
  password in an authorization database. Additionally `:last_attempt` and
  `:last_failed_ip` properties are deleted and `:login_attempts` is set to 0."
  ([db id ^Suites suites]
   (if suites
     (update-password db id (.shared suites) (.intrinsic suites))))
  ([db id shared-suite user-suite]
   (if (and db id shared-suite user-suite)
     (if-some [shared-id (create-or-get-shared-suite-id db shared-suite)]
       (::jdbc/update-count
        (sql/update! db :users
                     {:password_suite_id shared-id
                      :password          user-suite
                      :last_attempt      nil
                      :last_failed_ip    nil
                      :login_attempts    0}
                     {:id id}))))))

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

;; Session as identity

(pid/add-type! :session)

(defmethod identity/parser :session [_] session/of)
(defmethod query-id        :session [_ _ i] (session/user-id i))
(defmethod query-ids       :session [_ _ i] (map session/user-id i))

(defmethod identity/->str :session
  ([user-identity]   (session/any-id (pid/value user-identity)))
  ([t user-identity] (session/any-id (pid/value user-identity t))))

(defmethod identity/->db :session
  ([user-identity]   (session/db-sid (pid/value user-identity)))
  ([t user-identity] (session/db-sid (pid/value user-identity t))))

(extend-protocol pid/Identifiable

  Session

  (type ^Keyword [v] :session)

  (value
    (^Session [session]
     session)
    (^Session [session ^Keyword identity-type]
     (if (= :session identity-type) session)))

  (make
    (^Identity [session]
     (Identity. :session session))
    (^Identity [session ^Keyword identity-type]
     (if (= :session identity-type) (Identity. :session session)))))

;; Identity as a source of session

(extend-protocol sid/Sessionable

  Identity

  (session
    (^Session [src]   (pid/value src :session))
    (^Session [src _] (pid/value src :session)))

  (inject
    ([user-identity dst ^Keyword session-key] (sid/inject (sid/session user-identity) dst session-key))
    ([user-identity dst]                      (sid/inject (sid/session user-identity) dst)))

  (empty?
    (^Boolean [user-identity]                 (sid/empty? (sid/session user-identity)))
    (^Boolean [user-identity _]               (sid/empty? (sid/session user-identity))))

  (control
    (^SessionControl [user-identity]          (sid/control (sid/session user-identity)))
    (^SessionControl [user-identity _]        (sid/control (sid/session user-identity)))))

;; Authentication

(defn auth-by-user-id
  "Gets authentication configuration (`AuthConfig`) for the given user. Uses cached
  user props provided by `amelinium.model.user/props`."
  [settings-src user-id]
  (if-some [as (auth/settings settings-src)]
    (if-some [ac-type (prop-by-id (.db ^AuthSettings as) :account-type user-id)]
      (get (.types ^AuthSettings as) ac-type))))

(defn auth-by-session
  "Gets authentication configuration (`AuthConfig`) for the given user identified by a
  session object. Uses cached user props provided by `amelinium.model.user/props`."
  [settings-src smap]
  (if-some [as (auth/settings settings-src)]
    (if-some [ac-type (prop-of :session (.db ^AuthSettings as) :session smap)]
      (get (.types ^AuthSettings as) ac-type))))

(defn auth-config
  "Returns user authentication configuration for the user specified by `user-identity`
  and optional `identity-type` (which can be `:email`, `:phone`, `:id` or `:uid`). If
  the identity type is not given it will be guessed.

  The resulting map will be an authentication configuration (`AuthConfig` record)
  obtained for an account type associated with a user entry, or a generic
  authentication configuration if account type could not be found because either user
  does not exist in an authentication database or there is no account type specified
  for the user.

  The returned record will have extra entries present (available only via its map
  interface):

  - `:identity/type`     (detected or given identity type),
  - `:user/account-type` (detected account type or `nil` if it couldn't be obtained),
  - `:user/properties`   (user properties obtained when querying the database)."
  (^AuthConfig [req user-identity]
   (auth-config req user-identity nil))
  ([req user-identity identity-type]
   (if user-identity
     (if-some [identity-type (identity/type identity-type user-identity)]
       (if-some [^AuthSettings auth-settings (auth/settings req)]
         (let [auth-db                 (.db auth-settings)
               props                   (props-of identity-type auth-db user-identity)
               ac-type                 (if props (get props :account-type))
               ^AuthConfig auth-config (if ac-type (auth/config auth-settings ac-type))]
           (if-some [^AuthConfig auth-config (or auth-config (auth/config auth-settings))]
             (qassoc auth-config
                     :identity/type     identity-type
                     :user/account-type ac-type
                     :user/properties   props))))))))

;; Other

(defn prop-get-locked
  "Returns hard-lock status for the user account. Uses cached property."
  ([db user-id]
   (prop-by-id db :locked user-id))
  ([db user-id & ids]
   (apply prop-by-id db :locked user-id ids)))

(defn find-id
  "Gets user ID on a basis of a map with `:id` key or on a basis of a map with `:uid`
  key or on a basis of a number, a string or a keyword being ID, email, phone or
  UID. User must exist in a database. Uses cached properties if possible."
  [db user-spec]
  (if (and db user-spec)
    (if (map? user-spec)
      (some #(id db % user-spec) pid/valid-types)
      (id db user-spec))))

;; ID database getters for common identities

(defmethod query-id  :email [db _ i] (query-id-std  db "SELECT id FROM users WHERE email = ?" i))
(defmethod query-id  :phone [db _ i] (query-id-std  db "SELECT id FROM users WHERE phone = ?" i))
(defmethod query-id  :uid   [db _ i] (query-id-std  db "SELECT id FROM users WHERE uid   = ?" i))
(defmethod query-id  :id    [db _ i] (query-id-std  db "SELECT id FROM users WHERE id    = ?" i))

(defmethod query-ids :email [db _ i] (query-ids-std db "SELECT email, id FROM users WHERE email IN" i))
(defmethod query-ids :phone [db _ i] (query-ids-std db "SELECT phone, id FROM users WHERE phone IN" i))
(defmethod query-ids :uid   [db _ i] (query-ids-std db "SELECT uid,   id FROM users WHERE uid   IN" i))
(defmethod query-ids :id    [db _ i] (query-ids-std db "SELECT id,    id FROM users WHERE id    IN" i))
