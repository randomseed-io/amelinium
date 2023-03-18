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

  (:import (clojure.lang             Keyword)
           (phone_number.core        Phoneable)
           (amelinium.proto.auth     Authorizable)
           (amelinium.proto.identity Identifiable)
           (amelinium.proto.session  Sessionable)
           (amelinium                Suites
                                     SuitesJSON
                                     PasswordData
                                     Identity
                                     Session
                                     UserData
                                     AuthQueries
                                     DBPassword
                                     AuthConfig
                                     AuthSettings
                                     AuthLocking
                                     AuthConfirmation
                                     AccountTypes)
           (java.util                UUID)
           (javax.sql                DataSource)
           (java.time                Duration Instant)))

(defonce ^:redef props-cache    (atom nil))
(defonce ^:redef settings-cache (atom nil))
(defonce ^:redef identity-cache (atom nil))

;; Coercion

(defn- as-uuid         ^UUID    [u] (if (uuid? u) u (if u (uuid/as-uuid u))))
(defn- long-or-zero    ^Long    [n] (if n (long n) 0))
(defn- long-or-nil     ^Long    [n] (if n (long n)))
(defn- to-long-or-zero ^Long    [n] (safe-parse-long n 0))
(defn- to-instant      ^Instant [t] (if (t/instant? t) t (time/parse-dt t)))
(defn- id-to-db        ^Long    [v] (identity/to-db :id    v))
(defn- uid-to-db       ^UUID    [v] (identity/to-db :uid   v))
(defn- email-to-db     ^String  [v] (identity/to-db :email v))
(defn- phone-to-db     ^String  [v] (identity/to-db :phone v))

(db/defcoercions :users
  :id                id-to-db                     long
  :created-by        id-to-db                     long-or-nil
  :uid               uid-to-db                    as-uuid
  :email             email-to-db                  some-str
  :phone             phone-to-db                  identity/preparse-phone
  :account-type      some-str                     some-keyword
  :first-name        some-str                     some-str
  :middle-name       some-str                     some-str
  :last-name         some-str                     some-str
  :login-attempts    to-long-or-zero              long-or-zero
  :last-ok-ip        ip/to-address                ip/string-to-address
  :last-failed-ip    ip/to-address                ip/string-to-address
  :last-login        to-instant                   nil
  :last-attempt      to-instant                   nil
  :soft-locked       to-instant                   nil
  :locked            to-instant                   nil
  :created           to-instant                   nil
  :password-suite-id safe-parse-long              long-or-nil)

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
         ^String     pwd-shared  (if pwd-chains  (.shared pwd-chains))
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
  "Given a user ID, returns the user data in a form of lazy map."
  [db id]
  (if-some [id (parse-long id)]
    (db/lazy-get-by-id db :users id)))

(defn get-user-by-email
  "Given an email, returns the user data in a form of lazy map."
  [db email]
  (if-some [email (some-str email)]
    (db/lazy-get-by-id db :users email :email)))

(defn get-user-by-uid
  "Given a database connectable object `db` and a UID `uid`, returns
  the user data in a form of lazy map."
  [db uid]
  (if (pos-int? uid)
    (db/lazy-get-by-id db :users (long uid) :uid)))

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

(def ^:const prop-cols
  [:id :uid :email :account_type
   :first_name :last_name :middle_name :phone
   :login_attempts :last_ok_ip :last_failed_ip
   :last_attempt :last_login :created :created_by
   :soft_locked :locked])

(def ^{:private  true
       :arglists '([db ids] [db _ ids])}
  props-getter-coll
  (db/make-getter-coll db/lazy-execute! db/opts-lazy-simple-map
                       :users :id prop-cols))

(def ^{:private  true
       :arglists '([db id] [db _ id] [db _ id & ids])}
  props-getter
  (db/make-getter db/lazy-execute-one! db/opts-lazy-simple-map
                  :users :id prop-cols props-getter-coll))

(def ^{:private  true
       :arglists '([db id keys-vals])}
  props-setter
  (db/make-setter :users :id))

(def ^{:private  true
       :arglists '([db id])}
  props-deleter
  (db/make-deleter :users :id))

(defn props-set
  "Sets properties of a user identified with the given ID `user-id`."
  [db ^Long user-id keys-vals]
  (let [r (props-setter db user-id keys-vals)]
    (db/cache-evict! props-cache (db/<- :users/id user-id)) r))

(defn props-del
  "Deletes all properties of a user identified with the given ID  `user-id`."
  [db ^Long user-id]
  (let [r (props-deleter db user-id)]
    (db/cache-evict! props-cache (db/<- :users/id user-id)) r))

(defn prop-set
  "Sets property `k` of a user identified with the given ID `user-id` to value `v`."
  [db ^Long user-id ^Keyword k v]
  (let [r (props-setter db user-id {k v})]
    (db/cache-evict! props-cache (db/<- :users/id user-id)) r))

(defn prop-del
  "Deletes property of a user with the given ID `user-id` by setting it to `nil`."
  [db ^Long user-id ^Keyword k]
  (prop-set db user-id k nil))

;; Getting user properties by...

(defn prop-by-id
  "Returns user property `prop-id` for the given user ID `user-id` or a map of user
  properties keyed with their IDs if multiple IDs are given (cached)."
  ([db ^Keyword prop-id ^Long user-id]
   (if (and prop-id user-id)
     (db/get-cached-prop props-cache props-getter db prop-id user-id)))
  ([db ^Keyword prop-id ^Long user-id & ids]
   (if (and prop-id user-id)
     (db/get-cached-coll-prop props-cache props-getter-coll db prop-id (cons user-id ids)))))

(defn seq-prop-by-id
  "Returns user property `prop-id` for the given user IDs `user-ids` (cached)."
  [db ^Keyword prop-id user-ids]
  (if (and prop-id user-ids)
    (db/get-cached-coll-prop props-cache props-getter-coll db prop-id user-ids)))

(defn prop-by-ids
  "Returns property `prop-id` for the given user IDs `ids` (cached)."
  [db prop-id ids]
  (seq-prop-by-id db prop-id ids))

(defn prop-by-id-or-default
  "Returns user property `prop` for the given user ID `id` or a map of user property
  keyed with its ID if multiple IDs are given (cached). If the property is not found
  the default value, given as `default`, is returned instead of `nil`."
  ([db ^Keyword prop default ^Long id]
   (db/get-cached-prop-or-default props-cache props-getter db prop default id))
  ([db ^Keyword prop default ^Long id & ids]
   (apply db/get-cached-prop-or-default props-cache props-getter-coll
          db prop default id ids)))

(defn props-by-id
  "Returns user properties for the given user ID `user-id` (cached)."
  ([db ^Long user-id]
   (if user-id (db/get-cached props-cache props-getter db user-id)))
  ([db ^Long user-id & ids]
   (db/get-cached-coll props-cache props-getter-coll db (cons user-id ids))))

(defn seq-props-by-id
  "Returns user properties for each of the given user IDs `ids` (cached)."
  [db ids]
  (db/get-cached-coll props-cache props-getter-coll db ids))

(defn props-by-ids
  "Returns user properties for each of the given user IDs `ids` (cached)."
  [db ids]
  (seq-props-by-id db ids))

;; Getting attribute by identity

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
                (if (identical? ::db/not-found id)
                  (qassoc m ::db/not-found (conj (get m ::db/not-found) user-identity))
                  (qassoc m user-identity id))))
            {} identities)))

(defmulti query-id
  "Performs an ID-getting query for the given identity and identity type (must be a
  keyword). This method is used to get user ID for the given identity of a known
  type."
  {:see-also ["get-id" "query-ids"]
   :arglists '([db ^Keyword identity-type user-identity])}
  (fn ^Keyword [db ^Keyword identity-type user-identity] identity-type)
  :hierarchy #'pid/type-hierarchy)

(defmulti query-ids
  "Performs a multiple IDs-getting SQL query for the given identity and identity
  type (must be a keyword). This method is used to get user IDs for the given
  identities of known types."
  {:see-also ["get-ids" "query-id"]
   :arglists '([db ^Keyword identity-type user-identities])}
  (fn ^Keyword [db ^Keyword identity-type user-identities] identity-type)
  :hierarchy #'pid/type-hierarchy)

(defn query-id-std
  "Converts the given user identity `user-identity` to a database-suitable value and
  then performs a SQL query `query` with the obtained value as a parameter. Returns
  query result as vector. This is a standard querying function applicable to many
  simple database schemas. Used primarily as a default operation in method
  definitions of `query-id`."
  {:see-also ["query-id" "get-id"]}
  [db colspec query user-identity]
  (if-some [dbs (db/<- colspec user-identity)]
    (first (db/execute-one! db [query dbs] db/opts-simple-vec))))

(defn get-id
  "Takes a user identity `user-identity` and a database connectable object and returns
  a numerical user ID (not cached). Optional identity type will constrain the
  identity to be treated as it will be of certain type.

  This function is using `query-id` to perform the actual query on a database using
  identity record created with `amelinium.identify/of` or
  `amelinium.identity/of-type`.

  This function is used by cache-backed functions `id`, `id-of`, `trusted-id` and
  `trusted-id-of` to communicate with a database."
  {:see-also ["query-id"]}
  (^Long [db ^Identifiable user-identity]
   (if db
     (if-some [^Identity user-identity (identity/of user-identity)]
       (query-id db (identity/type user-identity) user-identity))))
  (^Long [db ^Keyword identity-type ^Identifiable user-identity]
   (if-some [user-identity (identity/of-type identity-type user-identity)]
     (get-id db user-identity))))

(defn query-ids-std
  "Converts the given user identities `user-identities` to a database-suitable values
  and then performs a SQL query `query` with the obtained value as parameters added
  using \"IN(...)\" clause. Returns query result as vector. Used primarily as a
  default operation in method definitions of `query-ids`."
  {:see-also ["query-ids" "get-ids"]}
  [db colspec query user-identities]
  (let [db-ids (db/<-seq colspec user-identities)]
    (->> db/opts-simple-vec
         (sql/query db (cons (str query " " (db/braced-join-? db-ids)) db-ids))
         next)))

(defn get-ids
  "Takes user identities and a database connectable object, and returns a numerical
  user IDs (not cached). Optional identity type will constrain the identity to be
  treated as it will be of certain type. Returns a map with `amelinium.Identity`
  objects as keys and numerical identifiers as values.

  This function is using `query-ids` to perform the actual query on a database using
  identity records created with `amelinium.identify/of` or
  `amelinium.identity/of-type`.

  This function is used by cache-backed functions `ids`, `ids-of`, `trusted-ids` and
  `trusted-ids-of` to communicate with a database."
  {:see-also ["query-ids" "get-id"]}
  ([db user-identities]
   (if db
     (->> (identity/some-seq user-identities)
          (group-by identity/type)
          (reduce-kv (db/groups-inverter get-ids db) {}))))
  ([db ^Keyword identity-type user-identities]
   (if db
     (if-some [user-ids (identity/some-seq identity-type user-identities)]
       (->> (query-ids db identity-type user-ids)
            (map #(vector (identity/of-type identity-type (nth % 0)) (nth % 1)))
            (into {}))))))

(defn- id-core
  (^Long [^Boolean trust? db ^Identifiable user-identity]
   (if db
     (if-some [^Identity user-identity (identity/of user-identity)]
       (if (and trust? (identical? :id (.id-type user-identity)))
         (identity/value user-identity)
         (cwr/lookup-or-miss identity-cache user-identity #(get-id db %))))))
  (^Long [^Boolean trust? db ^Keyword identity-type ^Identifiable user-identity]
   (if db
     (if-some [^Identity user-identity (identity/of-type identity-type user-identity)]
       (if (and trust? (identical? :id identity-type))
         (identity/value user-identity)
         (cwr/lookup-or-miss identity-cache user-identity #(get-id db identity-type %)))))))

(defn- ids-core
  ([^Boolean trust? db user-identities]
   (if db
     (->> (identity/some-seq user-identities)
          (group-by identity/type)
          (reduce-kv (db/groups-inverter ids-core trust? db) {}))))
  ([^Boolean trust? db ^Keyword identity-type user-identities]
   (if db
     (if-some [user-ids (identity/some-seq identity-type user-identities)]
       (if (and trust? (identical? :id identity-type))
         (->> user-ids (map (juxt identity identity/->db)) (into {}))
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
  {:see-also ["id-of" "trusted-id" "query-id" "get-id"]}
  (^Long [db ^Identifiable user-identity]
   (id-core false db user-identity))
  (^Long [db ^Keyword identity-type ^Identifiable user-identity]
   (id-core false db identity-type user-identity)))

(defn id-of
  "Like `id` but `identity-type` is a first argument."
  {:see-also ["id" "trusted-id-of" "query-id" "get-id"]}
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
  {:see-also ["id" "trusted-id-of" "id-of" "query-id" "get-id"]}
  (^Long [db ^Identifiable user-identity]
   (id-core true db user-identity))
  (^Long [db ^Keyword identity-type ^Identifiable user-identity]
   (id-core true db identity-type user-identity)))

(defn trusted-id-of
  "Like `trusted-id` but `identity-type` is a first argument."
  {:see-also ["id" "trusted-id" "id-of" "query-id" "get-id"]}
  ^Long [^Keyword identity-type db ^Identifiable user-identity]
  (trusted-id db identity-type user-identity))

(defn ids
  "Takes user identities, optional identity type and a database connectable object, and
  returns a map with `amelinium.Identity` keys and numerical user IDs values (cached).

  Optional identity type will constrain the identities to be treated as they will be of
  certain type."
  {:see-also ["ids-of" "trusted-ids" "query-ids" "get-ids"]}
  ([db user-identities]
   (ids-core false db user-identities))
  ([db ^Keyword identity-type user-identities]
   (ids-core false db identity-type user-identities)))

(defn ids-of
  "Like `ids` but `identity-type` is a first argument."
  {:see-also ["ids" "trusted-ids-of" "query-ids" "get-ids"]}
  ^Long [^Keyword identity-type db user-identities]
  (ids db identity-type user-identities))

(defn trusted-ids
  "Takes user identities, optional identity type and a database connectable object, and
  returns a map with `amelinium.Identity` keys and numerical user IDs
  values (cached).

  When any of the given identities is of type `:id` (a numerical identifier), it will
  NOT interact with a database to get the ID but simply trust that this ID exists and
  will simply put it into a map.

  Optional identity type will constrain the identities to be treated as they will be
  of certain type."
  {:see-also ["trusted-ids-of" "ids" "ids-of" "query-ids" "get-ids"]}
  ([db user-identities]
   (ids-core true db user-identities))
  ([db ^Keyword identity-type user-identities]
   (ids-core true db identity-type user-identities)))

(defn trusted-ids-of
  "Like `trusted-ids` but `identity-type` is a first argument."
  {:see-also ["trusted-ids" "ids" "ids-of" "query-ids" "get-ids"]}
  ^Long [^Keyword identity-type db user-identities]
  (trusted-ids db identity-type user-identities))

;; Existence testing (cached)

(defn exists?
  "Returns `true` if a user specified by the given `user-identity` with optional
  `identity-type` exists. Returns `false` otherwise."
  (^Boolean [db ^Identifiable user-identity]
   (some? (id db user-identity)))
  (^Boolean [db ^Keyword identity-type ^Identifiable user-identity]
   (some? (id identity-type user-identity))))

(defn existing
  "Returns user identity record (of type `amelinium.Identity`) if a user specified by
  the given `user-identity` with optional `identity-type` exists. Otherwise it
  returns `nil`."
  (^Identity [db ^Identifiable user-identity]
   (if-some [user-identity (identity/of user-identity)]
     (if (exists? user-identity) user-identity)))
  (^Identity [db ^Keyword identity-type  ^Identifiable user-identity]
   (if-some [user-identity (identity/of-type identity-type user-identity)]
     (if (exists? user-identity) user-identity))))

;; Generic identity mapping

(defn seq-props
  "For the given database connectable object `db`, optional identity type
  `identity-type` and identifiable object `user-identity`, returns users' properties
  as map keyed with `amelinium.Identity` records. The given identities must be of the
  given type if the type is given. (cached)"
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
  "For the given database connectable object `db` and identifiable object
  `user-identity`, returns user's properties as a map. If multiple identities are
  given, returns a sequence of maps. (cached)"
  ([db ^Identifiable user-identity]
   (some->> (trusted-id db user-identity) (props-by-id db)))
  ([db ^Identifiable user-identity & identities]
   (seq-props db (cons user-identity identities))))

(defn props-of
  "For the given database connectable object `db`, identity type `identity-type` and
  identifiable object `user-identity`, returns user's properties as a map. If
  multiple identities are given, returns a map keyed with `amelinium.Identity`
  records. The given identity must be of the given type. (cached)"
  ([^Keyword identity-type db ^Identifiable user-identity]
   (some->> (trusted-id db identity-type user-identity) (props-by-id db)))
  ([^Keyword identity-type db ^Identifiable user-identity & identities]
   (seq-props db identity-type (cons user-identity identities))))

(defn seq-prop
  "For the given database connectable object `db`, property identifier `prop-id`,
  optional identity type `identity-type` and identifiable objects `user-identities`,
  returns a map of `amelinium.Identity` records associated with selected property
  values. Each given identity must be of the given type if `identity-type` is
  used. (cached)"
  ([db ^Keyword prop-id user-identities]
   (if-some [prop-id (some-keyword prop-id)]
     (->> (seq-props db user-identities)
          (map/map-vals #(get % prop-id)))))
  ([db ^Keyword prop-id ^Keyword identity-type user-identities]
   (if-some [prop-id (some-keyword prop-id)]
     (->> (seq-props db identity-type user-identities)
          (map/map-vals #(get % prop-id))))))

(defn seq-prop-of
  "The same as `seq-prop` but `identity-type` is a mandatory first argument. (cached)"
  [^Keyword identity-type db ^Keyword prop-id user-identities]
  (seq-prop db prop-id identity-type user-identities))

(defn prop
  "For the given database connectable object `db`, property identifier `prop-id`,
  and identifiable object `user-identity`, returns user's property. If multiple
  identities are given, returns a map of `amelinium.Identity` records associated with
  selected property values. (cached)"
  ([db ^Keyword prop-id ^Identifiable user-identity]
   (some->> (some-keyword prop-id) (get (props db user-identity))))
  ([db ^Keyword prop-id ^Identifiable user-identity & identities]
   (seq-prop db prop-id (cons user-identity identities))))

(defn prop-of
  "For the given identity type `identity-type`, database connectable object `db`,
  property identifier `prop-id`, and identifiable object `user-identity`, returns
  user's property. If multiple identities are given, returns a map of
  `amelinium.Identity` records associated with selected property values. Identity
  type(s) must be of the given type. (cached)"
  ([^Keyword identity-type db ^Keyword prop-id ^Identifiable user-identity]
   (some->> (some-keyword prop-id) (get (props-of identity-type db user-identity))))
  ([^Keyword identity-type db ^Keyword prop-id ^Identifiable user-identity & identities]
   (seq-prop db prop-id identity-type (cons user-identity identities))))

;; Creation

(def ^:const create-with-token-query
  (db/build-query
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
      (if-some [r (db/execute-one! db [create-with-token-query token])]
        (qassoc r :created? true :identity (identity/of-type :email (get r :email)))
        (let [errs (confirmation/report-errors db token "creation" true)]
          {:created? false
           :errors   errs})))))

(def ^:const create-with-code-query
  (db/build-query
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
      (if-some [r (db/execute-one! db [create-with-code-query code email])]
        (qassoc r :created? true :identity (identity/of-type (get r :email)))
        (let [errs (confirmation/report-errors db email code "creation" true)]
          {:created? false
           :errors   errs})))))

(defn create
  "Creates new user account identified with the given e-mail `email` by checking if it
  was successfully confirmed. Token (`token`) or code (`code`) must be provided to
  authorize operation. Returns a map with `:created?` set to `true` if user account
  is created."
  [db email token code]
  (if-some [token (some-str token)]
    (create-with-token db token)
    (create-with-code  db email code)))

;; Identity management

(def update-identity-query-token
  (db/build-query
   "INSERT IGNORE INTO users (id, %(identity))"
   "SELECT requester_id,id FROM confirmations"
   "WHERE token = ?"
   "      AND confirmed = TRUE AND user_id IS NULL AND user_uid IS NULL"
   "      AND reason    = 'change' AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE  %(identity) = VALUE(%(identity))"
   "RETURNING id,uid,%(identity)"))

(defn update-identity-with-token
  [identity-type db token]
  (if-some [identity-col (db/column-kw identity-type)]
    (if-some [r (db/<exec-one! db [update-identity-query-token {:identity identity-col}]
                               [:confirmations token])]
      (qassoc r
              :updated? true
              :identity (identity/opt-type identity-type (get r identity-col)))
      {:updated? false
       :errors   (confirmation/report-errors db token "change" true)})))

(def update-identity-query-code
  (db/build-query
   "INSERT IGNORE INTO users (id, %(identity))"
   "SELECT requester_id,id FROM confirmations"
   "WHERE code      = ?"
   "  AND id        = ?"
   "  AND confirmed = TRUE AND user_id IS NULL AND user_uid IS NULL"
   "  AND reason    = 'change' AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE %(identity) = VALUE(%(identity))"
   "RETURNING id,uid,%(identity)"))

(defn update-identity-with-code
  [identity-type db code user-identity]
  (if-some [id (identity/opt-type identity-type user-identity)]
    (if-some [identity-col (db/column-kw identity-type)]
      (if-some [r (db/<exec-one! db [update-identity-query-code {:identity identity-col}]
                                 [:confirmations code id])]
        (qassoc r
                :updated? true
                :identity (identity/opt-type identity-type (get r identity-col)))
        {:updated? false
         :errors   (confirmation/report-errors db id code "change" true)}))))

(defn update-identity
  "Updates user's identity identified by `user-identity` and `code`, or by
  `token`. Code or token must exist in a database."
  ([identity-type db token]
   (update-identity-with-token identity-type db token))
  ([identity-type db code user-identity]
   (if user-identity
     (update-identity-with-code identity-type db code user-identity)
     (update-identity identity-type db code))))

;; Passwords and login data

(def ^:const password-query
  (db/build-query "SELECT password AS intrinsic, suite AS shared FROM users, password_suites"
                  "WHERE users.email = ? AND password_suites.id = users.password_suite_id"))

(def ^:const password-query-atypes-pre
  (db/build-query "SELECT password AS intrinsic, suite AS shared FROM users, password_suites"
                  "WHERE users.email = ? AND users.account_type"))

(def ^:const password-query-atypes-post
  " AND password_suites.id = users.password_suite_id")

(def ^:const password-query-atypes-single
  (db/build-query "SELECT password AS intrinsic, suite AS shared FROM users, password_suites"
                  "WHERE users.email = ? AND users.account_type = ?"
                  "AND password_suites.id = users.password_suite_id"))

(def ^:const login-query
  (db/build-query "SELECT password AS intrinsic, suite AS shared, users.id AS id, soft_locked, locked, account_type"
                  "FROM users, password_suites"
                  "WHERE users.email = ? AND password_suites.id = users.password_suite_id"))

(def ^:const login-query-atypes-pre
  (db/build-query "SELECT password AS intrinsic, suite AS shared, users.id AS id, soft_locked, locked, account_type"
                  "FROM users, password_suites"
                  "WHERE users.email = ? AND users.account_type"))

(def ^:const login-query-atypes-post
  " AND password_suites.id = users.password_suite_id")

(def ^:const login-query-atypes-single
  (db/build-query "SELECT password AS intrinsic, suite AS shared, users.id AS id, soft_locked, locked, account_type"
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
  (db/build-query
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
  (db/build-query
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
  (db/build-query
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

(defmethod identity/to-str :session
  ([user-identity]   (session/any-id (pid/value user-identity)))
  ([t user-identity] (session/any-id (pid/value user-identity t))))

(defmethod identity/to-db :session
  ([user-identity]   (session/db-sid (pid/value user-identity)))
  ([t user-identity] (session/db-sid (pid/value user-identity t))))

(extend-protocol pid/Identifiable

  Session

  (type ^Keyword [v] :session)

  (value
    (^Session [session]
     session)
    (^Session [session ^Keyword identity-type]
     (if (identical? :session identity-type) session)))

  (make
    (^Identity [session]
     (Identity. :session session))
    (^Identity [session ^Keyword identity-type]
     (if (identical? :session identity-type) (Identity. :session session)))))

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
  user props provided by `amelinium.model.user/prop-by-id`."
  [settings-src user-id]
  (if-some [as (auth/settings settings-src)]
    (if-some [ac-type (prop-by-id (.db ^AuthSettings as) :account-type user-id)]
      (get (.types ^AuthSettings as) ac-type))))

(defn auth-by-session
  "Gets authentication configuration (`AuthConfig`) for the given user identified by a
  session object. Uses cached user props provided by `amelinium.model.user/prop-of`."
  [settings-src smap]
  (if-some [as (auth/settings settings-src)]
    (if-some [ac-type (prop-of :session (.db ^AuthSettings as) :account-type smap)]
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
  "Using the database connectable object `db`, returns the hard-lock status for the
  user account of the given ID `user-id`. Uses cached property."
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

(defmethod query-id  :email [db _ i] (query-id-std  db :users/email "SELECT id FROM users WHERE email = ?" i))
(defmethod query-id  :phone [db _ i] (query-id-std  db :users/phone "SELECT id FROM users WHERE phone = ?" i))
(defmethod query-id  :uid   [db _ i] (query-id-std  db :users/uid   "SELECT id FROM users WHERE uid   = ?" i))
(defmethod query-id  :id    [db _ i] (query-id-std  db :users/id    "SELECT id FROM users WHERE id    = ?" i))

(defmethod query-ids :email [db _ i] (query-ids-std db :users/email "SELECT email, id FROM users WHERE email IN" i))
(defmethod query-ids :phone [db _ i] (query-ids-std db :users/phone "SELECT phone, id FROM users WHERE phone IN" i))
(defmethod query-ids :uid   [db _ i] (query-ids-std db :users/uid   "SELECT uid,   id FROM users WHERE uid   IN" i))
(defmethod query-ids :id    [db _ i] (query-ids-std db :users/id    "SELECT id,    id FROM users WHERE id    IN" i))

;; Wrappers for commonly used properties

(defn account-type
  "For the given database connectable object `db` and user identity `user-identity`
  returns an account type of the existing user. Optional `identity-type` can be given
  to constrain the given identity."
  ([db user-identity] (prop db :account-type user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :account-type user-identity)))

(defn first-name
  "For the given database connectable object `db` and user identity `user-identity`
  returns a first name of the existing user. Optional `identity-type` can be given to
  constrain the given identity."
  ([db user-identity] (prop db :first-name user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :first-name user-identity)))

(defn last-name
  "For the given database connectable object `db` and user identity `user-identity`
  returns a last name of the existing user. Optional `identity-type` can be given to
  constrain the given identity."
  ([db user-identity] (prop db :last-name user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :last-name user-identity)))

(defn middle-name
  "For the given database connectable object `db` and user identity `user-identity`
  returns a middle name of the existing user. Optional `identity-type` can be given to
  constrain the given identity."
  ([db user-identity] (prop db :middle-name user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :middle-name user-identity)))

(defn login-attempts
  "For the given database connectable object `db` and user identity `user-identity`
  returns a number of login attempts of the existing user. Optional `identity-type`
  can be given to constrain the given identity."
  ([db user-identity] (prop db :login-attempts user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :login-attempts user-identity)))

(defn last-ok-ip
  "For the given database connectable object `db` and user identity `user-identity`
  returns a last IP address from which the user has logged in successfully. Optional
  `identity-type` can be given to constrain the given identity."
  ([db user-identity] (prop db :last-ok-ip user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :last-ok-ip user-identity)))

(defn last-failed-ip
  "For the given database connectable object `db` and user identity `user-identity`
  returns a last IP address from which the user has failed to log in
  successfully. Optional `identity-type` can be given to constrain the given
  identity."
  ([db user-identity] (prop db :last-failed-ip user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :last-failed-ip user-identity)))

(defn last-attempt
  "For the given database connectable object `db` and user identity `user-identity`
  returns a time of last log-in attempt of the existing user. Optional
  `identity-type` can be given to constrain the given identity."
  ([db user-identity] (prop db :last-attempt user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :last-attempt user-identity)))

(defn last-login
  "For the given database connectable object `db` and user identity `user-identity`
  returns a time of last successful log-in of the existing user. Optional
  `identity-type` can be given to constrain the given identity."
  ([db user-identity] (prop db :last-login user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :last-login user-identity)))

(defn soft-locked
  "For the given database connectable object `db` and user identity `user-identity`
  returns a time of the account's soft-lock (if any). Optional `identity-type` can be
  given to constrain the given identity."
  ([db user-identity] (prop db :soft-locked user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :soft-locked user-identity)))

(defn hard-locked
  "For the given database connectable object `db` and user identity `user-identity`
  returns a time of the account's hard-lock (if any). Optional `identity-type` can be
  given to constrain the given identity."
  ([db user-identity] (prop db :locked user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :locked user-identity)))

(def locked hard-locked)
(reset-meta! #'locked (meta #'hard-locked))

(defn created
  "For the given database connectable object `db` and user identity `user-identity`
  returns the existing user's creation time. Optional `identity-type` can be given to
  constrain the given identity."
  ([db user-identity] (prop db :created user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :created user-identity)))

(defn created-by
  "For the given database connectable object `db` and user identity `user-identity`
  returns the existing user's creator ID. Optional `identity-type` can be given to
  constrain the given identity."
  ([db user-identity] (prop db :created-by user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :created-by user-identity)))

(defn email
  "For the given database connectable object `db` and user identity `user-identity`
  returns an e-mail of the existing user. Optional `identity-type` can be given to
  constrain the given identity."
  ([db user-identity] (prop db :email user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :email user-identity)))

(defn phone
  "For the given database connectable object `db` and user identity `user-identity`
  returns a phone number of the existing user. Optional `identity-type` can be given
  to constrain the given identity."
  ([db user-identity] (prop db :phone user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :phone user-identity)))

(defn uid
  "For the given database connectable object `db` and user identity `user-identity`
  returns a UID of the existing user. Optional `identity-type` can be given to
  constrain the given identity."
  ([db user-identity] (prop db :uid user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :uid user-identity)))

(defn session
  "For the given database connectable object `db` and user identity `user-identity`
  returns a session of the existing user. Optional `identity-type` can be given to
  constrain the given identity.

  Be aware that this operation will only work if the identity type is a
  session. There is no parsing of session ID, nor getting it from a database."
  ([db user-identity] (prop db :session user-identity))
  ([db identity-type user-identity] (prop-of identity-type db :session user-identity)))
