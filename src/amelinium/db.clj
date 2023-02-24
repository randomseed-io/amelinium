(ns

    ^{:doc    "Cross-category databases and generators for Amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.db

  (:refer-clojure :exclude [memoize parse-long uuid random-uuid])

  (:require [clojure.set                   :as                    set]
            [clojure.string                :as                    str]
            [clojure.core.cache            :as                  cache]
            [clojure.core.cache.wrapped    :as                    cwr]
            [clojure.core.memoize          :as                    mem]
            [clj-uuid                      :as                   uuid]
            [next.jdbc                     :as                   jdbc]
            [next.jdbc.sql                 :as                    sql]
            [next.jdbc.connection          :as             connection]
            [next.jdbc.prepare              :as                    jp]
            [ragtime.repl                  :as           ragtime-repl]
            [potemkin.namespaces           :as                      p]
            [io.randomseed.utils           :refer                :all]
            [io.randomseed.utils.ip        :as                     ip]
            [io.randomseed.utils.db        :as                     db]
            [io.randomseed.utils.db.types  :as               db-types]
            [io.randomseed.utils.fs        :as                     fs]
            [io.randomseed.utils.var       :as                    var]
            [io.randomseed.utils.map       :as                    map]
            [io.randomseed.utils.map       :refer            [qassoc]]
            [phone-number.util             :as                 phutil]
            [phone-number.core             :as                  phone]
            [taoensso.nippy                :as                  nippy]
            [amelinium                     :refer                :all]
            [amelinium.app                 :as                    app]
            [amelinium.system              :as                 system]
            [amelinium.logging             :as                    log]
            [amelinium.types.db            :refer                :all]
            [amelinium.types.identity      :refer                :all]
            [amelinium.identity            :as               identity])

  (:import [com.zaxxer.hikari HikariConfig HikariDataSource HikariPoolMXBean]
           [amelinium         DBConfig]
           [java.sql          Connection PreparedStatement]
           [javax.sql         DataSource]
           [java.io           Closeable]
           [java.lang.reflect Method]))

(set! *warn-on-reflection* true)

(alter-var-root #'nippy/*thaw-serializable-allowlist*
                conj "com.google.i18n.phonenumbers.Phonenumber$PhoneNumber")

(defonce auth      nil)
(defonce migrators nil)
(defonce caches    nil)

;; Database column readers and result set setters

(db-types/add-all-readers)
(db-types/add-all-setters)

;; Builder and conversion functions

(p/import-vars [io.randomseed.utils.db
                to-lisp-simple to-snake-simple to-lisp to-snake
                to-lisp-slashed to-snake-slashed
                opts-simple-map opts-map opts-simple-vec opts-vec
                opts-slashed-map opts-slashed-vec])

;; SQL strings preparation

(p/import-vars [io.randomseed.utils.db
                join-col-names braced-join-col-names braced-join-col-names-no-conv
                join-? braced-join-? join-v=? values-? braced-?])

;; Type checks

(p/import-vars [io.randomseed.utils.db data-source?])

;; Memoization

(p/import-vars [io.randomseed.utils.db memoize memoizer invalidate! invalidator])

;; Generic getters and setters

(p/import-vars [io.randomseed.utils.db
                make-getter-coll make-getter make-setter make-deleter
                get-ids get-id not-found?])

;; Cached database access

(p/import-vars [io.randomseed.utils.db
                cache-prepare cache-create cache-evict! cache-lookup-coll cache-lookup
                get-cached-coll get-cached get-cached-coll-prop
                get-cached-prop get-cached-prop-or-default])

;; SQL helpers

(p/import-vars [io.randomseed.utils.db
                for-replace for-insert-or for-replace-multi for-insert-multi-or
                insert-or! insert-multi-or!
                insert-or-replace-multi! insert-or-ignore-multi!
                insert-or-replace! insert-or-ignore!
                replace! replace-multi!])

;; Database result processing helpers

(p/import-vars [io.randomseed.utils.db get-failed? id-from-db id-to-db])

;; Settings abstraction

(p/import-vars [io.randomseed.utils.db make-setting-getter make-setting-setter make-setting-deleter])

;; Cached settings handling

(p/import-vars [io.randomseed.utils.db cached-setting-get cached-setting-set cached-setting-del])

;; Single-point cache management

(p/import-vars [io.randomseed.utils.db init-cache init-caches remove-caches])

(defn print-caches
  ([]           (db/print-caches caches))
  ([caches-obj] (db/print-caches caches-obj)))

(defn list-caches [] (print-caches))

;; Keywordized properties mapping

(defn key-as-keyword
  [m k]
  (map/update-existing m k keyword))

;; IP properties mapping

(defn key-as-ip
  [m k]
  (map/update-existing m k ip/string-to-address))

;; Phone number mapping

(defn key-as-phone
  [m k]
  (map/update-existing m k identity/preparse-phone))

;; UUID mapping

(defn key-as-uuid
  [m k]
  (map/update-existing m k identity/preparse-uid))

;; Memoization

(defn mem-assoc-existing!
  "Set a key `k` to a value `v` in a map being a cached result of prior calling
  memoized function `f`. Will not associate anything if the destination does not
  exist. The caching key should be given as a `key` vector."
  ([f key k v]
   (mem/memo-swap! f #(if-some [e (cache/lookup %1 %2)]
                        (cache/miss %1 %2 (delay (map/qassoc @e k v))) %1)
                   key))
  ([f key k v & kvs]
   (mem/memo-swap! f #(if-some [e (cache/lookup %1 %2)]
                        (cache/miss %1 %2 (delay (apply map/qassoc @e k v kvs))) %1)
                   key)))

;; Email/phone/uid/id mapping to a DB-consumable values

(defn identity->str
  "Converts the given `id` to a string unless it's a positive integer. Used to prepare
  user identities to a format acceptable by raw query strings (where they cannot be
  passed as parameters in database prepared statements)."
  [id]
  (cond
    (string? id)       (some-str id)
    (phone/native? id) (phone/format id nil :phone-number.format/e164)
    (pos-int? id)      id
    :else              (some-str id)))

(defn identity->kw
  "Calls `identity->str` and then converts the result into a keyword."
  [id]
  (some-keyword (identity->str id)))

(defn get-user-id-by-identity
  "For the given `db` (database connectable object), a query string `query` and user's
  identity `identity`, performs a query on a database and returns its result.

  Identity can optionally be pre-processed with the given `key-transformer` which
  should be a function. It will be called to transform `user-identity` before
  querying a database. The purpose of it is to transform an identifier into its
  native form and let the JDBC layer coerce it in a proper way."
  ([db query user-identity]
   (get-user-id-by-identity db query identity))
  ([db query user-identity key-transformer]
   (if db
     (if-let [user-identity (key-transformer user-identity)]
       (first (jdbc/execute-one! db [query user-identity] opts-simple-vec))))))

(defn get-user-ids-by-identities
  "For the given `db` (database connectable object), a query string `query` and a
  sequence of user's identities `identities`, performs a query on a database and
  returns its result.

  Identities can optionally be pre-processed with the given `key-transformer` which
  should be a function. It will be called to transform each identity before querying
  a database, and after results are obtained in a form of a map, for each key of that
  map. The purpose of it is to transform any identifier into its native form and let
  the JDBC layer coerce it in a proper way, and then to ensure that when it comes
  back from a database it is in the same form. That way this function results can
  safely be processed with other function calls since the types of keys are
  normalized."
  ([db query identities]
   (get-user-ids-by-identities db query identities identity))
  ([db query identities key-transformer]
   (if (and db identities)
     (let [identities (filter identity (map key-transformer identities))
           query      (str query " " (braced-join-? identities))]
       (->> (sql/query db (cons query identities) opts-simple-vec)
            next
            (map #(vector (key-transformer (nth % 0)) (nth % 1)))
            (into {}))))))

;; Email/phone/uid/id caching

(defn cache-lookup-user-id
  "Performs a cache lookup of user identity `user-identity` using a cache object
  `cache`. Returns a value being a result of a cache lookup and, if the entry
  identified by `user-identity` is not in a cache, returns
  `:io.randomseed.utils.db/not-found`.

  Does not perform any pre-processing or post-processing of the `user-identity`."
  [cache user-identity]
  (if user-identity
    (cwr/lookup cache user-identity ::db/not-found)))

(defn cache-lookup-user-ids
  "Takes a cache object `cache` and a sequence of identities `identities`, and returns
  a map with keys and values found in the cache, plus a special key
  `:io.randomseed.utils.db/not-found` with a list of keys which were not found
  associated to it.

  Does not perform any pre-processing or post-processing of the `user-identity`."
  [cache identities]
  (if (seq identities)
    (reduce (fn [m user-identity]
              (let [id (cwr/lookup cache user-identity ::db/not-found)]
                (if (db/not-found? id)
                  (qassoc m ::db/not-found (conj (get m ::db/not-found) user-identity))
                  (qassoc m user-identity id))))
            {} identities)))

(defn identity-to-user-id
  "Takes a cache object `cache`, a database connectable object `db`, and a user
  identifier `user-identity`, and returns a value found in the cache for it. If there
  is a cache miss, the `getter` function is called to obtain the user ID and return
  it.

  Identity can optionally be pre-processed with the given `key-transformer` which
  should be a function. It will be called to transform `user-identity` before
  querying a database. The purpose of it is to transform an identifier into its
  native form and let the JDBC layer coerce it in a proper way.

  As a side effect it updates the cache in a thread-safe way."
  ([db cache getter user-identity]
   (identity-to-user-id db cache getter user-identity identity))
  ([db cache getter user-identity key-transformer]
   (if-let [user-identity (key-transformer user-identity)]
     (cwr/lookup-or-miss cache user-identity #(getter db %)))))

(defn identities-to-user-ids
  "Takes a cache object `cache`, a database connectable object `db`, and a sequence of
  user identifiers `user-identities`, and returns a map in which keys are the given
  identities and values are user IDs. At first the cache is looked up for any
  identity and if it is found, it's collected. Next, for all missing identities a
  database query is performed using `getter` function. It should return a map of
  assignments from the identity to a numeric identifier. Then the results are merged
  and presented as a map.

  Identities can optionally be pre-processed with the given `key-transformer` which
  should be a function. It will be called on each identity before querying a database
  and before returning the result. The purpose of it is to transform each identifier
  into its native form and let the JDBC layer coerce it in a proper way, and then to
  normalize the keys of the returned map so they are look the same and are in sync
  with the cache.

  As a side effect it updates the cache in a thread-safe way."
  ([db cache getter identities]
   (identities-to-user-ids db cache getter identities identity))
  ([db cache getter identities key-transformer]
   (if (and db identities)
     (let [identities (filter identity (map key-transformer identities))
           looked-up  (cache-lookup-user-ids cache identities)
           missing    (seq (get looked-up ::db/not-found))]
       (if-not missing
         looked-up
         (let [db-ids  (getter db missing)
               present (dissoc looked-up ::db/not-found)]
           (reduce #(qassoc %1 %2 (cwr/lookup-or-miss cache %2 db-ids))
                   present missing)))))))

;; Settable parameters

(extend-protocol jp/SettableParameter

  amelinium.Identity

  (set-parameter [^amelinium.Identity v ^PreparedStatement ps ^long i]
    (jp/set-parameter (identity/->db v) ps i)))

;; Configuration record

(defn db-config?
  "Returns true if a value of the given argument is an instance of DBConfig record
  type."
  [v]
  (instance? DBConfig v))

(defn ds
  "Gets the data source from the DBConfig record. If the given argument is not an
  instance of DBConfig, it simply returns it."
  ^DataSource [v]
  (if (instance? DBConfig v) (:datasource v) v))

;; Configuration helpers

(def dbname-key-finder
  (some-fn (comp some-str :orig-key)
           #(if (or (string? %) (ident? %)) (some-str %))
           (comp some-str :dbkey)
           (comp some-str :dbkey :properties)
           (comp some-str :dbkey :datasource)
           (comp some-str :dbkey :datasource :datastore)
           (comp some-str :dbkey :datastore :datasource)
           (comp some-str :dbkey :datastore)
           (comp some-str :dbkey :db-spec :datastore)
           (comp some-str :dbkey :db-spec)
           (comp some-str :dbkey :properties :datasource)
           (comp some-str :dbkey :properties :datastore)))

(def dbname-finder
  (some-fn #(if (or (string? %) (ident? %)) (some-str %))
           (comp some-str :dbname :properties)
           (comp some-str :dbname :datasource)
           (comp some-str :dbname)
           (comp some-str :dsname)
           (comp some-str :name :db)
           (comp some-str :dbname :db)
           (comp some-str :db-name)
           (comp some-str :dbname :datasource :datastore)
           (comp some-str :dbname :datastore :datasource)
           (comp some-str :dbname :datastore)
           (comp some-str :dbname :db-spec :datastore)
           (comp some-str :dbname :db-spec)
           (comp some-str :dbname :properties :datasource)
           (comp some-str :dbname :properties :datasource)
           (comp some-str :name)))

(defn db-name
  "Obtains the database (data source) name from the given configuration data structure
  by using known patterns."
  ([v]
   (if v
     (or (and (db-config? v) (some-str (get v :dbname)))
         (dbname-finder v)
         nil)))
  ([v & more]
   (or (db-name v)
       (some dbname-finder (filter identity (cons v more)))
       nil)))

(defn db-key-name
  "Obtains the database (data source) key name from the given configuration data
  structure by using known patterns."
  ([v]
   (if v
     (or (and (db-config? v) (some-str (get v :dbkey)))
         (dbname-key-finder v)
         nil)))
  ([v & more]
   (or (db-key-name v)
       (some dbname-key-finder (filter identity (cons v more)))
       nil)))

;; Migrations

(declare init-db)
(declare close-db)
(declare close-mig)

(defn migration
  ([migrator-obj]
   (migrator-obj)))

(defn migrations
  ([]
   (migrations migrators))
  ([migrators-vec]
   ((apply juxt migrators-vec))))

(defn try-initialize-db
  [config]
  (let [db-spec (merge (:properties config) (:datasource (:datastore config)))
        db-name (or (db-name db-spec) (db-name config))]
    (if (and db-name db-spec)
      (jdbc/execute! (dissoc db-spec :dbname) [(str-spc "CREATE DATABASE IF NOT EXISTS" db-name)]))))

(defn migration-databases
  [config]
  (if (and config (sequential? config) (seq config))
    (->> (filter fn? config)
         (map #(:dbkey (%)))
         (filter identity)
         distinct seq)))

(defn- migrators-state
  [mig-key]
  (let [migrators (get app/state mig-key)
        mig-dbs   (set (migration-databases migrators))]
    {:migrators? (some? (seq migrators))
     :dbs-up     mig-dbs
     :props-up   (set (map #(get-in app/post-config [%1 :properties :key]) mig-dbs))}))

(defn- migrators-key
  [v]
  (or (if (map? v) (get v :migrators-key) (valuable v))
      ::migrators))

(defn migrate!
  "Migrates all databases (or a database specified by a migrator function passed as an
  argument) up to the latest migration. Optional map of options can be passed which
  will be merged with each migration options."
  ([]
   (migrate! nil))
  ([opts]
   (let [mig-key      (migrators-key opts)
         state-pre    (migrators-state mig-key)
         start-admin! (get opts :fn/start-admin app/start-admin!)]
     (if-not (:migrators? state-pre) (start-admin! mig-key))
     (if (fn? opts)
       (ragtime-repl/migrate (opts))
       (doseq [mconfig (get app/state mig-key)]
         (let [config (merge (mconfig) opts)
               dbname (db-name config)
               dbkey  (db-key-name config)]
           (if (pos-int? (::jdbc/update-count (first (try-initialize-db config))))
             (log/msg "Created empty database" dbname (str "(" dbkey ")")))
           (ragtime-repl/migrate config))))
     (if-not (:migrators? state-pre)
       (let [state-post (migrators-state mig-key)
             stop-keys  (concat (set/difference (:dbs-up   state-post) (:dbs-up   state-pre))
                                (set/difference (:props-up state-post) (:props-up state-pre)))]
         (apply app/stop! mig-key (filter identity stop-keys)))))
   nil))

(defn rollback!
  "Rolls back all databases or a database specified by a migrator function passed as an
  argument. Optional map of options can be passed which will be merged with each
  migration options. If a value is passed instead of a map or a function it will be
  used as an additional argument meaning a number of migrations or a migration ID."
  ([]
   (rollback! nil))
  ([opts]
   (let [mig-key      (migrators-key opts)
         state-pre    (migrators-state mig-key)
         start-admin! (get opts :fn/start-admin app/start-admin!)]
     (if-not (:migrators? state-pre) (start-admin! mig-key))
     (if (fn? opts)
       (ragtime-repl/rollback (opts))
       (if (or (not opts) (map? opts))
         (doseq [migrator (get app/state mig-key)] (ragtime-repl/rollback (merge (migrator) opts)))
         (doseq [migrator (get app/state mig-key)] (ragtime-repl/rollback (migrator) opts))))
     (if-not (:migrators? state-pre)
       (let [state-post (migrators-state mig-key)
             stop-keys  (concat (set/difference (:dbs-up   state-post) (:dbs-up   state-pre))
                                (set/difference (:props-up state-post) (:props-up state-pre)))]
         (apply app/stop! mig-key (filter identity stop-keys))))))
  ([opts amount-or-id]
   (let [mig-key      (migrators-key opts)
         state-pre    (migrators-state mig-key)
         start-admin! (get opts :fn/start-admin app/start-admin!)]
     (if-not (:migrators? state-pre) (start-admin! mig-key))
     (if (fn? opts)
       (ragtime-repl/rollback (opts) amount-or-id)
       (doseq [migrator (get app/state mig-key)] (ragtime-repl/rollback (merge (migrator) opts) amount-or-id)))
     (if-not (:migrators? state-pre)
       (let [state-post (migrators-state mig-key)
             stop-keys  (concat (set/difference (:dbs-up   state-post) (:dbs-up   state-pre))
                                (set/difference (:props-up state-post) (:props-up state-pre)))]
         (apply app/stop! mig-key (filter identity stop-keys)))))
   nil))

(defn migration-index
  "Gets a current value of ragtime-repl/migration-indexes."
  []
  (deref ragtime-repl/migration-index))

;; Generic close

(defn- unary-close-method
  ^Boolean [^Method met]
  (and (= "close" (.getName met)) (nil? (seq (.getParameterTypes met)))))

(defn close!
  [obj]
  (if obj
    (if (isa? (class obj) Closeable)
      (.close ^Closeable obj)
      (some-> unary-close-method
              (filter (.getMethods ^Class (class obj)))
              first
              (^Method identity)
              (.invoke obj (object-array []))))))

;; Connection pool (HikariCP)

(defn pool-datasource
  ^HikariDataSource [db-props]
  (when-some [^HikariDataSource ds (connection/->pool HikariDataSource db-props)]
    (.setPoolName ^HikariDataSource ds (db-key-name db-props))
    (.setAllowPoolSuspension ^HikariDataSource ds true)
    (close! (jdbc/get-connection ^HikariDataSource ds))
    ds))

(defn close-pool
  [^HikariDataSource ds]
  (.close ^HikariDataSource ds))

(defn suspend-pool
  [^HikariDataSource ds]
  (.suspendPool ^HikariPoolMXBean (.getHikariPoolMXBean ^HikariDataSource ds)))

(defn resume-pool
  [^HikariDataSource ds]
  (.resumePool ^HikariPoolMXBean (.getHikariPoolMXBean ^HikariDataSource ds))
  (close! (jdbc/get-connection ^HikariDataSource ds)))

;; Configuration initializers

(defn prep-db
  [config]
  (if-not (map? config)
    config
    (-> config
        (map/update-existing :dbname         fs/parse-java-properties)
        (map/update-existing :migrations-dir fs/parse-java-properties)
        (map/assoc-missing  :user            (get config :username))
        (map/assoc-missing  :username        (get config :user))
        (map/dissoc-if      :username        nil?)
        (map/dissoc-if      :user            nil?))))

(defn init-db
  ([k config]
   (init-db k config
            (var/deref-symbol (:initializer config))
            (var/deref-symbol (:finalizer   config))
            (var/deref-symbol (:suspender   config))
            (var/deref-symbol (:resumer     config))))
  ([k config ds-getter]
   (init-db k config ds-getter nil nil nil))
  ([k config ds-getter ds-closer]
   (init-db k config ds-getter ds-closer nil nil))
  ([k config ds-getter ds-closer ds-suspender]
   (init-db k config ds-getter ds-closer ds-suspender nil))
  ([k config ds-getter ds-closer ds-suspender ds-resumer]
   (if config
     (let [db-props (-> :properties config (dissoc :logger :migrations-dir) prep-db)
           db-name  (db-name db-props config k)
           db-key   (db-key-name k db-props config)
           db-props (map/assoc-missing db-props :name db-name :dbkey db-key)]
       (log/msg "Configuring database" db-name (str "(" db-key ")"))
       (DBConfig. ^clojure.lang.Fn      ds-getter
                  ^clojure.lang.Fn      ds-closer
                  ^clojure.lang.Fn      ds-suspender
                  ^clojure.lang.Fn      ds-resumer
                  ^clojure.lang.Keyword db-key
                  ^String               db-name
                  (ds-getter db-props))))))

(defn close-db
  [k config]
  (when config
    (log/msg "Closing database connection to" (db-name config k) (str "(" (db-key-name k config) ")"))
    (let [ds-closer (or (:finalizer config) close!)]
      (if-some [ds (or (:datasource config) (:datastore config) (:database config))]
        (ds-closer ds))
      nil)))

(defn suspend-db
  [k config]
  (if-some [ds-suspender (:suspender config)]
    (when-some [ds (:datasource config)]
      (log/msg "Suspending database" (db-name config k) (str "(" (db-key-name k config) ")"))
      (ds-suspender ds))
    (system/halt-key! k config)))

(defn resume-db
  [k config old-config old-impl]
  (let [ds-resumer (or (:resumer old-impl) (:resumer config) (:resumer old-config))]
    (if (and ds-resumer (= (dissoc config :initializer :finalizer :suspender :resumer)
                           (dissoc config :initializer :finalizer :suspender :resumer)))
      (if-some [ds (:datasource old-impl)] (ds-resumer ds) old-impl)
      (do (system/halt-key! k old-impl)
          (system/init-key k config)))))

(defn default-reporter
  [db-k-name ds op id]
  (case op
    :up   (log/msg "Applying DB migration"     id "on" (db-key-name db-k-name ds))
    :down (log/msg "Rolling back DB migration" id "on" (db-key-name db-k-name ds))))

(defn migrator-config
  [config loader migration-dir]
  (let [db-key (db-key-name config)]
    (-> config
        (assoc :migrations (loader migration-dir))
        (map/assoc-missing  :initializer identity)
        (map/assoc-missing  :reporter  (partial default-reporter db-key))
        (map/update-missing :datastore (:initializer config)))))

(defn init-mig
  [k config]
  (let [ds     (ds (init-db k config))
        loader (var/deref (:loader config))
        migdir (fs/parse-java-properties (or (:migrations-dir config)
                                             (get-in config [:properties :migrations-dir])))
        config (-> config
                   (assoc :dbkey k :datastore ds)
                   (map/update-existing :reporter  var/deref-symbol)
                   (map/update-existing :strategy  keyword)
                   (dissoc :loader :logger :initializer :properties))]
    (fn []
      (migrator-config config loader migdir))))

(defn init-migrators
  [config]
  (if (and config (sequential? config) (seq config))
    (mapv #(if (fn? %) % (init-mig nil %)) config)))

(defn close-mig
  [k config]
  (if (and (ident? k) (fn? config))
    (when-some [config (config)]
      (close-db k config)
      nil)))

(system/add-prep     ::properties  [_ config] (prep-db config))
(system/add-init     ::properties  [_ config] config)
(system/add-halt!    ::properties  [_ config] nil)

(system/add-prep     ::initializer [_ config] (prep-db config))
(system/add-init     ::initializer [k config] (let [d (init-db k config)] (var/make k (ds d)) d))
(system/add-suspend! ::initializer [k config] (suspend-db k config))
(system/add-resume   ::initializer [k config old-config old-impl] (resume-db k config old-config old-impl))
(system/add-halt!    ::initializer [k config] (var/make k (close-db k config)))

(system/add-prep     ::migrator    [_ config] (prep-db config))
(system/add-init     ::migrator    [k config] (var/make k (init-mig  k config)))
(system/add-halt!    ::migrator    [k config] (var/make k (close-mig k config)))

(system/add-init     ::migrators   [k config] (var/make k (init-migrators config)))
(system/add-halt!    ::migrators   [k config] (var/make k nil))

(system/add-init     ::caches      [k config] (var/make k (init-caches  config)))
(system/add-halt!    ::caches      [k config] (var/make k (remove-caches config)))

(derive ::main                ::initializer)
(derive ::main.props          ::properties)
(derive ::main-migrator.props ::properties)
(derive ::main-migrator       ::migrator)
