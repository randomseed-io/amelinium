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
            [clj-uuid                      :as                   uuid]
            [next.jdbc                     :as                   jdbc]
            [next.jdbc.sql                 :as                    sql]
            [next.jdbc.connection          :as             connection]
            [ragtime.repl                  :as           ragtime-repl]
            [potemkin.namespaces           :as                      p]
            [io.randomseed.utils           :refer                :all]
            [io.randomseed.utils.db        :as                     db]
            [io.randomseed.utils.db.types  :as               db-types]
            [io.randomseed.utils.fs        :as                     fs]
            [io.randomseed.utils.var       :as                    var]
            [io.randomseed.utils.map       :as                    map]
            [amelinium.app                 :as                    app]
            [amelinium.system              :as                 system]
            [amelinium.logging             :as                    log])

  (:import [com.zaxxer.hikari HikariConfig HikariDataSource HikariPoolMXBean]
           [java.sql Connection]
           [javax.sql DataSource]
           [java.lang.reflect Method]
           [java.io Closeable]))

(set! *warn-on-reflection* true)

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
                get-ids get-id])

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

(p/import-vars [io.randomseed.utils.db init-cache init-caches purge-caches])

(defn print-caches
  ([]           (db/print-caches caches))
  ([caches-obj] (db/print-caches caches-obj)))

(defn list-caches [] (print-caches))

;; UUID mapping

(defn some-uuid-str
  [s]
  (let [s (some-str s)]
    (if (uuid/uuidable? s) s)))

(defn uuidable?
  [v]
  (uuid/uuidable? v))

(defn as-uuid
  [v]
  (if (uuid/uuidable? v)
    (uuid/as-uuid v)))

(defn key-as-uuid
  [m k]
  (map/update-existing m k as-uuid))

(defn get-id-by-uid
  [db query uid]
  (let [uid (some-uuid-str uid)]
    (if (and db uid)
      (first (jdbc/execute-one! db [query uid] opts-simple-vec)))))

(defn get-ids-by-uids
  [db query uids]
  (if (and db uids)
    (let [uids  (map some-uuid-str uids)
          query (str query " " (braced-join-? uids))]
      (->> (sql/query db (cons query uids) opts-simple-vec)
           next
           (map #(vector (as-uuid (nth % 0)) (nth % 1)))
           (into {})))))

;; UUID caching

(defn cache-lookup-uuid
  [cache db id-getter uid]
  (if (and db (uuidable? uid))
    (cwr/lookup-or-miss cache (uuid/as-uuid uid) #(id-getter db %))))

(defn cache-lookup-uuids
  [cache uids]
  (if (seq uids)
    (let [uids (map #(when-valuable % (as-uuid %)) uids)]
      (reduce (fn [m uid]
                (let [id (cwr/lookup cache uid false)]
                  (if (false? id)
                    (update m false conj uid)
                    (assoc m uid id))))
              {} uids))))

(defn uid-to-id
  ([db cache getter uid]
   (cache-lookup-uuid cache db getter uid)))

(defn uids-to-ids
  [db cache getter uids]
  (if (and db uids)
    (let [looked-up (cache-lookup-uuids cache uids)
          missing   (seq (get looked-up false))]
      (if-not missing
        looked-up
        (let [db-ids  (getter db missing)
              present (or (dissoc looked-up false) {})]
          (reduce #(assoc %1 %2 (cwr/lookup-or-miss cache %2 db-ids))
                  present missing))))))

;; Email mapping

(defn get-id-by-email
  [db query email]
  (let [email (some-str email)]
    (if (and db email)
      (first (jdbc/execute-one! db [query email] opts-simple-vec)))))

(defn get-ids-by-emails
  [db query emails]
  (if (and db emails)
    (let [emails (map some-str emails)
          query  (str query " " (braced-join-? emails))]
      (->> (sql/query db (cons query emails) opts-simple-vec)
           next
           (map #(vector (keyword (nth % 0)) (nth % 1)))
           (into {})))))

;; Email caching

(defn cache-lookup-email
  [cache db id-getter email]
  (if (and db (valuable? email))
    (cwr/lookup-or-miss cache (keyword email) #(id-getter db %))))

(defn cache-lookup-emails
  [cache emails]
  (if (seq emails)
    (let [emails (map #(when-valuable % (keyword %)) emails)]
      (reduce (fn [m email]
                (let [id (cwr/lookup cache email false)]
                  (if (false? id)
                    (update m false conj email)
                    (assoc m email id))))
              {} emails))))

(defn email-to-id
  ([db cache getter email]
   (cache-lookup-email cache db getter email)))

(defn emails-to-ids
  [db cache getter emails]
  (if (and db emails)
    (let [looked-up (cache-lookup-emails cache emails)
          missing   (seq (get looked-up false))]
      (if-not missing
        looked-up
        (let [db-ids  (getter db missing)
              present (or (dissoc looked-up false) {})]
          (reduce #(assoc %1 %2 (cwr/lookup-or-miss cache %2 db-ids))
                  present missing))))))

;; Configuration record

(defrecord DBConfig [^clojure.lang.Fn initializer
                     ^clojure.lang.Fn finalizer
                     ^clojure.lang.Fn suspender
                     ^clojure.lang.Fn resumer
                     ^clojure.lang.Keyword dbkey
                     ^String dbname
                     datasource])

(defn db-config?
  "Returns true if a value of the given argument is an instance of DBConfig record
  type."
  [v]
  (instance? DBConfig v))

(defn ds
  "Gets the data source from the DBConfig record. If the given argument is not an
  instance of DBConfig, it simply returns it."
  [v]
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
        (map/assoc-missing  :reporter    (partial default-reporter db-key))
        (map/update-missing :datastore   (:initializer config)))))

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
(system/add-halt!    ::caches      [k config] (var/make k (purge-caches config)))

(derive ::main                ::initializer)
(derive ::main.props          ::properties)
(derive ::main-migrator.props ::properties)
(derive ::main-migrator       ::migrator)
