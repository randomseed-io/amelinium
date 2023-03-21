(ns

    ^{:doc    "Cross-category databases and generators for Amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.db

  (:refer-clojure :exclude [memoize parse-long uuid random-uuid -> <-])

  (:require [clojure.set                   :as                    set]
            [clojure.string                :as                    str]
            [clojure.core                  :as                      c]
            [clojure.core.cache            :as                  cache]
            [clojure.core.cache.wrapped    :as                    cwr]
            [clojure.core.memoize          :as                    mem]
            [clj-uuid                      :as                   uuid]
            [next.jdbc                     :as                   jdbc]
            [next.jdbc.sql                 :as                    sql]
            [next.jdbc.quoted              :as                 quoted]
            [next.jdbc.connection          :as             connection]
            [next.jdbc.result-set          :as                     rs]
            [next.jdbc.prepare             :as                     jp]
            [next.jdbc.protocols           :as                    jpr]
            [ragtime.repl                  :as           ragtime-repl]
            [potemkin.namespaces           :as                      p]
            [io.randomseed.utils           :refer                :all]
            [io.randomseed.utils.ip        :as                     ip]
            [io.randomseed.utils.db        :as                     db]
            [io.randomseed.utils.db.types  :as               db-types]
            [io.randomseed.utils.fs        :as                     fs]
            [io.randomseed.utils.var       :as                    var]
            [io.randomseed.utils.map       :as                    map]
            [io.randomseed.utils.map       :refer    [qassoc qupdate]]
            [phone-number.util             :as                 phutil]
            [phone-number.core             :as                  phone]
            [taoensso.nippy                :as                  nippy]
            [lazy-map.core                 :as               lazy-map]
            [amelinium                     :refer                :all]
            [amelinium.app                 :as                    app]
            [amelinium.system              :as                 system]
            [amelinium.logging             :as                    log]
            [amelinium.types.db            :refer                :all]
            [amelinium.types.identity      :refer                :all]
            [amelinium.identity            :as               identity])

  (:import (amelinium         DBConfig)
           (lazy_map.core     LazyMap)
           (clojure.lang      IPersistentMap
                              PersistentVector
                              Keyword
                              Fn)
           (com.zaxxer.hikari HikariConfig
                              HikariDataSource
                              HikariPoolMXBean)
           (javax.sql         DataSource)
           (java.io           Closeable)
           (java.lang.reflect Method)
           (java.sql          Connection
                              PreparedStatement
                              DatabaseMetaData
                              ParameterMetaData
                              ResultSet
                              ResultSetMetaData
                              Statement)))

(set! *warn-on-reflection* true)

(alter-var-root #'nippy/*thaw-serializable-allowlist*
                conj "com.google.i18n.phonenumbers.Phonenumber$PhoneNumber")

(defonce ^:redef auth      nil)
(defonce ^:redef migrators nil)
(defonce ^:redef caches    nil)

;; Database column readers and result set setters

(db-types/add-all-readers)
(db-types/add-all-setters)

;; Builder and conversion functions

(p/import-vars [io.randomseed.utils.db
                to-lisp-simple to-snake-simple to-lisp to-snake
                to-lisp-slashed to-snake-slashed])

;; SQL strings preparation

(p/import-vars [io.randomseed.utils.db
                join-col-names braced-join-col-names braced-join-col-names-no-conv
                join-? braced-join-? join-v=? values-? braced-?])

;; Type checks

(p/import-vars [io.randomseed.utils.db data-source?])

;; Memoization

(p/import-vars [io.randomseed.utils.db
                memoize+ memoize memoizer invalidate! invalidate+! invalidator])

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

;; Settable parameters

(extend-protocol jp/SettableParameter

  amelinium.Identity

  (set-parameter [^amelinium.Identity v ^PreparedStatement ps ^long i]
    (jp/set-parameter (identity/to-db v) ps i)))

;; Column and table names processing

(defn quoted
  "Quotes the given string according to MySQL / MariaDB quoting rules."
  ^String [s]
  (if-some [^String s (some-str s)]
    (quoted/mysql s)))

(defn idname
  "If the given value `v` is an ident, it returns its (optional) namespace and name
  joined with a dot character. Otherwise it returns the string representation of
  the given object with slashes replaced by dot characters."
  ^String [v]
  (if (ident? v)
    (if-some [^String nsp (namespace v)]
      (str nsp "." (name v))
      (name v))
    (if-some [^String v (some-str v)]
      (str/replace v \/ \.))))

(defn idname-simple
  "If the given value `v` is an ident, it returns its name. Otherwise it returns the
  string representation of the given object."
  ^String [v]
  (if (ident? v) (name v) (str v)))

(defn dbname
  "If the given value `v` is an ident, it returns its (optional) namespace and name
  joined with a dot character. Otherwise it returns a string representation of the
  given object with a first slash replaced by a dot."
  ^String [v]
  (if (ident? v)
    (if-some [^String nsp (namespace v)]
      (strb nsp "." (name v))
      (name v))
    (if-some [^String v (some-str v)]
      (str/replace-first v \/ \.))))

(defn dbname-quoted
  "If the given value `v` is an ident, it returns its (optional) namespace and name
  joined with a dot character. Otherwise it returns a string representation of the
  given object with a first slash replaced by a dot. Each part of a name will be
  quoted."
  ^String [v]
  (if (ident? v)
    (if-some [^String nsp (namespace v)]
      (strb (quoted nsp) "." (quoted (name v)))
      (name v))
    (if-some [^String v (some-str v)]
      (if (str/index-of v \/)
        (dbname-quoted (keyword v))
        (if (str/index-of v \.)
          (dbname-quoted (keyword (str/replace-first v \. \/)))
          (quoted v))))))

(defn dbname-kw
  "If the given value `v` is an ident, it returns its keyword representation. Otherwise
  it returns a string representation of the given object with dots replaced by
  slashes."
  ^String [v]
  (if (ident? v)
    (keyword v)
    (if-some [^String v (some-str v)]
      (if (str/index-of v \/)
        (keyword v)
        (keyword (str/replace-first v \. \/))))))

(defn make-kw
  "Creates a keyword with the given name and namespace which both can be expressed as
  strings or idents. If the second argument is `nil` then a keyword is created using
  the first argument by simply converting it with the `keyword` function. If both
  `ns` and `name` are given then the following is applied: if `ns` or `name` is a
  qualified ident, its name and namespace will be joined with a dot character before
  producing a keyword; additionally, if `ns` or `name` is a simple ident, any slash
  character in its name will be replaced with a dot. If `ns` or `name` is not an
  ident then any slash character in its string representation will be replaced with a
  dot before creating a keyword."
  (^Keyword [name]
   (if (keyword? name) name (keyword name)))
  (^Keyword [ns name]
   (if name
     (keyword (idname ns) (idname name))
     (if (keyword? ns) ns (keyword ns)))))

(defn make-kw-simple
  "Creates a keyword with the given name and namespace which both can be expressed as
  strings or idents. If the second argument is `nil` then a keyword is created using
  the first argument by simply converting it with the `keyword` function. If any
  given ident is namespaced, only its name is used."
  (^Keyword [name]
   (if (keyword? name) name (keyword name)))
  (^Keyword [ns name]
   (if name
     (keyword (idname-simple ns) (idname-simple name))
     (if (keyword? ns) ns (keyword ns)))))

(defn make-kw-snake
  "Creates a keyword with the given name and namespace which both can be expressed as
  strings or idents. All hyphen characters will be replaced by underscores. If the
  second argument is `nil` then a keyword is created using the first argument by
  simply converting it with the `keyword` function. If any given ident is namespaced,
  only its name is used."
  (^Keyword [name]
   (keyword (db/to-snake name)))
  (^Keyword [ns name]
   (if name
     (keyword (db/to-snake (idname-simple ns)) (db/to-snake (idname-simple name)))
     (keyword (db/to-snake ns)))))

(defn make-kw-lisp
  "Creates a keyword with the given name and namespace which both can be expressed as
  strings or idents. All underscore characters will be replaced by hyphens. If the
  second argument is `nil` then a keyword is created using the first argument by
  simply converting it with the `keyword` function. If any given ident is namespaced,
  only its name is used."
  (^Keyword [name]
   (keyword (db/to-lisp name)))
  (^Keyword [ns name]
   (if name
     (keyword (db/to-lisp (idname-simple ns)) (db/to-lisp (idname-simple name)))
     (keyword (db/to-lisp ns)))))

;; Tables and columns

(defn colspec
  "Converts a `table/column`-formatted identifier `table-col` into a snake-cased
  string. If `table-id` and `col-id` are given, it creates a string of those parts
  joined with a dot character. If identifier is given, it uses its namespace and
  name."
  (^String [col-spec]
   (if (ident? col-spec)
     (colspec (namespace col-spec) (name col-spec))
     (if-some [^String col-spec (db/to-snake col-spec)]
       (str/replace-first col-spec \/ \.))))
  (^String [table-id col-id]
   (if-some [^String table-id (some-str table-id)]
     (if-some [^String col-id (some-str col-id)]
       (db/to-snake (strb table-id "." col-id))
       (db/to-snake table-id))
     (if-some [^String col-id (some-str col-id)]
       (db/to-snake col-id)))))

(defn colspec-quoted
  "Converts a `table/column`-formatted identifier `table-col` into a snake-cased
  string. If `table-id` and `col-id` are given, it creates a string of those parts
  joined with a dot character. If identifier is given, it uses its namespace and
  name. Each part of the name will be quoted."
  (^String [col-spec]
   (if (ident? col-spec)
     (colspec-quoted (namespace col-spec) (name col-spec))
     (if-some [^String col-spec (some-str col-spec)]
       (if (str/index-of col-spec \/)
         (colspec-quoted (keyword col-spec))
         (if (str/index-of col-spec \.)
           (colspec-quoted (keyword (str/replace-first col-spec \. \/)))
           (quoted (db/to-snake col-spec)))))))
  (^String [table-id col-id]
   (if-some [^String table-id (some-str table-id)]
     (if-some [^String col-id (some-str col-id)]
       (db/to-snake (strb (quoted table-id) "." (quoted col-id)))
       (quoted (db/to-snake table-id)))
     (if-some [^String col-id (some-str col-id)]
       (quoted (db/to-snake col-id))))))

(defn colspec-kw
  "Converts a `table/column` or `table.column`-formatted identifier `table-col` into a
  lisp-cased keyword. If `table-id` and `col-id` are given, it creates a string of
  those parts joined with a dot character. If identifier is given, it uses its
  namespace and name.

  If the `col-spec` is a string and there is a slash character present in it, it will
  not be checked for a dot character presence."
  (^String [col-spec]
   (if (ident? col-spec)
     (colspec-kw (namespace col-spec) (name col-spec))
     (if-some [col-spec (some-str col-spec)]
       (if (str/index-of col-spec \/)
         (keyword (db/to-lisp col-spec))
         (if (str/index-of col-spec \.)
           (keyword (db/to-lisp (str/replace-first col-spec \. \/)))
           (keyword (db/to-lisp col-spec)))))))
  (^String [table-id col-id]
   (if-some [^String table-id (some-str table-id)]
     (if-some [^String col-id (some-str col-id)]
       (keyword (db/to-lisp (strb table-id "/" col-id)))
       (keyword (db/to-lisp table-id)))
     (if-some [^String col-id (some-str col-id)]
       (keyword (db/to-lisp col-id))))))

(defn table
  "Extracts table name as a snake-cased string from `col-spec` which may be an
  identifier or a string. If the identifier has a namespace, it will be used,
  otherwise its name will be used. For strings, it will look for a slash separator to
  detect namespace (a table name) and name (a column name), to pick a table name. If
  two arguments are given, the second one is ignored."
  (^String [col-spec]
   (if (ident? col-spec)
     (db/to-snake (or (namespace col-spec) (name col-spec)))
     (if-some [col-spec (some-str col-spec)]
       (if (str/index-of col-spec \.)
         (table (keyword (str/replace-first col-spec \. \/)))
         (if (str/index-of col-spec \/)
           (table (keyword col-spec))
           (db/to-snake col-spec))))))
  (^String [col-spec _] (table col-spec)))

(defn column
  "Extracts column name as a snake-cased string from `col-spec` which may be an
  identifier or a string. If the identifier has a name, it will be used. For strings,
  it will look for a slash separator to detect namespace (a table name) and name (a
  column name), to pick a column name. If two arguments are given, the first one is
  ignored."
  (^String [col-spec]
   (if (ident? col-spec)
     (db/to-snake (name col-spec))
     (if-some [col-spec (some-str col-spec)]
       (if (str/index-of col-spec \.)
         (column (keyword (str/replace-first col-spec \. \/)))
         (if (str/index-of col-spec \/)
           (column (keyword col-spec))
           (db/to-snake col-spec))))))
  (^String [_ col-spec] (column col-spec)))

(def ^{:tag String
       :arglists '(^String [col-spec] ^String [_ col-spec])}
  col
  "Alias for `column`. Extracts column name as a snake-cased string from `col-spec`
  which may be an identifier or a string. If the identifier has a name, it will be
  used. For strings, it will look for a slash separator to detect namespace (a table
  name) and name (a column name), to pick a column name. If two arguments are given,
  the first one is ignored."
  column)

(defn table-column
  "Extracts table and column names from `col-spec` (which may be an identifier or a
  string) as snake-cased strings of a 2-element vector (first element being a table
  name, second a column name). If `col-spec` is an identifier, its namespace and name
  will be used. If there is no namespace, it will be considered a table name. If two
  arguments are given, names are extracted separately using `table` and `column`
  functions)."
  ([col-spec]
   (if (ident? col-spec)
     (table-column (some-str col-spec))
     (if-some [col-spec (some-str col-spec)]
       (if-some [col-spec (keyword (db/to-snake (str/replace-first col-spec \. \/)))]
         (if-some [n (namespace col-spec)]
           [n (name col-spec)]
           [(name col-spec) nil])))))
  ([col-spec col-id]
   [(table col-spec) (column col-id)]))

(def ^{:arglists '([col-spec] [col-spec col-id])}
  table-col
  "Alias for `table-column`. Extracts table and column names from `col-spec` (which may
  be an identifier or a string) as snake-cased strings of a 2-element vector (first
  element being a table name, second a column name). If `col-spec` is an identifier,
  its namespace and name will be used. If there is no namespace, it will be
  considered a column name. If two arguments are given, names are extracted
  separately using `table` and `column` functions)."
  table-column)

(defn column-table
  "Extracts column and table names from `col-spec` (which may be an identifier or a
  string) as snake-cased strings of a 2-element vector (first element being a column
  name, second a table name). If `col-spec` is an identifier, its namespace and name
  will be used. If there is no namespace, it will be considered a column name. If two
  arguments are given, names are extracted separately using `column` and `table`
  functions)."
  ([col-spec]
   (if (ident? col-spec)
     (column-table (some-str col-spec))
     (if-some [col-spec (some-str col-spec)]
       (if-some [col-spec (keyword (db/to-snake (str/replace-first col-spec \. \/)))]
         [(name col-spec) (namespace col-spec)]))))
  ([col-id col-spec]
   [(column col-id) (table col-spec)]))

(def ^{:arglists '([col-spec] [col-id col-spec])}
  col-table
  "Alias for `column-table`. Extracts column and table names from `col-spec` (which may
  be an identifier or a string) as snake-cased strings of a 2-element vector (first
  element being a column name, second a table name). If `col-spec` is an identifier,
  its namespace and name will be used. If there is no namespace, it will be
  considered a column name. If two arguments are given, names are extracted
  separately using `column` and `table` functions)."
  column-table)

(defn table-kw
  "Extracts table name as a lisp-cased keyword from `col-spec` which may be an
  identifier or a string. If the identifier has a namespace, it will be used,
  otherwise its name will be used. For strings, it will first transform them into
  keywords (detecting slash character as a separator of a namespace and name) to pick
  a table name. If two arguments are given, the second one is ignored."
  (^Keyword [table-id]
   (if (ident? table-id)
     (keyword (db/to-lisp (or (namespace table-id) (name table-id))))
     (if-some [table-id (some-str table-id)]
       (if (str/index-of table-id \.)
         (table-kw (keyword (str/replace-first table-id \. \/)))
         (if (str/index-of table-id \/)
           (table-kw (keyword table-id))
           (keyword (db/to-lisp table-kw)))))))
  (^Keyword [table-id _] (table-kw table-id)))

(defn column-kw
  "Extracts column name as a lisp-cased keyword from `col-spec` which may be an
  identifier or a string. If the identifier has a name, it will be used. For strings,
  it will first transform them into keywords (detecting slash character as a
  separator of a namespace and name) to pick a column name. If two arguments are
  given, the first one is ignored."
  (^Keyword [col-id]
   (if (ident? col-id)
     (keyword (db/to-lisp (name col-id)))
     (if-some [col-id (some-str col-id)]
       (if (str/index-of col-id \.)
         (column-kw (keyword (str/replace-first col-id \. \/)))
         (if (str/index-of col-id \/)
           (column-kw (keyword col-id))
           (keyword (db/to-lisp col-id)))))))
  (^Keyword [_ col-id] (column-kw col-id)))

(def ^{:tag Keyword
       :arglists '(^Keyword [col-id] ^Keyword [_ col-id])}
  col-kw
  "Alias for `column-kw`. Extracts column name as a lisp-cased keyword from `col-spec`
  which may be an identifier or a string. If the identifier has a name, it will be
  used. For strings, it will first transform them into keywords (detecting slash
  character as a separator of a namespace and name) to pick a column name. If two
  arguments are given, the first one is ignored."
  column-kw)

(defn table-column-kw
  "Extracts table and column names from `col-spec` (which may be an identifier or a
  string) as lisp-cased keywords of a 2-element vector (first element being a table
  name, second a column name). If `col-spec` is an identifier, its namespace and name
  will be used. If there is no namespace, it will be considered a table name. If two
  arguments are given, names are extracted separately using `table-kw` and
  `column-kw` functions)."
  ([col-spec]
   (if-some [col-spec (some-str col-spec)]
     (let [k (keyword (db/to-lisp (str/replace-first col-spec \. \/)))]
       (if-some [n (namespace k)]
         [(keyword (namespace k)) (keyword (name k))]
         [(keyword (name k)) nil]))))
  ([col-spec col-id]
   [(table-kw col-spec) (column-kw col-id)]))

(def ^{:tag      Keyword
       :arglists '(^Keyword [col-id] ^Keyword [col-spec col-id])}
  table-col-kw
  "Alias for `table-column-kw`. Extracts table and column names from `col-spec` (which
  may be an identifier or a string) as lisp-cased keywords of a 2-element
  vector (first element being a table name, second a column name). If `col-spec` is
  an identifier, its namespace and name will be used. If there is no namespace, it
  will be considered a table name. If two arguments are given, names are extracted
  separately using `table-kw` and `column-kw` functions)."
  table-column-kw)

(defn column-table-kw
  "Extracts column and table names from `col-spec` (which may be an identifier or a
  string) as lisp-cased keywords of a 2-element vector (first element being a column
  name, second a table name). If `col-spec` is an identifier, its namespace and name
  will be used. If there is no namespace, it will be considered a column name. If two
  arguments are given, names are extracted separately using `column-kw` and
  `table-kw` functions)."
  ([col-spec]
   (if-some [col-spec (some-str col-spec)]
     (let [k (keyword (db/to-lisp (str/replace-first col-spec \. \/)))]
       [(keyword (name k)) (keyword (namespace k))])))
  ([col-id col-spec]
   [(column-kw col-id) (table-kw col-spec)]))

(def ^{:tag      Keyword
       :arglists '(^Keyword [col-spec] ^Keyword [col-id col-spec])}
  col-table-kw
  "Alias for `column-table-kw`. Extracts column and table names from `col-spec` (which
  may be an identifier or a string) as lisp-cased keywords of a 2-element
  vector (first element being a column name, second a table name). If `col-spec` is
  an identifier, its namespace and name will be used. If there is no namespace, it
  will be considered a column name. If two arguments are given, names are extracted
  separately using `column-kw` and `table-kw` functions)."
  column-table-kw)

;; SQL query preparation

(defn- interpolate-tag
  ^String [substitutions [_ quote? ^String modifier ^String tag]]
  (if-some [tag (and tag (get substitutions (some-keyword tag)))]
    (let [msym (and modifier (symbol modifier))
          f    (or (if msym (var/deref-symbol
                             (if (nil? (namespace msym))
                               (symbol "amelinium.db" modifier)
                               msym)))
                   identity)]
      (if-some [^String v (some-str (f tag))]
        (if quote? (quoted v) v)
        ""))
    ""))

(defn- interpolate-some?
  [substitutions [_ ^String tag ^String on-true ^String on-false]]
  (if-let [tag (and tag (get substitutions (some-keyword tag)))]
    (str (some-str on-true))
    (str (some-str on-false))))

(defn- quote-tag
  ^String [[_ ^String tag]]
  (or (colspec-quoted tag) ""))

(defn- interpolate-tags
  "Tag interpolation for substitution patterns."
  (^String [substitutions ^String q]
   (if substitutions
     (c/-> q
           (str/replace #"%SOME\? +([^ \:]+): *([^#]*)#(?:([^#]*)#)?" #(interpolate-some? substitutions %))
           (str/replace #"%\[([^\]]+)\]" "%%table{$1}")
           (str/replace #"%\(([^\)]+)\)" "%%column{$1}")
           (str/replace #"%\<([^\>]+)\>" "%colspec-quoted{$1}")
           (str/replace #"%\'([^\']+)\'" quote-tag)
           (str/replace #"%(%)?([^\<\>\{\}\[\]\s]+)?\{([^\}]+)?\}" #(interpolate-tag substitutions %)))
     (interpolate-tags q)))
  (^String [^String q]
   (str/replace q #"%\'([^\']+)\'" quote-tag)))

(def ^{:tag    String
       :no-doc true}
  build-query-core
  "For the given SQL query `q` and substitution map performs pattern
  interpolation. Uses `memoize+` memoization."
  (db/memoize+
   (fn build-query-fn
     (^String []                "")
     (^String [q]               (if q (interpolate-tags q) ""))
     (^String [q substitutions] (if q (interpolate-tags substitutions q) "")))
   2048 1024))

(def ^{:tag    String
       :no-doc true}
  build-query-dynamic-core
  "For the given SQL query `q` and substitution map performs pattern
  interpolation. Uses `memoize+` memoization."
  (db/memoize+
   (fn build-query-fn
     (^String []                "")
     (^String [q]               (if q (interpolate-tags q) ""))
     (^String [q substitutions] (if q (interpolate-tags substitutions q) "")))
   1024 4096))

(defmacro build-query
  "For the given SQL query `q` and substitution map performs pattern interpolation.
  If multiple arguments are given the last one will be treated as substitution map.

  Tries to convert possible literals given as query parts to strings and then trim
  them while squeezing repeated spaces at compile time. If some operation cannot be
  performed in that phase, it generates code which will convert an expression to a
  string at runtime. Then pattern interpolation is performed on the resulting string,
  using the provided `substitutions` map.

  If a source string contains `%{tag-name}` special pattern, `tag-name` will be
  looked up in substitution map and the whole pattern will be replaced by the
  corresponding value.

  If a tag name from pattern cannot be found in a substitution map, the pattern will
  be replaced by an empty string.

  A pattern may have a form of `%%{tag-name}`. In such case any non-`nil` value being
  a result of tag name resolution will be quoted using `amelinium.db/quote`.

  A synonym of `%%table{tag-name}` is `%[table-name]`.
  A synonym of `%%column{tag-name}` is `%(column-name)`.
  A synonym of `%%colspec{tag-name}` is `%<column-table-specification>`.

  A pattern may have additional modifier before the opening brace. It will be
  resolved as a symbolic function name to be called in order to transform a value
  associated with a tag name. If the name is not fully-qualified (does not contain a
  namespace part) its default namespace will be set to `amelinium.db`.

  There is also additional pattern `%'column-table-specification'` which is a
  quotation pattern. It uses `colspec-quoted` function on a given text.

  Example:

  ```
  (build-query \"select %%column{id} from %%table{users}\"
               \"where\" :points '> 100
               {:id    :users/id
                :users :users/id})
  ```

  The above call will generate the following result:

  ```
  \"select `id` from `users` where points > 100\"
  ```

  This is synonymous to:

  ```
  (build-query \"select %(id) from %[users]\"
               \"where\" :points '> 100
               {:id    :users/id
                :users :users/id})
  ```

  This macro can optionally be called with a single literal sequence given as its
  first and only argument. In such cache the sequence should contain all arguments,
  including a substitution map, if applicable.

  This macro should NOT be used to dynamically generate queries having thousands of
  variant substitution parameters as it uses unlimited underlying cache. For such
  purposes please use `build-query-dynamic`, or simply utilize parameters of prepared
  statements.

  WARNING: Interpolation pattern may execute arbitrary code since it allows for any
  function name."
  {:arglists '([] [q] [q substitution-map] [coll] [& query-parts substitution-map])}
  ([] "")
  ([q]
   (if (sequential? q)
     `(build-query ~@q)
     `(build-query-core (strspc-squeezed ~q))))
  ([q substitutions]
   `(build-query-core (strspc-squeezed ~q) ~substitutions))
  ([a b & args]
   (let [v# (vec args)
         l# (peek v#)
         r# (subvec v# 0 (unchecked-dec-int (count v#)))]
     (if (str-convertable? l#)
       `(build-query-core (strspc-squeezed ~a ~b ~@v#))
       `(build-query-core (strspc-squeezed ~a ~b ~@r#) ~l#)))))

(defmacro build-query-dynamic
  "For the given SQL query `q` and substitution map performs pattern interpolation.
  If multiple arguments are given the last one will be treated as substitution map.

  Tries to convert possible literals given as query parts to strings and then trim
  them while squeezing repeated spaces at compile time. If some operation cannot be
  performed in that phase, it generates code which will convert an expression to a
  string at runtime. Then pattern interpolation is performed on the resulting string,
  using the provided `substitutions` map.

  If a source string contains `%{tag-name}` special pattern, `tag-name` will be
  looked up in substitution map and the whole pattern will be replaced by the
  corresponding value.

  If a tag name from pattern cannot be found in a substitution map, the pattern will
  be replaced by an empty string.

  A pattern may have a form of `%%{tag-name}`. In such case any non-`nil` value being
  a result of tag name resolution will be quoted using `amelinium.db/quote`.

  A synonym of `%%table{tag-name}` is `%[table-name]`.
  A synonym of `%%column{tag-name}` is `%(column-name)`.
  A synonym of `%%colspec{tag-name}` is `%<column-table-specification>`.

  A pattern may have additional modifier before the opening brace. It will be
  resolved as a symbolic function name to be called in order to transform a value
  associated with a tag name. If the name is not fully-qualified (does not contain a
  namespace part) its default namespace will be set to `amelinium.db`.

  There is also additional pattern `%'column-table-specification'` which is a
  quotation pattern. It uses `colspec-quoted` function on a given text.

  Example:

  ```
  (build-query-dynamic \"select %%column{id} from %%table{users}\"
                       \"where\" :points '> 100
                       {:id    :users/id
                        :users :users/id})
  ```

  The above call will generate the following result:

  ```
  \"select `id` from `users` where points > 100\"
  ```

  This is synonymous to:

  ```
  (build-query-dynamic \"select %%column{id} from %[users]\"
                       \"where\" :points '> 100
                       {:users :users/id})
  ```

  This macro can optionally be called with a single literal sequence given as its
  first and only argument. In such cache the sequence should contain all arguments,
  including a substitution map, if applicable.

  This macro should be used to dynamically generate queries having thousands of
  variant substitution parameters.

  WARNING: Interpolation pattern may execute arbitrary code since it allows for any
  function name."
  {:arglists '([] [q] [q substitution-map] [coll] [& query-parts substitution-map])}
  ([] "")
  ([q]
   (if (sequential? q)
     `(build-query ~@q)
     `(build-query-core (strspc-squeezed ~q))))
  ([q substitutions]
   `(build-query-dynamic-core (strspc-squeezed ~q) ~substitutions))
  ([a b & args]
   (let [v# (vec args)
         l# (peek v#)
         r# (subvec v# 0 (unchecked-dec-int (count v#)))]
     (if (str-convertable? l#)
       `(build-query-dynamic-core (strspc-squeezed ~a ~b ~@v#))
       `(build-query-dynamic-core (strspc-squeezed ~a ~b ~@r#) ~l#)))))

;; Grouped sequences processing

(defn groups-inverter
  "Helper function for transforming a map of sequences keyed with keywords into a map
  of elements found in those sequences (as keys) associated with results of calling a
  function on them with additional arguments, including original map's keys.

  In other words: transforms results of `clojure.core/group-by` into a single map,
  changing values found in sequences into keys, and associating values to those keys
  resulting from calling a function.

  Takes a function `f` and additional arguments (zero or more), and returns a
  function which takes a map `m`, identity type `id-type` and a sequence of
  identifiers `ids`, and calls `f` with all arguments and `id-type` passed on the
  sequence. Then it calls `clojure.core/into` to put the result of calling `f` into a
  map `m`.

  Example: `(groups-inverter get-ids db)`

  In this example a function will be returned, similar to the below:

  `(fn [m id-type ids] (into m (get-ids db id-type)))`.

  It is used mainly as a transformer in `reduce-kv` when dealing with multiple user
  identifiers grouped by identity type. Having a map of vectors grouped by identity
  type:

  ```
  {:email [#amelinium.Identity {:id-type :email :value \"pw@gnu.org\"}],
   :id    [#amelinium.Identity {:id-type :id :value 1}
           #amelinium.Identity {:id-type :id, :value 42}]}
  ```

  we can call `(reduce-kv (groups-inverter get-ids db) {})` to get:

  ```
  {#amelinium.Identity{:id-type :id, :value 1}               1
   #amelinium.Identity{:id-type :id, :value 42}             42
   #amelinium.Identity{:id-type :email, :value \"pw@gnu.org\"} 1}
  ```

  The `get-ids` will be called for each identity group, receiving a list of
  identities and passed arguments with identity type. After getting numerical user
  identifiers it will associate them with identity objects in a map."
  ([f]
   (fn [m ^Keyword id-type ids]
     (if id-type (or (some->> (not-empty ids) (f id-type) (into m)) m) m)))
  ([f a]
   (fn [m ^Keyword id-type ids]
     (if id-type (or (some->> (not-empty ids) (f a id-type) (into m)) m) m)))
  ([f a b]
   (fn [m ^Keyword id-type ids]
     (if id-type (or (some->> (not-empty ids) (f a b id-type) (into m)) m) m)))
  ([f a b c]
   (fn [m ^Keyword id-type ids]
     (if id-type (or (some->> (not-empty ids) (f a b c id-type) (into m)) m) m)))
  ([f a b c & more]
   (let [fargs (apply vector a b c more)]
     (fn [m ^Keyword id-type ids]
       (if id-type
         (or (some->> (not-empty ids) (conj fargs id-type) (apply f) (into m)) m) m)))))

;; Coercion

(defmulti in-coercer
  "Returns a coercer suitable for transforming the given argument `v` to a
  database-suitable value, assuming table and column specified by the given qualified
  keyword `table-column`."
  {:arglists '([^Keyword table-column v] [^Keyword column v])}
  identity)

(defmulti out-coercer
  "Returns a coercer suitable for transforming the given argument `v` read from a
  database, assuming table and column specified by the given qualified keyword
  `table-column`."
  {:arglists '([^Keyword table-column v] [^Keyword column v])}
  identity)

(defmethod in-coercer  :default [_] nil)
(defmethod out-coercer :default [_] nil)

(defn get-in-coercer*
  "Same as `get-in-coercer` but trusts that `table-column` is a fully-qualified,
  lisp-cased keyword (already a result of `colspec-kw`)."
  {:see-also ["get-in-coercer"]}
  [table-column]
  (if-some [f (in-coercer table-column)] f
           (if (namespace table-column)
             (in-coercer (keyword (name table-column))))))

(defn get-in-coercer
  "Tries to obtain a database coercion function by calling `in-coercer` multimethod for
  column and table specified with `table` and `column`, or by a single
  `table-column`. Transforms arguments to a lisp-cased keyword. If there is no
  coercer for table and column, tries to getting one using the column alone.

  Returns coercer when it is found. Returns `false` when a coercer is found but
  explicitly set to undefined. Returns `nil` when there is no coercer."
  ([table column]
   (if-some [f (in-coercer (colspec-kw table column))] f (in-coercer (column-kw column))))
  ([table-column]
   (get-in-coercer* (colspec-kw table-column))))

(defn get-out-coercer*
  "Same as `get-out-coercer` but trusts that `table-column` is a lisp-cased
  keyword (already a result of `colspec-kw`)."
  {:see-also ["get-out-coercer"]}
  [table-column]
  (if-some [f (out-coercer table-column)] f
           (if (namespace table-column)
             (out-coercer (keyword (name table-column))))))

(defn get-out-coercer
  "Tries to obtain a database coercion function by calling `out-coercer` multimethod
  for column and table specified with `table` and `column`, or by a single
  `table-column`. Transforms arguments to a lisp-cased keyword. If there is no
  coercer for table and column, falls back to getting one using the column alone.

  Returns coercer when it is found. Returns `false` when a coercer is found but
  explicitly set to undefined. Returns `nil` when there is no coercer."
  ([table column]
   (if-some [f (out-coercer (colspec-kw table column))] f (out-coercer (column-kw column))))
  ([table-column]
   (get-out-coercer* (colspec-kw table-column))))

(defn- statically-convertable?
  "Returns `true` if `v` can be statically handled at compile-time, `false` otherwise."
  ([v]
   (or (nil? v) (keyword? v) (string? v) (number? v) (boolean? v)))
  ([v ts cs]
   (and (string? ts) (string? cs)
        (or (nil? v) (keyword? v) (string? v) (number? v) (boolean? v)))))

(defn coerce-in
  "Coerces the given value `v` to a database type by calling a function returned by
  invoking `amelinium.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function. If there is no coercer, returns unchanged `v`.

  Will immediately return the given value `v` if the coercer exists but it is set to
  `false`."
  ([table column v] (if-let [f (get-in-coercer table column)] (f v) v))
  ([table-column v] (if-let [f (get-in-coercer table-column)] (f v) v)))

(defn coerce-in*
  "Same as `coerce-in` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column v] (if-let [f (get-in-coercer* table-column)] (f v) v))

(defn coerce-out
  "Coerces the given value `v` from a database type by calling a function returned by
  invoking `amelinium.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function (to use column alone). If there is no coercer, returns
  unchanged `v`.

  Will immediately return the given value `v` if the coercer exists but it is
  set to `false`."
  ([table column v] (if-let [f (get-out-coercer table column)] (f v) v))
  ([table-column v] (if-let [f (get-out-coercer table-column)] (f v) v)))

(defn coerce-out*
  "Same as `coerce-out` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column v] (if-let [f (get-out-coercer* table-column)] (f v) v))

(defn coerce-seq-in
  "Coerces a sequence of values `coll` to database types by calling a function returned
  by invoking `amelinium.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function (to use column alone). If there is no coercer, returns
  unchanged `coll`."
  ([table column coll] (if-let [f (get-in-coercer table column)] (map f coll) coll))
  ([table-column coll] (if-let [f (get-in-coercer table-column)] (map f coll) coll)))

(defn coerce-seq-in*
  "Same as `coerce-seq-in` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column coll] (if-let [f (get-in-coercer* table-column)] (map f coll) coll))

(defn coerce-seq-out
  "Coerces a sequence of values `coll` from database types by calling a function
  returned by invoking `amelinium.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function (to use column alone). If there is no coercer, returns
  unchanged `coll`."
  ([table column coll] (if-let [f (get-out-coercer table column)] (map f coll) coll))
  ([table-column coll] (if-let [f (get-out-coercer table-column)] (map f coll) coll)))

(defn coerce-seq-out*
  "Same as `coerce-seq-out` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column coll] (if-let [f (get-out-coercer* table-column)] (map f coll) coll))

(defmacro <-
  "Coerces value `v` to a database type by calling a function returned by invoking
  `amelinium.db/in-coercer` multimethod on a qualified keyword `table-column` (or a
  qualified keyword made out of `table` and `column`). If there is no coercer
  attached for the keyword, returns unchanged `v`.

  If a coercer can be obtained at compile-time, a coercion function-call form will be
  generated. If a coercer can be obtained at compile-time and the given value is
  statically convertable, value resulting from applying coercion function will be
  generated immediately."
  ([table column v]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (colspec-kw table column)]
       (if-some [coercer-fn (get-in-coercer* tc)]
         (if coercer-fn
           (if (statically-convertable? v) (coercer-fn v) (list `~coercer-fn `~v))
           `~v)
         `(coerce-in* ~tc ~v)))
     `(coerce-in ~table ~column ~v)))
  ([table-column v]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (colspec-kw table-column)]
       (if-some [coercer-fn (get-in-coercer* tc)]
         (if coercer-fn
           (if (statically-convertable? v) (coercer-fn v) (list `~coercer-fn `~v))
           `~v)
         `(coerce-in* ~tc ~v)))
     `(coerce-in ~table-column ~v))))

(defn ->
  "Coerces value `v` from a database type by calling a function returned by invoking
  `amelinium.db/out-coercer` multimethod on a qualified keyword `table-column` (or a
  qualified keyword made out of `table` and `column`). If there is no coercer
  attached for the keyword, returns unchanged `v`.

  If a coercer can be obtained at compile-time, a coercion function-call form will be
  generated. If a coercer can be obtained at compile-time and the given value is
  statically convertable, value resulting from applying coercion function will be
  generated immediately."
  ([table column v]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (colspec-kw table column)]
       (if-some [coercer-fn (get-out-coercer* tc)]
         (if coercer-fn
           (if (statically-convertable? v) (coercer-fn v) (list `~coercer-fn `~v))
           `~v)
         `(coerce-out* ~tc ~v)))
     `(coerce-out ~table ~column ~v)))
  ([table-column v]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (colspec-kw table-column)]
       (if-some [coercer-fn (get-out-coercer* tc)]
         (if coercer-fn
           (if (statically-convertable? v) (coercer-fn v) (list `~coercer-fn `~v))
           `~v)
         `(coerce-out* ~tc ~v)))
     `(coerce-out ~table-column ~v))))

(defmacro <-seq
  "Coerces a sequence of values `coll` to database types by calling a function returned
  by invoking `amelinium.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, returns unchanged `coll`.

  If both, a table and a column can be used to establish coercion function at
  compile-time, a mapping form will be generated which uses that function."
  ([table column coll]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (colspec-kw table column)]
       (if-some [coercer-fn (get-in-coercer* tc)]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-in* ~tc ~coll)))
     `(coerce-seq-in ~table ~column ~coll)))
  ([table-column coll]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (colspec-kw table-column)]
       (if-some [coercer-fn (get-in-coercer* tc)]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-in* ~tc ~coll)))
     `(coerce-seq-in ~table-column ~coll))))

(defmacro seq->
  "Coerces a sequence of values `coll` from database types by calling a function
  returned by invoking `amelinium.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, returns unchanged `coll`.

  If both, a table and a column can be used to establish coercion function at
  compile-time, a mapping form will be generated which uses that function."
  ([table column coll]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (colspec-kw table column)]
       (if-some [coercer-fn (get-out-coercer* tc)]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-out* ~tc ~coll)))
     `(coerce-seq-out ~table ~column ~coll)))
  ([table-column coll]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (colspec-kw table-column)]
       (if-some [coercer-fn (get-out-coercer* tc)]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-out* ~tc ~coll)))
     `(coerce-seq-out ~table-column ~coll))))

(defrecord QSlot [^String t ^String c v])

(defn- not-empty-qslot?
  "Returns `true` if the given `e` is of type `QSlot` and all of its essential fields
  are not empty."
  [e]
  (and (instance? QSlot e) (.t ^QSlot e) (.c ^QSlot e) (.v ^QSlot e)))

(defn- join-qslots
  "Joins consecutive `QSlot` records if their column and table fields are equal."
  [done qs]
  (let [prev (peek done)]
    (if (and (not-empty-qslot? qs) (not-empty-qslot? prev))
      (if (and (= (.t ^QSlot prev) (.t ^QSlot qs))
               (= (.c ^QSlot prev) (.c ^QSlot qs)))
        (conj (pop done) (qupdate prev :v into (.v ^QSlot qs)))
        (conj done qs))
      (conj done qs))))

(defn- c-t
  "Extracts column and table specs as a vector from the given control keyword and
  predefined hints for table and column spec."
  [ctrl table-spec column-spec]
  (if (keyword? ctrl)
    (let [[c t] (column-table ctrl)]
      (if c
        (if t [c t] (if table-spec [c table-spec] [table-spec c]))
        [column-spec table-spec]))))

(defn- pp-conv-specs
  "Pre-parses table/column conversion specifications."
  ([coll]
   (->> (mapcat #(pp-conv-specs % nil nil) coll)
        (reduce join-qslots [])))
  ([e tspec cspec]
   (cond

     ;; control vector
     ;; (group of values followed by table or column spec)

     (vector? e)
     (let [ctrl (nth e 0 nil), coll (subvec e 1)]
       (if (keyword? ctrl)
         ;; static table/column name
         (let [[c t] (c-t ctrl tspec cspec)]
           (mapcat #(pp-conv-specs % t c) coll))
         ;; dynamic table/column name
         (let [[c t] (if tspec [ctrl tspec] [nil ctrl])]
           (mapcat #(pp-conv-specs % t c) coll))))

     ;; single value
     ;; (known table and column)

     (and tspec cspec (statically-convertable? e tspec cspec))
     (if-some [coercer-fn (get-in-coercer tspec cspec)]
       (cons (if coercer-fn (coercer-fn e) e) nil)
       (cons (QSlot. tspec cspec [e]) nil))

     ;; regular dynamically-convertable element
     ;; (known table and column)

     (and tspec cspec)
     (cons (QSlot. tspec cspec [e]) nil)

     ;; single value expressed with a simple symbol
     ;; (known table, missing column)

     (and tspec (simple-symbol? e))
     (cons (QSlot. tspec (name e) [e]) nil)

     ;; any value

     :else (cons e nil))))

(defn- parse-conv-spec
  "Parses value with optional table/column conversion specification to produce a source
  code. Expects values or `QSlot` records."
  [e]
  (if (or (not (instance? QSlot e)) (nil? (.t ^QSlot e)))
    (cons e nil)
    (let [t  (.t ^QSlot e)
          c  (.c ^QSlot e)
          v  (.v ^QSlot e)
          tc (if (and (or (keyword? t) (string? t))
                      (or (keyword? c) (string? c))) (colspec-kw t c))]
      (if (= (count v) 1)
        (if tc
          (cons `(<- ~tc   ~(nth v 0)) nil)
          (cons `(<- ~t ~c ~(nth v 0)) nil))
        (if tc
          (if-some [coercer-fn (get-in-coercer* tc)]
            (if coercer-fn (map (fn [e] `(~coercer-fn ~e)) v) `~v)
            (map (fn [e] `(<- ~tc ~e)) v))
          (map (fn [e] `(<- ~t ~c ~e)) v))))))

(defn gen-qs-keyword
  "Generates unique but deterministic symbolic name for `t` (presumably table name),
  `c` (column name) and `v` (value, being an identifier). Returns a keyword named
  like `DB__[t]_[c]_[v]_[nnnnnnn]` where `[t]`, `[c]` and `[v]` are string
  representations of the given argument values, and `[nnnnnnn]` is a numeric
  representation of combined hash of all values given as arguments."
  ([^QSlot qs]   (gen-qs-keyword (.t qs) (.c qs) (.v qs)))
  ([^QSlot qs v] (gen-qs-keyword (.t qs) (.c qs) v))
  ([t c v]
   (let [^String h (c/-> (hash t) (hash-combine (hash c)) (hash-combine (hash v)) strb)
         ^String h (if (identical? \- (.charAt h 0)) (strb "0" (subs h 1)) h)]
     (keyword (strb "DB__" (some-str t) "_" (some-str c) "_" (some-str v) "_" h)))))

(defn- repeating-qslot-bindings
  "Returns a map with keys being keywords representing unique table/column/value names
  identifying repeated, single-valued `QSlot` elements from the given `coll`, and
  with associated values being expressions for performing output database coercion."
  [coll]
  (->> (filter #(instance? QSlot %) coll)
       (remove #(or (nil? (.t ^QSlot %)) (nil? (.c ^QSlot %)) (list? (.t ^QSlot %)) (list? (.c ^QSlot %))))
       (mapcat #(map (fn [v] (qassoc % :v v)) (.v ^QSlot %)))
       (frequencies) (seq)
       (filter #(> (val %) 1))
       (map (juxt #(gen-qs-keyword (key %)) #(qupdate (key %) :v vector)))
       (into {})
       (map/map-vals (comp first parse-conv-spec))))

(defn bindable-sym
  "Returns a bindable, auto-generated symbol for the table/column (from `qs`, which
  should be a `QSlot` record) and a value `v`. The unique identifier (obtained using
  `gen-qs-keyword`) must exists in `bindings` map. Otherwise, `nil` is returned."
  [bindings ^QSlot qs v]
  (let [h (gen-qs-keyword qs v)]
    (if (contains? bindings h) (symbol h))))

(defn bindable-sym?
  "Returns `true` if a bindable, auto-generated symbol for the table/column (from `qs`,
  which should be a `QSlot` record) and a value `v` exists in `bindings`
  map. Otherwise it returns `false`."
  [bindings ^QSlot qs v]
  (contains? bindings (gen-qs-keyword qs v)))

(defn- replace-bindable
  "Replaces bindable expressions from `:v` fields of `QSlot` records present in `coll`
  by unique symbols corresponding to them, with names created with `gen-qs-keyword`
  which exist as keys in `bindings` map."
  [bindings coll]
  (vec
   (mapcat
    (fn [qs]
      (if (instance? QSlot qs)
        (let [parts    (partition-by #(bindable-sym? bindings qs %) (.v ^QSlot qs))
              first-b? (bindable-sym? bindings qs (ffirst parts))]
          (mapcat (fn [qvals bindable?]
                    (if bindable?
                      (map #(bindable-sym bindings qs %) qvals)
                      (cons (qassoc qs :v (vec qvals)) nil)))
                  parts (iterate not first-b?)))
        (cons qs nil)))
    coll)))

(defn- prepend-qslot-bindings
  "Wraps the given `coll` in a `let` block with `bindings`."
  [bindings coll]
  (if-some [bindings (seq bindings)]
    (list `let (vec (mapcat #(update % 0 symbol) bindings)) coll)
    coll))

(defmacro <<-
  "Magical macro which converts a sequence of values with optional table and column
  specifications to a database-suitable formats. Pre-processing of arguments is
  executed at compile-time, further processing is performed at run-time.

  Any type of argument is accepted but literal vectors, including nested vectors, are
  **control structures**. Their first elements are table names (for the first one),
  column names (for nested vectors) or both (when expressed using fully-qualified
  keywords).

  Example: `(<<- 1 2 3 [:table-name [:column-name :val1 val2 (exp3) \"val4\"])`

  `1`, `2` and `3` are regular values, `:table-name` is literally expressed table
  name for coercer, `:column-name` is literally expressed column name for coercer,
  other expressions are values to be coerced. Table and column names can be dynamic,
  expressed with symbols or call forms.

  All macro arguments are sequentially transformed with the following rules:

  - If there is a **literal vector** at **1st level**, its first element should be a
  table name. All elements of that vector will inherit that table name during
  conversion to a database-suitable format.

  - If there is a **literal vector** nested within existing vector its first element
  will be considered a **column name**. All elements of that vector will inherit that
  column name during conversion to a database-suitable format.

  - If the given literal vector contains a **fully-qualified keyword** at its first
  position then both **table** and **column** will be memorized to be applied during
  conversion of elements contained in that vector (table taken from a namespace and
  column from a name of the keyword).

  - If there is a table name inherited but no column specified, a value is not
  converted but returned as is, with the exception: when the value is expressed as a
  **literal, simple symbol** then a column name will be derived from its name.

  - A sub-vector may begin with an unspecified value `nil`. In such case it will
  group values to be converted but column name will not be set. The values will be
  left unconverted unless they are simple symbol forms; in such case column names
  will be derived from their names.

  Values used to set column and table names at the beginnings of vectors can be
  expressed with any valid code. However, literal strings or literal keywords (or
  symbols in case of identifier-derived column names) will be pre-processed at
  compile time. So, if both column and table name are expressed that way, the
  conversion specifier will be generated ahead.

  Moreover, if apart from the above, a value to be coerced is expressed as a literal
  number, string, keyword, boolean, or a `nil`, it will be converted at compile-time.

  Conversion of repeating atomic expressions sharing the same table and column name
  identifiers (including symbolic identifiers) will be performed in ad-hoc created
  `let` block, and the results will be referenced using auto-generated symbols
  replacing the original expressions. Therefore, conversion for repeated symbols (and
  other atomic expressions) will be performed just once.

  Examples:

  `(<<- [:users [:id id] [:email email]])`

  The above will convert values expressed by `id` and `email` symbol forms using a
  variant of coercion multimethod registered for `:users/id` and `:users/email`
  keywords, accordingly.

  `(<<- [:users/id id [:email e]])`

  The above will convert values of `id` and `e` symbol forms using a variant of
  coercion multimethod registered for `:users/id` and `:users/email` keywords,
  accordingly.

  `(<<- [:users [:id id] [:confirmations/email email [:expires expires]])`

  The above will convert values of `id`, `email` and `expires` symbol forms using a
  variant of coercion multimethod registered for `:users/id`, `:confirmations/email`,
  and `:confirmations/expires` keywords, accordingly. We can see that second vector
  changes the table name to `confirmations` in its scope so later `expires` derives
  it and is converted with `:confirmations/expires` specification.

  This is synonymous to:

  `(<<- [:users id] [:confirmations email expires])`

  As we can see, the `id` symbolic identifier is used to set the column name, same as
  `email` and `expires` symbols. The above code will be expanded to:

  ```
  [(amelinium.db/<- :users/id id)
   (amelinium.db/<- :confirmations/email email)
   (amelinium.db/<- :confirmations/expires expires)]
  ```

  And then to:

  ```
  [(#<Fn@4d7e49d amelinium.model.user/id_to_db> id)
   (amelinium.db/coerce-in* :confirmations/email email)
   (#<Fn@3bf6fdc6 amelinium.model.confirmation/to_expiry> expires)]
  ```

  We can see that coerces for `id` and `expires` symbols were resolved and function
  call forms were created at compile-time. That's because `:users/id` and
  `:confirmations/expires` were recognized as existing dispatch values when calling
  `in-coercer` internally. A coercer for the `email` symbol (using
  `:confirmations/email` dispatch value) was not recognized at compile-time so
  the call to `amelinium.db/coerce-in*` was generated instead.

  Let's have a quick look at some real-world example:

  ```
  (amelinium.db/<<-  [:confirmations id code token reason id-type expires id id])
  ```

  And generated Clojure code (phase 1):

  ```
  (let [DB__confirmations_id_id_54377141 (amelinium.db/<- :confirmations/id id)]
    [DB__confirmations_id_id_54377141
     (amelinium.db/<- :confirmations/code       code)
     (amelinium.db/<- :confirmations/token     token)
     (amelinium.db/<- :confirmations/reason   reason)
     (amelinium.db/<- :confirmations/id-type id-type)
     (amelinium.db/<- :confirmations/expires expires)
     DB__confirmations_id_id_54377141
     DB__confirmations_id_id_54377141])
  ```

  And fully expanded:

  ```
  (let* [DB__confirmations_id_id_54377141 (#<Fn@5a424a5a amelinium.identity/__GT_db> id)]
    [DB__confirmations_id_id_54377141
     (#<Fn@279d4dd9 io.randomseed.utils/safe_parse_long>       code)
     (#<Fn@7f1abd95 io.randomseed.utils/some_str>             token)
     (#<Fn@7f1abd95 io.randomseed.utils/some_str>            reason)
     (#<Fn@7f1abd95 io.randomseed.utils/some_str>           id-type)
     (#<Fn@4ac5d426 amelinium.model.confirmation/to_expiry> expires)
     DB__confirmations_id_id_54377141
     DB__confirmations_id_id_54377141])
  ```

  A SQL query which uses the sequence of values presented above needs one of
  them (identified with the `id`) to be repeated. We can observe that the macro
  generated `let` binding for it to assign the result of calling
  `amelinium.identity/->db` on `id` to auto-generated symbol named
  `DB__confirmations_id_id_54377141`. This symbol is then re-used in output vector
  multiple times so the calculation is performed just once.

  Also, other coercers were successfully resolved to function objects during macro
  expansion since we have static table and column specifiers given.

  Rule of a thumb is: if you can express certain values or specifications with
  literal strings or keywords, it may speed things up."
  [& specs]
  (let [pre-converted (pp-conv-specs specs)
        bindings      (repeating-qslot-bindings pre-converted)]
    (->> (replace-bindable bindings pre-converted)
         (mapcat parse-conv-spec) vec
         (prepend-qslot-bindings bindings))))

(defmacro <<-*
  "Same as `<<-` but its last argument should be a sequence to be concatenated with the
  results without any pre-processing."
  {:see-also ["<<-"]}
  ([spec]
   `(<<- ~spec))
  ([spec & specs]
   `(into (or (<<- ~@(cons spec (butlast specs))) []) ~(last specs))))

(defn simple->
  [table m]
  (let [table (some-str table)]
    (map/map-vals-by-kv #(-> (keyword table (name %1)) %2) m)))

(defn map->
  ([m]
   (map/map-vals-by-kv -> m))
  ([table m]
   (let [table (some-str table)]
     (map/map-vals-by-kv
      #(-> (if (namespace %1) %1 (keyword table (name %1))) %2)
      m))))

(defmacro defcoercions
  "Defines input and output coercions for a database table `table`. The `specs` should
  be an argument list consisting of triples in a form of `column-name`,
  `input-coercer`, `output-coercer`.

  For each definition 4 multimethod implementations will be emitted, identified by a
  keyword having a namespace the same as the given table, and a name the same as
  currently processed column name, both in 2 variants: one for snake, and one for
  lisp case. Two multimethod definitions will be created for
  `amelinium.db/in-coercer` and two for `amelinium.db/out-coercer`.

  If the given coercer is a literal `nil` or `false` value, it will be marked as
  undefined using `false` value. It is advised to use that approach instead of
  assigning `identity` function to express that coercion is not needed since it can
  cause compile-time calculations to short-circuit instead of generating fallback
  code.

  Example:

  `(defcoercions :users :some-identifier str keyword)`

  The above will expand the following code:

  ```
  (defmethod amelinium.db/in-coercer  :users/some-identifier [_] str)
  (defmethod amelinium.db/in-coercer  :users/some_identifier [_] str)
  (defmethod amelinium.db/out-coercer :users/some-identifier [_] keyword)
  (defmethod amelinium.db/out-coercer :users/some_identifier [_] keyword)
  ```

  This will allow specialized database coercion functions to transformed values which
  are exchanged with a database.

  Optionally coercions can be defined without a table name. In such case the table
  should be set to either `nil`, `false` or `:amelinium.db/any`."
  [table & specs]
  (let [t# (if (identical? table ::any) nil (some-str table))]
    `(do
       ~@(mapcat
          (fn [[c# in# out#]]
            `((defmethod  in-coercer ~(colspec-kw    t# c#) [~'_] (or ~in#  false))
              (defmethod  in-coercer ~(make-kw-snake t# c#) [~'_] (or ~in#  false))
              (defmethod out-coercer ~(colspec-kw    t# c#) [~'_] (or ~out# false))
              (defmethod out-coercer ~(make-kw-snake t# c#) [~'_] (or ~out# false))))
          (partition 3 specs))
       nil)))

(defn- get-db-out-coercer
  "Gets database output coercer on a basis of table name and column label from a result
  set metadata object (`rsm`) and index number (`i`). If there is no coercer found
  and column label differs from column name, tries with table and column
  name. Returns a function (coercer found), `false` (coercer found but value should
  remain as-is) or `nil` (coercer not found)."
  [^ResultSetMetaData rsm ^Integer i]
  (let [tab-name  (.getTableName   rsm i)
        col-label (.getColumnLabel rsm i)]
    (if-some [coercer-fn (get-out-coercer* (make-kw-simple tab-name col-label))]
      coercer-fn
      (let [col-name (.getColumnName rsm i)]
        (if (identical? col-label col-name)
          (get-out-coercer* (make-kw-simple tab-name col-name)))))))

(defn- delayed-column-by-index-fn
  "Adds coercion to a database result set `rs` handled by builder `builder` with result
  index `i`. For each result it reads its table name and column label (or name, if
  label is not set or is different from label but no coercer has been found), and
  calls output coercer obtained using `amelinium.db/out-coercer`. Each result is
  wrapped in a Delay object unless it does not require coercion."
  [builder ^ResultSet rs ^Integer i]
  (let [^ResultSetMetaData rsm (.getMetaData rs)
        v                      (.getObject rs i)
        coercer-fn             (get-db-out-coercer rsm i)]
    (rs/read-column-by-index (if coercer-fn (delay (coercer-fn v)) v) rsm i)))

(defn- column-by-index-fn
  "Adds coercion to a database result set `rs` handled by builder `builder` with result
  index `i`. For each result it reads its table name and column label (or name, if
  label is not set or is different from label but no coercer has been found), and
  calls output coercer using `amelinium.db/->` passing to it the mentioned names and
  a value."
  [builder ^ResultSet rs ^Integer i]
  (let [^ResultSetMetaData rsm (.getMetaData rs)
        v                      (.getObject rs i)
        coercer-fn             (get-db-out-coercer rsm i)]
    (rs/read-column-by-index (if coercer-fn (coercer-fn v) v) rsm i)))

(defn gen-builder
  "Generates result set builder on a basis of the given builder `rs-builder`. Uses
  `amelinium.db/column-by-index-fn` to coerce the results."
  [rs-builder]
  (rs/builder-adapter rs-builder column-by-index-fn))

(defn gen-builder-delayed
  "Generates result set builder on a basis of the given builder `rs-builder`. Uses
  `amelinium.db/delayed-column-by-index-fn` to coerce the results."
  [rs-builder]
  (rs/builder-adapter rs-builder delayed-column-by-index-fn))

;; Predefined database options

(def opts-map               (update db/opts-map         :builder-fn gen-builder))
(def opts-simple-map        (update db/opts-simple-map  :builder-fn gen-builder))
(def opts-vec               (update db/opts-vec         :builder-fn gen-builder))
(def opts-simple-vec        (update db/opts-simple-vec  :builder-fn gen-builder))
(def opts-slashed-map       (update db/opts-slashed-map :builder-fn gen-builder))
(def opts-slashed-vec       (update db/opts-slashed-vec :builder-fn gen-builder))
(def opts-lazy-vec          (update db/opts-vec         :builder-fn gen-builder-delayed))
(def opts-lazy-simple-vec   (update db/opts-simple-vec  :builder-fn gen-builder-delayed))
(def opts-lazy-slashed-vec  (update db/opts-slashed-vec :builder-fn gen-builder-delayed))
(def opts-lazy-map          (update db/opts-map         :builder-fn gen-builder-delayed))
(def opts-lazy-simple-map   (update db/opts-simple-map  :builder-fn gen-builder-delayed))
(def opts-lazy-slashed-map  (update db/opts-slashed-map :builder-fn gen-builder-delayed))

;; Query params

(defmacro <q
  "Simple wrapper around `build-query` and `<<-` macros. First argument should be a
  query (possibly grouped with a vector, if multiple arguments need to be passed),
  all other arguments are passed to `<<-`.

  Produces a sequence suitable to be used with `execute-*` family of functions (a
  parameterized query as its first element and coerced query parameters as other
  elements).

  Example:

  ```
  (<q [\"select %(id) from %[u] where %(id) = ? AND %(t) = ?\"
       {:id :users/id, :u :users/id, :t :users/account-type}]
      [:users [:id \"42\" :account-type :user]])
  ```

  The above will return:

  ```
  (\"select `id` from `users` where `id` = ? AND `account_type` = ?\" 42 \"user\")
  ```

  Note that `\"42\"` and `:user` are values which are to be coerced (first with a
  coercer registered for `:users/id` and second with a coercer registered for
  `:users/account-type`). After the coercion resulting values (`42` and `\"user\"`)
  are placed in a sequence to become query parameters."
  [query & params]
  `(cons (build-query ~query) (<<- ~@params)))

(defmacro <dq
  "Simple wrapper around `build-query-dynamic` and `<<-` macros. First argument should
  be a query (possibly grouped with a vector, if multiple arguments need to be
  passed), all other arguments are passed to `<<-`.

  Produces a sequence suitable to be used with `execute-*` family of functions (a
  parameterized query as its first element and coerced query parameters as other
  elements).

  Intended to be used for dynamically generated database queries. Uses a bit slower
  but safer FIFO cache of default size (about 150k items).

  Example:

  ```
  (<q [\"select %(id) from %[u] where %(id) = ? AND %(t) = ?\"
       {:id :users/id, :u :users/id, :t :users/account-type}]
      [:users [:id \"42\"] [:account-type :user]])
  ```

  The above will return:

  ```
  (\"select `id` from `users` where `id` = ? AND `account_type` = ?\" 42 \"user\")
  ```

  Note that `\"42\"` and `:user` are values which are to be coerced (first with a
  coercer registered for `:users/id` and second with a coercer registered for
  `:users/account-type`). After the coercion resulting values (`42` and `\"user\"`)
  are placed in a sequence to become query parameters."
  [query & params]
  `(cons (build-query-dynamic ~query) (<<- ~@params)))

(defmacro <d-do!
  [f db query & params]
  `(~f ~db (cons (build-query-dynamic ~query) (<<- ~@params))))

(defmacro <d-exec!
  [db query & params]
  `(execute! ~db (cons (build-query-dynamic ~query) (<<- ~@params))))

(defmacro <d-exec-one!
  [db query & params]
  `(execute-one! ~db (cons (build-query-dynamic ~query) (<<- ~@params))))

(defmacro <do!
  [f db query & params]
  `(~f ~db (cons (build-query ~query) (<<- ~@params))))

(defmacro <exec!
  [db query & params]
  `(execute! ~db (cons (build-query ~query) (<<- ~@params))))

(defmacro <exec-one!
  [db query & params]
  `(execute-one! ~db (cons (build-query ~query) (<<- ~@params))))

;; Main wrappers

(defn lazy-execute-one!
  ([connectable sql-params opts]
   (let [m (jdbc/execute-one! connectable sql-params (into opts-lazy-simple-map opts))]
     (if m (map/to-lazy m) m)))
  ([connectable sql-params]
   (lazy-execute-one! connectable sql-params nil)))

(defn lazy-execute!
  ([connectable sql-params opts]
   (if-some [coll (jdbc/execute! connectable sql-params (into opts-lazy-simple-map opts))]
     (mapv #(if % (map/to-lazy %) %) coll)))
  ([connectable sql-params]
   (lazy-execute! connectable sql-params nil)))

(defn execute-one!
  ([connectable sql-params opts]
   (jdbc/execute-one! connectable sql-params (into opts-simple-map opts)))
  ([connectable sql-params]
   (execute-one! connectable sql-params nil)))

(defn execute!
  ([connectable sql-params opts]
   (jdbc/execute! connectable sql-params (into opts-simple-map opts)))
  ([connectable sql-params]
   (execute! connectable sql-params nil)))

(defmacro lazy-do
  [& cmd]
  `(let [r# ~@cmd] (if r# (map/to-lazy r#) r#)))

(defn lazy-get-by-id
  "Like `next.jdbc/get-by-id` but supports lazy maps."
  ([connectable table pk]
   (lazy-get-by-id connectable table pk :id {}))
  ([connectable table pk opts]
   (lazy-get-by-id connectable table pk :id opts))
  ([connectable table pk pk-name opts]
   (let [opts (into (into (or (:options connectable) {}) opts-lazy-simple-map) opts)]
     (lazy-do (sql/get-by-id connectable table pk pk-name opts)))))

;; Abstract getters and setters

(defn make-getter
  ([f opts id-col cols]
   (make-getter f opts nil id-col cols nil))
  ([f opts table id-col cols]
   (make-getter f opts table id-col cols nil))
  ([f opts table id-col cols getter-coll-fn]
   (let [id-col (keyword id-col)
         cols   (if (map? cols)  (keys cols) cols)
         cols   (if (coll? cols) (seq cols) cols)
         cols   (if (coll? cols) cols [(or cols "*")])
         table  (db/to-snake-simple table)
         q      (str-spc "SELECT" (db/join-col-names cols)
                         "FROM"   (or table "?")
                         "WHERE"  (db/to-snake-simple id-col) "= ?")]
     (if table
       (if getter-coll-fn
         (fn db-lazy-getter
           ([db id]          (db-lazy-getter db nil id))
           ([db _ id]        (f db [q id] opts))
           ([db _ id & more] (getter-coll-fn db (cons id more))))
         (fn [db _ id]
           (f db [q id] opts)))
       (if getter-coll-fn
         (fn
           ([db table id]        (f db [q (db/to-snake-simple table) id] opts))
           ([db table id & more] (getter-coll-fn db table (cons id more))))
         (fn [db table id]
           (f db [q (db/to-snake-simple table) id] opts)))))))

(defn make-getter-coll
  "Creates a database getter suitable for use with `get-cached-coll-` family of
  functions. The returned function should accept an argument containing multiple
  identifiers."
  ([f opts id-col]
   (make-getter-coll f opts nil id-col nil))
  ([f opts id-col cols]
   (make-getter-coll f opts nil id-col cols))
  ([f opts table id-col cols]
   (let [id-col (keyword id-col)
         cols   (if (map? cols) (keys cols) cols)
         cols   (if (coll? cols) (seq cols) cols)
         cols   (if (coll? cols) cols [(or cols "*")])
         table  (db/to-snake-simple table)
         q      (str-spc "SELECT" (join-col-names cols)
                         "FROM"   (or table "?")
                         "WHERE"  (db/to-snake-simple id-col)
                         "IN (")]
     (if table
       (fn db-getter-coll
         ([db ids]   (db-getter-coll db nil ids))
         ([db _ ids] (if-some [ids (seq ids)]
                       (let [query (str q (join-? ids) ")")]
                         (->> (f db (cons query ids) opts)
                              (reduce #(qassoc %1 (get %2 id-col) %2) {}))))))
       (fn [db table ids]
         (if-some [ids (seq ids)]
           (let [ids   (map id-to-db ids)
                 table (db/to-snake-simple table)
                 query (str q (join-? ids) ")")]
             (->> (f db (cons query (cons table ids)) opts)
                  (reduce #(qassoc %1 (get %2 id-col) %2) {})))))))))

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
    (c/-> config
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
     (let [db-props (c/-> :properties config (dissoc :logger :migrations-dir) prep-db)
           db-name  (db-name db-props config k)
           db-key   (db-key-name k db-props config)
           db-props (map/assoc-missing db-props :name db-name :dbkey db-key)]
       (log/msg "Configuring database" db-name (str "(" db-key ")"))
       (DBConfig. ^Fn      ds-getter
                  ^Fn      ds-closer
                  ^Fn      ds-suspender
                  ^Fn      ds-resumer
                  ^Keyword db-key
                  ^String  db-name
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
    (c/-> config
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
        config (c/-> config
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

;; Generic coercers

(defn- email-to-db ^String [v] (identity/to-db :email v))
(defn- phone-to-db ^String [v] (identity/to-db :phone v))
(defn- long-or-nil ^Long   [n] (if n (long n)))

(defcoercions ::any
  :email             email-to-db                  some-str
  :phone             phone-to-db                  identity/preparse-phone
  :account-type      some-str                     some-keyword
  :first-name        some-str                     some-str
  :middle-name       some-str                     some-str
  :last-name         some-str                     some-str
  :ip                ip/to-address                ip/string-to-address
  :ip-address        ip/to-address                ip/string-to-address
  :password-suite-id safe-parse-long              long-or-nil
  :password          nil                          nil)
