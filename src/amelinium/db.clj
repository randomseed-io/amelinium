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
            [io.randomseed.utils.map       :refer            [qassoc]]
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
            [amelinium.identity            :as               identity]
            [puget.printer :refer [cprint]])

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
    (jp/set-parameter (identity/->db v) ps i)))

;; Keyword processing

(defn idname
  "If the given value `v` is an ident, it returns its (optional) namespace and name
  joined with a dot character. Otherwise it returns the string representation of
  the given object."
  ^String [v]
  (if (ident? v)
    (if-some [nsp (namespace v)]
      (str nsp "." (name v))
      (str/replace (name v) \/ \.))
    (str/replace (str v) \/ \.)))

(defn idname-simple
  "If the given value `v` is an ident, it returns its name. Otherwise it returns the
  string representation of the given object."
  ^String [v]
  (if (ident? v) (name v) (str v)))

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
  "Converts a `table/column` identifier `table-col` into a snake-cased string. If
  `table-id` and `col-id` are given, it creates a string of those parts joined with a
  slash character. If identifier is given, it uses its namespace and name."
  (^String [col-spec]
   (db/to-snake col-spec))
  (^String [table-id col-id]
   (if col-id
     (db/to-snake (str (idname table-id) "/" (idname col-id)))
     (colspec table-id))))

(defn colspec-kw
  "Converts a `table/column` identifier `col-spec` into a lisp-cased keyword,
  qualified if possible. If `table-id` and `col-id` are given, it creates a keyword of
  those parts, `table-id` being its namespace and `col-id` being its name."
  (^String [col-spec]
   (some-keyword (db/to-lisp col-spec)))
  (^String [table-id col-id]
   (if col-id
     (keyword (db/to-lisp (idname table-id)) (db/to-lisp (idname col-id)))
     (colspec-kw table-id))))

(defn table
  "Extracts table name as a snake-cased string from `col-spec` which may be an
  identifier or a string. If the identifier has a namespace, it will be used,
  otherwise its name will be used. For strings, it will look for a slash separator to
  detect namespace (a table name) and name (a column name), to pick a table name. If
  two arguments are given, the second one is ignored."
  (^String [col-spec]
   (if (ident? col-spec)
     (db/to-snake (or (namespace col-spec) (name col-spec)))
     (if (some? col-spec) (table (some-keyword col-spec)))))
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
     (if (some? col-spec) (column (some-keyword col-spec)))))
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
  will be used. If two arguments are given, names are extracted separately using
  `table` and `column` functions)."
  ([col-spec]
   (if (ident? col-spec)
     [(db/to-snake (namespace col-spec)) (db/to-snake (name col-spec))]
     (if (some? col-spec) (table-column (some-keyword col-spec)))))
  ([col-spec col-id]
   [(table col-spec) (column col-id)]))

(def ^{:arglists '([col-spec] [col-spec col-id])}
  table-col
  "Alias for `table-column`. Extracts table and column names from `col-spec` (which may
  be an identifier or a string) as snake-cased strings of a 2-element vector (first
  element being a table name, second a column name). If `col-spec` is an identifier,
  its namespace and name will be used. If two arguments are given, names are
  extracted separately using `table` and `column` functions)."
  table-column)

(defn table-kw
  "Extracts table name as a lisp-cased keyword from `col-spec` which may be an
  identifier or a string. If the identifier has a namespace, it will be used,
  otherwise its name will be used. For strings, it will first transform them into
  keywords (detecting slash character as a separator of a namespace and name) to pick
  a table name. If two arguments are given, the second one is ignored."
  (^Keyword [table-id]
   (if (ident? table-id)
     (some-keyword (db/to-lisp (or (namespace table-id) (name table-id))))
     (if (some? table-id) (table-kw (some-keyword table-id)))))
  (^Keyword [table-id _] (table-kw table-id)))

(defn column-kw
  "Extracts column name as a lisp-cased keyword from `col-spec` which may be an
  identifier or a string. If the identifier has a name, it will be used. For strings,
  it will first transform them into keywords (detecting slash character as a
  separator of a namespace and name) to pick a column name. If two arguments are
  given, the first one is ignored."
  (^Keyword [col-id]
   (if (ident? col-id)
     (some-keyword (db/to-lisp (name col-id)))
     (if (some? col-id) (column-kw (some-keyword col-id)))))
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
  will be used. If two arguments are given, names are extracted separately using
  `table-kw` and `column-kw` functions)."
  ([col-spec]
   (let [k (some-keyword (db/to-lisp col-spec))]
     [(some-keyword (namespace k)) (some-keyword (name k))]))
  ([col-spec col-id]
   [(table-kw col-spec) (column-kw col-id)]))

(def ^{:tag Keyword
       :arglists '(^Keyword [col-id] ^Keyword [_ col-id])}
  table-col-kw
  "Alias for `table-column-kw`. Extracts table and column names from `col-spec` (which
  may be an identifier or a string) as lisp-cased keywords of a 2-element
  vector (first element being a table name, second a column name). If `col-spec` is
  an identifier, its namespace and name will be used. If two arguments are given,
  names are extracted separately using `table-kw` and `column-kw` functions)."
  table-column-kw)

;; SQL query preparation

(defn quoted
  "Quotes the given string according to MySQL / MariaDB quoting rules."
  ^String [s]
  (if-some [^String s (some-str s)]
    (quoted/mysql s)))

(defn- interpolate-tag
  [substitutions [_ quote? ^String modifier ^String tag]]
  (if-some [tag (and tag (get substitutions (keyword tag)))]
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

(defn- interpolate-tags
  [substitutions q]
  (if substitutions
    (str/replace q #"%(%)?([^\}]+)?\{([^\}]+)?\}" #(interpolate-tag substitutions %))
    q))

(def ^{:tag    String
       :no-doc true}
  build-query-core
  "For the given SQL query `q` and substitution map performs pattern interpolation."
  (clojure.core/memoize
   (fn build-query-fn
     (^String []                "")
     (^String [q]                q)
     (^String [q substitutions] (if q (interpolate-tags substitutions q))))))

(def ^{:tag    String
       :no-doc true}
  build-query-dynamic-core
  "For the given SQL query `q` and substitution map performs pattern interpolation."
  (memoize
   (fn build-query-fn
     (^String []                "")
     (^String [q]                q)
     (^String [q substitutions] (if q (interpolate-tags substitutions q))))))

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

  A pattern may have additional modifier before the opening brace. It will be
  resolved as a symbolic function name to be called in order to transform a value
  associated with a tag name. If the name is not fully-qualified (does not contain a
  namespace part) its default namespace will be set to `amelinium.db`.

  Example:

  `(build-query \"select * from %%table{users}\"
                \"where\" :points '> 100
                {:users :users/id})`

  The above call will generate the following result:

  \"select * from `users` where points > 100\".

  This macro can optionally be called with a single literal sequence given as its
  first and only argument. In such cache the sequence should contain all arguments,
  including a substitution map, if applicable.

  This macro should NOT be used to dynamically generate queries having thousands of
  variant substitution parameters as it uses unlimited underlying cache. For such
  purposes please use `build-query-dynamic`, or simply utilize parameters of prepared
  statements."
  {:arglists '([] [q] [q substitution-map] [coll] [& query-parts substitution-map])}
  ([] "")
  ([q]
   (if (sequential? q)
     `(build-query ~@q)
     (#'strspc-squeezed &form &env q)))
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

  A pattern may have additional modifier before the opening brace. It will be
  resolved as a symbolic function name to be called in order to transform a value
  associated with a tag name. If the name is not fully-qualified (does not contain a
  namespace part) its default namespace will be set to `amelinium.db`.

  Example:

  `(build-query-dynamic \"select * from %%table{users}\"
                        \"where\" :points '> 100
                        {:users :users/id})`

  The above call will generate the following result:

  \"select * from `users` where points > 100\".

  This macro can optionally be called with a single literal sequence given as its
  first and only argument. In such cache the sequence should contain all arguments,
  including a substitution map, if applicable.

  This macro should be used to dynamically generate queries having thousands of
  variant substitution parameters."
  {:arglists '([] [q] [q substitution-map] [coll] [& query-parts substitution-map])}
  ([] "")
  ([q]
   (if (sequential? q)
     `(build-query ~@q)
     (#'strspc-squeezed &form &env q)))
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

  Example: `(groups-inverter get-ids db)

  In this example a function will be returned, similar to the below:

  `(fn [m id-type ids] (into m (get-ids db id-type)))`.

  It is used mainly as a transformer in `reduce-kv` when dealing with multiple user
  identifiers grouped by identity type. Having a map of vectors grouped by identity
  type:

  `{:email [#amelinium.Identity {:id-type :email :value \"pw@gnu.org\"}],
    :id    [#amelinium.Identity {:id-type :id :value 1}
            #amelinium.Identity {:id-type :id, :value 42}]}`

  we can call `(reduce-kv (groups-inverter get-ids db) {})` to get:

  `{#amelinium.Identity{:id-type :id, :value 1}                 1
    #amelinium.Identity{:id-type :id, :value 42}                42
    #amelinium.Identity{:id-type :email, :value \"pw@gnu.org\"} 1}`.

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
  {:arglists '([table-column v])}
  identity)

(defmulti out-coercer
  "Returns a coercer suitable for transforming the given argument `v` read from a
  database, assuming table and column specified by the given qualified keyword
  `table-column`."
  {:arglists '([table-column v])}
  identity)

(defmethod in-coercer  :default [_] nil)
(defmethod out-coercer :default [_] nil)

(defn <-
  "Coerces value `v` to a database type by calling a function returned by invoking
  `amelinium.db/in-coercer` multimethod on a qualified keyword `table-column` (or a
  qualified keyword made out of `table` and `column`). If there is no coercer
  attached for the keyword, returns unchanged `v`."
  ([table column v] (if-some [f (in-coercer (make-kw-simple table column))] (f v) v))
  ([table-column v] (if-some [f (in-coercer table-column)] (f v) v)))

(defn ->
  "Coerces value `v` from a database type by calling a function returned by invoking
  `amelinium.db/out-coercer` multimethod on a qualified keyword `table-column` (or a
  qualified keyword made out of `table` and `column`). If there is no coercer
  attached for the keyword, returns unchanged `v`."
  ([table column v] (if-some [f (out-coercer (make-kw-simple table column))] (f v) v))
  ([table-column v] (if-some [f (out-coercer table-column)] (f v) v)))

(defn <-seq
  "Coerces a sequence of values `coll` to database types by calling a function returned
  by invoking `amelinium.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, returns unchanged `coll`."
  ([table column coll] (if-some [f (in-coercer (make-kw-simple table column))] (map f coll) coll))
  ([table-column coll] (if-some [f (in-coercer table-column)] (map f coll) coll)))

(defn seq->
  "Coerces a sequence of values `coll` from database types by calling a function
  returned by invoking `amelinium.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, returns unchanged `coll`."
  ([table column coll] (if-some [f (out-coercer (make-kw-simple table column))] (map f coll) coll))
  ([table-column coll] (if-some [f (out-coercer table-column)] (map f coll) coll)))

(defn- ex-nm
  [v]
  (if (= \! (first (name v))) (subs (name v) 1)))

(defn- pre-parse-conv-specs
  "Used to pre-parse conversion specifications at compile-time."
  [coll]
  (first
   (reduce
    (fn [[done cs table pval?] cur]
      (cond
        (nil?    cur)                             [(conj done cur) nil table true]
        (list?   cur)                             [(conj done cur) nil table true]
        (vector? cur)                             [(conj done cur) nil table true]
        (and (simple-keyword? cur) (ex-nm cur))   [done nil (table-kw (ex-nm cur)) pval?]
        (and pval? (simple-keyword? cur) table)   [done (colspec-kw table cur) table false]
        (and pval? (simple-keyword? cur))         [done nil (col-kw cur) false]
        (and pval? (qualified-keyword? cur))      [done (colspec-kw cur) (table-kw cur) false]
        (and cs (symbol? cur))                    [(conj done [cs cur]) nil table true]
        (and (simple-symbol? cur) (not cs) table) [(conj done [(colspec-kw table cur) cur]) nil table true]
        :else                                     [(apply conj done (remove nil? [(or cs table) cur]))
                                                   nil nil true]))
    [[] nil nil true] coll)))

(defn- spec-convert
  "Used to parse conversion specifications at run-time."
  ^PersistentVector [[^PersistentVector done ^Keyword colspec ^Keyword table ^Boolean pval?] cur]
  (if (vector? cur)
    (let [[^Keyword k v ^Boolean pval?] cur] [(conj done (<- k v)) nil nil true])
    (if (and pval? (keyword? cur))
      (if-some [^String n (namespace cur)]
        [done (colspec-kw n (name cur)) (table-kw n) false]
        (if table
          [done (colspec-kw table cur) table false]
          [done nil (col-kw cur) false]))
      (if colspec
        [(conj done (<- colspec cur)) nil table true]
        [(conj done cur) nil table true]))))

(defmacro <<-
  "Magical macro which converts a sequence of values with optional table and column
  specifications to a database-suitable forms. Pre-processing of arguments is
  executed at compile-time, processing is performed at run-time. Any literal keywords
  given as arguments will be normalized by transforming to lisp case.

  First appearance of a simple keyword when there was no keyword encountered before
  will set the table name for future reference.

  Any fully-qualified keyword placed before a value will make that value converted
  using a `<-` (`in-coercer` multimethod), and also store the table name for future
  reference.

  Any value preceded by a simple keyword when there is a table memorized will be
  converted using `<-` with its first argument set to a keyword (having a namespace
  set to a table name, and its name set to a name of that keyword).

  Any value, other than one expressed with a simple symbolic identifier, preceded by
  a simple keyword, when there is no table memorized yet, will be left as is. After
  that operation a table name will be updated for future reference.

  Any value expressed with a simple symbolic identifier, preceded by a simple
  keyword, when there is no table memorized yet, will be converted using `<-` with
  its first argument set to a keyword (having namespace set to this keyword and name
  set to a name of the symbol expressing some value). After that operation a table
  name will be updated for future reference.

  Any value expressed with a simple symbolic identifier, not preceded by a keyword,
  when there is a table memorized, will be converted using `<-` with its first
  argument set to a keyword (having namespace set to the table name and name set to a
  name of the symbol expressing some value).

  To enforce table name reset when it has already been set, without explicitly
  specifying coercer for the consecutive form, prepend the exclamation mark to a
  simple keyword's name. This only works for literal keywords given as arguments
  since it is detected at compile-time (macro expansion phase).

  If the one and only argument is a map, it will be transformed to a sequence of keys
  interleaved with values. Obviously, the order of pairs in such operation cannot be
  determined.

  Examples:

  `(<<- :users id email)`

  The above will convert values expressed by `id` and `email` symbol forms using a
  variant of coercion multimethod registered for `:users/id` and `:users/email`
  keywords, accordingly.

  `(<<- :users id :email e)`

  The above will convert values of `id` and `e` symbol forms using a variant of
  coercion multimethod registered for `:users/id` and `:users/email` keywords,
  accordingly. Note that by placing `:email` keyword before `e` we are not setting
  new table name but instead we are specifying coercion dispatch value which will be
  a keyword built with `:users` (a table) and `:email` (a column).

  `(<<- :users id :confirmations/email email expires)`

  The above will convert values of `id`, `email` and `expires` symbol forms using a
  variant of coercion multimethod registered for `:users/id`, `:confirmations/email`,
  and `:confirmations/expires` keywords, accordingly. Note that by placing
  `:confirmations/email` keyword before `email` we are setting a table name to
  `:confirmations` and also hinting the engine to apply
  `:confirmations/email`-dispatched transformation to a value of `email`. Also, note
  that coercion of an `expiration` value will rely on memorized table name
  `:confirmations` and its symbolic name.

  `(<<- :users id :!confirmations email expires)`.

   The above has the same effect as the previous example but uses table resetting
  feature without explicitly specifying the coercer for a value behind the `email`
  symbol."
  [spec & more]
  (if more
    (let [specs# (cons spec more)]
      `(nth (reduce ~#'spec-convert [[] nil nil true] ~(#'pre-parse-conv-specs specs#)) 0))
    (let [spec# (if (list? spec) (cons spec nil) spec)
          spec# (if (map? spec#) (mapcat identity spec#) spec#)]
      `(nth (reduce ~#'spec-convert [[] nil nil true]  ~(#'pre-parse-conv-specs spec#)) 0))))

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

  Example:

  `(defcoercions :users :some-identifier str keyword)`

  The above will expand the following code:

  - `(defmethod amelinium.db/in-coercer  :users/some-identifier [_] str)`
  - `(defmethod amelinium.db/in-coercer  :users/some_identifier [_] str)`
  - `(defmethod amelinium.db/out-coercer :users/some-identifier [_] keyword)`,
  - `(defmethod amelinium.db/out-coercer :users/some_identifier [_] keyword)`.

  This will allow specialized database coercion functions to transformed values which
  are exchanged with a database."
  [table & specs]
  (let [t# (some-str table)]
    `(do
       ~@(mapcat (fn [[c# in# out#]]
                   `((defmethod  in-coercer ~(make-kw-lisp  t# c#) [~'_] ~in#)
                     (defmethod  in-coercer ~(make-kw-snake t# c#) [~'_] ~in#)
                     (defmethod out-coercer ~(make-kw-lisp  t# c#) [~'_] ~out#)
                     (defmethod out-coercer ~(make-kw-snake t# c#) [~'_] ~out#)))
                 (partition 3 specs))
       nil)))

(defn- delayed-column-by-index-fn
  "Adds coercion to a database result set `rs` handled by builder `builder` with result
  index `i`. For each result it reads its table name and column label (or name, if
  label is not set), and calls output coercer obtained using
  `amelinium.db/out-coercer`. Each result is wrapped in a Delay object unless it does
  not require coercion."
  [builder ^ResultSet rs ^Integer i]
  (let [^ResultSetMetaData rsm (.getMetaData rs)
        coercer-fn             (out-coercer (make-kw-simple (.getTableName   rsm i)
                                                            (.getColumnLabel rsm i)))
        v                      (.getObject rs i)]
    (rs/read-column-by-index (if coercer-fn (delay (coercer-fn v)) v) rsm i)))

(defn- column-by-index-fn
  "Adds coercion to a database result set `rs` handled by builder `builder` with result
  index `i`. For each result it reads its table name and column label (or name, if
  label is not set), and calls output coercer using `amelinium.db/->` passing to it
  the mentioned names and a value."
  [builder ^ResultSet rs ^Integer i]
  (let [^ResultSetMetaData rsm (.getMetaData rs)]
    (rs/read-column-by-index (-> (.getTableName   rsm i)
                                 (.getColumnLabel rsm i)
                                 (.getObject      rs  i))
                             rsm
                             i)))

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
  elements)."
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
  but safer FIFO cache of default size (about 150k items)."
  [query & params]
  `(cons (build-query-dynamic ~query) (<<- ~@params)))

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
