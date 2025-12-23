(ns

    ^{:doc    "I18N support for amelinium"
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.i18n

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string          :as                 str]
            [tongue.core             :as              tongue]
            [io.randomseed.utils     :refer             :all]
            [io.randomseed.utils.var :as                 var]
            [io.randomseed.utils.map :as                 map]
            [amelinium.app           :as                 app]
            [amelinium.system        :as              system]
            [amelinium.logging       :as                 log]
            [amelinium.http          :as                http]
            [amelinium.db.sql        :refer [idname make-kw
                                             idname-simple
                                             make-kw-simple]]))

(defonce ^:redef translations nil)

(def ^:dynamic *handle-missing-keys* true)

;; Accessors

(defn lang
  "Tries to obtain a language from a request map (`:language/id` key). Falls back to a
  default language (`:language/default`) if the first one is `nil`. Returns a keyword."
  ^clojure.lang.Keyword [req]
  (or (get req :language/id)
      (get req :language/default)))

(defn translation-fn
  "Tries to obtain translation function from a route data in a request map or a `Match`
  object and if that fails from a request map itself. Falls back to a global variable
  `amelinium.i18n/translations`.

  If `*handle-missing-keys*` dynamic variable is set to `false` or `nil` then the
  generated translation function will always generate `nil` when a key is missing,
  ignoring dynamic binding."
  [req-or-match]
  (let [f (or (http/get-route-data req-or-match :translations)
              (if-not (http/match? req-or-match) (get req-or-match :translations))
              translations)]
    (if *handle-missing-keys*
      f
      (fn translate-without-default
        ([key]              (binding [*handle-missing-keys* false] (f key)))
        ([key x]            (binding [*handle-missing-keys* false] (f key x)))
        ([key x y]          (binding [*handle-missing-keys* false] (f key x y)))
        ([key x y z]        (binding [*handle-missing-keys* false] (f key x y z)))
        ([key x y z & more] (binding [*handle-missing-keys* false] (apply f key x y z more)))))))

;; Builders

(defn translator
  "Tries to obtain translation function from a route data in a request map or a `Match`
  object and if that fails from a request map itself. Falls back to a global variable
  `amelinium.i18n/translations`.

  When `locale` is given it will generate a translation function with predefined
  translator and locale. If it's not given, it will use language obtained from the
  context map `req`.

  If `*handle-missing-keys*` dynamic variable is set to `false` or `nil` then the
  generated translation function will always generate `nil` when a key is missing,
  ignoring dynamic binding."
  ([req-or-match]
   (translator req-or-match nil))
  ([req-or-match locale]
   (let [tr-fn (translation-fn req-or-match)
         tr-l  (make-kw-simple (or locale (lang req-or-match)))]
     (fn
       ([key]            (tr-fn tr-l key))
       ([key x]          (tr-fn tr-l key x))
       ([key x y]        (tr-fn tr-l key x y))
       ([key x y & more] (apply tr-fn tr-l key x y more))))))

(defn translator-sub
  "Tries to obtain translation function from a route data in a request map or a `Match`
  object and if that fails from a request map itself. Falls back to a global variable
  `amelinium.i18n/translations`. The translation function will accept `key-ns` and
  `key-name` arguments which will be used to build a keyword with the given namespace
  and name. This keyword will be used as a translation key.

  When `locale` is given it will generate a translation function with predefined
  translator and locale. If it's not given, it will use language obtained from the
  context map `req`.

  If `*handle-missing-keys*` dynamic variable is set to `false` or `nil` then the
  generated translation function will always generate `nil` when a key is missing,
  ignoring dynamic binding."
  ([req-or-match]
   (translator-sub req-or-match nil))
  ([req-or-match locale]
   (let [tr-fn (translation-fn req-or-match)
         tr-l  (make-kw-simple (or locale (lang req-or-match)))]
     (fn
       ([key]                        (tr-fn tr-l key))
       ([key-ns key-name]            (tr-fn tr-l (make-kw key-ns key-name)))
       ([key-ns key-name x]          (tr-fn tr-l (make-kw key-ns key-name) x))
       ([key-ns key-name x y]        (tr-fn tr-l (make-kw key-ns key-name) x y))
       ([key-ns key-name x y & more] (apply tr-fn tr-l (make-kw key-ns key-name) x y more))))))

;; Translators

(defn translate-with
  "Returns a translation string for the given `locale` (language ID) and the keyword
  `key` using a translation function `tf`. Any optional arguments are passed as they
  are.

  If `*handle-missing-keys*` dynamic variable is set to `false` or `nil` (which can
  be set using `no-default` macro) then the function will return `nil` when a key is
  missing instead of a warning string.

  If a translation function `tr` was generated (using `translation-fn`, `translator`
  or `translator-sub`) with `*handle-missing-keys*` dynamic variable set to `false`
  or `nil` then it will always return `nil` when a key is missing, regardless of
  current value of `*handle-missing-keys*` in the calling context."
  (^String [tf locale key]            (tf (make-kw-simple locale) key))
  (^String [tf locale key x]          (tf (make-kw-simple locale) key x))
  (^String [tf locale key x y]        (tf (make-kw-simple locale) key x y))
  (^String [tf locale key x y & more] (apply tf (make-kw-simple locale) key x y more)))

(defn translate-sub-with
  "Returns a translation string for the given `locale` (language ID), the namespace
  name `ns-name` and the key name `key-name`, using the given translation function
  `tf`. Useful to translate nested keys which are translated to fully-qualified
  keywords. Any additional arguments are passed as they are.

  If `*handle-missing-keys*` dynamic variable is set to `false` or `nil` (which can
  be set using `no-default` macro) then the function will return `nil` when a key is
  missing instead of a warning string.

  If a translation function `tr` was generated (using `translation-fn`, `translator`
  or `translator-sub`) with `*handle-missing-keys*` dynamic variable set to `false`
  or `nil` then it will always return `nil` when a key is missing, regardless of
  current value of `*handle-missing-keys*` in the calling context."
  (^String [tf locale key-ns key-name]            (tf (make-kw-simple locale) (make-kw key-ns key-name)))
  (^String [tf locale key-ns key-name x]          (tf (make-kw-simple locale) (make-kw key-ns key-name) x))
  (^String [tf locale key-ns key-name x y]        (tf (make-kw-simple locale) (make-kw key-ns key-name) x y))
  (^String [tf locale key-ns key-name x y & more] (apply tf (make-kw-simple locale) (make-kw key-ns key-name) x y more)))

(defn translate
  "Returns a translation string for the given `locale` (language ID) and the keyword
  `key` using a translation function obtained from the given request map (`req`) by
  calling `translator` function on it. Any optional arguments are passed as they
  are.

  If `*handle-missing-keys*` dynamic variable is set to `false` or `nil` (which can
  be set using `no-default` macro) then the function will return `nil` when a key is
  missing instead of a warning string.

  If a translation function `tr` was generated (using `translation-fn`, `translator`
  or `translator-sub`) with `*handle-missing-keys*` dynamic variable set to `false`
  or `nil` then it will always return `nil` when a key is missing, regardless of
  current value of `*handle-missing-keys*` in the calling context."
  (^String [req locale key]            ((translator req (make-kw-simple locale)) key))
  (^String [req locale key x]          ((translator req (make-kw-simple locale)) key x))
  (^String [req locale key x y]        ((translator req (make-kw-simple locale)) key x y))
  (^String [req locale key x y & more] (apply (translator req (make-kw-simple locale)) key x y more)))

(defn translate-sub
  "Returns a translation string for the given `locale` (language ID), the namespace
  name `ns-name` and the key name `key-name`. Useful to translate nested keys which
  are translated to fully-qualified keywords. The translation function will be
  obtained by calling `translator` on `req` (which may be a request map or a `Match`
  object). Any additional arguments are passed as they are.

  If `*handle-missing-keys*` dynamic variable is set to `false` or `nil` (which can
  be set using `no-default` macro) then the function will return `nil` when a key is
  missing instead of a warning string.

  If a translation function `tr` was generated (using `translation-fn`, `translator`
  or `translator-sub`) with `*handle-missing-keys*` dynamic variable set to `false`
  or `nil` then it will always return `nil` when a key is missing, regardless of
  current value of `*handle-missing-keys*` in the calling context."
  (^String [req locale key-ns key-name]            ((translator req (make-kw-simple locale)) (make-kw key-ns key-name)))
  (^String [req locale key-ns key-name x]          ((translator req (make-kw-simple locale)) (make-kw key-ns key-name) x))
  (^String [req locale key-ns key-name x y]        ((translator req (make-kw-simple locale)) (make-kw key-ns key-name) x y))
  (^String [req locale key-ns key-name x y & more] (apply (translator req (make-kw-simple locale)) (make-kw key-ns key-name) x y more)))

(defn tr
  "Returns a translation string for the given locale (obtained from a request map)
  and the keyword `key` using a translation function (obtained from a
  request map or a `Match` object). Any optional arguments are passed as they are.

  If `*handle-missing-keys*` dynamic variable is set to `false` or `nil` (which can
  be set using `no-default` macro) then the function will return `nil` when a key is
  missing instead of a warning string.

  If a translation function `tr` was generated (using `translation-fn`, `translator`
  or `translator-sub`) with `*handle-missing-keys*` dynamic variable set to `false`
  or `nil` then it will always return `nil` when a key is missing, regardless of
  current value of `*handle-missing-keys*` in the calling context."
  (^String [req key]            ((translator req) key))
  (^String [req key x]          ((translator req) key x))
  (^String [req key x y]        ((translator req) key x y))
  (^String [req key x y & more] (apply (translator req) key x y more)))

(defn tr-sub
  "Returns a translation string for the given locale (obtained from a request map),
  the namespace name `key-ns` and the key name `key-name`. Useful to translate nested
  keys which are translated to fully-qualified keywords. The translation function
  will be obtained by calling `translator` on `req` (which may be a request map or a
  `Match` object). Any additional arguments are passed as they are.

  If `*handle-missing-keys*` dynamic variable is set to `false` or `nil` (which can
  be set using `no-default` macro) then the function will return `nil` when a key is
  missing instead of a warning string.

  If a translation function `tr` was generated (using `translation-fn`, `translator`
  or `translator-sub`) with `*handle-missing-keys*` dynamic variable set to `false`
  or `nil` then it will always return `nil` when a key is missing, regardless of
  current value of `*handle-missing-keys*` in the calling context."
  (^String [req key-ns key-name]            ((translator req) (make-kw key-ns key-name)))
  (^String [req key-ns key-name x]          ((translator req) (make-kw key-ns key-name) x))
  (^String [req key-ns key-name x y]        ((translator req) (make-kw key-ns key-name) x y))
  (^String [req key-ns key-name x y & more] (apply (translator req) (make-kw key-ns key-name) x y more)))

(defmacro no-default
  "Sets `*handle-missing-keys*` dynamic variable to `false`, causing translation
  functions to return `nil` when translation key is not found instead of a warning
  string. Also, when used with `translation-fn`, `translator` or `translator-sub`
  causes generated function to always behave that way."
  [& body]
  `(binding [*handle-missing-keys* false]
     ~@body))

;; Initialization

(defn missing-key
  "Returns a warning string for a missing key (defined under a special key
  `:amelinium/missing-key`) if the dynamic variable `*handle-missing-keys*` is set to
  a truthy value. Otherwise it returns `nil`."
  [f locale k]
  (if *handle-missing-keys* (f locale :amelinium/missing-key k)))

(defn wrap-translate
  [f]
  (fn translate
    (^String [locale k]
     (if (keyword? k)
       (or (f locale k) (missing-key f locale k))
       (if-some [k (keyword k)] (or (f locale k) (missing-key f locale k)))))
    (^String [locale k a]
     (if (keyword? k)
       (or (f locale k a) (missing-key f locale k))
       (if-some [k (keyword k)] (or (f locale k a) (missing-key f locale k)))))
    (^String [locale k a b]
     (if (keyword? k)
       (or (f locale k a b) (missing-key f locale k))
       (if-some [k (keyword k)] (or (f locale k a b) (missing-key f locale k)))))
    (^String [locale k a b & more]
     (if (keyword? k)
       (or (apply f locale k a b more) (missing-key f locale k))
       (if-some [k (keyword k)] (or (apply f locale k a b more) (missing-key f locale k)))))))

(defn prep-pluralizer
  [config lang translations]
  (if-some [pluralizer-fn (some-> config (get lang) (get :tongue/pluralizer) var/deref-symbol)]
    (let [[a b c d e & more] translations]
      (if (map? a)
        (prep-pluralizer config lang (pluralizer-fn :parse-args a))
        (case (count translations)
          0 (fn pluralize [n] (pluralizer-fn n))
          1 (fn pluralize [n] (pluralizer-fn n a))
          2 (fn pluralize [n] (pluralizer-fn n a b))
          3 (fn pluralize [n] (pluralizer-fn n a b c))
          4 (fn pluralize [n] (pluralizer-fn n a b c d))
          5 (fn pluralize [n] (pluralizer-fn n a b c d e))
          (fn pluralize [n] (apply pluralizer-fn n a b c d e more)))))))

(defn- zero-missing-keys
  [config]
  (reduce-kv (fn [m lang translations]
               (if (and (keyword? lang) (map? translations))
                 (update m lang assoc :tongue/missing-key nil)
                 m))
             config config))

(defn- handle-val
  [config v kpath]
  (if (and (sequential? v) (identical? :pluralize (first v)))
    (prep-pluralizer config (first kpath) (rest v))
    (var/deref-symbol v)))

(defn prep-translations
  [config]
  (->> config
       (map/map-values-with-path (partial handle-val config))
       zero-missing-keys))

(defn expand-translations
  [k config]
  {k (prep-translations config)})

(defn init-translations
  [config]
  (-> config
      prep-translations
      tongue/build-translate
      wrap-translate))

(system/add-expand ::translations [k config] (expand-translations k config))
(system/add-init   ::translations [k config] (var/make k (init-translations config)))
(system/add-halt!  ::translations [k config] (var/make k nil))
