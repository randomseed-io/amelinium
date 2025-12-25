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
  "Returns the current language keyword for `req`.

  Looks up `:language/id` first and falls back to `:language/default` when missing.
  Always returns a keyword (or nil if both keys are absent)."
  ^clojure.lang.Keyword [req]
  (or (get req :language/id)
      (get req :language/default)))

(defn translation-fn
  "Returns a Tongue translation function for `req-or-match`.

  Lookup order:
  1) route data `:translations` (via `amelinium.http/get-route-data`),
  2) `:translations` in the request map (when `req-or-match` is not a Match),
  3) the global var `amelinium.i18n/translations`.

  When `amelinium.i18n/*handle-missing-keys*` is false/nil, the returned function is
  wrapped so missing keys yield nil (even if the caller later re-binds
  `amelinium.i18n/*handle-missing-keys*`)."
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
  "Builds a locale-bound translator function for `req-or-match` (request map or Match).

  The translator is derived via `translation-fn` and then partially applied with a
  locale keyword:

  - when `locale` is provided, it is used;
  - otherwise the locale is taken from `(lang req-or-match)`.

  May fall back to a default `amelinium.i18n/translations` if translator function
  cannot be found.

  Returned function signatures mirror Tongue:
  `([key] ...) ([key x] ...) ([key x y] ...) ([key x y & more] ...)`.

  When `amelinium.i18n/*handle-missing-keys*` is false or nil, missing keys yield
  nil (see `translation-fn`)."
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
  "Builds a locale-bound translator function for `req-or-match` that supports
  namespaced translation keys built with `(make-kw key-ns key-name)`.

  Arity options include:
  - `([key] ...)` for direct keyword keys,
  - `([key-ns key-name] ...)` and higher arities for building fully-qualified keywords.

  Locale resolution and missing-key behavior match `translator` / `translation-fn`."
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
  "Translates `key` using an explicit Tongue translation function `tf` and `locale`.

  The `locale` is normalized with `make-kw-simple`. Additional arguments are
  forwarded to `tf`."
  (^String [tf locale key]            (tf (make-kw-simple locale) key))
  (^String [tf locale key x]          (tf (make-kw-simple locale) key x))
  (^String [tf locale key x y]        (tf (make-kw-simple locale) key x y))
  (^String [tf locale key x y & more] (apply tf (make-kw-simple locale) key x y more)))

(defn translate-sub-with
  "Translates a key built from `key-ns` and `key-name` using an explicit Tongue
  translation function `tf` and `locale`.

  `locale` is normalized with `make-kw-simple`, and the translation key is built with
  `(make-kw key-ns key-name)`. Additional arguments are forwarded to `tf`."
  (^String [tf locale key-ns key-name]            (tf (make-kw-simple locale) (make-kw key-ns key-name)))
  (^String [tf locale key-ns key-name x]          (tf (make-kw-simple locale) (make-kw key-ns key-name) x))
  (^String [tf locale key-ns key-name x y]        (tf (make-kw-simple locale) (make-kw key-ns key-name) x y))
  (^String [tf locale key-ns key-name x y & more] (apply tf (make-kw-simple locale) (make-kw key-ns key-name) x y more)))

(defn translate
  "Translates `key` for an explicit `locale` using a translator derived from `req`
  (request map or Match).

  Uses `(translator req locale)` and forwards any additional arguments to the
  underlying Tongue function."
  (^String [req locale key]            ((translator req (make-kw-simple locale)) key))
  (^String [req locale key x]          ((translator req (make-kw-simple locale)) key x))
  (^String [req locale key x y]        ((translator req (make-kw-simple locale)) key x y))
  (^String [req locale key x y & more] (apply (translator req (make-kw-simple locale)) key x y more)))

(defn translate-sub
  "Translates a key built from `key-ns` and `key-name` for an explicit `locale` using a
  translator derived from `req` (request map or Match).

  Uses `(make-kw key-ns key-name)` to build the translation key and forwards any
  additional arguments."
  (^String [req locale key-ns key-name]            ((translator req (make-kw-simple locale)) (make-kw key-ns key-name)))
  (^String [req locale key-ns key-name x]          ((translator req (make-kw-simple locale)) (make-kw key-ns key-name) x))
  (^String [req locale key-ns key-name x y]        ((translator req (make-kw-simple locale)) (make-kw key-ns key-name) x y))
  (^String [req locale key-ns key-name x y & more] (apply (translator req (make-kw-simple locale)) (make-kw key-ns key-name) x y more)))

(defn tr
  "Translates `key` using locale inferred from `req` (via `lang`) and a translator
  derived from `req` (request map or Match).

  Additional arguments are forwarded to the underlying Tongue function."
  (^String [req key]            ((translator req) key))
  (^String [req key x]          ((translator req) key x))
  (^String [req key x y]        ((translator req) key x y))
  (^String [req key x y & more] (apply (translator req) key x y more)))

(defn tr-sub
  "Translates a key built from `key-ns` and `key-name` using locale inferred from `req`
  (via `lang`) and a translator derived from `req` (request map or Match).

  Additional arguments are forwarded to the underlying Tongue function."
  (^String [req key-ns key-name]            ((translator req) (make-kw key-ns key-name)))
  (^String [req key-ns key-name x]          ((translator req) (make-kw key-ns key-name) x))
  (^String [req key-ns key-name x y]        ((translator req) (make-kw key-ns key-name) x y))
  (^String [req key-ns key-name x y & more] (apply (translator req) (make-kw key-ns key-name) x y more)))

(defmacro no-default
  "Executes `body` with `amelinium.i18n/*handle-missing-keys*` bound to false.

  This makes translation return nil for missing keys instead of producing a warning
  string. When used while creating translators (`translation-fn` / `translator` /
  `translator-sub`), it also forces the generated function to always behave that
  way."
  [& body]
  `(binding [*handle-missing-keys* false]
     ~@body))

;; Initialization

(defn missing-key
  "Returns the missing-key warning string for translation key `k` in `locale`.

  When `amelinium.i18n/*handle-missing-keys*` is truthy, delegates to `f` using the
  special key `:amelinium/missing-key` and passes `k` as an argument (so the template
  can include it).  When `*handle-missing-keys*` is false/nil, returns nil."
  [f locale k]
  (if *handle-missing-keys* (f locale :amelinium/missing-key k)))

(defn wrap-translate
  "Wraps a Tongue translate function `f` to add two behaviors:

  1) Missing keys:
   - when `amelinium.i18n/*handle-missing-keys*` is truthy, falls back to
     `(missing-key f locale k)`,
   - otherwise returns nil.

  2) Key normalization:
   - accepts keyword keys directly,
   - for non-keyword keys, tries to coerce via `keyword` before translating.

  Preserves Tongue-like arities:
  `([locale k] ...) ([locale k a] ...) ([locale k a b] ...) ([locale k a b & more] ...)`."
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
  "Builds a pluralization function for `lang` based on `config` and `translations`.

  Looks up `:tongue/pluralizer` under the language entry, resolves it via
  `io.randomseed.utils.var/deref-symbol`, and returns a function of one argument `n`
  that applies the pluralizer to `translations`.

  Supports a special leading map in `translations` (passed to pluralizer as
  `(pluralizer-fn :parse-args m)`), allowing pluralizers to pre-parse arguments.

  For performance, returns fixed-arity closures for small argument counts and falls
  back to `apply` for larger counts."
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
          (fn pluralize   [n] (apply pluralizer-fn n a b c d e more)))))))

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
  "Prepares an I18N config map for Tongue.

  Transforms the input `config` by:

  - dereferencing symbolic values via `io.randomseed.utils.var/deref-symbol`,

  - expanding pluralization forms of the shape `[:pluralize ...]`
    (via `amelinium.i18n/prep-pluralizer`),

  - forcing `:tongue/missing-key` to nil per-language (so missing-key handling is
    fully controlled by `wrap-translate` / `*handle-missing-keys*`).

  Returns the prepared config map suitable for `tongue.core/build-translate`."
  [config]
  (->> config
       (map/map-values-with-path (partial handle-val config))
       zero-missing-keys))

(defn expand-translations
  "Integrant `expand-key` handler for `::translations`.
  Returns `{k prepared-config}` where `prepared-config` is produced by
  `amelinium.i18n/prep-translations`."
  [k config]
  {k (prep-translations config)})

(defn init-translations
  "Initializes Tongue translations from `config`.

  Pipeline:
  1) `amelinium.i18n/prep-translations`
  2) `tongue.core/build-translate`
  3) `amelinium.i18n/wrap-translate`

  Returns the final translation function."
  [config]
  (-> config
      prep-translations
      tongue/build-translate
      wrap-translate))

(system/add-expand ::translations [k config] (expand-translations k config))
(system/add-init   ::translations [k config] (var/make k (init-translations config)))
(system/add-halt!  ::translations [k config] (var/make k nil))
