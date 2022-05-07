(ns

    ^{:doc    "I18N support for amelinium"
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.i18n

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [tongue.core             :as    tongue]
            [io.randomseed.utils     :refer   :all]
            [io.randomseed.utils.var :as       var]
            [io.randomseed.utils.map :as       map]
            [amelinium.app           :as       app]
            [amelinium.system        :as    system]
            [amelinium.logging       :as       log]
            [amelinium.http          :as      http]
            [amelinium.locale        :as         l]))

(defonce translations nil)

(defn translator
  "Tries to obtain translation function from a route data in a request map or a `Match`
  object and if that fails from a request map itself. Falls back to a global variable
  `amelinium.i18n/translations`."
  [req-or-match]
  (or (http/get-route-data req-or-match ::translations)
      (when-not (http/match? req-or-match)
        (get req-or-match ::translations))
      translations))

(defn lang
  "Tries to obtain a language from a request map (`:language/id` key) or returns the
  value of the given argument if it is not a map."
  [locale-or-req]
  (if (map? locale-or-req)
    (get locale-or-req :language/id)
    locale-or-req))

(defn idname
  "If the given value `v` is an ident, it returns its name. Otherwise it returns the
  string representation of the given object."
  [v]
  (if (ident? v) (name v) (str v)))

(defn translate-with
  "Returns a translation string for the given `locale` (language ID) and the keyword
  `key` using a translation function `tf`. Any optional arguments are passed as they
  are."
  ([tf locale key]
   (tf locale key))
  ([tf locale key x]
   (tf locale key x))
  ([tf locale key x & more]
   (apply tf locale key x more)))

(defn translate-sub-with
  "Returns a translation string for the given `locale` (language ID), the namespace
  name `ns-name` and the key name `key-name`, using the given translation function
  `tf`. Useful to translate nested keys which are translated to fully-qualified
  keywords. Any additional arguments are passed as they are."
  ([tf locale key-ns key-name]
   (tf locale (keyword (idname key-ns) (idname key-name))))
  ([tf locale key-ns key-name x]
   (tf locale (keyword (idname key-ns) (idname key-name)) x))
  ([tf locale key-ns key-name x & more]
   (apply tf locale (keyword (idname key-ns) (idname key-name)) x more)))

(defn translate
  "Returns a translation string for the given `locale` (language ID) and the keyword
  `key` using a translation function obtained from the given request map (`req`) by
  calling `translator` function on it. Any optional arguments are passed as they
  are."
  ([req locale key]
   ((translator req) locale key))
  ([req locale key x]
   ((translator req) locale key x))
  ([req locale key x & more]
   (apply (translator req) locale key x more)))

(defn translate-sub
  "Returns a translation string for the given `locale` (language ID), the namespace
  name `ns-name` and the key name `key-name`. Useful to translate nested keys which
  are translated to fully-qualified keywords. The translation function will be
  obtained by calling `translator` on `req` (which may be a request map or a `Match`
  object). Any additional arguments are passed as they are."
  ([req locale key-ns key-name]
   ((translator req) locale (keyword (idname key-ns) (idname key-name))))
  ([req locale key-ns key-name x]
   ((translator req) locale (keyword (idname key-ns) (idname key-name)) x))
  ([req locale key-ns key-name x & more]
   (apply (translator req) locale (keyword (idname key-ns) (idname key-name)) x more)))

(defn tr
  "Returns a translation string for the given locale (obtained from a request map or a
  `Match` object) and the keyword `key` using a translation function (obtained from a
  request map or a `Match` object). Any optional arguments are passed as they are."
  ([req key]
   ((translator req) (lang req) key))
  ([req key x]
   ((translator req) (lang req) key x))
  ([req key x & more]
   (apply (translator req) (lang req) key x more)))

(defn tr-sub
  "Returns a translation string for the given `locale` (language ID), the namespace name
  `ns-name` and the key name `key-name`. Useful to translate nested keys which are
  translated to fully-qualified keywords. The translation function will be obtained
  by calling `translator` on `req` (which may be a request map or a `Match`
  object). Any additional arguments are passed as they are."
  ([req key-ns key-name]
   ((translator req) (lang req) (keyword (idname key-ns) (idname key-name))))
  ([req key-ns key-name x]
   ((translator req) (lang req) (keyword (idname key-ns) (idname key-name)) x))
  ([req key-ns key-name x & more]
   (apply (translator req) (lang req) (keyword (idname key-ns) (idname key-name)) x more)))

(defn prep-translations
  [config]
  (map/map-values var/deref-symbol config))

(defn init-translations
  [config]
  (-> config
      prep-translations
      tongue/build-translate))

(system/add-prep  ::translations [_ config] (prep-translations config))
(system/add-init  ::translations [k config] (var/make k (init-translations config)))
(system/add-halt! ::translations [k config] (var/make k nil))
