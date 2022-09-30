(ns

    ^{:doc    "amelinium service, error handling."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.errors

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string           :as           str]
            [tick.core                :as             t]
            [clj-uuid                 :as          uuid]
            [amelinium.system         :as        system]
            [amelinium.http           :as          http]
            [io.randomseed.utils.time :as          time]
            [io.randomseed.utils.map  :as           map]
            [io.randomseed.utils.var  :as           var]
            [io.randomseed.utils      :refer       :all]))

(defrecord ErrorsConfig [priorities responses default-response])

(defprotocol ErrorsConfigurable
  (config [src]))

(extend-protocol ErrorsConfigurable

  ErrorsConfig
  (config [src] src)

  clojure.lang.IPersistentMap
  (config
    [src]
    (http/get-route-data src :errors/config)))

(defn config?
  [v]
  (instance? ErrorsConfig v))

(defn configurable?
  [v]
  (satisfies? ErrorsConfigurable v))

(defn most-significant
  "Returns a most significant error from the given `errors` using the given
  `config` (which may be of type `ErrorsConfig`, a request map, or a `Match`
  object). If `errors` is not a sequence but a keyword, it is returned as-is. Returns
  `nil` when `config-src` or `errors` is `nil`."
  [config-src errors]
  (if errors
    (if (keyword? errors)
      errors
      (if-some [config (config config-src)]
        (or (some errors (.priorities ^ErrorsConfig config))
            (first errors))))))

(defn render-fn
  "Gets a response rendering function using the given configuration source `config-src`
  (which may be of type `ErrorsConfig`, a request map, or a `Match` object) and
  `error` (expressed as a keyword). Returns `nil` when `config-src` or `error` is
  `nil`."
  ([config-src error]
   (render-fn config-src error nil))
  ([config-src error default]
   (if error
     (if-some [config (config config-src)]
       (or (get (.responses ^ErrorsConfig config) (most-significant config error))
           default)))))

(defn default-response
  "Returns the default error response rendering function. Returns `nil` when
  `config-src` is `nil`."
  [config-src]
  (if-some [config (config config-src)]
    (.default-response ^ErrorsConfig config)))

(defn render
  "Renders an error response using `render-fn`. If the response rendering function
  cannot be established, configuration default is used. Returns `nil` when
  `config-src` is `nil`."
  ([config-src]
   (render config-src nil nil))
  ([config-src error]
   (render config-src error nil))
  ([config-src error default]
   (if-some [config (config config-src)]
     (if-some [render-fn (or (render-fn config error default)
                             (.default-response ^ErrorsConfig config))]
       (render-fn))))
  ([config-src error default req]
   (if-some [config (config config-src)]
     (if-some [render-fn (or (render-fn config error default)
                             (.default-response ^ErrorsConfig config))]
       (render-fn req))))
  ([config-src error default req & more]
   (if-some [config (config config-src)]
     (if-some [render-fn (or (render-fn config error default)
                             (.default-response ^ErrorsConfig config))]
       (apply render-fn req more)))))

(defn specific-id
  "Makes errors `errors` more specific by replacing generic bad ID error (as a keyword)
  with a bad e-mail or phone error."
  ([errors id src-id email-id phone-id]
   (if errors
     (if (contains? errors src-id)
       (if-some [id (some-str id)]
         (if-some [dst-id (cond (str/index-of id \@ 1) email-id
                                (= (first id) \+)      phone-id)]
           (conj (disj errors src-id) dst-id)
           errors)
         errors)
       errors)))
  ([errors src-id dst-id]
   (if errors
     (if (contains? errors src-id)
       (conj (disj errors src-id) dst-id)
       errors))))

;; Initializers

(defn init-errors
  [config]
  (map->ErrorsConfig (map/map-values var/deref-symbol config)))

(system/add-init  ::settings [_ config] (init-errors config))
(system/add-halt! ::settings [_ config] nil)

(derive ::web        ::settings)
(derive ::api        ::settings)
(derive ::all        ::settings)
(derive ::priorities ::system/value)
(derive ::responses  ::system/value)