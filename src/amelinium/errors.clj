(ns

    ^{:doc    "amelinium service, error handling."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.errors

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string           :as           str]
            [tick.core                :as             t]
            [clj-uuid                 :as          uuid]
            [amelinium.system         :as        system]
            [amelinium.http           :as          http]
            [amelinium.proto.errors   :as             p]
            [io.randomseed.utils.time :as          time]
            [io.randomseed.utils.map  :as           map]
            [io.randomseed.utils.var  :as           var]
            [io.randomseed.utils      :refer       :all]
            [amelinium                :refer       :all]
            [amelinium.types.errors   :refer       :all])

  (:import [clojure.lang           Keyword IFn IPersistentMap]
           [amelinium              ErrorsConfig]
           [amelinium.proto.errors ErrorsConfigurable]))

(extend-protocol p/ErrorsConfigurable

  ErrorsConfig

  (config ^ErrorsConfig [src] src)
  (configurable? [src] true)

  clojure.lang.IPersistentMap

  (config ^ErrorsConfig [src] (http/get-route-data src :errors/config))
  (configurable? [src] (p/configurable? (http/get-route-data src :errors/config)))

  nil

  (config [src] nil)
  (configurable? [src] false)

  Object

  (configurable? [src] false))

(defn config?
  "Returns `true` if the given object is an instance of `ErrorsConfig`."
  ^Boolean [v]
  (instance? ErrorsConfig v))

(defn configurable?
  ^Boolean [v]
  "Returns `true` if the given object implements `ErrorsConfigurable` protocol and can
  be used to actually access the protocol methods (in case of indirect object)."
  (p/configurable? v))

(defn most-significant
  "Returns the most significant error from the given `errors` using the given
  `config` (which may be of type `ErrorsConfig`, a request map, or a `Match`
  object). If `errors` is not a sequence but an identifier, it is returned as-is.
  Returns `nil` when `config-src` is `nil` or `errors` is `nil`."
  ^Keyword [^ErrorsConfigurable config-src errors]
  (if errors
    (if (ident? errors)
      errors
      (if-some [^ErrorsConfig config (p/config config-src)]
        (or (some errors (.priorities config))
            (first errors))))))

(defn render-fn
  "Gets a response rendering function using the given configuration source `config-src`
  (which may be of type `ErrorsConfig`, a request map, or a `Match` object) and
  `errors` (expressed as a keyword or a sequence of keywords). Returns `nil` when
  `config-src` or `error` is `nil`."
  (^IFn [^ErrorsConfigurable config-src errors]
   (render-fn config-src errors nil))
  (^IFn [^ErrorsConfigurable config-src errors ^IFn default]
   (if errors
     (if-some [^ErrorsConfig config (p/config config-src)]
       (or (get (.responses config) (most-significant config errors))
           default)))))

(defn default-response
  "Returns the default error response rendering function. Returns `nil` when
  `config-src` is `nil`."
  ^IFn [^ErrorsConfigurable config-src]
  (if-some [^ErrorsConfig config (p/config config-src)]
    (.default-response config)))

(defn render
  "Renders an error or status response using `render-fn`. If the response rendering
  function cannot be established, configuration default is used. Returns `nil` when
  `config-src` is `nil`. Any additional arguments, including (and starting from)
  `req` are passed to the rendering function call."
  ([^ErrorsConfigurable config-src]
   (render config-src nil nil))
  ([^ErrorsConfigurable config-src error]
   (render config-src error nil))
  ([^ErrorsConfigurable config-src error ^IFn default]
   (if-some [^ErrorsConfig config (p/config config-src)]
     (if-some [^IFn render-fn (or (render-fn config error default)
                                  (.default-response config))]
       (render-fn))))
  ([^ErrorsConfigurable config-src error ^IFn default req]
   (if-some [^ErrorsConfig config (p/config config-src)]
     (if-some [^IFn render-fn (or (render-fn config error default)
                                  (.default-response config))]
       (render-fn req))))
  ([^ErrorsConfigurable config-src error ^IFn default req a]
   (if-some [^ErrorsConfig config (p/config config-src)]
     (if-some [^IFn render-fn (or (render-fn config error default)
                                  (.default-response config))]
       (render-fn req a))))
  ([^ErrorsConfigurable config-src error ^IFn default req a b]
   (if-some [^ErrorsConfig config (p/config config-src)]
     (if-some [^IFn render-fn (or (render-fn config error default)
                                  (.default-response config))]
       (render-fn req a b))))
  ([^ErrorsConfigurable config-src error ^IFn default req a b c]
   (if-some [^ErrorsConfig config (p/config config-src)]
     (if-some [^IFn render-fn (or (render-fn config error default)
                                  (.default-response config))]
       (render-fn req a b c))))
  ([^ErrorsConfigurable config-src error ^IFn default req a b c d]
   (if-some [^ErrorsConfig config (p/config config-src)]
     (if-some [^IFn render-fn (or (render-fn config error default)
                                  (.default-response config))]
       (render-fn req a b c d))))
  ([^ErrorsConfigurable config-src error ^IFn default req a b c d e]
   (if-some [^ErrorsConfig config (p/config config-src)]
     (if-some [^IFn render-fn (or (render-fn config error default)
                                  (.default-response config))]
       (render-fn req a b c d e))))
  ([^ErrorsConfigurable config-src error ^IFn default req a b c d e & more]
   (if-some [^ErrorsConfig config (p/config config-src)]
     (if-some [^IFn render-fn (or (render-fn config error default)
                                  (.default-response config))]
       (apply render-fn req a b c d e more)))))

(defn config
  "Returns `ErrorsConfig` record extracted from configuration source `config-src`."
  ^ErrorsConfig [^ErrorsConfigurable config-src]
  (p/config config-src))

(defn specific-id
  "Makes errors `errors` more specific by replacing generic bad ID error (as a keyword)
  with a bad e-mail or phone error."
  ([errors id src-id ^String email-id ^String phone-id]
   (if errors
     (if (contains? errors src-id)
       (if-some [^String id (some-str id)]
         (if-some [^String dst-id (cond (str/index-of id \@ 1) email-id
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
  "Initializes errors configuration. Returns `ErrorsConfig` record."
  ^ErrorsConfig [config]
  (map->ErrorsConfig (map/map-values var/deref-symbol config)))

(system/add-init  ::settings [_ config] (init-errors config))
(system/add-halt! ::settings [_ config] nil)

(derive ::web        ::settings)
(derive ::api        ::settings)
(derive ::all        ::settings)
(derive ::priorities ::system/value)
(derive ::responses  ::system/value)
