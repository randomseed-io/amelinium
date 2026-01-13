(ns

    ^{:doc    "amelinium service, HTTP routing."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.router

  (:require [reitit.core               :as  route]
            [reitit.ring               :as   ring]
            [ring.middleware.cors      :as   cors]
            [amelinium.system          :as system]
            [io.randomseed.utils       :as  utils]
            [io.randomseed.utils.map   :as    map]
            [io.randomseed.utils.var   :as    var]
            [clojurewerkz.balagan.core :as      b]))

(defonce ^:redef routes  nil)
(defonce ^:redef default nil)

;; Initializers

(extend-protocol route/Expand
  clojure.lang.Var
  (expand [this _] {:handler (var/deref this)})
  clojure.lang.Symbol
  (expand [this _] {:handler (var/deref this)}))

(defn new-router
  [config]
  (apply ring/router config))

(defn new-routes
  [config]
  config)

(defn- apply-with-meta
  [f v]
  (if (meta v)
    (with-meta (f v) (meta v))
    (f v)))

(defn- set-with-meta
  [v]
  (apply-with-meta set v))

(defn- reduce-sets-with-meta
  [v keyz]
  (reduce #(map/update-existing %1 %2 set-with-meta) v keyz))

(defn- descriptive?
  [v]
  (and (map? v)
       (or (contains? v :description)
           (contains? v :summary))))

(defn- prep-descriptions
  [m]
  (-> m
      (map/update-existing :description #(if (sequential? %) (apply utils/str-spc %) %))
      (map/update-existing :summary     #(if (sequential? %) (apply utils/str-spc %) %))))

(defn routes-parse
  [v keyz]
  (cond-> v
    (symbol?      v)    var/deref-symbol
    (descriptive? v)    prep-descriptions
    (and (map? v) keyz) (reduce-sets-with-meta keyz)))

(defn deref-symbols
  [config keyz]
  (let [keyz    (seq keyz)
        prepper #(routes-parse % keyz)]
    (b/update config
              [:* :* :* :* :* :* :* :* :* :* :*] prepper
              [:* :* :* :* :* :* :* :* :* :*]    prepper
              [:* :* :* :* :* :* :* :* :*]       prepper
              [:* :* :* :* :* :* :* :*]          prepper
              [:* :* :* :* :* :* :*]             prepper
              [:* :* :* :* :* :*]                prepper
              [:* :* :* :* :*]                   prepper
              [:* :* :* :*]                      prepper
              [:* :* :*]                         prepper
              [:* :*]                            prepper)))

(defn prep-routes
  [config]
  (if (map? config)
    (deref-symbols (:routes config) (:preserve-metas (:options config)))
    (deref-symbols config nil)))

(defn prep-router
  [config]
  (deref-symbols config nil))

(defn expand-routes
  [k config]
  {k (prep-routes config)})

(defn expand-router
  [k config]
  {k (prep-router config)})

(system/add-expand ::routes  [k config] (expand-routes k config))
(system/add-init   ::routes  [k config] (var/make k (new-routes config)))
(system/add-halt!  ::routes  [k      _] (var/make k nil))

(system/add-expand ::default [k config] (expand-router k config))
(system/add-init   ::default [k config] (var/make k (new-router config)))
(system/add-halt!  ::default [k      _] (var/make k nil))

(derive ::web        ::default)
(derive ::api        ::default)
(derive ::web-routes ::routes)
(derive ::api-routes ::routes)
