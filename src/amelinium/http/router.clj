(ns

    ^{:doc    "amelinium service, HTTP routing."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.http.router

  (:require [reitit.core               :as  route]
            [reitit.ring               :as   ring]
            [amelinium.system          :as system]
            [io.randomseed.utils.map   :as    map]
            [io.randomseed.utils.var   :as    var]
            [clojurewerkz.balagan.core :as      b]))

(defonce routes  nil)
(defonce default nil)

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

(defn routes-parse
  [v keyz]
  (if (symbol? v)
    (var/deref-symbol v)
    (if (and keyz (map? v))
      (reduce #(map/update-existing %1 %2 set-with-meta) v keyz)
      v)))

(defn deref-symbols
  [config keyz]
  (let [keyz    (seq keyz)
        prepper #(routes-parse % keyz)]
    (b/update config
              [:* :* :* :* :* :* :* :* :*] prepper
              [:* :* :* :* :* :* :* :*]    prepper
              [:* :* :* :* :* :* :*]       prepper
              [:* :* :* :* :* :*]          prepper
              [:* :* :* :* :*]             prepper
              [:* :* :* :*]                prepper
              [:* :* :*]                   prepper
              [:* :*]                      prepper)))

(defn prep-routes
  [config]
  (if (map? config)
    (deref-symbols (:routes config) (:preserve-metas (:options config)))
    (deref-symbols config nil)))

(defn prep-router
  [config]
  (deref-symbols config nil))

(system/add-prep  ::routes  [_ config] (prep-routes config))
(system/add-init  ::routes  [k config] (var/make k (new-routes config)))
(system/add-halt! ::routes  [k config] (var/make k nil))

(system/add-prep  ::default [_ config] (prep-router config))
(system/add-init  ::default [k config] (var/make k (new-router config)))
(system/add-halt! ::default [k config] (var/make k nil))

(derive ::web        ::default)
(derive ::api        ::default)
(derive ::web-routes ::routes)
(derive ::api-routes ::routes)
