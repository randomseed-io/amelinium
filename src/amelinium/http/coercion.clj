(ns

    ^{:doc    "amelinium service, HTTP parameters coercion."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.coercion

  (:require [amelinium.system        :as   system]
            [amelinium.logging       :as      log]
            [io.randomseed.utils.var :as      var]
            [io.randomseed.utils.map :as      map]
            [reitit.coercion.malli   :as      rcm]))

(defn init-coercer
  [k {:keys [coercer config] :or {coercer rcm/create}}]
  (if-some [coercer (var/deref-symbol coercer)]
    (coercer (map/map-values var/deref-symbol config))))

(system/add-init  ::all [k config] (var/make k (init-coercer k config)))
(system/add-halt! ::all [k config] (var/make k nil))

(derive ::web ::all)
(derive ::api ::all)
