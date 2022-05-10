(ns

    ^{:doc    "amelinium service, http handling."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.http

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces             :as        p]
            [reitit.core                     :as        r]
            [reitit.ring                     :as     ring]
            [io.randomseed.utils             :refer  :all]
            [io.randomseed.utils.reitit.http :as     http]))

(p/import-vars [io.randomseed.utils.reitit.http
                router? router match? match
                route-data route-data-param route-name
                route-middleware route-handler route-conflicting?
                path req-or-route-param])

(defn inject-route-data
  [req]
  (assoc req :route/data (get (get req ::r/match) :data)))

(defn get-route-data
  ([req-or-match]
   (or (get req-or-match :route/data)
       (route-data req-or-match)))
  ([req-or-match param]
   (get (get-route-data req-or-match) param))
  ([req match param]
   (get (or (get req :route/data) (route-data match)) param)))
