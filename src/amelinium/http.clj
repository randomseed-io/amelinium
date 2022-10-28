(ns

    ^{:doc    "amelinium service, http handling."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.http

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces             :as               p]
            [reitit.core                     :as               r]
            [reitit.ring                     :as            ring]
            [io.randomseed.utils             :refer         :all]
            [io.randomseed.utils.map         :refer     [qassoc]]
            [io.randomseed.utils.reitit.http :as            http])

  (:import [reitit.core Match]))

(p/import-vars [io.randomseed.utils.reitit.http
                router? router match match?
                route-data-param route-name
                route-middleware route-handler route-conflicting?
                path req-or-route-param])

(defprotocol Datable
  (get-route-data [req-or-match] [req-or-match param] [req match param]))

(extend-protocol Datable

  Match

  (get-route-data
    ([^Match match]
     (.data ^Match match))
    ([^Match match param]
     (get (.data ^Match match) param))
    ([^Match match req param]
     (get (or (.data ^Match match)
              (get req :route/data)
              (get (get req ::r/match) :data))
          param)))

  clojure.lang.IPersistentMap

  (get-route-data
    ([req]
     (or (get req :route/data)
         (get (get req ::r/match) :data)))
    ([req param]
     (get (or (get req :route/data)
              (get (get req ::r/match) :data))
          param))
    ([req match param]
     (get (or (get req :route/data)
              (get match :data)
              (get (get req ::r/match) :data))
          param)))

  clojure.lang.Associative

  (get-route-data
    ([req]
     (or (get req :route/data)
         (get (get req ::r/match) :data)))
    ([req param]
     (get (or (get req :route/data)
              (get (get req ::r/match) :data))
          param))
    ([req match param]
     (get (or (get req :route/data)
              (get match :data)
              (get (get req ::r/match) :data))
          param)))

  nil

  (get-route-data
    ([req-or-match]
     nil)
    ([req-or-match param]
     nil)
    ([req match param]
     (if (nil? match) nil (get-route-data match req param)))))

(defn inject-route-data
  [req]
  (qassoc req :route/data (get (get req ::r/match) :data)))
