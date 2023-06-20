(ns

    ^{:doc    "amelinium service, http handling."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces             :as               p]
            [reitit.core                     :as               r]
            [reitit.ring                     :as            ring]
            [lazy-map.core                   :as        lazy-map]
            [amelinium.proto.http            :as            http]
            [amelinium                       :refer         :all]
            [io.randomseed.utils             :refer         :all]
            [io.randomseed.utils.map         :refer     [qassoc]]
            [io.randomseed.utils.reitit.http])

  (:import (clojure.lang         Associative
                                 IPersistentMap)
           (amelinium.proto.http HTTP)
           (lazy_map.core        LazyMap)
           (reitit.core          Match)))

(p/import-vars [io.randomseed.utils.reitit.http
                router? router match match?
                route-data route-data-param route-name
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
              (if-some [^Match m (get req ::r/match)] (.data ^Match match)))
          param)))

  Associative

  (get-route-data
    ([req]
     (or (get req :route/data)
         (if-some [^Match m (get req ::r/match)] (.data ^Match match))))
    ([req param]
     (get (or (get req :route/data)
              (if-some [^Match m (get req ::r/match)] (.data ^Match match)))
          param))
    ([req match param]
     (get (or (get req :route/data)
              (get match :data)
              (if-some [^Match m (get req ::r/match)] (.data ^Match match)))
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

(extend-protocol http/HTTP

  nil

  (request?            ^Boolean [_] false)
  (response?           ^Boolean [_] false)
  (app-status                   [_] nil)
  (response-headers             [_] nil)
  (response-status              [_] nil)
  (response-body                [_] nil)
  (response-location
    ([_]        nil)
    ([_ f]      nil)
    ([_ f args] nil)))
