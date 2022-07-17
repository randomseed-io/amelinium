(ns

    ^{:doc    "I18N pluralizing functions"
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.i18n.pluralizers

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [tongue.core             :as    tongue]
            [io.randomseed.utils     :refer   :all]
            [io.randomseed.utils.var :as       var]
            [io.randomseed.utils.map :as       map]
            [amelinium.locale        :as         l]))

(defn en
  ([n] (str n))
  ([n any] any)
  ([n singular plural] (if (= 1 n) singular plural))
  ([n zero singular plural] (if (= 1 n) singular (if (= 0 n) zero plural))))

(defn- pl-nominative?
  [n]
  (and (>= 4 (mod n 10) 2)
       (let [mh (mod n 100)] (or (< mh 10) (>= mh 20)))))

(defn pl
  ([n] (str n))
  ([n any] any)
  ([n singular plural] (if (= 1 n) singular plural))
  ([n zero singular plural] (case n 1 singular 0 zero plural))
  ([n zero singular plural-nominative plural-genitive]
   (cond
     (= 1 n)            singular
     (= 0 n)            zero
     (pl-nominative? n) plural-nominative
     :else              plural-genitive)))
