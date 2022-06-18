(ns

    ^{:doc    "amelinium service, context map population middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.http.middleware.populators

  (:refer-clojure :exclude [parse-long uuid random-uuid compile])

  (:require [amelinium.system        :as     system]
            [amelinium.logging       :as        log]
            [io.randomseed.utils.var :as        var]
            [io.randomseed.utils.map :as        map]
            [io.randomseed.utils     :refer    :all]))

(defn compile
  "Prepares population map."
  [populators]
  (->> populators
       (map #(if (map? %) (seq %) %))
       (map #(if (coll? %) % (list (keyword %) (symbol %))))
       flatten (partition 2)
       (map    (fn [[k v]] (list (keyword k) (if (ident? v) (var/deref v) v))))
       (filter (fn [[k v]] (and (keyword? k) (instance? clojure.lang.IFn v))))
       (map vec) vec))

(defn populate
  "For each populator map calls the function identified by map's value and associates
  its result in a request with the key it's identified by. Called function must
  accept two arguments (a request/context map and a key) and return a value to be
  associated."
  [req compiled-populators]
  (reduce (fn [req [k f]] (assoc req k (f req k))) req compiled-populators))

(defn wrap-populators
  "Populators wrapping middleware."
  [k compiled-populators]
  (log/msg "Installing populators:" k)
  {:name    k
   :compile (fn [_ _]
              (fn [handler]
                (fn [req]
                  (handler (populate req compiled-populators)))))})

(system/add-init  ::default [k config] (wrap-populators k (compile config)))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
