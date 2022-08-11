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

(defn compile-populator
  "Prepares a single populator."
  {:no-doc true}
  [data entry]
  (cond
    (sequential? entry) (compile-populator data {:id (first entry) :fn (second entry) :args (nnext entry)})
    (ident?      entry) (compile-populator data (list (some-keyword entry) (some-symbol entry)))
    (string?     entry) (compile-populator data (some-symbol entry))
    (map?        entry)
    (let [{id    :id
           f     :fn
           cf    :compile
           args  :args
           cargs :compile-args} entry]
      (if-some [id (some-keyword id)]
        (let [f  (var/deref f)
              cf (if (not f) (var/deref cf))
              f  (or f (if cf (cf data id cargs) (var/deref (symbol id))))]
          (if (fn? f) [id (fn populator [req] (f req id args))]))))))

(defn compile
  "Prepares population map an a basis of configuration sequence by processing its
  elements.

  If the element is a map, it will look for `:id` and either `:fn` or `:compile`
  keys. The first should be a keyword (a populator ID used as a key when injecting
  value to a request map), the `:fn` should be a populator function or an ident (will
  be dereferenced), and the `:compile` should be a function (or an ident naming the
  function) which should return a populator. The compiling function will receive a
  value as an argument which will be a map containing route data.

  If the element is a sequence, the first element should be an identifier (a
  populator ID used as a key when injecting value to a request map) and the second
  should be a population function or an ident naming the function.

  If the element is a single value it should be a string or an ident naming the
  population function."
  [data config]
  (->> config (map (partial compile-populator data)) (filter vector?) vec))

(defn populate
  "For each populator map calls the function identified by map's value and associates
  its result in a request with the key it's identified by. Called function must
  accept two arguments (a request/context map and a key) and return a value to be
  associated."
  [req populators]
  (reduce (fn [req [k f]] (assoc req k (f req))) req populators))

(defn wrap-populators
  "Populators wrapping middleware."
  [k config]
  (log/msg "Installing populators:" k)
  {:name    k
   :compile (fn [data _]
              (if-some [populators (compile data config)]
                (fn [handler]
                  (fn [req]
                    (handler (populate req populators))))))})

(system/add-init  ::default [k config] (wrap-populators k config))
(system/add-halt! ::default [_ config] nil)

(derive ::web  ::default)
(derive ::api  ::default)
(derive ::all  ::default)
(derive ::post ::default)
(derive ::web-post ::default)
(derive ::api-post ::default)
