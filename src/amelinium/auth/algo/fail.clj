(ns

    ^{:doc    "amelinium service, appender which always fails."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.auth.algo.fail

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [amelinium.auth.pwd      :as       pwd]
            [io.randomseed.utils     :refer   :all]
            [io.randomseed.utils.map :as       map]))

(def ^:const default-options       {})
(def ^:const required-keys         [])

(defn encrypt
  ([plain]
   (encrypt plain {} {}))
  ([plain options]
   (encrypt plain options {}))
  ([plain options settings]
   (let [options (if (or (nil? options) (map? options)) options {})]
     (assoc options :password nil))))

(def check (partial pwd/standard-check (constantly nil)))

(def handler
  {:encrypt-fn encrypt
   :check-fn   check})
