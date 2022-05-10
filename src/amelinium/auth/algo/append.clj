(ns

    ^{:doc    "amelinium service, plain-text appender."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.auth.algo.append

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string          :as       str]
            [amelinium.auth.pwd        :as       pwd]
            [io.randomseed.utils     :refer   :all]
            [io.randomseed.utils.map :as       map]))

(def ^:const default-options       {})
(def ^:const required-keys         [:prefix :suffix])
(def ^:const default-random-length 8)
(def ^:const default-charset       (vec "abcdefghijklmnopqrstuvwzyxABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
(def ^:const re-rnd                #"\{\{RND(?:\s*)(\d+)?\}\}")

(defn parse-random
  [v charset]
  (if (string? v)
    (str/replace
     v re-rnd
     (fn [m] (pwd/salt-string (to-long (last m) default-random-length) charset)))
    v))

(defn encrypt
  "Append the given prefix and/or suffix to a password."
  ([plain]
   (encrypt plain {} {}))
  ([plain options]
   (encrypt plain options {}))
  ([plain options settings]
   (let [options  (if (or (nil? options) (map? options)) options {})
         no-check (not (:checking options false))
         salt-set (:salt-charset options default-charset)
         options (cond-> options
                   true     (select-keys required-keys)
                   no-check (map/update-existing :prefix parse-random salt-set)
                   no-check (map/update-existing :suffix parse-random salt-set)
                   true     (map/update-to-bytes :prefix :suffix)
                   true     map/remove-empty-values)
         options (merge default-options options)
         result  (bytes-concat (:prefix options bzero) (text-to-bytes plain) (:suffix options bzero))]
     (assoc options :password result))))

(def check (partial pwd/standard-check encrypt))

(def handler
  {:encrypt-fn encrypt
   :check-fn   check})
