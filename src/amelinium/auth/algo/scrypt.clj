(ns

    ^{:doc    "amelinium service, scrypt algorithm."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.auth.algo.scrypt

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:import com.lambdaworks.crypto.SCrypt)

  (:require [amelinium.auth.pwd         :as      pwd]
            [io.randomseed.utils      :refer  :all]
            [io.randomseed.utils.map  :as      map]))

(def ^:const default-options
  {:cpu-cost 32768
   :mem-cost     8
   :parallel     1})

(def ^:const required-keys
  [:salt :cpu-cost :mem-cost :parallel])

(defn encrypt
  "Encrypt a password string using the scrypt algorithm."
  ([plain]
   (encrypt plain {} {}))
  ([plain options]
   (encrypt plain options {}))
  ([plain options settings]
   (let [options (if (or (nil? options) (map? options)) options {:salt options})
         options (merge default-options (map/remove-empty-values (select-keys options required-keys)))
         salt    (to-bytes (map/lazy-get options :salt (pwd/salt-bytes 16)))
         result  (SCrypt/scrypt
                  (text-to-bytes plain)
                  salt
                  (int (:cpu-cost options))
                  (int (:mem-cost options))
                  (int (:parallel options))
                  (int 32))]
     (merge options {:salt salt :password result}))))

(def check (partial pwd/standard-check encrypt))

(def handler
  {:encrypt-fn encrypt
   :check-fn   check
   :defaults   default-options
   :shared     [:cpu-cost :mem-cost :parallel]})
