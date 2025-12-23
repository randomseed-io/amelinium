(ns

    ^{:doc    "amelinium config readers."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.readers

  (:refer-clojure :exclude [ref])

  (:require [integrant.core             :as          ig]
            [maailma.core               :as        conf]
            [amelinium                  :as   amelinium]
            [tick.core                  :as           t]
            [clojure.java.io            :as          io]
            [clojure.string             :as         str]
            [io.randomseed.utils        :as       utils]
            [io.randomseed.utils.var    :as         var]
            [io.randomseed.utils.fs     :as          fs]))

(defn regex
  "Tagged literal reader function for regular expressions."
  [rgx]
  (re-pattern rgx))
