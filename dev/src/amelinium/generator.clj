(ns

    ^{:doc    "Sample data generator interface for amelinium library."
      :author "Paweł Wilk"
      :no-doc true}

    amelinium.generator

  (:require [amelinium.core             :as phone]
            [clojure.test.check.rose-tree  :as  rose]
            [clojure.test.check.generators :as  gens])

  (:import [java.util Random]))

(def amelinium
  (gens/no-shrink
   (clojure.test.check.generators/->Generator
    (fn [^Random rng _]
      (rose/make-rose
       (phone/generate nil nil phone/valid? nil nil (.nextLong ^Random rng))
       [])))))
