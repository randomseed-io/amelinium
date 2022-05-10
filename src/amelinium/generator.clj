(ns

    ^{:doc    "Sample data generator interface for amelinium library."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.generator

  (:require [amelinium.core                :as  amelinium]
            [clojure.test.check.rose-tree  :as  rose]
            [clojure.test.check.generators :as  gens]))

(def amelinium
  (gens/no-shrink
   (clojure.test.check.generators/->Generator
    (fn [rng _]
      (rose/make-rose
       (amelinium/generate nil nil (constantly true) nil nil (.nextLong rng))
       [])))))
