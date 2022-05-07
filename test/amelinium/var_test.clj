(ns

    ^{:doc    "Amelinium, var tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.var-test

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.spec.alpha              :as               s]
            [clojure.spec.gen.alpha          :as             gen]
            [orchestra.spec.test             :as              st]
            [amelinium                       :refer         :all]
            [expound.alpha                   :as         expound]))

(s/check-asserts true)

