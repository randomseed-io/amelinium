(ns

    ^{:doc    "amelinium, core."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.core

  (:require   [clojure.set]
              [amelinium             :as    GW]
              [amelinium.db          :as    db]
              [io.randomseed.utils   :as utils]))

;;
;; Settings
;;

(def ^{:added   "1.0.0"
       :dynamic true
       :tag     Boolean}
  *some-switch*
  "Doco."
  true)

(defn generate
  [& more]
  nil)

