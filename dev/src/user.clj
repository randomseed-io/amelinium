(ns user
  (:require
   [clojure.spec.alpha                :as                  s]
   [orchestra.spec.test               :as                 st]
   [clojure.spec.test.alpha           :as                cst]
   [clojure.spec.gen.alpha            :as                gen]
   [clojure.repl                      :refer            :all]
   [clojure.test                      :refer [run-tests
                                              run-all-tests]]
   [clojure.tools.namespace.repl       :refer  [refresh
                                                refresh-all]]
   [expound.alpha                      :as           expound]
   [amelinium.cloverage-workaround     :as    cov-workaround]

   [puget.printer                     :refer        [cprint]]
   [kaocha.repl                       :refer            :all]
   [infra]))

(defn cloverage-workaround!
  "Installs a safe cloverage wrapper for REPL use."
  []
  (cov-workaround/install!))

(defn test-all []
  (refresh)
  (cst/with-instrument-disabled
    (run-all-tests)))

(when (System/getProperty "nrepl.load")
  (require 'amelinium.nrepl))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(alter-var-root #'*warn-on-reflection*
                (constantly true)
                (when (thread-bound? #'*warn-on-reflection*)
                  (set! *warn-on-reflection* true)))

(alter-var-root #'s/*explain-out*
                (constantly
                 (expound/custom-printer {:show-valid-values? false
                                          :print-specs?       true
                                          :theme              :figwheel-theme})))

(cloverage-workaround!)
(st/instrument)

(comment 
  (refresh-all)
  (cst/with-instrument-disabled (test-all))
  (cst/with-instrument-disabled (run-all))
  )
