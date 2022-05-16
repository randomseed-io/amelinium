(ns user

  (:require [clojure.spec.alpha               :as                s]
            [orchestra.spec.test              :as               st]
            [clojure.spec.test.alpha          :as              cst]
            [clojure.spec.gen.alpha           :as              gen]
            [clojure.string                   :as              str]
            [clojure.repl                     :refer          :all]
            [clojure.test                     :refer [run-tests
                                                      run-all-tests]]
            [clojure.tools.namespace.repl     :refer [refresh
                                                      refresh-all]]
            [next.jdbc                        :as             jdbc]
            [next.jdbc.sql                    :as              sql]
            [next.jdbc.result-set             :as       result-set]
            [next.jdbc.sql.builder            :as          builder]
            [clj-uuid                         :as             uuid]
            [expound.alpha                    :as          expound]
            [taoensso.nippy                   :as            nippy]
            [alphabase.core                   :as        alphabase]
            [clojurewerkz.balagan.core        :as                b]
            [ragtime.repl                     :as         database]
            [reitit.core                      :as                r]
            [amelinium.logging                :as              log]
            [amelinium.app                    :as              app]
            [amelinium.system                 :as           system]
            [amelinium.web                    :as              web]
            [amelinium.web.model.user         :as             user]
            [amelinium.http                   :as             http]
            [amelinium.http.middleware        :as       middleware]
            [amelinium.http.router            :as           router]
            [amelinium.http.handler           :as          handler]
            [amelinium.auth                   :as             auth]
            [amelinium.auth.preference        :as        auth-pref]
            [amelinium.auth.pwd               :as              pwd]
            [io.randomseed.utils.bus          :as              bus]
            [io.randomseed.utils.fs           :as               fs]
            [io.randomseed.utils.ip           :as               ip]
            [io.randomseed.utils.map          :as              map]
            [io.randomseed.utils.crypto       :as           crypto]
            [io.randomseed.utils              :refer    [some-str]]
            [io.randomseed.utils              :as            utils]
            [amelinium.spec                   :as             spec]
            [amelinium.db                     :as               db]

            [clojure.core.async               :as             async]
            [clojure.core.async               :refer  [<! <!! >! >!!
                                                       alts! alts!!
                                                       chan close!
                                                       go go-loop
                                                       put! thread]]

            [tick.core                         :as                t]
            [puget.printer                     :as            puget]
            [puget.printer                     :refer      [cprint]]
            [kaocha.repl                       :refer          :all]))

;; Printing and reflection warnings

(set! *warn-on-reflection* true)

(alter-var-root
 #'s/*explain-out*
 (constantly
  (expound/custom-printer {:show-valid-values? false
                           :print-specs?       true
                           :theme              :figwheel-theme})))

;; nREPL on demand

(when (System/getProperty "nrepl.load")
  (require 'nrepl)
  ;;(require 'infra)
  )

;; Testing

(st/instrument)

(defn test-all []
  (refresh)
  (cst/with-instrument-disabled
    (binding [amelinium.core/*some-switch* :some-value]
      (run-all-tests))))

;; generate preferred auth for account-types

(comment 
  (refresh-all)
  (cst/with-instrument-disabled (test-all))
  (cst/with-instrument-disabled (run-all))
  )
