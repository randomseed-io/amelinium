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

;; Users and passwords

(defn set-password
  "Sets a user password in a database for the given user-spec (a user map, user ID or
  UID). It will use the password configuration obtained from authentication
  configuration associated with user's account type."
  ([user-spec]
   (set-password nil user-spec nil))
  ([user-spec plain-password]
   (set-password nil user-spec plain-password))
  ([db user-spec plain-password]
   (if-some [db (if db db (do (println "Using db/auth as the data source.") db/auth))]
     (if-some [user-id (user/find-id db user-spec)]
       (if-some [user (user/props db user-id)]
         (let [atype       (:account-type user)
               atexp       (when-not atype " (default)")
               auth-config (auth/config-or-default atype)
               auth-model  (some-str (:id auth-config))
               atype       (or atype (:account-types/default auth-config))]
           (println (str "Changing password for user " user-id "." \newline
                         "Account type is " atype atexp
                         ", chosen auth model is " auth-model "." \newline))
           (if-some [plain-password (or (some-str plain-password) (crypto/ask-pass))]
             (if-some [chains (auth/make-password-json plain-password auth-config)]
               (let [ret (user/update-password db user-id chains)
                     cnt (or (::jdbc/update-count ret) 0)]
                 (when (pos-int? cnt) (println "Password changed successfully."))
                 (println (str "Updated rows: " cnt)))
               (println "Authentication engine could not produce password chains."))
             (println "Password is empty or blank.")))
         (println "Cannot retrieve user properties."))
       (println "Cannot find user in a database."))
     (println "Data source is not active. Run the application?"))))

(defn lock-account
  ([user-spec]
   (lock-account nil user-spec))
  ([db user-spec]
   (let [db (if db db (do (println "Using db/auth as the data source.") db/auth))]
     (if-some [user-id (user/find-id db user-spec)]
       (do (println (str "Locking account for user " user-id))
           (user/prop-set db user-id :locked (t/now)))
       (println "Cannot find user in a database.")))))

(defn unlock-account
  ([user-spec]
   (unlock-account nil user-spec))
  ([db user-spec]
   (let [db (if db db (do (println "Using db/auth as the data source.") db/auth))]
     (if-some [user-id (user/find-id db user-spec)]
       (do (println (str "Unlocking account for user " user-id))
           (user/prop-del db user-id :locked))
       (println "Cannot find user in a database.")))))

;; generate preferred auth for account-types

(comment 
  (refresh-all)
  (cst/with-instrument-disabled (test-all))
  (cst/with-instrument-disabled (run-all))
  )
