(ns

    ^{:doc    "amelinium service, debugging middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.debug

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string          :as         str]
            [amelinium.logging       :as         log]
            [amelinium.system        :as      system]
            [puget.printer           :refer [cprint]]))

;; Configuration initializers

(defn wrap
  "Debugging middleware."
  [k {:keys [enabled? request-keys response-keys]
      :or   {enabled? true}}]
  (when enabled?
    (log/msg "Initializing debugging middleware")
    {:name    k
     :compile (fn [_ _]
                (fn [handler]
                  (fn [req]
                    (println "--------")
                    (println "REQUEST:")
                    (cprint (select-keys req request-keys))
                    (let [resp (handler req)]
                      (println "---------")
                      (println "RESPONSE:")
                      (cprint (select-keys resp response-keys))
                      resp))))}))

(system/add-init  ::default [k config] (wrap k config))
(system/add-halt! ::default [_ config] nil)
