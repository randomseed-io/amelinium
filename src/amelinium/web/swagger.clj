(ns

    ^{:doc    "amelinium service, Swagger support."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.swagger

  (:refer-clojure :exclude [parse-long uuid random-uuid run!])

  (:require [reitit.swagger                  :as    swagger]
            [reitit.swagger-ui               :as swagger-ui]
            [amelinium.logging               :as        log]
            [amelinium.system                :as     system]
            [io.randomseed.utils             :refer    :all]))


(defn init-swagger
  [k {:keys [enabled?] :or {enabled? true}}]
  (when enabled?
    (log/msg "Creating Swagger handler")
    (swagger/create-swagger-handler)))

(defn init-swagger-ui
  [k {:keys [enabled?] :or {enabled? true} :as config}]
  (when enabled?
    (log/msg "Creating Swagger UI handler")
    (let [config (dissoc config :enabled?)]
      (if (seq config)
        (swagger-ui/create-swagger-ui-handler config)
        (swagger-ui/create-swagger-ui-handler)))))

(system/add-init  ::handler    [k config] (init-swagger k config))
(system/add-halt! ::handler    [k config] nil)

(system/add-init  ::handler-ui [k config] (init-swagger-ui k config))
(system/add-halt! ::handler-ui [k config] nil)
