(ns

    ^{:doc    "amelinium service, content types middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.content

  (:refer-clojure :exclude [uuid random-uuid parse-long])

  (:require [ring.middleware.accept  :as    accept]
            [amelinium.logging       :as       log]
            [amelinium.system        :as    system]
            [io.randomseed.utils.vec :as       vec]
            [io.randomseed.utils.var :as       var]
            [io.randomseed.utils     :refer   :all]))

(defonce settings nil)

(defn wrap-accept
  "Content types handling middleware."
  [k config]
  (let []
    (log/msg "Installing content types handler")
    {:name    k
     :compile (fn [_ _]
                (fn [handler]
                  (let [handler (accept/wrap-accept handler config)]
                    (fn [req]
                      (handler req)))))}))

(defn prep-accept
  [config]
  (if-not (map? config)
    config
    (-> config
        (update :mime     vec/of-strings system/ref?)
        (update :language vec/of-strings system/ref?)
        (update :charset  vec/of-strings system/ref?)
        (update :encoding vec/of-strings system/ref?)
        (update :mime     (fnil identity ["text/html"]))
        (update :language (fnil identity ["en"]))
        (update :charset  (fnil identity ["utf-8"]))
        (update :encoding (fnil identity ["identity"])))))

(system/add-init  ::content [k config] (wrap-accept k (var/reset settings (prep-accept config))))
(system/add-prep  ::content [_ config] (prep-accept config))
(system/add-halt! ::content [_ config] (var/reset settings nil))
