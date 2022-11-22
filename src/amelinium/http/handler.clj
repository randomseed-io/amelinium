(ns

    ^{:doc    "amelinium service, http handlers configuration."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.handler

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [reitit.ring               :as      ring]
            [io.randomseed.utils       :refer   :all]
            [io.randomseed.utils.var   :as       var]
            [amelinium.system          :as    system]
            [clojurewerkz.balagan.core :as         b]))

(defonce default nil)

(defn new-handler
  [{:keys [router resource-pathname not-found redirect-slash]}]
  (let [not-found         (if not-found (var/deref-symbol not-found) {:status 404 :body "Not found"})
        not-found         (if not-found (if (fn? not-found) not-found (constantly not-found)))
        redirect-slash    (if redirect-slash (if (true? redirect-slash) :both (some-keyword redirect-slash)))
        resource-pathname (if (true? resource-pathname) "/" resource-pathname)
        resource-pathname (some-str resource-pathname)
        extra-handlers    nil
        extra-handlers    (if-not not-found
                            extra-handlers
                            (cons (ring/create-default-handler {:not-found not-found})
                                  extra-handlers))
        extra-handlers    (if-not resource-pathname
                            extra-handlers
                            (cons (ring/create-resource-handler {:path resource-pathname})
                                  extra-handlers))
        extra-handlers    (if-not redirect-slash
                            extra-handlers
                            (cons (ring/redirect-trailing-slash-handler {:method redirect-slash})
                                  extra-handlers))]
    (ring/ring-handler
     router
     (apply ring/routes extra-handlers))))

(defn prep-handler
  [config]
  (if-not (or (vector? config) (map? config))
    config
    (b/update config [:*] var/deref-symbol)))

(system/add-prep  ::default [_ config] config)
(system/add-init  ::default [k config] (var/make k (new-handler config)))
(system/add-halt! ::default [k config] (var/make k nil))

(derive ::web ::default)
(derive ::api ::default)
