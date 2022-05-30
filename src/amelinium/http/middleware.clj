(ns

    ^{:doc    "amelinium service, middleware handling."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware

  (:refer-clojure :exclude [uuid random-uuid parse-long])

  (:require [ring.util.response]
            [ring.util.http-response   :as  response]
            [amelinium.logging         :as       log]
            [amelinium.system          :as    system]
            [io.randomseed.utils       :refer   :all]
            [io.randomseed.utils.var   :as       var]
            [io.randomseed.utils.map   :as       map]))

(defn init-renderer
  "Generic middleware which renders results by calling configured web handler
  function (`:post` key of `config` if `config` is a map or a value of `config`)."
  [k config]
  (let [web-handler-sym (if (map? config) (:post config) config)
        web-handler     (var/deref-symbol web-handler-sym)]
    (when web-handler
      (log/msg "Installing rendering web handler:"  web-handler-sym)
      {:name    (or (keyword k) ::renderer)
       :compile (fn [data opts]
                  (fn [handler]
                    (fn [req]
                      (let [resp (handler req)]
                        (if (response/response? resp)
                          resp
                          (web-handler resp))))))})))

(defn init-preparer
  "Generic middleware which prepares a request for the controller."
  [k config]
  (let [preparer-sym (if (map? config) (:pre config) config)
        preparer     (var/deref-symbol preparer-sym)]
    (when preparer
      (log/msg "Installing generic request preparer:"  preparer-sym)
      {:name    (or  (keyword k) ::preparer)
       :compile (fn [data opts]
                  (fn [handler]
                    (fn [req]
                      (let [resp (preparer req)]
                        (if (response/response? resp) resp (handler resp))))))})))

(defn prep-chain
  [config]
  (if (system/ref? config)
    config
    (mapv var/deref-symbol config)))

(system/add-prep  ::chain    [_ config] (prep-chain config))
(system/add-init  ::chain    [k config] (var/make k (prep-chain config)))
(system/add-halt! ::chain    [k config] (var/make k nil))

(system/add-init  ::renderer [k config] (init-renderer k config))
(system/add-halt! ::renderer [_ config] nil)

(system/add-init  ::preparer [k config] (init-preparer k config))
(system/add-halt! ::preparer [_ config] nil)

(derive ::default-chain ::chain)
