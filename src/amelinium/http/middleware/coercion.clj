(ns

    ^{:doc    "amelinium service, HTTP parameters coercion."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.coercion

  (:require [amelinium.system        :as   system]
            [amelinium.logging       :as      log]
            [reitit.ring.coercion    :as      rrc]
            [io.randomseed.utils.var :as      var]
            [io.randomseed.utils.map :as      map]))

(defn default-exception-handler
  [req e responder raiser]
  (rrc/handle-coercion-exception e responder raiser))

;; Initializers

(defn init-exceptions-handler
  [k {enabled?          :enabled?
      exception-handler :handler
      responder         :responder
      raiser            :raiser
      :or               {enabled?          true
                         exception-handler default-exception-handler
                         responder         identity
                         raiser            #(throw %)}}]
  {:name    k
   :compile (fn [{:keys [coercion parameters responses]} _]
              (if (and enabled? coercion (or parameters responses))
                (let [exception-handler (var/deref-symbol exception-handler)
                      responder         (var/deref-symbol responder)
                      raiser            (var/deref-symbol raiser)]
                  (fn [handler]
                    (fn coercion-exception-handler
                      ([req]
                       (try
                         (handler req)
                         (catch Exception e
                           (exception-handler e responder raiser))))
                      ([req respond raise]
                       (try
                         (handler req respond #(exception-handler % respond raise))
                         (catch Exception e
                           (exception-handler e respond raise)))))))))})

(defn init-coercer
  [k {:keys [init initializer config enabled?] :or {enabled? true}}]
  (if enabled?
    (if-some [coercer (var/deref-symbol (or init initializer))]
      (coercer (map/map-values var/deref-symbol config)))))

(system/add-init  ::coercer [k config] (var/make k (init-coercer k config)))
(system/add-halt! ::coerder [k config] (var/make k nil))

(system/add-init  ::exceptions [k config] (var/make k (init-exceptions-handler k config)))
(system/add-halt! ::exceptions [k config] (var/make k nil))

(derive ::coercer-all ::coercer)
(derive ::coercer-web ::coercer)
(derive ::coercer-api ::coercer)

(derive ::exceptions-all ::exceptions)
(derive ::exceptions-web ::exceptions)
(derive ::exceptions-api ::exceptions)
