(ns

    ^{:doc    "amelinium service, debugging middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.debug

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string          :as         str]
            [amelinium.logging       :as         log]
            [amelinium.system        :as      system]
            [zprint.core             :refer [zprint czprint]]))

(defonce ^:private counter  (atom 0))
(defonce ^:private icounter (atom 0))

;; Configuration initializers

(defn wrap
  "Debugging middleware."
  [k {:keys [enabled? request-keys response-keys tag]
      :or   {enabled? true}}]
  (when enabled?
    (let [id  (swap! counter inc)
          tag (or tag (str (name k) "#" id))]
      (log/msg "Initializing debugging middleware")
      (let [request-keys  (if (seq request-keys)  request-keys)
            response-keys (if (seq response-keys) response-keys)]
        {:name    k
         :compile (fn [_ _]
                    (let [iid  (swap! icounter inc)
                          itag (str iid)]
                      (fn [handler]
                        (fn [req]
                          (println "--------" itag "@" tag "IN  req-nil?" (nil? req) "type" (type req))
                          (when request-keys
                            (czprint (select-keys req request-keys)))
                          (let [resp (handler req)]
                            (println "--------" itag "@" tag "OUT resp-nil?" (nil? resp) "type" (type resp))
                            (when (some? response-keys)
                              (czprint (select-keys resp response-keys)))
                            resp)))))}))))

(system/add-init  ::default [k config] (wrap k config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
