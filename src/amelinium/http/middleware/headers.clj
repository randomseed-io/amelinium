(ns

    ^{:doc    "amelinium service, server headers middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.headers

  (:require [clojure.string      :as    str]
            [io.randomseed.utils :as  utils]
            [amelinium.logging   :as    log]
            [amelinium.system    :as system]))

(defn- map-entry
  [k v]
  (first {k v}))

(defn prep-hdr-line
  [[k v]]
  (str (utils/some-str-simple k) " "
       (if (coll? v)
         (str/join \space (map utils/some-str v))
         (utils/some-str-simple v))
       ";"))

(defn- prep-hdr-key
  [el]
  (utils/some-str-simple (key el)))

(defn- prep-hdr-val
  [el]
  (let [v (val el)
        v (if (fn? v) (v (key el)) v)]
    (if (coll? v)
      (str/join \space (map prep-hdr-line (seq v)))
      (utils/some-str-simple v))))

(defn- prep-hdr-entry
  [el]
  (map-entry (prep-hdr-key el)
             (prep-hdr-val el)))

(defn deleter
  [delete-list]
  (when (seq delete-list)
    (fn [headers]
      (apply dissoc headers delete-list))))

(defn adder
  [entries entries-map]
  (when (seq entries)
    (fn [headers]
      (if headers
        (into headers entries)
        entries-map))))

(defn transformer
  [& fns]
  (->> fns (filter identity) reverse (apply comp)))

(defn prep-config
  [config]
  (if (fn? (:fn/transformer config))
    config
    (let [config (group-by (comp #{:header/remove} val) (seq config))
          to-del (seq (map utils/some-str-simple (keys (get config :header/remove))))
          to-add (seq (map prep-hdr-entry (get config nil)))
          to-map (into {} to-add)
          del-fn (deleter to-del)
          add-fn (adder   to-add to-map)]
      {:fn/transformer (transformer add-fn del-fn)
       :fn/adder       (or add-fn identity)
       :fn/deleter     (or del-fn identity)
       :headers/add    (when to-add (vec to-add))
       :headers/del    (when to-del (vec to-del))
       :headers/map    (when (seq to-map) to-map)})))

(defn wrap-headers
  "Headers handler wrapper."
  [handler trf]
  (fn [req]
    (update (handler req) :headers trf)))

(defn init-headers
  "Server headers middleware."
  [k config]
  (log/msg "Initializing HTTP server headers:" k)
  (let [trf (:fn/transformer (prep-config config))]
    {:name    k
     :compile (fn [_ _]
                (fn [handler]
                  (let [handler (wrap-headers handler trf)]
                    (fn [req]
                      (handler req)))))}))

(system/add-init  ::default [k config] (init-headers k config))
(system/add-prep  ::default [k config] (prep-config config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
