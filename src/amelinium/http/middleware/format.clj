(ns

    ^{:doc    "amelinium service, content format middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.format

  (:refer-clojure :exclude [uuid random-uuid parse-long])

  (:require [reitit.ring.middleware.muuntaja :as  muuntaja]
            [muuntaja.core                   :as         m]
            [amelinium.logging               :as       log]
            [amelinium.system                :as    system]
            [io.randomseed.utils.vec         :as       vec]
            [io.randomseed.utils.map         :as       map]
            [io.randomseed.utils.var         :as       var]
            [io.randomseed.utils             :refer   :all]))

(def ^:const format-default-charset            "utf-8")
(def ^:const format-default-format             "application/json")
(def ^:const format-default-return             :bytes)
(def ^:const format-default-allow-empty-input? true)
(def ^:const format-default-enabled?           true)

(defn- prep-formats
  [m]
  (if-not (map? m)
    m
    (map/map-keys-and-vals #(vector (some-str %1) (var/deref-symbol %2)) m)))

(defn init-format
  "Content format middleware."
  [k config]
  (when (:enabled? config)
    (log/msg "Initializing content format handler:" k)
    (m/create (if (map? config) (dissoc config :enabled?) config))))

(defn prep-format
  [{:keys [enabled? allow-empty-input? return default-charset default-format]
    :or   {enabled?           format-default-enabled?
           allow-empty-input? format-default-allow-empty-input?
           return             format-default-return
           default-charset    format-default-charset
           default-format     format-default-format}
    :as   config}]
  (if-not (map? config)
    config
    (-> (merge m/default-options config)
        (assoc
         :enabled?           (boolean enabled?)
         :allow-empty-input? (boolean allow-empty-input?)
         :return             (or (some-keyword return)      format-default-return)
         :default-charset    (or (some-str default-charset) format-default-charset)
         :default-format     (or (some-str default-format)  format-default-format))
        (update :charsets    (comp set (partial vec/of-strings system/ref?)))
        (update :formats     prep-formats))))

(system/add-init  ::default [k config] (init-format k (prep-format config)))
(system/add-prep  ::default [_ config] (prep-format config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
