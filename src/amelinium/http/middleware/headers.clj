(ns

    ^{:doc    "amelinium service, server headers middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.headers

  (:require [clojure.string          :as             str]
            [reitit.core             :as               r]
            [potpuri.core            :refer [deep-merge]]
            [io.randomseed.utils     :as           utils]
            [io.randomseed.utils.map :refer     [qassoc]]
            [amelinium.logging       :as             log]
            [amelinium.system        :as          system]))

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

(defn- add-missing
  ([m k v]
   (if (contains? m k) m (qassoc m k v)))
  ([m entries-map]
   (reduce-kv (fn [m k v] (if (contains? m k) m (qassoc m k v))) m entries-map)))

(defn deleter
  [delete-list]
  (if (seq delete-list)
    (if (nil? (next delete-list))
      ;; removing single header
      (let [delete-header (first delete-list)]
        (fn [headers] (dissoc headers delete-header)))
      ;; removing multiple headers
      (fn [headers] (apply dissoc headers delete-list)))))

(defn adder
  [entries entries-map replace?]
  (if (seq entries)
    (if replace?
      (if (nil? (next entries))
        ;; replacing single header
        (let [[h v] (first entries)]
          (fn [headers] (if headers (qassoc headers h v) entries-map)))
        ;; replacing multiple headers
        (fn [headers] (if headers (conj headers entries-map) entries-map)))
      (if (nil? (next entries))
        ;; adding header if does not exist
        (let [[h v] (first entries)]
          (fn [headers] (if headers (add-missing headers h v) entries-map)))
        ;; adding multiple headers which do not exist
        (fn [headers] (if headers (add-missing headers entries-map) entries-map))))))

(defn make-transformer
  [& fns]
  (->> fns (filter identity) reverse (apply comp)))

(defn prep-config
  [config]
  (if (not-empty config)
    (if (fn? (:fn/transformer config))
      config
      (let [replace? (boolean (:headers/replace? config))
            config   (dissoc config :headers/replace?)
            config   (group-by (comp #{:header/remove} val) (seq config))
            to-del   (seq (doall (map utils/some-str-simple (keys (get config :header/remove)))))
            to-add   (seq (doall (map prep-hdr-entry (get config nil))))
            to-map   (into {} to-add)
            del-fn   (deleter to-del)
            add-fn   (adder   to-add to-map replace?)]
        {:fn/transformer   (make-transformer add-fn del-fn)
         :fn/adder         (or add-fn identity)
         :fn/deleter       (or del-fn identity)
         :headers/add      (if to-add (vec to-add))
         :headers/del      (if to-del (vec to-del))
         :headers/map      (if (seq to-map) to-map)
         :headers/replace? replace?}))))

(defn wrap-headers
  "Headers handler wrapper."
  [req trf]
  (qassoc req :headers (trf (get req :headers))))

(defn transformer
  "Parses headers configuration and returns a transformer. Helpful when generating
  a non-Reitit handler."
  [config]
  (if (not-empty config) (:fn/transformer (prep-config config))))

(defn headers-compile
  "Prepares a middleware handler to be associated with an HTTP route."
  [config data opts]
  (let [local-config   (get data :headers)
        mergeable?     (or (nil? config)
                           (nil? local-config)
                           (and (map? config)
                                (map? local-config)
                                (:headers/replace? local-config)
                                (:headers/replace? config)
                                (or (not (contains? local-config :headers/merge?))
                                    (get local-config :headers/merge?))
                                (or (not (contains? config :headers/merge?))
                                    (get config :headers/merge?))))
        merged-prepped (if mergeable? (prep-config (conj (or config {}) local-config)))
        local-prepped  (if (map? local-config) (prep-config local-config))
        global-prepped (if (map? config) (prep-config config))
        trf            (if mergeable? (transformer merged-prepped))
        local-trf      (if-not trf (if (fn? local-config)  local-config  (transformer local-prepped)))
        global-trf     (if-not trf (if (fn? config) config (transformer global-prepped)))
        trf            (or trf (if local-trf
                                 (if global-trf (comp local-trf global-trf) local-trf)
                                 (if global-trf global-trf)))]
    (if trf
      (fn [handler]
        (fn [req]
          (wrap-headers (handler req) trf))))))

(defn init-headers
  "Server headers middleware."
  [k config]
  (log/msg "Initializing HTTP server headers:" k)
  {:name    k
   :compile (partial headers-compile config)})

(system/add-init  ::default [k config] (init-headers k config))
(system/add-halt! ::default [_ config] nil)

(system/add-init  ::handler [_ config] (transformer config))
(system/add-halt! ::handler [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)

(derive ::web-handler ::handler)
(derive ::api-handler ::handler)
(derive ::all-handler ::handler)
