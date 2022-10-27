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

(defn wrap-accept
  "Content types handling middleware."
  [k config]
  (log/msg "Installing content types handler")
  {:name    k
   :compile (fn [_ _]
              (fn [handler]
                (let [handler (accept/wrap-accept handler config)]
                  (fn [req]
                    (handler req)))))})

(defn prep-language
  [config]
  (if (or (nil? (valuable config)) (system/ref? config))
    config
    (let [language (valuable (:language config))]
      (if (or (nil? language) (system/ref? language))
        config
        (let [default   (vec/of-strings (:default-language config) system/ref?)
              default   (first default)
              supported (if (map? language) (:supported language) language)
              supported (vec/of-strings supported system/ref?)
              supported (if (or (nil? default) (system/ref? supported)
                                (some #(and (some? %) (= default %)) supported))
                          supported
                          (conj (or supported []) default))]
          (-> config
              (dissoc :init :default-language)
              (assoc :language supported)))))))

(defn prep-accept
  [config]
  (if-not (map? config)
    config
    (-> config
        prep-language
        (update :mime     vec/of-strings system/ref?)
        (update :charset  vec/of-strings system/ref?)
        (update :encoding vec/of-strings system/ref?)
        (update :mime     (fnil identity ["text/html"]))
        (update :charset  (fnil identity ["utf-8"]))
        (update :encoding (fnil identity ["identity"])))))

(system/add-init  ::default [k config] (wrap-accept k (prep-accept config)))
(system/add-prep  ::default [_ config] (prep-accept config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
