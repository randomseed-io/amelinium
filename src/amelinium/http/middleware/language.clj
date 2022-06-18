(ns

    ^{:doc    "amelinium service, language middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.language

  (:refer-clojure :exclude [parse-long uuid random-uuid force])

  (:require [reitit.ring             :as    ring]
            [amelinium.logging       :as     log]
            [amelinium.system        :as  system]
            [io.randomseed.utils.var :as     var]
            [io.randomseed.utils     :refer :all]))

(def default-fallback-language :en)

(def ^:const re-lang (re-pattern "[A-Za-z_\\-\\:\\.]{2,7}"))

;; Language detection

(defn lang-settings
  ([req]
   (get req :language/settings))
  ([req config]
   (or config (get req :language/settings))))

(defn default-lang-id
  ([req]
   (default-lang-id req (get req :language/settings)))
  ([req config]
   (or (get config :default)
       default-fallback-language)))

(defn default-lang-str
  ([req]
   (some-str (default-lang-id req (get req :language/settings))))
  ([req config]
   (some-str (default-lang-id req config))))

(defn path-lang-id
  ([req]
   (path-lang-id req (get req :language/settings) nil))
  ([req config]
   (path-lang-id req config nil))
  ([req config ring-match]
   (let [ring-match (or ring-match (ring/get-match req))
         config     (or config (get req :language/settings))
         lang-param (get config :language-param :lang)
         supported  (get config :supported)]
     (or (get supported (some-keyword-simple (get (get req        :path-params) lang-param)))
         (get supported (some-keyword-simple (get (get ring-match :path-params) lang-param))))))
  ([_ config ring-match path-params]
   (let [lang-param (get config :language-param :lang)
         supported  (get config :supported)]
     (or (get supported (some-keyword-simple path-params))
         (get supported (some-keyword-simple (get (get ring-match :path-params) lang-param)))))))

(defn path-lang-str
  ([req]
   (some-str (path-lang-id req (get req :language/settings) nil)))
  ([req config]
   (some-str (path-lang-id req config nil)))
  ([req config ring-match]
   (some-str (path-lang-id req config ring-match))))

(defn accept-lang-id
  ([req]
   (some-keyword-simple (get (get req :accept) :language)))
  ([req _]
   (some-keyword-simple (get (get req :accept) :language)))
  ([_ _ accept-data]
   (some-keyword-simple (get accept-data :language))))

(defn accept-lang-str
  ([req]
   (some-str (accept-lang-id req)))
  ([req _]
   (some-str (accept-lang-id req)))
  ([_ _ accept-data]
   (some-str (accept-lang-id nil nil accept-data))))

(defn form-lang-id
  ([req]
   (form-lang-id nil (get req :language/settings) (get req :form-params)))
  ([req config]
   (form-lang-id nil config (get req :form-params)))
  ([_ config fp]
   (when fp
     (let [lang-param (or (get config :language-param) "lang")]
       (some->> (get fp lang-param)
                some-str
                (re-matches re-lang)
                some-keyword-simple
                (get (get config :supported #{})))))))

(defn form-lang-str
  ([req]
   (some-str (form-lang-id req (get req :language/settings) (get req :form-params))))
  ([req config]
   (some-str (form-lang-id req config (get req :form-params))))
  ([_ config fp]
   (some-str (form-lang-id nil config fp))))

(defn body-lang-id
  ([req]
   (body-lang-id nil (get req :language/settings) (get req :body)))
  ([req config]
   (body-lang-id nil config (get req :body)))
  ([_ config fp]
   (when fp
     (let [lang-param (or (get config :language-param-api) :lang)]
       (some->> (get fp lang-param)
                some-str
                (re-matches re-lang)
                some-keyword-simple
                (get (get config :supported #{})))))))

(defn body-lang-str
  ([req]
   (some-str (body-lang-id req (get req :language/settings) (get req :body))))
  ([req config]
   (some-str (body-lang-id req config (get req :body))))
  ([_ config fp]
   (some-str (body-lang-id nil config fp))))

(defn guess-lang-nodefault-id
  ([req]
   (guess-lang-nodefault-id req (get req :language/settings)))
  ([req config]
   (or (path-lang-id    req config)
       (form-lang-id    req config)
       (accept-lang-id  req config)
       (body-lang-id    req config))))

(defn guess-lang-nodefault-str
  ([req]
   (some-str (guess-lang-nodefault-id req (get req :language/settings))))
  ([req config]
   (some-str (guess-lang-nodefault-id req config))))

(defn guess-lang-id
  ([req]
   (let [config (get req :language/settings)]
     (or (guess-lang-nodefault-id req config)
         (default-lang-id         req config))))
  ([req config]
   (or (guess-lang-nodefault-id req config)
       (default-lang-id         req config))))

(defn guess-lang-str
  ([req]
   (some-str (guess-lang-id req (get req :language/settings))))
  ([req config]
   (some-str (guess-lang-id req config))))

;; Setter

(defn force
  "Forces different language in a request map by setting :language/id
  and :language/str."
  [req language]
  (if-not (and req language)
    req
    (let [language-id  (delay (some-keyword-simple language))
          language-str (delay (some-str @language-id))]
      (assoc req
             :language/id  language-id
             :language/str language-str))))

;; Initializers

(defn prep-supported
  [v]
  (when-valuable v
    (if (system/ref? v) v
        (let [v (if (coll? v) v (cons v nil))
              v (filter identity (map some-keyword-simple v))]
          (valuable (vec v))))))

(defn prep-language
  [config]
  (let [default (or (some-keyword-simple (:default config)) default-fallback-language)]
    (-> config
        (assoc  :default            default)
        (update :supported          #(when % (if (system/ref? %) % (map some-keyword-simple (if (coll? %) % (cons % nil))))))
        (update :supported          #(when % (if (system/ref? %) % (disj (conj (set %) default) nil))))
        (update :supported          #(if (system/ref? %) % (or (not-empty %) #{default})))
        (update :language-param     (fnil some-keyword-simple :lang))
        (update :language-param-api (fnil some-keyword-simple :lang)))))

(defn wrap-language
  "Language wrapping middleware."
  [k config]
  (log/msg "Installing language handler")
  {:name    k
   :compile (fn [_ _]
              (fn [handler]
                (fn [req]
                  (let [lang-id  (delay (guess-lang-id req config))
                        lang-str (delay (some-str @lang-id))]
                    (handler (assoc req
                                    :language/settings config
                                    :language/id       lang-id
                                    :language/str      lang-str))))))})

(system/add-init  ::default [k config] (wrap-language k (prep-language config)))
(system/add-prep  ::default [_ config] (prep-language config))
(system/add-halt! ::default [_ config] nil)

(system/add-init  ::supported [_ config] (prep-supported config))
(system/add-prep  ::supported [_ config] (prep-supported config))
(system/add-halt! ::supported [_ config] nil)

(system/add-init  ::pickers   [_ config] config)
(system/add-halt! ::pickers   [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)

(derive ::web-pickers ::pickers)
(derive ::api-pickers ::pickers)
(derive ::all-pickers ::pickers)

(derive ::web-supported ::supported)
(derive ::api-supported ::supported)
(derive ::all-supported ::supported)
