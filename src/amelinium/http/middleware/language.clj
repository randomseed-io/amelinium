(ns

    ^{:doc    "amelinium service, language middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.language

  (:refer-clojure :exclude [parse-long uuid random-uuid force])

  (:require [reitit.ring             :as            ring]
            [reitit.core             :as               r]
            [amelinium.logging       :as             log]
            [amelinium.system        :as          system]
            [phone-number.core       :as           phone]
            [io.randomseed.utils.var :as             var]
            [io.randomseed.utils.map :as             map]
            [io.randomseed.utils.map :refer     [qassoc]]
            [io.randomseed.utils     :refer         :all]))

(def default-lang-param      :lang)
(def default-fallback-language :en)

(def ^:const re-lang (re-pattern "[A-Za-z_\\-\\:\\.]{2,7}"))

;; Configuration getters

(defn default-lang-id
  "Returns an ID (a keyword) of a default language set in config (under the `:default`
  key) or a value of the `default-fallback-language` variable (`:en`)."
  ([req]
   (default-lang-id nil (get req :language/settings)))
  ([_ config]
   (or (get config :default)
       default-fallback-language)))

(defn param
  "Returns a value associated with `:param` key in settings obtained from a request map
  or a config. If it does not exist or has falsy value, `default-lang-param` is
  returned."
  ([req]
   (or (get (get req :language/settings) :param) default-lang-param))
  ([_ config]
   (or (get config :param) default-lang-param)))

(defn supported
  "Returns a value associated with `:supported` key in settings obtained from a request
  map or a config."
  ([req]
   (get (get req :language/settings) :supported))
  ([_ config]
   (get config :supported)))

(defn config
  "Returns language settings obtained from a request map."
  ([req]
   (get req :language/settings))
  ([req config]
   (or config (get req :language/settings))))

(defn pickers
  "Returns configured pickers map obtained from a request."
  [req]
  (get req :language/pickers))

;; Generic pickers

(defn pick-without-fallback
  "The same as `pick` but returns `nil` instead of a default language when pickers
  cannot get one."
  ([req]
   (pick-without-fallback req (get req :language/pickers) :default))
  ([req pickers-or-picker-id]
   (if (map? pickers-or-picker-id)
     (pick-without-fallback req pickers-or-picker-id :default)
     (pick-without-fallback req (get req :language/pickers) pickers-or-picker-id)))
  ([req pickers picker-id]
   (if-some [picker (get pickers picker-id)]
     (picker req))))

(defn pick
  "Picks the right language using a chain of picking functions identified by a
  keyword. In its unary variant it obtains language picker chains configuration from
  a request map (under `:language/pickers` key) and looks for the picker chain
  identified by the `:default` key. When it's not `nil`, it is used.

  In its binary variant it takes a request map and pickers configuration given as a
  map OR a request map and an ID of a picker given as a keyword. When a map is given,
  the picker chain associated with the `:default` key will be used. When a picker ID
  is given the chain will be selected from picker chains obtained from the `req`
  under the key `:language/pickers`. Next, the picker chain will be run with a
  request map as its argument and the result will be then returned.

  In its ternary variant the given `pickers` map will be used to look for a chain
  under the given `picker-id`. This chain will be run with a request map as its
  argument to get the language.

  If the value returned by a language picker chain will be `nil` or `false` then the
  default language will be returned (if configured)."
  {:arglists '([req]
               [req pickers]
               [req picker-id]
               [req pickers picker-id])}
  ([req]
   (or (pick-without-fallback req (get req :language/pickers) :default)
       (default-lang-id req)))
  ([req pickers-or-picker-id]
   (or (if (map? pickers-or-picker-id)
         (pick-without-fallback req pickers-or-picker-id :default)
         (pick-without-fallback req (get req :language/pickers) pickers-or-picker-id))
       (default-lang-id req)))
  ([req pickers picker-id]
   (or (if-some [picker (get pickers picker-id)] (picker req))
       (default-lang-id req))))

;; Language pickers

(defn from-default
  ([req]         (or (get (config req) :default) default-fallback-language))
  ([req config]  (or (get config :default) default-fallback-language))
  ([_ _ default] (or default default-fallback-language)))

(defn get-in-req
  "Reads a language from the given request map by getting a value specified by a
  sequence of keys, converting the result to a keyword and checking against a set of
  supported languages."
  ([req supported k]
   (some->> (get req k)
            some-str
            (re-matches re-lang)
            some-keyword-simple
            (get supported)))
  ([req supported k1 k2]
   (some->> (get (get req k1) k2)
            some-str
            (re-matches re-lang)
            some-keyword-simple
            (get supported)))
  ([req supported k1 k2 k3]
   (some->> (get (get (get req k1) k2) k3)
            some-str
            (re-matches re-lang)
            some-keyword-simple
            (get supported)))
  ([req supported k1 k2 k3 & kpath]
   (some->> (get-in req (cons k1 (cons k2 (cons k3 kpath))))
            some-str
            (re-matches re-lang)
            some-keyword-simple
            (get supported))))

(defn from-form-params
  ([req]                   (get-in-req req (supported req) :form-params (param req)))
  ([req config]            (get-in-req req (supported nil config) :form-params (param nil config)))
  ([req _ lang-param supp] (get-in-req req supp :form-params lang-param)))

(defn from-body
  ([req]                   (get-in-req req (supported req) :body-params (param req)))
  ([req config]            (get-in-req req (supported nil config) :body-params (param nil config)))
  ([req _ lang-param supp] (get-in-req req supp :body-params lang-param)))

(defn from-accept
  ([req]                   (get-in-req req (supported req) :accept :language))
  ([req config]            (get-in-req req (supported nil config) :accept :language))
  ([req _ _ supp]          (get-in-req req supp :accept :language)))

(def default-picker
  {:compile (fn [config]
              (let [default (or (:default config) default-fallback-language)]
                (fn [req]
                  default)))})

(def req-picker
  {:compile (fn [config]
              (let [lang-param        (param nil config)
                    lang-param        (if (get config :stringify-lang-param?) (some-str lang-param) lang-param)
                    last-key          (if (some? lang-param) (cons lang-param nil))
                    supported         (supported nil config)
                    key-path          (get config :key-path)
                    key-path          (if (valuable? key-path) (if (seqable? key-path) key-path (cons key-path nil)))
                    path              (seq (concat (seq key-path) last-key))
                    [k1 k2 k3 & rest] path]
                (case (count path)
                  0 (fn [req] nil)
                  1 (fn [req] (get-in-req req supported k1))
                  2 (fn [req] (get-in-req req supported k1 k2))
                  3 (fn [req] (get-in-req req supported k1 k2 k3))
                  (fn [req] (apply get-in-req req supported k1 k2 k3 rest)))))})

(def body-picker
  (-> req-picker
      (qassoc :key-path :body-params)))

(def body-phone-picker
  {:compile (fn [config]
              (let [supported (supported nil config)]
                (fn [req]
                  (if-some [phone (some-str (get (get req :body-params) :phone))]
                    (if (phone/has-region? phone)
                      (-> (phone/region phone) name some-keyword (get supported)))))))})

(def body-phone-picker-str
  {:compile (fn [config]
              (let [supported (supported nil config)]
                (fn [req]
                  (if-some [phone (some-str (get (get req :body-params) "phone"))]
                    (if (phone/has-region? phone)
                      (-> (phone/region phone) name some-keyword (get supported)))))))})

(def accept-picker
  (-> req-picker
      (qassoc :key-path :accept
              :param :language)))

(def query-params-picker
  (-> req-picker
      (qassoc :key-path :query-params)))

(def query-params-picker-str
  (-> req-picker
      (qassoc :key-path :query-params
              :stringify-lang-param? true)))

(def form-params-picker-str
  {:compile (fn [config]
              (let [lang-param (some-str (param nil config))
                    supported  (supported nil config)]
                (fn [req]
                  (get-in-req req supported :form-params lang-param))))})

(def form-params-picker
  {:compile (fn [config]
              (let [lang-param (some-keyword-simple (param nil config))
                    supported  (supported nil config)]
                (fn [req]
                  (get-in-req req supported :form-params lang-param))))})

(def form-param-phone-picker-str
  {:compile (fn [config]
              (let [supported (supported nil config)]
                (fn [req]
                  (if-some [phone (some-str (get (get req :form-params) "phone"))]
                    (if (phone/has-region? phone)
                      (-> (phone/region phone) name some-keyword (get supported)))))))})

(def form-param-phone-picker
  {:compile (fn [config]
              (let [supported (supported nil config)]
                (fn [req]
                  (if-some [phone (some-str (get (get req :form-params) :phone))]
                    (if (phone/has-region? phone)
                      (-> (phone/region phone) name some-keyword (get supported)))))))})

(def path-picker
  {:compile (fn [config]
              (let [lang-param (param nil config)
                    supported  (supported nil config)]
                (fn [req]
                  (or (get-in-req req supported :path-params lang-param)
                      (get-in-req req supported ::r/match :path-params lang-param)))))})

(defn from-path
  ([req]
   (pick-without-fallback req :path-picker))
  ([req config]
   (from-path req (param nil config) (supported nil config)))
  ([req lang-param supp]
   (or (get-in-req req supp :path-params lang-param)
       (get-in-req req supp ::r/match :path-params lang-param))))

;; Setter

(defn force
  "Forces different language in a request map by setting `:language/id`
  and `:language/str`."
  [req language]
  (if-not (and req language)
    req
    (let [language-id  (delay (some-keyword-simple language))
          language-str (delay (some-str @language-id))]
      (qassoc req
              :language/id  language-id
              :language/str language-str))))

;; Language default pickers

(def ^:const default-picker-chain
  [path-picker
   form-params-picker-str
   body-picker
   :language/user
   :language/client
   accept-picker])

;; Initializers

(defn process-picker
  "Prepares a single picker. If it is a map, `:compile` key should be associated with a
  function which will return a language-picking function. Symbol may be given and it
  will be resolved. The compiling function should take a single argument which is a
  middleware configuration. All keys of the map except `:handler` and `:compile` will
  be passed to it.

  If `:handler` is specified in a map, it should be a function or a symbol which
  resolves to a function.

  If the `p` is not a map, it should be a function.

  A function returned by the compiling function, given as `:handler`, or simply
  assigned should take one argument, a request map, and return a language
  identifier as a keyword.

  When a language-picking function is created on a basis of a map (specified by
  `:handler` or `:compile`) then it has the responsibility of transforming its result
  to a keyword and checking if the chosen language identifier belongs to a supported
  languages expressed in a configuration as a set under the `:supported` key. Note
  that picker compiling function will receive configuration under the `config` key
  and should generate a function which takes a single argument (request map) whereas
  handler should take two arguments, the first being a configuration.

  When a language-picking function is not a map but a function (or a symbolic,
  resolvable identifier of a function) then it will be wrapped in another function
  and its result will be converted to a keyword and tested against existence in the
  aforementioned set of supported languages."
  [config p]
  (if-some [p (var/deref-symbol p)]
    (if (map? p)
      (let [parent     (var/deref-symbol (:derive p))
            p          (if (map? parent) (conj parent (dissoc p :parent)) p)
            config     (conj (or config {}) (dissoc p :compile :handler))
            compile-fn (var/deref-symbol (:compile p))
            handler-fn (var/deref-symbol (:handler p))
            picker-fn  (if compile-fn (compile-fn config) #(handler-fn config %))]
        (if (ifn? picker-fn) picker-fn))
      (if (ifn? p)
        (comp (supported nil config) some-keyword p)))))

(defn init-picker-chain
  [config pickers]
  (->> pickers
       (map (partial process-picker config))
       (filter identity)
       (apply some-fn)))

(defn- map-entry
  [k v]
  (first {k v}))

(defn init-pickers
  [config]
  (let [pickers-map (get config :pickers)]
    (if (system/ref? pickers-map)
      config
      (some->> pickers-map
               (map #(map-entry (key %) (init-picker-chain config (val %))))
               (remove (comp nil? val))
               (into {})
               (assoc config :pickers)))))

(defn prep-supported
  [v]
  (when-valuable v
    (if (system/ref? v) v
        (let [v (if (coll? v) v (cons v nil))
              v (filter identity (map some-keyword-simple v))
              v (valuable (vec v))]
          v))))

(defn prep-language
  [config]
  (let [default (or (some-keyword-simple (:default config)) default-fallback-language)]
    (-> config
        (assoc  :default   default)
        (update :supported #(if % (if (system/ref? %) % (map some-keyword-simple (if (coll? %) % (cons % nil))))))
        (update :supported #(if % (if (system/ref? %) % (disj (conj (set %) default) nil))))
        (update :supported #(if (system/ref? %) % (or (not-empty %) #{default})))
        (update :param     (fnil some-keyword-simple :lang))
        init-pickers)))

(defn wrap-language
  "Language wrapping middleware."
  [k config]
  (log/msg "Installing language handler")
  (let [picker-chain    (get-in config [:pickers :default])
        picker-fn       (or picker-chain (init-picker-chain config default-picker-chain))
        lang-pickers    (get config :pickers)
        default-lang-id (get config :default)
        config          (dissoc config :pickers)]
    {:name    k
     :config  config
     :compile (fn [_ _]
                (fn [handler]
                  (fn [req]
                    (let [lang-id  (delay (picker-fn req))
                          lang-str (delay (some-str @lang-id))]
                      (handler (qassoc req
                                       :language/settings config
                                       :language/pickers  lang-pickers
                                       :language/default  default-lang-id
                                       :language/id       lang-id
                                       :language/str      lang-str))))))}))

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
