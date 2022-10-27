(ns

    ^{:doc    "amelinium service, HTTP parameters coercion."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.coercion

  (:refer-clojure :exclude [parse-long uuid random-uuid compile])

  (:require [clojure.string          :as             str]
            [amelinium.system        :as          system]
            [amelinium.logging       :as             log]
            [amelinium.i18n          :as            i18n]
            [amelinium.schemas       :as         schemas]
            [amelinium.common        :as          common]
            [reitit.coercion         :as        coercion]
            [reitit.ring.coercion    :as             rrc]
            [reitit.impl             :refer [fast-assoc]]
            [malli.core              :as               m]
            [malli.registry          :as       mregistry]
            [io.randomseed.utils.var :as             var]
            [io.randomseed.utils.map :as             map]
            [io.randomseed.utils     :refer   :all]))

;; Common functions

(defn param-type
  "Takes a coercion error expressed as a map `e` and returns a string with parameter
  type if the type can easily be obtained (is a simple name expressed as a string or
  a string representation of keyword). For complex schemas it returns `nil`."
  [e]
  (if-some [s (some-str (get e :schema))]
    (if (some? (re-find #"^\:?[a-zA-Z0-9\-_\+\?\!]+$" s))
      (some-str (if (= \: (.charAt ^String s 0)) (subs s 1))))))

(defn translate-error
  "Takes a translation function with already applied language ID or a request map,
  parameter ID and parameter type and tries to find the best translations describing
  the erroneous field. Returns a map with the following keys `:parameter/name`,
  `:error/summary` and `:error/description`. To be used after receiving form errors
  data after a redirect."
  {:arglists '([req param-error-properties
                req param-id param-type
                req lang param-id param-type
                translate-sub param-error-properties
                translate-sub param-id param-type])}
  ([req-or-sub param-error-properties]
   (translate-error req-or-sub
                    (get param-error-properties :parameter/id)
                    (get param-error-properties :parameter/type)))
  ([req lang param-id param-type]
   (translate-error (common/translator-sub req lang) param-id param-type))
  ([req-or-sub param-id param-type]
   (if (map? req-or-sub)
     (translate-error (common/translator-sub req-or-sub nil) param-id param-type nil nil)
     (translate-error req-or-sub param-id param-type nil nil)))
  ([translate-sub param-id param-type _ _]
   (let [param-type? (some? param-type)
         param-id?   (some? param-id)
         mixed-id    (if (and param-type? param-id?) (str param-id "." param-type))
         mixed-id?   (some? mixed-id)
         param-name  (i18n/no-default (translate-sub :parameter param-id param-type))
         param-name? (some? param-name)
         param-name  (if param-name? param-name (some-str param-id))
         type-name   (if param-type? (i18n/no-default (translate-sub :parameter-type param-type param-id)))
         type-name?  (some? type-name)
         output      {:parameter/name param-name :parameter-type/name type-name}]
     (i18n/no-default
      (-> output
          (fast-assoc :error/summary
                      (or (if param-id?   (translate-sub :parameter-error param-id
                                                         param-name
                                                         param-id
                                                         param-type))
                          (if param-name? (translate-sub :error/parameter-name nil
                                                         param-name
                                                         param-id
                                                         param-type))
                          (if param-type? (translate-sub :type-error param-type
                                                         param-name
                                                         param-id
                                                         param-type))
                          (if type-name?  (translate-sub :error/type-name nil
                                                         type-name
                                                         param-id
                                                         param-type))
                          (if param-id?   (translate-sub :error/parameter nil
                                                         param-id
                                                         param-type))
                          (if param-type? (translate-sub :error/parameter-of-type
                                                         nil param-type))))
          (fast-assoc :error/description
                      (or (if mixed-id?   (translate-sub :parameter-should mixed-id
                                                         param-name
                                                         param-id
                                                         param-type))
                          (if param-id?   (translate-sub :parameter-should param-id
                                                         param-name
                                                         param-id
                                                         param-type))
                          (if param-type? (translate-sub :type-should param-type
                                                         param-name
                                                         param-id
                                                         param-type)))))))))

(defn recode-errors
  "Uses exception data to recode coercion errors in a form of a map. To be used mainly
  with API handlers. For web form error reporting `map-errors`, `list-errors`
  and `explain-errors` are better suited."
  [data]
  (let [dat (coercion/encode-error data)
        src (get dat :in)
        err (get dat :errors)
        err (if (coll? err) err (if (some? err) (cons err nil)))
        src (if (coll? src) src (if (some? src) (cons src nil)))
        src (if (= (first src) :request) (rest src) src)
        src (or (first src) :unknown)]
    (if err
      (->> err
           (map
            (fn [e]
              (if (map? e)
                (if-some [param-path (get e :path)]
                  (if-some [param-id (and (coll? param-path) (some-str (first param-path)))]
                    {:parameter/id    param-id
                     :parameter/src   src
                     :parameter/path  param-path
                     :parameter/type  (param-type e)
                     :parameter/value (get e :value)})))))
           (filter identity)))))

(defn explain-errors
  "Like `recode-errors` but each error map contains the additional key
  `:parameter/message` containing a human-readable messages created with translation
  function `translate-sub`. Enriches the output map with `:parameter/name`,
  `:error/summary` and `:error/description` entries. To be used in API responses."
  [data translate-sub]
  (if-some [r (recode-errors data)]
    (map #(into % (translate-error translate-sub %)) r)))

(defn list-errors
  "Returns a sequence of coercion errors containing 3-element sequences. First element
  of each being a parameter identifier, second element being a parameter type
  described by schema (if detected), and third being its current value. Takes an
  exception data map which should contain the `:coercion` key. Used, among other
  applications, to expose form errors to another page which should indicate them to
  a visitor."
  [data]
  (let [dat (coercion/encode-error data)
        err (get dat :errors)
        err (if (coll? err) err (if (some? err) (cons err nil)))]
    (->> err (filter identity) (map (juxt-seq (comp some-str first :path) param-type :value)))))

(defn map-errors
  "Like `list-errors` but returns a map in which keys are parameter names and values
  are parameter types (as defined in a schema used to validate and coerce them). Used
  to pass form errors to another page which should expose them to a visitor."
  [data]
  (if-some [r (list-errors data)]
    (reduce (partial apply common/fast-assoc-multi) {} (map butlast r))))

(defn join-errors
  "Used to produce a string containing parameter names and their types (as defined in
  schema) from a coercion errors simple map or coercion errors sequence (produced by
  `list-errors` or `map-errors` respectively). For anon-empty string it simply returns
  it. Used to generate a query string containing form errors in a form of `parameter-id`
  or `parameter-id:parameter-type` separated by commas. The whole string can then be
  encoded and used as a query parameter `:form-errors` when redirecting anonymous user
  to a page with a previous form which needs to be corrected."
  [errors]
  (if (and (string? errors) (pos? (count errors)))
    errors
    (if-some [errors (seq errors)]
      (->> errors
           (map (fn [[param-id param-type _]]
                  (if-some [param-id (and param-id (some-str (str/trim (str param-id))))]
                    (if-some [param-type (and param-type (some-str (str/trim (str param-type))))]
                      (str param-id ":" param-type)
                      param-id))))
           (filter identity)
           (str/join ",")))))

(defn join-errors-with-values
  "Used to produce a string containing parameter names and their types (as defined in
  schema) from a coercion errors simple map or coercion errors sequence (produced by
  `list-errors` or `map-errors` respectively). For anon-empty string it simply
  returns it. Used to generate a query string containing form errors in a form of
  `parameter-id` or `parameter-id:parameter-type:parameter-value` separated by
  commas. The whole string can then be encoded and used as a query parameter
  `:form-errors` when redirecting anonymous user to a page with a previous form which
  needs to be corrected. Be aware that it might be hard to parse the output string if
  a value contains a comma character."
  [errors]
  (if (and (string? errors) (pos? (count errors)))
    errors
    (if-some [errors (seq errors)]
      (->> errors
           (map (fn [[param-id param-type param-value]]
                  (if-some [param-id (and param-id (some-str (str/trim (str param-id))))]
                    (let [param-type (and param-type (some-str (str/trim (str param-type))))]
                      (str param-id ":" param-type ":" param-value)))))
           (filter identity)
           (str/join ",")))))

(defn split-error
  "Takes `param-id` and optional `param-type` and tries to clean-up their string
  representations to produce a 3-element vector. When only 1 argument is present or
  when the second argument is `nil` or empty, it will try to parse the first argument
  so if it contains a colon character it will be split into three parts: parameter
  ID, parameter type and value. Used as a helper by `parse-errors` and by argument
  parsers in template tags."
  ([param-id param-type param-value]
   (if-not param-type
     (if param-value
       (if param-id (split-error param-id nil))
       (if param-id (split-error param-id)))
     (let [id (some-str param-id)
           ty (some-str param-type)
           va (some-str param-value)
           id (if id (str/trim id))
           ty (if ty (str/trim ty))]
       (if (or (and id (pos? (count id)))
               (and ty (pos? (count ty))))
         [id ty va]))))
  ([param-id param-type]
   (if-not param-type
     (if param-id (split-error param-id))
     (let [id (some-str param-id)
           ty (some-str param-type)
           id (if id (str/trim id))
           ty (if ty (str/trim ty))]
       (if (or (and id (pos? (count id)))
               (and ty (pos? (count ty))))
         [id ty nil]))))
  ([param-id]
   (if (and (sequential? param-id) (seq param-id))
     (apply split-error (take 3 param-id))
     (if-some [param-id (some-str param-id)]
       (let [[f s v] (str/split (str/trim param-id) #":" 3)
             f       (if f (some-str (str/trim f)))
             s       (if s (some-str (str/trim s)))
             v       (if v (some-str v))]
         (if (or f s) [f s v]))))))

(defn parse-errors
  "Transforms a string previously exposed with `join-errors`, a list created
  with `list-errors` or a map resulted from calling `map-errors` into a map
  containing parameter names as keys and parameter types as values. Used to parse
  input from a query string or saved session variable when visiting a page with
  a previously visited form which to be corrected (after redirecting to it).
  The `explain-form-error` template tag can make use of such map to decide whether
  an input field had invalid data."
  [errors]
  (cond
    (map?        errors) errors
    (string?     errors) (if (pos? (count errors))
                           (->> (str/split errors #",+" 108)
                                (map split-error)
                                (map #(take 2 %))
                                (filter identity)
                                (into {})
                                not-empty))
    (sequential? errors) (->> (seq errors)
                              (map #(take 2 %))
                              (filter identity)
                              (reduce (partial apply common/fast-assoc-multi) {})
                              not-empty)))

(defn inject-errors
  "Takes coercion errors, parses them and injects into a `req` under a key named
  `:form/errors`."
  [req errors]
  (fast-assoc req :form/errors (delay (parse-errors errors))))

;; Default exception handler

(defn default-exception-handler
  [req e responder raiser]
  (rrc/handle-coercion-exception e responder raiser))

;; Initializers

(defn- process-schema-entry
  [id {:keys [compile options] :as sch}]
  (if (and compile (some? id))
    (if-some [compile (if (symbol? compile) (var/deref compile))]
      (compile id options))
    sch))

(defn init-registry
  [config]
  (let [local-schemas (->> config
                           (map/map-vals-by-kv process-schema-entry)
                           map/remove-empty-values)]
    (mregistry/fast-registry
     (merge (m/default-schemas) schemas/schemas local-schemas))))

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

(system/add-init  ::registry [k config] (var/make k (init-registry config)))
(system/add-halt! ::registry [k config] (var/make k nil))

(derive ::coercer-all ::coercer)
(derive ::coercer-web ::coercer)
(derive ::coercer-api ::coercer)

(derive ::exceptions-all ::exceptions)
(derive ::exceptions-web ::exceptions)
(derive ::exceptions-api ::exceptions)
