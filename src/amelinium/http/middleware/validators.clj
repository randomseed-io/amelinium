(ns

    ^{:doc    "amelinium service, validators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.validators

  (:refer-clojure :exclude [uuid random-uuid parse-long])

  (:require [clojure.string                        :as             str]
            [clojure.set                           :as             set]
            [amelinium.http.middleware.session     :as         session]
            [amelinium.logging                     :as             log]
            [amelinium.system                      :as          system]
            [phone-number.core                     :as           phone]
            [io.randomseed.utils.validators        :as               v]
            [io.randomseed.utils.validators.common :as              vc]
            [io.randomseed.utils.var               :as             var]
            [io.randomseed.utils.vec               :as             vec]
            [io.randomseed.utils.map               :as             map]
            [io.randomseed.utils.map               :refer     [qassoc]]
            [io.randomseed.utils                   :refer         :all]
            [potpuri.core                          :refer [deep-merge]]))

;; Default validation strategy.
;; If `true` then parameters without validators assigned are considered valid.
;; If `false` then unknown parameters are causing validation to fail.

(def ^:const default-default-pass? true)

;; Required parameters checker.
;; If set to `true` then the required params are checked.

(def ^:const default-check-required? false)

;; Explanation messages

(def ^:const default-explain? true)
(def ^:const default-explain-key :validators/reasons)

;; Validation map.

(def ^:const default-validators
  {"login"      vc/valid-email?
   "user/login" vc/valid-email?
   "phone"      vc/valid-phone?
   "url"        vc/valid-url?
   "password"   #"|.{5,256}"
   "session-id" session/sid-match})

(def ^:const default-required
  ["some-antispam-val" "other-antispam-val"])

(def ^:const default-required-some
  ["some-nonempty-token"])

;; Required parameters which must have blank values (nil, empty or only space characters).
;; At least 1 of them must be present.

(def ^:const default-required-blank
  ["some-blank-token"])

(def ^:const default-result-key
  :validators/params-valid?)

(def ^:const default-config-key
  :validators/config)

;; Required generation

(defn gen-required-blank
  "Generates a list of randomly selected, unique names from a vector which should be
  available under the key `:required/blank` of the provided map.

  The optional second argument `items` is a number of items to generate. The optional
  third argument `rng` should be a random number generator object."
  ([{required-blank :required/blank}]
   (if required-blank
     (vec/rand-nths required-blank)))
  ([{required-blank :required/blank} items]
   (if required-blank
     (vec/rand-nths required-blank items)))
  ([{required-blank :required/blank} items rng]
   (if required-blank
     (vec/rand-nths required-blank items rng))))

(defn gen-required-some
  "Generates a list of randomly selected, unique names from a vector which should be
  available under the key `:required/some` of the provided map.

  The optional second argument `items` is a number of items to generate. The optional
  third argument `rng` should be a random number generator object."
  ([{required-some :required/some}]
   (if required-some
     (vec/rand-nths required-some)))
  ([{required-some :required/some} items]
   (if required-some
     (vec/rand-nths required-some items)))
  ([{required-some :required/some} items rng]
   (if required-some
     (vec/rand-nths required-some items rng))))

(defn gen-required
  "Generates a map with keys `:blank`, `:some` and `:any` containing vectors of
  randomly selected, unique names which fit into certain categories, accordingly:
  required keys which must be blank, required keys which must not be blank, required
  keys which may have any value.

  Data for randomization is taken from a map under the key `:required/cat` and from a
  vector under the key `:required/special` of the provided configuration
  argument. This vector is randomized to get a list of `items` values marked in
  validators configuration as special (coming from `:required/blank`,
  `:required/some` or `:required` keys, without its validation function being
  overriden by any manually configured validator). When the list is ready its items
  are grouped by the mentioned categories and returned as a map.

  The optional second argument `items` is a number of items to generate. The optional
  third argument `rng` should be a random number generator object."
  ([{required-cat     :required/cat
     required-special :required/special}
    items rng]
   (if (and required-cat required-special)
     (group-by required-cat (vec/rand-nths required-special items rng))))
  ([{required-cat     :required/cat
     required-special :required/special}
    items]
   (if (and required-cat required-special)
     (group-by required-cat (vec/rand-nths required-special items))))
  ([{required-cat     :required/cat
     required-special :required/special}]
   (if (and required-cat required-special)
     (group-by required-cat (vec/rand-nths required-special)))))

;; Initialization

(def bad-keys
  #{nil (keyword "") (symbol "") "" true false})

(defn- pair-keys
  [coll val]
  (map vector coll (repeat val)))

(defn prep-validators
  "Prepares validators configuration.

  Sequences of values provided under `:required/some`, `:required/blank` and
  `:required` keys are cleaned-up and transformed into strings. Map under the key
  `:validators` is also transformed: its keys are converted into strings and values
  are dereferenced if they are symbols.

  Any parameter in a sequence provided under `:required/some` or `:required/blank`
  key is removed if it has a custom validator assigned in `:validators` map.

  Parameter names which exist in both `:required/some` and `:required/blank`
  sequences are removed from them and moved to a sequence under the `:required/any`
  key. Marking some required parameter to be blank and non-blank at the same time
  causes it to fall into a category where is can have any value to be considered
  valid.

  Parameter names which exist in `:required` sequence and also exist in
  `:required/some` or `:required/blank` sequence are removed from the `:required`
  sequence.

  Parameter names which exist in `:required/some`, `:required/blank` and `:required`
  are removed from `:required/some` and `:required/blank`, and only exist in
  `:required`. Unless their validator was already specified in `:validators` they are
  also added to `:required/any`.

  For each parameter name from `:required/some` a validator is generated (checking if
  a given value is not blank). For each parameter name from `:required/blank` a
  validator is generated (checking if a given value is blank). For each parameter
  name from `:required/any` a validator is generated (accepting any value, including
  blanks).

  Manually assigned validators are expressed as a map under `:validators` key.

  Automatically created validators (for the aforementioned keys: `:required/some`,
  `:required/blank` and `:required/any`) are associated with their parameter names in
  a map under the `:validators/all` key, which also contains manually assigned
  validators.

  During the operation a map is created under the `:required/cat` key. It contains
  all of the required parameter names as keys with validation categories
  assigned (expressed as keywords):

  * `:some`   – parameter is valid if it has a non-blank value
  * `:blank`  – parameter is valid if it has a blank value
  * `:any`    – parameter is valid if it has any value
  * `:custom` – parameter is valid if it passes a custom validation (see `:validators`).

  This function returns a given configuration map with the following keys transformed
  as described earlier:

  Booleans:
    * `:enabled?` (`true` if enabled in configuration with `:enabled?`)
    * `:default-pass?` (`true` if unknown parameters are considered valid)
    * `:check-required?` (`true` if required parameters are to be checked)

  Vectors:
    * `:required/some`    (required parameters having auto-generated validators for non-blanks)
    * `:required/blank`   (required parameters having auto-generated validators for blanks)
    * `:required/any`     (required parameters having auto-generated validators for any values)
    * `:required/special` (all required parameters having auto-generated validators)
    * `:required/user`    (required parameters which were manually configured)
    * `:required`         (all required parameters, including those having auto-generated validators)

  Maps:
    * `:required/cat` (all required parameters with categories assigned)
    * `:validators` (manually configured validators for parameter names)
    * `:validators/all` (manually and automatically configured validators)

  Keywords:
    * `:result-key`      (key identifying validation results in a request map)
    * `:config-key`      (key identifying configuration in a request map)
    * `:required/source` (required parameters source, a keyword, defaults to `:required/user`)
    * `:required/mode`   (required parameters checking mode: :all (default), :one or a number)."
  [{required-mode   :required/mode
    required-source :required/source
    required-some   :required/some
    required-blank  :required/blank
    result-key      :result-key
    config-key      :config-key
    explain-key     :explain-key
    required        :required
    validators      :validators
    enabled?        :enabled?
    check-required? :check-required?
    default-pass?   :default-pass?
    explain?        :explain?
    :or             {enabled?        true
                     required        default-required
                     required-some   default-required-some
                     required-blank  default-required-blank
                     check-required? default-check-required?
                     default-pass?   default-default-pass?
                     explain?        default-explain?
                     validators      default-validators
                     result-key      default-result-key
                     config-key      default-config-key
                     explain-key     default-explain-key}
    :as             config}]
  (let [cr?                 (boolean check-required?)
        default-pass?       (boolean default-pass?)
        enabled?            (boolean enabled?)
        explain?            (boolean explain?)
        validators          (->> validators (map/map-keys some-str) (map/map-vals var/deref-symbol))
        validators          (apply dissoc validators bad-keys)
        validators          (if (seq validators) validators)
        validators-keys     (set (keys validators))
        required-source     (or (some-keyword required-source) :required/user)
        required-mode       (or (some-keyword required-mode)   :all)
        required-user       (map some-str required)
        required-some       (map some-str required-some)
        required-blank      (map some-str required-blank)
        required-user       (set (remove bad-keys required-user))
        required-some       (set (remove bad-keys required-some))
        required-blank      (set (remove bad-keys required-blank))
        required-sb         (set/union required-some required-blank)
        required-nospecial  (set/difference required-user required-sb)
        required-both       (set/intersection required-some required-blank)
        required-blank      (set/difference required-blank required-both validators-keys)
        required-some       (set/difference required-some  required-both validators-keys)
        required-any        (set/union required-nospecial required-both)
        required-any        (set/difference required-any validators-keys)
        required-nospecial  (set/union required-nospecial required-any)
        required-params     (set/union required-user required-sb)
        required-params     (if (seq required-params)     required-params)
        required-user       (if (seq required-user)       required-user)
        required-nospecial  (if (seq required-nospecial)  required-nospecial)
        required-any        (if (seq required-any)        required-any)
        required-some-trxs  (pair-keys required-some (complement str/blank?))
        required-blank-trxs (pair-keys required-blank str/blank?)
        required-trxs       (pair-keys required-any         true)
        required-some-tags  (pair-keys required-some       :some)
        required-blank-tags (pair-keys required-blank     :blank)
        required-any-tags   (pair-keys required-any         :any)
        required-tags       (pair-keys required-params   :custom)
        required-cat        (into {} (concat required-tags
                                             required-some-tags
                                             required-blank-tags
                                             required-any-tags))
        required-cat        (if (seq required-cat) required-cat)
        required-special    (set/union required-some required-blank required-any)
        required-map        (into {} (concat required-some-trxs
                                             required-blank-trxs
                                             required-trxs))
        validators-map      (merge (or required-map {}) (or validators {}))
        validators-map      (if (seq validators-map) validators-map)]
    (assoc config
           :result-key         (keyword result-key)      ;; results identifier
           :config-key         (keyword config-key)      ;; configuration identifier
           :explain-key        (keyword explain-key)     ;; explanation message identifier
           :required/mode      (keyword required-mode)   ;; required checking mode
           :required/source    (keyword required-source) ;; source of the required parameters
           :check-required?    cr?                       ;; check for required params
           :enabled?           enabled?                  ;; validation enabled
           :default-pass?      default-pass?             ;; default strategy for unknown params
           :explain?           explain?                  ;; explanatory messages enabler
           :required/some      (vec required-some)       ;; required params with non-blank content
           :required/blank     (vec required-blank)      ;; required params with blank content
           :required/any       (vec required-any)        ;; required params with any content
           :required/special   (vec required-special)    ;; required params with auto-created validators
           :required/nospecial (vec required-nospecial)  ;; required params without auto-created validators
           :required/user      (vec required-user)       ;; required params which weren't auto-created
           :required           (vec required-params)     ;; all usable, required params
           :required/cat       required-cat              ;; required params by category
           :validators         validators                ;; validators provided in config
           :validators/all     validators-map)))         ;; all validators (manual and auto-created)

(defn expand-validators
  "See amelinium.http.middleware.validators/prep-validators"
  [k config]
  {k (prep-validators config)})

(defn wrap-validators
  "Validators wrapping middleware initializer. Processes the configuration expressed as
  a map which may have the following keys specified:

  Switches:
    * `:enabled?`        (`true` if enabled in configuration with `:enabled?`)
    * `:default-pass?`   (`true` if unknown parameters are considered valid)
    * `:check-required?` (`true` if required parameters are to be checked)

  Sequential collections:
    * `:required/some`      (parameters which should have generated validators for non-blanks)
    * `:required/blank`     (parameters which should have generated validators for blanks)
    * `:required/any`       (parameters which should have generated validators for any values)
    * `:required/user`      (parameters associated with the `:required` key)
    * `:required/special`   (parameters for which validator was automatically generated)
    * `:required/nospecial` (parameters for which validator was not automatically generated)
    * `:required`           (all required parameters)

  Maps:
    * `:validators` (validators assigned to parameter names)

  Keywords:
    * `:result-key` (key identifying validation results in a request map)
    * `:config-key` (key identifying configuration in a request map).

  Validator names should be strings or objects which are convertible to strings.
  Validators can be various objects, see `io.randomseed.utils.validators/Validating`
  protocol.

  The result of calling this function is a map intended to be used with Reitit router
  as a middleware. The handler function itself will add `:validators/config` and
  `:validators/params-valid?` entries to a request map. Names of these identifying
  keys can be changed by setting `:result-key` and/or `:config-key` configuration
  options.

  The presence of validation function can be controlled by setting
  `:validators/disabled?` in a request context to override the defaults. It is a
  switch which disables the validation completely (any parameter will be considered
  valid).

  By default, only manually configured required validators (assigned to the
  `:required/user` configuration key) will be checked for presence! It can be changed
  by setting `:required/check` to:

    * `:required/some`    (parameters which should have generated validators for non-blanks)
    * `:required/blank`   (parameters which should have generated validators for blanks)
    * `:required/any`     (parameters which should have generated validators for any values)
    * `:required/user`    (parameters associated with the `:required` key)
    * `:required/special` (parameters for which validator was automatically generated)
    * `:required/nospecial` (parameters for which validator was not automatically generated)
    * `:required`         (all required parameters)

  See `amelinium.http.middleware.validators/prep-validators` to see the detailed
  logic behind preparing the configuration."
  [k config]
  (log/msg "Installing validators:" k)
  {:name    k
   :config  config
   :compile (fn [data _]
              (let [local-config    (or (get data (:config-key config)) {})
                    config          (deep-merge :into config local-config)
                    {config-key      :config-key
                     result-key      :result-key
                     explain-key     :explain-key
                     enabled?        :enabled?
                     check-required? :check-required?
                     default-pass?   :default-pass?
                     explain?        :explain?
                     validators-all  :validators/all
                     required-mode   :required/mode
                     required-source :required/source}
                    config
                    disabled?       (not enabled?)
                    required-source (or required-source :required/user)
                    required-mode   (or required-mode   :all)
                    required        (if check-required? (get config required-source))]
                (if disabled?
                  (fn [handler]
                    (fn [req]
                      (handler
                       (qassoc req result-key true))))
                  (if explain?
                    (let [explain-fn (case required-mode
                                       :all v/explain-all-required
                                       :one v/explain-required
                                       (if (int? required-mode)
                                         (partial v/explain-required (unchecked-int required-mode))
                                         v/explain-all-required))]
                      (fn [handler]
                        (fn [req]
                          (handler
                           (if (get req :validators/disabled?)
                             (qassoc req config-key config result-key true)
                             (let [reasons (v/explain (get req :form-params)
                                                      validators-all
                                                      default-pass?
                                                      required
                                                      explain-fn)]
                               (qassoc req
                                       config-key config
                                       result-key (nil? (first reasons))
                                       explain-key reasons)))))))
                    (let [check-fn (case required-mode
                                     :all v/has-all-required?
                                     :one v/has-required?
                                     (if (int? required-mode)
                                       (partial v/has-n-required? (unchecked-int required-mode))
                                       v/has-all-required?))]
                      (fn [handler]
                        (fn [req]
                          (handler
                           (let [result (or (get req :validators/disabled?)
                                            (v/validate (get req :form-params)
                                                        validators-all
                                                        default-pass?
                                                        required
                                                        check-fn))]
                             (qassoc req config-key config result-key result))))))))))})

(system/add-expand ::default [k config] (expand-validators k config))
(system/add-init   ::default [k config] (wrap-validators k (if (:required/cat config)
                                                            config
                                                            (prep-validators config))))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
