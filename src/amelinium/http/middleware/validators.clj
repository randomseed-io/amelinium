(ns

    ^{:doc    "amelinium service, validators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.validators

  (:refer-clojure :exclude [uuid random-uuid parse-long])

  (:require [clojure.string                 :as     str]
            [clojure.set                    :as     set]
            [amelinium.logging              :as     log]
            [amelinium.system               :as  system]
            [io.randomseed.utils.validators :as       v]
            [io.randomseed.utils.var        :as     var]
            [io.randomseed.utils.vec        :as     vec]
            [io.randomseed.utils.map        :as     map]
            [io.randomseed.utils            :refer :all]))

;; Default validation strategy.
;; If `true` then parameters without validators assigned are considered valid.
;; If `false` then unknown parameters are causing validation to fail.

(def ^:const default-default-pass? true)

;; Required parameters checker.
;; If set to `true` then the `required-params` is used (at least 1 must be present).

(def ^:const default-check-required? false)

;; Validation map.

(def ^:const default-validators
  {"login"      #"|(^[a-zA-Z0-9_\.+\-]{1,64}@[a-zA-Z0-9\-]{1,64}\.[a-zA-Z0-9\-\.]{1,128}$)"
   "password"   #"|.{5,256}"
   "session-id" #"|[a-f0-9]{30,128}"})

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
   (when required-blank
     (vec/rand-nths required-blank)))
  ([{required-blank :required/blank} items]
   (when required-blank
     (vec/rand-nths required-blank items)))
  ([{required-blank :required/blank} items rng]
   (when required-blank
     (vec/rand-nths required-blank items rng))))

(defn gen-required-some
  "Generates a list of randomly selected, unique names from a vector which should be
  available under the key `:required/some` of the provided map.

  The optional second argument `items` is a number of items to generate. The optional
  third argument `rng` should be a random number generator object."
  ([{required-some :required/some}]
   (when required-some
     (vec/rand-nths required-some)))
  ([{required-some :required/some} items]
   (when required-some
     (vec/rand-nths required-some items)))
  ([{required-some :required/some} items rng]
   (when required-some
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
   (when (and required-cat required-special)
     (group-by required-cat (vec/rand-nths required-special items rng))))
  ([{required-cat     :required/cat
     required-special :required/special}
    items]
   (when (and required-cat required-special)
     (group-by required-cat (vec/rand-nths required-special items))))
  ([{required-cat     :required/cat
     required-special :required/special}]
   (when (and required-cat required-special)
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

  * `:some` – parameter is valid if it has a non-blank value
  * `:blank` – parameter is valid if it has a blank value
  * `:any` – parameter is valid if it has any value
  * `:custom` – parameter is valid if it passes a custom validation (see `:validators`).

  This function returns a given configuration map with the following keys transformed
  as described earlier:

  Booleans:
    * `:enabled?` (`true` if enabled in configuration with `:enabled?`)
    * `:disabled?` (`false` if enabled in configuration with `:enabled?`)
    * `:default-pass?` (`true` if unknown parameters are considered valid)
    * `:check-required?` (`true` if required parameters are to be checked)

  Vectors:
    * `:required/some` (required parameters having auto-generated validators for non-blanks)
    * `:required/blank` (required parameters having auto-generated validators for blanks)
    * `:required/any` (required parameters having auto-generated validators for any values)
    * `:required/special` (all required parameters having auto-generated validators)
    * `:required` (all required parameters, including those having auto-generated validators)

  Maps:
    * `:required/cat` (all required parameters with categories assigned)
    * `:validators` (manually configured validators for parameter names)
    * `:validators/all` (manually and automatically configured validators)

  Keywords:
    * `:result-key` (key identifying validation results in a request map)
    * `:config-key` (key identifying configuration in a request map)."
  [{required-some   :required/some
    required-blank  :required/blank
    result-key      :result-key
    config-key      :config-key
    required        :required
    validators      :validators
    enabled?        :enabled?
    check-required? :check-required?
    default-pass?   :default-pass?
    :or             {enabled?        true
                     required        default-required
                     required-some   default-required-some
                     required-blank  default-required-blank
                     check-required? default-check-required?
                     default-pass?   default-default-pass?
                     validators      default-validators
                     result-key      default-result-key
                     config-key      default-config-key}
    :as             config}]
  (let [cr?                 (boolean check-required?)
        default-pass?       (boolean default-pass?)
        enabled?            (boolean enabled?)
        validators          (->> validators (map/map-keys some-str) (map/map-vals var/deref-symbol))
        validators          (apply dissoc validators bad-keys)
        validators          (when (seq validators) validators)
        validators-keys     (set (keys validators))
        required-user       (map some-str required)
        required-some       (map some-str required-some)
        required-blank      (map some-str required-blank)
        required-user       (set (remove bad-keys required-user))
        required-some       (set (remove bad-keys required-some))
        required-blank      (set (remove bad-keys required-blank))
        required-sb         (set/union required-some required-blank)
        required-user       (set/difference required-user required-sb)
        required-both       (set/intersection required-some required-blank)
        required-blank      (set/difference required-blank required-both validators-keys)
        required-some       (set/difference required-some  required-both validators-keys)
        required-any        (set/union required-user required-both)
        required-any        (set/difference required-any validators-keys)
        required-user       (set/union required-user required-any)
        required-params     (set/union required-user required-sb)
        required-params     (when (seq required-params)  required-params)
        required-user       (when (seq required-user)      required-user)
        required-any        (when (seq required-any)        required-any)
        required-some-trxs  (pair-keys required-some (complement str/blank?))
        required-blank-trxs (pair-keys required-blank str/blank?)
        required-trxs       (pair-keys required-any true)
        required-some-tags  (pair-keys required-some   :some)
        required-blank-tags (pair-keys required-blank  :blank)
        required-any-tags   (pair-keys required-any    :any)
        required-tags       (pair-keys required-params :custom)
        required-cat        (into {} (concat required-tags
                                             required-some-tags
                                             required-blank-tags
                                             required-any-tags))
        required-cat        (when (seq required-cat) required-cat)
        required-special    (set/union required-some required-blank required-any)
        required-map        (into {} (concat required-some-trxs
                                             required-blank-trxs
                                             required-trxs))
        validators-map      (merge (or required-map {}) (or validators {}))
        validators-map      (when (seq validators-map) validators-map)]
    (assoc config
           :result-key       (keyword result-key)   ;; results identifier
           :config-key       (keyword config-key)   ;; configuration identifier
           :check-required?  cr?                    ;; check for required params
           :enabled?         enabled?               ;; validation enabled
           :disabled?        (not enabled?)         ;; validation disabled
           :default-pass?    default-pass?          ;; default strategy for unknown params
           :required/some    (vec required-some)    ;; required params with non-blank content
           :required/blank   (vec required-blank)   ;; required params with blank content
           :required/any     (vec required-any)     ;; required params with any content
           :required/special (vec required-special) ;; required params with auto-created validators
           :required         (vec required-params)  ;; all usable, required params
           :required/cat     required-cat           ;; required params by category
           :validators       validators             ;; validators provided in config
           :validators/all   validators-map)))      ;; all validators (manual and auto-created)

(defn wrap-validators
  "Validators wrapping middleware initializer. Processes the configuration expressed as
  a map which may have the following keys specified:

  Switches:
    * `:enabled?` (`true` if enabled in configuration with `:enabled?`)
    * `:disabled?` (`false` if enabled in configuration with `:enabled?`)
    * `:default-pass?` (`true` if unknown parameters are considered valid)
    * `:check-required?` (`true` if required parameters are to be checked)

  Sequential collections:
    * `:required/some` (required parameters which should have generated validators for non-blanks)
    * `:required/blank` (required parameters which should have generated validators for blanks)
    * `:required/any` (required parameters which should have generated validators for any values)
    * `:required` (required parameters with validators specified elsewhere )

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

  The behavior of validation function can be controlled by setting
  `:validators/disabled?` and/or `:validators/check-required?` in a request context
  to override the defaults. First is a switch which disables the validation
  completely (any parameter will be considered valid). Second is a switch which only
  disables checks for required parameters. Please note that using the second switch
  to disable checking will not remove automatically assigned validators for blank,
  non-blank parameters. It will just remove the requirement of certain, configured
  parameters to be present.

  See `amelinium.http.middleware.validators/prep-validators` to see the detailed
  logic behind preparing the configuration."
  [k {:keys [required required-all validators-all
             config-key result-key
             disabled? default-pass? check-required?]
      :as   config}]
  (log/msg "Installing validators:" k)
  {:name    k
   :compile (fn [_ _]
              (fn [handler]
                (fn [req]
                  (handler
                   (assoc req
                          config-key config
                          result-key
                          (or (get req :validators/disabled? disabled?)
                              (v/validate (get req :form-params)
                                          validators-all
                                          default-pass?
                                          (when (get req :validators/check-required? check-required?)
                                            required-all))))))))})

(system/add-prep  ::default [_ config] (prep-validators config))
(system/add-init  ::default [k config] (wrap-validators k (if (:required/cat config)
                                                            config
                                                            (prep-validators config))))
(system/add-halt! ::default [_ config] nil)
