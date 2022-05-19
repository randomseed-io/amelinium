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
  ["user-val" "bboth" "login"])

(def ^:const default-required-some
  ["both-val" "some-val" "bboth" "password"])

;; Required parameters which must have blank values (nil, empty or only space characters).
;; At least 1 of them must be present.

(def ^:const default-required-blank
  ["both-val" "bboth" "blank-val" "session-id"])

;; Required generation

(defn gen-required-blank
  ([{:keys [required-blank]}]
   (when required-blank
     (vec/rand-nths required-blank)))
  ([{:keys [required-blank]} items]
   (when required-blank
     (vec/rand-nths required-blank items)))
  ([{:keys [required-blank]} items rng]
   (when required-blank
     (vec/rand-nths required-blank items rng))))

(defn gen-required-some
  ([{:keys [required-some]}]
   (when required-some
     (vec/rand-nths required-some)))
  ([{:keys [required-some]} items]
   (when required-some
     (vec/rand-nths required-some items)))
  ([{:keys [required-some]} items rng]
   (when required-some
     (vec/rand-nths required-some items rng))))

(defn gen-required
  "Generates a map with keys `:some`, `:blank` and `:any` containing vectors of
  randomly selected, unique names which fit to certain categories (required keys
  which must be blank, required keys which must not be blank, required keys which may
  have any value)."
  ([{:keys [required-cat required-auto]} items rng]
   (when (and required-cat required-auto)
     (group-by required-cat (vec/rand-nths required-auto items rng))))
  ([{:keys [required-cat required-auto]} items]
   (when (and required-cat required-auto)
     (group-by required-cat (vec/rand-nths required-auto items))))
  ([{:keys [required-cat required-auto]}]
   (when (and required-cat required-auto)
     (group-by required-cat (vec/rand-nths required-auto)))))

;; Initialization

(def bad-keys
  #{nil (keyword "") (symbol "") "" true false})

(defn- pair-keys
  [coll val]
  (map vector coll (repeat val)))

;; when blank or some is set but the key is already in validators map
;; then it should remain as required, not being removed

(defn prep-validators
  [{:keys [required-some required-blank required validators
           enabled? check-required? default-pass?]
    :or   {enabled?        true
           required        default-required
           required-some   default-required-some
           required-blank  default-required-blank
           check-required? default-check-required?
           default-pass?   default-default-pass?
           validators      default-validators}
    :as   config}]
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
        required-any        (set/difference required-user validators-keys)
        required-any        (set/union required-any required-both)
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
           :check-required?  cr?                    ;; check for required params
           :enabled?         enabled?               ;; validation enabled
           :disabled?        (not enabled?)         ;; validation disabled
           :default-pass?    default-pass?          ;; default strategy for unknown params
           :required-some    (vec required-some)    ;; required params with non-blank content
           :required-blank   (vec required-blank)   ;; required params with blank content
           :required-any     (vec required-any)     ;; required params with any content
           :required-special (vec required-special) ;; required params with auto-created validators
           :required         (vec required-user)    ;; required params specified elsewhere
           :required-all     (vec required-params)  ;; all usable, required params
           :required-cat     required-cat           ;; required params by category
           :validators       validators             ;; validators provided in config
           :validators-all   validators-map)))      ;; all validators (manual and auto-created)

(defn wrap-validators
  "Validators wrapping middleware."
  [k {:keys [required required-all validators-all
             disabled? default-pass? check-required?]
      :as   config}]
  (log/msg "Installing validators:" k)
  {:name    k
   :compile (fn [_ _]
              (fn [handler]
                (fn [req]
                  (handler
                   (assoc req
                          :validators/config config
                          :validators/params-valid?
                          (or disabled?
                              (v/validate (get req :form-params)
                                          validators-all
                                          default-pass?
                                          (when check-required? required-all))))))))})

(system/add-prep  ::default [_ config] (prep-validators config))
(system/add-init  ::default [k config] (wrap-validators k (prep-validators config)))
(system/add-halt! ::default [_ config] nil)
