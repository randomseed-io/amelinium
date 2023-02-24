(ns

    ^{:doc    "amelinium service, identity-related protocols and functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.proto.identity

  (:refer-clojure :exclude [type value])

  (:require [amelinium]
            [amelinium.types.identity  :refer      :all]
            [io.randomseed.utils.db    :as          rdb]
            [io.randomseed.utils       :as        utils]
            [io.randomseed.utils       :refer [defdoc!]])

  (:import  [amelinium    Identity]
            [clojure.lang Keyword PersistentVector IPersistentMap]))

(defonce type-hierarchy (make-hierarchy))
(defdoc! type-hierarchy
  "A type hierarchy for identity types expressed as unqualified and qualified
  keywords. Any tag derived from `:amelinium.identity/valid` will be considered
  valid.")

(defonce type-string-matchers [])
(defdoc! type-string-matchers
  "Identity string matchers repository. A vector of functions executed in a sequence
  until one will return anything but `nil`.")

(defonce type-string-match (constantly nil))
(defdoc! type-string-match "Internal function for matching strings on a basis of `type-string-matchers`.")

(defonce valid-types (constantly nil))
(defdoc! valid-types "List of valid types, regenerated each time types are added or deleted.")

(defonce prioritized-types [:id :email :phone :uid])
(defdoc! prioritized-types
  "Prioritized identity types. If they appear on a `valid-types` list, they will be
  placed at the beginning.")

(defn- get-valid-types
  [prio]
  (let [types (filter simple-keyword? (descendants type-hierarchy ::valid))
        prios (set prio)]
    (concat
     (keep #(if (contains? prios %) %) prio)
     (remove #(contains? prios %) types))))

(defn- reduce-hierarchy
  [h types f & args]
  (reduce #(apply f %1 %2 args) h types))

(defn- on-hierarchy-type!
  [f parent t]
  (if parent
    (if-some [t (utils/some-keyword t)]
      (locking #'type-hierarchy
        (alter-var-root #'type-hierarchy f t parent)
        (alter-var-root #'valid-types (constantly (get-valid-types prioritized-types)))
        nil))))

(defn- on-hierarchy-types!
  [f parent types]
  (if parent
    (if-some [types (->> types (map utils/some-keyword) (filter identity) seq)]
      (locking #'type-hierarchy
        (alter-var-root #'type-hierarchy reduce-hierarchy types f parent)
        (alter-var-root #'valid-types (constantly (get-valid-types prioritized-types)))
        nil))))

(defn add-subtype!
  "Adds an identity type `t` as a subtype of a tag `parent`. Multiple types can be
  given. Updates the global identity type hierarchy
  `amelinium.identity.proto/type-hierarchy`."
  ([parent t]        (on-hierarchy-type!  derive parent t))
  ([parent t & more] (on-hierarchy-types! derive parent (cons t more))))

(defn del-subtype!
  "Removes an identity type `t` from being a subtype of a tag `parent`. Multiple types
  can be given. Updates the global identity type hierarchy
  `amelinium.identity.proto/type-hierarchy`."
  ([parent t]        (on-hierarchy-type!  underive parent t))
  ([parent t & more] (on-hierarchy-types! underive parent (cons t more))))

(defn add-type!
  "Adds a new identity type to the global identity type hierarchy
  `amelinium.identity.proto/type-hierarchy` and marks it as valid."
  ([t]        (add-subtype!       :amelinium.identity/valid t))
  ([t & more] (apply add-subtype! :amelinium.identity/valid t more)))

(defn del-type!
  "Removes an identity type from the global identity type hierarchy
  `amelinium.identity.proto/type-hierarchy`."
  ([t]        (del-subtype!       :amelinium.identity/valid t))
  ([t & more] (apply del-subtype! :amelinium.identity/valid t more)))

(defn add-acceptable-type!
  "For the given parent tag `acceptable-tag` (which should be a qualified keyword) and
  an identity type `t`, creates a relation so that the identity type is a descendant
  of the given parent. It also ensures that the parent itself is a descendant of
  `:amelinium.identity/valid` tag.

  Useful when there is a need to accept a limited set of recognized identity
  types. Then the detection function can check whether an identity belongs to a
  parent.

  Makes changes in the global identity type hierarchy
  `amelinium.identity.proto/type-hierarchy`."
  ([acceptable-tag t]
   (add-subtype! acceptable-tag t)
   (if-not (isa? type-hierarchy acceptable-tag :amelinium.identity/valid)
     (add-subtype! :amelinium.identity/valid acceptable-tag)))
  ([acceptable-tag t & more]
   (apply add-subtype! acceptable-tag t more)
   (if-not (isa? type-hierarchy acceptable-tag :amelinium.identity/valid)
     (add-subtype! :amelinium.identity/valid acceptable-tag))))

(defn unaccept-type!
  "Removes identity type `t` from the given parent `acceptable-tag`. Makes changes in
  the global identity type hierarchy `amelinium.identity.proto/type-hierarchy`."
  ([acceptable-tag t]
   (del-subtype! acceptable-tag t))
  ([acceptable-tag t & more]
   (apply del-subtype! acceptable-tag t more)))

(defprotocol Identifiable
  "This protocol allows to extend known identity types."

  (^{:tag Keyword}
   type
   [user-identity]
   "Returns a keyword describing identity type detected by analyzing the given
  value (`:phone` for a phone number, `:email` for e-mail address, `:id` for numeric
  user ID, `:uid` for UUID). Does not perform full validation, just detection.")

  (value
    [user-identity] [user-identity ^Keyword identity-type]
    "Returns a value of the given identity which is an object which represents
  it best.")

  (^{:tag Identity}
   make
   [user-identity] [user-identity ^Keyword identity-type]
   "Creates `amelinium.identity.type.Identity` record by detecting identity type and
  parsing the identity. If `identity-type` is given, parsing for the given identity
  type will be called explicitly.

  For the `Identity` record it simply returns it unless the `identity-type` is given
  and it differs from a value of its `:id-type` field."))

(defn add-type-string-matcher!
  "Adds new identity type string matcher to a global chain. Multiple functions may be
  given."
  ([f]
   (locking #'type-string-matchers
     (alter-var-root #'type-string-matchers conj f)
     (alter-var-root #'type-string-match
                     (constantly
                      (rdb/memoize
                       (apply some-fn type-string-matchers) 4096)))))
  ([f & more]
   (locking #'type-string-matchers
     (doseq [f (cons f more)] (alter-var-root #'type-string-matchers conj f))
     (alter-var-root #'type-string-match
                     (constantly
                      (rdb/memoize
                       (apply some-fn type-string-matchers) 4096))))))

(defn del-type-string-matcher!
  "Deletes identity type string matcher of the given index `n` from a global
  chain. Multiple indexes may be given."
  ([n]
   (locking #'type-string-matchers
     (alter-var-root #'type-string-matchers #(into (subvec % 0 n) (subvec % (inc n))))
     (alter-var-root #'type-string-match
                     (constantly
                      (rdb/memoize
                       (apply some-fn type-string-matchers) 4096)))))
  ([n & more]
   (locking #'type-string-matchers
     (doseq [n (cons n more)]
       (alter-var-root #'type-string-matchers #(into (subvec % 0 n) (subvec % (inc n)))))
     (alter-var-root #'type-string-match
                     (constantly
                      (rdb/memoize
                       (apply some-fn type-string-matchers) 4096))))))
