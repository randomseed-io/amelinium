(ns

    ^{:doc    "Basic identity management for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.identity

  (:refer-clojure :exclude [parse-long uuid random-uuid type value])

  (:require [clojure.set                  :as         set]
            [clojure.string               :as         str]
            [clj-uuid                     :as        uuid]
            [amelinium.proto.identity     :as           p]
            [amelinium.types.identity     :refer     :all]
            [amelinium                    :refer     :all]
            [amelinium.logging            :as         log]
            [phone-number.core            :as       phone]
            [phone-number.util            :as      phutil]
            [io.randomseed.utils.db        :as        rdb]
            [io.randomseed.utils.map      :as         map]
            [io.randomseed.utils.map      :refer [qassoc]]
            [io.randomseed.utils          :refer     :all])

  (:import  (java.util                    UUID)
            (clojure.lang                 Symbol
                                          Keyword
                                          PersistentVector
                                          IPersistentMap)
            (amelinium                    Identity)
            (amelinium.proto.identity     Identifiable)
            (com.google.i18n.phonenumbers Phonenumber$PhoneNumber)))

;; Standard identity types

(p/add-type! :email :phone :uid :id)

;; Standard identity string matchers

(defn id-phone-string
  "Returns `true` if the given string looks like a phone number."
  ^Keyword [^String v]
  (if (and (= \+ (.charAt v 0))
           (> (.length v) 4))
    :phone))

(defn id-email-string
  "Returns `true` if the given string looks like an e-mail address."
  ^Keyword [^String v]
  (if (and (> (.length v) 2)
           (pos-int? (str/index-of v \@ 1)))
    :email))

(defn id-uid-string
  "Returns `true` if the given string looks like a UID (UUID)."
  ^Keyword [^String v]
  (if (uuid/uuid-string? v) :uid))

(defn id-id-string
  "Returns `true` if the given string looks like a user ID."
  ^Keyword [^String v]
  (if (pos-int? (safe-parse-long v)) :id))

(p/add-type-string-matcher! id-phone-string
                            id-email-string
                            id-uid-string
                            id-id-string)

;; Standard identity parsers

(defn preparse-email
  "Parses e-mail by doing basic checks and transforming it to a string."
  ^String [v]
  (if-some [^String v (some-str v)]
    (let [l (unchecked-int (.length v))]
      (if (> l 2)
        (if-some [idx ^long (str/index-of v \@ 1)]
          (if (and (> (unchecked-dec-int l) idx 0))
            (str (subs v 0 idx) (str/lower-case (subs v idx l)))))))))

(defn parse-email
  "Parses e-mail by doing basic checks and transforming it to an Identity object."
  ^Identity [v]
  (if-some [^String v (preparse-email v)]
    (Identity. :email v)))

(defn preparse-id
  "Parses user ID by doing basic checks and transforming it to a long number."
  ^Long [v]
  (if-let [v (safe-parse-long v)]
    (if (pos-int? v)
      v)))

(defn parse-id
  "Parses user ID by doing basic checks and transforming it to an Identity record."
  ^Identity [v]
  (if-some [^Long v (preparse-id v)]
    (Identity. :id v)))

(defn preparse-phone
  "Tries to create an object representing phone number. Returns `nil` if the input
  cannot be parsed."
  ^Phonenumber$PhoneNumber [v]
  (phutil/try-parse
   (phone/number-optraw v)))

(defn parse-phone
  "Tries to interpret `v` as a phone number and returns an Identity record."
  ^Identity [v]
  (if-some [v (preparse-phone v)]
    (Identity. :phone v)))

(defn preparse-uid
  "Tries to create UUID. Returns `nil` if the input cannot be converted to it."
  ^UUID [v]
  (if (uuid? v) v
      (if (and v (uuid/uuidable? v))
        (uuid/as-uuid v))))

(defn parse-uid
  "Tries to interpret `v` as a UUID and returns an Identity record."
  ^Identity [v]
  (if-some [u (preparse-uid v)]
    (Identity. :uid u)))

;; Public functions

(defn type?
  "Returns `true` if the given identity type identifier `t` exists.

  If `acceptable-tag` is given this function will check if the given tag is a parent
  of the given type identifier. To add acceptable type(s) use
  `amelinium.proto.identity/add-acceptable-type`."
  ([t]
   (isa? p/type-hierarchy (some-keyword t) ::valid))
  ([t ^Keyword acceptable-tag]
   (and acceptable-tag (isa? p/type-hierarchy (some-keyword t) acceptable-tag))))

(defn check-type
  "Returns a keyword for the given identity type `identity-type` if it is a valid
  identity. Otherwise it returns `nil`. To add acceptable type(s) use
  `amelinium.proto.identity/add-acceptable-type`.

  If acceptable parent tag `acceptable-tag` is given, it must be a parent of the
  given identity type tag."
  (^Keyword [id-type]
   (if-some [t (some-keyword id-type)]
     (if (isa? p/type-hierarchy t ::valid) t)))
  (^Keyword [id-type acceptable-tag]
   (if-some [t (some-keyword id-type)]
     (if (and acceptable-tag (isa? p/type-hierarchy t acceptable-tag)) t))))

(defn type
  "Returns a keyword describing identity type detected by analyzing the given user
  identity `user-identity` and optional identity type `identity-type` given
  explicitly (e.g. `:phone` for a phone number, `:email` for e-mail address, `:id`
  for numeric user ID, `:uid` for UUID). If the type is not given, analysis of the
  given identity will be performed.

  Does not perform full validation of identities, just detection.

  Value of the given identity type `identity-type` must match the identity type, and
  not be `nil` nor `false`. If any of these happens, `nil` will be returned."
  (^Keyword [user-identity]
   (if user-identity (p/type user-identity)))
  (^Keyword [^Keyword identity-type user-identity]
   (if user-identity
     (if-some [t (some-keyword identity-type)]
       (if (identical? t (p/type user-identity)) t)))))

(defn type-opt
  "Returns a keyword describing identity type detected by analyzing the given user
  identity `user-identity` and optional identity type `identity-type` given
  explicitly (e.g. `:phone` for a phone number, `:email` for e-mail address, `:id`
  for numeric user ID, `:uid` for UUID). If the type is not given or it is `nil`,
  analysis of the given identity will be performed.

  Does not perform full validation of identities, just detection.

  Value of the given identity type `identity-type` must match the identity type. If
  it does not, `nil` will be returned."
  (^Keyword [user-identity]
   (if user-identity (p/type user-identity)))
  (^Keyword [^Keyword identity-type user-identity]
   (if user-identity
     (if-some [t (some-keyword identity-type)]
       (if (identical? t (p/type user-identity)) t)
       (p/type user-identity)))))

(defn acceptable-type
  "Returns a keyword describing identity type detected by analyzing the given user
  identity `user-identity` and optional identity type `identity-type` given
  explicitly (e.g. `:phone` for a phone number, `:email` for e-mail address, `:id`
  for numeric user ID, `:uid` for UUID). If the type is not given, or if it is `nil`
  or `false`, analysis of the given identity will be performed.

  Does not perform full validation of identities, just detection.

  The given identity type must always be valid (registered with
  `amelinium.proto.identity/add-type!`) and the identity must not be `nil` nor
  `false`. If any of these happens, `nil` will be returned.

  The `acceptable-tag` should be a tag type from which the given type is derived
  within the hierarchy `amelinium.proto.identity/type-hierarchy`.

  To add acceptable type(s) use `amelinium.proto.identity/add-acceptable-type`."
  (^Keyword [user-identity ^Keyword acceptable-tag]
   (if (and user-identity acceptable-tag)
     (if-let [t (p/type user-identity)]
       (if (and acceptable-tag (isa? p/type-hierarchy t acceptable-tag)) t))))
  (^Keyword [user-identity identity-type ^Keyword acceptable-tag]
   (if (and user-identity acceptable-tag)
     (if-let [t (some-keyword identity-type)]
       (if (and acceptable-tag (isa? p/type-hierarchy t acceptable-tag)) t)
       (if-let [t (p/type user-identity)]
         (if (and acceptable-tag (isa? p/type-hierarchy t acceptable-tag)) t))))))

(defn of-known-type?
  "Returns `true` if the given value `v` is a user identity of the known type.

  If the `acceptable-tag` is present then it should be a tag type from which the
  given type is derived within hierarchy `amelinium.proto.identity/type-hierarchy`.

  To add acceptable type(s) use `amelinium.proto.identity/add-acceptable-type`."
  ([v]
   (if v
     (if-let [t (p/type v)]
       (isa? p/type-hierarchy t ::valid)
       false)
     false))
  ([v acceptable-tag]
   (if (and v acceptable-tag)
     (if-let [t (p/type v)]
       (isa? p/type-hierarchy t acceptable-tag)
       false)
     false)))

(defn value
  "Returns a value of the given identity type `user-identity` and optional identity
  type `identity-type`. If the type is not given, analysis of the given identity will
  be performed to establish the type first. If the type is given, it must be a valid
  type, otherwise `nil` will be returned."
  (^Keyword [user-identity]
   (if user-identity (p/value user-identity)))
  (^Keyword [^Keyword identity-type user-identity]
   (if user-identity
     (if-some [t (some-keyword identity-type)]
       (p/value user-identity t)))))

;; Parsing

(defmulti parser
  "Takes an identity type expressed as keyword and returns a parser suitable for that
  identity type. The parser function takes 1 argument and converts the given value to
  identity record (of type `amelinium.Identity`).

  Caution: The identity type must be a keyword (it will not be coerced)."
  (fn ^Keyword [identity-type] identity-type)
  :hierarchy #'p/type-hierarchy)

(defn- parse-single
  ([^Keyword identity-type user-identity]
   (if user-identity ((parser identity-type) user-identity)))
  ([user-identity]
   (if user-identity ((parser (p/type user-identity)) user-identity))))

(def ^{:tag      Identity
       :arglists '(^Identity [user-identity]
                   ^Identity [^Keyword identity-type user-identity])
       :see-also ["of"]}
  parse
  "Parses the given identity `user-identity` with optional `identity-type`
  predefined. Returns an identity record of type `amelinium.Identity`. Memoized proxy
  for parsing strings and other non-native data. Do not use it directly, use `of`
  instead.

  Caution: The identity type must be a keyword (it will not be coerced)."
  (rdb/memoize parse-single 4096))

(defn- parse-indexed
  "(Premature optimization is the root of all evil.)"
  [[^Keyword t indexed-ids]]
  (let [[indexes ids] (reduce
                       (fn [[indexes ids] [i v]] [(cons i indexes) (cons v ids)])
                       [() ()] indexed-ids)]
    (map vector indexes (map (parser t) ids))))

(defn- parse-multi
  "(Premature optimization is the root of all evil.)"
  ([^Keyword identity-type user-identities]
   (map (parser identity-type) user-identities))
  ([user-identities]
   (->> (map-indexed vector user-identities)
        (group-by #(p/type (nth % 1)))
        (mapcat parse-indexed)
        (sort-by #(nth % 0))
        (map #(nth % 1)))))

(defmethod parser :email   [_] parse-email)
(defmethod parser :phone   [_] parse-phone)
(defmethod parser :uid     [_] parse-uid)
(defmethod parser :id      [_] parse-id)
(defmethod parser ::any    [_] parse)
(defmethod parser :default [_] (constantly nil))

(def identity-map-keys
  "Commonly known map keys which may contain user identity."
  [[:identity       ::any]
   [:user/identity  ::any]
   [:id               :id]
   [:user-id          :id]
   [:phone         :phone]
   [:email         :email]
   [:uid             :uid]
   [:login          ::any]
   [:user/id          :id]
   [:user/phone    :phone]
   [:user/email    :email]
   [:user/uid        :uid]
   [:user/login     ::any]
   [:id             ::any]
   [:user-identity  ::any]
   [:user           ::any]])

(def identity-map-keys-by-type
  "Commonly known map keys which may contain user identity grouped by identity types."
  (map/map-vals-by-k
   #(->> identity-map-keys
         (filter (comp #{% ::any} second))
         (map first)
         (distinct)
         (vec))
   (dissoc (group-by second identity-map-keys) ::any)))

(defn parse-map
  "Tries to extract identity from a map `m` by searching for commonly known identity
  keys.

  Optional identity type `identity-type` will be used to constrain the conversion. If
  a known key is found but its associated value cannot be converted to `Identity`
  object, process continues and other keys are tried.

  Uses `identity-map-keys` when no identity type is given, or it is set to
  `:amelinium.identity/any`.

  Uses `identity-map-keys-by-type` when an identity type is given to select a group
  of keys to be tried out. When the group is not found for the given identity type,
  `nil` is returned."
  ([m]
   (if (seq m)
     (some (fn [[k t]]
             (if-let [i (get m k)]
               (if (and (map? i) (not (record? i))) nil (p/make i t))))
           identity-map-keys)))
  ([identity-type m]
   (if-some [ks (get identity-map-keys-by-type identity-type)]
     (if (seq m)
       (some (fn [k]
               (if-let [i (get m k)]
                 (if (and (map? i) (not (record? i))) nil (p/make i identity-type))))
             ks))
     (if (identical? ::any identity-type)
       (parse-map m)))))

;; Creating identities

(defn of
  "For the given user identity `user-identity` tries to parse the identity and
  return an `amelinium.Identity` record containing a detected identity type and
  identity value in a form it expresses it best. If the identity type cannot be
  established and it was not given, `nil` is returned.

  Optional identity type `identity-type` (which must be a keyword) may be given as a
  first argument. It is used to pick the right parser (if parsing is needed) and to
  reject wrong identity type (if `amelinium.Identity` record is given). If there is a
  need to give list of keywords as user identities (including the first), and not to
  give an identity type, then first argument's value needs to be converted to a
  string, or `of-value` should be used as an alternative.

  If multiple identities are given it will return a sequence of these identities
  parsed with parsing functions chosen for detected identity types. If an identity
  type is cannot be detected and/or is not valid, `nil` value will be inserted into
  the corresponding location of generated output sequence."
  {:see-also ["of-value" "of-type" "of-seq"]
   :arglists '([user-identity]
               [identity-type user-identity]
               [identity-type user-identity & user-identities]
               [user-identity & user-identities])}
  (^Identity [user-identity]
   (p/make user-identity))
  ([identity-type user-identity]
   (if (keyword? identity-type)
     (p/make user-identity identity-type)
     (map p/make (cons identity-type (cons user-identity nil)))))
  ([identity-type user-identity & ids]
   (if (keyword? identity-type)
     (map #(p/make % identity-type) (cons user-identity ids))
     (map p/make (cons identity-type (cons user-identity ids))))))

(defn of-value
  "For the given user identity `user-identity` tries to parse the identity and
  return an `amelinium.Identity` record containing a detected identity type and
  identity value in a form it expresses it best. If the identity type cannot be
  established and it was not given, `nil` is returned.

  If multiple identities are given it will return a sequence of these identities
  parsed with parsing functions chosen for detected identity types. If an identity
  type is cannot be detected and/or is not valid, `nil` value will be inserted into
  the corresponding location of generated output sequence."
  {:see-also ["of" "of-type" "of-seq"]}
  (^Identity [user-identity]
   (p/make user-identity))
  ([user-identity & ids]
   (map p/make (cons user-identity ids))))

(defn of-type
  "For the given user identity `user-identity` and identity type `identity-type` it
  tries to parse the identity and return an `amelinium.Identity` record containing an
  identity type and identity value in a form it expresses it best. If the identity
  type is `nil` then `nil` is returned.

  If multiple identities are given it will return a sequence of these identities
  parsed with parsing functions chosen for the given identity type. If the identity
  type is not valid or the parsing cannot be applied for an input value, `nil` will
  be inserted into corresponding location of the output sequence."
  {:see-also ["of" "of-value" "of-seq"]}
  (^Identity [identity-type user-identity]
   (p/make user-identity (some-keyword identity-type)))
  ([identity-type user-identity & ids]
   (let [identity-type (some-keyword identity-type)]
     (map #(p/make % identity-type) (cons user-identity ids)))))

(defn opt-type
  "For the given user identity `user-identity` and identity type `identity-type` it
  tries to parse the identity and return an `amelinium.Identity` record containing an
  identity type and identity value in a form it expresses it best. If the identity
  type is `nil` or empty then any identity type is accepted.

  If multiple identities are given it will return a sequence of these identities
  parsed with parsing functions chosen for the given identity type. If the identity
  type is not valid or the parsing cannot be applied for an input value, `nil` will
  be inserted into corresponding location of the output sequence."
  {:see-also ["of" "of-value" "of-seq"]}
  (^Identity [identity-type user-identity]
   (p/make user-identity  (or (some-keyword identity-type) ::any)))
  ([identity-type user-identity & ids]
   (let [identity-type (some-keyword identity-type)]
     (if identity-type
       (map #(p/make % identity-type) (cons user-identity ids))
       (map p/make (cons user-identity ids))))))

(defn of-seq
  "For the given user identities `user-identities` tries to parse each identity and
  return an `amelinium.Identity` record containing a detected identity type and
  identity value in a form it expresses it best. If the given identity type was given
  as an optional `identity-type` argument, it will be assumed as the expected type
  for all input values.

  If an identity type was not given but it cannot be detected and/or is not valid,
  `nil` value will be inserted into the corresponding location of generated output
  sequence. Same with the given identity type which is not applicable to a particular
  value or invalid."
  {:see-also ["of" "of-value" "of-type"]}
  ([user-identities]
   (map p/make user-identities))
  ([identity-type user-identities]
   (if-some [identity-type (some-keyword identity-type)]
     (map #(p/make % identity-type) user-identities))))

(defn some-seq
  "Tries to coerce identities to `amelinium.Identity` objects and filters out those who
  could not be coerced."
  ([user-identities]
   (->> (of-seq user-identities) (filter identity) seq))
  ([^Keyword identity-type user-identities]
   (->> (of-seq identity-type user-identities) (filter identity) seq)))

;; Standard class-based matchers and getters

(extend-protocol p/Identifiable

  Identity

  (literal? ^Boolean [v] false)

  (type ^Keyword [v]
    (.id-type ^Identity v))

  (value
    ([v] (.value ^Identity v))
    ([v ^Keyword identity-type]
     (if (or (identical? ::any identity-type)
             (identical? identity-type (.id-type ^Identity v)))
       (.value ^Identity v))))

  (make
    (^Identity [v] v)
    (^Identity [v ^Keyword identity-type]
     (if (or (identical? ::any identity-type)
             (identical? identity-type (.id-type ^Identity v))) v)))

  String

  (literal? ^Boolean [v] true)

  (type ^Keyword [v]
    (if (not-empty-string? v)
      (p/type-string-match v)))

  (value
    ([v] (p/value (p/make v)))
    ([v ^Keyword identity-type]
     (p/value (p/make v identity-type))))

  (make
    (^Identity [v]
     (if (not-empty-string? v) (parse v)))
    (^Identity [v ^Keyword identity-type]
     (if (not-empty-string? v) (parse identity-type v))))

  clojure.lang.IPersistentMap

  (literal? ^Boolean [v] false)

  (type ^Keyword [v]
    (p/type (parse-map v)))

  (value
    ([v] (p/value (parse-map v)))
    ([v ^Keyword identity-type]
     (p/value (parse-map identity-type v))))

  (make
    (^Identity [v]
     (parse-map v))
    (^Identity [v ^Keyword identity-type]
     (parse-map identity-type v)))

  Keyword

  (literal? ^Boolean [v] true)

  (type ^Keyword [v]
    (p/type (some-str v)))

  (value
    ([v] (p/value (some-str v)))
    ([v ^Keyword identity-type]
     (p/value (some-str v) identity-type)))

  (make
    (^Identity [v]
     (p/make (some-str v)))
    (^Identity [v ^Keyword identity-type]
     (p/make (some-str v) identity-type)))

  Symbol

  (literal? ^Boolean [v] false)

  (type ^Keyword [v]
    (p/type (some-str v)))

  (value
    ([v] (p/value (some-str v)))
    ([v ^Keyword identity-type]
     (p/value (some-str v) identity-type)))

  (make
    (^Identity [v]
     (p/make (some-str v)))
    (^Identity [v ^Keyword identity-type]
     (p/make (some-str v) identity-type)))

  Number

  (literal? ^Boolean [v] true)

  (type ^Keyword [v]
    (if (pos-int? v) :id))

  (value
    (^Long [v]
     (if (pos-int? v) (long v)))
    (^Long [v ^Keyword identity-type]
     (if (and (pos-int? v)
              (or (identical? :id identity-type)
                  (identical? ::any identity-type)))
       (long v))))

  (make
    (^Identity [v]
     (if (pos-int? v)
       (Identity. :id (long v))))
    (^Identity [v ^Keyword identity-type]
     (if (and (pos-int? v)
              (or (identical? :id identity-type)
                  (identical? ::any identity-type)))
       (Identity. :id (long v)))))

  Phonenumber$PhoneNumber

  (literal? ^Boolean [v] true)

  (type ^Keyword [v] :phone)

  (value
    (^Phonenumber$PhoneNumber [v] v)
    (^Phonenumber$PhoneNumber [v ^Keyword identity-type]
     (if (or (identical? :phone identity-type)
             (identical? ::any identity-type)) v)))

  (make
    (^Identity [v] (Identity. :phone v))
    (^Identity [v ^Keyword identity-type]
     (if (or (identical? :phone identity-type)
             (identical? ::any identity-type))
       (Identity. :phone v))))

  UUID

  (literal? ^Boolean [v] true)

  (type ^Keyword [v] :uid)

  (value
    (^UUID [v] v)
    (^UUID [v ^Keyword identity-type]
     (if (or (identical? :uid identity-type)
             (identical? ::any identity-type)) v)))

  (make
    (^Identity [v] (Identity. :uid v))
    (^Identity [v ^Keyword identity-type]
     (if (or (identical? :uid identity-type)
             (identical? ::any identity-type))
       (Identity. :uid v))))

  Character

  (literal? ^Boolean [v] true)

  (type [v] nil)

  (value
    ([v] nil)
    ([v identity-type] nil))

  (make
    ([v] nil)
    ([v identity-type] nil))

  Boolean

  (literal? ^Boolean [v] true)

  (type [v] nil)

  (value
    ([v] nil)
    ([v identity-type] nil))

  (make
    ([v] nil)
    ([v identity-type] nil))

  nil

  (literal? ^Boolean [v] true)

  (type [v] nil)

  (value
    ([v] nil)
    ([v identity-type] nil))

  (make
    ([v] nil)
    ([v identity-type] nil))

  Object

  (literal? ^Boolean [v] false))

;; DB conversions

(defmulti to-db*
  "For the given user identity `user-identity` which must be of type
  `amelinium.Identity`, tries to express the identity in a database suitable format.

  If the given value cannot be used as valid identity, `nil` is returned.

  If the `identity-type` is given, it must be a keyword and it should be a valid
  identity type. It instructs the function to treat the given identity as of this
  type.

  This is internal multimethod which does not perform conversions or checks. Use
  `to-db` or `->db` instead."
  {:arglists '([^Identifiable user-identity]
               [identity-type ^Identifiable user-identity])
   :see-also ["to-db" "->db"]}
  (fn
    (^Keyword [^Identity user-identity] (p/type user-identity))
    (^Keyword [^Keyword identity-type ^Identity user-identity] identity-type))
  :hierarchy #'p/type-hierarchy)

(defn to-db
  "For the given user identity `user-identity` tries to express the identity in a
  database suitable format.

  Uses `to-db*` multimethod to perform `user-identity` transformation on a basis of
  its identity type.

  If the given identity is not a kind of `amelinium.Identity` record it will be
  converted to it first. If the given value cannot be used as valid identity, `nil`
  is returned.

  If the `identity-type` is given, it should be a valid identity type. It instructs
  the function to treat the given identity as of this type during pre-conversion. If
  the identity is already an `amelinium.Identity` record but its type is different,
  `nil` will be returned.

  If possible, use `->db` macro instead to get some compile-time optimizations."
  {:see-also ["->db" "to-db*"]}
  ([^Identifiable user-identity]
   (to-db* (p/make user-identity)))
  ([identity-type ^Identifiable user-identity]
   (to-db* (p/make user-identity (some-keyword identity-type)))))

(defmacro ->db
  "For the given user identity `user-identity` and optional identity type
  `identity-type` tries to express the given identity's value in a database suitable
  format.

  Uses `to-db*` multimethod to perform `user-identity` transformation on a basis of
  its identity type.

  If the given identity is not a kind of `amelinium.Identity` record, it will be
  converted to it first. If the given value cannot be used as valid identity, `nil`
  is returned.

  If the `identity-type` is given, it should be a valid identity type. It instructs
  the function to treat the given identity as of this type during pre-conversion. If
  the identity is already an `amelinium.Identity` record but its type is different,
  `nil` will be returned.

  If the `identity-type` is given as literal keyword, string or `nil`, then a
  specific conversion function will be obtained at compile-time.

  If `identity-type` is an acceptable literal (or not given at all) and
  `user-identity` expression is a value for which the function
  `amelinium.proto.identity/literal?` returns `true` then the conversion is done
  immediately, and its result replaces the macro call at compile-time.

  However, if the immediate conversion result is `nil`, or it is not a value for
  which the function `amelinium.proto.identity/literal?` returns `true`, an
  expression with call to `to-db` or `to-db*` will be generated as fallback to
  perform the conversion at run-time."
  {:see-also ["->str" "of"]}
  ([user-identity]
   (if (nil? user-identity)
     nil
     (if-some [r (if (p/literal? user-identity)
                   (let [r (to-db `~user-identity)]
                     (if (p/literal? r) r)))]
       r
       `(to-db ~user-identity))))
  ([identity-type user-identity]
   (if (nil? user-identity)
     nil
     (let [identity-type (if (or (keyword? identity-type) (string? identity-type))
                           (some-keyword identity-type)
                           identity-type)]
       (if-some [r (if (and (or (nil? identity-type) (keyword? identity-type))
                            (p/literal? user-identity))
                     (let [r (to-db `~identity-type `~user-identity)]
                       (if (p/literal? r) r)))]
         r
         (or (if (nil? identity-type) (keyword? identity-type)
                 (if (identical? :amelinium.identity/any identity-type)
                   (#'->db &form &env user-identity)
                   (if-some [f (get (methods to-db*) `~identity-type)]
                     `(~f (p/make ~user-identity ~identity-type)))))
             `(to-db ~identity-type ~user-identity)))))))

(defmethod to-db* :email
  (^String [^Identity user-identity]
   (p/value user-identity))
  (^String [^Keyword identity-type ^Identity user-identity]
   (p/value user-identity identity-type)))

(defmethod to-db* :phone
  (^Phonenumber$PhoneNumber [^Identity user-identity]
   (phone/format (p/value user-identity) nil
                 :phone-number.format/e164))
  (^Phonenumber$PhoneNumber [^Keyword identity-type ^Identity user-identity]
   (phone/format (p/value user-identity identity-type) nil
                 :phone-number.format/e164)))

(defmethod to-db* :uid
  (^UUID [^Identity user-identity]
   (p/value user-identity))
  (^UUID [^Keyword identity-type ^Identity user-identity]
   (p/value user-identity identity-type)))

(defmethod to-db* :id
  (^Long [^Identity user-identity]
   (p/value user-identity))
  (^Long [^Keyword identity-type ^Identity user-identity]
   (p/value user-identity identity-type)))

(defmethod to-db* :default
  (^Long [^Identity user-identity]
   (some-str (p/value user-identity)))
  (^Long [^Keyword identity-type ^Identity user-identity]
   (some-str (p/value user-identity identity-type))))

;; String conversions

(defmulti to-str*
  "Takes a user identity `user-identity` expressed as `Identity` record, and converts
  it to a string.

  If the `identity-type` is given, it should be a valid identity type expressed with
  a keyword. It instructs the function to treat the given identity as of this
  type. If the identity type is different than in the given object, `nil` is
  returned.

  This is internal multimethod which does not perform conversions or checks. Use
  `to-str` function or `->str` macro instead."
  {:arglists '(^String [^Identity user-identity]
               ^String [^Keyword identity-type ^Identity user-identity])
   :see-also ["to-str" "->str"]}
  (fn
    (^Keyword [^Identity user-identity] (p/type user-identity))
    (^Keyword [^Keyword identity-type ^Identity user-identity] identity-type))
  :hierarchy #'p/type-hierarchy)

(defn to-str
  "Takes a user identity `user-identity` and optional identity type `identity-type`,
  and converts it to a string.

  If the given identity is not a kind of `amelinium.Identity` record it will be
  converted to it first. If the given value cannot be used as valid identity, `nil`
  is returned.

  If the `identity-type` is given, it should be a valid identity type. It instructs
  the function to treat the given identity as of this type. If the identity is an
  `amelinium.Identity` record but its type is different, `nil` will be returned."
  {:see-also ["->str" "->db" "of"]}
  (^String [^Identifiable user-identity]
   (to-str* (p/make user-identity)))
  (^String [identity-type ^Identifiable user-identity]
   (let [t (some-keyword identity-type)] (to-str* t (p/make user-identity t)))))

(defmacro ->str
  "For the given user identity `user-identity` and optional identity type
  `identity-type` tries to express the given identity's value as a string.

  Uses `to-str*` multimethod to perform `user-identity` transformation on a basis of
  its identity type.

  If the given identity is not a kind of `amelinium.Identity` record, it will be
  converted to it first. If the given value cannot be used as valid identity, `nil`
  is returned.

  If the `identity-type` is given, it should be a valid identity type. It instructs
  the function to treat the given identity as of this type during pre-conversion. If
  the identity is already an `amelinium.Identity` record but its type is different,
  `nil` will be returned.

  If the `identity-type` is given as literal keyword, string or `nil`, then a
  specific conversion function will be obtained at compile-time.

  If `identity-type` is an acceptable literal (or not given at all) and
  `user-identity` expression is a value for which the function
  `amelinium.proto.identity/literal?` returns `true` then the conversion is done
  immediately, and its result replaces the macro call at compile-time.

  However, if the immediate conversion result is `nil`, or it is not a value for
  which the function `amelinium.proto.identity/literal?` returns `true`, an
  expression with call to `to-str` or `to-str*` will be generated as fallback to
  perform the conversion at run-time."
  {:see-also ["->str" "of"]}
  ([user-identity]
   (if (nil? user-identity)
     nil
     (if-some [r (if (p/literal? user-identity)
                   (let [r (to-str `~user-identity)]
                     (if (p/literal? r) r)))]
       r
       `(to-str ~user-identity))))
  ([identity-type user-identity]
   (if (nil? user-identity)
     nil
     (let [identity-type (if (or (keyword? identity-type) (string? identity-type))
                           (some-keyword identity-type)
                           identity-type)]
       (if-some [r (if (and (or (nil? identity-type) (keyword? identity-type))
                            (p/literal? user-identity))
                     (let [r (to-str `~identity-type `~user-identity)]
                       (if (p/literal? r) r)))]
         r
         (or (if (nil? identity-type) (keyword? identity-type)
                 (if (identical? :amelinium.identity/any identity-type)
                   (#'->str &form &env user-identity)
                   (if-some [f (get (methods to-str*) `~identity-type)]
                     `(~f (p/make ~user-identity ~identity-type)))))
             `(to-str ~identity-type ~user-identity)))))))

(defmethod to-str* :email
  (^String [^Identifiable user-identity]
   (some-str (p/value user-identity)))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (some-str (p/value user-identity identity-type))))

(defmethod to-str* :uid
  (^String [^Identifiable user-identity]
   (some-str (p/value user-identity)))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (some-str (p/value user-identity identity-type))))

(defmethod to-str* :id
  (^String [^Identifiable user-identity]
   (some-str (p/value user-identity)))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (some-str (p/value user-identity identity-type))))

(defmethod to-str* :phone
  (^String [^Identifiable user-identity]
   (phone/format (p/value user-identity) nil :phone-number.format/e164))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (phone/format (p/value user-identity identity-type) nil :phone-number.format/e164)))

(defmethod to-str* :default
  (^String [^Identifiable user-identity]
   (some-str (p/value user-identity)))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (some-str (p/value user-identity identity-type))))
