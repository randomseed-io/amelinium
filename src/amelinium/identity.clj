(ns

    ^{:doc    "Basic identity management for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.identity

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                  :as         set]
            [clojure.string               :as         str]
            [clojure.core.memoize         :as         mem]
            [clj-uuid                     :as        uuid]
            [amelinium.proto.identity     :as           p]
            [amelinium                    :refer     :all]
            [amelinium.logging            :as         log]
            [amelinium.db                 :as          db]
            [phone-number.core            :as       phone]
            [phone-number.util            :as      phutil]
            [io.randomseed.utils.map      :as         map]
            [io.randomseed.utils.map      :refer [qassoc]]
            [io.randomseed.utils          :refer     :all])

  (:import  [java.util UUID]
            [clojure.lang Symbol Keyword PersistentVector IPersistentMap]
            [com.google.i18n.phonenumbers Phonenumber$PhoneNumber]
            [lazy_map.core LazyMapEntry LazyMap]))

;; Standard identity types.

(p/add-type! :email :phone :uid :id)

;; Standard identity string matchers.

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

;; Functions.

(defn type?
  "Returns `true` if the given identity type identifier `t` exists.

  If `acceptable-tag` is given this function will check if the given tag is a parent
  of the given type identifier. To add acceptable type(s) use
  `amelinium.proto.identity/add-acceptable-type`."
  ([t]                (isa? p/type-hierarchy (some-keyword t) ::p/valid))
  ([t acceptable-tag] (isa? p/type-hierarchy (some-keyword t) acceptable-tag)))

(defn guess-type
  "Returns a keyword describing identity type detected by analyzing the given
  value (e.g. `:phone` for a phone number, `:email` for e-mail address, `:id` for
  numeric user ID, `:uid` for UUID). Does not perform full validation, just
  detection."
  (^Keyword [v]
   (if v (p/guess-type v)))
  (^Keyword [v id-type]
   (if v
     (if-some [t (some-keyword id-type)]
       (if (isa? p/type-hierarchy t ::p/valid) t)
       (p/guess-type v)))))

(defn guess-acceptable-type
  "Returns a keyword describing identity type detected by analyzing the given
  value (e.g. `:phone` for a phone number, `:email` for e-mail address, `:id` for
  numeric user ID, `:uid` for UUID). Does not perform full validation, just
  detection.

  The `acceptable-tag` should be a tag type from which the given type is derived
  within hierarchy `amelinium.proto.identity/type-hierarchy`

  To add acceptable type(s) use `amelinium.proto.identity/add-acceptable-type`."
  (^Keyword [v ^Keyword acceptable-tag]
   (if (and v acceptable-tag)
     (if-let [t (p/guess-type v)]
       (if (isa? p/type-hierarchy t acceptable-tag) t))))
  (^Keyword [v id-type ^Keyword acceptable-tag]
   (if (and v acceptable-tag)
     (if-let [t (some-keyword id-type)]
       (if (isa? p/type-hierarchy t acceptable-tag) t)
       (if-let [t (p/guess-type v)]
         (if (isa? p/type-hierarchy t acceptable-tag) t))))))

(defn of-known-type?
  "Returns `true` if the given value `v` is a user identity of the known type.

  If the `acceptable-tag` is present then it should be a tag type from which the
  given type is derived within hierarchy `amelinium.proto.identity/type-hierarchy`.

  To add acceptable type(s) use `amelinium.proto.identity/add-acceptable-type`."
  ([v]
   (if v
     (if-let [t (p/guess-type v)]
       (isa? p/type-hierarchy t ::p/valid)
       false)
     false))
  ([v acceptable-tag]
   (if (and v acceptable-tag)
     (if-let [t (p/guess-type v)]
       (isa? p/type-hierarchy t acceptable-tag)
       false)
     false)))

;; Standard class-based matchers.

(extend-protocol p/Identifiable

  String

  (guess-type ^Boolean [v] (if (not-empty-string? v) (p/type-string-match v)))

  Keyword

  (guess-type ^Boolean [v] (guess-type (some-str v)))

  Symbol

  (guess-type ^Boolean [v] (guess-type (some-str v)))

  Number

  (guess-type ^Boolean [v] (if (pos-int? v) :id))

  Phonenumber$PhoneNumber

  (guess-type ^Boolean [v] :phone)

  UUID

  (guess-type ^Boolean [v] :uid)

  Character

  (guess-type ^Boolean [v] nil)

  Boolean

  (guess-type ^Boolean [v] nil)

  nil

  (guess-type ^Boolean [v] nil)

  Object

  (guess-type ^Boolean [v] (guess-type (some-str v))))

(defmulti prop
  "Gets properties of a user of the given identity `user-identity`. May take optional
  `identity-type` hint which will be preferred if not `false` nor `nil`."
  (fn
    ([db prop-id user-identity]                     (guess-type user-identity))
    ([db prop-id identity-type user-identity]       (guess-type user-identity identity-type))
    ([db prop-id identity-type user-identity & ids] identity-type))
  :hierarchy #'p/type-hierarchy)
