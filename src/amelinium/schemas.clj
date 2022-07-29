(ns

    ^{:doc    "Schemas of amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.schemas

  (:require [malli.core                            :as           m]
            [malli.util                            :as          mu]
            [malli.error                           :as        merr]
            [malli.generator                       :as        mgen]
            [malli.transform                       :as      mtform]
            [malli.json-schema                     :as json-schema]
            [malli.registry                        :as   mregistry]
            [clojure.string                        :as         str]
            [clojure.test.check.generators         :as         gen]
            [phone-number.core                     :as       phone]
            [phone-number.util                     :as      phutil]
            [io.randomseed.utils.validators.common :as          vc]
            [io.randomseed.utils                   :as       utils]
            [amelinium.locale                      :as      locale])

  (:import [java.time Instant]
           [java.util Date UUID]))

;; Validator functions

(defn pwd-no-number?
  [s]
  (nil? (re-find #"[0-9]" s)))

(defn pwd-no-lower?
  [s]
  (nil? (re-find #"[a-z]" s)))

(defn pwd-no-upper?
  [s]
  (nil? (re-find #"[A-Z]" s)))

(defn pwd-no-symbol?
  [s]
  (nil? (re-find #"[^A-Za-z0-9]" s)))

(defn pwd-no-different-chars?
  [s]
  (if-some [f (first s)] (nil? (some #(not= f %) s)) false))

(defn pwd-no-proper-length?
  [s]
  (not (> 62 (count s) 8)))

(def invalid-password?
  (some-fn pwd-no-proper-length?
           pwd-no-number?
           pwd-no-upper?
           pwd-no-lower?
           pwd-no-symbol?
           pwd-no-different-chars?))

(defn valid-password?
  [p]
  (not (invalid-password? p)))

;; Generators

(defn make-gen-string-alphanumeric
  ([length]
   (gen/fmap str/join (gen/vector gen/char-alphanumeric length)))
  ([min-length max-length]
   (gen/fmap  str/join (gen/vector gen/char-alphanumeric min-length max-length))))

(defn make-gen-string-alpha
  ([length]
   (gen/fmap str/join (gen/vector gen/char-alpha length)))
  ([min-length max-length]
   (gen/fmap str/join (gen/vector gen/char-alpha min-length max-length))))

(def gen-non-empty-string-alphanumeric
  (gen/such-that not-empty gen/string-alphanumeric))

(def gen-non-empty-string-ascii
  (gen/such-that not-empty gen/string-ascii))

(def gen-non-empty-string-alpha
  (gen/such-that not-empty (make-gen-string-alpha 1 32)))

(def gen-non-empty-string-alpha-small
  (gen/such-that not-empty (make-gen-string-alpha 2 3)))

(def gen-non-empty-string-alphanum-mid
  (gen/such-that not-empty (make-gen-string-alphanumeric 1 5)))

(def gen-non-empty-string-tld
  (gen/such-that not-empty (gen/elements ["pl" "de" "us" "uk" "com.pl"
                                          "org" "net" "info" "com" "co.uk"
                                          "org.pl" "net.pl"])))

(def gen-string-password
  (make-gen-string-alphanumeric 4 6))

(def gen-email
  (gen/such-that vc/valid-email?
                 (gen/fmap (fn [[name host top]]
                             (str name "@" (str/lower-case host) "."
                                  (if (> (count top) 3)
                                    (subs top 0 3)
                                    (if (< (count top) 2) "net" (str/lower-case top)))))
                           (gen/tuple gen-non-empty-string-alphanum-mid
                                      gen-non-empty-string-alphanum-mid
                                      gen-non-empty-string-tld))))

(def gen-password
  (gen/such-that valid-password?
                 (gen/fmap (partial str/join "-")
                           (gen/tuple gen-string-password
                                      gen-string-password
                                      gen-string-password))))

(def gen-instant
  (gen/fmap #(.toInstant ^Date %) (mgen/generator inst?)))

(defn make-gen-phone
  ([]
   (make-gen-phone {}))
  ([options]
   (gen/fmap
    (fn [random-uuid]
      (phone/generate (:region          options)
                      (:type            options)
                      (:predicate       options)
                      (:retries         options 150)
                      (:min-digits      options 3)
                      (:locale          options)
                      (:random-seed     options (.getMostSignificantBits ^UUID random-uuid))
                      (:early-shrinking options false)
                      (:preserve-raw    options true)))
    (gen/uuid))))

(def gen-regular-phone
  (make-gen-phone {:min-digits 5 :predicate vc/valid-regular-phone?}))

(def gen-phone
  (make-gen-phone {:predicate phone/valid?}))

;; Schema definitions

(def instant
  (let [string->instant #(if (string? %) (Instant/parse %))]
    (m/-simple-schema
     {:type            :instant
      :pred            (partial instance? java.time.Instant)
      :type-properties {:error/message       "should be Instant"
                        :decode/string       string->instant
                        :decode/json         string->instant
                        :json-schema/type    "string"
                        :json-schema/format  "date-time"
                        :json-schema/example (gen/generate gen-instant)
                        :gen/gen             gen-instant}})))

(def email
  (m/-simple-schema
   {:type            :email
    :pred            vc/valid-email?
    :property-pred   (m/-min-max-pred count)
    :type-properties {:error/message       "should be an e-mail address"
                      :json-schema/type    "string"
                      :json-schema/format  "email"
                      :json-schema/example (gen/generate gen-email)
                      :gen/gen             gen-email}}))

(def regular-phone
  (let [obj->phone #(phutil/try-parse (phone/number %))
        phone->str #(phone/format % :phone-number.format/e164)]
    (m/-simple-schema
     {:type            :regular-phone
      :pred            vc/valid-regular-phone?
      :type-properties {:error/message      "should be a regular phone number"
                        :decode/string      obj->phone
                        :decode/json        obj->phone
                        :encode/string      phone->str
                        :encode/json        phone->str
                        :json-schema/type   "string"
                        :json-schema/format "phone"
                        ;;:json-schema/example (gen/generate gen-regular-phone)
                        :gen/gen            gen-regular-phone}})))

(def phone
  (let [obj->phone #(phutil/try-parse (phone/number %))
        phone->str #(phone/format % :phone-number.format/e164)]
    (m/-simple-schema
     {:type            :phone
      :pred            phone/valid?
      :type-properties {:error/message      "should be a phone number"
                        :decode/string      obj->phone
                        :decode/json        obj->phone
                        :encode/string      phone->str
                        :encode/json        phone->str
                        :json-schema/type   "string"
                        :json-schema/format "phone"
                        ;;:json-schema/example (gen/generate gen-phone)
                        :gen/gen            gen-phone}})))

(def password
  (m/-simple-schema
   {:type            :password
    :pred            valid-password?
    :property-pred   (m/-min-max-pred count)
    :type-properties {:error/message       "should be a password"
                      :json-schema/type    "string"
                      :json-schema/format  "password"
                      :json-schema/example (gen/generate gen-password)
                      :gen/gen             gen-password}}))

(def schemas
  {:email         email
   :password      password
   :instant       instant
   :phone         phone
   :regular-phone regular-phone})

(mregistry/set-default-registry!
 (mregistry/fast-registry
  (merge (m/default-schemas) schemas)))
