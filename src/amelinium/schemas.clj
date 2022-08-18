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
            [tick.core                             :as           t]
            [clojure.string                        :as         str]
            [clojure.test.check.generators         :as         gen]
            [phone-number.core                     :as       phone]
            [phone-number.util                     :as      phutil]
            [io.randomseed.utils.validators.common :as          vc]
            [io.randomseed.utils                   :as       utils]
            [amelinium.locale                      :as      locale])

  (:import [java.util UUID]
           [java.time Duration]))

;; Helpers

(defn pad-zero
  [n]
  (format "%02d" n))

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
  (some-fn (complement string?)
           pwd-no-proper-length?
           pwd-no-number?
           pwd-no-upper?
           pwd-no-lower?
           pwd-no-symbol?
           pwd-no-different-chars?))

(defn valid-password?
  [p]
  (not (invalid-password? p)))

(defn valid-password-relaxed?
  [p]
  (and (string? p) (pos-int? (count p))))

(defn valid-session-id?
  [s]
  (and (string? s)
       (some? (re-find #"^[a-f0-9]{32}(-[a-f0-9]{32})?$" s))))

(defn valid-secure-session-id?
  [s]
  (and (string? s)
       (some? (re-find #"^[a-f0-9]{32}-[a-f0-9]{32}$" s))))

(defn valid-string-md5?
  [s]
  (and (string? s)
       (some? (re-find #"^[a-f0-9]{32}$" s))))

(defn valid-name?
  [s]
  (and (string? s)
       (some? (re-find #"^\p{L}[\p{L} ,.'-]*$" s))))

;; Generators

(def gen-char-hex
  (gen/fmap char
            (gen/one-of [(gen/choose (int \a) (int \f))
                         (gen/choose (int \0) (int \9))])))

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

(defn make-gen-string-hex
  ([length]
   (gen/fmap str/join (gen/vector gen-char-hex length)))
  ([min-length max-length]
   (gen/fmap str/join (gen/vector gen-char-hex min-length max-length))))

(def gen-non-empty-string-alphanumeric
  (gen/such-that not-empty gen/string-alphanumeric))

(def gen-non-empty-string-ascii
  (gen/such-that not-empty gen/string-ascii))

(def gen-non-empty-string-alpha
  (gen/such-that not-empty (make-gen-string-alpha 1 32)))

(def gen-non-empty-string-alpha-small
  (gen/such-that not-empty (make-gen-string-alpha 2 3)))

(def gen-non-empty-string-alphanum-mid
  (gen/such-that not-empty (make-gen-string-alphanumeric 1 7)))

(def gen-non-empty-string-tld
  (gen/such-that not-empty (gen/elements ["pl" "de" "us" "uk" "com.pl"
                                          "org" "net" "info" "com" "co.uk"
                                          "org.pl" "net.pl"])))

(def gen-string-password
  (make-gen-string-alphanumeric 4 6))

(def gen-name
  (gen/such-that valid-name? (gen/fmap str/capitalize gen-non-empty-string-alpha)))

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
  (gen/such-that t/instant?
                 (gen/fmap (fn [[Y M D h m s]]
                             (t/instant (str Y "-" (pad-zero M) "-" (pad-zero D)
                                             "T"
                                             (pad-zero h) ":"
                                             (pad-zero m) ":"
                                             (pad-zero s) "Z")))
                           (gen/tuple (gen/choose 1969 2050)
                                      (gen/choose 1 12)
                                      (gen/choose 1 28)
                                      (gen/choose 0 23)
                                      (gen/choose 0 59)
                                      (gen/choose 0 59)))))

(def gen-duration
  (gen/such-that t/duration?
                 (gen/fmap (fn [[d h m s]] (t/+ (t/new-duration d :days)
                                                (t/new-duration h :hours)
                                                (t/new-duration m :minutes)
                                                (t/new-duration s :seconds)))
                           (gen/tuple (gen/choose 0 2)
                                      (gen/choose 0 23)
                                      (gen/choose 0 59)
                                      (gen/choose 0 59)))))

(defn make-gen-phone
  ([]
   (make-gen-phone {}))
  ([options]
   (gen/fmap
    (fn [random-uuid]
      (:phone-number/number
       (phone/generate (:region          options)
                       (:type            options)
                       (:predicate       options)
                       (:retries         options 150)
                       (:min-digits      options 3)
                       (:locale          options)
                       (:random-seed     options (.getMostSignificantBits ^UUID random-uuid))
                       (:early-shrinking options false)
                       (:preserve-raw    options true))))
    gen/uuid)))

(def gen-regular-phone
  (make-gen-phone {:min-digits 5 :predicate vc/valid-regular-phone?}))

(def gen-phone
  (make-gen-phone {:predicate phone/valid?}))

(def gen-string-md5
  (make-gen-string-hex 32))

(def gen-session-id
  (gen/such-that valid-session-id?
                 (gen/fmap (fn [[a b]] (if b (str a "-" b) a))
                           (gen/tuple gen-string-md5
                                      (gen/one-of [gen-string-md5 (gen/return nil)])))))

(def gen-secure-session-id
  (gen/such-that valid-secure-session-id?
                 (gen/fmap (fn [[a b]] (str a "-" b))
                           (gen/tuple gen-string-md5
                                      gen-string-md5))))

;; Schema definitions

(def instant
  (let [obj->instant #(try (t/instant %) (catch Throwable _ nil))]
    (m/-simple-schema
     {:type            :instant
      :pred            t/instant?
      :type-properties {:error/message       "should be Instant"
                        :encode/string       utils/some-str
                        :encode/json         utils/some-str
                        :decode/string       obj->instant
                        :decode/json         obj->instant
                        :json-schema/type    "string"
                        :json-schema/format  "date-time"
                        :json-schema/example (utils/some-str (gen/generate gen-instant))
                        :gen/gen             gen-instant}})))

(def duration
  (let [obj->duration #(if (t/duration? %) % (try (Duration/parse %) (catch Throwable _ nil)))]
    (m/-simple-schema
     {:type            :duration
      :pred            t/duration?
      :type-properties {:error/message       "should be Duration"
                        :encode/string       utils/some-str
                        :encode/json         utils/some-str
                        :decode/string       obj->duration
                        :decode/json         obj->duration
                        :json-schema/type    "string"
                        :json-schema/format  "duration"
                        :json-schema/example (utils/some-str (gen/generate gen-duration))
                        :gen/gen             gen-duration}})))

(def email
  (m/-simple-schema
   {:type            :email
    :pred            vc/valid-email?
    :property-pred   (m/-min-max-pred count)
    :type-properties {:error/message       "should be an e-mail address"
                      :decode/json         utils/some-str
                      :encode/string       utils/some-str
                      :encode/json         utils/some-str
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
      :type-properties {:error/message       "should be a regular phone number"
                        :decode/string       obj->phone
                        :decode/json         obj->phone
                        :encode/string       phone->str
                        :encode/json         phone->str
                        :json-schema/type    "string"
                        :json-schema/format  "phone"
                        :json-schema/example (phone->str (gen/generate gen-regular-phone))
                        :gen/gen             gen-regular-phone}})))

(def phone
  (let [obj->phone #(phutil/try-parse (phone/number %))
        phone->str #(phone/format % :phone-number.format/e164)]
    (m/-simple-schema
     {:type            :phone
      :pred            phone/valid?
      :type-properties {:error/message       "should be a phone number"
                        :decode/string       obj->phone
                        :decode/json         obj->phone
                        :encode/string       phone->str
                        :encode/json         phone->str
                        :json-schema/type    "string"
                        :json-schema/format  "phone"
                        :json-schema/example (phone->str (gen/generate gen-phone))
                        :gen/gen             gen-phone}})))

(def password
  (m/-simple-schema
   {:type            :password
    :pred            valid-password?
    :property-pred   (m/-min-max-pred count)
    :type-properties {:error/message       "should be a password"
                      :decode/json         utils/some-str
                      :encode/string       utils/some-str
                      :encode/json         utils/some-str
                      :json-schema/type    "string"
                      :json-schema/format  "password"
                      :json-schema/example (gen/generate gen-password)
                      :gen/gen             gen-password}}))

(def password-relaxed
  (m/-simple-schema
   {:type            :password-relaxed
    :pred            valid-password-relaxed?
    :property-pred   (m/-min-max-pred count)
    :type-properties {:error/message       "should be a relaxed password"
                      :decode/json         utils/some-str
                      :encode/string       utils/some-str
                      :encode/json         utils/some-str
                      :json-schema/type    "string"
                      :json-schema/format  "password"
                      :json-schema/example (gen/generate gen-password)
                      :gen/gen             gen-password}}))

(def session-id
  (m/-simple-schema
   {:type            :session-id
    :pred            valid-session-id?
    :type-properties {:error/message       "should be a session ID"
                      :decode/json         utils/some-str
                      :encode/string       utils/some-str
                      :encode/json         utils/some-str
                      :json-schema/type    "string"
                      :json-schema/pattern "^[a-f0-9]{32}(-[a-f0-9]{32})?$"
                      :json-schema/example (gen/generate gen-session-id)
                      :gen/gen             gen-session-id}}))

(def secure-session-id
  (m/-simple-schema
   {:type            :secure-session-id
    :pred            valid-secure-session-id?
    :type-properties {:error/message       "should be a secure session ID"
                      :decode/json         utils/some-str
                      :encode/string       utils/some-str
                      :encode/json         utils/some-str
                      :json-schema/type    "string"
                      :json-schema/pattern "^[a-f0-9]{32}-[a-f0-9]{32}$"
                      :json-schema/example (gen/generate gen-secure-session-id)
                      :gen/gen             gen-secure-session-id}}))

(def md5-string
  (m/-simple-schema
   {:type            :md5-string
    :pred            valid-string-md5?
    :type-properties {:error/message       "should be an MD5 string"
                      :decode/json         utils/some-str
                      :encode/string       utils/some-str
                      :encode/json         utils/some-str
                      :json-schema/type    "string"
                      :json-schema/pattern "^[a-f0-9]{32}$"
                      :json-schema/example (gen/generate gen-string-md5)
                      :gen/gen             gen-string-md5}}))

(def confirmation-token
  (m/-simple-schema
   {:type            :confirmation-token
    :pred            valid-string-md5?
    :type-properties {:error/message       "should be a confirmation token"
                      :decode/json         utils/some-str
                      :encode/string       utils/some-str
                      :encode/json         utils/some-str
                      :json-schema/type    "string"
                      :json-schema/pattern "^[a-f0-9]{32}$"
                      :json-schema/example (gen/generate gen-string-md5)
                      :gen/gen             gen-string-md5}}))

(def personal-name
  (m/-simple-schema
   {:type            :name
    :pred            valid-name?
    :property-pred   (m/-min-max-pred count)
    :type-properties {:error/message       "should be a valid name"
                      :decode/json         utils/some-str
                      :encode/string       utils/some-str
                      :encode/json         utils/some-str
                      :json-schema/type    "string"
                      :json-schema/pattern "^\\p{L}[\\p{L} ,.'-]*$"
                      :json-schema/example (gen/generate gen-name)
                      :gen/gen             gen-name}}))

(def schemas
  {:email              email
   :name               personal-name
   :password           password
   :password-relaxed   password-relaxed
   :instant            instant
   :duration           duration
   :phone              phone
   :regular-phone      regular-phone
   :md5-string         md5-string
   :confirmation-token confirmation-token
   :session-id         session-id
   :secure-session-id  secure-session-id})

(mregistry/set-default-registry!
 (mregistry/fast-registry
  (merge (m/default-schemas) schemas)))

;; Dynamic schemas

(defn gen-gen-language
  [langs]
  (let [langs (vec langs)]
    (gen/such-that not-empty (gen/elements langs))))

(defn gen-language-schema
  [id supported-languages]
  (let [id    (utils/some-keyword id)
        langs (if (utils/valuable? supported-languages) supported-languages [:en])
        langs (if (coll? langs) langs [langs])
        langs (set (map keyword langs))
        enums (mapv name langs)
        stype (keyword (or (and (keyword? name) (name id)) :language))]
    (m/-simple-schema
     {:type            stype
      :pred            #(and (keyword? %) (contains? langs %))
      :type-properties {:error/message       "should be a supported language"
                        :decode/string       utils/some-keyword
                        :decode/json         utils/some-keyword
                        :encode/string       utils/some-str
                        :encode/json         utils/some-str
                        :json-schema/type    "string"
                        :json-schema/pattern "[A-Za-z_\\-\\:\\.]{2,7}"
                        :json-schema/enum    enums
                        :json-schema/example "en"
                        :gen/gen             (gen-gen-language langs)}})))
