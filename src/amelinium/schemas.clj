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
            [clojure.test.check.generators         :as         gen]
            [phone-number.core                     :as       phone]
            [phone-number.spec                     :as     phospec]
            [phone-number.util                     :as      phutil]
            [io.randomseed.utils.validators.common :as          vc]
            [io.randomseed.utils                   :as       utils]
            [amelinium.locale                      :as      locale])

  (:import [java.time Instant]
           [java.util Date]))

(def gen-non-empty-string-alphanumeric
  (gen/such-that not-empty gen/string-alphanumeric))

(def gen-non-empty-string-ascii
  (gen/such-that not-empty gen/string-ascii))

(def gen-email
  (gen/such-that vc/valid-email?
                 (gen/fmap (fn [[name host top]]
                             (str name "@" host
                                  (if (> (count top) 3)
                                    (subs top 0 3)
                                    (if (< (count top) 2) "net" top))))
                           (gen/tuple gen-non-empty-string-alphanumeric
                                      gen-non-empty-string-alphanumeric
                                      gen-non-empty-string-ascii))))

(def instant
  (let [string->instant #(if (string? %) (Instant/parse %))]
    (m/-simple-schema
     {:type            :instant
      :pred            (partial instance? java.time.Instant)
      :type-properties {:error/message      "should be Instant"
                        :decode/string      string->instant
                        :decode/json        string->instant
                        :json-schema/type   "string"
                        :json-schema/format "date-time"
                        :gen/gen            (gen/fmap #(.toInstant ^Date %)
                                                      (mgen/generator inst?))}})))

(def email
  (m/-simple-schema
   {:type            :email
    :pred            vc/valid-email?
    :property-pred   (m/-min-max-pred count)
    :type-properties {:error/message      "should be an e-mail address"
                      :json-schema/type   "string"
                      :json-schema/format "email"
                      :gen/gen            gen-email}}))

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
                        :gen/gen            (phospec/phone-gen {:min-digits 5
                                                                :predicate  vc/valid-regular-phone?})}})))

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
                        :gen/gen            (phospec/phone-gen {:predicate phone/valid?})}})))

(def schemas
  {:email         email
   :instant       instant
   :phone         phone
   :regular-phone regular-phone})

(mregistry/set-default-registry!
 (mregistry/fast-registry
  (merge (m/default-schemas) schemas)))
