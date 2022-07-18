(ns

    ^{:doc    "amelinium service, confirmation model."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.model.confirmation

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string           :as           str]
            [next.jdbc                :as          jdbc]
            [next.jdbc.sql            :as           sql]
            [next.jdbc.types          :refer [as-other]]
            [tick.core                :as             t]
            [buddy.core.hash          :as          hash]
            [buddy.core.codecs        :as        codecs]
            [clj-uuid                 :as          uuid]
            [amelinium.db             :as            db]
            [io.randomseed.utils.time :as          time]
            [io.randomseed.utils.map  :as           map]
            [io.randomseed.utils.ip   :as            ip]
            [io.randomseed.utils      :refer       :all]))

;; Queries

(def ^:const new-email-confirmation-query
  (str-spc
   "INSERT INTO confirmations(id,code,token,reason,expires)"
   "SELECT ?,?,?,?,?"
   "ON DUPLICATE KEY UPDATE"
   "attempts = IF(NOW()>expires, 1, attempts),"
   "code     = IF(NOW()>expires, VALUE(code), code),"
   "token    = IF(NOW()>expires, VALUE(token), token),"
   "created  = IF(NOW()>expires, NOW(), created),"
   "expires  = IF(NOW()>expires, VALUE(expires), expires)"
   "RETURNING *"))

(def ^:const new-email-confirmation-query-with-attempt
  (str-spc
   "INSERT INTO confirmations(id,code,token,reason,expires,attempts)"
   "SELECT ?,?,?,?,?,1"
   "ON DUPLICATE KEY UPDATE"
   "attempts = IF(NOW()>expires, 1, attempts + 1),"
   "code     = IF(NOW()>expires, VALUE(code), code),"
   "token    = IF(NOW()>expires, VALUE(token), token),"
   "created  = IF(NOW()>expires, NOW(), created),"
   "expires  = IF(NOW()>expires, VALUE(expires), expires)"
   "RETURNING *"))

(def ^:const new-phone-confirmation-query
  (str-spc
   "INSERT INTO confirmations(id,code,token,reason,expires)"
   "SELECT ?,?,?,?,?"
   "ON DUPLICATE KEY UPDATE"
   "attempts = IF(NOW()>expires, 1, attempts),"
   "code     = IF(NOW()>expires, VALUE(code), code),"
   "token    = IF(NOW()>expires, VALUE(token), token),"
   "created  = IF(NOW()>expires, NOW(), created),"
   "expires  = IF(NOW()>expires, VALUE(expires), expires)"
   "RETURNING *"))

(def ^:const new-phone-confirmation-query-with-attempt
  (str-spc
   "INSERT INTO confirmations(id,code,token,reason,expires,attempts)"
   "SELECT ?,?,?,?,?,1"
   "ON DUPLICATE KEY UPDATE"
   "attempts = IF(NOW()>expires, 1, attempts + 1),"
   "code     = IF(NOW()>expires, VALUE(code), code),"
   "token    = IF(NOW()>expires, VALUE(token), token),"
   "created  = IF(NOW()>expires, NOW(), created),"
   "expires  = IF(NOW()>expires, VALUE(expires), expires)"
   "RETURNING *"))

(def ^:const inc-attempts-query
  "UPDATE confirmations SET attempts = attempts + 1 WHERE id = ?")

(def ^:const email-exists-query
  "SELECT uid FROM users WHERE email = ?")

(def ^:const phone-exists-query
  "SELECT uid FROM users WHERE phone = ?")

(defn gen-code
  []
  (format "%07d" (unchecked-int (rand 9999999))))

(defn gen-token
  []
  (-> (random-uuid) uuid/to-byte-array hash/md5 codecs/bytes->hex))

(defn phone-exists?
  [db phone]
  (when db
    (when-some [phone (some-str phone)]
      (-> (jdbc/execute-one! db [phone-exists-query phone] db/opts-simple-vec)
          first some?))))

(defn email-exists?
  [db email]
  (when db
    (when-some [email (some-str email)]
      (-> (jdbc/execute-one! db [email-exists-query email] db/opts-simple-vec)
          first some?))))

(defn- new-confirmation-core
  "Creates a confirmation code for a new user identified by the given e-mail
  address. When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given identity (`id`) is already
  assigned to a registered user the returned map will contain 4 keys: `:exists?` set
  to `true`, `:uid` set to UID of existing user, `:id` set to the given identity (as
  a string) and `:reason` set to the given reason (as a keyword or `nil` if not
  given)."
  [db id exp query exists-query]
  (when db
    (when-some [id (some-str id)]
      (let [uid     (first (jdbc/execute-one! db [exists-query id] db/opts-simple-vec))
            exists? (some? uid)
            code    (when-not exists? (gen-code))
            token   (when-not exists? (gen-token))
            exp     (or exp (t/new-duration 10 :minutes))
            exp     (if (t/duration? exp) (t/hence exp) exp)]
        (when-some [r (jdbc/execute-one! db [query id code token "creation" exp] db/opts-simple-map)]
          (-> (map/update-existing r :reason some-keyword)
              (assoc :exists? exists? :uid uid)))))))

(defn new-email
  "Creates a confirmation code for a new user identified by the given e-mail
  address. When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given e-mail is already assigned
  to a registered user the returned map will contain 4 keys: `:exists?` set to
  `true`, `:uid` set to UID of existing user, `:id` set to the given e-mail (as a
  string) and `:reason` set to the given reason (as a keyword or `nil` if not
  given)."
  ([db email]
   (new-email db email nil))
  ([db email exp]
   (new-confirmation-core db email exp
                          new-email-confirmation-query
                          email-exists-query)))

(defn new-email-with-attempt
  "Creates a confirmation code for a new user identified by the given e-mail
  address. When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given e-mail is already assigned
  to a registered user the returned map will contain 4 keys: `:exists?` set to
  `true`, `:uid` set to UID of existing user, `:id` set to the given e-mail (as a
  string) and `:reason` set to the given reason (as a keyword or `nil` if not
  given). Attempts counter is increased each time this function is called."
  ([db email]
   (new-email-with-attempt db email nil))
  ([db email exp]
   (new-confirmation-core db email exp
                          new-email-confirmation-query-with-attempt
                          email-exists-query)))

(defn new-phone
  "Creates a confirmation code for a new user identified by the given phone
  number. When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given e-mail is already assigned
  to a registered user the returned map will contain 4 keys: `:exists?` set to
  `true`, `uid` set to UID of existing user, `:id` set to the given phone number (as
  a string) and `:reason` set to the given reason (as a keyword or `nil` if not
  given). Attempts counter is increased each time this function is called."
  ([db phone]
   (new-phone db phone nil))
  ([db phone exp]
   (new-confirmation-core db phone exp
                          new-phone-confirmation-query
                          phone-exists-query)))

(defn new-phone-with-attempt
  "Creates a confirmation code for a new user identified by the given phone
  number. When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given e-mail is already assigned
  to a registered user the returned map will contain 4 keys: `:exists?` set to
  `true`, `:uid` set to UID of existing user, `:id` set to the given phone number (as
  a string) and `:reason` set to the given reason (as a keyword or `nil` if not
  given)."
  ([db phone]
   (new-phone-with-attempt db phone nil))
  ([db phone exp]
   (new-confirmation-core db phone exp
                          new-phone-confirmation-query-with-attempt
                          phone-exists-query)))
