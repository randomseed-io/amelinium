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

;; Generation of confirmation tokens and codes

(def ^:const new-email-confirmation-query
  (str-spc
   "INSERT INTO confirmations(id,code,token,reason,expires,confirmed,user_id)"
   "SELECT ?,?,?,?,?,0,(SELECT users.id FROM users WHERE users.email = ?)"
   "ON DUPLICATE KEY UPDATE"
   "user_id   = VALUE(user_id),"
   "attempts  = IF(NOW()>expires, 1, attempts),"
   "code      = IF(NOW()>expires, VALUE(code), code),"
   "token     = IF(NOW()>expires, VALUE(token), token),"
   "created   = IF(NOW()>expires, NOW(), created),"
   "confirmed = IF(NOW()>expires, VALUE(confirmed), confirmed),"
   "expires   = IF(NOW()>expires, VALUE(expires), expires)"
   "RETURNING *"))

(def ^:const new-email-confirmation-query-with-attempt
  (str-spc
   "INSERT INTO confirmations(id,code,token,reason,expires,attempts,confirmed,user_id)"
   "SELECT ?,?,?,?,?,1,0,(SELECT users.id FROM users WHERE users.email = ?)"
   "ON DUPLICATE KEY UPDATE"
   "user_id   = VALUE(user_id),"
   "attempts  = IF(NOW()>expires, 1, attempts + 1),"
   "code      = IF(NOW()>expires, VALUE(code), code),"
   "token     = IF(NOW()>expires, VALUE(token), token),"
   "created   = IF(NOW()>expires, NOW(), created),"
   "confirmed = IF(NOW()>expires, VALUE(confirmed), confirmed),"
   "expires   = IF(NOW()>expires, VALUE(expires), expires)"
   "RETURNING *"))

(def ^:const new-phone-confirmation-query
  (str-spc
   "INSERT INTO confirmations(id,code,token,reason,expires,confirmed,user_id)"
   "SELECT ?,?,?,?,?,0,(SELECT users.id FROM users WHERE users.phone = ?)"
   "ON DUPLICATE KEY UPDATE"
   "user_id   = VALUE(user_id),"
   "attempts  = IF(NOW()>expires, 1, attempts),"
   "code      = IF(NOW()>expires, VALUE(code), code),"
   "token     = IF(NOW()>expires, VALUE(token), token),"
   "created   = IF(NOW()>expires, NOW(), created),"
   "confirmed = IF(NOW()>expires, VALUE(confirmed), confirmed),"
   "expires   = IF(NOW()>expires, VALUE(expires), expires)"
   "RETURNING *"))

(def ^:const new-phone-confirmation-query-with-attempt
  (str-spc
   "INSERT INTO confirmations(id,code,token,reason,expires,attempts,confirmed,user_id)"
   "SELECT ?,?,?,?,?,1,0,(SELECT users.id FROM users WHERE users.phone = ?)"
   "ON DUPLICATE KEY UPDATE"
   "user_id   = VALUE(user_id),"
   "attempts  = IF(NOW()>expires, 1, attempts + 1),"
   "code      = IF(NOW()>expires, VALUE(code), code),"
   "token     = IF(NOW()>expires, VALUE(token), token),"
   "created   = IF(NOW()>expires, NOW(), created),"
   "confirmed = IF(NOW()>expires, VALUE(confirmed), confirmed),"
   "expires   = IF(NOW()>expires, VALUE(expires), expires)"
   "RETURNING *"))

(defn- gen-confirmation-core
  "Creates a confirmation code for the given identity (an e-mail address or a
  phone). When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given identity (`id`) is already
  assigned to a registered user the returned map will contain 4 keys: `:exists?` set
  to `true`, `:user/id` set to ID of existing user, `:id` set to the given
  identity (as a string) and `:reason` set to the given reason (as a keyword or `nil`
  if not given)."
  ([db query id exp]
   (gen-confirmation-core db id exp query nil))
  ([db query id exp reason]
   (when db
     (when-some [id (some-str id)]
       (let [code   (gen-code)
             token  (gen-token)
             reason (or (some-str reason) "creation")
             exp    (or exp (t/new-duration 10 :minutes))
             exp    (if (t/duration? exp) (t/hence exp) exp)]
         (when-some [r (jdbc/execute-one! db [query id code token reason exp id] db/opts-simple-map)]
           (let [user-id    (get r :user-id)
                 exists?    (pos-int? user-id)
                 confirmed? (pos-int? (get r :confirmed))]
             (-> (if exists? (assoc r :exists? true :user/id user-id) (assoc r :exists? false))
                 (assoc :confirmed? (pos-int? (get r :confirmed)))
                 (dissoc :confirmed)
                 (map/update-existing :reason some-keyword)))))))))

(defn create-for-registration-without-attempt
  "Creates a confirmation code for a new user identified by the given e-mail
  address. When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given e-mail is already assigned
  to a registered user the returned map will contain 4 keys: `:exists?` set to
  `true`, `:user/id` set to ID of existing user, `:id` set to the given e-mail (as a
  string) and `:reason` set to the given reason (as a keyword or `nil` if not
  given)."
  ([db email]
   (create-for-registration-without-attempt db email nil))
  ([db email exp]
   (gen-confirmation-core db new-email-confirmation-query email exp "creation")))

(defn create-for-registration
  "Creates a confirmation code for a new user identified by the given e-mail
  address. When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given e-mail is already assigned
  to a registered user the returned map will contain 4 keys: `:exists?` set to
  `true`, `:user/id` set to ID of existing user, `:id` set to the given e-mail (as a
  string) and `:reason` set to the given reason (as a keyword or `nil` if not
  given). Attempts counter is increased each time this function is called."
  ([db email]
   (create-for-registration db email nil))
  ([db email exp]
   (gen-confirmation-core db new-email-confirmation-query-with-attempt email exp "creation")))

;; Confirming identity with a token or code

(def ^:const confirmation-report-error-token-query
  (str-spc
   "SELECT (confirmed = TRUE) AS confirmed,"
   "(reason <> ?) AS bad_reason,"
   "(expires < NOW()) AS expired"
   "FROM confirmations WHERE token = ?"))

(def ^:const confirmation-report-error-code-query
  (str-spc
   "SELECT (confirmed = TRUE) AS confirmed,"
   "(reason <> ?) AS bad_reason,"
   "(expires < NOW()) AS expired"
   "FROM confirmations WHERE code = ? AND id = ?"))

(defn- confirmation-report-error
  ([r]
   (cond
     (nil? r)                      :verify/bad-token
     (pos-int? (:bad_reason    r)) :verify/bad-reason
     (pos-int? (:expired       r)) :verify/expired
     (pos-int? (:confirmed     r)) :verify/confirmed
     :bad-token                    :verify/bad-token))
  ([db code email reason]
   (let [r (confirmation-report-error
            (jdbc/execute-one! db [confirmation-report-error-code-query
                                   (or reason "creation")
                                   code email]
                               db/opts-simple-map))]
     (if (= :verify/bad-token r) :verify/bad-code r)))
  ([db token reason]
   (confirmation-report-error
    (jdbc/execute-one! db [confirmation-report-error-token-query
                           (or reason "creation")
                           token]
                       db/opts-simple-map))))

(def confirm-token-query
  (str-spc
   "UPDATE confirmations"
   "SET expires = DATE_ADD(expires, INTERVAL ? MINUTE), confirmed = TRUE"
   "WHERE token = ? AND confirmed <> TRUE AND reason = ? AND expires >= NOW()"))

(def confirm-code-query
  (str-spc
   "UPDATE confirmations"
   "SET expires = DATE_ADD(expires, INTERVAL ? MINUTE), confirmed = TRUE"
   "WHERE id = ? AND code = ? AND confirmed <> TRUE AND reason = ? AND expires >= NOW()"))

(defn establish
  "Confirms an identity (`id`), which may be an e-mail or a phone number, using a token
  or an identifier with a code. If the verification is successful, sets `confirmed`
  flag to `TRUE` (1) in a database which prevents from further confirmations and
  marks identity as confirmed for other operations. The `exp-inc` argument should be
  a positive integer and will be used to increase expiration time by the given amount
  of minutes. This is to ensure that next operation, which may take some time (like
  entering user details by newly registered user), will succeed. The `reason`
  argument is the confirmation reason and should match the reason given during the
  generation of a token or code.

  Returns a map with `:confirmed?` set to `true` if the token or code was
  verified. Returns a map with `:confirmed?` set to `false` and `:error` set to a
  keyword describing the cause if the token or code was not verified. Returns `nil`
  if something went wrong during interaction with a database or when the required
  input parameters were empty."
  ([db id code exp-inc reason]
   (let [reason (or (some-str reason) "creation")
         id     (some-str id)
         code   (some-str code)]
     (when (and id code (pos-int? exp-inc))
       (when-some [r (::jdbc/update-count
                      (jdbc/execute-one! db [confirm-code-query exp-inc id code reason]
                                         db/opts-simple-map))]
         (if (pos-int? r)
           {:confirmed? true}
           (when (int? r)
             (let [err (confirmation-report-error db id code reason)]
               {:confirmed? (= err :verify/confirmed) :error err})))))))
  ([db id code token exp-inc reason]
   (if-some [token (some-str token)]
     (establish db token exp-inc reason)
     (establish db id code exp-inc reason)))
  ([db token exp-inc reason]
   (when-some [token (some-str token)]
     (let [reason (or (some-str reason) "creation")]
       (when-some [r (::jdbc/update-count
                      (jdbc/execute-one! db [confirm-token-query exp-inc token reason]
                                         db/opts-simple-map))]
         (if (pos-int? r)
           {:confirmed? true}
           (when (int? r)
             (let [err (confirmation-report-error db token reason)]
               {:confirmed? false :error err}))))))))
