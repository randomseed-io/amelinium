(ns

    ^{:doc    "amelinium service, confirmation model."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.model.confirmation

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string           :as           str]
            [next.jdbc                :as          jdbc]
            [next.jdbc.types          :refer [as-other]]
            [taoensso.nippy           :as         nippy]
            [tick.core                :as             t]
            [buddy.core.hash          :as          hash]
            [buddy.core.codecs        :as        codecs]
            [clj-uuid                 :as          uuid]
            [amelinium.db             :as            db]
            [amelinium.db.sql         :as           sql]
            [amelinium.identity       :as      identity]
            [amelinium.common         :as        common]
            [io.randomseed.utils.time :as          time]
            [io.randomseed.utils.ip   :as            ip]
            [io.randomseed.utils.map  :as           map]
            [io.randomseed.utils.map  :refer   [qassoc]]
            [io.randomseed.utils      :refer       :all])

  (:import (clojure.lang Keyword)
           (java.util    UUID)
           (java.time    Duration
                         Instant)
           (amelinium    Suites
                         SuitesJSON
                         PasswordData
                         Identity
                         Session
                         UserData
                         AuthQueries
                         DBPassword)))

;; Coercion

(defn- as-uuid         ^UUID    [u] (if (uuid? u) u (if u (uuid/as-uuid u))))
(defn- long-or-zero    ^Long    [n] (if n (long n) 0))
(defn- long-or-nil     ^Long    [n] (if n (long n)))
(defn- to-long-or-zero ^Long    [n] (safe-parse-long n 0))
(defn- to-instant      ^Instant [t] (if (t/instant? t) t (time/parse-dt t)))
(defn- to-exp-minutes  ^Long    [t] (time/minutes t 1))
(defn- to-bin-num      ^Integer [n] (if n 1 0))
(defn- num-to-boolean  ^Boolean [n] (or (pos-int? n) (true? n)))
(defn- id-to-db         ^Long   [v] (identity/->db :id  v))
(defn- uid-to-db        ^UUID   [v] (identity/->db :uid v))
(defn- identity-to-db           [v] (identity/->db v))

(defn- to-expiry
  ^Instant [t]
  (cond
    (t/instant?  t) t
    (t/duration? t) (t/hence t)
    (vector?     t) (t/hence (time/parse-duration t))
    :else           (time/parse-dt t)))

(db/defcoercions :confirmations
  :id                identity-to-db             identity/of
  :identity          identity-to-db             identity/of
  :user-id           id-to-db                   long-or-nil
  :user-uid          uid-to-db                  as-uuid
  :requester-id      id-to-db                   long-or-nil
  :code              safe-parse-long            long-or-nil
  :token             some-str                   some-str
  :reason            some-str                   some-keyword
  :id-type           some-str                   some-keyword
  :attempts          to-long-or-zero            long-or-zero
  :max-attempts      to-long-or-zero            long-or-zero
  :created           to-expiry                  to-instant
  :expires           to-expiry                  to-instant
  :confirmed         to-bin-num                 num-to-boolean
  :req-id            some-str                   some-str
  :account-type      some-str                   some-keyword
  :first-name        some-str                   some-str
  :middle-name       some-str                   some-str
  :last-name         some-str                   some-str
  :password          nil                        nil
  :password-suite-id safe-parse-long            long-or-nil
  :no-attempts       to-bin-num                 num-to-boolean
  :bad-reason        to-bin-num                 num-to-boolean
  :expired           to-bin-num                 num-to-boolean
  :present           to-bin-num                 num-to-boolean
  :exp-minutes       to-exp-minutes             nil)

;; Helper functions

(def ^:private ten-minutes
  (t/new-duration 10 :minutes))

(def ^:const ^:private
  phone-id-types
  #{:phone :user/phone "phone" "user/phone"})

(defn- phone?
  ^Boolean [id-type]
  (contains? phone-id-types id-type))

(defn gen-code
  "Generates pseudo-random, 7-digits confirmation code. Returns a number."
  ^Long []
  (let [code (format "%07d" (unchecked-int (rand 9999999)))]
    (Long/parseUnsignedLong
     (if (= (first code) \0)
       (str (inc (rand-int 8)) (subs code 1))
       code))))

(defn gen-token
  "Generates random confirmation token. Returns a string."
  ^String []
  (-> (random-uuid) uuid/to-byte-array hash/md5 codecs/bytes->hex))

(defn make-qtoken
  "Generates a quick token on a basis of a string representation of the given
  argument(s). Returns a string."
  (^String [x]
   (-> (or (some-str x) "") codecs/str->bytes hash/md5 codecs/bytes->hex))
  (^String [x y]
   (make-qtoken (strb (some-str x) (some-str y))))
  (^String [x y z]
   (make-qtoken (strb (some-str x) (some-str y) (some-str z))))
  (^String [x y z & more]
   (make-qtoken (apply strb (some-str x) (some-str y) (some-str z) (map some-str more)))))

(defn make-qtoken-some
  "Generates a quick token on a basis of a string representation of the given
  argument(s) which combined may not be an empty string. Returns a string or `nil`."
  (^String [x]
   (if-some [x (some-str x)] (-> x codecs/str->bytes hash/md5 codecs/bytes->hex)))
  (^String [x y]
   (make-qtoken (strb (some-str x) (some-str y))))
  (^String [x y z]
   (make-qtoken (strb (some-str x) (some-str y) (some-str z))))
  (^String [x y z & more]
   (make-qtoken (apply strb (some-str x) (some-str y) (some-str z) (map some-str more)))))

(defn qtoken-matches?
  "Checks if a quick token `qtoken` matches the result of applying `make-qtoken` to all
  other arguments which cannot be an empty string after concatenation. If combined
  arguments are empty, `nil` or `false`, returns `false`. If `qtoken` is falsy or an
  empty string, returns `false` too. If `qtoken` matches the calculated quick token,
  returns `true`."
  ([qtoken] false)
  ([qtoken a] (or (and qtoken (= qtoken (make-qtoken-some a))) false))
  ([qtoken a b] (or (and qtoken (= qtoken (make-qtoken-some a b))) false))
  ([qtoken a b c] (or (and qtoken (= qtoken (make-qtoken-some a b c))) false))
  ([qtoken a b c d] (or (and qtoken (= qtoken (make-qtoken-some a b c d))) false))
  ([qtoken a b c d & more] (or (and qtoken (= qtoken (apply make-qtoken-some a b c d more))) false)))

;; Confirmation deletion

(defn delete
  "Deletes confirmation of identity `id` from a database."
  ([db id]
   (delete db id "creation"))
  ([db id reason]
   (if-some [id (db/<- :confirmations/id id)]
     (sql/delete!
      db :confirmations
      {:id     id
       :reason (db/<- :confirmations/reason (or (some-str reason) "creation"))}))))

;; Generation of confirmation tokens and codes

(defn- calc-attempts-query
  [dec-att?]
  (if dec-att? "IF(attempts > 0, attempts - 1, attempts)" "attempts"))

(defn gen-full-confirmation-query
  "Generates a confirmation query for an e-mail or a phone used during registration of
  a NEW USER account."
  [identity-type]
  (sql/build-query
   "INSERT INTO confirmations(id,code,token,reason,id_type,expires,confirmed,"
   "                          user_id,user_uid,attempts,account_type,first_name,"
   "                          middle_name,last_name,password,password_suite_id)"
   "SELECT ?,?,?,?,?,?,0,"
   "       (SELECT %'users/id'  FROM %'users' WHERE %'users'.%(identity) = ?),"
   "       (SELECT %'users/uid' FROM %'users' WHERE %'users'.%(identity) = ?),"
   "       ?,?,?,?,?,?,?"
   "ON DUPLICATE KEY UPDATE"
   "user_id           = IF(NOW()>expires, VALUE(user_id),      user_id),"
   "user_uid          = IF(NOW()>expires, VALUE(user_uid),     user_uid),"
   "attempts          = IF(NOW()>expires, VALUE(attempts),     IF(attempts > 0, attempts - 1, attempts)),"
   "code              = IF(NOW()>expires, VALUE(code),         code),"
   "token             = IF(NOW()>expires, VALUE(token),        token),"
   "reason            = IF(NOW()>expires, VALUE(reason),       reason),"
   "id_type           = IF(NOW()>expires, VALUE(id_type),      id_type),"
   "created           = IF(NOW()>expires, NOW(),               created),"
   "confirmed         = IF(NOW()>expires, VALUE(confirmed),    confirmed),"
   "account_type      = IF(NOW()>expires, VALUE(account_type), account_type),"
   "first_name        = IF(NOW()>expires, VALUE(first_name),   first_name),"
   "middle_name       = IF(NOW()>expires, VALUE(middle_name),  middle_name),"
   "last_name         = IF(NOW()>expires, VALUE(last_name),    last_name),"
   "password          = IF(NOW()>expires, VALUE(password),     password),"
   "password_suite_id = IF(NOW()>expires, VALUE(password_suite_id), password_suite_id),"
   "req_id            = IF(NOW()>expires, NULL,                req_id),"
   "expires           = IF(NOW()>expires, VALUE(expires),      expires)"
   "RETURNING user_id,user_uid,account_type,attempts,code,token,id_type,created,"
   "          confirmed,expires"
   {:identity identity-type}))

(def ^:const registration-confirmation-query (gen-full-confirmation-query :email))

(defn gen-confirmation-query
  "Generates a confirmation query for an e-mail or a phone UPDATED by an existing user
  or used in other process (like password recovery).

  Note: it may return a query giving an empty result set if there is no requesting
  user in a database."
  [identity-type user-required?]
  (sql/build-query
   "INSERT INTO confirmations(id,code,token,reason,id_type,expires,confirmed,attempts,"
   "                          requester_id,user_id,user_uid)"
   "SELECT ?,?,?,?,?,?,0,?,?,"
   "       (SELECT %'users/id'  FROM %'users' WHERE %'users'.%(identity) = ?),"
   "       (SELECT %'users/uid' FROM %'users' WHERE %'users'.%(identity) = ?)"
   "%SOME? user-required?: FROM %'users' WHERE %'users/id' = ?#"
   "ON DUPLICATE KEY UPDATE"
   "user_id      = IF(NOW()>expires, VALUE(user_id),      user_id),"
   "user_uid     = IF(NOW()>expires, VALUE(user_uid),     user_uid),"
   "requester_id = IF(NOW()>expires, VALUE(requester_id), requester_id),"
   "attempts     = IF(NOW()>expires, VALUE(attempts),     IF(attempts > 0, attempts - 1, attempts)),"
   "code         = IF(NOW()>expires, VALUE(code),         code),"
   "token        = IF(NOW()>expires, VALUE(token),        token),"
   "reason       = IF(NOW()>expires, VALUE(reason),       reason),"
   "id_type      = IF(NOW()>expires, VALUE(id_type),      id_type),"
   "created      = IF(NOW()>expires, NOW(),               confirmations.created),"
   "confirmed    = IF(NOW()>expires, VALUE(confirmed),    confirmed),"
   "req_id       = IF(NOW()>expires, NULL,                req_id),"
   "expires      = IF(NOW()>expires, VALUE(expires),      expires)"
   "RETURNING requester_id,user_id,user_uid,attempts,code,token,id_type,"
   "          created,confirmed,expires"
   {:identity       (or identity-type "email")
    :user-required? user-required?}))

(def ^:const email-confirmation-query (gen-confirmation-query :email true))
(def ^:const phone-confirmation-query (gen-confirmation-query :phone true))
(def ^:const email-confirmation-query-nouser (gen-confirmation-query :email false))
(def ^:const phone-confirmation-query-nouser (gen-confirmation-query :phone false))

(defn- gen-full-confirmation-core
  "Creates a confirmation code for the given identity (an e-mail address or a
  phone).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given identity (`id`) is already assigned to a registered user, the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set
  to ID of existing user, `:id` set to the given identity (as a string) and `:reason`
  set to the given reason (as a keyword, or `nil` if not given)."
  ([db id exp udata]
   (gen-full-confirmation-core db id exp udata nil (get udata :reason)))
  ([db id exp udata id-type]
   (gen-full-confirmation-core db id exp udata id-type (get udata :reason)))
  ([db id exp udata id-type reason]
   (if db
     (if-some [id (identity/opt-type id-type id)]
       (let [code    (if exp (gen-code))
             token   (if exp (gen-token))
             reason  (or reason "creation")
             expires (or exp ten-minutes)
             udata   (mapv #(db/<- :confirmations % (get udata %))
                           [:max-attempts :account-type
                            :first-name :middle-name :last-name
                            :password :password-suite-id])
             id-type (identity/type-opt id-type id)
             query   (gen-full-confirmation-query id-type)
             qargs   (db/<<-* query [:confirmations id code token reason
                                     id-type expires id id] udata)]
         (if-some [{:keys [user-id user-uid requester-id confirmed] :as r}
                   (db/execute-one! db qargs)]
           (-> r
               (map/assoc-if user-id      :existing-user/id   user-id)
               (map/assoc-if user-uid     :existing-user/uid user-uid)
               (map/assoc-if requester-id :user/id       requester-id)
               (qassoc :confirmed? confirmed
                       :exists?    (some? user-id))
               (dissoc :confirmed :user-id :user-uid :requester-id))))))))

(defn- gen-confirmation-core
  "Creates a confirmation code for the given identity (an e-mail address or a
  phone).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given identity (`id`) is already assigned to a registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id`
  set to ID of existing user, `:id` set to the given identity (as a string) and
  `:reason` set to the given reason (as a keyword, or `nil` if not given)."
  ([db id user-id exp attempts]
   (gen-confirmation-core db id user-id exp attempts nil true "change"))
  ([db id user-id exp attempts id-type]
   (gen-confirmation-core db id user-id exp attempts id-type true "change"))
  ([db id user-id exp attempts id-type user-required?]
   (gen-confirmation-core db id user-id exp attempts id-type user-required? "change"))
  ([db id user-id exp attempts id-type user-required? reason]
   (if db
     (if-some [id (identity/opt-type id-type id)]
       (let [need-gen? (or user-required? (some? user-id))
             code      (if (and need-gen? exp) (gen-code))
             token     (if (and need-gen? exp) (gen-token))
             reason    (or reason "change")
             expires   (or exp ten-minutes)
             id-type   (identity/type-opt id-type id)
             query     (gen-confirmation-query id-type user-required?)
             qargs     (db/<<- query [:confirmations id code token reason id-type
                                      expires attempts user-id id id])
             qargs     (if user-required? (conj qargs (nth qargs 7 nil)) qargs)]
         (if-some [{:keys [user-id user-uid requester-id confirmed] :as r}
                   (db/execute-one! db qargs)]
           (-> r
               (map/assoc-if user-id      :existing-user/id     user-id)
               (map/assoc-if user-uid     :existing-user/uid   user-uid)
               (map/assoc-if requester-id :user/id         requester-id)
               (qassoc :user/required? user-required?
                       :exists?        (some? user-id)
                       :confirmed?     confirmed)
               (dissoc :confirmed :user-id :user-uid :requester-id))))))))

;; Registration

(defn create-for-registration
  "Creates a confirmation code for a new user identified by the given e-mail
  address.

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to a registered user the returned map
  will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set to ID
  of existing user, `:id` set to the given e-mail (as a string) and `:reason`
  set to the given reason (as a keyword, or `nil` if not given).

  Attempts counter is increased each time this function is called."
  ([udata]
   (create-for-registration (get udata :db) udata))
  ([db udata]
   (gen-full-confirmation-core db
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               :email
                               (or (get udata :reason) "creation")))
  ([db udata reason]
   (gen-full-confirmation-core db
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               :email
                               reason)))

;; Generic confirmation for existing user

(defn create
  "Creates a confirmation code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail
  address (`:email`), a phone number (`:phone`) or any supported value.

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to some other registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set
  to ID of existing user, `:id` set to the given e-mail (as a string) and `:reason`
  set to the given reason (as a keyword, or `nil` if not given).

  Attempts counter is increased each time this function is called."
  ([db id user-id exp attempts user-required? reason]
   (gen-confirmation-core db id user-id exp attempts :email user-required? :reason))
  ([db id user-id exp attempts id-type user-required? reason]
   (gen-confirmation-core db id user-id exp attempts id-type user-required? reason)))

;; E-mail/phone/... change

(defn create-for-change
  "Creates a confirmation code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail
  address (`:email`), a phone number (`:phone`) or any supported value.

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to some other registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set
  to ID of existing user, `:id` set to the given e-mail (as a string) and `:reason` set
  to the given reason (as a keyword, or `nil` if not given).

  Attempts counter is increased each time this function is called."
  ([db id user-id exp attempts]         (create db id user-id exp attempts true "change"))
  ([db id user-id exp attempts id-type] (create db id user-id exp attempts id-type true "change")))

;; Password recovery

(defn create-for-recovery
  "Creates a recovery code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail
  address (`:email`), a phone number (`:phone`) or any supported value.

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is assigned to the given user the returned map will contain 4
  keys: `:exists?` set to `true`, `:existing-user/id` set to ID of existing user,
  `:id` set to the given e-mail (as a string) and `:reason` set to the given reason
  (as a keyword, or `nil` if not given).

  Attempts counter is increased each time this function is called."
  ([db id user-id exp attempts]         (create db id user-id exp attempts false "recovery"))
  ([db id user-id exp attempts id-type] (create db id user-id exp attempts id-type false "recovery")))

;; Confirming identity with a token or code

(defn gen-report-errors-query
  "Generates SQL query for reporting errors found during confirmation process. When
  executed the query returns a map with boolean values and the following keys:
  `:confirmed` (already confirmed), `:attempts` (attempts exceeded),
  `:reason` (reason for the given token or code is different from the reason for
  which the confirmation had been created for), `:expires` (confirmation expired),
  `:present` (an e-mail or a phone number is already assigned to an existing user)."
  [where]
  (sql/build-query
   "SELECT * FROM"
   "(SELECT"
   " (confirmed  = TRUE)  AS confirmed,"
   " (attempts  <= 0)     AS no_attempts,"
   " (reason    <> ?)     AS bad_reason,"
   " (expires   <  NOW()) AS expired,"
   " ((SELECT DISTINCT 1 FROM users"
   "   WHERE users.email = confirmations.id"
   "      OR users.phone = confirmations.id) <=> 1) AS present"
   " FROM confirmations"
   " %{where}) AS confirmations"
   {:where where}))

(def ^:const report-errors-simple-id-query (gen-report-errors-query "WHERE id = ?"))
(def ^:const report-errors-id-query        (gen-report-errors-query "WHERE id = ? AND reason = ?"))
(def ^:const report-errors-code-query      (gen-report-errors-query "WHERE id = ? AND code = ?"))
(def ^:const report-errors-token-query     (gen-report-errors-query "WHERE token = ?"))

(def verify-bad-id-set                     #{:verify/not-found :verify/bad-id})
(def verify-bad-code-set                   #{:verify/not-found :verify/bad-code})
(def verify-bad-token-set                  #{:verify/not-found :verify/bad-token})

(defn- process-errors
  [r should-be-confirmed?]
  (if r
    (let [r (reduce-kv #(if %3 (conj %1 (keyword "verify" (name %2))) %1) #{} r)]
      (if (some? (not-empty r))
        (if (contains? r :verify/confirmed)
          (if should-be-confirmed? (disj r :verify/confirmed) r)
          (if should-be-confirmed? (conj r :verify/not-confirmed) r))))))

(defn report-errors
  "Returns a set of keywords indicating confirmation errors detected when querying the
  confirmations table. When `token` is given then it will be used to match the
  correct data row. When `id` and `code` are given then they will be used to match
  the correct data row. When the `id` is given but the `code` is `nil` then the
  matching will be performed on `id` and `reason` (to match on `id` only and not
  `reason`, explicitly set code to `false`)."
  ([db token reason should-be-confirmed?]
   (let [reason (or (some-str reason) "creation")
         qargs  (db/<<- report-errors-token-query [:confirmations reason token])
         result (db/execute-one! db qargs)]
     (or (process-errors result should-be-confirmed?)
         verify-bad-token-set)))
  ([db id code reason should-be-confirmed?]
   (let [id     (identity/->db id)
         reason (or (some-str reason) "creation")
         qargs  (cond
                  code          (db/<<- report-errors-code-query      [:confirmations reason id code])
                  (false? code) (db/<<- report-errors-simple-id-query [:confirmations reason id])
                  :no-code      (db/<<- report-errors-id-query        [:confirmations reason id reason]))]
     (or (process-errors (db/execute-one! db qargs) should-be-confirmed?)
         (if code verify-bad-code-set verify-bad-id-set))))
  ([db id token code reason should-be-confirmed?]
   (if token
     (report-errors db token   reason should-be-confirmed?)
     (report-errors db id code reason should-be-confirmed?))))

(defn specific-id
  "Makes errors more specific by replacing generic bad ID error (as a keyword) with a
  bad e-mail or phone error."
  ([errs id src-id email-id phone-id]
   (if errs
     (if (contains? errs src-id)
       (if-some [dst-id (identity/type id)]
         (conj (disj errs src-id) dst-id)
         errs)
       errs)))
  ([errs src-id dst-id]
   (if errs
     (if (contains? errs src-id)
       (conj (disj errs src-id) dst-id)
       errs))))

(defn code-to-token
  "Returns a confirmation token associated with the given confirmation code and
  identity. Additionally, returns confirmation status."
  [db id code]
  (if-some [{confirmed? :confirmed :as r}
            (first
             (sql/find-by-keys
              db :confirmations
              {:id   (db/<- :confirmations/id   id)
               :code (db/<- :confirmations/code code)}
              (qassoc db/opts-simple-map :columns [:token :confirmed])))]
    (-> (qassoc r :confirmed? confirmed?) (dissoc :confirmed))))

(def confirm-token-query
  (sql/build-query
   "INSERT IGNORE INTO confirmations(id,reason,expires,confirmed)"
   "SELECT id,reason,expires,confirmed FROM confirmations"
   "WHERE token = ? AND reason = ? AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE"
   "expires   = IF(VALUE(confirmed) = TRUE, VALUE(expires), DATE_ADD(VALUE(expires), INTERVAL ? MINUTE)),"
   "confirmed = TRUE"
   "RETURNING id AS identity,id_type,reason,confirmed,token,code,requester_id"))

(def confirm-code-query
  (sql/build-query
   "INSERT IGNORE INTO confirmations(id,reason,expires,confirmed)"
   "SELECT id,reason,expires,confirmed FROM confirmations"
   "WHERE id = ? AND code = ? AND reason = ? AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE"
   "expires   = IF(VALUE(confirmed) = TRUE, VALUE(expires), DATE_ADD(VALUE(expires), INTERVAL ? MINUTE)),"
   "confirmed = TRUE"
   "RETURNING id AS identity,id_type,reason,confirmed,token,code,requester_id"))

(defn establish
  "Confirms an identity (`id`), which may be an e-mail or a phone number, using a token
  or an identifier with a code. If the verification is successful, sets `confirmed`
  flag to `TRUE` (1) in a database which prevents from further confirmations and
  marks identity as confirmed for other operations. The `exp-minutes` argument should
  be a positive integer and will be used to increase expiration time by the given
  amount of minutes. This is to ensure that the next operation, if any, which may
  take some time, will succeed. The `reason` argument is the confirmation reason and
  should match the reason given during the generation of a token or code.

  Returns a map with `:confirmed?` set to `true` if the given token or code was
  verified. Returns a map with `:confirmed?` set to `false` and `:error` set to a
  keyword describing the cause if the token or code was not verified. Returns `nil`
  if something went wrong during the interaction with a database or when the required
  input parameters were empty.

  The `:user/id` key of a result, if exists, contains numeric user identifier of a
  requester (the user for whom the verification was initiated).

  If the identity is already confirmed and there is no error (i.e. confirmation has
  not yet expired), it will also return a map with `:confirmed?` set to `true`."
  ([db id code exp-minutes reason]
   (let [reason (or (some-str reason) "creation")]
     (if (and id code)
       (let [{user-id :requester-id confirmed? :confirmed :keys [id-type] :as r}
             (db/<exec-one! db confirm-code-query [:confirmations id code reason exp-minutes])]
         (if confirmed?
           (-> (dissoc r :confirmed :requester-id)
               (qassoc :confirmed? true :user/id user-id))
           (let [errs (report-errors db id code reason false)
                 errs (specific-id errs id :verify/bad-id :verify/bad-email :verify/bad-phone)]
             (if r
               {:confirmed? false
                :id-type    id-type
                :identity   id
                :user/id    user-id
                :code       code
                :errors     errs}
               (let [i (or (identity/of id) id)]
                 {:confirmed? false
                  :id-type    (identity/type i)
                  :identity   i
                  :user/id    nil
                  :code       code
                  :errors     errs}))))))))
  ([db id code token exp-minutes reason]
   (if-some [token (some-str token)]
     (establish db token   exp-minutes reason)
     (establish db id code exp-minutes reason)))
  ([db token exp-minutes reason]
   (if-some [token (some-str token)]
     (let [reason (or (some-str reason) "creation")]
       (let [{id :identity user-id :requester-id confirmed? :confirmed :keys [id-type] :as r}
             (db/<exec-one! db confirm-token-query [:confirmation token reason exp-minutes])]
         (if confirmed?
           (-> (dissoc r :confirmed :requester-id)
               (qassoc :confirmed? true :user/id user-id))
           (let [errs (report-errors db token reason false)
                 i    (or (identity/of id) id)]
             (if r
               {:confirmed? false
                :identity   i
                :id-type    id-type
                :user/id    user-id
                :token      token
                :errors     errs}
               {:confirmed? false
                :user/id    nil
                :token      token
                :errors     errs}))))))))

;; Updating attempts

(def ^:const decrease-attempts-query
  (sql/build-query
   "INSERT IGNORE INTO confirmations"
   "SELECT * FROM confirmations"
   "WHERE id = ? AND reason = ? AND expires > NOW()"
   "ON DUPLICATE KEY UPDATE"
   "attempts = IF(VALUE(confirmed) = FALSE AND VALUE(attempts) > 0,"
   "              VALUE(attempts)-1, VALUE(attempts))"
   "RETURNING id,user_id,user_uid,account_type,attempts,code,token,created,confirmed,expires"))

(defn- decrease-attempts-core
  [db id reason]
  (if db
    (if-some [id (identity/->db id)]
      (let [reason (or (some-str reason) "creation")]
        (if-some [r (jdbc/execute-one! db [decrease-attempts-query id reason] db/opts-simple-map)]
          (-> r
              (qassoc :exists?    (some? (get r :user-id))
                      :confirmed? (get r :confirmed))
              (dissoc :confirmed))
          (let [errs (report-errors db id nil reason false)
                errs (specific-id errs id :verify/bad-id :verify/bad-email :verify/bad-phone)]
            {:confirmed? false
             :errors     errs}))))))

;; Retries

(defn retry-email
  ([udata]        (decrease-attempts-core (get udata :db) (get udata :email) (get udata :reason)))
  ([db id]        (decrease-attempts-core db id "creation"))
  ([db id reason] (decrease-attempts-core db id reason)))

(defn retry-phone
  ([udata]        (decrease-attempts-core (get udata :db) (get udata :phone) (get udata :reason)))
  ([db id]        (decrease-attempts-core db id "creation"))
  ([db id reason] (decrease-attempts-core db id reason)))

;; Updating confirmation request ID

(defn update-request-id
  ([db token request-id]
   (if db
     (if-some [token (some-str token)]
       (if-some [request-id (some-str request-id)]
         (sql/update! db :confirmations
                      {:req-id request-id}
                      {:token token}
                      db/opts-simple-map)))))
  ([db id code request-id]
   (if db
     (if-some [request-id (some-str request-id)]
       (if-some [code (some-str code)]
         (if-some [id (identity/->db id)]
           (sql/update! db :confirmations
                        {:req-id request-id}
                        {:id id :code code}
                        db/opts-simple-map))))))
  ([db id code token request-id]
   (if-some [token (some-str token)]
     (update-request-id db token request-id)
     (update-request-id db id code request-id))))

;; Reading properties

(defn status
  "Returns confirmation status as a map containing the following keys: `:id`,
  `:id_type`, `:attempts`, `:expires`, `:token`, `:confirmed?` and `:qtoken`. The
  arguments should be `db` (database connection object), `id` (user's identity for
  which the verification is required), `qtoken` (quick token derived from
  confirmation token to authorize the operation) and optional `reason` (confirmation
  reason). If reason is not given then all user's confirmation for the given identity
  will be analyzed and their quick tokens calculated; the first quick token that
  matches the one passed as an argument will cause the result to be returned."
  ([db id qtoken]
   (status db id qtoken nil))
  ([db id qtoken reason]
   (if (and id qtoken)
     (if-some [{:keys [expires token confirmed] :as r}
               (if reason
                 (if-some [{:keys [token] :as r}
                           (db/<exec-one! db
                                          ["SELECT id,id_type,attempts,expires,token,confirmed"
                                           "FROM confirmations "
                                           "WHERE id=? AND reason=?"]
                                          [:confirmations id reason])]
                   (if (qtoken-matches? qtoken id token) (qassoc r :qtoken qtoken)))
                 (some #(if (qtoken-matches? qtoken id (get % :token)) (qassoc % :qtoken qtoken))
                       (db/<exec! db
                                  ["SELECT id,id_type,attempts,expires,token,confirmed"
                                   "FROM confirmations "
                                   "WHERE id=?"]
                                  [:confirmations id])))]
       (if (and expires token)
         (-> (qassoc r :confirmed? confirmed)
             (dissoc r :confirmed)))))))
