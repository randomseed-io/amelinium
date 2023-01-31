(ns

    ^{:doc    "amelinium service, confirmation model."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.model.confirmation

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string           :as           str]
            [next.jdbc                :as          jdbc]
            [next.jdbc.sql            :as           sql]
            [next.jdbc.types          :refer [as-other]]
            [taoensso.nippy           :as         nippy]
            [tick.core                :as             t]
            [buddy.core.hash          :as          hash]
            [buddy.core.codecs        :as        codecs]
            [clj-uuid                 :as          uuid]
            [amelinium.db             :as            db]
            [amelinium.common         :as        common]
            [phone-number.core        :as         phone]
            [io.randomseed.utils.time :as          time]
            [io.randomseed.utils.ip   :as            ip]
            [io.randomseed.utils.map  :as           map]
            [io.randomseed.utils.map  :refer   [qassoc]]
            [io.randomseed.utils      :refer       :all]))

(def ten-minutes
  (t/new-duration 10 :minutes))

(def ^:const email-exists-query
  "SELECT uid FROM users WHERE email = ?")

(def ^:const phone-exists-query
  "SELECT uid FROM users WHERE phone = ?")

(defn gen-code
  []
  (let [code (format "%07d" (unchecked-int (rand 9999999)))]
    (if (= (first code) \0)
      (str (inc (rand-int 8)) (subs code 1))
      code)))

(defn gen-token
  []
  (-> (random-uuid) uuid/to-byte-array hash/md5 codecs/bytes->hex))

(defn phone-exists?
  [db phone]
  (if db
    (if-some [phone (db/identity->str phone)]
      (-> (jdbc/execute-one! db [phone-exists-query phone] db/opts-simple-vec)
          first some?))))

(defn email-exists?
  [db email]
  (if db
    (if-some [email (some-str email)]
      (-> (jdbc/execute-one! db [email-exists-query email] db/opts-simple-vec)
          first some?))))

(def ^:const ^:private
  phone-id-types
  #{:phone :user/phone "phone" "user/phone"})

(defn- phone?
  ^Boolean [id-type]
  (contains? phone-id-types id-type))

;; Generation of confirmation tokens and codes

(defn- calc-attempts-query
  [dec-att?]
  (if dec-att?
    (str "IF(attempts > 0, attempts - 1, attempts)")
    (str "attempts")))

(defn gen-full-confirmation-query
  "Generates a confirmation query for an e-mail or a phone used during registration of
  a NEW USER."
  [id-column dec-att?]
  (str-squeeze-spc
   "INSERT INTO confirmations(id,code,token,reason,id_type,expires,confirmed,user_id,user_uid,"
   "attempts,account_type,first_name,middle_name,last_name,password,password_suite_id)"
   (str "SELECT ?,?,?,?,?,?,0,"
        " (SELECT users.id  FROM users WHERE users." (or (some-str id-column) "email") " = ?),"
        " (SELECT users.uid FROM users WHERE users." (or (some-str id-column) "email") " = ?),"
        "?,?,?,?,?,?,?")
   "ON DUPLICATE KEY UPDATE"
   "user_id           = IF(NOW()>expires, VALUE(user_id),      user_id),"
   "user_uid          = IF(NOW()>expires, VALUE(user_uid),     user_uid),"
   "attempts          = IF(NOW()>expires, VALUE(attempts),"    (str (calc-attempts-query dec-att?) "),")
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
   "RETURNING user_id,user_uid,account_type,attempts,code,token,id_type,created,confirmed,expires"))

(def ^:const new-email-confirmation-query
  (gen-full-confirmation-query :email true))

(def ^:const new-phone-confirmation-query
  (gen-full-confirmation-query :phone true))

(def ^:const new-email-confirmation-query-without-attempt
  (gen-full-confirmation-query :email false))

(def ^:const new-phone-confirmation-query-without-attempt
  (gen-full-confirmation-query :phone false))

(defn gen-confirmation-query
  "Generates a confirmation query for an e-mail or a phone UPDATED by an existing user
  or used in other process (like password recovery).

  Note: it may return a query giving an empty result set if there is no requesting
  user in a database."
  [id-column dec-att? user-required?]
  (str-squeeze-spc
   "INSERT INTO confirmations(id,code,token,reason,id_type,expires,confirmed,attempts,requester_id,user_id,user_uid)"
   (str "SELECT ?,?,?,?,?,?,0,?,?,"
        "(SELECT users.id  FROM users WHERE users." (or (some-str id-column) "email") " = ?),"
        "(SELECT users.uid FROM users WHERE users." (or (some-str id-column) "email") " = ?)"
        (when user-required? " FROM users WHERE users.id = ?"))
   "ON DUPLICATE KEY UPDATE"
   "user_id      = IF(NOW()>expires, VALUE(user_id),      user_id),"
   "user_uid     = IF(NOW()>expires, VALUE(user_uid),     user_uid),"
   "requester_id = IF(NOW()>expires, VALUE(requester_id), requester_id),"
   "attempts     = IF(NOW()>expires, VALUE(attempts),"    (str (calc-attempts-query dec-att?) "),")
   "code         = IF(NOW()>expires, VALUE(code),         code),"
   "token        = IF(NOW()>expires, VALUE(token),        token),"
   "reason       = IF(NOW()>expires, VALUE(reason),       reason),"
   "id_type      = IF(NOW()>expires, VALUE(id_type),      id_type),"
   "created      = IF(NOW()>expires, NOW(),               confirmations.created),"
   "confirmed    = IF(NOW()>expires, VALUE(confirmed),    confirmed),"
   "req_id       = IF(NOW()>expires, NULL,                req_id),"
   "expires      = IF(NOW()>expires, VALUE(expires),      expires)"
   "RETURNING requester_id,user_id,user_uid,attempts,code,token,id_type,created,confirmed,expires"))

(def ^:const email-confirmation-query
  (gen-confirmation-query :email true true))

(def ^:const phone-confirmation-query
  (gen-confirmation-query :phone true true))

(def ^:const email-confirmation-query-without-attempt
  (gen-confirmation-query :email false true))

(def ^:const phone-confirmation-query-without-attempt
  (gen-confirmation-query :phone false true))

(def ^:const email-confirmation-query-nouser
  (gen-confirmation-query :email true false))

(def ^:const phone-confirmation-query-nouser
  (gen-confirmation-query :phone true false))

(def ^:const email-confirmation-query-without-attempt-nouser
  (gen-confirmation-query :email false false))

(def ^:const phone-confirmation-query-without-attempt-nouser
  (gen-confirmation-query :phone false false))

(defn- parse-id-type
  [id id-type]
  (if id-type
    (if (ident? id-type)
      (name id-type)
      (if-some [id-type (some-str id-type)]
        (name (symbol id-type))
        "email"))
    (if-some [id-type (if id (common/guess-identity-type id))]
      (name id-type)
      "email")))

(defn- gen-full-confirmation-core
  "Creates a confirmation code for the given identity (an e-mail address or a
  phone).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given identity (`id`) is already assigned to a registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set
  to ID of existing user, `:id` set to the given identity (as a string) and `:reason`
  set to the given reason (as a keyword, or `nil` if not given)."
  ([db query id exp udata]
   (gen-full-confirmation-core db query id exp udata nil (get udata :reason)))
  ([db query id exp udata id-type]
   (gen-full-confirmation-core db query id exp udata id-type (get udata :reason)))
  ([db query id exp udata id-type reason]
   (if db
     (if-some [id (some-str id)]
       (let [code    (if exp (gen-code))
             token   (if exp (gen-token))
             reason  (or (some-str reason) "creation")
             exp     (or exp ten-minutes)
             exp     (if (t/duration? exp) (t/hence exp) exp)
             udata   (mapv #(get udata %) [:max-attempts :account-type
                                           :first-name :middle-name :last-name
                                           :password :password-suite-id])
             id-type (parse-id-type id id-type)
             qargs   (list* query id code token reason id-type exp id id udata)]
         (if-some [r (jdbc/execute-one! db qargs db/opts-simple-map)]
           (let [user-id       (get r :user-id)
                 user-uid      (parse-uuid (str (get r :user-uid)))
                 requester-id  (get r :requester-id)
                 user-id?      (pos-int? user-id)
                 user-uid?     (uuid? user-uid)
                 requester-id? (pos-int? requester-id)]
             (-> r
                 (map/assoc-if user-id?      :existing-user/id   user-id)
                 (map/assoc-if user-uid?     :existing-user/uid user-uid)
                 (map/assoc-if requester-id? :user/id       requester-id)
                 (qassoc :exists? user-id? :confirmed? (pos-int? (get r :confirmed)))
                 (dissoc :confirmed :user-id :user-uid :requester-id)
                 (map/update-existing :account-type some-keyword)
                 (map/update-existing :reason       some-keyword)
                 (map/update-existing :id-type      some-keyword)))))))))

(defn- gen-confirmation-core
  "Creates a confirmation code for the given identity (an e-mail address or a
  phone).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given identity (`id`) is already assigned to a registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id`
  set to ID of existing user, `:id` set to the given identity (as a string) and
  `:reason` set to the given reason (as a keyword, or `nil` if not given)."
  ([db query id user-id exp attempts]
   (gen-confirmation-core db query id user-id exp attempts nil true "change"))
  ([db query id user-id exp attempts id-type]
   (gen-confirmation-core db query id user-id exp attempts id-type true "change"))
  ([db query id user-id exp attempts id-type user-required?]
   (gen-confirmation-core db query id user-id exp attempts id-type user-required? "change"))
  ([db query id user-id exp attempts id-type user-required? reason]
   (if db
     (if-some [id (some-str id)]
       (let [need-gen? (or user-required? (some? user-id))
             code      (if (and need-gen? exp) (gen-code))
             token     (if (and need-gen? exp) (gen-token))
             reason    (or (some-str reason) "change")
             exp       (or exp ten-minutes)
             exp       (if (t/duration? exp) (t/hence exp) exp)
             id-type   (parse-id-type id id-type)
             qargs     (if user-required?
                         [query id code token reason id-type exp attempts user-id id id user-id]
                         [query id code token reason id-type exp attempts user-id id id])]
         (if-some [r (jdbc/execute-one! db qargs db/opts-simple-map)]
           (let [user-id       (get r :user-id)
                 user-uid      (parse-uuid (str (get r :user-uid)))
                 requester-id  (get r :requester-id)
                 user-id?      (pos-int? user-id)
                 user-uid?     (uuid? user-uid)
                 requester-id? (pos-int? requester-id)]
             (-> r
                 (map/assoc-if user-id?      :existing-user/id     user-id)
                 (map/assoc-if user-uid?     :existing-user/uid   user-uid)
                 (map/assoc-if requester-id? :user/id         requester-id)
                 (qassoc :user/required? user-required?
                         :exists?        user-id?
                         :confirmed?     (pos-int? (get r :confirmed)))
                 (dissoc :confirmed :user-id :user-uid :requester-id)
                 (map/update-existing :reason  some-keyword)
                 (map/update-existing :id-type some-keyword)))))))))

;; Registration

(defn create-for-registration-without-attempt
  "Creates a confirmation code for a new user identified by the given e-mail
  address.

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to a registered user the returned map
  will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set to ID
  of existing user, `:id` set to the given e-mail (as a string) and `:reason`
  set to the given reason (as a keyword, or `nil` if not given)."
  ([udata]
   (create-for-registration-without-attempt (get udata :db) udata))
  ([db udata]
   (gen-full-confirmation-core db new-email-confirmation-query-without-attempt
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               :email
                               (or (get udata :reason) "creation")))
  ([db udata reason]
   (gen-full-confirmation-core db new-email-confirmation-query-without-attempt
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               :email
                               reason)))

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
   (gen-full-confirmation-core db new-email-confirmation-query
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               :email
                               (or (get udata :reason) "creation")))
  ([db udata reason]
   (gen-full-confirmation-core db new-email-confirmation-query
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               :email
                               reason)))

;; Generic confirmation for existing user

(defn create-without-attempt
  "Creates a confirmation code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail address or a
  phone number (if the `id-type` is set to `:phone`).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to some other registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set
  to ID of existing user, `:id` set to the given e-mail (as a string) and `:reason`
  set to the given reason (as a keyword, or `nil` if not given)."
  ([db id user-id exp attempts user-required? reason]
   (gen-confirmation-core
    db
    (if user-required?
      email-confirmation-query-without-attempt
      email-confirmation-query-without-attempt-nouser)
    id user-id exp attempts :email user-required? reason))
  ([db id user-id exp attempts id-type user-required? reason]
   (if (phone? id-type)
     (gen-confirmation-core
      db
      (if user-required?
        phone-confirmation-query-without-attempt
        phone-confirmation-query-without-attempt-nouser)
      (db/identity->str id) user-id exp attempts :phone user-required? reason)
     (gen-confirmation-core
      db
      (if user-required?
        email-confirmation-query-without-attempt
        email-confirmation-query-without-attempt-nouser)
      id user-id exp attempts :email user-required? reason))))

(defn create
  "Creates a confirmation code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail address or a
  phone number (if the `id-type` is set to `:phone`).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to some other registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set
  to ID of existing user, `:id` set to the given e-mail (as a string) and `:reason`
  set to the given reason (as a keyword, or `nil` if not given).

  Attempts counter is increased each time this function is called."
  ([db id user-id exp attempts user-required? reason]
   (gen-confirmation-core
    db (if user-required? email-confirmation-query email-confirmation-query-nouser)
    id user-id exp attempts :email user-required? :reason))
  ([db id user-id exp attempts id-type user-required? reason]
   (if (phone? id-type)
     (gen-confirmation-core
      db (if user-required? phone-confirmation-query phone-confirmation-query-nouser)
      (db/identity->str id) user-id exp attempts :phone user-required? reason)
     (gen-confirmation-core
      db (if user-required? email-confirmation-query email-confirmation-query-nouser)
      id user-id exp attempts :email user-required? reason))))

;; E-mail/phone change

(defn create-for-change-without-attempt
  "Creates a confirmation code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail address or a
  phone number (if the `id-type` is set to `:phone`).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to some other registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set
  to ID of existing user, `:id` set to the given e-mail (as a string) and `:reason` set
  to the given reason (as a keyword, or `nil` if not given)."
  ([db id user-id exp attempts]
   (create-without-attempt db id user-id exp attempts true "change"))
  ([db id user-id exp attempts id-type]
   (create-without-attempt db id user-id exp attempts id-type true "change")))

(defn create-for-change
  "Creates a confirmation code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail address or a
  phone number (if the `id-type` is set to `:phone`).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to some other registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:existing-user/id` set
  to ID of existing user, `:id` set to the given e-mail (as a string) and `:reason` set
  to the given reason (as a keyword, or `nil` if not given).

  Attempts counter is increased each time this function is called."
  ([db id user-id exp attempts]
   (create db id user-id exp attempts true "change"))
  ([db id user-id exp attempts id-type]
   (create db id user-id exp attempts id-type true "change")))

;; Password recovery

(defn create-for-recovery-without-attempt
  "Creates a recovery code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail address or a
  phone number (if the `id-type` is set to `:phone`).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is assigned to the given user the returned map will contain 4
  keys: `:exists?` set to `true`, `:existing-user/id` set to ID of existing user,
  `:id` set to the given e-mail (as a string) and `:reason` set to the given reason
  (as a keyword, or `nil` if not given)."
  ([db id user-id exp attempts]
   (create-without-attempt db id user-id exp attempts false "recovery"))
  ([db id user-id exp attempts id-type]
   (create-without-attempt db id user-id exp attempts id-type false "recovery")))

(defn create-for-recovery
  "Creates a recovery code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail address or a
  phone number (if the `id-type` is set to `:phone`).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is assigned to the given user the returned map will contain 4
  keys: `:exists?` set to `true`, `:existing-user/id` set to ID of existing user,
  `:id` set to the given e-mail (as a string) and `:reason` set to the given reason
  (as a keyword, or `nil` if not given).

  Attempts counter is increased each time this function is called."
  ([db id user-id exp attempts]
   (create db id user-id exp attempts false "recovery"))
  ([db id user-id exp attempts id-type]
   (create db id user-id exp attempts id-type false "recovery")))

;; Confirming identity with a token or code

(defn gen-report-errors-query
  "Generates SQL query for reporting errors found during confirmation process. When
  executed the query returns a map with boolean values and the following keys:
  `:confirmed` (already confirmed), `:attempts` (attempts exceeded),
  `:reason` (reason for the given token or code is different from the reason for
  which the confirmation had been created for), `:expires` (confirmation expired),
  `:present` (an e-mail or a phone number is already assigned to an existing user). "
  [where]
  (str-squeeze-spc
   "SELECT (confirmed = TRUE) AS confirmed,"
   "(attempts <= 0)           AS no_attempts,"
   "(reason <> ?)             AS bad_reason,"
   "(expires < NOW())         AS expired,"
   "(SELECT 1 FROM users WHERE users.email = confirmations.id"
   "                        OR users.phone = confirmations.id) AS present"
   "FROM confirmations" (if-some [w (some-str where)] (str "WHERE " w))))

(def ^:const report-errors-simple-id-query
  (gen-report-errors-query "id = ?"))

(def ^:const report-errors-id-query
  (gen-report-errors-query "id = ? AND reason = ?"))

(def ^:const report-errors-code-query
  (gen-report-errors-query "id = ? AND code = ?"))

(def ^:const report-errors-token-query
  (gen-report-errors-query "token = ?"))

(def verify-bad-id-set
  #{:verify/not-found :verify/bad-id})

(def verify-bad-code-set
  #{:verify/not-found :verify/bad-code})

(def verify-bad-token-set
  #{:verify/not-found :verify/bad-token})

(defn- process-errors
  [r should-be-confirmed?]
  (if r
    (let [r (reduce-kv #(if (pos-int? %3) (conj %1 (keyword "verify" (name %2))) %1) #{} r)]
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
         qargs  [report-errors-token-query reason token]]
     (or (process-errors (jdbc/execute-one! db qargs db/opts-simple-map) should-be-confirmed?)
         verify-bad-token-set)))
  ([db id code reason should-be-confirmed?]
   (let [id     (db/identity->str id)
         reason (or (some-str reason) "creation")
         qargs  (cond code          [report-errors-code-query      reason id code]
                      (false? code) [report-errors-simple-id-query reason id]
                      :no-code      [report-errors-id-query        reason id reason])]
     (or (process-errors (jdbc/execute-one! db qargs db/opts-simple-map) should-be-confirmed?)
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
       (if-some [id (db/identity->str id)]
         (if-some [dst-id (cond (str/index-of id \@ 1) email-id
                                (= (first id) \+)      phone-id)]
           (conj (disj errs src-id) dst-id)
           errs)
         errs)
       errs)))
  ([errs src-id dst-id]
   (if errs
     (if (contains? errs src-id)
       (conj (disj errs src-id) dst-id)
       errs))))

(defn code-to-token
  "Returns a confirmation token associated with the given confirmation code and
  identity. Additionally returns confirmation status."
  [db id code]
  (if-some [r (first
               (sql/find-by-keys
                db :confirmations
                {:id id :code code}
                (qassoc db/opts-simple-map :columns [:token :confirmed])))]
    (-> (qassoc r :confirmed? (pos-int? (get r :confirmed)))
        (dissoc :confirmed))))

(def confirm-token-query
  (str-squeeze-spc
   "INSERT IGNORE INTO confirmations(id,reason,expires,confirmed)"
   "SELECT id,reason,expires,confirmed from confirmations"
   "WHERE token = ? AND reason = ? AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE"
   "expires   = IF(VALUE(confirmed) = TRUE, VALUE(expires), DATE_ADD(VALUE(expires), INTERVAL ? MINUTE)),"
   "confirmed = TRUE"
   "RETURNING id AS identity,id_type,reason,confirmed,token,code,requester_id"))

(def confirm-code-query
  (str-squeeze-spc
   "INSERT IGNORE INTO confirmations(id,reason,expires,confirmed)"
   "SELECT id,reason,expires,confirmed from confirmations"
   "WHERE id = ? AND code = ? AND reason = ? AND expires >= NOW()"
   "ON DUPLICATE KEY UPDATE"
   "expires   = IF(VALUE(confirmed) = TRUE, VALUE(expires), DATE_ADD(VALUE(expires), INTERVAL ? MINUTE)),"
   "confirmed = TRUE"
   "RETURNING id AS identity,id_type,reason,confirmed,token,code,requester_id"))

(defn establish
  "Confirms an identity (`id`), which may be an e-mail or a phone number, using a token
  or an identifier with a code. If the verification is successful, sets `confirmed`
  flag to `TRUE` (1) in a database which prevents from further confirmations and
  marks identity as confirmed for other operations. The `exp-inc` argument should be
  a positive integer and will be used to increase expiration time by the given amount
  of minutes. This is to ensure that the next operation, if any, which may take some
  time, will succeed. The `reason` argument is the confirmation reason and should
  match the reason given during the generation of a token or code.

  Returns a map with `:confirmed?` set to `true` if the given token or code was
  verified. Returns a map with `:confirmed?` set to `false` and `:error` set to a
  keyword describing the cause if the token or code was not verified. Returns `nil`
  if something went wrong during the interaction with a database or when the required
  input parameters were empty.

  The `:user/id` key of a result, if exists, contains numeric user identifier of a
  requester (the user for whom the verification was initiated).

  If the identity is already confirmed and there is no error (i.e. confirmation has
  not yet expired), it will also return a map with `:confirmed?` set to `true`."
  ([db id code exp-inc reason]
   (let [reason  (or (some-str reason) "creation")
         id      (db/identity->str id)
         code    (some-str code)
         exp-inc (time/minutes exp-inc 1)]
     (if (and id code)
       (let [r          (jdbc/execute-one! db [confirm-code-query id code reason exp-inc]
                                           db/opts-simple-map)
             confirmed? (if r (pos-int? (get r :confirmed)))
             user-id    (if r (get r :requester-id))]
         (if confirmed?
           (qassoc r :confirmed? true
                   :id-type     (some-keyword (get r :id-type))
                   :user/id     user-id)
           (let [errs (report-errors db id code reason false)
                 errs (specific-id errs id :verify/bad-id :verify/bad-email :verify/bad-phone)]
             (if r
               {:confirmed? false
                :id-type    (some-keyword (get r :id-type))
                :identity   id
                :user/id    user-id
                :code       code
                :errors     errs}
               {:confirmed? false
                :identity   id
                :user/id    nil
                :code       code
                :errors     errs})))))))
  ([db id code token exp-inc reason]
   (if-some [token (some-str token)]
     (establish db token   exp-inc reason)
     (establish db id code exp-inc reason)))
  ([db token exp-inc reason]
   (if-some [token (some-str token)]
     (let [reason  (or (some-str reason) "creation")
           exp-inc (time/minutes exp-inc 1)]
       (let [r          (jdbc/execute-one! db [confirm-token-query token reason exp-inc]
                                           db/opts-simple-map)
             confirmed? (if r (pos-int? (get r :confirmed)))
             user-id    (if r (get r :requester-id))]
         (if confirmed?
           (qassoc r
                   :confirmed? true
                   :id-type   (some-keyword (get r :id-type))
                   :user/id   user-id)
           (let [errs (report-errors db token reason false)]
             (if r
               {:confirmed? false
                :identity   (get r :identity)
                :id-type    (some-keyword (get r :id-type))
                :user/id    user-id
                :token      token
                :errors     errs}
               {:confirmed? false
                :user/id    nil
                :token      token
                :errors     errs}))))))))

(defn delete
  "Deletes confirmation identified with an `id` from a database."
  ([db id]
   (delete db id "creation"))
  ([db id reason]
   (if id
     (let [reason (or (some-str reason) "creation")]
       (sql/delete! db :confirmations {:id id :reason reason})))))

;; Updating attempts

(def ^:const decrease-attempts-query
  (str-squeeze-spc
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
    (if-some [id (db/identity->str id)]
      (let [reason (or (some-str reason) "creation")]
        (if-some [r (jdbc/execute-one! db [decrease-attempts-query id reason] db/opts-simple-map)]
          (-> r
              (qassoc :exists?    (pos-int? (get r :user-id))
                      :confirmed? (pos-int? (get r :confirmed)))
              (dissoc :confirmed)
              (map/update-existing :user-uid (comp parse-uuid str))
              (map/update-existing :account-type some-keyword))
          (let [errs (report-errors db id nil reason false)
                errs (specific-id errs id :verify/bad-id :verify/bad-email :verify/bad-phone)]
            {:confirmed? false
             :errors     errs}))))))

;; Retries

(defn retry-email
  ([udata]
   (decrease-attempts-core (get udata :db) (get udata :email) (get udata :reason)))
  ([db id]
   (decrease-attempts-core db id "creation"))
  ([db id reason]
   (decrease-attempts-core db id reason)))

(defn retry-phone
  ([udata]
   (decrease-attempts-core (get udata :db) (get udata :phone) (get udata :reason)))
  ([db id]
   (decrease-attempts-core db id "creation"))
  ([db id reason]
   (decrease-attempts-core db id reason)))

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
         (if-some [id (db/identity->str id)]
           (sql/update! db :confirmations
                        {:req-id request-id}
                        {:id id :code code}
                        db/opts-simple-map))))))
  ([db id code token request-id]
   (if-some [token (some-str token)]
     (update-request-id db token request-id)
     (update-request-id db id code request-id))))
