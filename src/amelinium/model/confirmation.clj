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
  (format "%07d" (unchecked-int (rand 9999999))))

(defn gen-token
  []
  (-> (random-uuid) uuid/to-byte-array hash/md5 codecs/bytes->hex))

(defn phone-exists?
  [db phone]
  (if db
    (if-some [phone (some-str phone)]
      (-> (jdbc/execute-one! db [phone-exists-query phone] db/opts-simple-vec)
          first some?))))

(defn email-exists?
  [db email]
  (if db
    (if-some [email (some-str email)]
      (-> (jdbc/execute-one! db [email-exists-query email] db/opts-simple-vec)
          first some?))))

;; Generation of confirmation tokens and codes

(defn- calc-attempts-query
  [dec-att?]
  (if dec-att?
    (str "IF(attempts > 0, attempts - 1, attempts)")
    (str "attempts")))

(defn gen-full-confirmation-query
  "Generates a confirmation query for an e-mail or a phone used during registration of
  a new user."
  [id-column dec-att?]
  (str-squeeze-spc
   "INSERT INTO confirmations(id,code,token,reason,expires,confirmed,user_id,user_uid,"
   "attempts,account_type,first_name,middle_name,last_name,password,password_suite_id)"
   (str "SELECT ?,?,?,?,?,0,"
        " (SELECT users.id  FROM users WHERE users." (or (some-str id-column) "email") " = ?),"
        " (SELECT users.uid FROM users WHERE users." (or (some-str id-column) "email") " = ?),"
        "?,?,?,?,?,?,?")
   "ON DUPLICATE KEY UPDATE"
   "user_id           = IF(NOW()>expires, VALUE(user_id),      user_id),"
   "user_uid          = IF(NOW()>expires, VALUE(user_uid),     user_uid),"
   "attempts          = IF(NOW()>expires, VALUE(attempts),"    (str (calc-attempts-query dec-att?) "),")
   "code              = IF(NOW()>expires, VALUE(code),         code),"
   "token             = IF(NOW()>expires, VALUE(token),        token),"
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
   "RETURNING user_id,user_uid,account_type,attempts,code,token,created,confirmed,expires"))

(def ^:const new-email-confirmation-query
  (gen-full-confirmation-query :email true))

(def ^:const new-phone-confirmation-query
  (gen-full-confirmation-query :phone true))

(def ^:const new-email-confirmation-query-without-attempt
  (gen-full-confirmation-query :email false))

(def ^:const new-phone-confirmation-query-without-attempt
  (gen-full-confirmation-query :phone false))

(defn gen-confirmation-query
  "Generates a confirmation query for an e-mail or a phone updated by an existing
  user."
  [id-column dec-att?]
  (str-squeeze-spc
   "INSERT INTO confirmations(id,code,token,reason,expires,confirmed,attempts,requester_id,user_id,user_uid)"
   (str "SELECT ?,?,?,?,?,0,?,?"
        "(SELECT users.id  FROM users WHERE users." (or (some-str id-column) "email") " = ?),"
        "(SELECT users.uid FROM users WHERE users." (or (some-str id-column) "email") " = ?),"
        " FROM users"
        " WHERE users.id = ? AND users." (or (some-str id-column) "email") " <> ?")
   "ON DUPLICATE KEY UPDATE"
   "user_id      = IF(NOW()>expires, VALUE(user_id),      user_id),"
   "user_uid     = IF(NOW()>expires, VALUE(user_uid),     user_uid),"
   "requester_id = IF(NOW()>expires, VALUE(requester),    requester),"
   "attempts     = IF(NOW()>expires, VALUE(attempts),"    (str (calc-attempts-query dec-att?) "),")
   "code         = IF(NOW()>expires, VALUE(code),         code),"
   "token        = IF(NOW()>expires, VALUE(token),        token),"
   "created      = IF(NOW()>expires, NOW(),               created),"
   "confirmed    = IF(NOW()>expires, VALUE(confirmed),    confirmed),"
   "req_id       = IF(NOW()>expires, NULL,                req_id),"
   "expires      = IF(NOW()>expires, VALUE(expires),      expires)"
   "RETURNING requester_id,user_id,user_uid,attempts,code,token,created,confirmed,expires"))

(def ^:const email-confirmation-query
  (gen-confirmation-query :email true))

(def ^:const phone-confirmation-query
  (gen-confirmation-query :phone true))

(def ^:const email-confirmation-query-without-attempt
  (gen-confirmation-query :email false))

(def ^:const phone-confirmation-query-without-attempt
  (gen-confirmation-query :phone false))

(defn- gen-full-confirmation-core
  "Creates a confirmation code for the given identity (an e-mail address or a
  phone).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given identity (`id`) is already assigned to a registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:user/id` set to ID of
  existing user, `:id` set to the given identity (as a string) and `:reason` set to
  the given reason (as a keyword or `nil` if not given)."
  ([db query id exp udata]
   (gen-full-confirmation-core db query id exp udata (get udata :reason)))
  ([db query id exp udata reason]
   (if db
     (if-some [id (some-str id)]
       (let [code   (if exp (gen-code))
             token  (if exp (gen-token))
             reason (or (some-str reason) "creation")
             exp    (or exp ten-minutes)
             exp    (if (t/duration? exp) (t/hence exp) exp)
             udata  (mapv #(get udata %) [:max-attempts :account-type
                                          :first-name :middle-name :last-name
                                          :password :password-suite-id])
             qargs  (list* query id code token reason exp id id udata)]
         (if-some [r (jdbc/execute-one! db qargs db/opts-simple-map)]
           (let [user-id       (get r :user-id)
                 user-uid      (parse-uuid (str (get r :user-uid)))
                 requester-id  (get r :requester-id)
                 user-id?      (pos-int? user-id)
                 user-uid?     (uuid? user-uid)
                 requester-id? (pos-int? requester-id)]
             (-> r
                 (map/assoc-if user-id?      :user/id      user-id)
                 (map/assoc-if user-uid?     :user/uid     user-uid)
                 (map/assoc-if requester-id? :requester/id requester-id)
                 (qassoc :exists? user-id? :confirmed? (pos-int? (get r :confirmed)))
                 (dissoc :confirmed :user-id :user-uuid :requester-id)
                 (map/update-existing :account-type some-keyword)
                 (map/update-existing :reason some-keyword)))))))))

(defn- gen-confirmation-core
  "Creates a confirmation code for the given identity (an e-mail address or a
  phone).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given identity (`id`) is already assigned to a registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:user/id` set to ID of
  existing user, `:id` set to the given identity (as a string) and `:reason` set to
  the given reason (as a keyword or `nil` if not given).

  The query will return no result when the given identity (`id`) is already assigned
  to the requesting user (identified by `user-id`)."
  ([db query id user-id exp attempts]
   (gen-confirmation-core db query id exp "change"))
  ([db query id user-id exp attempts reason]
   (if db
     (if-some [id (some-str id)]
       (let [code   (if exp (gen-code))
             token  (if exp (gen-token))
             reason (or (some-str reason) "change")
             exp    (or exp ten-minutes)
             exp    (if (t/duration? exp) (t/hence exp) exp)
             qargs  (query id code token reason exp attempts user-id id id user-id id)]
         (if-some [r (jdbc/execute-one! db qargs db/opts-simple-map)]
           (let [user-id       (get r :user-id)
                 user-uid      (parse-uuid (str (get r :user-uid)))
                 requester-id  (get r :requester-id)
                 user-id?      (pos-int? user-id)
                 user-uid?     (uuid? user-uid)
                 requester-id? (pos-int? requester-id)]
             (-> r
                 (map/assoc-if requester-id? :requester/id requester-id)
                 (map/assoc-if user-id?      :user/id      user-id)
                 (map/assoc-if user-uid?     :user/uid     user-uid)
                 (qassoc :exists? user-id? :confirmed? (pos-int? (get r :confirmed)))
                 (dissoc :confirmed :user-id :user-uuid :requester-id)
                 (map/update-existing :reason some-keyword)))))))))

(defn create-for-registration-without-attempt
  "Creates a confirmation code for a new user identified by the given e-mail
  address.

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to a registered user the returned map
  will contain 4 keys: `:exists?` set to `true`, `:user/id` set to ID of existing
  user, `:id` set to the given e-mail (as a string) and `:reason` set to the given
  reason (as a keyword or `nil` if not given)."
  ([udata]
   (create-for-registration-without-attempt (get udata :db) udata))
  ([db udata]
   (gen-full-confirmation-core db
                               new-email-confirmation-query-without-attempt
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               (or (get udata :reason) "creation")))
  ([db udata reason]
   (gen-full-confirmation-core db
                               new-email-confirmation-query-without-attempt
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               reason)))

(defn create-for-registration
  "Creates a confirmation code for a new user identified by the given e-mail
  address.

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to a registered user the returned map
  will contain 4 keys: `:exists?` set to `true`, `:user/id` set to ID of existing
  user, `:id` set to the given e-mail (as a string) and `:reason` set to the given
  reason (as a keyword or `nil` if not given). Attempts counter is increased each
  time this function is called."
  ([udata]
   (create-for-registration (get udata :db) udata))
  ([db udata]
   (gen-full-confirmation-core db
                               new-email-confirmation-query
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               (or (get udata :reason) "creation")))
  ([db udata reason]
   (gen-full-confirmation-core db
                               new-email-confirmation-query
                               (get udata :email)
                               (get udata :expires-in)
                               udata
                               reason)))

(defn create-for-change-without-attempt
  "Creates a confirmation code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail address or a
  phone number (if the `id-type` is set to `:phone`).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to some other registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:user/id` set to ID of
  existing user, `:id` set to the given e-mail (as a string) and `:reason` set to the
  given reason (as a keyword or `nil` if not given). Attempts counter is increased
  each time this function is called."
  ([db id user-id exp attempts]
   (gen-confirmation-core db
                          email-confirmation-query-without-attempt
                          id user-id exp attempts "change"))
  ([db id user-id exp attempts id-type]
   (gen-confirmation-core db
                          (if (= id-type :phone)
                            phone-confirmation-query-without-attempt
                            email-confirmation-query-without-attempt)
                          id user-id exp attempts "change"))
  ([db id user-id exp attempts id-type reason]
   (gen-confirmation-core db
                          (if (= id-type :phone)
                            phone-confirmation-query-without-attempt
                            email-confirmation-query-without-attempt)
                          id user-id exp attempts reason)))

(defn create-for-change
  "Creates a confirmation code for an existing user identified by the given user
  ID (`user-id`). The identity to be confirmed (`id`) can be an e-mail address or a
  phone number (if the `id-type` is set to `:phone`).

  When the confirmation was already generated and it hasn't expired, it is returned
  with an existing code and token.

  When the given e-mail is already assigned to some other registered user the
  returned map will contain 4 keys: `:exists?` set to `true`, `:user/id` set to ID of
  existing user, `:id` set to the given e-mail (as a string) and `:reason` set to the
  given reason (as a keyword or `nil` if not given). Attempts counter is increased
  each time this function is called."
  ([db id user-id exp attempts]
   (gen-confirmation-core db
                          email-confirmation-query
                          id user-id exp attempts "change"))
  ([db id user-id exp attempts id-type]
   (gen-confirmation-core db
                          (if (= :phone (name id-type))
                            phone-confirmation-query
                            email-confirmation-query)
                          id user-id exp attempts "change"))
  ([db id user-id exp attempts id-type reason]
   (gen-confirmation-core db
                          (if (= :phone (name id-type))
                            phone-confirmation-query
                            email-confirmation-query)
                          id user-id exp attempts reason)))

;; Confirming identity with a token or code

(defn gen-report-errors-query
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
   (let [id     (some-str id)
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
       (if-some [id (some-str id)]
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
   "UPDATE confirmations"
   "SET expires = DATE_ADD(expires, INTERVAL ? MINUTE), confirmed = TRUE"
   "WHERE token = ? AND confirmed <> TRUE AND reason = ? AND expires >= NOW()"))

(def confirm-code-query
  (str-squeeze-spc
   "UPDATE confirmations"
   "SET expires = DATE_ADD(expires, INTERVAL ? MINUTE), confirmed = TRUE"
   "WHERE id = ? AND code = ? AND confirmed <> TRUE AND reason = ? AND expires >= NOW()"))

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

  If the identity is already confirmed and there is no error (i.e. confirmation has
  not yet expired), it will also return a map with `:confirmed?` set to `true`."
  ([db id code exp-inc reason]
   (let [reason (or (some-str reason) "creation")
         id     (some-str id)
         code   (some-str code)]
     (if (and id code (pos-int? exp-inc))
       (if-some [r (::jdbc/update-count
                    (jdbc/execute-one! db [confirm-code-query exp-inc id code reason]
                                       db/opts-simple-map))]
         (if (pos-int? r)
           {:confirmed? true}
           (let [errs (report-errors db id code reason false)
                 errs (specific-id errs id :verify/bad-id :verify/bad-email :verify/bad-phone)]
             (if (and (= 1 (count errs)) (contains? errs :verify/confirmed))
               {:confirmed? true}
               {:confirmed? false
                :errors     errs})))))))
  ([db id code token exp-inc reason]
   (if-some [token (some-str token)]
     (establish db token   exp-inc reason)
     (establish db id code exp-inc reason)))
  ([db token exp-inc reason]
   (if-some [token (some-str token)]
     (let [reason  (or (some-str reason) "creation")
           exp-inc (time/minutes exp-inc 1)]
       (if-some [r (::jdbc/update-count
                    (jdbc/execute-one! db [confirm-token-query exp-inc token reason]
                                       db/opts-simple-map))]
         (if (int? r)
           (if (pos-int? r)
             {:confirmed? true}
             (let [errs (report-errors db token reason false)]
               (if (and (= 1 (count errs)) (contains? errs :verify/confirmed))
                 {:confirmed true}
                 {:confirmed? false
                  :token      token
                  :errors     errs})))))))))

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
    (if-some [id (some-str id)]
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
         (if-some [id (some-str id)]
           (sql/update! db :confirmations
                        {:req-id request-id}
                        {:id id :code code}
                        db/opts-simple-map))))))
  ([db id code token request-id]
   (if-some [token (some-str token)]
     (update-request-id db token request-id)
     (update-request-id db id code request-id))))
