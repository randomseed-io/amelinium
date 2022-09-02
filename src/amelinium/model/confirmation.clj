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
            [taoensso.nippy           :as         nippy]
            [tick.core                :as             t]
            [buddy.core.hash          :as          hash]
            [buddy.core.codecs        :as        codecs]
            [clj-uuid                 :as          uuid]
            [amelinium.db             :as            db]
            [io.randomseed.utils.time :as          time]
            [io.randomseed.utils.map  :as           map]
            [io.randomseed.utils.ip   :as            ip]
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

(defn gen-confirmation-query
  [id-column inc-att?]
  (str-squeeze-spc
   "INSERT INTO confirmations(id,code,token,reason,expires,confirmed,user_id,"
   "account_type,first_name,middle_name,last_name,password,password_suite_id)"
   (str "SELECT ?,?,?,?,?,0,(SELECT users.id FROM users WHERE users."
        (or (some-str id-column) "email") " = ?),?,?,?,?,?,?")
   "ON DUPLICATE KEY UPDATE"
   "user_id           = VALUE(user_id),"
   "attempts          = IF(NOW()>expires, 1," (str "attempts" (if inc-att? " + 1") "),")
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
   "expires           = IF(NOW()>expires, VALUE(expires),      expires)"
   "RETURNING user_id, account_type, attempts, code, token, created, confirmed, expires"))

(def ^:const new-email-confirmation-query
  (gen-confirmation-query :email true))

(def ^:const new-phone-confirmation-query
  (gen-confirmation-query :phone true))

(def ^:const new-email-confirmation-query-without-attempt
  (gen-confirmation-query :email false))

(def ^:const new-phone-confirmation-query-without-attempt
  (gen-confirmation-query :phone false))

(defn- gen-confirmation-core
  "Creates a confirmation code for the given identity (an e-mail address or a
  phone). When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given identity (`id`) is already
  assigned to a registered user the returned map will contain 4 keys: `:exists?` set
  to `true`, `:user/id` set to ID of existing user, `:id` set to the given
  identity (as a string) and `:reason` set to the given reason (as a keyword or `nil`
  if not given)."
  ([db query id exp udata]
   (gen-confirmation-core db query id exp udata nil true))
  ([db query id exp udata reason]
   (if db
     (if-some [id (some-str id)]
       (let [code   (if exp (gen-code))
             token  (if exp (gen-token))
             reason (or (some-str reason) "creation")
             exp    (or exp ten-minutes)
             exp    (if (t/duration? exp) (t/hence exp) exp)
             udata  (mapv #(get udata %) [:account-type
                                          :first-name :middle-name :last-name
                                          :password :password-suite-id])
             query  (list* query id code token reason exp id udata)]
         (if-some [r (jdbc/execute-one! db query db/opts-simple-map)]
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
  ([udata]
   (create-for-registration-without-attempt (get udata :db) udata))
  ([db udata]
   (gen-confirmation-core db
                          new-email-confirmation-query-without-attempt
                          (get udata :email)
                          (get udata :expires-in)
                          udata
                          "creation"))
  ([db udata reason]
   (gen-confirmation-core db
                          new-email-confirmation-query-without-attempt
                          (get udata :email)
                          (get udata :expires-in)
                          udata
                          reason)))

(defn create-for-registration
  "Creates a confirmation code for a new user identified by the given e-mail
  address. When the confirmation was already generated and it hasn't expired, it is
  returned with an existing code and token. When the given e-mail is already assigned
  to a registered user the returned map will contain 4 keys: `:exists?` set to
  `true`, `:user/id` set to ID of existing user, `:id` set to the given e-mail (as a
  string) and `:reason` set to the given reason (as a keyword or `nil` if not
  given). Attempts counter is increased each time this function is called."
  ([udata]
   (create-for-registration (get udata :db) udata))
  ([db udata]
   (gen-confirmation-core db
                          new-email-confirmation-query
                          (get udata :email)
                          (get udata :expires-in)
                          udata
                          "creation"))
  ([db udata reason]
   (gen-confirmation-core db
                          new-email-confirmation-query
                          (get udata :email)
                          (get udata :expires-in)
                          udata
                          reason)))

;; Updating attempts

(def ^:const resent-confirmation-query
  (str-spc
   "INSERT IGNORE INTO confirmations"
   "SELECT * FROM confirmations WHERE id=? AND reason=?"
   "ON DUPLICATE KEY UPDATE"
   "attempts = IF((NOW() > expires OR attempts >= ?), attempts, attempts + 1)"
   "RETURNING user_id, account_type, attempts, code, token, created, confirmed, expires"))

(defn update-attempts-core
  [db query id max-attempts reason]
  (if db
    (if-some [id (some-str id)]
      (let [reason (or (some-str reason) "creation")]
        (if-some [r (jdbc/execute-one! db [query id max-attempts reason])]
          r)))))

(defn update-email-attempts
  "Updates attempt count for an email confirmation entry. The given reason must be the
  same as the reason stored for the e-mail."
  ([udata]
   (update-email-attempts (get udata :db) udata))
  ([db udata]
   (update-attempts-core db resent-confirmation-query
                         (get udata :email)
                         (get udata :max-attempts)
                         "creation"))
  ([db udata reason]
   (update-attempts-core db resent-confirmation-query
                         (get udata :email)
                         (get udata :max-attempts)
                         reason)))

(defn update-phone-attempts
  "Updates attempt count for a phone number confirmation entry. The given reason must
  be the same as the reason stored for the e-mail."
  ([udata]
   (update-phone-attempts (get udata :db) udata))
  ([db udata]
   (update-attempts-core db resent-confirmation-query
                         (get udata :phone)
                         (get udata :max-attempts)
                         "creation"))
  ([db udata reason]
   (update-attempts-core db resent-confirmation-query
                         (get udata :phone)
                         (get udata :max-attempts)
                         reason)))

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

(def ^:private ^:const errs-prioritized
  [:bad-result
   :verify/bad-result
   :verify/not-found
   :verify/bad-token
   :verify/bad-code
   :verify/bad-id
   :verify/bad-reason
   :verify/expired
   :verify/exists
   :verify/not-confirmed
   :verify/confirmed])

(defn most-significant-error
  [errors]
  (if errors (some errors errs-prioritized)))

(defn- process-errors
  [r should-be-confirmed?]
  (let [r (reduce-kv #(if (pos-int? %3) (conj %1 (keyword "verify" (name %2))) %1) #{} r)]
    (if (contains? r :verify/confirmed)
      (if should-be-confirmed? (disj r :verify/confirmed) r)
      (if should-be-confirmed? (conj r :verify/not-confirmed) r))))

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
     (or (process-errors (jdbc/execute-one! db qargs db/opts-simple-map))
         verify-bad-token-set)))
  ([db id code reason should-be-confirmed?]
   (let [id     (some-str id)
         reason (or (some-str reason) "creation")
         qargs  (cond code          [report-errors-code-query      reason id code]
                      (false? code) [report-errors-simple-id-query reason id reason]
                      :no-code      [report-errors-id-query        reason id])]
     (or (process-errors (jdbc/execute-one! db qargs db/opts-simple-map) should-be-confirmed?)
         (if code verify-bad-code-set verify-bad-id-set))))
  ([db id token code reason should-be-confirmed?]
   (if token
     (report-errors db token   reason should-be-confirmed?)
     (report-errors db id code reason should-be-confirmed?))))

(defn code-to-token
  "Returns a confirmation token associated with the given confirmation code and
  identity. Additionally returns confirmation status."
  [db id code]
  (if-some [r (first
               (sql/find-by-keys
                db :confirmations
                {:id id :code code}
                (assoc db/opts-simple-map :columns [:token :confirmed])))]
    (-> r
        (assoc  :confirmed? (pos-int? (get r :confirmed)))
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
           (let [errs (report-errors db id code reason false)]
             (if (and (= 1 (count errs)) (contains? errs :verify/confirmed))
               {:confirmed? true}
               {:confirmed? false
                :errors     errs
                :error      (most-significant-error errs)})))))))
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
                  :errors     errs
                  :error      (most-significant-error errs)})))))))))

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
   "         WHERE id = ? AND confirmed = FALSE AND attempts > 0"
   "               AND NOW() <= expires AND reason = ?"
   "ON DUPLICATE KEY UPDATE"
   "attempts = attempts - 1"
   "RETURNING id, user_id, account_type, attempts, code, token, created, confirmed, expires"))

(defn- decrease-attempts-core
  [db id reason]
  (if db
    (if-some [id (some-str id)]
      (let [reason (or (some-str reason) "creation")]
        (if-some [r (jdbc/execute-one! db [decrease-attempts-query id reason] db/opts-simple-map)]
          (dissoc (assoc r :confirmed? false) :confirmed)
          (let [errs (report-errors db id nil reason false)]
            {:errors errs
             :error  (most-significant-error errs)}))))))

(defn retry-email
  ([udata]
   (decrease-attempts-core (:db udata) (:email udata)))
  ([db id]
   (decrease-attempts-core db id "creation"))
  ([db id reason]
   (decrease-attempts-core db id reason)))

(defn retry-phone
  ([udata]
   (decrease-attempts-core (:db udata) (:phone udata)))
  ([db id]
   (decrease-attempts-core db id "creation"))
  ([db id reason]
   (decrease-attempts-core db id reason)))
