(ns

    ^{:doc    "amelinium service, session middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.session

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                  :as        set]
            [clojure.string               :as        str]
            [tick.core                    :as          t]
            [buddy.core.hash              :as       hash]
            [buddy.core.codecs            :as     codecs]
            [taoensso.nippy               :as      nippy]
            [next.jdbc.sql                :as        sql]
            [next.jdbc                    :as       jdbc]
            [amelinium.db                 :as         db]
            [amelinium.logging            :as        log]
            [amelinium.system             :as     system]
            [io.randomseed.utils          :refer    :all]
            [io.randomseed.utils.time     :as       time]
            [io.randomseed.utils.var      :as        var]
            [io.randomseed.utils.map      :as        map]
            [io.randomseed.utils.ip       :as         ip]
            [io.randomseed.utils.db.types :as      types]))

(def ^:const sid-match (re-pattern "[a-f0-9]{30,128}"))

;; Session validation

(defn ip-state
  [smap user-id user-email remote-ip]
  (when-some [session-ip (or (get smap :ip) (get smap :ip-address))]
    (if-some [remote-ip (ip/to-address remote-ip)]
      (when-not (or (= (ip/to-v6 remote-ip) (ip/to-v6 session-ip))
                    (= (ip/to-v4 remote-ip) (ip/to-v4 session-ip)))
        {:cause    :bad-ip
         :reason   (str-spc "Session IP address" (str "(" (ip/plain-ip-str session-ip) ")")
                            "is different than the remote IP address"
                            (str "(" (ip/plain-ip-str remote-ip) ")")
                            (log/for-user user-id user-email))
         :severity :warn})
      (when-some [str-addr (ip/to-str remote-ip)]
        (when-not (or (= str-addr (ip/to-str session-ip))
                      (= str-addr (ip/to-str (ip/to-v4 session-ip)))
                      (= str-addr (ip/to-str (ip/to-v6 session-ip))))
          {:cause    :bad-ip
           :reason   (str-spc "Session IP string" (str "(" (ip/to-str remote-ip) ")")
                              "is different than the remote IP string"
                              (str "(" str-addr ")")
                              (log/for-user user-id user-email))
           :severity :warn})))))

(defn same-ip?
  ([state-result]
   (nil? state-result))
  ([smap user-id user-email remote-ip]
   (nil? (ip-state smap user-id user-email remote-ip))))

(defn time-exceeded?
  ([dur max-dur]
   (t/> dur max-dur))
  ([t-start t-stop max-dur]
   (t/> (t/between t-start t-stop) max-dur)))

(defn expired?
  ([smap opts]
   (when-some [exp (get opts :expires)]
     (and (pos-int? (time/seconds exp))
          (time-exceeded? (get smap :active) (t/now) exp)))))

(defn hard-expired?
  [smap opts]
  (when-some [hexp (get opts :hard-expires)]
    (and (pos-int? (time/seconds hexp))
         (time-exceeded? (get smap :active) (t/now) hexp))))

(defn soft-expired?
  [smap opts]
  (and (expired? smap opts)
       (not (hard-expired? smap opts))))

(defn sid-valid?
  [sid]
  (and sid (string? sid) (<= 30 (count sid) 128) (re-matches sid-match sid)))

(defn created-valid?
  [smap]
  (t/instant? (get smap :created)))

(defn active-valid?
  [smap]
  (t/instant? (get smap :active)))

(defn state
  "Returns session state. If there is anything wrong it returns an error
  string. Otherwise it returns nil. Unknown session detection is performed by
  checking if a value associated with the `:id` key is `nil` and a value associated
  with the `:err/id` key is not `nil`."
  ([smap opts ip-address]
   (if-not (and smap (map? smap))
     {:cause    :no-session-map
      :reason   (str-spc "No session map:" smap)
      :severity :info}
     (let [sid        (get smap :id)
           esid       (get smap :err/id)
           any-sid    (or sid esid)
           user-id    (valuable (get smap :user/id))
           user-email (some-str (get smap :user/email))
           for-user   (delay (log/for-user user-id user-email
                                           (ip/plain-ip-str ip-address)))]
       (cond
         (not any-sid)               {:cause    :no-session-id
                                      :reason   (some-str-spc "No session ID" @for-user)
                                      :severity :info}
         (not sid)                   {:cause    :unknown-session-id
                                      :reason   (some-str-spc "Unknown session ID" esid @for-user)
                                      :severity :info}
         (not (sid-valid? any-sid))  {:cause    :malformed-session-id
                                      :reason   (str "Malformed session ID " @for-user)
                                      :severity :info}
         (not user-id)               {:cause    :malformed-user-id
                                      :reason   (str "User ID not found or malformed " @for-user)
                                      :severity :info}
         (not user-email)            {:cause    :malformed-user-email
                                      :reason   (str "User e-mail not found or malformed " @for-user)
                                      :severity :info}
         (not (created-valid? smap)) {:cause    :bad-creation-time
                                      :reason   (str "No creation time " @for-user)
                                      :severity :warn}
         (not (active-valid? smap))  {:cause    :bad-last-active-time
                                      :reason   (str "No last active time " @for-user)
                                      :severity :warn}
         (expired? smap opts)        {:cause    :expired
                                      :reason   (str "Session expired " @for-user)
                                      :severity :info}
         :ip-address-check           (ip-state smap user-id user-email ip-address ))))))

(defn correct?
  ([state-result]         (nil? state-result))
  ([smap opts ip-address] (nil? (state smap opts ip-address))))

(defn valid?
  [smap]
  (boolean (get smap :valid?)))

;; SID generation

(defn gen-session-id
  [& args]
  (codecs/bytes->hex
   (hash/md5
    (str (apply str args) (time/timestamp) (gen-digits 10)))))

;; SQL

(defn get-last-active
  [db sid]
  (first (jdbc/execute-one! db ["SELECT active FROM sessions WHERE id = ?" sid]
                            db/opts-simple-vec)))

(defn update-last-active
  ([db sid]
   (::jdbc/update-count
    (jdbc/execute-one! db ["UPDATE sessions SET active = NOW() WHERE id = ?" sid]
                       db/opts-simple-map)))
  ([db sid t]
   (::jdbc/update-count
    (sql/update! db :sessions {:active (t/instant t)} {:id sid} db/opts-simple-map))))

;; Marking

(defn mkgood
  [smap]
  (-> (assoc smap :valid? true)
      (dissoc :expired? :hard-expired? :error)))

(defn mkbad
  "Marks session as invalid and renames :id key to :err/id."
  ([smap opts k v & pairs]
   (mkbad (apply assoc smap k v pairs) opts))
  ([smap opts]
   (let [cause         (get (get smap :error) :cause)
         expired?      (or (= :expired cause)
                           (and (= :bad-ip cause) (get opts :wrong-ip-expires)))
         hard-expired? (and expired? (hard-expired? smap opts))
         err-id        (if-some [sid (get smap :id)] sid (get smap :err/id))]
     (-> (update-in smap [:error :severity] (fnil identity :warn))
         (assoc :valid?        false
                :err/id        err-id
                :expired?      expired?
                :hard-expired? hard-expired?)
         (dissoc :id)))))

;; Configuration

(defn session-key
  "Returns a string of configured session ID field name by extracting it from `opts`
  which can be a map containing `:session-id-key`, a request map containing the given
  `result-key` associated with a map with `:session-id-key`, a request map containing
  the given `config-key` associated with a map with `:session-id-key` or a
  keyword (returned immediately). Optional `other` map can be provided which will be
  used as a second try when `opts` lookup will fail. The function returns
  \"session-id\" string when other methods fail."
  ([opts]
   (session-key opts :session :session/config))
  ([opts other]
   (session-key opts other :session :session/config))
  ([opts result-key config-key]
   (if (keyword? opts)
     opts
     (or (get opts :session-id-key)
         (get (get opts result-key) :session-id-key)
         (get (get opts config-key) :session-id-key)
         "session-id")))
  ([opts other result-key config-key]
   (if (keyword? opts)
     opts
     (or (get opts :session-id-key)
         (get (get opts result-key) :session-id-key)
         (get (get opts config-key) :session-id-key)
         (:session-if-key other)
         (get (result-key other)    :session-id-key)
         (get (config-key other)    :session-id-key)
         "session-id"))))

(def session-field session-key)

;; Session variables

(defn- prep-opts
  [opts-or-smap sid-or-opts]
  (let [secar? (map? sid-or-opts)
        opts   (if secar? sid-or-opts           opts-or-smap)
        sid    (if secar? (get opts-or-smap :id) sid-or-opts)
        smap   (if secar? opts-or-smap                   nil)]
    [smap opts sid]))

(defn- prep-names
  [coll]
  (when (coll? coll)
    (seq (if (map? coll) (keys coll) coll))))

(def ^{:private  true
       :arglists '([db session-id setting-id])}
  get-var-core
  "Gets a session variable and de-serializes it to a Clojure data structure."
  (db/make-setting-getter :session-variables :session-id))

(def ^{:private  true
       :arglists '([db session-id setting-id value]
                   [db session-id setting-id value & pairs])}
  put-var-core
  "Stores one or more session variables in a database. Max. object size is 32 KB."
  (db/make-setting-setter :session-variables :session-id))

(def ^{:private  true
       :arglists '([db session-id]
                   [db session-id setting-id]
                   [db session-id setting-id & setting-ids])}
  del-var-core
  "Deletes one or more session variables settings from a database."
  (db/make-setting-deleter :session-variables :session-id))

(defn get-var
  "Gets session variable and de-serializes it to a Clojure data structure."
  ([opts-or-smap sid-or-opts var-name]
   (let [[smap opts sid] (prep-opts opts-or-smap sid-or-opts)]
     (if (and smap (not (valid? smap)))
       (log/err "Cannot get session variable" var-name "because session is not valid")
       (get-var-core (get opts :db) sid var-name)))))

(defn get-variable-failed?
  [v]
  (= ::db/get-failed v))

(defn put-var!
  [opts-or-smap sid-or-opts var-name value & pairs]
  (let [[smap opts sid] (prep-opts opts-or-smap sid-or-opts)]
    (if-not sid
      (log/err "Cannot store session variable" var-name
               "because session ID is not valid")
      (if pairs
        (put-var-core (get opts :db) sid var-name value)
        (apply put-var-core (get opts :db) sid var-name value pairs)))))

(defn del-var!
  [opts-or-smap sid-or-opts var-name & names]
  (let [[smap opts sid] (prep-opts opts-or-smap sid-or-opts)]
    (if-not sid
      (log/err "Cannot delete session variable" var-name
               "because session ID is not valid")
      (if names
        (del-var-core (get opts :db) sid var-name)
        (apply del-var-core (get opts :db) sid var-name names)))))

(defn del-vars!
  [opts-or-smap sid-or-opts]
  (let [[smap opts sid] (prep-opts opts-or-smap sid-or-opts)]
    (if-not sid
      (log/err "Cannot delete session variables"
               "because session ID is not valid")
      (del-var-core (get opts :db) sid))))

(def ^:const mass-del-sql
  (str-spc "DELETE FROM session_variables"
           "WHERE EXISTS (SELECT 1 FROM sessions"
           "WHERE sessions.user_id = ?"
           " AND session_variables.session_id = sessions.id)"))

(defn del-user-vars!
  [opts-or-smap sid-or-opts]
  (let [[smap opts sid] (prep-opts opts-or-smap sid-or-opts)
        user-id         (get smap :user/id)
        user-email      (get smap :user/email)
        db              (get opts :db)]
    (cond
      (not sid)     (log/err "Cannot delete session variables"
                             (log/for-user user-id user-email)
                             (when sid (str "of" sid))
                             "because session ID is not valid")
      (not user-id) (log/err "Cannot delete session variables of" sid
                             "because user ID" user-id "is invalid"
                             (log/for-user nil user-email))
      (not db)      (log/err "Cannot delete session variables"
                             (log/for-user user-id user-email)
                             "because there is no database connection")
      :else         (jdbc/execute-one! db [mass-del-sql user-id]))))

;; Cache invalidation.

(defn invalidate-cache!
  "Invalidates cache."
  {:arglists '([req sid]
               [req smap]
               [req sid config-key]
               [req smap config-key]
               [opts sid ip-address]
               [opts db sid ip-address])}
  ([req sid-or-smap]
   (let [opts (get req :session/config)
         sid  (if (map? sid-or-smap) (get sid-or-smap :id) sid-or-smap)]
     (invalidate-cache! opts (get opts :db) sid (get req :remote-ip))))
  ([opts-or-req sid-or-smap ip-address-or-config-key]
   (if (keyword? ip-address-or-config-key)
     (invalidate-cache! opts-or-req (get opts-or-req :db) sid-or-smap ip-address-or-config-key)
     (let [opts (get opts-or-req ip-address-or-config-key)
           sid  (if (map? sid-or-smap) (get sid-or-smap :id) sid-or-smap)]
       (invalidate-cache! opts (get opts-or-req :db) sid (get opts-or-req :remote-ip)))))
  ([opts db sid ip-address]
   (when-some [invalidator (get opts :invalidator)]
     (invalidator opts db sid ip-address))))

;; Cache invalidation when time-sensitive value (last active time) exceeds TTL.

(defn refresh-times
  "If the time left before expiry is smaller than a cache TTL then the session map will
  be updated using database query."
  [db smap opts remote-ip]
  (or (when-some [cache-expires (get opts :cache-expires)]
        (when-some [last-active (get smap :active)]
          (let [inactive-for (t/between last-active (t/now))]
            (when (t/> inactive-for cache-expires)
              (let [sid (get smap :id)]
                (invalidate-cache! opts db sid remote-ip)
                (when-some [last-active (get-last-active db sid)]
                  (assoc smap :active last-active)))))))
      smap))

;; Session handling, creation and prolongation

(defn handler
  "Processes session information by taking configuration options, session ID string,
  remote IP. request map and configuration options. It tries to get session-id string
  from form parameters and if the string is valid it will obtain session from a
  database. The database connection object should be present in options under the :db
  key. If there is no session-id present in a request nil is returned."
  ([opts sid remote-ip]
   (handler opts (get opts :db) sid remote-ip))
  ([opts db sid remote-ip]
   (let [smap (sql/get-by-id db :sessions sid db/opts-slashed-map)
         smap (update smap :ip ip/to-address)
         smap (if (and (not (get smap :id)) (not (get smap :err/id))) (assoc smap :err/id sid) smap)
         smap (map/assoc-missing smap :session-id-key (or (get opts :session-id-key) "session-id"))
         stat (state smap opts remote-ip)]
     (if (get stat :cause)
       (mkbad smap opts :error stat)
       (mkgood smap)))))

(defn process
  "Takes a session processing handler, a request map and an optional session options or
  a config key and validates session against database or memoized session
  data. Returns a session map."
  {:arglists '([handler-fn req]
               [handler-fn req config-key]
               [handler-fn req opts])}
  ([handler-fn req]
   (process handler-fn req (get req :session/config)))
  ([handler-fn req opts-or-config-key]
   (let [opts (if (map? opts-or-config-key) opts-or-config-key (get req opts-or-config-key))
         db   (get opts :db)
         skey (or (get opts :session-id-key) "session-id")]
     (if-some [sid (some-str (get-in req [:form-params skey]))]
       (if-not (sid-valid? sid)
         (mkbad {:id sid} opts
                :session-id-key skey
                :error {:reason   "Malformed session-id parameter"
                        :cause    :malformed-session-id
                        :severity :info})
         (let [remote-ip (get req :remote-ip)
               smap      (handler-fn opts db sid remote-ip)]
           (if-not (valid? smap)
             smap
             (let [smap (refresh-times db smap opts remote-ip)]
               (if-not (valid? smap)
                 smap
                 (if (pos-int? (update-last-active db sid))
                   (mkgood smap)
                   (mkbad smap opts
                          :error {:severity :error
                                  :cause    :database-problem
                                  :reason   (some-str-spc "Problem updating session data"
                                                          (log/for-user
                                                           (:user/id    smap)
                                                           (:user/email smap)
                                                           (or (ip/plain-ip-str (ip/to-address (:ip smap)))
                                                               (:remote-ip/str req))))})))))))
       {:id nil :err/id nil :session-id-key skey}))))

(defn prolong
  "Re-validates session by updating its timestamp and re-running validation."
  [smap opts ip-address]
  (when-some [sid (or (get smap :err/id) (get smap :id))]
    (let [ip-address (ip/to-address ip-address)
          ipplain    (ip/plain-ip-str ip-address)
          new-time   (t/now)]
      (log/msg "Prolonging session" (log/for-user (get smap :user/id) (get smap :user/email) ipplain))
      (let [test-smap (assoc smap :id sid :active new-time)
            stat      (state test-smap opts ip-address)
            db        (get opts :db)]
        (invalidate-cache! opts db sid ip-address)
        (if (correct? (get stat :cause))
          (do (update-last-active db sid (t/instant new-time))
              (assoc (handler opts db sid ip-address) :prolonged? true))
          (do (log/wrn "Session re-validation error" (log/for-user (:user/id smap) (:user/email smap) ipplain))
              (mkbad smap opts :error stat)))))))

(defn create
  "Creates a session and puts it into a database. Returns the created session map."
  ([opts-or-db user-id user-email ip-address]
   (let [[db opts]  (if (db/data-source? opts-or-db)
                      [opts-or-db {:db opts-or-db}]
                      [(get opts-or-db :db) opts-or-db])
         user-id    (valuable user-id)
         user-email (some-str user-email)]
     (if-not (and db user-id user-email)
       (do (when-not db         (log/err "No database connection given when creating a session"))
           (when-not user-id    (log/err "No user ID given when creating a session"))
           (when-not user-email (log/err "No user e-mail given when creating a session"))
           nil)
       (let [t       (t/now)
             ip      (ip/to-address ip-address)
             ipplain (ip/plain-ip-str ip)
             sid     (gen-session-id user-id t (ip/to-str-v6 ip))
             sess    {:user/id    user-id
                      :user/email user-email
                      :id         sid
                      :ip         ip
                      :created    t
                      :active     t}
             stat    (state sess opts ip)]
         (log/msg "Opening session" (log/for-user user-id user-email ipplain))
         (if-not (correct? (get stat :cause))
           (do (log/err "Session incorrect after creation" (log/for-user user-id user-email ipplain))
               (mkbad sess opts :error stat))
           (let [sess-db (set/rename-keys sess {:user/id :user_id :user/email :user_email})
                 r       (db/replace! db :sessions sess-db db/opts-simple-map)
                 sess    (assoc sess :session-id-key (or (get opts :session-id-key) "session-id"))]
             (invalidate-cache! opts db (get sess :id) ip)
             (if (and r (pos-int? (::jdbc/update-count r)))
               (do (if (get opts :single-session)
                     (del-user-vars! sess opts)
                     (del-vars! sess opts))
                   (mkgood sess))
               (do (log/err "Problem saving session" (log/for-user user-id user-email ipplain))
                   (mkbad
                    sess
                    opts
                    :error  {:reason   (str "Session cannot be saved" (log/for-user user-id user-email ipplain))
                             :cause    :db-problem
                             :severity :error}))))))))))

;; Initialization

(defn- setup-invalidator
  [processor mem-processor]
  (if (or (not mem-processor)
          (= mem-processor processor))
    (constantly nil)
    (db/invalidator mem-processor)))

(defn- calc-cache-expires
  [config]
  (let [expires   (get config :expires)
        cache-ttl (get config :cache-ttl)]
    (assoc config :cache-expires
           (when (and expires cache-ttl)
             (if (t/> cache-ttl expires)
               (t/new-duration 1 :seconds)
               (t/- expires cache-ttl))))))

(defn wrap-session
  "Session maintaining middleware."
  [k config]
  (let [handler-sym (get config :handler)]
    (when-some [processor (var/deref-symbol handler-sym)]
      (let [dbname        (db/db-name (get config :db))
            config        (-> config
                              (dissoc :handler)
                              (update :db             db/ds)
                              (update :expires        time/parse-duration)
                              (update :hard-expires   time/parse-duration)
                              (update :cache-ttl      time/parse-duration)
                              (update :cache-size     safe-parse-long)
                              (update :session-key    #(or (some-keyword %) :session))
                              (update :config-key     #(or (some-keyword %) :session/config))
                              (update :session-id-key #(or (some-str %) "session-id"))
                              (calc-cache-expires))
            session-key   (get config :session-key :session)
            config-key    (get config :config-key  :session/config)
            mem-processor (db/memoizer processor config)
            invalidator   (setup-invalidator processor mem-processor)
            config        (assoc config :invalidator invalidator)]
        (log/msg "Installing session handler:" handler-sym)
        (log/msg "Using database" dbname "for storing sessions")
        {:name    (keyword k)
         :compile (fn [{:keys [no-session?]} opts]
                    (when (and (not no-session?) (get config :db))
                      (fn [h]
                        (fn [req]
                          (h
                           (assoc req
                                  session-key (delay (process mem-processor req config))
                                  config-key config))))))}))))

(system/add-init  ::session [k config] (wrap-session k config))
(system/add-halt! ::session [_ config] nil)
