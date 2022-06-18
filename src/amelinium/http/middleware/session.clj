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

(defn get-session-by-id
  "Standard session getter. Uses `db` to connect to a database and gets data identified
  by `sid` from a table `table`. Returns a map."
  [opts db table sid remote-ip]
  (sql/get-by-id db table sid db/opts-slashed-map))

(defn get-last-active
  [opts db table sid remote-ip]
  (first (jdbc/execute-one! db
                            [(str "SELECT active FROM " table " WHERE id = ?") sid]
                            db/opts-simple-vec)))

(defn update-last-active
  ([opts db table sid remote-ip]
   (::jdbc/update-count
    (sql/update! db table {:active (t/now)} {:id sid} db/opts-simple-map)))
  ([opts db table sid remote-ip t]
   (::jdbc/update-count
    (sql/update! db table {:active (t/instant t)} {:id sid} db/opts-simple-map))))

(defn set-session
  [opts db table smap]
  (let [sess-db (set/rename-keys smap {:user/id :user_id :user/email :user_email})]
    (::jdbc/update-count
     (db/replace! db table sess-db db/opts-simple-map))))

(defn delete-user-vars
  [opts db sessions-table variables-table user-id]
  (jdbc/execute-one! db [(str-spc "DELETE FROM" variables-table
                                  "WHERE EXISTS (SELECT 1 FROM" sessions-table
                                  (str "WHERE " sessions-table ".user_id = ?")
                                  (str "AND " variables-table ".session_id = " sessions-table ".id)"))
                         user-id]))

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
  which can be a map containing `:session-id-field`, a request map containing the given
  `result-key` associated with a map with `:session-id-field`, a request map containing
  the given `config-key` associated with a map with `:session-id-field` or a
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
     (or (get opts :session-id-field)
         (get (get opts result-key) :session-id-field)
         (get (get opts config-key) :session-id-field)
         "session-id")))
  ([opts other result-key config-key]
   (if (keyword? opts)
     opts
     (or (get opts :session-id-field)
         (get (get opts result-key) :session-id-field)
         (get (get opts config-key) :session-id-field)
         (:session-if-key other)
         (get (result-key other)    :session-id-field)
         (get (config-key other)    :session-id-field)
         "session-id"))))

(def session-field session-key)

(defn- config-options
  ([req opts-or-config-key]
   (if (map? opts-or-config-key)
     opts-or-config-key
     (get req (or opts-or-config-key :session/config)))))

;; Session variables

(defn- prep-opts
  [opts id-or-smap]
  (if (map? id-or-smap)
    [id-or-smap opts (get id-or-smap :id)]
    [nil opts id-or-smap]))

(defn- prep-names
  [coll]
  (when (coll? coll)
    (seq (if (map? coll) (keys coll) coll))))

(defn get-var
  "Gets session variable and de-serializes it to a Clojure data structure."
  {:arglists '([opts sid var-name]
               [opts smap var-name])}
  ([opts sid-or-smap var-name]
   (let [[smap opts sid] (prep-opts opts sid-or-smap)]
     (if (and smap (not (valid? smap)))
       (log/err "Cannot get session variable" var-name "because session is not valid")
       ((get opts :fn/var-get) sid var-name)))))

(defn get-variable-failed?
  "Returns `true` if the value `v` obtained from a session variable indicates that it
  actually could not be successfully fetched from a database."
  [v]
  (= ::db/get-failed v))

(defn put-var!
  "Puts a session variable `var-name` with a value `value` into a database. The session
  can be identified with a session ID (`sid`) or a session map (`smap`). Optional
  `pairs` of variable names and values can be given to perform a batch operation for
  multiple variables."
  {:arglists '([opts sid var-name value & pairs]
               [opts smap var-name value & pairs])}
  [opts sid-or-smap var-name value & pairs]
  (let [[smap opts sid] (prep-opts opts sid-or-smap)]
    (if-not sid
      (log/err "Cannot store session variable" var-name
               "because session ID is not valid")
      (if pairs
        ((get opts :fn/var-set) sid var-name value)
        (apply (get opts :fn/var-set) sid var-name value pairs)))))

(defn del-var!
  "Deletes from a session variable `var-name` assigned to a session of the given
  ID (`sid`) or a session map (`smap`). Optional variable `names` can be given to
  perform a batch operation for multiple variables."
  {:arglists '([opts sid var-name & names]
               [opts smap var-name & names])}
  [opts sid-or-smap var-name & names]
  (let [[smap opts sid] (prep-opts opts sid-or-smap)]
    (if-not sid
      (log/err "Cannot delete session variable" var-name
               "because session ID is not valid")
      (if names
        ((get opts :fn/var-del) sid var-name)
        (apply (get opts :fn/var-del) sid var-name names)))))

(defn del-vars!
  "Deletes all session variables which belong to a session of the given ID (`sid`) or a
  session map (`smap`)."
  {:arglists '([opts sid]
               [opts smap])}
  [opts sid-or-smap]
  (let [[smap opts sid] (prep-opts opts sid-or-smap)]
    (if-not sid
      (log/err "Cannot delete session variables"
               "because session ID is not valid")
      ((get opts :fn/var-del) sid))))

(defn del-user-vars!
  "Deletes all session variables which belong to a user. The user can be specified as
  `user-id`, `sid` (indirectly) or `smap` (indirectly)."
  {:arglists '([opts user-id]
               [opts smap])}
  [opts smap-or-user-id]
  (let [[smap opts user-id] (prep-opts opts smap-or-user-id)
        del-fn              (get opts :fn/vars-del-user)]
    (if user-id
      (del-fn user-id)
      (let [user-id    (get smap :user/id)
            user-email (get smap :user/email)]
        (if-not user-id
          (log/err "Cannot delete session variables"
                   "because user ID" user-id "is invalid"
                   (log/for-user nil user-email))
          (del-fn user-id))))))

;; Cache invalidation.

(defn invalidate-cache!
  "Invalidates cache."
  {:arglists '([req]
               [req opts]
               [req config-key]
               [opts sid ip-address]
               [invalidator-fn sid ip-address])}
  ([req]
   (invalidate-cache! req :session/config))
  ([req opts-or-config-key]
   (let [opts (config-options req opts-or-config-key)]
     (when-some [invalidator (get opts :fn/invalidator)]
       (invalidator (get (get req (or (get opts :session-key) :session)) :id)
                    (get req :remote-ip)))))
  ([opts-or-fn sid ip-address]
   (when-some [invalidator (if (map? opts-or-fn) (get opts-or-fn :fn/invalidator) opts-or-fn)]
     (invalidator sid ip-address))))

;; Cache invalidation when time-sensitive value (last active time) exceeds TTL.

(defn refresh-times
  "If the time left before expiry is smaller than a cache TTL then the session map will
  be updated using database query."
  ([req]
   (refresh-times req :session/config))
  ([req opts-or-config-key]
   (let [opts (config-options req opts-or-config-key)]
     ((get opts :fn/refresh) (get req (or (get opts :session-key) :session)) (get req :remote-ip))))
  ([req opts-or-config-key smap remote-ip]
   (let [opts (config-options req opts-or-config-key)]
     ((get opts :fn/refresh) smap remote-ip)))
  ([opts last-active-fn invalidator-fn cache-expires smap remote-ip]
   (or (when cache-expires
         (when-some [last-active (get smap :active)]
           (let [inactive-for (t/between last-active (t/now))]
             (when (t/> inactive-for cache-expires)
               (let [sid (get smap :id)]
                 (invalidator-fn sid remote-ip)
                 (when-some [last-active (last-active-fn sid remote-ip)]
                   (assoc smap :active last-active)))))))
       smap)))

;; Session handling, creation and prolongation

(defn handler
  "Processes session information by taking configuration options, session ID string,
  remote IP, request map and configuration options. It tries to get session ID string
  from form parameters of the request map and if the string is valid obtains session
  from a database using `getter-fn` (passing configuration options, database
  connection, table, session ID and remote IP to the call). The database connection
  object should be present in options under the `:db` key or given as an argument. If
  there is no session ID present in a request (under the `\"session-id\"` form param
  or under a key passed as `session-id-field` argument), `nil` is returned."
  ([req]
   (handler req :session/config))
  ([req opts-or-config-key]
   (let [opts (config-options opts-or-config-key)]
     (handler opts
              (some-str (get (get req :form-params) (or (some-str (get opts :session-id-field)) "session-id")))
              (get req :remote-ip))))
  ([req opts-or-config-key sid remote-ip]
   (let [opts (config-options req opts-or-config-key)]
     (handler opts sid remote-ip)))
  ([opts sid remote-ip]
   (handler opts
            (get opts :fn/getter)
            (get opts :session-id-field)
            sid remote-ip))
  ([opts getter-fn session-id-field sid remote-ip]
   (let [smap (getter-fn sid remote-ip)
         smap (update smap :ip ip/to-address)
         smap (if (and (not (get smap :id)) (not (get smap :err/id))) (assoc smap :err/id sid) smap)
         smap (map/assoc-missing smap :session-id-field session-id-field)
         stat (state smap opts remote-ip)]
     (if (get stat :cause)
       (mkbad smap opts :error stat)
       (mkgood smap)))))

(defn process
  "Takes a session handler, last active time getter, last active time updater, a
  request map and an optional session options or a config key and validates session
  against database or memoized session data. Returns a session map."
  {:arglists '([req]
               [req config]
               [req config-key]
               [handler-fn refresh-fn update-active-fn invalidator-fn req]
               [handler-fn refresh-fn update-active-fn invalidator-fn req config-key]
               [handler-fn refresh-fn update-active-fn invalidator-fn req opts]
               [handler-fn refresh-fn update-active-fn invalidator-fn req opts session-id-field]
               [handler-fn refresh-fn update-active-fn invalidator-fn req config-key session-id-field])}
  ([req]
   (process req :session/config))
  ([req opts-or-config-key]
   (let [opts (config-options req opts-or-config-key)]
     (process (get opts :fn/handler)
              (get opts :fn/refresh)
              (get opts :fn/update-active)
              req
              opts
              (get opts :session-id-field) "session-id")))
  ([handler-fn refresh-fn update-active-fn req]
   (process handler-fn refresh-fn update-active-fn req (get req :session/config)))
  ([handler-fn refresh-fn update-active-fn req opts-or-config-key]
   (process handler-fn refresh-fn update-active-fn req opts-or-config-key nil))
  ([handler-fn refresh-fn update-active-fn req opts-or-config-key session-id-field]
   (let [opts             (config-options req opts-or-config-key)
         session-id-field (or (some-str (or session-id-field (get opts :session-id-field))) "session-id")]
     (if-some [sid (some-str (get (get req :form-params) session-id-field))]
       (if-not (sid-valid? sid)
         (mkbad {:id sid} opts
                :session-id-field session-id-field
                :error {:reason   "Malformed session-id parameter"
                        :cause    :malformed-session-id
                        :severity :info})
         (let [remote-ip (get req :remote-ip)
               smap      (handler-fn sid remote-ip)]
           (if-not (valid? smap)
             smap
             (let [smap (refresh-fn smap remote-ip)]
               (if-not (valid? smap)
                 smap
                 (if (pos-int? (update-active-fn sid remote-ip))
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
       {:id nil :err/id nil :session-id-field session-id-field}))))

(defn prolong
  "Re-validates session by updating its timestamp and re-running validation."
  {:arglists '([req]
               [req config]
               [req config-key]
               [req config smap ip-address]
               [req config-key smap ip-address]
               [opts handler-fn update-active-fn invalidator-fn smap ip-address])}
  ([req]
   (prolong req :session/config))
  ([req opts-or-config-key]
   (let [opts (config-options req opts-or-config-key)]
     ((get opts :fn/prolong) (get req (or (get opts :session-key) :session)) (get req :ip-address))))
  ([opts smap ip-address]
   ((get opts :fn/prolong) smap ip-address))
  ([req opts-or-config-key smap ip-address]
   (let [opts (config-options req opts-or-config-key)]
     ((get opts :fn/prolong) smap ip-address)))
  ([opts handler-fn update-active-fn invalidator-fn smap ip-address]
   (when-some [sid (or (get smap :err/id) (get smap :id))]
     (let [ip-address (ip/to-address ip-address)
           ipplain    (ip/plain-ip-str ip-address)
           new-time   (t/now)]
       (log/msg "Prolonging session" (log/for-user (get smap :user/id) (get smap :user/email) ipplain))
       (let [test-smap (assoc smap :id sid :active new-time)
             stat      (state test-smap opts ip-address)]
         (invalidator-fn sid ip-address)
         (if (correct? (get stat :cause))
           (do (update-active-fn sid ip-address (t/instant new-time))
               (assoc (handler-fn sid ip-address) :prolonged? true))
           (do (log/wrn "Session re-validation error" (log/for-user (:user/id smap) (:user/email smap) ipplain))
               (mkbad smap opts :error stat))))))))

(defn create
  "Creates a session and puts it into a database. Returns the created session map."
  {:arglists '([req user-id user-email ip-address]
               [opts user-id user-email ip-address]
               [req opts-or-config-key user-id user-email ip-address]
               [opts setter-fn invalidator-fn user-id user-email ip-address])}
  ([opts-or-req user-id user-email ip-address]
   (if-some [create-fn (get opts-or-req :fn/create)]
     (create-fn user-id user-email ip-address)
     (create opts-or-req :session/config user-id user-email ip-address)))
  ([req opts-or-config-key user-id user-email ip-address]
   (let [opts (config-options req opts-or-config-key)]
     ((get opts :fn/create) user-id user-email ip-address)))
  ([opts setter-fn invalidator-fn var-del-fn var-del-user-fn single-session?
    user-id user-email ip-address]
   (let [user-id    (valuable user-id)
         user-email (some-str user-email)]
     (if-not (and user-id user-email)
       (do (when-not user-id    (log/err "No user ID given when creating a session"))
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
           (let [updated-count (setter-fn sess)
                 sess          (assoc sess :session-id-field (or (some-str (get opts :session-id-field)) "session-id"))]
             (invalidator-fn sid ip)
             (if (pos-int? updated-count)
               (do (if single-session?
                     (var-del-user-fn user-id)
                     (var-del-fn sid))
                   (mkgood sess))
               (do (log/err "Problem saving session" (log/for-user user-id user-email ipplain))
                   (mkbad sess opts
                          :error  {:reason   (str "Session cannot be saved"
                                                  (log/for-user user-id user-email ipplain))
                                   :cause    :db-problem
                                   :severity :error}))))))))))

;; Initialization

(defn- setup-invalidator
  [pre-handler mem-handler]
  (if (or (not mem-handler)
          (= mem-handler pre-handler))
    (constantly nil)
    (db/invalidator mem-handler)))

(defn- calc-cache-expires
  [config]
  (let [expires   (get config :expires)
        cache-ttl (get config :cache-ttl)]
    (assoc config :cache-expires
           (when (and expires cache-ttl)
             (if (t/> cache-ttl expires)
               (t/new-duration 1 :seconds)
               (t/- expires cache-ttl))))))

(defn- setup-fn
  [config k default]
  (or (var/deref (get config k)) default))

(defn wrap-session
  "Session maintaining middleware."
  [k config]
  (let [dbname             (db/db-name (get config :db))
        config             (-> config
                               (update :db               db/ds)
                               (update :table/sessions   #(or (to-snake-simple-str %) "sessions"))
                               (update :table/variables  #(or (to-snake-simple-str %) "session_variables"))
                               (update :expires          time/parse-duration)
                               (update :hard-expires     time/parse-duration)
                               (update :cache-ttl        time/parse-duration)
                               (update :cache-size       safe-parse-long)
                               (update :session-key      #(or (some-keyword %) :session))
                               (update :config-key       #(or (some-keyword %) :session/config))
                               (update :session-id-field #(or (some-str %) "session-id"))
                               (update :single-session?  boolean)
                               (calc-cache-expires))
        db                 (get config :db)
        session-key        (get config :session-key)
        config-key         (get config :config-key)
        sessions-table     (get config :table/sessions)
        variables-table    (get config :table/variables)
        session-id-field   (get config :session-id-field)
        cache-expires      (get config :expires)
        single-session?    (get config :single-session?)
        getter-fn          (setup-fn config :fn/getter get-session-by-id)
        getter-fn-w        #(getter-fn config db sessions-table %1 %2)
        config             (assoc config :fn/getter getter-fn-w)
        pre-handler        #(handler config getter-fn-w session-id-field %1 %2)
        mem-handler        (db/memoizer pre-handler config)
        invalidator-fn     (setup-invalidator pre-handler mem-handler)
        config             (assoc config :fn/invalidator invalidator-fn :fn/handler mem-handler)
        last-active-fn     (setup-fn config :fn/last-active get-last-active)
        update-active-fn   (setup-fn config :fn/update-active update-last-active)
        last-active-fn-w   #(last-active-fn config db sessions-table %1 %2)
        config             (assoc config :fn/last-active last-active-fn-w)
        update-active-fn-w (fn
                             ([sid remote-ip]
                              (update-active-fn config db sessions-table sid remote-ip))
                             ([sid remote-ip t]
                              (update-active-fn config db sessions-table sid remote-ip t)))
        config             (assoc config :fn/update-active update-active-fn-w)
        refresh-fn         #(refresh-times config last-active-fn-w invalidator-fn cache-expires %1 %2)
        config             (assoc config :fn/refresh refresh-fn)
        setter-fn          (setup-fn config :fn/setter set-session)
        setter-fn-w        #(setter-fn config db sessions-table %)
        config             (assoc config :fn/setter setter-fn-w)
        prolong-fn         #(prolong config mem-handler update-active-fn-w invalidator-fn %1 %2)
        config             (assoc config :fn/prolong prolong-fn)
        var-get-core-fn    (db/make-setting-getter  variables-table :session-id)
        var-set-core-fn    (db/make-setting-setter  variables-table :session-id)
        var-del-core-fn    (db/make-setting-deleter variables-table :session-id)
        var-del-user-fn    (setup-fn config :fn/vars-del-user delete-user-vars)
        var-get-fn         #(var-get-core-fn db %1 %2)
        var-set-fn         (fn
                             ([session-id setting-id value]
                              (var-set-core-fn db session-id setting-id value))
                             ([session-id setting-id value & pairs]
                              (apply var-set-core-fn db session-id setting-id value pairs)))
        var-del-fn         (fn
                             ([session-id]
                              (var-del-core-fn db session-id))
                             ([session-id setting-id]
                              (var-del-core-fn db session-id setting-id))
                             ([session-id setting-id & setting-ids]
                              (apply var-del-core-fn db session-id setting-id setting-ids)))
        var-del-user-fn-w  #(var-del-user-fn config db sessions-table variables-table %)
        config             (assoc config
                                  :fn/var-get var-get-fn
                                  :fn/var-set var-set-fn
                                  :fn/var-del var-del-fn
                                  :fn/vars-del-user var-del-user-fn-w)
        create-fn          #(create config
                                    setter-fn-w invalidator-fn var-del-fn var-del-user-fn-w
                                    single-session? %1 %2 %3)
        config             (assoc config :fn/create create-fn)]
    (log/msg "Installing session handler:" k)
    (when dbname (log/msg "Using database" dbname "for storing sessions"))
    {:name    (keyword k)
     :compile (fn [{:keys [no-session?]} opts]
                (when (and (not no-session?) db)
                  (fn [h]
                    (fn [req]
                      (h
                       (assoc req
                              session-key (delay (process mem-handler
                                                          refresh-fn
                                                          update-active-fn-w
                                                          req config session-id-field))
                              config-key config))))))}))

(system/add-init  ::default [k config] (wrap-session k config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
