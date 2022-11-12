(ns

    ^{:doc    "amelinium service, session middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.session

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                  :as        set]
            [clojure.string               :as        str]
            [crypto.equality              :as     crypto]
            [tick.core                    :as          t]
            [buddy.core.hash              :as       hash]
            [buddy.core.codecs            :as     codecs]
            [next.jdbc.sql                :as        sql]
            [next.jdbc                    :as       jdbc]
            [amelinium.db                 :as         db]
            [amelinium.logging            :as        log]
            [amelinium.system             :as     system]
            [amelinium.auth.algo.scrypt   :as     scrypt]
            [io.randomseed.utils          :refer    :all]
            [io.randomseed.utils.time     :as       time]
            [io.randomseed.utils.var      :as        var]
            [io.randomseed.utils.map      :as        map]
            [io.randomseed.utils.ip       :as         ip]
            [io.randomseed.utils.db.types :as      types])

  (:import [java.time Instant]
           [inet.ipaddr IPAddress]))

(def ^:const sid-match (re-pattern "|^[a-f0-9]{30,128}(-[a-f0-9]{30,128})?$"))

(def one-second (t/new-duration 1 :seconds))

(defrecord Session [^String    id
                    ^String    err-id
                    ^String    db-id
                    ^String    db-token
                    ^Long      user-id
                    ^String    user-email
                    ^Instant   created
                    ^Instant   active
                    ^IPAddress ip
                    ^Boolean   valid?
                    ^Boolean   expired?
                    ^Boolean   hard-expired?
                    ^Boolean   secure?
                    ^Boolean   security-passed?
                    ^String    session-key
                    ^String    id-field
                    ^clojure.lang.IPersistentMap error
                    ^clojure.lang.IPersistentMap config])

(defn session?
  ^Boolean [v]
  (instance? Session v))

(defprotocol Sessionable
  "This protocol is used to access session data."

  (^{:tag Session}
   -session
   [src] [src session-key]
   "Returns a session record of type `Session` on a basis of configuration source
  provided and an optional `session-key` if session must be looked in an associative
  structure (defaults to `:session`).")

  (^{:tag Boolean}
   -present?
   [src] [src session-key]
   "Returns `true` is `src` contains a session or is a session. Optional `session-key`
  can be given to express a key in associative structure (defaults to `:session`.")

  (-inject
    [dst smap] [dst smap session-key]
    "Returns an object updated with session record of type `Session` under an optional
  `session-key` if session is to be put into an associative structure (defaults to
  `:session`)."))

(extend-protocol Sessionable

  Session

  (-session  (^Session [src] src)  (^Session [src _] src))
  (-present? (^Boolean [src] true) (^Boolean [src _] true))
  (-inject   ([dst smap] smap)     ([dst smap _] smap))

  clojure.lang.IPersistentMap

  (-session
    (^Session [req]             (if-some [s (get req :session)] s))
    (^Session [req session-key] (if-some [s (get req (or session-key :session))] s)))

  (-present?
    (^Boolean [req]             (contains? req :session))
    (^Boolean [req session-key] (contains? req (or session-key :session))))

  (-inject
    (^Session [dst smap]
     (if-some [^Session smap (-session smap)]
       (map/qassoc dst (or (get (.config ^Session smap) :session-key) :session) smap)
       dst))
    (^Session [dst smap session-key]
     (if-some [^Session smap (-session smap)]
       (map/qassoc dst (or session-key (get (.config ^Session smap) :session-key) :session) smap)
       dst)))

  clojure.lang.Associative

  (-session
    (^Session [req]             (if-some [s (get req :session)] s))
    (^Session [req session-key] (if-some [s (get req (or session-key :session))] s)))

  (-present?
    (^Boolean [req]             (contains? req :session))
    (^Boolean [req session-key] (contains? req (or session-key :session))))

  (-inject
    (^Session [dst smap]
     (if-some [^Session smap (-session smap)]
       (map/qassoc dst (or (get (.config ^Session smap) :session-key) :session) smap)
       dst))
    (^Session [dst smap session-key]
     (if-some [^Session smap (-session smap)]
       (map/qassoc dst (or session-key (get (.config ^Session smap) :session-key) :session) smap)
       dst)))

  nil

  (-session
    ([src] nil)
    ([src session-key] nil))

  (-present?
    ([src] false)
    ([src session-key] false))

  (-inject
    ([src smap] nil)
    ([src smap session-key] nil)))

(defn of
  (^Session [src] (-session src))
  (^Session [src session-key] (-session src session-key)))

(defn present?
  (^Boolean [src] (-present? src))
  (^Boolean [src session-key] (-present? src session-key)))

(defn inject
  ([dst smap] (-inject dst smap))
  ([dst smap session-key] (-inject dst smap session-key)))

(defn config
  ([src] (if-some [^Session s (-session src)] (.config ^Session s)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.config ^Session s))))

(defn id
  ([src] (if-some [^Session s (-session src)] (.id ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.id ^Session src))))

(defn err-id
  ([src] (if-some [^Session s (-session src)] (.err-id ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.err-id ^Session src))))

(defn db-token
  ([src] (if-some [^Session s (-session src)] (.db-token ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.db-token ^Session src))))

(defn user-id
  ([src] (if-some [^Session s (-session src)] (.user-id ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.user-id ^Session src))))

(defn user-email
  ([src] (if-some [^Session s (-session src)] (.user-email ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.user-email ^Session src))))

(defn created
  ([src] (if-some [^Session s (-session src)] (.created ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.created ^Session src))))

(defn active
  ([src] (if-some [^Session s (-session src)] (.active ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.active ^Session src))))

(defn ip
  ([src] (if-some [^Session s (-session src)] (.ip ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.ip ^Session src))))

(defn session-key
  ([src] (if-some [^Session s (-session src)] (.session-key ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.session-key ^Session src))))

(defn id-field
  ([src] (if-some [^Session s (-session src)] (.id-field ^Session src)))
  ([src session-key] (if-some [^Session s (-session src session-key)] (.id-field ^Session src))))

;; Secure sessions

(def ^:const scrypt-options
  {:cpu-cost 512
   :mem-cost 2
   :parallel 1})

(def ^:const token-splitter (re-pattern "-"))
(def ^:const salt-splitter  (re-pattern "\\$"))

(defn- bytes->b64u
  [v]
  (try (if v (codecs/bytes->str (codecs/bytes->b64u v)))
       (catch Throwable _
         nil)))

(defn- b64u->bytes
  [v]
  (try (if v (codecs/b64u->bytes (codecs/str->bytes v)))
       (catch Throwable _
         nil)))

(defn encrypt
  [plain-token]
  (if plain-token
    (if-some [enc (scrypt/encrypt plain-token scrypt-options)]
      (str (bytes->b64u (bytes (get enc :salt))) "$"
           (bytes->b64u (bytes (get enc :password)))))))

(defn check-encrypted
  ([plain-token encrypted-token-b64-str]
   (if (and plain-token encrypted-token-b64-str)
     (if-some [salt-pass (str/split encrypted-token-b64-str salt-splitter 2)]
       (crypto/eq? (b64u->bytes (nth salt-pass 1 nil))
                   (get (scrypt/encrypt plain-token
                                        (b64u->bytes (nth salt-pass 0 nil))
                                        scrypt-options) :password))))))

(defn split-secure-sid
  [session-id]
  (str/split session-id token-splitter 2))

(defn db-id
  ([src]
   (if-some [^Session s (-session src)]
     (.db-id ^Session src)))
  ([src session-key]
   (if-some [^Session s (-session src session-key)]
     (.db-id ^Session src))))

(defn db-sid
  [smap-or-sid]
  (if (session? smap-or-sid)
    (or (.db-id ^Session smap-or-sid)
        (db-sid (.id ^Session smap-or-sid)))
    (nth (split-secure-sid smap-or-sid) 0 nil)))

(defn db-sid-smap
  [^Session smap]
  (or (.db-id ^Session smap)
      (nth (split-secure-sid (.id ^Session smap) 0 nil))))

(defn db-sid-str
  [sid]
  (nth (split-secure-sid sid) 0 nil))

;; SID generation

(defn gen-session-id
  [^Session smap ^Boolean secured? & args]
  (let [rnd (str (apply str args) (time/timestamp) (gen-digits 8))
        sid (-> rnd hash/md5 codecs/bytes->hex)]
    (if secured?
      (let [pass (-> (gen-digits 10) hash/md5 codecs/bytes->hex)
            stok (encrypt pass)
            ssid (str sid "-" pass)]
        (if (not-empty stok)
          (map/qassoc smap :id ssid :db-id sid :db-token stok :secure? true :security-passed? true)
          (map/qassoc smap :id  sid :db-id sid :secure? false)))
      (map/qassoc smap :id sid :db-id sid :secure? false))))

;; Session validation

(defn secure?
  "Checks if a session is secure. If `:secured?` option is not enabled in configuration,
  it always returns `true`. If `:secure?` flag is set to a truthy value, it returns
  it."
  (^Boolean [src]
   (if-some [^Session s (-session src)]
     (or (not (get (.config ^Session s) :secured?))
         (.secure? ^Session s))
     false))
  (^Boolean [src session-key]
   (if-some [^Session s (-session src session-key)]
     (or (not (get (.config ^Session s) :secured?))
         (.secure? ^Session s))
     false)))

(defn insecure?
  "Checks if session is not secure where it should be. If `:secured?` option is not
  enabled in configuration, it always returns `false`. If `:secure?` flag is set to a
  falsy value, it returns `false`."
  (^Boolean [src]
   (if-some [^Session s (-session src)]
     (and (not (.secure? ^Session s))
          (get (.config ^Session s) :secured?))
     true))
  (^Boolean [src session-key]
   (if-some [^Session s (-session src session-key)]
     (and (not (.secure? ^Session s))
          (get (.config ^Session s) :secured?))
     true)))

(defn security-passed?
  "Checks if the additional security token was validated correctly unless the session
  is not secured (in such case returns `true`)."
  (^Boolean [src]
   (if-some [^Session s (-session src)]
     (or (not (.secure? ^Session s))
         (.security-passed? ^Session s))
     false))
  (^Boolean [src session-key]
   (if-some [^Session s (-session src session-key)]
     (or (not (.secure? ^Session s))
         (.security-passed? ^Session s))
     false)))

(defn security-failed?
  "Checks if the additional security token was validated incorrectly unless the session
  is not secured (in such case it returns `false`)."
  (^Boolean [src]
   (if-some [^Session s (-session src)]
     (and (.secure? ^Session s)
          (not (.security-passed? ^Session s)))
     true))
  (^Boolean [src session-key]
   (if-some [^Session s (-session src session-key)]
     (and  (.secure? ^Session s)
           (not (.security-passed? ^Session s)))
     true)))

(defn ip-state
  ([src session-key user-id user-email remote-ip]
   (ip-state (-session src session-key) user-id user-email remote-ip))
  ([src user-id user-email remote-ip]
   (if-some [^Session smap (-session src)]
     (if-some [session-ip (or (.ip ^Session smap) (get smap :ip-address))]
       (if-some [remote-ip (ip/to-address remote-ip)]
         (if-not (or (= (ip/to-v6 remote-ip) (ip/to-v6 session-ip))
                     (= (ip/to-v4 remote-ip) (ip/to-v4 session-ip)))
           {:cause    :session/bad-ip
            :reason   (str-spc "Session IP address" (str "(" (ip/plain-ip-str session-ip) ")")
                               "is different than the remote IP address"
                               (str "(" (ip/plain-ip-str remote-ip) ")")
                               (log/for-user user-id user-email))
            :severity :warn})
         (if-some [str-addr (ip/to-str remote-ip)]
           (if-not (or (= str-addr (ip/to-str session-ip))
                       (= str-addr (ip/to-str (ip/to-v4 session-ip)))
                       (= str-addr (ip/to-str (ip/to-v6 session-ip))))
             {:cause    :session/bad-ip
              :reason   (str-spc "Session IP string" (str "(" (ip/to-str remote-ip) ")")
                                 "is different than the remote IP string"
                                 (str "(" str-addr ")")
                                 (log/for-user user-id user-email))
              :severity :warn})))))))

(defn same-ip?
  (^Boolean [state-result]
   (nil? state-result))
  (^Boolean [src user-id user-email remote-ip]
   (nil? (ip-state src user-id user-email remote-ip)))
  (^Boolean [src session-key user-id user-email remote-ip]
   (nil? (ip-state src session-key user-id user-email remote-ip))))

(defn time-exceeded?
  (^Boolean [dur max-dur]
   (t/> dur max-dur))
  (^Boolean [t-start t-stop max-dur]
   (t/> (t/between t-start t-stop) max-dur)))

(defn calc-expired?
  (^Boolean [src session-key]
   (calc-expired? (-session src session-key)))
  (^Boolean [src]
   (if-some [^Session smap (-session src)]
     (if-some [exp (get (.config ^Session smap) :expires)]
       (and (pos-int? (time/seconds exp))
            (time-exceeded? (.active ^Session smap) (t/now) exp))))))

(defn calc-hard-expired?
  (^Boolean [src session-key]
   (calc-hard-expired? (-session src session-key)))
  (^Boolean [src]
   (if-some [^Session smap (-session src)]
     (if-some [hexp (get (.config ^Session smap) :hard-expires)]
       (and (pos-int? (time/seconds hexp))
            (time-exceeded? (.active ^Session smap) (t/now) hexp))))))

(defn calc-soft-expired?
  (^Boolean [src session-key]
   (calc-soft-expired? (-session src session-key)))
  (^Boolean [src]
   (if-some [^Session smap (-session src)]
     (and (calc-expired? ^Session smap)
          (not (calc-hard-expired? ^Session smap))))))

(defn expired?
  ([src]
   (if-some [^Session s (-session src)]
     (.expired? ^Session s)
     false))
  ([src session-key]
   (if-some [^Session s (-session src session-key)]
     (.expired? ^Session s)
     false)))

(defn hard-expired?
  ([src]
   (if-some [^Session s (-session src)]
     (.hard-expired? ^Session s)
     false))
  ([src session-key]
   (if-some [^Session s (-session src session-key)]
     (.hard-expired? ^Session s)
     false)))

(defn soft-expired?
  ([src]
   (if-some [^Session s (-session src)]
     (and (.expired? ^Session s)
          (not (.hard-expired? ^Session s)))
     false))
  ([src session-key]
   (if-some [^Session s (-session src session-key)]
     (and (.expired? ^Session s)
          (not (.hard-expired? ^Session s)))
     false)))

(defn sid-valid?
  ^Boolean [sid]
  (boolean
   (and sid (string? sid)
        (<= 30 (count sid) 256)
        (re-matches sid-match sid))))

(defn created-valid?
  (^Boolean [src] (t/instant? (created src)))
  (^Boolean [src session-key] (t/instant? (created src session-key))))

(defn active-valid?
  (^Boolean [src] (t/instant? (active src)))
  (^Boolean [src session-key] (t/instant? (active src session-key))))

(defn state
  "Returns session state. If there is anything wrong, returns an error
  string. Otherwise it returns `nil`. Unknown session detection is performed by
  checking if a value associated with the `:id` key is `nil` and a value associated
  with the `:err-id` key is not `nil`. First argument `smap` must be a session object
  of type `Session`."
  [smap ip-address]
  (if-not (session? smap)
    {:cause    :session/missing
     :reason   (str-spc "No session map:" smap)
     :severity :info}
    (let [^Session smap smap
          sid           (.id         ^Session smap)
          esid          (.err-id     ^Session smap)
          user-id       (.user-id    ^Session smap)
          user-email    (.user-email ^Session smap)
          any-sid       (or sid esid)
          user-ident    (or user-id user-email)
          user-id       (valuable user-id)
          user-email    (some-str user-email)
          for-user      (delay (log/for-user user-id user-email
                                             (ip/plain-ip-str ip-address)))]
      (cond
        (not any-sid)               {:cause    :session/no-id
                                     :reason   (some-str-spc "No session ID" @for-user)
                                     :severity :info}
        (not sid)                   {:cause    :session/unknown-id
                                     :reason   (some-str-spc "Unknown session ID" esid @for-user)
                                     :severity :info}
        (not (sid-valid? any-sid))  {:cause    :session/malformed-session-id
                                     :reason   (str "Malformed session ID " @for-user)
                                     :severity :info}
        (not user-ident)            {:cause    :session/unknown-id
                                     :reason   (some-str-spc "Unknown session ID" sid @for-user)
                                     :severity :info}
        (not user-id)               {:cause    :session/malformed-user-id
                                     :reason   (str "User ID not found or malformed " @for-user)
                                     :severity :info}
        (not user-email)            {:cause    :session/malformed-user-email
                                     :reason   (str "User e-mail not found or malformed " @for-user)
                                     :severity :info}
        (not (created-valid? smap)) {:cause    :session/bad-creation-time
                                     :reason   (str "No creation time " @for-user)
                                     :severity :warn}
        (not (active-valid? smap))  {:cause    :session/bad-last-active-time
                                     :reason   (str "No last active time " @for-user)
                                     :severity :warn}
        (calc-expired? smap)        {:cause    :session/expired
                                     :reason   (str "Session expired " @for-user)
                                     :severity :info}
        (insecure? smap)            {:cause    :session/insecure
                                     :reason   (str "Session not secured with encrypted token " @for-user)
                                     :severity :warn}
        (security-failed? smap)     {:cause    :session/bad-security-token
                                     :reason   (str "Bad session security token " @for-user)
                                     :severity :warn}
        :ip-address-check           (ip-state smap user-id user-email ip-address )))))

(defn correct?
  "Returns `true` if a session exists and its state is correct. Never throws an
  exception."
  (^Boolean [state-result]
   (nil? state-result))
  (^Boolean [src ip-address]
   (nil? (state (try (-session src) (catch Throwable _ nil)) ip-address)))
  (^Boolean [src session-key ip-address]
   (nil? (state (try (-session src session-key) (catch Throwable _ nil)) ip-address))))

(defn valid?
  "Returns `true` if a session is marked as valid."
  (^Boolean [src]
   (if-some [^Session s (-session src)]
     (.valid? ^Session s)
     false))
  (^Boolean [src session-key]
   (if-some [^Session s (-session src session-key)]
     (.valid? ^Session s)
     false)))

(defn error?
  ([src]
   (if-some [^Session s (-session src)]
     (some? (.error ^Session s))))
  ([src session-key]
   (if-some [^Session s (-session src session-key)]
     (some? (.error ^Session s)))))

(defn error
  ([src]
   (if-some [^Session s (-session src)]
     (.error ^Session s)))
  ([src session-key]
   (if-some [^Session s (-session src session-key)]
     (.error ^Session s))))

(defn allow-expired
  "Temporarily marks expired session as valid."
  ([src]
   (if-some [^Session smap (-session src)]
     (if (and (.expired?      ^Session smap)
              (not   (.valid? ^Session smap))
              (nil?  (.id     ^Session smap))
              (some? (.err-id ^Session smap )))
       (map/qassoc smap :valid? true :id (.err-id ^Session smap))
       smap)))
  ([src session-key]
   (if-some [^Session smap (-session src session-key)]
     (if (and (.expired?      ^Session smap)
              (not   (.valid? ^Session smap))
              (nil?  (.id     ^Session smap))
              (some? (.err-id ^Session smap )))
       (map/qassoc smap :valid? true :id (.err-id ^Session smap))
       smap))))

(defn allow-soft-expired
  "Temporarily mark soft-expired session as valid."
  ([src]
   (if-some [^Session smap (-session src session-key)]
     (if (.hard-expired? ^Session smap) smap (allow-expired smap))))
  ([src session-key]
   (if-some [^Session smap (-session src session-key)]
     (if (.hard-expired? ^Session smap) smap (allow-expired smap)))))

(defn allow-hard-expired
  "Temporarily mark hard-expired session as valid."
  ([src]
   (if-some [^Session smap (-session src)]
     (if (.hard-expired? ^Session smap) (allow-expired smap) smap)))
  ([src session-key]
   (if-some [^Session smap (-session src session-key)]
     (if (.hard-expired? ^Session smap) (allow-expired smap) smap))))

;; Request processing

(defn identify-session-path-compile
  "Returns a function which takes a request map and returns a session ID."
  [path]
  (let [[a b c d & more] path]
    (case (count path)
      0 #(get % :session-id)
      1 #(get % a)
      2 #(get (get % a) b)
      3 #(get (get (get % a) b) c)
      4 #(get (get (get (get % a) b) c) d)
      #(get-in % path))))

;; SQL defaults

(defn get-session-by-id
  "Standard session getter. Uses `db` to connect to a database and gets data identified
  by `sid` from a table `table`. Returns a map."
  [opts db table sid-db remote-ip]
  (sql/get-by-id db table sid-db db/opts-simple-map))

(defn get-last-active
  [opts db table sid-db remote-ip]
  (first (jdbc/execute-one! db
                            [(str "SELECT active FROM " table " WHERE id = ?") sid-db]
                            db/opts-simple-vec)))

(defn update-last-active
  ([opts db table sid-db remote-ip]
   (::jdbc/update-count
    (sql/update! db table {:active (t/now)} {:id sid-db} db/opts-simple-map)))
  ([opts db table sid-db remote-ip t]
   (::jdbc/update-count
    (sql/update! db table {:active (t/instant t)} {:id sid-db} db/opts-simple-map))))

(defn set-session
  [opts db table smap]
  (::jdbc/update-count
   (db/replace! db table
                (-> smap
                    (set/rename-keys {:db-id :id :db-token :secure-token})
                    (select-keys [:user-id :user-email :secure-token :id :ip :active :created]))
                db/opts-simple-map)))

(defn delete-user-vars
  [opts db sessions-table variables-table user-id]
  (jdbc/execute-one! db [(str-spc "DELETE FROM" variables-table
                                  "WHERE EXISTS (SELECT 1 FROM" sessions-table
                                  (str "WHERE " sessions-table ".user_id = ?")
                                  (str "AND " variables-table ".session_id = " sessions-table ".id)"))
                         user-id]))

;; Marking

(defn mkgood
  "Marks the given session `smap` as valid by setting `:valid?` field to `true`,
  `:expired?` and `:hard-expired?` fields to `false`, and `:error` field to
  `nil`. The given object should be a session."
  [^Session smap]
  (if-some [^Session smap (-session smap)]
    (-> (map/qassoc smap
                    :valid?        true
                    :expired?      false
                    :hard-expired? false
                    :error         nil))))

(defn mkbad
  "Marks session as invalid and sets `:err-id` field's value to the value of `:id`
  field, then sets `:id` to `nil`. The given object should be a session."
  ([^Session smap k v a b c d & pairs]
   (mkbad (apply map/qassoc smap k v a b c d pairs)))
  ([^Session smap k v a b c d]
   (mkbad (map/qassoc smap k v a b c d)))
  ([^Session smap k v a b]
   (mkbad (map/qassoc smap k v a b)))
  ([^Session smap k v]
   (mkbad (map/qassoc smap k v)))
  ([^Session smap]
   (if-some [^Session smap (-session smap)]
     (let [cause         (get (.error ^Session smap) :cause)
           expired?      (or (= :session/expired cause)
                             (and (= :session/bad-ip cause)
                                  (get (.config ^Session smap) :wrong-ip-expires)))
           hard-expired? (and expired? (hard-expired? smap))
           err-id        (or (.id ^Session smap) (.err-id ^Session smap))
           err-map       (.error ^Session smap)]
       (if (get err-map :severity)
         (map/qassoc smap
                     :valid?        false
                     :err-id        err-id
                     :expired?      expired?
                     :hard-expired? hard-expired?
                     :id            nil)
         (map/qassoc smap
                     :valid?        false
                     :err-id        err-id
                     :error         (map/qassoc err-map :severity :warn)
                     :expired?      expired?
                     :hard-expired? hard-expired?
                     :id            nil))))))

;; Configuration

(defn session-field
  "Returns a string or an ident of configured session ID field name by extracting it
  from `opts` which can be a map containing the last (or only) element of `:id-path`
  configuration option (exposed as `:id-field`), a request map containing the given
  `result-key` associated with a map with `:id-field`, or a keyword (returned
  immediately). Optional `other` map can be provided which will be used as a second
  try when `opts` lookup will fail. The function returns \"session-id\" string when
  all methods fail."
  ([opts]
   (session-field opts :session))
  ([opts result-key]
   (if (keyword? opts)
     opts
     (or (get opts :id-field)
         (get (get opts result-key) :id-field)
         (get (get opts :config) :id-field)
         (get (:config (get opts result-key)) :id-field)
         "session-id")))
  ([opts other result-key]
   (if (keyword? opts)
     opts
     (or (get opts :id-field)
         (get (get opts result-key) :id-field)
         (get (get opts :config) :id-field)
         (get (:config (get opts result-key)) :id-field)
         (:id-field other)
         (get (result-key other) :id-field)
         (get (:config other) :id-field)
         (get (get (result-key other) :config) :id-field)
         "session-id"))))

(defn- config-options
  [req opts-or-session-key]
  (if (keyword? opts-or-session-key)
    (if-some [^Session s (-session req (or opts-or-session-key :session))] (.config ^Session s))
    opts-or-session-key))

;; Session variables

(defn del-var!
  "Deletes a session variable `var-name` assigned to a session of the given ID (`sid`)
  or a session map (`smap`). Optional variable `names` can be given to perform a
  batch operation for multiple variables."
  {:arglists '([smap var-name & names]
               [opts sid var-name & names])}
  ([smap-or-opts var-name-or-sid & names]
   (if (session? smap-or-opts)
     (let [^Session smap smap-or-opts
           opts          (.config ^Session smap)
           db-sid        (db-sid-smap ^Session smap)
           deleter       (get (.config ^Session smap) :fn/xvar-del)
           var-name      var-name-or-sid]
       (if-not db-sid
         (log/err "Cannot delete session variable" var-name "because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (if names (apply deleter db-sid var-name names) (deleter db-sid var-name))))
     (let [db-sid   (db-sid-str var-name-or-sid)
           deleter  (get smap-or-opts :fn/var-del)
           var-name (first names)
           names    (next names)]
       (if-not db-sid
         (log/err "Cannot delete session variable" var-name "because session ID is not valid")
         (if names (apply deleter db-sid var-name names) (deleter db-sid var-name)))))))

(defn del-vars!
  "Deletes all session variables which belong to a session of the given ID (`sid`) or a
  session map (`smap`)."
  {:arglists '([smap]
               [opts sid]
               [opts smap])}
  ([smap]
   (if-some [^Session smap (-session smap)]
     (let [deleter (get (.config ^Session smap) :fn/var-del)
           db-sid  (db-sid-smap ^Session smap)]
       (if-not db-sid
         (log/err "Cannot delete session variables because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (deleter db-sid)))))
  ([opts sid-or-smap]
   (if (session? sid-or-smap)
     (let [^Session smap sid-or-smap
           opts          (or opts (.config ^Session smap))
           deleter       (get opts :fn/var-del)
           db-sid        (db-sid-smap ^Session smap)]
       (if-not db-sid
         (log/err "Cannot delete session variables because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (deleter db-sid)))
     (let [db-sid  (db-sid-str sid-or-smap)
           deleter (get opts :fn/var-del)]
       (if-not db-sid
         (log/err "Cannot delete session variables because session ID is not valid")
         (deleter db-sid))))))

(defn del-user-vars!
  "Deletes all session variables which belong to a user. The user may be specified as
  `user-id` or `smap` (indirectly)."
  {:arglists '([smap]
               [smap user-id]
               [opts user-id])}
  ([smap]
   (if-some [^Session smap (-session smap)]
     (let [deleter (get (.config ^Session smap) :fn/vars-del-user)
           user-id (.user-id ^Session smap)]
       (if-not user-id
         (log/err "Cannot delete session variables because user ID is not valid"
                  (log/for-user nil (user-email smap)))
         (deleter user-id)))))
  ([smap-or-opts user-id]
   (let [smap    (if (session? smap-or-opts) smap-or-opts)
         opts    (if smap (.config ^Session smap) smap-or-opts)
         deleter (get opts :fn/vars-del-user)]
     (if-not user-id
       (log/err "Cannot delete session variables because user ID is not set"
                (log/for-user (user-id smap) (user-email smap)))
       (deleter user-id)))))

(defn get-var
  "Gets a session variable and de-serializes it to a Clojure data structure."
  {:arglists '([smap var-name]
               [smap var-name & names]
               [opts sid var-name]
               [opts sid var-name & names])}
  ([smap-or-opts var-name-or-sid & names]
   (if (session? smap-or-opts)
     (let [^Session smap smap-or-opts
           getter        (get (.config ^Session smap) :fn/var-get)
           db-sid        (db-sid-smap smap)
           var-name      var-name-or-sid]
       (if-not db-sid
         (log/err "Cannot get session variable" var-name "because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (if names (apply getter db-sid var-name names) (getter db-sid var-name))))
     (let [getter   (get smap-or-opts :fn/var-get)
           db-sid   (db-sid-str var-name-or-sid)
           var-name (first names)
           names    (next names)]
       (if-not db-sid
         (log/err "Cannot get session variable" var-name "because session ID is not valid")
         (if names (apply getter db-sid var-name names) (getter db-sid var-name)))))))

(defn fetch-var!
  "Like `get-var` but removes session variable after it is successfully read from a
  database."
  {:arglists '([smap var-name & names]
               [opts sid var-name & names])}
  [smap-or-opts var-name-or-sid & names]
  (if (session? smap-or-opts)
    (let [^Session smap smap-or-opts
          sid           (.id ^Session smap)
          opts          (.config ^Session smap)
          getter        (get opts :fn/var-get)
          db-sid        (db-sid-smap smap)
          var-name      var-name-or-sid]
      (if-not db-sid
        (log/err "Cannot get session variable" var-name "because session ID is not valid"
                 (log/for-user (user-id smap) (user-email smap)))
        (if names
          (let [r (apply getter db-sid var-name names)] (apply del-var! opts sid var-name names) r)
          (let [v (getter db-sid var-name)] (del-var! opts sid var-name) v))))
    (let [sid      var-name-or-sid
          opts     smap-or-opts
          getter   (get opts :fn/var-get)
          db-sid   (db-sid-str sid)
          var-name (first names)
          names    (next names)]
      (if-not db-sid
        (log/err "Cannot get session variable" var-name "because session ID is not valid")
        (if names
          (let [r (apply getter db-sid var-name names)] (apply del-var! opts sid var-name names) r)
          (let [v (getter db-sid var-name)] (del-var! opts sid var-name) v))))))

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
  {:arglists '([smap var-name value & pairs]
               [opts sid var-name value & pairs])}
  [smap-or-opts var-name-or-sid value-or-var-name & pairs]
  (if (session? smap-or-opts)
    (let [^Session smap smap-or-opts
          setter        (get (.config ^Session smap) :fn/var-set)
          db-sid        (db-sid-smap smap)
          var-name      var-name-or-sid
          value         value-or-var-name]
      (if-not db-sid
        (log/err "Cannot store session variable" var-name "because session ID is not valid")
        (if pairs (apply setter db-sid var-name value pairs) (setter db-sid var-name value))))
    (let [setter   (get smap-or-opts :fn/var-set)
          db-sid   (db-sid-str var-name-or-sid)
          var-name value-or-var-name
          value    (first pairs)
          pairs    (next pairs)]
      (if-not db-sid
        (log/err "Cannot store session variable" var-name "because session ID is not valid")
        (if pairs (apply setter db-sid var-name value pairs) (setter db-sid var-name value))))))

;; Cache invalidation.

(defn invalidate-cache!
  "Invalidates cache."
  {:arglists '([req]
               [req opts]
               [req session-key]
               [opts sid ip-address]
               [invalidator-fn sid ip-address]
               [invalidator-fn smap ip-address])}
  ([req]
   (invalidate-cache! req :session))
  ([req opts-or-session-key]
   (if (keyword? opts-or-session-key)
     (if-some [^Session smap (-session req opts-or-session-key)]
       (if-some [invalidator (get (.config ^Session smap) :fn/invalidator)]
         (invalidator (or (.id ^Session smap) (.err-id ^Session smap)) (get req :remote-ip))))
     (if-some [invalidator (get opts-or-session-key :fn/invalidator)]
       (if-some [^Session smap (-session req (get opts-or-session-key :session-key))]
         (invalidator (or (.id ^Session smap) (.err-id ^Session smap)) (get req :remote-ip))))))
  ([opts-or-fn sid-or-smap ip-address]
   (if-some [invalidator (if (map? opts-or-fn) (get opts-or-fn :fn/invalidator) opts-or-fn)]
     (invalidator (if (session? sid-or-smap)
                    (or (.id ^Session sid-or-smap) (.err-id ^Session sid-or-smap))
                    sid-or-smap)
                  ip-address))))

;; Cache invalidation when time-sensitive value (last active time) exceeds TTL.

(defn refresh-times
  "If the time left before expiry is smaller than the cache TTL then the session map
  will be updated using a database query."
  {:arglists '([req]
               [req opts]
               [req session-key]
               [req opts smap remote-ip]
               [opts last-active-fn invalidator-fn cache-expires smap remote-ip])}
  ([req]
   (if-some [^Session smap (-session req)]
     (if-some [refresher (get (.config ^Session smap) :fn/refresh)]
       (refresher smap (get req :remote-ip)))))
  ([req opts-or-session-key]
   (if (keyword? opts-or-session-key)
     (if-some [^Session smap (-session req opts-or-session-key)]
       (if-some [refresher (get (.config ^Session smap) :fn/refresh)]
         (refresher smap (get req :remote-ip))))
     (if-some [refresher (get opts-or-session-key :fn/refresh)]
       (if-some [^Session smap (-session req (get opts-or-session-key :session-key))]
         (refresher smap (get req :remote-ip))))))
  ([req opts smap remote-ip]
   (if-some [refresher (get opts :fn/refresh)]
     (refresher smap remote-ip)))
  ([opts last-active-fn invalidator-fn cache-expires smap remote-ip]
   (or (if cache-expires
         (if-some [^Session smap (-session smap)]
           (if-some [last-active (.active ^Session smap)]
             (let [inactive-for (t/between last-active (t/now))]
               (when (t/> inactive-for cache-expires)
                 (invalidator-fn (or (.id ^Session smap) (.err-id ^Session smap)) remote-ip)
                 (if-some [last-active (last-active-fn (db-sid-smap smap) remote-ip)]
                   (map/qassoc smap :active last-active)))))))
       smap)))

;; Session handling, creation and prolongation

(defn handler
  "Processes session information by taking configuration options, session ID string,
  remote IP, request map and configuration options. It tries to get session ID string
  from form parameters of the request map and if the string is valid obtains session
  from a database using `getter-fn` (passing configuration options, database
  connection, table, session ID and remote IP to the call). The database connection
  object should be present in options under the `:db` key or given as an argument. If
  there is no session ID present in a request (obtained with `identifier-fn`), `nil`
  is returned."
  ([req]
   (handler req :session))
  ([req opts-or-session-key]
   (let [opts (config-options opts-or-session-key)]
     (handler opts
              (get opts :fn/getter)
              (get opts :fn/checker)
              (get opts :id-field)
              (get opts :session-key)
              (some-str ((get opts :fn/identifier) req))
              (get req :remote-ip))))
  ([req opts-or-session-key sid remote-ip]
   (let [opts (config-options req opts-or-session-key)]
     (handler opts
              (get opts :fn/getter)
              (get opts :fn/checker)
              (get opts :id-field)
              (get opts :session-key)
              sid remote-ip)))
  ([opts sid remote-ip]
   (handler opts
            (get opts :fn/getter)
            (get opts :fn/checker)
            (get opts :id-field)
            (get opts :session-key)
            sid remote-ip))
  ([opts getter-fn checker-fn session-id-field session-key sid remote-ip]
   (let [[sid-db pass] (split-secure-sid sid)
         secure?       (some? (not-empty pass))
         smap-db       (getter-fn sid-db remote-ip)
         ^Session smap (map->Session smap-db)
         smap          (if secure?
                         (-> smap
                             (map/qassoc :security-passed? (checker-fn pass (get smap :secure-token)))
                             (dissoc :secure-token))
                         smap)
         smap          (map/qassoc
                        smap
                        :id          sid
                        :db-id       sid-db
                        :ip          (ip/to-address (.ip ^Session smap))
                        :secure?     secure?
                        :session-key (or session-key (get opts :session-key) :session)
                        :id-field    (or session-id-field (get opts :id-field) "session-id")
                        :config      opts)
         stat          (state smap remote-ip)]
     (if (get stat :cause)
       (mkbad smap :error stat)
       (mkgood smap)))))

(defn process
  "Takes a session handler, last active time getter, last active time updater, a
  request map and an optional session options or a session key and validates session
  against a database or memoized session data (since `handler-fn` can use a
  cache). Returns a session map."
  {:arglists '([req]
               [req config]
               [req session-key]
               [handler-fn identifier-fn refresh-fn update-active-fn invalidator-fn req]
               [handler-fn identifier-fn refresh-fn update-active-fn invalidator-fn req config]
               [handler-fn identifier-fn refresh-fn update-active-fn invalidator-fn req session-key]
               [handler-fn identifier-fn refresh-fn update-active-fn invalidator-fn req opts session-id-field session-key])}
  ([req]
   (process req :session))
  ([req opts-or-session-key]
   (let [opts (config-options req opts-or-session-key)]
     (process (get opts :fn/handler)
              (get opts :fn/identifier)
              (get opts :fn/refresh)
              (get opts :fn/update-active)
              req
              opts
              (get opts :id-field)
              (get opts :session-key))))
  ([handler-fn identifier-fn refresh-fn update-active-fn req]
   (process handler-fn identifier-fn refresh-fn update-active-fn req :session))
  ([handler-fn identifier-fn refresh-fn update-active-fn req opts-or-session-key]
   (let [opts (config-options req opts-or-session-key)]
     (process handler-fn identifier-fn refresh-fn update-active-fn req opts
              (get opts :id-field) (get opts :session-key))))
  ([handler-fn identifier-fn refresh-fn update-active-fn req opts session-id-field session-key]
   (if-some [sid (some-str (identifier-fn req))]
     (if-not (sid-valid? sid)
       (mkbad (Session. sid nil nil nil nil nil nil nil nil
                        false false false false false
                        (or session-key (get opts :session-key) :session)
                        (or session-id-field (get opts :id-field) "session-id")
                        {:reason   "Malformed session-id parameter"
                         :cause    :session/malformed-session-id
                         :severity :info}
                        opts)
              :id-path (get opts :id-path))
       (let [remote-ip     (get req :remote-ip)
             ^Session smap (handler-fn sid remote-ip)]
         (if-not (valid? smap)
           smap
           (let [^Session smap (refresh-fn smap remote-ip)]
             (if-not (valid? smap)
               smap
               (if (pos-int? (update-active-fn (db-sid-smap smap) remote-ip))
                 (mkgood smap)
                 (mkbad smap
                        :error {:severity :error
                                :cause    :session/db-problem
                                :reason   (some-str-spc
                                           "Problem updating session data"
                                           (log/for-user
                                            (.user-id    ^Session smap)
                                            (.user-email ^Session smap)
                                            (or (ip/plain-ip-str (ip/to-address (.ip ^Session smap)))
                                                (:remote-ip/str req))))})))))))
     (Session. nil nil nil nil nil nil nil nil nil
               false false false false false
               (or session-key (get opts :session-key) :session)
               (or session-id-field (get opts :id-field) "session-id")
               nil opts))))

(defn prolong
  "Re-validates session by updating its timestamp and re-running validation."
  {:arglists '([req]
               [req config]
               [req session-key]
               [req config smap ip-address]
               [req session-key smap ip-address]
               [opts handler-fn update-active-fn invalidator-fn smap ip-address])}
  ([req]
   (prolong req :session))
  ([req opts-or-session-key]
   (if (keyword? opts-or-session-key)
     (if-some [^Session smap (-session req opts-or-session-key)]
       (if-some [prolonger (get (.config ^Session smap) :fn/prolong)]
         (prolonger smap (get req :ip-address))))
     (if-some [prolonger (get opts-or-session-key :fn/prolong)]
       (if-some [^Session smap (-session req (get opts-or-session-key :session-key))]
         (prolonger smap (get req :ip-address))))))
  ([opts smap ip-address]
   (if-some [prolonger (get opts :fn/prolong)]
     (prolonger smap ip-address)))
  ([req opts-or-session-key smap ip-address]
   (let [opts (config-options req opts-or-session-key)]
     (if-some [prolonger (get opts :fn/prolong)]
       (prolonger smap ip-address))))
  ([opts handler-fn update-active-fn invalidator-fn smap ip-address]
   (if-some [^Session smap (-session smap)]
     (if-some [sid (or (.err-id ^Session smap) (.id ^Session smap))]
       (let [ip-address (ip/to-address ip-address)
             ipplain    (ip/plain-ip-str ip-address)
             new-time   (t/now)
             sid-db     (or (db-sid-smap smap) (db-sid-str sid))]
         (log/msg "Prolonging session" (log/for-user (.user-id ^Session smap) (.user-email ^Session smap) ipplain))
         (let [test-smap (map/qassoc smap :id sid :active new-time)
               stat      (state test-smap ip-address)]
           (invalidator-fn sid ip-address)
           (if (correct? (get stat :cause))
             (do (update-active-fn sid-db ip-address (t/instant new-time))
                 (map/qassoc (handler-fn sid ip-address) :prolonged? true))
             (do (log/wrn "Session re-validation error" (log/for-user (:user-id smap) (:user-email smap) ipplain))
                 (mkbad smap :error stat)))))))))

(defn create
  "Creates a new session and puts it into a database. Returns the created session map."
  {:arglists '([req  user-id user-email ip-address]
               [opts user-id user-email ip-address]
               [smap user-id user-email ip-address]
               [req opts user-id user-email ip-address]
               [req session-key user-id user-email ip-address]
               [opts setter-fn invalidator-fn var-del-fn vars-del-user-fn
                single-session? secured? session-id-field session-key
                user-id user-email ip-address])}
  ([opts-or-req user-id user-email ip-address]
   (if (session? opts-or-req)
     (let [create-fn (get (.config ^Session opts-or-req) :fn/create)]
       (create-fn user-id user-email ip-address))
     (if-some [create-fn (get opts-or-req :fn/create)]
       (create-fn user-id user-email ip-address)
       (create opts-or-req :session user-id user-email ip-address))))
  ([req opts-or-session-key user-id user-email ip-address]
   (let [opts (config-options req opts-or-session-key)]
     (if-some [creator (get opts :fn/create)]
       (creator user-id user-email ip-address))))
  ([opts setter-fn invalidator-fn var-del-fn vars-del-user-fn single-session? secured?
    session-id-field session-key user-id user-email ip-address]
   (let [user-id    (valuable user-id)
         user-email (some-str user-email)]
     (if-not (and user-id user-email)
       (do (if-not user-id    (log/err "No user ID given when creating a session"))
           (if-not user-email (log/err "No user e-mail given when creating a session"))
           nil)
       (let [t             (t/now)
             ip            (ip/to-address ip-address)
             ipplain       (ip/plain-ip-str ip)
             id-field      (or session-id-field (get opts :id-field) "session-id")
             skey          (or session-key (get opts :session-key) :session)
             ^Session sess (Session. nil nil nil nil user-id user-email t t ip
                                     false false false false false skey id-field nil opts)
             sess          (gen-session-id sess secured? user-id ipplain)
             sid-db        (db-sid-smap sess)
             stat          (state sess ip)]
         (log/msg "Opening session" (log/for-user user-id user-email ipplain))
         (if-not (correct? (get stat :cause))
           (do (log/err "Session incorrect after creation" (log/for-user user-id user-email ipplain))
               (mkbad sess :error stat))
           (let [updated-count (setter-fn sess)
                 sess          (map/qassoc sess :db-token nil)]
             (invalidator-fn (or (.id ^Session sess) (.err-id ^Session sess)) ip)
             (if (pos-int? updated-count)
               (do (if single-session?
                     (vars-del-user-fn user-id)
                     (var-del-fn sess))
                   (mkgood sess))
               (do (log/err "Problem saving session" (log/for-user user-id user-email ipplain))
                   (mkbad sess
                          :error  {:reason   (str "Session cannot be saved"
                                                  (log/for-user user-id user-email ipplain))
                                   :cause    :session/db-problem
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
    (map/qassoc config :cache-expires
                (if (and expires cache-ttl)
                  (if (t/> cache-ttl expires)
                    one-second
                    (t/- expires cache-ttl))))))

(defn- setup-fn
  [config k default]
  (or (var/deref (get config k)) default))

(defn- setup-id-fn
  [id-path]
  (let [id-path (if (coll? id-path) id-path (cons id-path nil))]
    (identify-session-path-compile id-path)))

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
                               (update :token-cache-ttl  time/parse-duration)
                               (update :token-cache-size safe-parse-long)
                               (update :session-key      #(or (some-keyword %) :session))
                               (update :id-path          #(if (valuable? %) (if (coll? %) (vec %) %) "session-id"))
                               (update :id-field         #(if (ident? %) % (some-str %)))
                               (update :single-session?  boolean)
                               (update :secured?         boolean)
                               (calc-cache-expires))
        db                 (get config :db)
        session-key        (get config :session-key)
        sessions-table     (get config :table/sessions)
        variables-table    (get config :table/variables)
        session-id-path    (get config :id-path)
        session-id-field   (get config :id-field)
        cache-expires      (get config :expires)
        single-session?    (get config :single-session?)
        secured?           (get config :secured?)
        checker-config     (set/rename-keys config {:token-cache-size :cache-size :token-cache-ttl :cache-ttl})
        session-id-field   (or session-id-field (if (coll? session-id-path) (last session-id-path) session-id-path))
        config             (assoc config :id-field (or session-id-field "session-id"))
        identifier-fn      (setup-id-fn session-id-path)
        config             (assoc config :fn/identifier identifier-fn)
        getter-fn          (setup-fn config :fn/getter get-session-by-id)
        getter-fn-w        #(getter-fn config db sessions-table %1 %2)
        config             (assoc config :fn/getter getter-fn-w)
        checker-fn         (setup-fn config :fn/checker check-encrypted)
        checker-fn-w       (db/memoizer checker-fn checker-config)
        config             (assoc config :fn/checker checker-fn-w)
        pre-handler        #(handler config getter-fn-w checker-fn-w session-id-field session-key %1 %2)
        mem-handler        (db/memoizer pre-handler config)
        invalidator-fn     (setup-invalidator pre-handler mem-handler)
        config             (assoc config :fn/invalidator invalidator-fn :fn/handler mem-handler)
        last-active-fn     (setup-fn config :fn/last-active get-last-active)
        update-active-fn   (setup-fn config :fn/update-active update-last-active)
        last-active-fn-w   #(last-active-fn config db sessions-table %1 %2)
        config             (assoc config :fn/last-active last-active-fn-w)
        update-active-fn-w (fn
                             ([sid-db remote-ip]
                              (update-active-fn config db sessions-table sid-db remote-ip))
                             ([sid-db remote-ip t]
                              (update-active-fn config db sessions-table sid-db remote-ip t)))
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
        var-get-fn         (fn
                             ([session-id setting-id]
                              (var-get-core-fn db session-id setting-id))
                             ([session-id setting-id & setting-ids]
                              (apply var-get-core-fn db session-id setting-id setting-ids)))
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
                                    setter-fn-w invalidator-fn var-del-fn vars-del-user-fn-w
                                    single-session? secured? session-id-field session-key
                                    %1 %2 %3)
        config             (assoc config :fn/create create-fn)]
    (log/msg "Installing session handler:" k)
    (if dbname (log/msg "Using database" dbname "for storing sessions"))
    {:name    (keyword k)
     :config  config
     :compile (fn [{:keys [no-session?]} opts]
                (if (and (not no-session?) db)
                  (fn [h]
                    (fn [req]
                      (h
                       (map/qassoc
                        req
                        session-key (delay (process mem-handler
                                                    identifier-fn
                                                    refresh-fn
                                                    update-active-fn-w
                                                    req config
                                                    session-id-field session-key))))))))}))

(system/add-init  ::default [k config] (wrap-session k config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
