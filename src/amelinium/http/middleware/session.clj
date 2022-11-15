(ns

    ^{:doc    "amelinium service, session middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.session

  (:refer-clojure :exclude [parse-long uuid random-uuid empty?])

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
            [amelinium.proto.session      :as          p]
            [io.randomseed.utils          :refer    :all]
            [io.randomseed.utils.time     :as       time]
            [io.randomseed.utils.var      :as        var]
            [io.randomseed.utils.map      :as        map]
            [io.randomseed.utils.ip       :as         ip]
            [io.randomseed.utils.db.types :as      types])

  (:import [java.time Instant Duration]
           [javax.sql DataSource]
           [inet.ipaddr IPAddress]
           [amelinium.proto.session SessionControl Sessionable]))

(def ^:const sid-match (re-pattern "|^[a-f0-9]{30,128}(-[a-f0-9]{30,128})?$"))

(def one-second (t/new-duration 1 :seconds))

(defrecord SessionConfig
    [^DataSource                    db
     ^String                        sessions-table
     ^String                        variables-table
     ^clojure.lang.Keyword          session-key
     ^clojure.lang.PersistentVector id-path
     ^Object                        id-field
     ^Duration                      expires
     ^Duration                      hard-expires
     ^Duration                      cache-ttl
     ^Long                          cache-size
     ^Duration                      token-cache-ttl
     ^Duration                      token-cache-size
     ^Duration                      cache-expires
     ^Boolean                       single-session?
     ^Boolean                       secured?])

(defn config?
  ^Boolean [v]
  (instance? SessionConfig v))

(defrecord Session
    [^String                        id
     ^String                        err-id
     ^String                        db-id
     ^String                        db-token
     ^Long                          user-id
     ^String                        user-email
     ^Instant                       created
     ^Instant                       active
     ^IPAddress                     ip
     ^Boolean                       valid?
     ^Boolean                       expired?
     ^Boolean                       hard-expired?
     ^Boolean                       secure?
     ^Boolean                       security-passed?
     ^String                        session-key
     ^Object                        id-field
     ^clojure.lang.IPersistentMap   error
     ^SessionControl                control])

(defn session?
  ^Boolean [v]
  (instance? Session v))

(extend-protocol p/Sessionable

  Session

  (session (^Session [src] src) (^Session [src _] src))

  (inject  ([dst smap] smap) ([dst smap _] smap))

  (empty?
    (^Boolean [src]
     (and (nil? (.id     ^Session src))
          (nil? (.err-id ^Session src))
          (nil? (.error  ^Session src))))
    (^Boolean [src _]
     (and (nil? (.id     ^Session src))
          (nil? (.err-id ^Session src))
          (nil? (.error  ^Session src)))))

  (control
    (^SessionControl [src]   (.control ^Session src))
    (^SessionControl [src _] (.control ^Session src)))

  SessionControl

  (control (^SessionControl [src] src) (^SessionControl [src _] src))
  (^Boolean -empty? [src] (not (config? (p/config src))))

  clojure.lang.Associative

  (session
    (^Session [req]             (if-some [s (get req :session)] s))
    (^Session [req session-key] (if-some [s (get req (or session-key :session))] s)))

  (empty?
    (^Boolean [req]
     (if-some [^Session s (p/session req :session)]
       (and (nil? (.id     ^Session s))
            (nil? (.err-id ^Session s))
            (nil? (.error  ^Session s)))
       true))
    (^Boolean [req session-key]
     (if-some [^Session s (p/session req session-key)]
       (and (nil? (.id     ^Session s))
            (nil? (.err-id ^Session s))
            (nil? (.error  ^Session s)))
       true)))

  (inject
    (^Session [dst smap]
     (if-some [^Session smap (p/session smap)]
       (map/qassoc dst (or  (.session-key ^Session smap)
                            (if-some [^SessionControl ctrl (.control ^Session smap)]
                              (if-some [^SessionConfig cfg (p/config ^SessionControl ctrl)]
                                (.session-key ^SessionConfig cfg)))
                            :session)
                   smap)
       dst))
    (^Session [dst smap session-key]
     (if-some [^Session smap (p/session smap)]
       (map/qassoc dst (or session-key
                           (.session-key ^Session smap)
                           (if-some [^SessionControl ctrl (.control ^Session smap)]
                             (if-some [^SessionConfig cfg (p/config ^SessionControl ctrl)]
                               (.session-key ^SessionConfig cfg)))
                           :session)
                   smap)
       dst)))

  (control
    (^SessionControl [req]
     (if-some [^Session s (get req :session)]
       (.control ^Session s)))
    (^SessionControl [req session-key]
     (if-some [^Session s (get req session-key)]
       (.control ^Session s))))

  nil

  (session
    ([src] nil)
    ([src session-key] nil))

  (empty?
    ([src] true)
    ([src session-key] true))

  (inject
    ([src smap] nil)
    ([src smap session-key] nil))

  (control
    ([src] nil)
    ([src session-key] nil)))

(declare db-sid-smap)

(extend-protocol p/SessionControl

  SessionConfig

  (^SessionConfig p/config [s] s)

  Session

  (get-var       [s k]   (p/get-var       (.control ^Session s) (db-sid-smap s) k))
  (get-vars      [s ks]  (p/get-vars      (.control ^Session s) (db-sid-smap s) ks))
  (put-var       [s k v] (p/put-var       (.control ^Session s) (db-sid-smap s) k v))
  (put-vars      [s kvs] (p/put-vars      (.control ^Session s) (db-sid-smap s) kvs))
  (del-var       [s k]   (p/del-var       (.control ^Session s) (db-sid-smap s) k))
  (del-vars      [s ks]  (p/del-vars      (.control ^Session s) (db-sid-smap s) ks))
  (del-svars     [s]     (p/del-svars     (.control ^Session s) (db-sid-smap s)))
  (del-uvars     [s]     (p/del-uvars     (.control ^Session s) (.user-id ^Session s)))

  (config
    (^SessionConfig [s]       (p/config (.control ^Session s)))
    (^SessionConfig [s _]     (p/config (.control ^Session s))))

  (to-db
    (^Long    [s]             (p/to-db (.control ^Session s) s))
    (^Long    [s _]           (p/to-db (.control ^Session s) s)))

  (identify
    ([s]                      (or (.id ^Session s) (.err-id ^Session s)))
    ([s req]                  (or (.id ^Session s) (.err-id ^Session s)
                                  (p/identify (.control ^Session s) req))))

  (from-db
    (^Session [s]             (p/from-db (.control ^Session s) (db-sid-smap s) (.ip ^Session s)))
    (^Session [s db-sid]      (p/from-db (.control ^Session s) db-sid (.ip ^Session s)))
    (^Session [s db-sid ip]   (p/from-db (.control ^Session s) db-sid ip)))

  (handle
    (^Session [s]             (p/handle (.control ^Session s) (p/identify ^Session s) (.ip ^Session s)))
    (^Session [s sid]         (p/handle (.control ^Session s) sid (.ip ^Session s)))
    (^Session [s sid ip]      (p/handle (.control ^Session s) sid ip)))

  (invalidate
    ([s]                      (p/invalidate (.control ^Session s) (p/identify ^Session s) (.ip ^Session s)))
    ([s sid]                  (p/invalidate (.control ^Session s) sid (.ip ^Session s)))
    ([s sid ip]               (p/invalidate (.control ^Session s) sid ip)))

  (get-active
    (^Instant [s]             (p/get-active (.control ^Session s) (p/identify ^Session s) (.ip ^Session s)))
    (^Instant [s db-sid]      (p/get-active (.control ^Session s) db-sid (.ip ^Session s)))
    (^Instant [s db-sid ip]   (p/get-active (.control ^Session s) db-sid ip)))

  (set-active
    (^Long [s]                (p/set-active (.control ^Session s) (p/identify ^Session s) (.ip ^Session s)))
    (^Long [s db-sid]         (p/set-active (.control ^Session s) db-sid (.ip ^Session s)))
    (^Long [s db-sid ip]      (p/set-active (.control ^Session s) db-sid ip))
    (^Long [s db-sid ip t]    (p/set-active (.control ^Session s) db-sid ip t)))

  clojure.lang.Associative

  (config
    (^SessionConfig [src]             (p/config (p/session src)))
    (^SessionConfig [src session-key] (p/config (p/session src session-key))))

  (-identify
    (^String [req]
     (or (p/identify (p/session req))
         (some-str (or (get-in req [:params :session-id])
                       (get-in req [:params "session-id"])))))

    (^String [req session-key-or-req-path]
     (if (coll? session-key-or-req-path)
       (some-str (get-in req session-key-or-req-path))
       (p/identify (p/session req session-key-or-req-path)))))

  nil

  (to-db       [s]       nil)
  (token-ok?   [s e p] false)
  (config     ([s]             nil) ([s s-k]       nil))
  (identify   ([s]             nil) ([s req]       nil))
  (from-db    ([s db-sid ip]   nil) ([s db-sid]    nil) ([s]        nil))
  (handle     ([s db-sid ip]   nil) ([s db-sid]    nil) ([s]        nil))
  (invalidate ([s db-sid ip]   nil) ([s db-sid]    nil) ([s]        nil))
  (get-active ([s db-sid ip]   nil  ([s db-sid]    nil) ([s]        nil)))
  (set-active ([s db-sid ip t] nil  ([s db-sid ip] nil) ([s db-sid] nil) ([s] nil)))
  (get-var    ([s db-sid k]    nil) ([s k]         nil))
  (get-vars   ([s db-sid ks]   nil) ([s ks]        nil))
  (put-var    ([s db-sid k v]  nil) ([s k v]       nil))
  (put-vars   ([s db-sid kvs]  nil) ([s kvs]       nil))
  (del-var    ([s db-sid k]    nil) ([s k]         nil))
  (del-vars   ([s db-sid ks]   nil) ([s ks]        nil))
  (del-svars  ([s db-sid]      nil) ([s]           nil))
  (del-uvars  ([s uid]         nil) ([s]           nil)))

(defn of
  "Returns a session record of type `Session` on a basis of configuration source
  provided and an optional `session-key` if session must be looked in an associative
  structure (defaults to `:session`)."
  (^Session [src] (p/session src))
  (^Session [src session-key] (p/session src session-key)))

(defn empty?
  "Returns `false` is `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` can be given to express a key in associative
  structure (defaults to `:session`)."
  (^Boolean [src] (p/empty? src))
  (^Boolean [src session-key] (p/empty? src session-key)))

(defn not-empty?
  "Returns `true` is `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` can be given to express a key in associative
  structure (defaults to `:session`)."
  (^Boolean [src] (not (p/empty? src)))
  (^Boolean [src session-key] (not (p/empty? src session-key))))

(defn inject
  "Returns an object updated with session record of type `Session` under an optional
  `session-key` if session is to be put into an associative structure (defaults to
  `:session`)."
  ([dst smap] (p/inject dst smap))
  ([dst smap session-key] (p/inject dst smap session-key)))

(defn not-empty-of
  "Returns a session if `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` can be given to express a key in associative
  structure (defaults to `:session`). Returns `nil` if session is not usable (does
  not have `:id`, `:err-id` not `:error` set)."
  (^Session [^Sessionable  src]
   (let [^Session s (p/session src)]
     (if-not (p/empty? s) s)))
  (^Session [^Sessionable src session-key]
   (let [^Session s (p/session src session-key)]
     (if-not (p/empty? s) s))))

(defn control?
  ^Boolean [v]
  (satisfies? SessionControl v))

(defn control
  (^SessionControl [src] (p/control src))
  (^SessionControl [src session-key] (p/control src)))

(defn config
  (^SessionConfig [src] (p/config src))
  (^SessionConfig [src session-key] (p/config src session-key)))

(defn id
  ([src] (if-some [^Session s (p/session src)] (.id ^Session src)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (.id ^Session src))))

(defn err-id
  ([src] (if-some [^Session s (p/session src)] (.err-id ^Session src)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (.err-id ^Session src))))

(defn any-id
  ([src] (if-some [^Session s (p/session src)] (or (.id ^Session src) (.err-id ^Session src))))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (or  (.id ^Session src) (.err-id ^Session src)))))

(defn db-token
  ([src] (if-some [^Session s (p/session src)] (.db-token ^Session src)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (.db-token ^Session src))))

(defn user-id
  ([src] (if-some [^Session s (p/session src)] (.user-id ^Session src)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (.user-id ^Session src))))

(defn user-email
  ([src] (if-some [^Session s (p/session src)] (.user-email ^Session src)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (.user-email ^Session src))))

(defn created
  ([src] (if-some [^Session s (p/session src)] (.created ^Session src)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (.created ^Session src))))

(defn active
  ([src] (if-some [^Session s (p/session src)] (.active ^Session src)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (.active ^Session src))))

(defn ip
  ([src] (if-some [^Session s (p/session src)] (.ip ^Session src)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (.ip ^Session src))))

(defn session-key
  ([src] (if-some [^Session s (p/session src)] (or (.session-key ^Session src) :session)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (or (.session-key ^Session src) :session))))

(defn id-field
  ([src] (if-some [^Session s (p/session src)] (.id-field ^Session src)))
  ([src session-key] (if-some [^Session s (p/session src session-key)] (.id-field ^Session src))))

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
  [plain-token encrypted-token-b64-str]
  (if (and plain-token encrypted-token-b64-str)
    (if-some [salt-pass (str/split encrypted-token-b64-str salt-splitter 2)]
      (crypto/eq? (b64u->bytes (nth salt-pass 1 nil))
                  (get (scrypt/encrypt plain-token
                                       (b64u->bytes (nth salt-pass 0 nil))
                                       scrypt-options) :password)))))

(defn split-secure-sid
  [session-id]
  (str/split session-id token-splitter 2))

(defn db-id
  ([src]
   (if-some [^Session s (p/session src)]
     (.db-id ^Session src)))
  ([src session-key]
   (if-some [^Session s (p/session src session-key)]
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
  "Checks if a session is secure according to configured security level. If `:secured?`
  option is not enabled in configuration, it returns `true`. If `:secure?` flag is
  set to a truthy value, it returns it.  If there is no session, it returns `false`."
  (^Boolean [src]
   (if-some [^Session s (p/session src)]
     (or (.secure? ^Session s)
         (not (if-some [^SessionConfig c (config ^Session s)] (.secured? ^SessionConfig c))))
     false))
  (^Boolean [src session-key]
   (if-some [^Session s (p/session src session-key)]
     (or (.secure? ^Session s)
         (not (if-some [^SessionConfig c (config ^Session s)] (.secured? ^SessionConfig c))))
     false)))

(defn insecure?
  "Checks if session is not secure where it should be. If `:secured?` option is not
  enabled in configuration, it returns `false`. If `:secure?` flag is set to a falsy
  value, it returns `false`. If there is no session, it returns `true`."
  (^Boolean [src]
   (if-some [^Session s (p/session src)]
     (and (not (.secure? ^Session s))
          (if-some [^SessionConfig c (config ^Session s)] (.secured? ^SessionConfig c) false))
     true))
  (^Boolean [src session-key]
   (if-some [^Session s (p/session src session-key)]
     (and (not (.secure? ^Session s))
          (if-some [^SessionConfig c (config ^Session s)] (.secured? ^SessionConfig c) false))
     true)))

(defn security-passed?
  "Checks if the additional security token was validated correctly or there was not a
  need to validate it because the session is not secure (in such case returns
  `true`). Does not test if session should be secured; to check it, use `secure?` or
  `insecure?`."
  (^Boolean [src]
   (if-some [^Session s (p/session src)]
     (or (not (.secure? ^Session s))
         (.security-passed? ^Session s))
     false))
  (^Boolean [src session-key]
   (if-some [^Session s (p/session src session-key)]
     (or (not (.secure? ^Session s))
         (.security-passed? ^Session s))
     false)))

(defn security-failed?
  "Checks if the additional security token was validated incorrectly unless the session
  is not secure (in such case it returns `false`). Does not test if session should be
  secured; to check it, use `secure?` or `insecure?`."
  (^Boolean [src]
   (if-some [^Session s (p/session src)]
     (and (.secure? ^Session s)
          (not (.security-passed? ^Session s)))
     true))
  (^Boolean [src session-key]
   (if-some [^Session s (p/session src session-key)]
     (and  (.secure? ^Session s)
           (not (.security-passed? ^Session s)))
     true)))

(defn ip-state
  ([src session-key user-id user-email remote-ip]
   (ip-state (p/session src session-key) user-id user-email remote-ip))
  ([src user-id user-email remote-ip]
   (if-some [^Session smap (p/session src)]
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
   (calc-expired? (p/session src session-key)))
  (^Boolean [src]
   (if-some [^Session smap (p/session src)]
     (if-some [^SessionConfig cfg (config ^Session smap)]
       (if-some [exp (.expires ^SessionConfig cfg)]
         (and (pos-int? (time/seconds exp))
              (time-exceeded? (.active ^Session smap) (t/now) exp)))))))

(defn calc-hard-expired?
  (^Boolean [src session-key]
   (calc-hard-expired? (p/session src session-key)))
  (^Boolean [src]
   (if-some [^Session smap (p/session src)]
     (if-some [^SessionConfig cfg (config ^Session smap)]
       (if-some [hexp (.hard-expires ^SessionConfig cfg)]
         (and (pos-int? (time/seconds hexp))
              (time-exceeded? (.active ^Session smap) (t/now) hexp)))))))

(defn calc-soft-expired?
  (^Boolean [src session-key]
   (calc-soft-expired? (p/session src session-key)))
  (^Boolean [src]
   (if-some [^Session smap (p/session src)]
     (and (calc-expired? ^Session smap)
          (not (calc-hard-expired? ^Session smap))))))

(defn expired?
  ([src]
   (if-some [^Session s (p/session src)]
     (.expired? ^Session s)
     false))
  ([src session-key]
   (if-some [^Session s (p/session src session-key)]
     (.expired? ^Session s)
     false)))

(defn hard-expired?
  ([src]
   (if-some [^Session s (p/session src)]
     (.hard-expired? ^Session s)
     false))
  ([src session-key]
   (if-some [^Session s (p/session src session-key)]
     (.hard-expired? ^Session s)
     false)))

(defn soft-expired?
  ([src]
   (if-some [^Session s (p/session src)]
     (and (.expired? ^Session s)
          (not (.hard-expired? ^Session s)))
     false))
  ([src session-key]
   (if-some [^Session s (p/session src session-key)]
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
        :ip-address-check           (ip-state smap user-id user-email ip-address)))))

(defn correct?
  "Returns `true` if a session exists and its state is correct. Never throws an
  exception."
  (^Boolean [state-result]
   (nil? state-result))
  (^Boolean [src ip-address]
   (nil? (state (try (p/session src) (catch Throwable _ nil)) ip-address)))
  (^Boolean [src session-key ip-address]
   (nil? (state (try (p/session src session-key) (catch Throwable _ nil)) ip-address))))

(defn valid?
  "Returns `true` if a session is marked as valid."
  (^Boolean [src]
   (if-some [^Session s (p/session src)]
     (.valid? ^Session s)
     false))
  (^Boolean [src session-key]
   (if-some [^Session s (p/session src session-key)]
     (.valid? ^Session s)
     false)))

(defn error?
  ([src]
   (if-some [^Session s (p/session src)]
     (some? (.error ^Session s))))
  ([src session-key]
   (if-some [^Session s (p/session src session-key)]
     (some? (.error ^Session s)))))

(defn error
  ([src]
   (if-some [^Session s (p/session src)]
     (.error ^Session s)))
  ([src session-key]
   (if-some [^Session s (p/session src session-key)]
     (.error ^Session s))))

(defn allow-expired
  "Temporarily marks expired session as valid."
  ([src]
   (if-some [^Session smap (p/session src)]
     (if (and (.expired?      ^Session smap)
              (not   (.valid? ^Session smap))
              (nil?  (.id     ^Session smap))
              (some? (.err-id ^Session smap )))
       (map/qassoc smap :valid? true :id (.err-id ^Session smap))
       smap)))
  ([src session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if (and (.expired?      ^Session smap)
              (not   (.valid? ^Session smap))
              (nil?  (.id     ^Session smap))
              (some? (.err-id ^Session smap )))
       (map/qassoc smap :valid? true :id (.err-id ^Session smap))
       smap))))

(defn allow-soft-expired
  "Temporarily mark soft-expired session as valid."
  ([src]
   (if-some [^Session smap (p/session src session-key)]
     (if (.hard-expired? ^Session smap) smap (allow-expired smap))))
  ([src session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if (.hard-expired? ^Session smap) smap (allow-expired smap)))))

(defn allow-hard-expired
  "Temporarily mark hard-expired session as valid."
  ([src]
   (if-some [^Session smap (p/session src)]
     (if (.hard-expired? ^Session smap) (allow-expired smap) smap)))
  ([src session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if (.hard-expired? ^Session smap) (allow-expired smap) smap))))

;; Request processing

(defn identify-session-path-compile
  "Returns a function which takes a request map and returns a session ID."
  [path]
  (let [[a b c d & more] path]
    (case (count path)
      0 (fn ^String [req] (if-some [p (get req :params)] (some-str (or (get p :session-id) (get p "session-id")))))
      1 (fn ^String [req] (some-str (get req a)))
      2 (fn ^String [req] (get (get req a) b))
      3 (fn ^String [req] (get (get (get req a) b) c))
      4 (fn ^String [req] (get (get (get (get req a) b) c) d))
      (fn   ^String [req] (get-in req path)))))

;; SQL defaults

(defn get-session-by-id
  "Standard session getter. Uses `db` to connect to a database and gets data identified
  by `sid` from a table `table`. Returns a map."
  [opts db table sid-db remote-ip]
  (sql/get-by-id db table sid-db db/opts-simple-map))

(defn get-last-active
  ^Instant [opts db table sid-db remote-ip]
  (first (jdbc/execute-one! db
                            [(str "SELECT active FROM " table " WHERE id = ?") sid-db]
                            db/opts-simple-vec)))

(defn update-last-active
  (^Long [opts db table sid-db remote-ip]
   (::jdbc/update-count
    (sql/update! db table {:active (t/now)} {:id sid-db} db/opts-simple-map)))
  (^Long [opts db table sid-db remote-ip t]
   (::jdbc/update-count
    (sql/update! db table {:active (t/instant t)} {:id sid-db} db/opts-simple-map))))

(defn set-session
  ^Long [opts db table smap]
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

(defn delete-session-vars
  [opts db sessions-table variables-table db-id]
  (jdbc/execute-one! db [(str-spc "DELETE FROM" variables-table
                                  "WHERE EXISTS (SELECT 1 FROM" sessions-table
                                  (str "WHERE " sessions-table ".id = ?")
                                  (str "AND " variables-table ".session_id = " sessions-table ".id)"))
                         db-id]))

;; Marking

(defn mkgood
  "Marks the given session `smap` as valid by setting `:valid?` field to `true`,
  `:expired?` and `:hard-expired?` fields to `false`, and `:error` field to
  `nil`. The given object should be a session."
  [^Session smap]
  (if-some [^Session smap (p/session smap)]
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
   (if-some [^Session smap (p/session smap)]
     (let [cause         (get (.error ^Session smap) :cause)
           expired?      (or (= :session/expired cause)
                             (and (= :session/bad-ip cause)
                                  (if-some [^SessionConfig cfg (config ^Session smap)]
                                    (.wrong-ip-expires ^SessionConfig cfg))))
           hard-expired? (and expired? (calc-hard-expired? smap))
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
    (if-some [^Session s (p/session req (or opts-or-session-key :session))]
      (config ^Session s))
    opts-or-session-key))

;; Session variables

(defn del-var!
  "Deletes a session variable `var-name`."
  ([src var-name]
   (del-var! src :session var-name))
  ([src session-key var-name]
   (if var-name
     (let [^Session smap (p/session src)
           db-sid        (db-sid-smap ^Session smap)]
       (if-not db-sid
         (log/err "Cannot delete session variable" var-name "because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (p/del-var (.control ^Session smap) db-sid var-name))))))

(defn del-vars!
  "Deletes a session variables from `var-names`."
  ([src var-names]
   (del-vars! src :session var-names))
  ([src session-key var-names]
   (if (not-empty var-names)
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap ^Session smap)]
       (if-not db-sid
         (log/err "Cannot delete session variable" (first var-names)
                  "because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (p/del-vars (.control ^Session smap) db-sid var-names))))))

(defn del-all-vars!
  "Deletes all session variables which belong to a user (is `single-session?`
  configuration option is `true`) or just variables for this session (if
  `single-session?` configuration option is `false`)."
  ([src session-key]
   (if-some [^Session smap (p/session src session-key)]
     (let [^SessionControl ctrl (.control ^Session smap)
           ^SessionConfig  opts (p/config ^SessionControl ctrl)]
       (if (.single-session? ^SessionConfig opts)
         (if-some [user-id (.user-id ^Session smap)]
           (p/del-uvars ^SessionControl ctrl user-id)
           (log/err "Cannot delete session variables because user ID is not valid"
                    (log/for-user nil (user-email smap))))
         (if-some [db-sid (db-sid-smap ^Session smap)]
           (p/del-svars ^SessionControl ctrl db-sid)
           (log/err "Cannot delete session variables because session ID is not valid"
                    (log/for-user (user-id smap) (user-email smap))))))))
  ([src]
   (del-all-vars! src :session)))

(defn del-user-vars!
  "Deletes all session variables which belong to a user across all sessions."
  ([src session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if-some [user-id (.user-id ^Session smap)]
       (p/del-uvars (.control ^Session smap) user-id)
       (log/err "Cannot delete session variables because user ID is not valid"
                (log/for-user nil (user-email smap))))))
  ([src]
   (del-user-vars! src :session)))

(defn del-session-vars!
  "Deletes all session variables which belong to a user."
  ([src session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if-some [db-sid (db-sid-smap ^Session smap)]
       (p/del-svars (.control ^Session smap) db-sid)
       (log/err "Cannot delete session variables because session ID is not valid"
                (log/for-user (user-id smap) (user-email smap))))))
  ([src]
   (del-session-vars! src :session)))

(defn get-var
  "Gets a session variable and de-serializes it into a Clojure data structure."
  ([src var-name]
   (get-var src :session var-name))
  ([src session-key var-name]
   (if var-name
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot get session variable" var-name "because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (p/get-var (.control ^Session smap) db-sid var-name))))))

(defn get-vars
  "Gets a session variables and de-serializes them into a Clojure data structures."
  ([src var-names]
   (get-var src :session var-names))
  ([src session-key var-names]
   (if (not-empty var-names)
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot get session variable" (first var-names)
                  "because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (p/get-vars (.control ^Session smap) db-sid var-names))))))

(defn fetch-var!
  "Like `get-var` but removes session variable after it is successfully read from a
  database. Variable is not removed if there was a problem with reading or
  de-serializing it."
  ([src var-name]
   (fetch-var! src :session var-name))
  ([src session-key var-name]
   (if var-name
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot get session variable" var-name "because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (let [^SessionControl ctrl (.control ^Session smap)
               r                    (p/get-var ctrl db-sid var-name)]
           (if (not= ::db/get-failed r) (p/del-var ctrl db-sid var-name))
           r))))))

(defn fetch-vars!
  "Like `get-vars` but removes session variable after it is successfully read from a
  database. Variables are not removed if there was a problem with reading or
  de-serializing them."
  ([src var-names]
   (fetch-var! src :session var-names))
  ([src session-key var-names]
   (if (not-empty var-names)
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot get session variable" (first var-names)
                  "because session ID is not valid"
                  (log/for-user (user-id smap) (user-email smap)))
         (let [^SessionControl ctrl (.control ^Session smap)
               r                    (p/get-vars ctrl db-sid var-names)]
           (if (not= ::db/get-failed r) (p/del-vars ctrl db-sid var-names))
           r))))))

(defn put-var!
  "Puts a session variable `var-name` with a value `value` into a database."
  ([src var-name value]
   (put-var! src :session var-name value))
  ([src session-key var-name value]
   (if var-name
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot store session variable" var-name "because session ID is not valid")
         (p/put-var (.control ^Session smap) db-sid var-name value))))))

(defn put-vars!
  "Puts session variables with associated values (`var-names-values`) expressed as
  pairs into a database."
  ([src pairs]
   (put-var! src :session pairs))
  ([src session-key pairs]
   (if (not-empty pairs)
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot store session variable"
                  (let [fp (first pairs)] (if (coll? fp) (first fp) fp))
                  "because session ID is not valid")
         (p/put-vars (.control ^Session smap) db-sid pairs))))))

(defn get-variable-failed?
  "Returns `true` if the value `v` obtained from a session variable indicates that it
  actually could not be successfully fetched from a database."
  [v]
  (= ::db/get-failed v))

;; Cache invalidation.

(defn invalidate-cache!
  "Invalidates cache for the specific session."
  ([src]
   (invalidate-cache! src :session nil))
  ([src session-key]
   (invalidate-cache! src session-key nil))
  ([src session-key remote-ip]
   (if-some [^Session s (p/session src session-key)]
     (p/invalidate (.control  ^Session smap)
                   (p/identify ^Session smap)
                   (or remote-ip (.ip ^Session smap))))))

;; Cache invalidation when time-sensitive value (last active time) exceeds TTL.

(defn refresh-times
  "If the time left before expiry is smaller than the cache TTL then the session map
  will be updated using a database query."
  {:arglists '([src]
               [src session-key]
               [src remote-ip]
               [src session-key remote-ip]
               [src session-key ctrl remote-ip]
               [src session-key ctrl cache-expires remote-ip])}
  (^Session [^Sessionable src]
   (refresh-times src :session nil nil nil))
  (^Session [^Sessionable src session-key-or-ip]
   (if (session? src)
     (refresh-times src nil nil nil session-key-or-ip)
     (refresh-times src session-key-or-ip nil nil nil)))
  (^Session [^Sessionable src session-key remote-ip]
   (refresh-times src session-key nil nil remote-ip))
  (^Session [^Sessionable src session-key ^SessionControl ctrl remote-ip]
   (refresh-times src session-key ctrl nil remote-ip))
  (^Session [^Sessionable src session-key ^SessionControl ctrl cache-expires remote-ip]
   (if-some [^Session smap (p/session src session-key)]
     (let [^SessionControl ctrl (or ctrl (.control ^Session smap))
           cache-expires        (or cache-expires
                                    (.cache-expires ^SessionConfig (p/config ^SessionControl ctrl)))]
       (or (if cache-expires
             (if-some [last-active (.active ^Session smap)]
               (let [inactive-for (t/between last-active (t/now))]
                 (if (t/> inactive-for cache-expires)
                   (let [remote-ip (or remote-ip (.ip ^Session smap))]
                     (if (pos-int? (p/set-active ^SessionControl ctrl (db-sid-smap smap) remote-ip))
                       (map/qassoc smap :active last-active))
                     (p/invalidate ^SessionControl ctrl (or (.id ^Session smap)
                                                            (.err-id ^Session smap))
                                   remote-ip))))))
           smap)))))

;; Session handling, creation and prolongation

(defn handler
  "Processes session information by taking configuration options, session ID string,
  remote IP, request map and configuration options. It tries to get session ID string
  from form parameters of the request map and if the string is valid obtains session
  from a database using `getter-fn` (passing configuration options, database
  connection, table, session ID and remote IP to the call). Control field will not be
  set and it is the responsibility of a caller to update it afterwards when the
  session control object (which implements `SessionControl`) was not passed as an
  argument."
  (^Session [src sid remote-ip]
   (handler src :session sid remote-ip))
  (^Session [src session-key sid remote-ip]
   (if-some [^SessionControl (p/control src session-key)]
     (handler (p/config ctrl) #(p/from-db ctrl %1 %2) #(p/token-ok? ctrl %1 %2) sid remote-ip)))
  (^Session [^SessionConfig opts getter-fn checker-fn sid remote-ip]
   (let [[sid-db pass] (split-secure-sid sid)
         secure?       (some? (not-empty pass))
         smap-db       (getter-fn sid-db remote-ip)
         token-ok?     (checker-fn pass (get smap-db :secure-token))
         ^Session smap (map->Session (dissoc smap-db :secure-token))
         smap          (if secure? (map/qassoc smap :security-passed? token-ok?) smap)
         smap          (map/qassoc
                        smap
                        :id          sid
                        :db-id       sid-db
                        :ip          (ip/to-address (.ip ^Session smap))
                        :secure?     secure?
                        :session-key (or (.session-key ^SessionConfig opts) :session)
                        :id-field    (or (.id-field    ^SessionConfig opts) "session-id"))
         stat          (state smap remote-ip)]
     (if (get stat :cause)
       (mkbad smap :error stat)
       (mkgood smap)))))

(defn process
  "Takes a session control object and a request map, and validates session against a
  database or memoized session data. Returns a session map."
  ([^SessionControl ctrl ^clojure.lang.Associative req]
   (if-some [^String sid (p/identify ^SessionControl ctrl req)]
     (let [^SessionConfig opts (p/config ^SessionControl ctrl)]
       (if-not (sid-valid? sid)
         (mkbad (Session. sid nil nil nil nil nil nil nil nil
                          false false false false false
                          (or (.session-key      ^SessionConfig opts) :session)
                          (or (.session-id-field ^SessionConfig opts) "session-id")
                          {:reason   "Malformed session-id parameter"
                           :cause    :session/malformed-session-id
                           :severity :info}
                          ctrl))
         (let [remote-ip     (get req :remote-ip)
               ^Session smap (p/handle ^SessionControl ctrl sid remote-ip)]
           (if-not (valid? smap)
             smap
             (let [cache-expires (.cache-expires ^SessionConfig opts)
                   ^Session smap (if cache-expires
                                   (refresh-times ^Session smap nil ^SessionControl ctrl remote-ip)
                                   smap)]
               (if (and cache-expires (not (valid? smap)))
                 smap
                 (if (pos-int? (p/set-active ^SessionControl ctrl (db-sid-smap smap) remote-ip))
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
                                                  (get req :remote-ip/str))))}))))))))
     (Session. nil nil nil nil nil nil nil nil nil
               false false false false false
               (or (.session-key      ^SessionConfig opts) :session)
               (or (.session-id-field ^SessionConfig opts) "session-id")
               nil ctrl))))

(defn prolong
  "Re-validates session by updating its timestamp and re-running validation."
  {:arglists '([req]
               [req session-key]
               [smap ip-address]
               [opts handler-fn update-active-fn invalidator-fn smap ip-address])}
  ([req-or-smap]
   (if (session? req-or-smap)
     (prolong req-or-smap )
     (prolong req-or-smap :session)))
  ([req-or-smap session-key-or-ip]
   (if (session? req-or-smap)
     (if-some [prolonger (get (.config ^Session req-or-smap) :fn/prolong)]
       (prolonger req-or-smap session-key-or-ip))
     (if-some [^Session smap (p/session req-or-smap session-key-or-ip)]
       (if-some [prolonger (get (.config ^Session smap) :fn/prolong)]
         (prolonger smap (get req-or-smap :ip-address))))))
  ([opts handler-fn update-active-fn invalidator-fn smap ip-address]
   (if-some [^Session smap (p/session smap)]
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
                 (map/qassoc (handler-fn sid ip-address) :config opts :prolonged? true))
             (do (log/wrn "Session re-validation error" (log/for-user (:user-id smap) (:user-email smap) ipplain))
                 (mkbad smap :error stat)))))))))

(defn create
  "Creates a new session and puts it into a database. Returns a session record."
  (^Session [src user-id user-email ip-address]
   (let [^SessionControl ctrl (p/control src)
         user-id              (valuable user-id)
         user-email           (some-str user-email)
         ids?                 (and user-id user-email)]
     (cond

       (not ctrl) (do (log/err "Session control object was not found.") nil)
       (not ids?) (do (if-not user-id    (log/err "No user ID given when creating a session"))
                      (if-not user-email (log/err "No user e-mail given when creating a session"))
                      nil)
       :ok
       (let [t                   (t/now)
             ip                  (ip/to-address ip-address)
             ipplain             (ip/plain-ip-str ip)
             ^SessionConfig opts (p/config ^SessionControl ctrl)
             single-session?     (.single-session  ^SessionConfig opts)
             secured?            (.secured?        ^SessionConfig opts)
             id-field            (or (.id-field    ^SessionConfig opts) "session-id")
             skey                (or (.session-key ^SessionConfig opts) :session)
             ^Session sess       (Session. nil nil nil nil user-id user-email t t ip
                                           false false false false false skey id-field ctrl)
             sess                (gen-session-id sess secured? user-id ipplain)
             stat                (state sess ip)]
         (log/msg "Opening session" (log/for-user user-id user-email ipplain))
         (if-not (correct? (get stat :cause))
           (do (log/err "Session incorrect after creation" (log/for-user user-id user-email ipplain))
               (mkbad sess :error stat))
           (let [updated-count (p/to-db ^SessionControl ctrl ^Session sess)
                 sess          (map/qassoc sess :db-token nil)]
             (p/invalidate ^SessionControl ctrl (p/identify ^Session sess) ip)
             (if (pos-int? updated-count)
               (do (if single-session?
                     (p/del-uvars ^SessionControl ctrl user-id)
                     (p/del-svars ^SessionControl ctrl (db-sid-smap sess)))
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
  (let [id-path (if (coll? id-path) id-path [:params id-path])]
    (identify-session-path-compile id-path)))

(defn wrap-session
  "Session maintaining middleware."
  [k config]
  (let [dbname             (db/db-name (get config :db))
        config             (-> config
                               (update :db               db/ds)
                               (update :sessions-table   #(or (to-snake-simple-str %) "sessions"))
                               (update :variables-table  #(or (to-snake-simple-str %) "session_variables"))
                               (update :expires          time/parse-duration)
                               (update :hard-expires     time/parse-duration)
                               (update :cache-ttl        time/parse-duration)
                               (update :cache-size       safe-parse-long)
                               (update :token-cache-ttl  time/parse-duration)
                               (update :token-cache-size safe-parse-long)
                               (update :session-key      #(or (some-keyword %) :session))
                               (update :id-path          #(if (valuable? %) (if (coll? %) (vec %) [:params %]) [:params %]))
                               (update :id-field         #(if (ident? %) % (some-str %)))
                               (update :single-session?  boolean)
                               (update :secured?         boolean)
                               (calc-cache-expires)
                               (map->SessionConfig))
        db                 (get config :db)
        session-key        (get config :session-key)
        sessions-table     (get config :sessions-table)
        variables-table    (get config :variables-table)
        session-id-path    (get config :id-path)
        session-id-field   (get config :id-field)
        cache-expires      (get config :cache-expires)
        single-session?    (get config :single-session?)
        secured?           (get config :secured?)
        session-id-field   (or session-id-field (if (coll? session-id-path) (last session-id-path) session-id-path))
        config             (assoc config :id-field (or session-id-field "session-id"))
        identifier-fn      (setup-id-fn session-id-path)
        config             (assoc config :fn/identifier identifier-fn)
        getter-fn          (setup-fn config :fn/getter get-session-by-id)
        getter-fn-w        #(getter-fn config db sessions-table %1 %2)
        config             (assoc config :fn/getter getter-fn-w)
        checker-config     (set/rename-keys config {:token-cache-size :cache-size :token-cache-ttl :cache-ttl})
        checker-fn         (setup-fn config :fn/checker check-encrypted)
        checker-fn-w       (db/memoizer checker-fn checker-config)
        config             (assoc config :fn/checker checker-fn-w)
        last-active-fn     (setup-fn config :fn/last-active get-last-active)
        update-active-fn   (setup-fn config :fn/update-active update-last-active)
        last-active-fn-w   #(last-active-fn config db sessions-table %1 %2)
        config             (assoc config :fn/last-active last-active-fn-w)
        update-active-fn-w (fn
                             (^Long [sid-db remote-ip]
                              (update-active-fn config db sessions-table sid-db remote-ip))
                             (^Long [sid-db remote-ip t]
                              (update-active-fn config db sessions-table sid-db remote-ip t)))
        config             (assoc config :fn/update-active update-active-fn-w)
        var-get-fn         (db/make-setting-getter  variables-table :session-id)
        var-put-fn         (db/make-setting-setter  variables-table :session-id)
        var-del-fn         (db/make-setting-deleter variables-table :session-id)
        vars-put-fn        #(apply var-put-fn %1 %2 %3)
        vars-get-fn        #(apply var-get-fn %1 %2 %3)
        vars-del-fn        #(apply var-del-fn %1 %2 %3)
        vars-del-user-fn   (setup-fn config :fn/del-user-vars delete-user-vars)
        vars-del-sess-fn   (setup-fn config :fn/del-sess-vars delete-session-vars)
        vars-del-user-fn-w #(vars-del-user-fn config db sessions-table variables-table %)
        vars-del-sess-fn-w #(vars-del-sess-fn config db sessions-table variables-table %)
        config             (assoc config
                                  :fn/get-var       var-get-fn
                                  :fn/get-vars      vars-get-fn
                                  :fn/put-var       var-put-fn
                                  :fn/put-vars      vars-put-fn
                                  :fn/del-var       var-del-fn
                                  :fn/del-vars      vars-del-fn
                                  :fn/del-user-vars vars-del-user-fn-w
                                  :fn/del-sess-vars vars-del-sess-fn-w)
        setter-fn          (setup-fn config :fn/setter set-session)
        setter-fn-w        #(setter-fn config db sessions-table %)
        config             (assoc config :fn/setter setter-fn-w)
        pre-handler        #(handler config getter-fn-w checker-fn-w %1 %2)
        mem-handler        (db/memoizer pre-handler config)
        handler-fn-w       #(if-some [^Session s (mem-handler %2 %3)] (qassoc s :control %1))
        invalidator-fn     (setup-invalidator pre-handler mem-handler)
        config             (assoc config :fn/invalidator invalidator-fn :fn/handler mem-handler)
        prolong-fn         #(prolong config mem-handler update-active-fn-w invalidator-fn %1 %2)
        config             (assoc config :fn/prolong prolong-fn)
        ^SessionControl control (reify p/SessionControl
                                  (config        ^SessionConfig [_] config)
                                  (expired?      ^Boolean [_ t]         (expirer-fn t))
                                  (hard-expired? ^Boolean [_ t]         (expirer-hard-fn t))
                                  (token-ok?     ^Boolean [_ plain enc] (checker-fn-w plain enc))
                                  (from-db       ^Session [_ db-sid ip] (getter-fn-w db-sid ip))
                                  (handle        ^Session [c sid ip]    (handler-fn-w c sid ip))
                                  (to-db         ^Long    [_ smap]      (setter-fn-w smap))
                                  (set-active    ^Long    [_ db-sid ip] (update-active-fn-w db-sid ip))
                                  (get-active    ^Instant [_ db-sid ip] (last-active-fn-w db-sid ip))
                                  (identify      [_ req]        (identifier-fn req))
                                  (invalidate    [c sid ip]     (invalidator-fn c sid ip))
                                  (put-var       [_ db-sid k v] (var-put-fn  db db-sid k v))
                                  (get-var       [_ db-sid k]   (var-get-fn  db db-sid k))
                                  (del-var       [_ db-sid k]   (var-del-fn  db db-sid k))
                                  (put-vars      [_ db-sid kvs] (vars-put-fn db db-sid kvs))
                                  (get-vars      [_ db-sid ks]  (vars-get-fn db db-sid ks))
                                  (del-vars      [_ db-sid ks]  (var-del-fn  db db-sid ks))
                                  (del-svars     [_ db-sid]     (vars-del-sess-fn-w db-sid))
                                  (del-uvars     [_ user-id]    (vars-del-user-fn-w user-id)))]
    (log/msg "Installing session handler:" k)
    (if dbname (log/msg "Using database" dbname "for storing sessions"))
    {:name    (keyword k)
     :config  config
     :compile (fn [{:keys [no-session?]} opts]
                (if (and (not no-session?) db)
                  (fn [h]
                    (fn [req]
                      (h
                       (map/qassoc req session-key (delay (process control req))))))))}))

(system/add-init  ::default [k config] (wrap-session k config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
