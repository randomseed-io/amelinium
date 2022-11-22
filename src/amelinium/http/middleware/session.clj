(ns

    ^{:doc    "amelinium service, session middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.session

  (:refer-clojure :exclude [parse-long uuid random-uuid empty empty?])

  (:require [clojure.set                  :as        set]
            [clojure.string               :as        str]
            [clojure.core.memoize         :as        mem]
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
            [amelinium.types.session      :refer    :all]
            [io.randomseed.utils          :refer    :all]
            [io.randomseed.utils.time     :as       time]
            [io.randomseed.utils.var      :as        var]
            [io.randomseed.utils.map      :as        map]
            [io.randomseed.utils.ip       :as         ip]
            [io.randomseed.utils.db.types :as      types])

  (:import [java.time Instant Duration]
           [javax.sql DataSource]
           [inet.ipaddr IPAddress]
           [clojure.core.memoize PluggableMemoization]
           [clojure.core.cache TTLCacheQ]
           [amelinium.proto.session SessionControl Sessionable]
           [amelinium.types.session Session SessionConfig SessionError]))

(set! *warn-on-reflection* true)

(def ^:const sid-match (re-pattern "|^[a-f0-9]{30,128}(-[a-f0-9]{30,128})?$"))

(def one-second (t/new-duration 1 :seconds))

(defn config?
  ^Boolean [v]
  (instance? SessionConfig v))

(defn session?
  ^Boolean [v]
  (instance? Session v))

(extend-protocol p/Sessionable

  Session

  (session
    (^Session [src] src)
    (^Session [src _] src))

  (inject
    ([smap dst session-key] (map/qassoc (or session-key (.session-key ^Session smap) :session) smap))
    ([smap dst] (map/qassoc (or (.session-key ^Session smap) :session) smap)))

  (empty?
    (^Boolean [smap]
     (and (nil? (.id     ^Session smap))
          (nil? (.err-id ^Session smap))
          (nil? (.error  ^Session smap))))
    (^Boolean [smap _]
     (and (nil? (.id     ^Session smap))
          (nil? (.err-id ^Session smap))
          (nil? (.error  ^Session smap)))))

  (control
    (^SessionControl [smap]   (.control ^Session smap))
    (^SessionControl [smap _] (.control ^Session smap)))

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

  (empty
    (^Session [s]             (p/empty (.control ^Session s)))
    (^Session [s _]           (p/empty (.control ^Session s))))

  (to-db
    (^Long    [s]             (p/to-db (.control ^Session s) s))
    (^Long    [s _]           (p/to-db (.control ^Session s) s)))

  (identify
    (^String [s]              (or (.id ^Session s) (.err-id ^Session s)))
    (^String [s req]          (or (.id ^Session s) (.err-id ^Session s)
                                  (p/identify (.control ^Session s) req))))

  (from-db
    (^Session [s]             (p/from-db (.control ^Session s) (db-sid-smap s) (.ip ^Session s)))
    (^Session [s db-sid]      (p/from-db (.control ^Session s) db-sid (.ip ^Session s)))
    (^Session [s db-sid ip]   (p/from-db (.control ^Session s) db-sid ip)))

  (handle
    (^Session [s]             (p/handle (.control ^Session s) (p/identify ^Session s) (.ip ^Session s)))
    (^Session [s sid]         (p/handle (.control ^Session s) sid (.ip ^Session s)))
    (^Session [s sid ip]      (p/handle (.control ^Session s) sid ip)))

  (mem-handler
    ([s]                      (p/mem-handler (.control ^Session s)))
    ([s _]                    (p/mem-handler (.control ^Session s))))

  (invalidate
    ([s]                      (p/invalidate (.control ^Session s) (p/identify ^Session s) (.ip ^Session s)))
    ([s sid]                  (p/invalidate (.control ^Session s) sid (.ip ^Session s)))
    ([s sid ip]               (p/invalidate (.control ^Session s) sid ip)))

  (get-active
    (^Instant [s]             (p/get-active (.control ^Session s) (p/identify ^Session s) (.ip ^Session s)))
    (^Instant [s db-sid]      (p/get-active (.control ^Session s) db-sid (.ip ^Session s)))
    (^Instant [s db-sid ip]   (p/get-active (.control ^Session s) db-sid ip)))

  (set-active
    (^Long [s]                 (p/set-active (.control ^Session s) (p/identify s) (db-sid-smap s) (.ip ^Session s)))
    (^Long [s ip]              (p/set-active (.control ^Session s) (p/identify s) (db-sid-smap s) ip))
    (^Long [s ip t]            (p/set-active (.control ^Session s) (p/identify s) (db-sid-smap s) ip t))
    (^Long [s sid db-sid ip]   (p/set-active (.control ^Session s) sid db-sid ip))
    (^Long [s sid db-sid ip t] (p/set-active (.control ^Session s) sid db-sid ip t)))

  (expired?      ^Boolean [s]     (p/expired?      (.control ^Session s) (.active ^Session s)))
  (hard-expired? ^Boolean [s]     (p/hard-expired? (.control ^Session s) (.active ^Session s)))
  (token-ok?     ^Boolean [s p e] (p/token-ok?     (.control ^Session s) p e))

  clojure.lang.Associative

  (config
    (^SessionConfig [req]             (p/config (p/session req)))
    (^SessionConfig [req session-key] (p/config (p/session req session-key))))

  (empty
    (^Session [req]                   (p/empty (p/session req)))
    (^Session [req session-key]       (p/empty (p/session req session-key))))

  (mem-handler
    ([req]                            (p/mem-handler (p/session req)))
    ([req session-key]                (p/mem-handler (p/session req session-key))))

  (identify
    (^String [req]
     (or (p/identify (p/session req))
         (some-str (or (get-in req [:params :session-id])
                       (get-in req [:params "session-id"])))))
    (^String [req session-key-or-req-path]
     (if (coll? session-key-or-req-path)
       (some-str (get-in req session-key-or-req-path))
       (p/identify (p/session req session-key-or-req-path)))))

  nil

  (^Boolean token-ok? [s e p] false)
  (^Boolean expired?      [s] false)
  (^Boolean hard-expired? [s] false)

  (to-db        [s]             nil)
  (mem-atom     [s]             nil)
  (mem-cache    [s]             nil)
  (mem-handler ([s]             nil) ([s s-k]       nil))
  (empty       ([s]             nil) ([s s-k]       nil))
  (config      ([s]             nil) ([s s-k]       nil))
  (identify    ([s]             nil) ([s req]       nil))
  (from-db     ([s db-sid ip]   nil) ([s db-sid]    nil) ([s]        nil))
  (handle      ([s db-sid ip]   nil) ([s db-sid]    nil) ([s]        nil))
  (invalidate  ([s db-sid ip]   nil) ([s db-sid]    nil) ([s]        nil))
  (get-active  ([s db-sid ip]   nil  ([s db-sid]    nil) ([s]        nil)))
  (get-var     ([s db-sid k]    nil) ([s k]         nil))
  (get-vars    ([s db-sid ks]   nil) ([s ks]        nil))
  (put-var     ([s db-sid k v]  nil) ([s k v]       nil))
  (put-vars    ([s db-sid kvs]  nil) ([s kvs]       nil))
  (del-var     ([s db-sid k]    nil) ([s k]         nil))
  (del-vars    ([s db-sid ks]   nil) ([s ks]        nil))
  (del-svars   ([s db-sid]      nil) ([s]           nil))
  (del-uvars   ([s uid]         nil) ([s]           nil))
  (set-active
    ([s sid db-sid ip t] nil)
    ([s sid db-sid ip] nil)
    ([s smap ip] nil)
    ([s smap] nil)
    ([s] nil))

  Object

  (empty [s] (clojure.core/empty s)))

(defn of
  "Returns a session record of type `Session` on a basis of configuration source
  provided and an optional `session-key` if session must be looked in an associative
  structure (defaults to `:session`)."
  (^Session [src] (p/session src))
  (^Session [src session-key] (p/session src session-key)))

(defn empty?
  "Returns `false` is `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` may be given to express a key in associative
  structure (defaults to `:session`)."
  (^Boolean [src] (p/empty? src))
  (^Boolean [src session-key] (p/empty? src session-key)))

(defn not-empty?
  "Returns `true` is `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` may be given to express a key in associative
  structure (defaults to `:session`)."
  (^Boolean [src] (not (p/empty? src)))
  (^Boolean [src session-key] (not (p/empty? src session-key))))

(defn empty
  "Returns an empty session record. Optional `session-key` may be given to express a
  key in associative structure (defaults to `:session`) used to perform a session
  lookup used to access the control object."
  (^Boolean [src] (p/empty src))
  (^Boolean [src session-key] (p/empty src session-key)))

(defn inject
  "Returns an object updated with session record of type `Session` under an optional
  `session-key` if session is to be put into an associative structure (defaults to
  `:session`)."
  ([dst smap] (p/inject dst smap))
  ([dst smap session-key] (p/inject dst smap session-key)))

(defn not-empty-of
  "Returns a session if `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` may be given to express a key in associative
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
  (satisfies? p/SessionControl v))

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

(defn mem-ctime
  "Retrieves an entry creation time (in milliseconds) associated with the given `key`
  in a TTL map of a current handler cache. If the entry does not exist, `nil` is
  returned."
  [^SessionControl ctrl key]
  (if-some [^PluggableMemoization m (p/mem-cache ctrl)]
    (nth (get (.ttl ^TTLCacheQ (.cache m)) key) 1 nil)))

(defn mem-etime
  "Retrieves an expiration time (in milliseconds since the beginning of the Unix epoch)
  of an entry identified by the given key `key` in a TTL map of a current handler
  cache. It is calculated by adding cache's TTL to entry's creation time. If the
  entry does not exist, `nil` is returned."
  [^SessionControl ctrl key]
  (if-some [^PluggableMemoization m (p/mem-cache ctrl)]
    (let [^TTLCacheQ c (.cache m)]
      (if-some [ctime (nth (get (.ttl c) key) 1 nil)]
        (+ ctime (.ttl-ms c))))))

(defn mem-cache-expired?
  "Returns `true` if an entry associated with the given key `key` in a TTL map of a
  current handler cache has expired. If the entry does not exist, `true` is
  returned. Optional argument `t` may be given with a time expressed as an
  instant (`java.time.Instant`) to be used as a source of current time."
  ([^SessionControl ctrl key t]
   (if-some [etime (mem-etime ctrl key)]
     (> (time/timestamp t) etime)
     true))
  ([^SessionControl ctrl key]
   (if-some [etime (mem-etime ctrl key)]
     (> (java.lang.System/currentTimeMillis) etime)
     true)))

(defn mem-cache-almost-expired?
  "Returns `true` if an entry associated with the given key `key` in a TTL map of a
  current handler cache has almost expired. The `knee` argument should be a number of
  milliseconds (defaults to 1000 if not given) to be added to a current time in order
  to shift it forward. If the entry does not exist, `true` is returned. Negative
  `knee` values will cause it to have 0 impact. Optional argument `t` may be given
  with a time expressed as an instant (`java.time.Instant`) to be used as a source of
  current time."
  ([^SessionControl ctrl key]
   (mem-cache-almost-expired? ctrl key 1000))
  ([^SessionControl ctrl key knee]
   (if-some [etime (mem-etime ctrl key)]
     (let [knee (if (and knee (not (neg? knee))) knee 0)]
       (> (+ (java.lang.System/currentTimeMillis) knee) etime))
     true))
  ([^SessionControl ctrl key knee t]
   (if-some [etime (mem-etime ctrl key)]
     (let [knee (if (and knee (not (neg? knee))) knee 0)]
       (> (+ (time/timestamp t) knee) etime))
     true)))

(defn mem-cache-time-left
  "Returns a time left to a cache expiry for an entry associated with the given key
  `key` in a TTL map of a current handler cache. The returned object is of type
  `java.time.Duration`. If the entry does not exist, `nil` is returned. Optional `t`
  argument may be given to express current time as `java.time.Instant`."
  ([^SessionControl ctrl key]
   (if-some [etime (mem-etime ctrl key)]
     (let [tl (- etime (java.lang.System/currentTimeMillis))]
       (t/new-duration (if (pos? tl) tl 0) :millis))))
  ([^SessionControl ctrl key t]
   (if-some [etime (mem-etime ctrl key)]
     (let [tl (- etime (time/timestamp t))]
       (t/new-duration (if (pos? tl) tl 0) :millis)))))

(defn mem-cache-time-passed
  "Returns a time which passed till now from the creation of an entry associated with
  the given key `key` in a TTL map of a current handler cache. The returned object is
  of type `java.time.Duration` and its value may exceed the configured TTL when there
  was no cache update nor eviction for the entry. Optional `t` argument may be
  given to express current time as `java.time.Instant`."
  ([^SessionControl ctrl key]
   (if-some [ctime (mem-ctime ctrl key)]
     (let [tp (- (java.lang.System/currentTimeMillis) ctime)]
       (t/new-duration (if (pos? tp) tp 0) :millis))))
  ([^SessionControl ctrl key t]
   (if-some [ctime (mem-ctime ctrl key)]
     (let [tp (- (time/timestamp t) ctime)]
       (t/new-duration (if (pos? tp) tp 0) :millis)))))

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
           (SessionError. :warn :session/bad-ip
                          (str-spc "Session IP address" (str "(" (ip/plain-ip-str session-ip) ")")
                                   "is different than the remote IP address"
                                   (str "(" (ip/plain-ip-str remote-ip) ")")
                                   (log/for-user user-id user-email))))
         (if-some [str-addr (ip/to-str remote-ip)]
           (if-not (or (= str-addr (ip/to-str session-ip))
                       (= str-addr (ip/to-str (ip/to-v4 session-ip)))
                       (= str-addr (ip/to-str (ip/to-v6 session-ip))))
             (SessionError. :warn :session/bad-ip
                            (str-spc "Session IP string" (str "(" (ip/to-str remote-ip) ")")
                                     "is different than the remote IP string"
                                     (str "(" str-addr ")")
                                     (log/for-user user-id user-email))))))))))

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

(defn calc-expired-core
  (^Boolean [exp last-active]
   (time-exceeded? last-active (t/now) exp)))

(defn calc-expired?
  (^Boolean [src session-key]
   (calc-expired? (p/session src session-key)))
  (^Boolean [src]
   (if-some [^Session smap (p/session src)]
     (p/expired? ^Session smap))))

(defn calc-hard-expired?
  (^Boolean [src session-key]
   (calc-hard-expired? (p/session src session-key)))
  (^Boolean [src]
   (if-some [^Session smap (p/session src)]
     (p/hard-expired? ^Session smap))))

(defn calc-soft-expired?
  (^Boolean [src session-key]
   (calc-soft-expired? (p/session src session-key)))
  (^Boolean [src]
   (if-some [^Session smap (p/session src)]
     (and (p/expired? ^Session smap)
          (not (p/hard-expired? ^Session smap))))))

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
  "Returns session state. If there is anything wrong, returns a `SessionError`
  record. Otherwise it returns `nil`. Unknown session detection is performed by
  checking if a value associated with the `:id` key is `nil` and a value associated
  with the `:err-id` key is not `nil`. First argument must be a session object
  of type `Session`."
  [^Session smap ip-address]
  (if-not (session? smap)
    (SessionError. :info :session/missing (str-spc "No session:" smap))
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
        (not any-sid)               (SessionError. :info :session/no-id
                                                   (some-str-spc "No session ID" @for-user))
        (not (sid-valid? any-sid))  (SessionError. :info :session/malformed-session-id
                                                   (str "Malformed session ID " @for-user))
        (not user-ident)            (SessionError. :info :session/unknown-id
                                                   (some-str-spc "Unknown session ID" any-sid @for-user))
        (not sid)                   (SessionError. :info :session/unknown-id
                                                   (some-str-spc "Unknown session ID" esid @for-user))
        (not user-id)               (SessionError. :info :session/malformed-user-id
                                                   (str "User ID not found or malformed " @for-user))
        (not user-email)            (SessionError. :info :session/malformed-user-email
                                                   (str "User e-mail not found or malformed " @for-user))
        (not (created-valid? smap)) (SessionError. :warn :session/bad-creation-time
                                                   (str "No creation time " @for-user))
        (not (active-valid? smap))  (SessionError. :warn :session/bad-last-active-time
                                                   (str "No last active time " @for-user))
        (p/expired? smap)           (SessionError. :info :session/expired
                                                   (str "Session expired " @for-user))
        (insecure? smap)            (SessionError. :warn :session/insecure
                                                   (str "Session not secured with encrypted token " @for-user))
        (security-failed? smap)     (SessionError. :warn :session/bad-security-token
                                                   (str "Bad session security token " @for-user))
        :ip-address-check           (ip-state smap user-id user-email ip-address)))))

(defn correct-state?
  "Returns `true` if a session exists and its state is correct. Never throws an
  exception."
  (^Boolean [state-result]
   (not (instance? SessionError state-result)))
  (^Boolean [src ip-address]
   (not (instance? SessionError (state (try (p/session src) (catch Throwable _ nil)) ip-address))))
  (^Boolean [src session-key ip-address]
   (not (instance? SessionError (state (try (p/session src session-key) (catch Throwable _ nil)) ip-address)))))

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
      2 (fn ^String [req] (some-str (get (get req a) b)))
      3 (fn ^String [req] (some-str (get (get (get req a) b) c)))
      4 (fn ^String [req] (some-str (get (get (get (get req a) b) c) d)))
      (fn   ^String [req] (some-str (get-in req path))))))

;; SQL defaults

(defn get-session-by-id
  "Standard session getter. Uses `db` to connect to a database and gets data identified
  by `sid` from a table `table`. Returns a map."
  [^SessionConfig opts ^DataSource db table sid-db remote-ip]
  (sql/get-by-id db table sid-db db/opts-simple-map))

(defn get-last-active
  ^Instant [^SessionConfig opts ^DataSource db table sid-db remote-ip]
  (first (jdbc/execute-one! db
                            [(str "SELECT active FROM " table " WHERE id = ?") sid-db]
                            db/opts-simple-vec)))

(defn update-last-active
  (^Long [^SessionConfig opts ^DataSource db table sid-db remote-ip]
   (::jdbc/update-count
    (sql/update! db table {:active (t/now)} {:id sid-db} db/opts-simple-map)))
  (^Long [^SessionConfig opts ^DataSource db table sid-db remote-ip t]
   (::jdbc/update-count
    (sql/update! db table {:active (t/instant t)} {:id sid-db} db/opts-simple-map))))

(defn set-session
  ^Long [^SessionConfig opts ^DataSource db table smap]
  (::jdbc/update-count
   (db/replace! db table
                (-> smap
                    (set/rename-keys {:db-id :id :db-token :secure-token})
                    (select-keys [:user-id :user-email :secure-token :id :ip :active :created]))
                db/opts-simple-map)))

(defn delete-user-vars
  [^SessionConfig opts ^DataSource db sessions-table variables-table user-id]
  (jdbc/execute-one! db [(str-spc "DELETE FROM" variables-table
                                  "WHERE EXISTS (SELECT 1 FROM" sessions-table
                                  (str "WHERE " sessions-table ".user_id = ?")
                                  (str "AND " variables-table ".session_id = " sessions-table ".id)"))
                         user-id]))

(defn delete-session-vars
  [^SessionConfig opts ^DataSource db sessions-table variables-table db-id]
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
  ([src session-key]
   (if-some [^Session smap (p/session src session-key)]
     (-> (if (.id smap) smap (map/qassoc smap :id (.err-id smap)))
         (map/qassoc :valid?        true
                     :expired?      false
                     :hard-expired? false
                     :err-id        nil
                     :error         nil))))
  ([src]
   (if-some [^Session smap (p/session src)]
     (-> (if (.id smap) smap (map/qassoc smap :id (.err-id smap)))
         (map/qassoc :valid?        true
                     :expired?      false
                     :hard-expired? false
                     :err-id        nil
                     :error         nil)))))

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
     (let [^SessionError e (.error ^Session smap)
           have-error?     (instance? SessionError e)
           id              (.id ^SessionError e)
           id?             (some? id)
           expired?        (and id?
                                (or (= :session/expired id)
                                    (and (= :session/bad-ip id)
                                         (if-some [^SessionConfig cfg (p/config smap)]
                                           (.bad-ip-expires? cfg)))))
           h-expired?      (and expired? (p/hard-expired? ^Session smap))
           err-id          (or (.id ^Session smap) (.err-id ^Session smap))
           ^SessionError e (if have-error?
                             (let [cause?           (some? (.cause ^SessionError e))
                                   ^SessionError er (if (some? (.severity ^SessionError e)) e
                                                        (map/qassoc e :severity :warn))]
                               (if id?
                                 (if cause?
                                   er
                                   (if (= id :session/unknown-error)
                                     (map/qassoc er :cause "Unknown session error")
                                     er))
                                 (if cause?
                                   (map/qassoc er :id :session/unknown-error)
                                   (map/qassoc er :id :session/unknown-error :cause "Unknown session error"))))
                             (SessionError. :warn :session/unknown-error "Unknown session error"))]
       (map/qassoc smap
                   :error         e
                   :valid?        false
                   :err-id        err-id
                   :expired?      expired?
                   :hard-expired? h-expired?
                   :id            nil)))))

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
  ([^Sessionable src]
   (invalidate-cache! src :session nil))
  ([^Sessionable src session-key]
   (invalidate-cache! src session-key nil))
  ([^Sessionable src session-key remote-ip]
   (if-some [^Session smap (p/session src session-key)]
     (p/invalidate (.control  ^Session smap)
                   (p/identify ^Session smap)
                   (or remote-ip (.ip ^Session smap))))))

;; Cache invalidation when time-sensitive value (last active time) exceeds TTL.

(defn- refresh-times-core
  [^Session smap ^SessionControl ctrl cache-margin remote-ip]
  (or (if cache-margin
        (if-some [last-active (.active ^Session smap)]
          (let [inactive-for (t/between last-active (t/now))]
            (when (t/> inactive-for cache-margin)
              (let [fresh-active  (p/get-active ^SessionControl ctrl (db-sid-smap smap) remote-ip)
                    ^Session smap (if fresh-active (map/qassoc smap :active fresh-active) smap)
                    expired?      (p/expired? ^SessionControl ctrl (or fresh-active last-active))
                    sid           (or (.id ^Session smap) (.err-id ^Session smap))]
                (if (and fresh-active (not= fresh-active last-active))
                  (db/mem-assoc-existing! (p/mem-handler ^SessionControl ctrl) [sid remote-ip] :active fresh-active))
                (if expired?
                  (mkbad smap :error (state smap remote-ip))
                  smap))))))
      smap))

(defn refresh-times
  "Checks a last-active time of the session. If the time left to expiration is smaller
  than the cache TTL then the session record will be updated using a database query
  and session cache invalidated for a session ID. Uses pre-calculated value stored in
  the `:cache-margin` field of a session record (see `calc-cache-margin` for more
  info)."
  {:arglists '([^Sessionable src]
               [^Sessionable src session-key]
               [^Sessionable src remote-ip]
               [^Sessionable src session-key remote-ip])}
  (^Session [^Sessionable src]
   (if-some [^Session smap (p/session src)]
     (refresh-times-core smap
                         (.control       ^Session smap)
                         (if-some [^SessionConfig cfg (p/config smap)] (.cache-margin cfg))
                         (.ip            ^Session smap))))
  (^Session [^Sessionable src session-key-or-ip]
   (if (session? src)
     (refresh-times-core src
                         (.control       ^Session src)
                         (if-some [^SessionConfig cfg (p/config src)] (.cache-margin cfg))
                         session-key-or-ip)
     (if-some [^Session smap (p/session src session-key-or-ip)]
       (refresh-times-core smap
                           (.control       ^Session smap)
                           (if-some [^SessionConfig cfg (p/config smap)] (.cache-margin cfg))
                           (.ip            ^Session smap)))))
  (^Session [^Sessionable src session-key remote-ip]
   (if-some [^Session smap (p/session src session-key)]
     (refresh-times-core smap
                         (.control       ^Session smap)
                         (if-some [^SessionConfig cfg (p/config smap)] (.cache-margin cfg))
                         remote-ip))))

;; Session handling, creation and prolongation

(defn handler
  "Gets session data from a database and processes them with session control functions
  and configuration options to generate a session record. When the record is created
  it is validated for errors and expiration times.

  The results of calling this function may be memoized to reduce database hits. Note
  that if the cache TTL is larger than the remaining expiration time for a session,
  it may be required to refresh the times and re-validate the session object (see
  `refresh-times`)."
  (^Session [src sid remote-ip]
   (handler src :session sid remote-ip))
  (^Session [src session-key sid remote-ip]
   (if-some [^SessionControl ctrl (p/control src session-key)]
     (let [^SessionConfig opts (p/config ^SessionControl ctrl)]
       (handler ctrl (.session-key ^SessionConfig opts) (.id-field ^SessionConfig opts) sid remote-ip))))
  (^Session [^SessionControl ctrl session-key id-field sid remote-ip]
   (let [[sid-db pass] (split-secure-sid sid)
         secure?       (some? (not-empty pass))
         smap-db       (p/from-db   ^SessionControl ctrl sid-db remote-ip)
         token-ok?     (p/token-ok? ^SessionControl ctrl pass (get smap-db :secure-token))
         ^Session smap (map->Session (dissoc smap-db :secure-token))
         smap          (if secure? (map/qassoc smap :security-passed? token-ok?) smap)
         smap          (map/qassoc
                        smap
                        :id          sid
                        :db-id       sid-db
                        :ip          (ip/to-address (.ip ^Session smap))
                        :secure?     secure?
                        :control     ctrl
                        :session-key session-key
                        :id-field    id-field)
         stat          (state smap remote-ip)]
     (if (instance? SessionError stat)
       (mkbad  smap :error stat)
       (mkgood smap)))))

(defn- needs-refresh?
  [^SessionControl ctrl key cache-margin last-active expires-in expired?]
  (and (some? cache-margin) (some? last-active)
       (let [now (t/now)]
         (or (when (and (not expired?) (t/> (t/between last-active now) cache-margin))
               (log/dbg "Session margin" cache-margin "exceeded for" (db-sid-str (first key)))
               true)
             (let [ttl-margin (if expires-in (t/min expires-in cache-margin) cache-margin)]
               (when  (t/> (mem-cache-time-passed ctrl key now) ttl-margin)
                 (log/dbg "Cache TTL exceeded" ttl-margin "for" (db-sid-str (first key)))
                 true))))))

(defn process-handler
  "Session processing handler wrapper. For the given session ID `sid` and remote IP
  address `remote-ip` it runs `handle` (from `SessionControl` protocol) using
  `ctrl`. Then it quickly checks if the refresh is needed (by checking whether
  session is expired or by calling `needs-refresh?`). If re-reading expiration time
  from a database is required, it will do it and check if the time really has
  changed. In such case the session data in will be updated (by refreshing just the
  last active time or by handling it again after cache eviction)."
  [^SessionControl ctrl cache-margin expires-in sid remote-ip]
  (let [^Session smap  (p/handle ctrl sid remote-ip)
        s-args         [sid remote-ip]
        active         (.active   smap)
        expired?       (.expired? smap)
        needs-refresh? (needs-refresh? ctrl s-args cache-margin active expires-in expired?)
        new-expired?   (and needs-refresh? (not expired?) (p/expired? smap))
        new-active     (if (and needs-refresh? (not new-expired?))
                         (let [db-sid (db-sid-smap smap)]
                           (log/dbg "Getting last active time from a database for" db-sid)
                           (if-some [t (p/get-active ctrl db-sid remote-ip)]
                             (if (= t active) nil t))))]
    (if (nil? new-active)
      (if new-expired?
        (do (log/dbg "Session expiry detected after recalculating times for" (.db-id smap))
            (p/invalidate ctrl sid remote-ip)
            (mkbad smap :error (state smap remote-ip)))
        smap)
      (let [new-expired? (p/expired? ctrl new-active)]
        (cond

          ;; no change in expiration status after getting active time from a db
          ;; (update active time in cache and in current session)

          (= expired? new-expired?)
          (do (log/dbg "Updating active time of" (.db-id smap))
              (db/mem-assoc-existing! (p/mem-handler ctrl) s-args :active new-active)
              (map/qassoc smap :active new-active))

          ;; (was) expired -> (is) not expired
          ;; (possible cross-node change, clear the cache and re-run handling)

          expired?
          (do (log/dbg "Session no longer expired, re-running handler for" (.db-id smap))
              (p/invalidate ctrl sid remote-ip)
              (p/handle ctrl sid remote-ip))

          ;; (was) not expired -> (is) expired
          ;; (duration margin, clear the cache, re-run validation and mark session as bad)

          new-expired?
          (let [^Session smap (map/qassoc smap :active new-active)]
            (log/dbg "Session expired after syncing last active time for" (.db-id smap))
            (p/invalidate ctrl sid remote-ip)
            (mkbad smap :error (state smap remote-ip))))))))

(defn process
  "Takes a session control object, functions, settings and a request map, and validates
  session against a database or memoized session data. Returns a session map or dummy
  session map if session was not obtained (session ID was not found in a database)."
  [^SessionControl ctrl ^Session malformed-session ^Session empty-session cache-margin expires-in req]
  (if-some [sid (p/identify ctrl req)]
    (let [remote-ip (ip/to-address (get req :remote-ip))]
      (if-not (sid-valid? sid)
        (mkbad malformed-session :id sid :ip remote-ip)
        (let [^Session smap (process-handler ctrl cache-margin expires-in sid remote-ip)]
          (if-not (.valid? smap)
            smap
            (if (pos-int? (p/set-active ctrl sid (db-sid-smap smap) remote-ip))
              (mkgood smap)
              (mkbad smap :error (SessionError. :error :session/db-problem
                                                (some-str-spc
                                                 "Problem updating session data"
                                                 (log/for-user
                                                  (.user-id    smap)
                                                  (.user-email smap)
                                                  (or (ip/plain-ip-str (ip/to-address (.ip smap)))
                                                      (get req :remote-ip/str)))))))))))
    empty-session))

(defn prolong
  "Re-validates session by updating its timestamp and re-running validation."
  ([src]
   (prolong src nil nil))
  ([src ip-address]
   (prolong src nil ip-address))
  ([src session-key ip-address]
   (if-some [^Session smap (p/session src session-key)]
     (if-some [sid (or (.err-id ^Session smap) (.id ^Session smap))]
       (let [^SessionControl ctrl (.control ^Session smap)
             ip-address           (ip/to-address (or ip-address (.ip ^Session smap)))
             ipplain              (ip/plain-ip-str ip-address)
             new-time             (t/now)]
         (log/msg "Prolonging session" (log/for-user (.user-id ^Session smap) (.user-email ^Session smap) ipplain))
         (let [^Session new-smap (map/qassoc smap :id sid :active new-time)
               stat              (state new-smap ip-address)]
           (if (correct-state? stat)
             (do  (p/set-active ctrl sid (or (db-sid-smap new-smap) (db-sid-str sid)) ip-address new-time)
                  (p/invalidate ctrl sid ip-address)
                  (if (not= ip-address (.ip smap)) (p/invalidate ctrl sid (.ip smap)))
                  (map/qassoc (p/handle ctrl sid ip-address) :prolonged? true))
             (do (log/wrn "Session re-validation error"
                          (log/for-user (.user-id ^Session smap) (.user-email ^Session smap) ipplain))
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
                      (if-not user-email (log/err "No user e-mail given when creating a session")) nil)
       :ok
       (let [t                   (t/now)
             ip                  (ip/to-address ip-address)
             ipplain             (ip/plain-ip-str ip)
             ^SessionConfig opts (p/config ^SessionControl ctrl)
             secured?            (.secured?        ^SessionConfig opts)
             id-field            (or (.id-field    ^SessionConfig opts) "session-id")
             skey                (or (.session-key ^SessionConfig opts) :session)
             ^Session sess       (Session. nil nil nil nil user-id user-email t t ip
                                           false false false false false
                                           skey id-field nil ctrl)
             sess                (gen-session-id sess secured? user-id ipplain)
             stat                (state sess ip)]
         (log/msg "Opening session" (log/for-user user-id user-email ipplain))
         (if-not (correct-state? stat)
           (do (log/err "Session incorrect after creation" (log/for-user user-id user-email ipplain))
               (mkbad sess :error stat))
           (let [updated-count (p/to-db ^SessionControl ctrl ^Session sess)
                 sess          (map/qassoc sess :db-token nil)]
             (p/invalidate ^SessionControl ctrl (p/identify ^Session sess) ip)
             (if (pos-int? updated-count)
               (do (if (.single-session? ^SessionConfig opts)
                     (p/del-uvars ^SessionControl ctrl user-id)
                     (p/del-svars ^SessionControl ctrl (db-sid-smap sess)))
                   (mkgood sess))
               (do (log/err "Problem saving session" (log/for-user user-id user-email ipplain))
                   (mkbad sess
                          :error (SessionError. :error :session/db-problem
                                                (str "Session cannot be saved"
                                                     (log/for-user user-id user-email ipplain)))))))))))))

;; Initialization

(defn- setup-invalidator
  [pre-handler mem-handler]
  (if (or (not mem-handler) (= mem-handler pre-handler))
    (constantly nil)
    (db/invalidator mem-handler)))

(defn- get-mem-atom
  [f]
  (let [mc (::mem/cache (meta f))]
    (if (and (instance? clojure.lang.IRef    mc)
             (instance? PluggableMemoization @mc)
             (instance? TTLCacheQ (.cache ^PluggableMemoization @mc))
             (some? (.ttl ^TTLCacheQ (.cache ^PluggableMemoization @mc))))
      mc)))

(defn- calc-cache-margin
  "Calculates `:cache-margin` field which is a basis for deciding if session cache for
  the given ID must be refreshed due to potential miscalculation of the expiration
  time caused by the marginal duration. Uses `:expires` and `:cache-ttl`
  configuration settings. Returns a duration.

  If the expiration time is greater than cache TTL more than twice, the result will
  be a simple subtraction of TTL duration from expiration duration.

  If the expiration time is greater than cache TTL but no more than twice, the result
  will be a TTL duration.

  If the expiration time is lesser than cache TTL more than twice, the result will be
  an expiration duration.

  If the expiration time is lesser than cache TTL but no more than twice, the result
  will be a simple subtraction of expiration duration from TTL duration.

  The `:cache-margin` is used as a precondition when investigating real time left in
  cache before making a decision about getting last active time from a database and
  refreshing internal structures (session, session cache)."
  [^SessionConfig config]
  (let [expires   (get config :expires)
        cache-ttl (get config :cache-ttl)]
    (map/qassoc config :cache-margin
                (if (and expires cache-ttl (pos? (t/seconds cache-ttl)) (pos? (t/seconds expires)))
                  (if (t/> expires cache-ttl)
                    (if (>= (t/divide expires cache-ttl) 2)
                      (t/- expires cache-ttl)
                      cache-ttl)
                    (if (>= (t/divide cache-ttl expires) 2)
                      expires
                      (t/- cache-ttl expires)))))))

(defn- setup-fn
  [^SessionConfig config k default]
  (or (var/deref (get config k)) default))

(defn- setup-id-fn
  [id-path]
  (let [id-path (if (coll? id-path) id-path [:params id-path])]
    (identify-session-path-compile id-path)))

(defn- make-session-config
  ^SessionConfig [m]
  (-> m
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
      (calc-cache-margin)
      (map->SessionConfig)))

(defn wrap-session
  "Session maintaining middleware."
  [k config]
  (let [dbname                   (db/db-name (get config :db))
        ^SessionConfig cfg       (make-session-config config)
        ^DataSource db           (get cfg :db)
        ^Duration cache-ttl      (get cfg :cache-ttl)
        session-key              (get cfg :session-key)
        sessions-table           (get cfg :sessions-table)
        variables-table          (get cfg :variables-table)
        session-id-path          (get cfg :id-path)
        session-id-field         (get cfg :id-field)
        ^Duration cache-margin   (get cfg :cache-margin)
        ^Boolean single-session? (get cfg :single-session?)
        ^Boolean secured?        (get cfg :secured?)
        ^Duration expires        (get cfg :expires)
        ^Duration hard-expires   (get cfg :hard-expires)
        session-id-field         (or session-id-field (if (coll? session-id-path) (last session-id-path) session-id-path))
        cfg                      (assoc cfg :id-field (or session-id-field "session-id"))
        expirer-fn               (if (pos-int? (time/seconds expires)) #(calc-expired-core expires %1) (constantly false))
        expirer-hard-fn          (if (pos-int? (time/seconds hard-expires)) #(calc-expired-core hard-expires %1) (constantly false))
        identifier-fn            (setup-id-fn session-id-path)
        cfg                      (assoc cfg :fn/identifier identifier-fn)
        getter-fn                (setup-fn cfg :fn/getter get-session-by-id)
        getter-fn-w              #(getter-fn cfg db sessions-table %1 %2)
        cfg                      (assoc cfg :fn/getter getter-fn-w)
        checker-config           (set/rename-keys cfg {:token-cache-size :cache-size :token-cache-ttl :cache-ttl})
        checker-fn               (setup-fn cfg :fn/checker check-encrypted)
        checker-fn-w             (db/memoizer checker-fn checker-config)
        cfg                      (assoc cfg :fn/checker checker-fn-w)
        pre-handler              ^{::mem/args-fn rest} #(handler %1 session-key session-id-field %2 %3)
        mem-handler              (db/memoizer pre-handler cfg)
        handler-fn-w             mem-handler
        mem-atom                 (get-mem-atom mem-handler)
        last-active-fn           (setup-fn cfg :fn/last-active get-last-active)
        update-active-fn         (setup-fn cfg :fn/update-active update-last-active)
        last-active-fn-w         #(last-active-fn cfg db sessions-table %1 %2)
        ^SessionConfig cfg       (assoc cfg :fn/last-active last-active-fn-w)
        update-active-fn-w       (fn
                                   (^Long [sid db-sid remote-ip]
                                    (let [t (t/now)]
                                      ;; session prolongation causes invalid params to be injected without validation!
                                      (db/mem-assoc-existing! mem-handler [sid remote-ip] :active t)
                                      (update-active-fn cfg db sessions-table db-sid remote-ip t)))
                                   (^Long [sid db-sid remote-ip t]
                                    (db/mem-assoc-existing! mem-handler [sid remote-ip] :active t)
                                    (update-active-fn cfg db sessions-table db-sid remote-ip t)))
        cfg                      (assoc cfg :fn/update-active update-active-fn-w)
        var-get-fn               (db/make-setting-getter  variables-table :session-id)
        var-put-fn               (db/make-setting-setter  variables-table :session-id)
        var-del-fn               (db/make-setting-deleter variables-table :session-id)
        vars-put-fn              #(apply var-put-fn %1 %2 %3)
        vars-get-fn              #(apply var-get-fn %1 %2 %3)
        vars-del-fn              #(apply var-del-fn %1 %2 %3)
        vars-del-user-fn         (setup-fn cfg :fn/del-user-vars delete-user-vars)
        vars-del-sess-fn         (setup-fn cfg :fn/del-sess-vars delete-session-vars)
        vars-del-user-fn-w       #(vars-del-user-fn cfg db sessions-table variables-table %)
        vars-del-sess-fn-w       #(vars-del-sess-fn cfg db sessions-table variables-table %)
        cfg                      (assoc cfg
                                        :fn/get-var       var-get-fn
                                        :fn/get-vars      vars-get-fn
                                        :fn/put-var       var-put-fn
                                        :fn/put-vars      vars-put-fn
                                        :fn/del-var       var-del-fn
                                        :fn/del-vars      vars-del-fn
                                        :fn/del-user-vars vars-del-user-fn-w
                                        :fn/del-sess-vars vars-del-sess-fn-w)
        setter-fn                (setup-fn cfg :fn/setter set-session)
        setter-fn-w              #(setter-fn cfg db sessions-table %)
        cfg                      (assoc cfg :fn/setter setter-fn-w)
        invalidator-fn           (setup-invalidator pre-handler mem-handler)
        cfg                      (assoc cfg :fn/invalidator invalidator-fn :fn/handler mem-handler)
        prolong-fn               prolong
        ^Session empty-sess      (Session. nil nil nil nil nil nil nil nil nil
                                           false false false false false
                                           session-key session-id-field
                                           nil nil)
        ^SessionConfig  cfg      (assoc cfg :fn/prolong prolong-fn)
        ^SessionControl ctrl     (reify p/SessionControl
                                   (config        ^SessionConfig [_]           cfg)
                                   (empty         ^Session [c]                 (map/qassoc empty-sess :control c))
                                   (expired?      ^Boolean [_ t]               (expirer-fn t))
                                   (hard-expired? ^Boolean [_ t]               (expirer-hard-fn t))
                                   (token-ok?     ^Boolean [_ plain enc]       (checker-fn-w plain enc))
                                   (from-db       ^Session [_ db-sid ip]       (getter-fn-w db-sid ip))
                                   (handle        ^Session [c sid ip]          (handler-fn-w c sid ip))
                                   (to-db         ^Long    [_ smap]            (setter-fn-w smap))
                                   (set-active    ^Long    [_ sid db-sid ip]   (update-active-fn-w sid db-sid ip))
                                   (set-active    ^Long    [_ sid db-sid ip t] (update-active-fn-w sid db-sid ip t))
                                   (get-active    ^Instant [_ db-sid ip]       (last-active-fn-w db-sid ip))
                                   (identify      ^String  [_ req]             (identifier-fn req))
                                   (mem-handler   [_] mem-handler)
                                   (mem-atom      [_] mem-atom)
                                   (mem-cache     [_] (if mem-atom (deref mem-atom)))
                                   (invalidate    [_ sid ip]     (invalidator-fn sid ip))
                                   (put-var       [_ db-sid k v] (var-put-fn  db db-sid k v))
                                   (get-var       [_ db-sid k]   (var-get-fn  db db-sid k))
                                   (del-var       [_ db-sid k]   (var-del-fn  db db-sid k))
                                   (put-vars      [_ db-sid kvs] (vars-put-fn db db-sid kvs))
                                   (get-vars      [_ db-sid ks]  (vars-get-fn db db-sid ks))
                                   (del-vars      [_ db-sid ks]  (var-del-fn  db db-sid ks))
                                   (del-svars     [_ db-sid]     (vars-del-sess-fn-w db-sid))
                                   (del-uvars     [_ user-id]    (vars-del-user-fn-w user-id)))
        ^Session empty-sess      (p/empty ^SessionControl ctrl)
        ^Session mlf-sess        (map/qassoc empty-sess
                                             :error (SessionError. :info :session/malformed-session-id
                                                                   "Malformed session-id parameter"))]
    (log/msg "Installing session handler:" k)
    (log/msg (str "Session exp: "   (str/replace (or expires      "-") "PT" "")
                  ", hard-exp: "    (str/replace (or hard-expires "-") "PT" "")
                  ", cache TTL: "   (str/replace (or cache-ttl    "-") "PT" "")
                  ", time margin: " (str/replace (or cache-margin "-") "PT" "")))
    (if dbname (log/msg "Using database" dbname "for storing sessions of" k))
    {:name    (keyword k)
     :config  cfg
     :compile (fn [{:keys [no-session?]} opts]
                (if (and (not no-session?) db)
                  (fn [h]
                    (fn [req]
                      (h
                       (map/qassoc req session-key (delay (process ctrl
                                                                   mlf-sess
                                                                   empty-sess
                                                                   cache-margin
                                                                   expires
                                                                   req))))))))}))

(system/add-init  ::default [k config] (wrap-session k config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
