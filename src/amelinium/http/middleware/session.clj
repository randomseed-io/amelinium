(ns

    ^{:doc    "amelinium service, session middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.session

  (:refer-clojure :exclude [parse-long uuid random-uuid empty empty?])

  (:require [clojure.set                  :as        set]
            [clojure.string               :as        str]
            [clojure.core.memoize         :as        mem]
            [lazy-map.core                :as   lazy-map]
            [crypto.equality              :as     crypto]
            [tick.core                    :as          t]
            [buddy.core.hash              :as       hash]
            [buddy.core.codecs            :as     codecs]
            [next.jdbc.sql                :as        sql]
            [next.jdbc                    :as       jdbc]
            [amelinium.db                 :as         db]
            [amelinium.logging            :as        log]
            [amelinium.system             :as     system]
            [amelinium.http               :as       http]
            [amelinium.auth.algo.scrypt   :as     scrypt]
            [amelinium.proto.session      :as          p]
            [amelinium.types.session      :refer    :all]
            [amelinium                    :refer    :all]
            [io.randomseed.utils          :refer    :all]
            [io.randomseed.utils.time     :as       time]
            [io.randomseed.utils.var      :as        var]
            [io.randomseed.utils.map      :as        map]
            [io.randomseed.utils.ip       :as         ip]
            [io.randomseed.utils.db.types :as      types])

  (:import (clojure.lang             Keyword
                                     Associative
                                     PersistentVector
                                     IPersistentMap
                                     IFn
                                     Fn)
           (java.time                Instant
                                     Duration)
           (javax.sql                DataSource)
           (inet.ipaddr              IPAddress)
           (clojure.core.memoize     PluggableMemoization)
           (clojure.core.cache       TTLCacheQ)
           (lazy_map.core            LazyMap)
           (amelinium.proto.session  SessionControl
                                     Sessionable)
           (amelinium                Session
                                     SessionConfig
                                     SessionError)))

(set! *warn-on-reflection* true)

(def ^:const sid-match (re-pattern "|^[a-f0-9]{30,128}(-[a-f0-9]{30,128})?$"))

(def one-second (t/new-duration 1 :seconds))

(defn config?
  "Returns `true` if the given value `v` is an instance of `SessionConfig` record."
  ^Boolean [v]
  (instance? SessionConfig v))

(defn session?
  "Returns `true` if the given value `v` is an instance of `Session` record."
  ^Boolean [v]
  (instance? Session v))

(defn get-session-key
  "Returns a session key as keyword by reading the given `session-key`, and if it is
  `nil` or `false` by getting a value associated with the `:session-key` route data
  key of the `req`, and if that fails by returning the `:session` keyword."
  (^Keyword [req]
   (or (http/get-route-data req :session-key)
       :session))
  (^Keyword [req ^Keyword session-key]
   (or session-key
       (http/get-route-data req :session-key)
       :session)))

(defn get-session-by-key
  "Obtains a session object from associative structure (usually a request map `req`) by
  looking for a value associated with a session key returned by calling
  `get-session-key`."
  ([req]
   (get req (get-session-key req)))
  ([req ^Keyword session-key]
   (get req (get-session-key req session-key))))

(defn sid-valid?
  "Returns `true` if the given session ID is valid."
  ^Boolean [sid]
  (boolean
   (and sid (string? sid) (not-empty-string? sid)
        (<= 30 (count sid) 256)
        (re-matches sid-match sid))))

(defn sid-valid
  "Returns the given `sid` if it is valid after converting it to a string. Otherwise it
  returns `nil`."
  [sid]
  (let [sid (some-str sid)] (if (sid-valid? sid) sid)))

(defmacro when-sid-valid
  "Conditionally evaluates expressions from `body` in an implicit `do` when the session
  ID given as `sid` is valid. Otherwise it returns `nil` without evaluating
  anything."
  [sid & body]
  `(if (sid-valid? ~sid) (do ~@body)))

(extend-protocol p/Sessionable

  Session

  (session
    (^Session [src] src)
    (^Session [src _] src))

  (inject
    ([smap dst ^Keyword session-key]
     (map/qassoc dst
                 (or session-key
                     (.session-key ^Session smap)
                     (http/get-route-data dst :session-key)
                     :session)
                 smap))
    ([smap dst]
     (map/qassoc dst
                 (or (.session-key ^Session smap)
                     (http/get-route-data dst :session-key)
                     :session)
                 smap)))

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

  Associative

  (session
    (^Session [req] (get-session-by-key req))
    (^Session [req ^Keyword session-key] (get-session-by-key req session-key)))

  (empty?
    (^Boolean [req]
     (if-some [^Session s (p/session req :session)]
       (and (nil? (.id     ^Session s))
            (nil? (.err-id ^Session s))
            (nil? (.error  ^Session s)))
       true))
    (^Boolean [req ^Keyword session-key]
     (if-some [^Session s (p/session req session-key)]
       (and (nil? (.id     ^Session s))
            (nil? (.err-id ^Session s))
            (nil? (.error  ^Session s)))
       true)))

  (inject
    (^Session [dst src]
     (if-some [^Session smap (p/session src)]
       (map/qassoc dst
                   (or  (.session-key smap)
                        (if-some [^SessionControl ctrl (.control smap)]
                          (if-some [^SessionConfig cfg (p/config ctrl)]
                            (.session-key cfg)))
                        (http/get-route-data dst :session-key)
                        :session)
                   smap)
       dst))
    (^Session [dst src ^Keyword session-key]
     (if-some [^Session smap (p/session src)]
       (map/qassoc dst
                   (or session-key
                       (.session-key smap)
                       (if-some [^SessionControl ctrl (.control smap)]
                         (if-some [^SessionConfig cfg (p/config ctrl)]
                           (.session-key cfg)))
                       (http/get-route-data dst :session-key)
                       :session)
                   smap)
       dst)))

  (control
    (^SessionControl [req]
     (if-some [^Session s (get-session-by-key req)]
       (.control s)))
    (^SessionControl [req ^Keyword session-key]
     (if-some [^Session s (get-session-by-key req session-key)]
       (.control s))))

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

(declare db-sid-str)
(declare db-sid-smap)

(extend-protocol p/SessionControl

  SessionConfig

  (config   (^SessionConfig [s] s) (^SessionConfig [s _] s))
  (control? (^Boolean [s] false) (^Boolean [s _] false))

  Session

  (get-var      [s k]   (p/get-var       (.control ^Session s) (db-sid-smap s) k))
  (get-vars     [s ks]  (p/get-vars      (.control ^Session s) (db-sid-smap s) ks))
  (put-var      [s k v] (p/put-var       (.control ^Session s) (db-sid-smap s) k v))
  (put-vars     [s kvs] (p/put-vars      (.control ^Session s) (db-sid-smap s) kvs))
  (del-var      [s k]   (p/del-var       (.control ^Session s) (db-sid-smap s) k))
  (del-vars     [s ks]  (p/del-vars      (.control ^Session s) (db-sid-smap s) ks))
  (del-svars    [s]     (p/del-svars     (.control ^Session s) (db-sid-smap s)))
  (del-uvars    [s]     (p/del-uvars     (.control ^Session s) (.user-id ^Session s)))
  (del-session  [s]     (p/del-session   (.control ^Session s) (db-sid-smap s)))
  (del-sessions [s]     (p/del-sessions  (.control ^Session s) (.user-id ^Session s)))
  (mem-atom     [s]     (p/mem-atom      (.control ^Session s)))
  (mem-handler  [s]     (p/mem-handler   (.control ^Session s)))
  (mem-cache    [s]     (p/mem-cache     (.control ^Session s)))

  (config
    (^SessionConfig [s]       (p/config (.control ^Session s)))
    (^SessionConfig [s _]     (p/config (.control ^Session s))))

  (control?
    (^Boolean [s]             (p/control? (.control ^Session s)))
    (^Boolean [s k]           (p/control? (.control ^Session s))))

  (empty
    (^Session [s]             (p/empty (.control ^Session s)))
    (^Session [s _]           (p/empty (.control ^Session s))))

  (to-db
    (^Long    [s]             (p/to-db (.control ^Session s) s))
    (^Long    [s _]           (p/to-db (.control ^Session s) s)))

  (identify
    (^String  [s]              (or (.id ^Session s) (.err-id ^Session s)))
    (^String  [s req]          (or (.id ^Session s) (.err-id ^Session s)
                                   (p/identify (.control ^Session s) req))))

  (from-db
    (^Session [s]
     (p/from-db (.control ^Session s) (db-sid-smap s) (.ip ^Session s)))
    (^Session [s ^String db-sid]
     (p/from-db (.control ^Session s) db-sid (.ip ^Session s)))
    (^Session [s ^String db-sid ^IPAddress ip]
     (p/from-db (.control ^Session s) db-sid ip)))

  (handle
    (^Session [s]
     (p/handle (.control ^Session s) (p/identify ^Session s) (db-sid-smap ^Session s) (.ip ^Session s)))
    (^Session [s ^String sid]
     (p/handle (.control ^Session s) sid (db-sid-str sid) (.ip ^Session s)))
    (^Session [s ^String sid ^IPAddress ip]
     (p/handle (.control ^Session s) sid  (db-sid-str sid) ip))
    (^Session [s ^String sid ^String db-sid ^IPAddress ip]
     (p/handle (.control ^Session s) sid db-sid ip)))

  (mem-handler
    (^Fn [s]
     (p/mem-handler (.control ^Session s)))
    (^Fn [s _]
     (p/mem-handler (.control ^Session s))))

  (invalidate
    ([s]
     (p/invalidate (.control ^Session s) (db-sid-smap ^Session s) (.ip ^Session s)))
    ([s ^String db-sid]
     (p/invalidate (.control ^Session s) db-sid (.ip ^Session s)))
    ([s ^String db-sid ^IPAddress ip]
     (p/invalidate (.control ^Session s) db-sid ip)))

  (get-active
    (^Instant [s]
     (p/get-active (.control ^Session s) (p/identify ^Session s) (.ip ^Session s)))
    (^Instant [s ^String db-sid]
     (p/get-active (.control ^Session s) db-sid (.ip ^Session s)))
    (^Instant [s ^String db-sid ^IPAddress ip]
     (p/get-active (.control ^Session s) db-sid ip)))

  (set-active
    (^Long [s]
     (p/set-active (.control ^Session s) (db-sid-smap s) (.ip ^Session s)))
    (^Long [s ^IPAddress ip]
     (p/set-active (.control ^Session s) (db-sid-smap s) ip))
    (^Long [s ^IPAddress ip ^Instant t]
     (p/set-active (.control ^Session s) (db-sid-smap s) ip t))
    (^Long [s ^String db-sid ^IPAddress ip ^Instant t]
     (p/set-active (.control ^Session s) db-sid ip t)))

  (expired?      ^Boolean [s]     (p/expired?      (.control ^Session s) (.active ^Session s)))
  (hard-expired? ^Boolean [s]     (p/hard-expired? (.control ^Session s) (.active ^Session s)))
  (token-ok?     ^Boolean [s p e] (p/token-ok?     (.control ^Session s) p e))

  clojure.lang.Associative

  (config
    (^SessionConfig [req] (p/config (p/session req)))
    (^SessionConfig [req ^Keyword session-key] (p/config (p/session req session-key))))

  (control?
    (^Boolean [req] (p/control? (p/session req)))
    (^Boolean [req ^Keyword session-key] (p/control? (p/session req session-key))))

  (empty
    (^Session [req] (p/empty (p/session req)))
    (^Session [req ^Keyword session-key] (p/empty (p/session req session-key))))

  (mem-handler
    ([req] (p/mem-handler (p/session req)))
    ([req ^Keyword session-key] (p/mem-handler (p/session req session-key))))

  (identify
    (^String [req]
     (or (p/identify (p/session req))
         (sid-valid (or (some-str (get-in req [:params  :session-id]))
                        (some-str (get-in req [:params  "session-id"]))
                        (some-str (get-in req [:headers "session-id"]))))))
    (^String [req session-key-or-req-path]
     (if (coll? session-key-or-req-path)
       (sid-valid (get-in req session-key-or-req-path))
       (p/identify (p/session req session-key-or-req-path)))))

  nil

  (^Boolean token-ok? [s e p] false)
  (^Boolean expired?      [s] false)
  (^Boolean hard-expired? [s] false)

  (to-db         [s]               nil)
  (mem-atom      [s]               nil)
  (mem-cache     [s]               nil)
  (mem-handler  ([s]               nil) ([s s-k]       nil))
  (empty        ([s]               nil) ([s s-k]       nil))
  (config       ([s]               nil) ([s s-k]       nil))
  (control?     ([s]             false) ([s s-k]     false))
  (identify     ([s]               nil) ([s req]       nil))
  (from-db      ([s db-sid ip]     nil) ([s db-sid]    nil) ([s] nil))
  (invalidate   ([s db-sid ip]     nil) ([s db-sid]    nil) ([s] nil))
  (get-active   ([s db-sid ip]     nil  ([s db-sid]    nil) ([s] nil)))
  (get-var      ([s db-sid k]      nil) ([s k]         nil))
  (get-vars     ([s db-sid ks]     nil) ([s ks]        nil))
  (put-var      ([s db-sid k v]    nil) ([s k v]       nil))
  (put-vars     ([s db-sid kvs]    nil) ([s kvs]       nil))
  (del-var      ([s db-sid k]      nil) ([s k]         nil))
  (del-vars     ([s db-sid ks]     nil) ([s ks]        nil))
  (del-svars    ([s db-sid]        nil) ([s]           nil))
  (del-uvars    ([s uid]           nil) ([s]           nil))
  (del-session  ([s db-sid]        nil) ([s]           nil))
  (del-sessions ([s user-id]       nil) ([s]           nil))
  (handle       ([s sid db-sid ip] nil) ([s sid ip]    nil) ([s sid] nil) ([s] nil))
  (set-active
    ([s db-sid ip t] nil)
    ([s ip t] nil)
    ([s ip] nil)
    ([s] nil))

  Object

  (control? ([s] false) ([s s-k] false))
  (empty [s] (clojure.core/empty s)))

(defn of
  "Returns a session record of type `Session` on a basis of configuration source
  provided and an optional `session-key` if session must be looked in an associative
  structure (defaults to `:session`)."
  (^Session [^Sessionable src] (p/session src))
  (^Session [^Sessionable src ^Keyword session-key] (p/session src session-key)))

(defn empty?
  "Returns `false` is `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` may be given to express a key in associative
  structure (defaults to `:session`)."
  (^Boolean [^Sessionable src] (p/empty? src))
  (^Boolean [^Sessionable src ^Keyword session-key] (p/empty? src session-key)))

(defn not-empty?
  "Returns `true` is `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` may be given to express a key in associative
  structure (defaults to `:session`)."
  (^Boolean [^Sessionable src] (not (p/empty? src)))
  (^Boolean [^Sessionable src ^Keyword session-key] (not (p/empty? src session-key))))

(defn empty
  "Returns an empty session record. Optional `session-key` may be given to express a
  key in associative structure (defaults to `:session`) used to perform a session
  lookup used to access the control object."
  (^Boolean [^Sessionable src] (p/empty src))
  (^Boolean [^Sessionable src ^Keyword session-key] (p/empty src session-key)))

(defn inject
  "Returns an object updated with session record of type `Session` under an optional
  `session-key` if session is to be put into an associative structure (defaults to
  `:session`)."
  ([dst ^Sessionable smap] (p/inject dst smap))
  ([dst ^Sessionable smap ^Keyword session-key] (p/inject dst smap session-key)))

(defn not-empty-of
  "Returns a session if `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` may be given to express a key in associative
  structure (defaults to `:session`). Returns `nil` if session is not usable (does
  not have `:id`, `:err-id` not `:error` set)."
  (^Session [^Sessionable  src]
   (if-let [^Session s (p/session src)]
     (if-not (p/empty? s) s)))
  (^Session [^Sessionable src ^Keyword session-key]
   (if-let [^Session s (p/session src session-key)]
     (if-not (p/empty? s) s))))

(defn valid-of
  "Returns a session if `src` contains a session or is a session, and the session is
  valid. Optional `session-key` may be given to express a key in associative
  structure (defaults to `:session`). Returns `nil` if session is not valid."
  (^Session [^Sessionable  src]
   (when-let [^Session s (p/session src)]
     (when (.valid? s) s)))
  (^Session [^Sessionable src ^Keyword session-key]
   (when-let [^Session s (p/session src session-key)]
     (when (.valid? s) s))))

(defn control?
  "Returns `true` if the given value is an instance of a class which fully satisfies
  `SessionControl` protocol (directly or indirectly). If the indirect testing is
  required, the tested object must be convertable to a control object (as described
  by the `SessionControl` protocol)."
  (^Boolean [v]   (p/control? v))
  (^Boolean [v k] (p/control? v k)))

(defn control
  "Returns a `SessionControl` object from the given source and optional session
  key. The `src` can be a session record (`Session`) or any object which satisfies
  the `Sessionable` protocol and can be converted to a session. Optional
  `session-key` can be given when passing a request map."
  (^SessionControl [^Sessionable src] (p/control src))
  (^SessionControl [^Sessionable src ^Keyword session-key] (p/control src session-key)))

(defn config
  "Returns a `SessionConfig` object from the given source and optional session key. The
  `src` can be a session record (`Session`) or any object which satisfies the
  `Sessionable` protocol and can be converted to a session. Optional `session-key`
  can be given when passing a request map."
  (^SessionConfig [^Sessionable src] (p/config src))
  (^SessionConfig [^Sessionable src ^Keyword session-key] (p/config src session-key)))

(defn id
  "Returns a session ID from the given source and optional session key. Works only on
  valid sessions having `:id` field set. The `src` can be a session
  record (`Session`) or any object which satisfies the `Sessionable` protocol and can
  be converted to a session. Optional `session-key` can be given when passing a
  request map."
  (^String [^Sessionable src]
   (when-some [^Session s (p/session src)] (.id s)))
  (^String [^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (.id s))))

(defn err-id
  "Returns an error session ID from the given source and optional session key. Works
  only on invalid sessions having `:err-id` field set. The `src` can be a session
  record (`Session`) or any object which satisfies the `Sessionable` protocol and can
  be converted to a session. Optional `session-key` can be given when passing a
  request map."
  (^String [^Sessionable src]
   (when-some [^Session s (p/session src)] (.err-id s)))
  (^String [^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (.err-id s))))

(defn any-id
  "Returns a session ID from the given source and optional session key. May return a
  value of the `:id` or `:err-id` field. The `src` can be a session
  record (`Session`) or any object which satisfies the `Sessionable` protocol and can
  be converted to a session. Optional `session-key` can be given when passing a
  request map."
  (^String [^Sessionable src]
   (when-some [^Session s (p/session src)] (or (.id s) (.err-id s))))
  (^String [^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (or  (.id s) (.err-id s)))))

(defn db-token
  "Returns the `:db-token` (security token in a database-suitable form) property of a
  session. The `src` can be a session record (`Session`) or any object which
  satisfies the `Sessionable` protocol and can be converted to a session. Optional
  `session-key` can be given when passing a request map."
  (^String [^Sessionable src]
   (when-some [^Session s (p/session src)] (.db-token s)))
  (^String [^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (.db-token s))))

(defn user-id
  "Returns the `:user-id` (user ID) property of a session. The `src` can be a session
  record (`Session`) or any object which satisfies the `Sessionable` protocol and can
  be converted to a session. Optional `session-key` can be given when passing a
  request map."
  (^Long [^Sessionable src]
   (when-some [^Session s (p/session src)] (.user-id s)))
  (^Long [^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (.user-id s))))

(defn user-email
  "Returns the `:user-email` (user e-mail) property of a session. The `src` can be a
  session record (`Session`) or any object which satisfies the `Sessionable` protocol
  and can be converted to a session. Optional `session-key` can be given when passing
  a request map."
  (^String [^Sessionable src]
   (when-some [^Session s (p/session src)] (.user-email s)))
  (^String [^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (.user-email s))))

(defn created
  "Returns the `:created` (creation time) property of a session. The `src` can be a
  session record (`Session`) or any object which satisfies the `Sessionable` protocol
  and can be converted to a session. Optional `session-key` can be given when passing
  a request map."
  (^Instant [^Sessionable src]
   (when-some [^Session s (p/session src)] (.created s)))
  (^Instant [^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (.created s))))

(defn active
  "Returns the `:active` (last active time) property of a session. The `src` can be a
  session record (`Session`) or any object which satisfies the `Sessionable` protocol
  and can be converted to a session. Optional `session-key` can be given when passing
  a request map."
  (^Instant [^Sessionable src]
   (when-some [^Session s (p/session src)] (.active s)))
  (^Instant [^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (.active s))))

(defn ip
  "Returns the `:ip` (IP address) property of a session. The `src` can be a session
  record (`Session`) or any object which satisfies the `Sessionable` protocol and can
  be converted to a session. Optional `session-key` can be given when passing a
  request map."
  (^IPAddress [^Sessionable src]
   (when-some [^Session s (p/session src)] (.ip s)))
  (^IPAddress [^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (.ip s))))

(defn session-key
  "Returns the `:session-key` (session key) property of a session. The `src` can be a
  session record (`Session`) or any object which satisfies the `Sessionable` protocol
  and can be converted to a session. Optional `session-key` can be given when passing
  a request map."
  ([^Sessionable src]
   (when-some [^Session s (p/session src)] (or (.session-key s) :session)))
  ([^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (or (.session-key s) :session))))

(defn id-field
  "Returns the `:id-field` (field ID) property of a session. The `src` can be a session
  record (`Session`) or any object which satisfies the `Sessionable` protocol and can
  be converted to a session. Optional `session-key` can be given when passing a
  request map."
  ([^Sessionable src]
   (when-some [^Session s (p/session src)] (.id-field s)))
  ([^Sessionable src ^Keyword session-key]
   (when-some [^Session s (p/session src session-key)] (.id-field s))))

(defn prolonged?
  "Returns `true` if the given session has just been prolonged. The `src` can be a
  session record (`Session`) or any object which satisfies the `Sessionable` protocol
  and can be converted to a session. Optional `session-key` can be given when passing
  a request map."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)] (.prolonged? s) false))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)] (.prolonged? s) false)))

(defn mem-ctime
  "Retrieves an entry creation time (in milliseconds) associated with the given `key`
  in a TTL map of a current handler cache. If the entry does not exist, `nil` is
  returned."
  [^SessionControl ctrl key]
  (when-some [^PluggableMemoization m (p/mem-cache ctrl)]
    (nth (get (.ttl ^TTLCacheQ (.cache m)) key) 1 nil)))

(defn mem-etime
  "Retrieves an expiration time (in milliseconds since the beginning of the Unix epoch)
  of an entry identified by the given key `key` in a TTL map of a current handler
  cache. It is calculated by adding cache's TTL to entry's creation time. If the
  entry does not exist, `nil` is returned."
  [^SessionControl ctrl key]
  (when-some [^PluggableMemoization m (p/mem-cache ctrl)]
    (let [^TTLCacheQ c (.cache m)]
      (when-some [ctime (nth (get (.ttl c) key) 1 nil)]
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

;; Logging

(defn- for-user
  "Wrapper for `amelinium.logging/for-user` which accepts session object as a source."
  ([^Session s]              (if s (log/for-user (.user-id s) (.user-email s) (.ip s))))
  ([user-id user-email]            (log/for-user user-id user-email))
  ([user-id user-email ip-address] (log/for-user user-id user-email ip-address)))

;; Secure sessions

(def ^:const scrypt-options
  {:cpu-cost 512
   :mem-cost 2
   :parallel 1})

(def ^:const token-splitter (re-pattern "-"))
(def ^:const salt-splitter  (re-pattern "\\$"))

(defn- bytes->b64u
  "Takes a byte array and returns its content encoded with Base64. If the object cannot
  be converted, returns `nil`."
  ^String [^"[B" v]
  (try (if v (codecs/bytes->str (codecs/bytes->b64u v)))
       (catch Throwable _
         nil)))

(defn- b64u->bytes
  "Takes a Base64 string and decodes it into a byte array. If the object cannot be
  converted, returns `nil`."
  ^"[B" [^String v]
  (try (if v (codecs/b64u->bytes (codecs/str->bytes v)))
       (catch Throwable _
         nil)))

(defn encrypt
  "Encrypts the given `plain-token` with Scrypt algorithm using randomly generated
  salt. Returns a Base64-encoded string with salt and encrypted password separated
  with a dollar character. The encrypted password is a result of applying one-way
  hashing function to the `plain-token` and salt."
  ^String [^String plain-token]
  (if plain-token
    (if-some [enc (scrypt/encrypt plain-token scrypt-options)]
      (str (bytes->b64u (bytes (get enc :salt))) "$"
           (bytes->b64u (bytes (get enc :password)))))))

(defn check-encrypted
  "Checks if the encrypted token given as `encrypted-token-b64-str` matches the given
  `plain-token`. Verifies its correctness by re-encrypting it with the same salt
  parameter as it was used when original token was created."
  ^Boolean [^String plain-token ^String encrypted-token-b64-str]
  (if (and plain-token encrypted-token-b64-str)
    (if-some [^String salt-pass (str/split encrypted-token-b64-str salt-splitter 2)]
      (crypto/eq? (b64u->bytes (nth salt-pass 1 nil))
                  (get (scrypt/encrypt plain-token
                                       (b64u->bytes (nth salt-pass 0 nil))
                                       scrypt-options) :password)))))

(defn- split-secure-sid
  "Splits secure ID into a vector of 2 strings: session ID and password (a.k.a secure
  token)."
  ^String [^String session-id]
  (if session-id (str/split session-id token-splitter 2)))

(defn db-id
  "Returns a database session ID from a session object or any other value which can be
  transformed into a session."
  (^String [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (.db-id s)))
  (^String [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (.db-id s))))

(defn db-sid-smap
  "Extracts a database session ID from a secure session object. Tries `:db-id` field
  and if that fails uses `:id` or `:err-id` field of a session object and parses its
  string representation to obtain only its first part (if it consists of multiple
  parts separated with a dash character)."
  ^String [^Session smap]
  (or (.db-id smap)
      (nth (split-secure-sid (or (.id smap) (.err-id smap))) 0 nil)))

(defn db-sid-str
  "Extracts a database session ID from a secure session ID."
  ^String [^String sid]
  (nth (split-secure-sid sid) 0 nil))

(defn db-sid
  "Extracts a database session ID from a secure session object, a map, or session
  ID. Works with session objects, maps, strings and identifiers (symbols and
  keywords)."
  ^String [obj]
  (cond
    (session? obj) (db-sid-smap obj)
    (map?     obj) (or (get obj :db-sid) (db-sid (or (get obj :sid) (get obj :err-id) (get obj :id))))
    (string?  obj) (nth (split-secure-sid obj) 0 nil)
    (ident?   obj) (db-sid (name obj))
    :default       nil))

;; Session creation

(defn make
  "Generates a session ID and adds it to newly created session object. The `secured?`
  flag can be set to generate secure session ID."
  ^Session [^Boolean secured? ^SessionControl ctrl skey id-field
            ^Long user-id ^String user-email ^IPAddress ip]
  (let [^Instant t  (t/now)
        ^String rnd (str (time/timestamp) user-email (gen-digits 8))
        ^String sid (-> rnd hash/md5 codecs/bytes->hex)]
    (or (if secured?
          (let [^String pass (-> (gen-digits 10) hash/md5 codecs/bytes->hex)
                ^String stok (encrypt pass)]
            (if (not-empty stok)
              (Session. (str sid "-" pass)
                        nil sid stok user-id user-email t t ip
                        false false false false true true
                        skey id-field nil ctrl))))
        (Session. sid nil sid nil user-id user-email t t ip
                  false false false false false false
                  skey id-field nil ctrl))))

;; Session deletion

(defn invalidate-multi
  "Invalidates cache for multiple sessions by extracting their database IDs from
  maps. The `sessions` argument should be a sequence of maps or a sequence of
  `Session` objects. If they are maps they should have `:id` keys with associated
  database IDs. First argument should be a `SessionControl` object."
  [^SessionControl ctrl sessions]
  (if sessions
    (doseq [sdata (filter not-empty sessions)]
      (if-some [^String db-sid (if sdata (or (:id sdata) (db-sid sdata)))]
        (when-some [^String ip (:ip sdata)]
          (log/dbg "Invalidating session" db-sid
                   (for-user (get sdata :user-id) (get sdata :user-email) ip))
          (p/invalidate ctrl db-sid (ip/to-address ip)))))))

(defn delete!
  "Deletes a session from a database. The session is identified by a session object or
  any other value which can be transformed into a session (`src`) with optional
  key (`session-key`) if this object is associative. Session variables are also
  removed."
  ([^Sessionable src]
   (delete! src :session))
  ([^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (if-some [^String db-sid (db-sid-smap s)]
       (let [^SessionControl ctrl (.control s)]
         (p/del-svars   ctrl db-sid)
         (p/del-session ctrl db-sid)
         (p/invalidate  ctrl db-sid (.ip s)))
       (log/wrn "Cannot delete session because session ID is not valid" (for-user s))))))

(defn delete-all!
  "Deletes all user sessions from a database. The sessions are identified by a user ID
  derived from a session object, any other value which can be transformed into a
  session object (`src`) with optional session key for associative structures, or a
  user ID given directly (`user-id`). In the last case session will only be used to
  obtain control object so it may be a default session without any valid identifiers
  in it. Session variables are also removed."
  {:arglists '([^Sessionable src]
               [^Sessionable src ^Keyword session-key]
               [^Sessionable src ^Long user-id]
               [^Sessionable src ^Keyword session-key ^Long user-id]
               [^SessionControl ctrl ^Long user-id ^String user-email ip-address])}
  ([^Sessionable src]
   (delete-all! src :session))
  ([^Sessionable src skey-or-user-id]
   (if (pos-int? skey-or-user-id)
     (delete-all! src :session skey-or-user-id)
     (if-some [^Session s (p/session src skey-or-user-id)]
       (if-some [^Long user-id (.user-id s)]
         (let [^SessionControl ctrl (.control s)]
           (p/del-uvars ctrl user-id)
           (invalidate-multi ctrl (p/del-sessions ctrl user-id)))
         (log/wrn "Cannot delete sessions because user ID is not valid" (for-user s))))))
  ([^Sessionable src ^Keyword session-key ^Long user-id]
   (if user-id
     (if-some [^Session s (p/session src session-key)]
       (let [^SessionControl ctrl (.control s)]
         (p/del-uvars ctrl user-id)
         (invalidate-multi ctrl (p/del-sessions ctrl user-id)))
       (log/wrn "Cannot delete sessions because session control not available"
                (for-user user-id nil
                          (if (map? src) (or (get src :remote-ip/str)
                                             (get src :ip)
                                             (get src :remote-ip))))))))
  ([^SessionControl ctrl ^Long user-id ^String user-email ip-address]
   (if user-id
     (when-some [^SessionControl ctrl (p/control ctrl)]
       (p/del-uvars ctrl user-id)
       (invalidate-multi ctrl (p/del-sessions ctrl user-id))
       (log/wrn "Cannot delete sessions because session control not available"
                (for-user user-id user-email ip-address))))))

;; Session validation

(defn secure?
  "Checks if a session is secure according to configured security level. If `:secured?`
  option is not enabled in configuration, it returns `true`. If `:secure?` flag is
  set to a truthy value, it returns it.  If there is no session, it returns `false`."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (or (.secure? s)
         (not (if-some [^SessionConfig c (config s)] (.secured? c))))
     false))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (or (.secure? s)
         (not (if-some [^SessionConfig c (config s)] (.secured? c))))
     false)))

(defn insecure?
  "Checks if session is not secure where it should be. If `:secured?` option is not
  enabled in configuration, it returns `false`. If `:secure?` flag is set to a falsy
  value, it returns `false`. If there is no session, it returns `true`."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (and (not (.secure? s))
          (if-some [^SessionConfig c (config s)] (.secured? c) false))
     true))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (and (not (.secure? s))
          (if-some [^SessionConfig c (config s)] (.secured? c) false))
     true)))

(defn security-passed?
  "Checks if the additional security token was validated correctly or there was not a
  need to validate it because the session is not secure (in such case returns
  `true`). Does not test if session should be secured; to check it, use `secure?` or
  `insecure?`."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (or (not (.secure? s))
         (.security-passed? s))
     false))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (or (not (.secure? s))
         (.security-passed? s))
     false)))

(defn security-failed?
  "Checks if the additional security token was validated incorrectly unless the session
  is not secure (in such case it returns `false`). Does not test if session should be
  secured; to check it, use `secure?` or `insecure?`."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (and (.secure? s)
          (not (.security-passed? s)))
     true))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (and  (.secure? s)
           (not (.security-passed? s)))
     true)))

(defn ip-state
  "Returns session IP state for the given session. If IP address is correct, returns
  `nil`, otherwise a `SessionError` record."
  [^Session smap ^Long user-id ^String user-email ^IPAddress remote-ip]
  (if smap
    (if-some [session-ip (or (.ip smap) (get smap :ip-address))]
      (if-some [remote-ip (ip/to-address remote-ip)]
        (if-not (or (= (ip/to-v6 remote-ip) (ip/to-v6 session-ip))
                    (= (ip/to-v4 remote-ip) (ip/to-v4 session-ip)))
          (SessionError. :warn :session/bad-ip
                         (str-spc "Session IP address" (str "(" (ip/plain-ip-str session-ip) ")")
                                  "is different than the remote IP address"
                                  (str "(" (ip/plain-ip-str remote-ip) ")")
                                  (for-user user-id user-email))))
        (if-some [str-addr (ip/to-str remote-ip)]
          (if-not (or (= str-addr (ip/to-str session-ip))
                      (= str-addr (ip/to-str (ip/to-v4 session-ip)))
                      (= str-addr (ip/to-str (ip/to-v6 session-ip))))
            (SessionError. :warn :session/bad-ip
                           (str-spc "Session IP string" (str "(" (ip/to-str remote-ip) ")")
                                    "is different than the remote IP string"
                                    (str "(" str-addr ")")
                                    (for-user user-id user-email)))))))))

(defn- same-ip?
  (^Boolean [state-result]
   (nil? state-result))
  (^Boolean [^Session smap ^Long user-id ^String user-email ^IPAddress remote-ip]
   (nil? (ip-state smap user-id user-email remote-ip))))

(defn- time-exceeded?
  (^Boolean [^Duration dur ^Duration max-dur]
   (t/> dur max-dur))
  (^Boolean [^Instant t-start ^Instant t-stop ^Duration max-dur]
   (t/> (t/between t-start t-stop) max-dur)))

(defn- calc-expired-core
  (^Boolean [^Duration exp ^Instant last-active]
   (time-exceeded? last-active (t/now) exp)))

(defn calc-expired?
  "Returns `true` if the given session is expired by performing calculations on its
  last-active time and a configured timeout. The session can be passed directly or as
  a request map from which it should be extracted (in such case the optional
  `session-key` argument may be given)."
  (^Boolean [^Sessionable src ^Keyword session-key]
   (calc-expired? (p/session src session-key)))
  (^Boolean [^Sessionable src]
   (if-some [^Session smap (p/session src)]
     (p/expired? smap))))

(defn calc-hard-expired?
  "Returns `true` if the given session is hard-expired by performing calculations on
  its last-active time and a configured timeout. The session can be passed directly
  or as a request map from which it should be extracted (in such case the optional
  `session-key` argument may be given)."
  (^Boolean [^Sessionable src ^Keyword session-key]
   (calc-hard-expired? (p/session src session-key)))
  (^Boolean [^Sessionable src]
   (if-some [^Session smap (p/session src)]
     (p/hard-expired? smap))))

(defn calc-soft-expired?
  "Returns `true` if the given session is soft-expired by performing calculations on
  its last-active time and a configured timeout. The session can be passed directly
  or as a request map from which it should be extracted (in such case the optional
  `session-key` argument may be given)."
  (^Boolean [^Sessionable src ^Keyword session-key]
   (calc-soft-expired? (p/session src session-key)))
  (^Boolean [^Sessionable src]
   (if-some [^Session smap (p/session src)]
     (and (p/expired? ^Session smap)
          (not (p/hard-expired? smap))))))

(defn expired?
  "Returns `true` if the given session is marked as expired. The session can be passed
  directly or as a request map from which it should be extracted (in such case the
  optional `session-key` argument may be given)."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (.expired? s)
     false))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (.expired? s)
     false)))

(defn hard-expired?
  "Returns `true` if the given session is marked as hard-expired. The session can be
  passed directly or as a request map from which it should be extracted (in such case
  the optional `session-key` argument may be given)."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (.hard-expired? s)
     false))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (.hard-expired? s)
     false)))

(defn soft-expired?
  "Returns `true` if the given session is marked as soft-expired. The session can be
  passed directly or as a request map from which it should be extracted (in such case
  the optional `session-key` argument may be given)."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (and (.expired? s)
          (not (.hard-expired? s)))
     false))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (and (.expired? s)
          (not (.hard-expired? s)))
     false)))

(defn created-valid?
  "Returns `true` if the given session has valid creation time. The session can be
  passed directly or as a request map from which it should be extracted (in such case
  the optional `session-key` argument may be given)."
  (^Boolean [^Sessionable src]
   (let [^Instant ct (created src)]
     (and (t/instant? ct) (t/> (t/now) ct))))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (let [^Instant ct (created src session-key)]
     (and (t/instant? ct) (t/> (t/now) ct)))))

(defn active-valid?
  "Returns `true` if the given session has valid last-active time. The session can be
  passed directly or as a request map from which it should be extracted (in such case
  the optional `session-key` argument may be given)."
  (^Boolean [^Sessionable src]
   (let [^Instant at (active src)]
     (and (t/instant? at) (t/> (t/now) at))))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (let [^Instant at (active src session-key)]
     (and (t/instant? at) (t/> (t/now) at)))))

(defn state
  "Returns session state. If there is anything wrong, returns a `SessionError`
  record. Otherwise it returns `nil`. Unknown session detection is performed by
  checking if a value associated with the `:id` key is `nil` and a value associated
  with the `:err-id` key is not `nil`. First argument must be a session object
  of type `Session`."
  [^Session smap ^IPAddress ip-address]
  (if-not (session? smap)
    (SessionError. :info :session/missing (str-spc "No session:" smap))
    (let [^String sid        (.id         smap)
          ^String esid       (.err-id     smap)
          ^Long user-id      (.user-id    smap)
          ^String user-email (.user-email smap)
          ^String any-sid    (or sid esid)
          user-ident         (or user-id user-email)
          ^Long user-id      (valuable user-id)
          ^String user-email (some-str user-email)
          for-user           (delay (for-user user-id user-email ip-address))]
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
  (^Boolean [^Sessionable src ^IPAddress ip-address]
   (not (instance? SessionError (state (try (p/session src) (catch Throwable _ nil)) ip-address))))
  (^Boolean [^Sessionable src ^Keyword session-key ^IPAddress ip-address]
   (not (instance? SessionError (state (try (p/session src session-key) (catch Throwable _ nil)) ip-address)))))

(defn valid?
  "Returns the `:valid?` (validity) property of a session. The `src` can be a session
  record (`Session`) or any object which satisfies the `Sessionable` protocol and can
  be converted to a session. Optional `session-key` can be given when passing a
  request map. If there is no session, returns `false`."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (.valid? s)
     false))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (.valid? s)
     false)))

(defn invalid?
  "Returns negated value of the `:valid?` (validity) property of a session. The `src`
  can be a session record (`Session`) or any object which satisfies the `Sessionable`
  protocol and can be converted to a session. Optional `session-key` can be given
  when passing a request map. If there is no session, returns `true`."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (not (.valid? s))
     true))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (not (.valid? s))
     true)))

(defn error?
  "Returns `true` if the `:error` (session error) property of a session is not
  `nil`. The `src` can be a session record (`Session`) or any object which satisfies
  the `Sessionable` protocol and can be converted to a session. Optional
  `session-key` can be given when passing a request map."
  (^Boolean [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (some? (.error s))))
  (^Boolean [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (some? (.error s)))))

(defn error
  "Returns the `:error` (session error) property of a session. The `src` can be a
  session record (`Session`) or any object which satisfies the `Sessionable` protocol
  and can be converted to a session. Optional `session-key` can be given when passing
  a request map."
  (^SessionError [^Sessionable src]
   (if-some [^Session s (p/session src)]
     (.error s)))
  (^SessionError [^Sessionable src ^Keyword session-key]
   (if-some [^Session s (p/session src session-key)]
     (.error s))))

(defn allow-expired
  "Temporarily marks expired session as valid.  The `src` can be a session
  record (`Session`) or any object which satisfies the `Sessionable` protocol and can
  be converted to a session. Optional `session-key` can be given when passing a
  request map."
  (^Session [^Sessionable src]
   (if-some [^Session smap (p/session src)]
     (if (and (.expired?      smap)
              (not   (.valid? smap))
              (nil?  (.id     smap))
              (some? (.err-id smap )))
       (map/qassoc smap :valid? true :id (.err-id smap))
       smap)))
  (^Session [^Sessionable src ^Keyword session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if (and (.expired?      smap)
              (not   (.valid? smap))
              (nil?  (.id     smap))
              (some? (.err-id smap )))
       (map/qassoc smap :valid? true :id (.err-id smap))
       smap))))

(defn allow-soft-expired
  "Temporarily mark soft-expired session as valid.  The `src` can be a session
  record (`Session`) or any object which satisfies the `Sessionable` protocol and can
  be converted to a session. Optional `session-key` can be given when passing a
  request map."
  (^Session [^Sessionable src]
   (if-some [^Session smap (p/session src session-key)]
     (if (.hard-expired? smap) smap (allow-expired smap))))
  (^Session [^Sessionable src ^Keyword session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if (.hard-expired? smap) smap (allow-expired smap)))))

(defn allow-hard-expired
  "Temporarily mark hard-expired session as valid."
  (^Session [^Sessionable src]
   (if-some [^Session smap (p/session src)]
     (if (.hard-expired? smap) (allow-expired smap) smap)))
  (^Session [^Sessionable src ^Keyword session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if (.hard-expired? smap) (allow-expired smap) smap))))

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
  [^SessionConfig opts ^DataSource db ^String table ^String sid-db ^IPAddress remote-ip]
  (sql/get-by-id db table sid-db db/opts-simple-map))

(defn get-last-active
  "Returns last active time of a session identified with `sid-db` and selected from a
  database `db`."
  ^Instant [^SessionConfig opts ^DataSource db ^String table ^String sid-db ^IPAddress remote-ip]
  (first (jdbc/execute-one! db
                            [(str "SELECT active FROM " table " WHERE id = ?") sid-db]
                            db/opts-simple-vec)))

(defn update-last-active
  "Updates session last active time on a database `db`. Returns the number of updated
  rows in case of success."
  (^Long [^SessionConfig opts ^DataSource db ^String table ^String sid-db ^IPAddress remote-ip]
   (::jdbc/update-count
    (sql/update! db table {:active (t/now)} {:id sid-db} db/opts-simple-map)))
  (^Long [^SessionConfig opts ^DataSource db table sid-db remote-ip t]
   (::jdbc/update-count
    (sql/update! db table {:active (t/instant t)} {:id sid-db} db/opts-simple-map))))

(defn set-session
  "Puts session data into a database `db`. Returns the number of updated rows in case
  of success."
  ^Long [^SessionConfig opts ^DataSource db ^String table ^Session smap]
  (::jdbc/update-count
   (db/replace! db table
                (-> smap
                    (set/rename-keys {:db-id :id :db-token :secure-token})
                    (select-keys [:user-id :user-email :secure-token :id :ip :active :created]))
                db/opts-simple-map)))

(defn delete-user-vars
  "Deletes session variables which belong to a user `user-id` from a database
  `db`. Returns the number of affected rows in case of success."
  [^SessionConfig opts ^DataSource db ^String sessions-table ^String variables-table ^Long user-id]
  (::jdbc/update-count
   (jdbc/execute-one! db [(str-spc "DELETE FROM" variables-table
                                   "WHERE EXISTS (SELECT 1 FROM" sessions-table
                                   (str "WHERE " sessions-table ".user_id = ?")
                                   (str "AND " variables-table ".session_id = " sessions-table ".id)"))
                          user-id])))

(defn delete-session-vars
  "Deletes session variables of a session identified with `db-sid` from a database
  `db`. Returns the number of affected rows in case of success."
  [^SessionConfig opts ^DataSource db ^String sessions-table ^String variables-table ^String db-sid]
  (::jdbc/update-count
   (jdbc/execute-one! db [(str-spc "DELETE FROM" variables-table
                                   "WHERE EXISTS (SELECT 1 FROM" sessions-table
                                   (str "WHERE " sessions-table ".id = ?")
                                   (str "AND " variables-table ".session_id = " sessions-table ".id)"))
                          db-sid])))

(defn delete-session-by-id
  "Deletes session identified with `db-sid` from a database `db`. Returns a sequence a
  map indicating deleted session properties, having keys `:id`, `:user-id`, `:user-email`
  and `:ip`."
  [^SessionConfig opts ^DataSource db ^String table ^String db-sid]
  (if-some [r (jdbc/execute-one! db [(str "DELETE FROM " table
                                          " WHERE id = ? RETURNING id,user_id,user_email,ip") db-sid]
                                 db/opts-simple-map)]
    (if r (dissoc r ::jdbc/update-count))))

(defn delete-sessions-by-uid
  "Deletes sessions belonging to a used `user-id` from a database `db`. Returns a
  sequence of maps indicating deleted session rows, having keys `:id`, `:user-id`,
  `:user-email` and `:ip`."
  [^SessionConfig opts ^DataSource db ^String table ^Long user-id]
  (if-some [r (jdbc/execute! db [(str "DELETE FROM " table " WHERE user_id = ?"
                                      " RETURNING id,user_id,user_email,ip") user-id]
                             db/opts-simple-map)]
    (if r
      (if (= 1 (count r))
        (map #(dissoc % ::jdbc/update-count) r)
        r))))

;; Marking

(defn mkgood
  "Marks the given session `smap` as valid by setting `:valid?` field to `true`,
  `:expired?` and `:hard-expired?` fields to `false`, and `:error` field to
  `nil`. The given object should be a session."
  (^Session [^Sessionable src ^Keyword session-key]
   (if-some [^Session smap (p/session src session-key)]
     (-> (if (.id smap) smap (map/qassoc smap :id (.err-id smap)))
         (map/qassoc :valid?        true
                     :expired?      false
                     :hard-expired? false
                     :err-id        nil
                     :error         nil))))
  (^Session [^Sessionable src]
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
  (^Session [^Session smap k v a b c d & pairs]
   (mkbad (apply map/qassoc smap k v a b c d pairs)))
  (^Session [^Session smap k v a b c d]
   (mkbad (map/qassoc smap k v a b c d)))
  (^Session [^Session smap k v a b]
   (mkbad (map/qassoc smap k v a b)))
  (^Session [^Session smap k v]
   (mkbad (map/qassoc smap k v)))
  (^Session [^Session smap]
   (if-some [^Session smap (p/session smap)]
     (let [^SessionError e (.error smap)
           have-error?     (instance? SessionError e)
           ^Keyword id     (.id e)
           id?             (some? id)
           expired?        (and id?
                                (or (identical? :session/expired id)
                                    (and (identical? :session/bad-ip id)
                                         (if-some [^SessionConfig cfg (p/config smap)]
                                           (.bad-ip-expires? cfg)))))
           h-expired?      (and expired? (p/hard-expired? smap))
           ^String err-id  (or (.id smap) (.err-id smap))
           ^SessionError e (if have-error?
                             (let [cause?           (some? (.cause e))
                                   ^SessionError er (if (some? (.severity e)) e
                                                        (map/qassoc e :severity :warn))]
                               (if id?
                                 (if cause?
                                   er
                                   (if (identical? id :session/unknown-error)
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
  ([^Sessionable src var-name]
   (del-var! src :session var-name))
  ([^Sessionable src ^Keyword session-key var-name]
   (if var-name
     (let [^Session smap  (p/session src)
           ^String db-sid (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot delete session variable" var-name
                  "because session ID is not valid" (for-user smap))
         (p/del-var (.control smap) db-sid var-name))))))

(defn del-vars!
  "Deletes a session variables from `var-names`."
  ([^Sessionable src var-names]
   (del-vars! src :session var-names))
  ([^Sessionable src ^Keyword session-key var-names]
   (if (not-empty var-names)
     (let [^Session smap  (p/session src session-key)
           ^String db-sid (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot delete session variable" (first var-names)
                  "because session ID is not valid" (for-user smap))
         (p/del-vars (.control smap) db-sid var-names))))))

(defn del-all-vars!
  "Deletes all session variables which belong to a user (is `single-session?`
  configuration option is `true`) or just variables for this session (if
  `single-session?` configuration option is `false`)."
  ([^Sessionable src ^Keyword session-key]
   (if-some [^Session smap (p/session src session-key)]
     (let [^SessionControl ctrl (.control smap)
           ^SessionConfig  opts (p/config ctrl)]
       (if (.single-session? opts)
         (if-some [^Long user-id (.user-id smap)]
           (p/del-uvars ctrl user-id)
           (log/err "Cannot delete session variables because user ID is not valid" (for-user smap)))
         (if-some [db-sid (db-sid-smap smap)]
           (p/del-svars ctrl db-sid)
           (log/err "Cannot delete session variables because session ID is not valid" (for-user smap)))))))
  ([^Sessionable src]
   (del-all-vars! src :session)))

(defn del-user-vars!
  "Deletes all session variables which belong to a user across all sessions."
  ([^Sessionable src ^Keyword session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if-some [user-id (.user-id smap)]
       (p/del-uvars (.control smap) user-id)
       (log/err "Cannot delete session variables because user ID is not valid" (for-user smap)))))
  ([^Sessionable src]
   (del-user-vars! src :session)))

(defn del-session-vars!
  "Deletes all session variables."
  ([^Sessionable src ^Keyword session-key]
   (if-some [^Session smap (p/session src session-key)]
     (if-some [db-sid (db-sid-smap smap)]
       (p/del-svars (.control smap) db-sid)
       (log/err "Cannot delete session variables because session ID is not valid" (for-user smap)))))
  ([^Sessionable src]
   (del-session-vars! src :session)))

(defn get-var
  "Gets a session variable and de-serializes it into a Clojure data structure."
  ([^Sessionable src var-name]
   (get-var src :session var-name))
  ([^Sessionable src ^Keyword session-key var-name]
   (if var-name
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot get session variable" var-name
                  "because session ID is not valid" (for-user smap))
         (p/get-var (.control smap) db-sid var-name))))))

(defn get-vars
  "Gets session variables and de-serializes them into a Clojure data structures."
  ([^Sessionable src var-names]
   (get-vars src :session var-names))
  ([^Sessionable src ^Keyword session-key var-names]
   (if (not-empty var-names)
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot get session variable" (first var-names)
                  "because session ID is not valid" (for-user smap))
         (p/get-vars (.control smap) db-sid var-names))))))

(defn fetch-var!
  "Like `get-var` but removes session variable after it is successfully read from a
  database. Variable is not removed if there was a problem with reading or
  de-serializing it."
  ([^Sessionable src var-name]
   (fetch-var! src :session var-name))
  ([^Sessionable src ^Keyword session-key var-name]
   (if var-name
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot get session variable" var-name
                  "because session ID is not valid" (for-user smap))
         (let [^SessionControl ctrl (.control smap)
               r                    (p/get-var ctrl db-sid var-name)]
           (if (not= ::db/get-failed r) (p/del-var ctrl db-sid var-name))
           r))))))

(defn fetch-vars!
  "Like `get-vars` but removes session variable after it is successfully read from a
  database. Variables are not removed if there was a problem with reading or
  de-serializing them."
  ([^Sessionable src var-names]
   (fetch-vars! src :session var-names))
  ([^Sessionable src ^Keyword session-key var-names]
   (if (not-empty var-names)
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot get session variable" (first var-names)
                  "because session ID is not valid" (for-user smap))
         (let [^SessionControl ctrl (.control smap)
               r                    (p/get-vars ctrl db-sid var-names)]
           (if (not= ::db/get-failed r) (p/del-vars ctrl db-sid var-names))
           r))))))

(defn put-var!
  "Puts a session variable `var-name` with a value `value` into a database."
  ([^Sessionable src var-name value]
   (put-var! src :session var-name value))
  ([^Sessionable src ^Keyword session-key var-name value]
   (if var-name
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot store session variable" var-name
                  "because session ID is not valid" (for-user smap))
         (p/put-var (.control smap) db-sid var-name value))))))

(defn put-vars!
  "Puts session variables with associated values (`var-names-values`) expressed as
  pairs into a database."
  ([^Sessionable src pairs]
   (put-vars! src :session pairs))
  ([^Sessionable src ^Keyword session-key pairs]
   (if (not-empty pairs)
     (let [^Session smap (p/session src session-key)
           db-sid        (db-sid-smap smap)]
       (if-not db-sid
         (log/err "Cannot store session variable"
                  (let [fp (first pairs)] (if (coll? fp) (first fp) fp))
                  "because session ID is not valid" (for-user smap))
         (p/put-vars (.control smap) db-sid pairs))))))

(defn get-variable-failed?
  "Returns `true` if the value `v` obtained from a session variable indicates that it
  actually could not be successfully fetched from a database."
  [v]
  (identical? ::db/get-failed v))

;; Cache invalidation.

(defn invalidate-cache!
  "Invalidates cache for the specific session. The `src` can be a session object, a
  request (context) map with optional `session-key` as a second argument, or a
  `SessionControl` object with a session ID (`sid`). Remote IP address (used to match
  cache entries along with session database ID) will be obtained from the session
  object if not given or `nil`."
  {:arglists '([^Sessionable src]
               [^Sessionable src ^Keyword session-key]
               [^Sessionable src ^Keyword session-key ^IPAddress remote-ip]
               [^SessionControl ctrl ^String sid ^IPAddress remote-ip])}
  ([^Sessionable src]
   (invalidate-cache! src :session nil))
  ([^Sessionable src ^Keyword session-key]
   (invalidate-cache! src session-key nil))
  ([^Sessionable src-or-ctrl session-key-or-sid ^IPAddress remote-ip]
   (if (control? src-or-ctrl)
     (p/invalidate src-or-ctrl (db-sid-str session-key-or-sid) remote-ip)
     (if-some [^Session smap (p/session src-or-ctrl session-key-or-sid)]
       (p/invalidate (.control smap) (db-sid-smap smap) (or remote-ip (.ip smap)))))))

;; Cache invalidation when time-sensitive value (last active time) exceeds TTL.

(defn- refresh-times-core
  [^Session smap ^SessionControl ctrl ^Duration cache-margin ^IPAddress remote-ip]
  (or (if cache-margin
        (if-some [last-active (.active smap)]
          (let [inactive-for (t/between last-active (t/now))]
            (when (t/> inactive-for cache-margin)
              (let [fresh-active  (p/get-active ctrl (db-sid-smap smap) remote-ip)
                    ^Session smap (if fresh-active (map/qassoc smap :active fresh-active) smap)
                    expired?      (p/expired? ctrl (or fresh-active last-active))
                    sid           (or (.id smap) (.err-id smap))]
                (if (and fresh-active (not= fresh-active last-active))
                  (db/mem-assoc-existing! (p/mem-handler ctrl) [sid remote-ip] :active fresh-active))
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
               [^Sessionable src ^Keyword session-key]
               [^Sessionable src ^IPAddress remote-ip]
               [^Sessionable src ^Keyword session-key ^IPAddress remote-ip])}
  (^Session [^Sessionable src]
   (if-some [^Session smap (p/session src)]
     (refresh-times-core smap
                         (.control smap)
                         (if-some [^SessionConfig cfg (p/config smap)] (.cache-margin cfg))
                         (.ip smap))))
  (^Session [^Sessionable src session-key-or-ip]
   (if (session? src)
     (refresh-times-core src
                         (.control src)
                         (if-some [^SessionConfig cfg (p/config src)] (.cache-margin cfg))
                         session-key-or-ip)
     (if-some [^Session smap (p/session src session-key-or-ip)]
       (refresh-times-core smap
                           (.control smap)
                           (if-some [^SessionConfig cfg (p/config smap)] (.cache-margin cfg))
                           (.ip smap)))))
  (^Session [^Sessionable src ^Keyword session-key ^IPAddress remote-ip]
   (if-some [^Session smap (p/session src session-key)]
     (refresh-times-core smap
                         (.control smap)
                         (if-some [^SessionConfig cfg (p/config smap)] (.cache-margin cfg))
                         remote-ip))))

;; Session handling, creation and prolongation

(defn handler
  "Gets session data from a database and processes them with session control functions
  and configuration options to generate a session record. When the record is created
  it is validated for errors and expiration times.

  The results of calling this function may be memoized to reduce database hits. Note
  that if the cache TTL is larger than the remaining expiration time for a session,
  it may be required to refresh the times and re-validate the session object."
  (^Session [^Sessionable src ^String sid ^String db-sid ^IPAddress remote-ip]
   (handler src :session sid db-sid remote-ip))
  (^Session [^Sessionable src ^Keyword session-key
             ^String sid ^String db-sid ^IPAddress remote-ip]
   (if-some [^SessionControl ctrl (p/control src session-key)]
     (let [^SessionConfig opts (p/config ^SessionControl ctrl)]
       (handler ctrl
                (.session-key opts)
                (.id-field    opts)
                sid db-sid remote-ip))))
  (^Session [^SessionControl ctrl ^Keyword session-key
             id-field ^String sid ^String db-sid ^IPAddress remote-ip]
   (let [pass               (if (not= sid db-sid) (second (split-secure-sid sid)))
         secure?            (some? (not-empty pass))
         smap-db            (p/from-db ctrl db-sid remote-ip)
         token-ok?          (if secure? (p/token-ok? ctrl pass (get smap-db :secure-token)) false)
         ^Session smap      (map/qassoc
                             (map->Session (dissoc smap-db :secure-token))
                             :id               sid
                             :db-id            db-sid
                             :ip               (ip/to-address (get smap-db :ip))
                             :secure?          secure?
                             :prolonged?       false
                             :security-passed? token-ok?
                             :control          ctrl
                             :session-key      session-key
                             :id-field         id-field)
         ^SessionError stat (state smap remote-ip)]
     (if (nil? stat)
       (mkgood smap)
       (mkbad  smap :error stat)))))

(defn- needs-refresh?
  ^Boolean [^SessionControl ctrl key ^Duration cache-margin
            ^Instant last-active ^Duration expires-in ^Boolean expired?]
  (and (some? cache-margin) (some? last-active)
       (let [^Instant now (t/now)]
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
  ^Session [^SessionControl ctrl ^Duration cache-margin ^Duration expires-in ^String sid ^IPAddress remote-ip]
  (let [^String db-sid      (db-sid-str sid)
        ^Session smap       (p/handle ctrl sid db-sid remote-ip)
        memo-args           [db-sid remote-ip]
        active              (.active   smap)
        expired?            (.expired? smap)
        needs-refresh?      (needs-refresh? ctrl memo-args cache-margin active expires-in expired?)
        new-expired?        (and needs-refresh? (not expired?) (p/expired? smap))
        ^Instant new-active (when (and needs-refresh? (not new-expired?))
                              (log/dbg "Getting last active time from a database for" db-sid)
                              (if-some [t (p/get-active ctrl db-sid remote-ip)]
                                (if (= t active) nil t)))]
    (if (nil? new-active)
      (if new-expired?
        (do (log/dbg "Session expiry detected after recalculating times for" db-sid)
            (p/invalidate ctrl db-sid remote-ip)
            (mkbad smap :error (state smap remote-ip)))
        smap)
      (let [new-expired? (p/expired? ctrl new-active)]
        (cond

          ;; no change in expiration status after getting active time from a db
          ;; (update active time in cache and in current session)

          (= expired? new-expired?)
          (do (log/dbg "Updating active time of" db-sid)
              (db/mem-assoc-existing! (p/mem-handler ctrl) memo-args :active new-active)
              (map/qassoc smap :active new-active))

          ;; (was) expired -> (is) not expired
          ;; (possible cross-node change, clear the cache and re-run handling)

          expired?
          (do (log/dbg "Session no longer expired, re-running handler for" db-sid)
              (p/invalidate ctrl db-sid remote-ip)
              (p/handle ctrl sid db-sid remote-ip))

          ;; (was) not expired -> (is) expired
          ;; (duration margin, clear the cache, re-run validation and mark session as bad)

          new-expired?
          (let [^Session smap (map/qassoc smap :active new-active)]
            (log/dbg "Session expired after syncing last active time for" db-sid)
            (p/invalidate ctrl db-sid remote-ip)
            (mkbad smap :error (state smap remote-ip))))))))

(defn process
  "Takes a session control object, predefined session objects (for malformed and empty
  session), expiration settings and a request map, and validates a session against
  the database or memoized session data. Returns a session map or dummy session map
  if session was not obtained (session ID was not found in a database)."
  ^Session [^SessionControl ctrl
            ^Session        malformed-session
            ^Session        empty-session
            ^Duration       cache-margin
            ^Duration       expires-in
            req]
  (if-some [^String sid (p/identify ctrl req)]
    (let [^IPAddress remote-ip (ip/to-address (get req :remote-ip))]
      (if-not (sid-valid? sid)
        (mkbad malformed-session :id sid :ip remote-ip)
        (let [^Session smap (process-handler ctrl cache-margin expires-in sid remote-ip)]
          (if-not (.valid? smap)
            smap
            (if (pos-int? (p/set-active ctrl (db-sid-smap smap) remote-ip))
              (mkgood smap)
              (mkbad smap :error (SessionError. :error :session/db-problem
                                                (some-str-spc
                                                 "Problem updating session data"
                                                 (for-user
                                                  (.user-id    smap)
                                                  (.user-email smap)
                                                  (or (.ip smap) (get req :remote-ip/str)))))))))))
    empty-session))

(defn prolong
  "Re-validates session by updating its timestamp and re-running validation."
  ([^Sessionable src]
   (prolong src nil nil))
  ([^Sessionable src ^IPAddress ip-address]
   (prolong src nil ip-address))
  ([^Sessionable src ^Keyword session-key ^IPAddress ip-address]
   (if-some [^Session smap (p/session src session-key)]
     (if-some [sid (or (.err-id smap) (.id smap))]
       (let [^SessionControl  ctrl (.control smap)
             ^IPAddress ip-address (ip/to-address (or ip-address (.ip smap)))
             ^Instant     new-time (t/now)]
         (log/msg "Prolonging session" (for-user (.user-id smap) (.user-email smap) ip-address))
         (let [^Session      new-smap (map/qassoc smap :id sid :active new-time)
               ^SessionError stat     (state new-smap ip-address)
               ^String       db-sid   (or (db-sid-smap new-smap) (db-sid-str sid))]
           (if (correct-state? stat)
             (do  (p/set-active ctrl db-sid ip-address new-time)
                  (p/invalidate ctrl db-sid ip-address)
                  (if (not= ip-address (.ip smap)) (p/invalidate ctrl db-sid (.ip smap)))
                  (let [^Session smap (p/handle ctrl sid db-sid ip-address)]
                    (if (.valid? smap)
                      (map/qassoc smap :prolonged? true)
                      smap)))
             (do (log/wrn "Session re-validation error"
                          (for-user (.user-id smap) (.user-email smap) ip-address))
                 (mkbad smap :error stat)))))))))

(defn create
  "Creates a new session and puts it into a database. Returns a session record."
  (^Session [^SessionControl src ^Long user-id ^String user-email ^IPAddress ip-address]
   (let [^SessionControl ctrl (p/control src)
         ^Long   user-id      (valuable user-id)
         ^String user-email   (some-str user-email)
         ids?                 (and user-id user-email)]
     (cond
       (not ctrl) (do (log/err "Session control object was not found.") nil)
       (not ids?) (do (if-not user-id    (log/err "No user ID given when creating a session"))
                      (if-not user-email (log/err "No user e-mail given when creating a session")) nil)
       :ok
       (let [^IPAddress       ip (ip/to-address ip-address)
             ^SessionConfig opts (p/config ^SessionControl ctrl)
             secured?            (.secured?        opts)
             id-field            (or (.id-field    opts) "session-id")
             ^Keyword skey       (or (.session-key opts) :session)
             ^Session sess       (make secured? ctrl skey id-field user-id user-email ip)
             stat                (state sess ip)]
         (log/msg "Opening session" (for-user user-id user-email ip))
         (if-not (correct-state? stat)
           (do (log/err "Session incorrect after creation" (for-user user-id user-email ip))
               (mkbad sess :error stat))
           (let [updated-count  (p/to-db ctrl sess)
                 ^Session  sess (map/qassoc sess :db-token nil)
                 ^String db-sid (db-sid-smap sess)]
             (p/invalidate ctrl db-sid ip)
             (if (pos-int? updated-count)
               (do (if (.single-session? opts)
                     (p/del-uvars ctrl user-id)
                     (p/del-svars ctrl db-sid))
                   (mkgood sess))
               (do (log/err "Problem saving session" (for-user user-id user-email ip))
                   (mkbad sess
                          :error (SessionError. :error :session/db-problem
                                                (str "Session cannot be saved"
                                                     (for-user user-id user-email ip)))))))))))))

;; Helpers

(defn config+session
  "Gets a session map and a session config map from the given request map. Returns a
  two-element vector."
  ([req]
   (config+session req :session))
  ([req session-key]
   (if-some [^Session s (of req session-key)]
     [(or (config s) (config req session-key)) s]
     [nil nil])))

(defn get-session-id-header
  "For the given request map `req` it tries to get a request header identified by a
  name `id-field`, and then checks if it is valid session identifier. Returns a
  session identifier or `nil` if the obtained value is not valid session ID or the
  header is not found."
  [req id-field]
  (let [sid (get (get req :headers) id-field)]
    (when (sid-valid? sid) sid)))

(defn add-session-id-header
  "Adds session ID header to a map under the `response-headers-key` (defaults to
  `:response/headers`) key of the given `req` map. Name of the header is obtained
  from session ID field (by calling `id-field`) and its value is set to session
  ID (obtained by calling `any-id`). If the header already exists it is not added."
  ([req sess]
   (add-session-id-header req sess false :response/headers))
  ([req sess replace?]
   (add-session-id-header req sess replace? :response/headers))
  ([req sess replace? response-headers-key]
   (if sess
     (if-some [id-field (id-field sess)]
       (if-some [sid (any-id sess)]
         (let [response-headers-key (or response-headers-key :response-headers)
               headers              (get req response-headers-key)]
           (if (pos? (count headers))
             (if (or replace? (not (contains? headers id-field)))
               (qassoc req response-headers-key (qassoc headers id-field sid))
               req)
             (qassoc req response-headers-key {id-field sid})))
         req)
       req)
     req)))

(defn replace-session-id-header
  "Adds session ID header to the `:response/headers` map of the given `req` map. Name
  of the header is obtained from session ID field (by calling `id-field`) and its value
  is set to session ID (obtained by calling `any-id`). If the header already exists
  it is replaced."
  [req sess]
  (add-session-id-header req sess true))

(defn reflect-session-id-header
  "Adds session ID header to a map under the `response-headers-key` (defaults to
  `:response/headers`) key of the given `req` map. Name of the header is obtained
  from session ID field (by calling `id-field`) and its value is set to session
  ID (obtained by calling `any-id`, and if that fails by getting the value of client
  request header with the same name using `get-session-id-header`).  If the header
  already exists it is not added."
  ([req sess]
   (reflect-session-id-header req sess :response/headers))
  ([req sess response-headers-key]
   (if sess
     (if-some [id-field (id-field sess)]
       (if-some [sid (or (any-id sess) (get-session-id-header req id-field))]
         (let [response-headers-key (or response-headers-key :response/headers)
               headers              (get req response-headers-key)]
           (if (pos? (count headers))
             (if (contains? headers id-field)
               req
               (qassoc req response-headers-key (qassoc headers id-field sid)))
             (qassoc req response-headers-key {id-field sid})))
         req)
       req)
     req)))

(defn empty-session-id-header
  "Adds session ID header to a map under the `response-headers-key` (defaults to
  `:response/headers`) key of the given `req` map. Name of the header is obtained
  from session ID field (by calling `id-field`). If the header already exists it is
  replaced. The content is set to a string consisting of a single minus character."
  ([req sess]
   (empty-session-id-header req sess :response/headers))
  ([req sess response-headers-key]
   (if-some [id-field (id-field sess)]
     (let [headers (get req response-headers-key)]
       (if (pos? (count headers))
         (qassoc req response-headers-key (qassoc headers id-field "-"))
         (qassoc req response-headers-key {id-field "-"})))
     req)))

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
  ^Duration [^SessionConfig config]
  (let [^Duration expires   (get config :expires)
        ^Duration cache-ttl (get config :cache-ttl)]
    (map/qassoc config :cache-margin
                (if (and expires cache-ttl
                         (pos? (t/seconds cache-ttl))
                         (pos? (t/seconds expires)))
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
        ^Keyword session-key     (get cfg :session-key)
        ^String sessions-table   (get cfg :sessions-table)
        ^String variables-table  (get cfg :variables-table)
        session-id-path          (get cfg :id-path)
        session-id-field         (get cfg :id-field)
        ^Duration cache-margin   (get cfg :cache-margin)
        ^Boolean single-session? (get cfg :single-session?)
        ^Boolean secured?        (get cfg :secured?)
        ^Duration expires        (get cfg :expires)
        ^Duration hard-expires   (get cfg :hard-expires)
        session-id-field         (or session-id-field (if (coll? session-id-path) (last session-id-path) session-id-path))
        session-id-field         (or (some-str session-id-field) "session-id")
        ^SessionConfig cfg       (assoc cfg :id-field session-id-field)
        expirer-fn               (if (pos-int? (time/seconds expires)) #(calc-expired-core expires %1) (constantly false))
        expirer-hard-fn          (if (pos-int? (time/seconds hard-expires)) #(calc-expired-core hard-expires %1) (constantly false))
        identifier-fn            (setup-id-fn session-id-path)
        ^SessionConfig cfg       (assoc cfg :fn/identifier identifier-fn)
        getter-fn                (setup-fn cfg :fn/getter get-session-by-id)
        getter-fn-w              #(getter-fn cfg db sessions-table %1 %2)
        ^SessionConfig cfg       (assoc cfg :fn/getter getter-fn-w)
        checker-config           (set/rename-keys cfg {:token-cache-size :cache-size :token-cache-ttl :cache-ttl})
        checker-fn               (setup-fn cfg :fn/checker check-encrypted)
        checker-fn-w             (db/memoizer checker-fn checker-config)
        ^SessionConfig cfg       (assoc cfg :fn/checker checker-fn-w)
        pre-handler              ^{::mem/args-fn nnext} #(handler %1 session-key session-id-field %2 %3 %4)
        mem-handler              (db/memoizer pre-handler cfg)
        handler-fn-w             mem-handler
        mem-atom                 (get-mem-atom mem-handler)
        last-active-fn           (setup-fn cfg :fn/last-active get-last-active)
        update-active-fn         (setup-fn cfg :fn/update-active update-last-active)
        last-active-fn-w         #(last-active-fn cfg db sessions-table %1 %2)
        ^SessionConfig cfg       (assoc cfg :fn/last-active last-active-fn-w)
        update-active-fn-w       (fn
                                   (^Long [db-sid remote-ip]
                                    (let [t (t/now)]
                                      (db/mem-assoc-existing! mem-handler [db-sid remote-ip] :active t)
                                      (update-active-fn cfg db sessions-table db-sid remote-ip t)))
                                   (^Long [db-sid remote-ip t]
                                    (db/mem-assoc-existing! mem-handler [db-sid remote-ip] :active t)
                                    (update-active-fn cfg db sessions-table db-sid remote-ip t)))
        ^SessionConfig cfg       (assoc cfg :fn/update-active update-active-fn-w)
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
        delete-session-fn        (setup-fn cfg :fn/del-session  delete-session-by-id)
        delete-sessions-fn       (setup-fn cfg :fn/del-sessions delete-sessions-by-uid)
        delete-session-fn-w      #(delete-session-fn  cfg db sessions-table %)
        delete-sessions-fn-w     #(delete-sessions-fn cfg db sessions-table %)
        ^SessionConfig cfg       (assoc cfg
                                        :fn/get-var       var-get-fn
                                        :fn/get-vars      vars-get-fn
                                        :fn/put-var       var-put-fn
                                        :fn/put-vars      vars-put-fn
                                        :fn/del-var       var-del-fn
                                        :fn/del-vars      vars-del-fn
                                        :fn/del-user-vars vars-del-user-fn-w
                                        :fn/del-sess-vars vars-del-sess-fn-w
                                        :fn/del-session   delete-session-fn-w
                                        :fn/del-sessions  delete-sessions-fn-w)
        setter-fn                (setup-fn cfg :fn/setter set-session)
        setter-fn-w              #(setter-fn cfg db sessions-table %)
        ^SessionConfig cfg       (assoc cfg :fn/setter setter-fn-w)
        invalidator-fn           (setup-invalidator pre-handler mem-handler)
        ^SessionConfig cfg       (assoc cfg :fn/invalidator invalidator-fn :fn/handler mem-handler)
        prolong-fn               prolong
        ^Session empty-sess      (Session. nil nil nil nil nil nil nil nil nil
                                           false false false false false false
                                           session-key session-id-field
                                           nil nil)
        ^SessionConfig  cfg      (assoc cfg :fn/prolong prolong-fn)
        ^SessionControl ctrl     (reify p/SessionControl
                                   (config        ^SessionConfig [_]         cfg)
                                   (empty         ^Session [c]               (map/qassoc empty-sess :control c))
                                   (expired?      ^Boolean [_ t]             (expirer-fn t))
                                   (hard-expired? ^Boolean [_ t]             (expirer-hard-fn t))
                                   (token-ok?     ^Boolean [_ plain enc]     (checker-fn-w plain enc))
                                   (from-db       ^Session [_ db-sid ip]     (getter-fn-w db-sid ip))
                                   (handle        ^Session [c sid ip]        (handler-fn-w c sid (db-sid-str sid) ip))
                                   (handle        ^Session [c sid db-sid ip] (handler-fn-w c sid db-sid ip))
                                   (to-db         ^Long    [_ smap]          (setter-fn-w smap))
                                   (set-active    ^Long    [_ db-sid ip]     (update-active-fn-w db-sid ip))
                                   (set-active    ^Long    [_ db-sid ip t]   (update-active-fn-w db-sid ip t))
                                   (get-active    ^Instant [_ db-sid ip]     (last-active-fn-w   db-sid ip))
                                   (identify      ^String  [_ req]           (identifier-fn req))
                                   (control?      ^Boolean [_]   true)
                                   (control?      ^Boolean [_ k] true)
                                   (mem-handler   [_]            mem-handler)
                                   (mem-atom      [_]            mem-atom)
                                   (mem-cache     [_]            (if mem-atom (deref mem-atom)))
                                   (invalidate    [_ db-sid ip]  (invalidator-fn db-sid ip))
                                   (put-var       [_ db-sid k v] (var-put-fn  db db-sid k v))
                                   (get-var       [_ db-sid k]   (var-get-fn  db db-sid k))
                                   (del-var       [_ db-sid k]   (var-del-fn  db db-sid k))
                                   (put-vars      [_ db-sid kvs] (vars-put-fn db db-sid kvs))
                                   (get-vars      [_ db-sid ks]  (vars-get-fn db db-sid ks))
                                   (del-vars      [_ db-sid ks]  (var-del-fn  db db-sid ks))
                                   (del-svars     [_ db-sid]     (vars-del-sess-fn-w db-sid))
                                   (del-uvars     [_ user-id]    (vars-del-user-fn-w user-id))
                                   (del-session   [_ db-sid]     (delete-session-fn-w  db-sid))
                                   (del-sessions  [_ user-id]    (delete-sessions-fn-w user-id)))
        ^Session empty-sess      (p/empty ctrl)
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
     :control ctrl
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
