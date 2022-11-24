(ns

    ^{:doc    "amelinium service, session protocols."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.proto.session

  (:refer-clojure :exclude [empty empty?])

  (:require [amelinium] [amelinium.types.session])
  (:import  [amelinium            Session SessionError SessionConfig]
            [clojure.core.memoize PluggableMemoization]
            [java.time            Instant]))

(defprotocol SessionControl
  "This protocol promises access to session configuration data and basic actions which
  are configuration-dependent. The operations should keep access to settings and/or
  dynamically generated functions using lexical closures. Therefore, this protocol
  should later be reified, after settings are parsed, and the anonymous object with
  implementations should be stored in all created `Session` records, in their
  `:control` fields."

  (^{:tag Boolean}
   empty
   [c] [c s-k]
   "Returns an empty session with the `:control` field populated.")

  (^{:tag SessionConfig}
   config
   [c] [c s-k]
   "Gets a session configuration settings.")

  (^{:tag clojure.lang.Fn}
   mem-handler
   [c] [c s-k]
   "Returns memoized handler function used by `handle`.")

  (^{:tag Boolean}
   expired?
   [c] [c t]
   "Returns `true` if expiration time was exceeded for `t`.")

  (^{:tag Boolean}
   hard-expired?
   [c] [c t]
   "Returns `true` if hard-expiration time was exceeded for `t`.")

  (^{:tag String}
   identify
   [c req] [c]
   "Extracts session ID.")

  (^{:tag Session}
   handle
   [c sid ip] [c sid] [c]
   "Obtains a session from a database and creates its object.")

  (invalidate
    [c sid ip] [c sid] [c]
    "Invalidates internal cache.")

  (^{:tag Session}
   from-db
   [c db-sid ip] [c db-sid] [c]
   "Gets the session data from a database.")

  (to-db
    [c smap]
    "Puts the session data into a database.")

  (^{:tag clojure.lang.IRef}
   mem-atom
   [c]
   "Returns an Atom object keeping reference to a cache object associated with memoized
  session handler.")

  (^{:tag PluggableMemoization}
   mem-cache
   [c]
   "Returns a cache object associated with memoized session handler.")

  (^{:tag Instant}
   get-active
   [c db-sid ip] [c db-sid] [c]
   "Gets the session last active time from a database.")

  (set-active
    [c sid db-sid ip t] [c sid db-sid ip] [s ip t] [s ip] [s]
    "Sets the session last active time in a database.")

  (^{:tag Boolean}
   token-ok?
   [c plain enc]
   "Checks if the security token is valid.")

  (get-var
    [c db-sid k] [c k]
    "Gets session variable from a persistent storage.")

  (^{:tag clojure.lang.ISeq}
   get-vars
   [c db-sid ks] [c ks]
   "Gets session variables from a persistent storage.")

  (put-var
    [c db-sid k v] [c k v]
    "Puts session variable into a persistent storage.")

  (put-vars
    [c db-sid kvs] [c kvs]
    "Puts session variables into a persistent storage.")

  (del-var
    [c db-sid k] [c k]
    "Deletes session variable from a persistent storage.")

  (del-vars
    [c db-sid ks] [c ks]
    "Deletes session variables from a persistent storage.")

  (del-svars
    [c db-sid] [c]
    "Deletes all session variables from a persistent storage.")

  (del-uvars
    [c uid] [c]
    "Deletes all user's session variables from a persistent storage."))

(defprotocol Sessionable
  "This protocol is used to access session data."

  (^{:tag amelinium.Session}
   session
   [src] [src session-key]
   "Returns a session record of type `Session` on a basis of configuration source
  provided and an optional `session-key` if session must be looked in an associative
  structure (defaults to `:session`).")

  (^{:tag Boolean}
   empty?
   [src] [src session-key]
   "Returns `false` is `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` can be given to express a key in associative
  structure (defaults to `:session`).")

  (^{:tag amelinium.proto.session.Sessionable}
   inject
   [dst smap] [dst smap session-key]
   "Returns an object updated with session record of type `Session` under an optional
  `session-key` if session is to be put into an associative structure (defaults to
  `:session`).")

  (^{:tag amelinium.proto.session.SessionControl}
   control
   [src] [src session-key]
   "Returns a session control object (satisfying the `SessionControl` protocol) used to
  reach session configuration and internal operations."))
