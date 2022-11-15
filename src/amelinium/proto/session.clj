(ns

    ^{:doc    "amelinium service, session protocols."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.proto.session)

(defprotocol SessionControl
  "This protocol promises access to session configuration data and basic actions which
  are configuration-dependent. The operations should keep access to settings and/or
  dynamically generated functions using lexical closures. Therefore, this protocol
  should later be reified, after settings are parsed, and the anonymous object with
  implementations should be stored in all created `Session` records, in their
  `:control` fields."
  (config      [c] [c s-k]                    "Gets a session configuration settings.")
  (identify    [c req] [c]                    "Extracts session ID.")
  (handle      [c sid ip]      [c sid]    [c] "Obtains a session from a database and creates its object.")
  (invalidate  [c sid ip]      [c sid]    [c] "Invalidates internal cache.")
  (from-db     [c db-sid ip]   [c db-sid] [c] "Gets the session data from a database.")
  (to-db       [c smap]                       "Puts the session data into a database.")
  (get-active  [c db-sid ip]   [c db-sid] [c] "Gets the session last active time from a database.")
  (set-active  [c db-sid ip t] [c db-sid ip] [c db-sid] [c] "Sets the session last active time in a database.")
  (token-ok?   [c plain enc]                 "Checks if the security token is valid.")
  (get-var     [c db-sid k]    [c k]          "Gets session variable from a persistent storage.")
  (get-vars    [c db-sid ks]   [c ks]         "Gets session variables from a persistent storage.")
  (put-var     [c db-sid k v]  [c k v]        "Puts session variable into a persistent storage.")
  (put-vars    [c db-sid kvs]  [c kvs]        "Puts session variables into a persistent storage.")
  (del-var     [c db-sid k]    [c k]          "Deletes session variable from a persistent storage.")
  (del-vars    [c db-sid ks]   [c ks]         "Deletes session variables from a persistent storage.")
  (del-svars   [c db-sid]      [c]            "Deletes all session variables from a persistent storage.")
  (del-uvars   [c uid]         [c]            "Deletes all user's session variables from a persistent storage."))

(defprotocol Sessionable
  "This protocol is used to access session data."

  (session
    [src] [src session-key]
    "Returns a session record of type `Session` on a basis of configuration source
  provided and an optional `session-key` if session must be looked in an associative
  structure (defaults to `:session`).")

  (empty?
    [src] [src session-key]
    "Returns `false` is `src` contains a session or is a session, and the session has
  usable identifier set (`:id` or `:err-id` field is set) or has the `:error` field
  set. Optional `session-key` can be given to express a key in associative
  structure (defaults to `:session`).")

  (inject
    [dst smap] [dst smap session-key]
    "Returns an object updated with session record of type `Session` under an optional
  `session-key` if session is to be put into an associative structure (defaults to
  `:session`).")

  (control
    [src] [src session-key]
    "Returns a session control object (satisfying the `SessionControl` protocol) used to
  reach session configuration and internal operations."))
