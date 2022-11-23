(ns

    ^{:doc    "amelinium service, session record types."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.types.session

  (:import [java.time Instant Duration]
           [javax.sql DataSource]
           [inet.ipaddr IPAddress]))

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
     ^Long                          token-cache-size
     ^Duration                      cache-margin
     ^Boolean                       single-session?
     ^Boolean                       secured?
     ^Boolean                       bad-ip-expires?])

(defrecord SessionError
    [^clojure.lang.Keyword          severity
     ^clojure.lang.Keyword          id
     ^String                        cause])

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
     ^Boolean                       prolonged?
     ^Boolean                       secure?
     ^Boolean                       security-passed?
     ^clojure.lang.Keyword          session-key
     ^Object                        id-field
     ^SessionError                  error
     control])
