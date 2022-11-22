(ns

    ^{:doc    "amelinium service, authorization record types."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.types.auth

  (:require [phone-number.core])
  (:import  [phone_number.core Phoneable]
            [javax.sql DataSource]
            [java.time Duration]
            [reitit.core Match]))

(defrecord AccountTypes     [^String                        sql
                             ^clojure.lang.PersistentVector ids
                             ^clojure.lang.PersistentVector names
                             ^clojure.lang.Keyword          default
                             ^String                        default-name])

(defrecord AuthLocking      [^Long                max-attempts
                             ^Duration            lock-wait
                             ^Duration            fail-expires])

(defrecord AuthConfirmation [^Long                max-attempts
                             ^Duration            expires])

(defrecord AuthPasswords    [^clojure.lang.Keyword id
                             ^clojure.lang.ISeq    suite
                             ^clojure.lang.Fn      check
                             ^clojure.lang.Fn      check-json
                             ^clojure.lang.Fn      encrypt
                             ^clojure.lang.Fn      encrypt-json
                             ^clojure.lang.Fn      wait])

(defrecord AuthConfig       [^clojure.lang.Keyword id
                             ^DataSource           db
                             ^AccountTypes         account-types
                             ^AccountTypes         parent-account-types
                             ^AuthConfirmation     confirmation
                             ^AuthLocking          locking
                             ^AuthPasswords        passwords])

(defrecord AuthSettings     [^DataSource                  db
                             ^clojure.lang.Keyword        default-type
                             ^AuthConfig                  default
                             ^clojure.lang.IPersistentMap types])

;; DBPassword record is to pass intrinsic suite in JSON format and shared suite ID.
;; It's used to pass data around.

(defrecord DBPassword [^Long   password-suite-id
                       ^String password])

;; UserData record is to pass user data between models and controllers
;; in a bit faster way than with regular maps.

(defrecord UserData   [^String               email
                       ^Phoneable            phone
                       ^clojure.lang.Keyword account-type
                       ^AuthConfig           auth-config
                       ^DataSource           db
                       ^String               password
                       ^String               password-shared
                       ^Long                 password-suite-id
                       ^String               first-name
                       ^String               middle-name
                       ^String               last-name
                       ^Duration             expires-in
                       ^Long                 max-attempts])

;; AuthQueries record is used to pass a set of SQL queries in some structured form.

(defrecord AuthQueries [^String generic
                        ^String pre
                        ^String post
                        ^String single])

;; Password types

(defrecord Suites      [^clojure.lang.IPersistentMap shared
                        ^clojure.lang.IPersistentMap intrinsic])

(defrecord SuitesJSON  [^String shared
                        ^String intrinsic])
