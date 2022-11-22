(ns

    ^{:doc    "amelinium service, database record types."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.types.db

  (:import [java.sql Connection]
           [javax.sql DataSource]))

(defrecord DBConfig [^clojure.lang.Fn      initializer
                     ^clojure.lang.Fn      finalizer
                     ^clojure.lang.Fn      suspender
                     ^clojure.lang.Fn      resumer
                     ^clojure.lang.Keyword dbkey
                     ^String               dbname
                     ^DataSource           datasource])
