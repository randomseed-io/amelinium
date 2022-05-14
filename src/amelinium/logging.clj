(ns

    ^{:doc    "Logging support for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.logging

  (:require [potemkin.namespaces             :as                 p]
            [amelinium.system                :as            system]
            [buddy.core.hash                 :as              hash]
            [buddy.core.codecs               :as            codecs]
            [io.randomseed.utils.map         :as               map]
            [io.randomseed.utils.var         :as               var]
            [io.randomseed.utils.log         :as               log]
            [io.randomseed.utils             :refer [some-str-spc]])

  (:import  [logback_bundle.json                 FlatJsonLayout ValueDecoder]
            [ch.qos.logback.contrib.jackson      JacksonJsonFormatter]
            [ch.qos.logback.core.encoder         LayoutWrappingEncoder]
            [ch.qos.logback.contrib.json.classic JsonLayout]
            [ch.qos.logback.classic.filter       ThresholdFilter]
            [ch.qos.logback.classic.encoder      PatternLayoutEncoder]
            [ch.qos.logback.core                 ConsoleAppender]
            [java.nio.charset                    Charset]))

(def ^:dynamic *pseudo-salt* "98jj348jvj28ncJIJ21398")

(def ^:dynamic *already-logged* false)

;;
;; Logging wrappers
;;

(p/import-vars [io.randomseed.utils.log
                default-config log-context with-ctx
                log trace debug info warn warning error fatal
                msg-with-val err-with-val msg err wrn dbg
                log-exceptions])

;;
;; Context processing
;;

(defn mask         [_] "************")
(defn pseudonimize [v] (-> (hash/md5 (str v *pseudo-salt*)) (codecs/bytes->hex)))
(defn pr-session   [v] (if-not (map? v) v (assoc v :data "--------------" :api  "--------------")))

(def ctx-transformer
  {mask         [:password :pwd :private-key :private :secret :signature :request-id :anti-phisihng-code]
   pseudonimize [:user :username :nick :nickname]
   str          [:currency]
   pr-session   [:session]})

;;
;; Logging helpers
;;

(defn id-email
  ([user-id user-email]
   (some-str-spc user-email
                 (when user-id (str "(" user-id ")"))))
  ([user-id user-email ip-addr]
   (some-str-spc user-email
                 (when user-id (str "(" user-id ")"))
                 (when ip-addr (str "[" ip-addr "]")))))

(defn for-user
  ([user-id user-email]
   (when (or user-id user-email)
     (str "for " (id-email user-id user-email))))
  ([user-id user-email ip-addr]
   (when (or user-id user-email ip-addr)
     (if (or user-id user-email)
       (str "for " (id-email user-id user-email ip-addr))
       (str "for [" ip-addr "]")))))

;;
;; System handlers
;;

(defn prep-context-transformer
  [m]
  (when m
    (map/map-keys var/deref-symbol m)))

(system/add-prep
 ::unilog [_ config]
 (log/preprocess-config
  (map/update-existing config :context-transformer prep-context-transformer)))

(system/add-init
 ::unilog
 [_ config]
 (log/init! (-> config
                (map/update-existing :context-transformer prep-context-transformer)
                (map/assoc-missing   :context-transformer ctx-transformer)))
 (msg-with-val "Configuration profile:" (:profile (:system config)) config))

(system/add-halt!
 ::unilog
 [_ config]
 (log/stop! config))
