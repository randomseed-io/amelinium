(ns

    ^{:doc    "amelinium service, Jetty HTTP server support."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.server.jetty

  (:require  [ring.adapter.jetty           :as      jetty]
             [amelinium.server.ssl         :as        ssl]
             [amelinium.system             :as     system]
             [amelinium.logging            :as        log]
             [io.randomseed.utils.vec      :as        vec]
             [io.randomseed.utils.map      :as        map]
             [io.randomseed.utils          :as      utils]
             [io.randomseed.utils.fs       :as         fs])

  (:import (org.eclipse.jetty.server.handler.gzip GzipHandler)
           (org.eclipse.jetty.server              Server
                                                  Handler)))

(set! *warn-on-reflection* true)

;;
;; Jetty Gzip support
;;

(defn- jetty-gzip-handler-cfg
  [{:keys [gzip-types gzip-min-size]}]
  (if-not (seq gzip-types)
    identity
    (fn [server]
      (.setHandler ^Server server
                   (doto (GzipHandler.)
                     (.setIncludedMimeTypes (into-array gzip-types))
                     (.setMinGzipSize (utils/safe-parse-long gzip-min-size 128))
                     (.setHandler ^Handler (.getHandler ^Server server)))))))

;;
;; Jetty server control handlers
;;

(defn run
  [{:keys [enabled? port join? gzip? handler properties
           keystore truststore same-key-passwords? key-password trust-password]
    :as   options}]
  (if enabled?
    (log/msg-with-val
     "HTTP server (Jetty) is starting on port" port
     (let [handler        (atom (delay handler))
           hfun           (fn [request] (@@handler request))
           same-pwd?      (boolean same-key-passwords?)
           gzip?          (boolean gzip?)
           key-password   (ssl/ask-pass-keystore keystore key-password)
           trust-password (if same-pwd? key-password)
           trust-password (ssl/ask-pass-truststore truststore trust-password)
           options        (-> options
                              (map/assoc-missing   :name                 (properties :name))
                              (map/update-existing :ssl?                            boolean)
                              (map/update-existing :truststore                 ssl/keystore)
                              (map/update-existing :keystore                   ssl/keystore)
                              (map/assoc-missing   :key-password               key-password)
                              (map/assoc-missing   :trust-password           trust-password)
                              (map/update-existing :gzip-types               vec/of-strings)
                              (map/update-existing :gzip-min-size utils/safe-parse-long 128)
                              (assoc               :same-key-passwords?           same-pwd?)
                              (assoc               :gzip?                             gzip?)
                              (dissoc              :handler :properties :logger))
           options        (if gzip?
                            (assoc options :configurator (jetty-gzip-handler-cfg options))
                            options)]
       {:handler handler
        :options options
        :server  (jetty/run-jetty hfun options)}))))

(defn suspend
  [{:keys [handler options]}]
  (if handler
    (log/msg-with-val
     "HTTP server (Jetty) connections to port" (options :port) "are suspended"
     (reset! handler (promise)))))

(defn resume
  [k config old-config old-impl]
  (if (= (dissoc config :handler :logger) (dissoc old-config :handler :logger))
    (let [opts        (:options old-impl)
          handler     (:handler config)
          old-handler (:handler old-impl)]
      (when old-handler
        (log/msg "Resuming HTTP server (Jetty) connections to port" (:port opts))
        (deliver @old-handler handler)
        old-impl))
    (do (if old-impl (system/halt-key! k old-impl))
        (system/init-key  k config))))

(defn stop
  [{:keys [server options]}]
  (if (and server options)
    (log/msg-with-val
     "HTTP server (Jetty) is stopping on port" (options :port)
     (if-some [s server] (.stop ^Server s)))))
