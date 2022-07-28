(ns

    ^{:doc    "amelinium service, Undertow HTTP server support."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.server.undertow

  (:require  [clojure.string               :as        str]
             [ring.adapter.undertow        :as   undertow]
             [amelinium.server.ssl         :as        ssl]
             [amelinium.system             :as     system]
             [amelinium.logging            :as        log]
             [io.randomseed.utils.vec      :as        vec]
             [io.randomseed.utils.map      :as        map]
             [io.randomseed.utils          :as      utils]
             [io.randomseed.utils.fs       :as         fs])

  (:import [java.net InetSocketAddress]
           [io.undertow Undertow UndertowOptions Undertow$Builder Undertow$ListenerInfo]
           [io.undertow.server HttpHandler]
           [io.undertow.server.handlers BlockingHandler]
           [io.undertow.server.handlers.encoding EncodingHandler$Builder]
           [io.undertow.server.handlers.encoding EncodingHandler ContentEncodingRepository]
           [io.undertow.server.handlers.encoding GzipEncodingProvider DeflateEncodingProvider]
           [io.undertow.predicate Predicate Predicates]))

(set! *warn-on-reflection* true)

(defn parse-predicate
  "Takes a string and returns a predicate (`Predicate`). For an empty string or `nil`,
  returns a predicate which always matches."
  ^Predicate [s]
  (Predicates/parse ^String (or (utils/some-str s) "true")))

(defn main-handler-wrapper
  "Takes options and returns a proper initial handler wrapper for Undertow adapter
  initializer (a function taking a Ring handler and returning `HttpHandler`
  object. It can return asynchronous or synchronous handler (depends on value of
  `:async?` boolean option). If `:dispach?` option is set, it will return a handler
  wrapped in a blocking operation, otherwise it will just set the handler without
  blocking wrapper."
  [options]
  (let [main-handler (if (:async? options)
                       (undertow/async-undertow-handler options)
                       (undertow/undertow-handler options))]
    (fn [ring-handler]
      (if (:dispatch? options)
        (BlockingHandler. (main-handler ring-handler))
        (main-handler ring-handler)))))

(defn attach-handler
  "Adds a handler to a custom handlers chain. Takes an existing wrapper (a function
  taking Ring handler and returning `HttpHandler` object), Undertow handler
  generator (a function taking options and returning new `HttpHandler`) and
  options. Returns a new wrapper which can replace the current one."
  ([undertow-handler-fn options]
   (attach-handler nil undertow-handler-fn options))
  ([existing-handler-wrapper undertow-handler-fn options]
   (fn ^HttpHandler [ring-handler]
     (let [existing-handler-wrapper (or existing-handler-wrapper (main-handler-wrapper options))
           prev-handler             (existing-handler-wrapper ring-handler)]
       (undertow-handler-fn prev-handler options)))))

(defn compress-handler
  "Returns gzip and deflate compress handler of type
  `io.undertow.server.handlers.encoding.EncodingHandler`."
  ^HttpHandler [^HttpHandler prev-handler
                {:keys [compress-predicate] :as options}]
  (let [^Predicate predicate          (parse-predicate compress-predicate)
        ^ContentEncodingRepository cr (doto (ContentEncodingRepository.)
                                        (.addEncodingHandler
                                         "gzip"
                                         ^ContentEncodingProvider (GzipEncodingProvider.)
                                         (int 50) ^Predicate predicate)
                                        (.addEncodingHandler
                                         "deflate"
                                         ^ContentEncodingProvider (DeflateEncodingProvider.)
                                         (int 100) ^Predicate predicate))
        ^EncodingHandler enc-handler  (new EncodingHandler ^ContentEncodingRepository cr)]
    (.setNext ^EncodingHandler enc-handler ^HttpHandler prev-handler)
    enc-handler))

(defn run
  "Runs Undertow server instance."
  [{:keys [enabled? port ssl? ssl-port compress? handler properties server-name
           keystore truststore same-key-passwords? key-password trust-password]
    :as   options}]
  (if enabled?
    (let [name             (properties :name)
          server-name      (or (str server-name) (if name (str name "-undertow")))
          ssl-port         (if ssl-port (utils/safe-parse-long ssl-port 5004))
          ssl?             (if (and ssl-port (not (contains? options :ssl?))) true ssl?)
          ssl-port         (utils/safe-parse-long (if (and ssl? (not ssl-port)) 5004 ssl-port))
          same-pwd?        (boolean same-key-passwords?)
          compress?        (boolean compress?)
          key-password     (if ssl? (ssl/ask-pass-keystore keystore key-password))
          trust-password   (if same-pwd? key-password)
          trust-password   (if ssl? (ssl/ask-pass-truststore truststore trust-password))
          options          (if ssl? options (dissoc options :keystore :truststore))
          options          (-> options
                               (map/assoc-missing   :name               (properties :name))
                               (map/update-existing :host                   utils/some-str)
                               (map/assoc-missing   :port                             4004)
                               (map/update-existing :port       utils/safe-parse-long 4004)
                               (map/assoc-missing   :key-password             key-password)
                               (map/assoc-missing   :trust-password         trust-password)
                               (map/assoc-missing   :dispatch?                        true)
                               (map/update-existing :truststore               ssl/keystore)
                               (map/update-existing :keystore                 ssl/keystore)
                               (map/update-existing :key-managers                 identity)
                               (map/update-existing :trust-managers               identity)
                               (map/update-existing :custom-manager               identity)
                               (map/update-existing :max-entity-size utils/safe-parse-long)
                               (map/update-existing :io-threads      utils/safe-parse-long)
                               (map/update-existing :worker-threads  utils/safe-parse-long)
                               (map/update-existing :buffer-size     utils/safe-parse-long)
                               (map/update-existing :max-sessions    utils/safe-parse-long)
                               (map/update-existing :http2?                        boolean)
                               (map/update-existing :async?                        boolean)
                               (map/update-existing :websocket?                    boolean)
                               (map/update-existing :dispatch?                     boolean)
                               (map/update-existing :direct-buffers?               boolean)
                               (map/update-existing :session-manager               boolean)
                               (map/update-existing :client-auth utils/some-keyword-simple)
                               (assoc               :server-name               server-name)
                               (assoc               :same-key-passwords?         same-pwd?)
                               (assoc               :ssl?                   (boolean ssl?))
                               (assoc               :ssl-port                     ssl-port)
                               (assoc               :compress?                   compress?)
                               (dissoc              :handler :properties :logger))
          async?           (:async? options)
          custom-handlers? compress?
          last-handler     nil
          last-handler     (if compress? (attach-handler last-handler compress-handler options) last-handler)
          options          (if custom-handlers? (assoc options :handler-proxy last-handler) options)
          ssl-starts?      (and ssl? ssl-port)
          starts?          (or port ssl-starts?)
          ports            (if starts? (if ssl-starts? [port ssl-port] [port]))
          ports-str        (str (if (next ports) "ports " "port ") (str/join ", " ports))]
      (if starts? (log/msg "HTTP server (Undertow) is starting on" ports-str))
      {:handler handler
       :options options
       :server  (undertow/run-undertow handler options)})))

(defn- get-listener-port
  [^Undertow$ListenerInfo listener]
  (.getPort ^InetSocketAddress (.getAddress ^Undertow$ListenerInfo listener)))

(defn suspend
  [{:keys [server options] :as config}]
  (when server
    (doseq [listener (utils/lazy-iterator-seq (.getListenerInfo ^Undertow server))]
      (log/msg "Suspending HTTP server connections (Undertow) on port" (get-listener-port listener))
      (.suspend ^Undertow$ListenerInfo listener))
    (log/msg "HTTP server (Undertow) connections are suspended")))

(defn resume
  [k config old-config old-impl]
  (if (= (dissoc config :handler :server :logger)
         (dissoc old-config :handler :server :logger))
    (let [opts   (:options old-impl)
          server (:server old-impl)]
      (when server
        (doseq [listener (utils/lazy-iterator-seq (.getListenerInfo ^Undertow server))]
          (log/msg "Resuming HTTP server (Undertow) connections on port" (get-listener-port listener))
          (.resume ^Undertow$ListenerInfo listener))
        (log/msg "HTTP server (Undertow) connections are resumed")
        old-impl))
    (do
      (if old-impl (system/halt-key! k old-impl))
      (system/init-key k config))))

(defn stop
  [{:keys [server options]}]
  (if (and server options)
    (log/msg-with-val
     "HTTP server (Undertow) is stopping on port" (options :port)
     (if (some? server) (.stop ^Undertow server)))))
