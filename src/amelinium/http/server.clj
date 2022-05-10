(ns

    ^{:doc    "amelinium service, HTTP server."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.server

  (:require [amelinium.system               :as   system]
            [amelinium.logging              :as      log]
            [amelinium.http.server.jetty    :as    jetty]
            [amelinium.http.server.undertow :as undertow]))

;;
;; Jetty server control handlers
;;

(system/add-init     ::jetty   [_ config] (jetty/run (dissoc config :logger)))
(system/add-resolve  ::jetty   [_ {:keys [server]}] server)
(system/add-suspend! ::jetty   [_ config] (jetty/suspend config))
(system/add-resume   ::jetty   [k config old-config old-impl] (jetty/resume k config old-config old-impl))
(system/add-halt!    ::jetty   [_ config] (jetty/stop config))

;;
;; Undertow server control handlers
;;

(system/add-init     ::undertow   [_ config] (undertow/run (dissoc config :logger)))
(system/add-resolve  ::undertow   [_ {:keys [server]}] server)
(system/add-suspend! ::undertow   [_ config] (undertow/suspend config))
(system/add-resume   ::undertow   [k config old-config old-impl] (undertow/resume k config old-config old-impl))
(system/add-halt!    ::undertow   [_ config] (undertow/stop config))
