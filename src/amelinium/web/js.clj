(ns

    ^{:doc    "amelinium service, JS."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.js

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string                       :as        str]
            [tick.core                            :as          t]
            [reitit.core                          :as          r]
            [jsonista.core                        :as          j]
            [amelinium.i18n                       :as       i18n]
            [amelinium.common                     :as     common]
            [amelinium.http.middleware.session    :as    session]
            [amelinium.http.middleware.language   :as   language]
            [amelinium.http.middleware.validators :as validators]
            [amelinium.http.middleware.coercion   :as   coercion]
            [amelinium.logging                    :as        log]
            [amelinium.system                     :as     system]
            [io.randomseed.utils.map              :as        map]
            [io.randomseed.utils                  :refer    :all])

  (:import (java.net URLEncoder)))

;; Configuration initializers

(defn init-js-config
  "Initializes JS configuration."
  [{:keys [enabled? router language translations session properties]
    :or   {enabled? true}}]
  (when enabled?
    (log/msg "Preparing JS site configuration")
    (let [app-name         (or (some-str (get properties :name)) "Amelinium")
          session-id-field (or (some-str (get-in session [:config :id-field])) "session-id")
          config           {:app_name       app-name
                            :session_id_key session-id-field}
          json             (j/write-value-as-string config)]
      {:config config
       :json   json
       :script (str
                "<script type=\"text/javascript\">"
                "const ameliniumConfig = JSON.parse('" json "');"
                "</script>")})))

(system/add-init  ::config [_ config] (init-js-config config))
(system/add-halt! ::config [_ config] nil)
