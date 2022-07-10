(ns

    ^{:doc    "amelinium service, Twilio client."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.client.twilio

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                  :as        set]
            [clojure.string               :as        str]
            [tick.core                    :as          t]
            [hato.client                  :as         hc]
            [amelinium.db                 :as         db]
            [amelinium.logging            :as        log]
            [amelinium.system             :as     system]
            [io.randomseed.utils          :refer    :all]
            [io.randomseed.utils.time     :as       time]
            [io.randomseed.utils.var      :as        var]
            [io.randomseed.utils.map      :as        map]))

(def ^:const config-tag (re-pattern ":([a-zA-Z][a-zA-Z0-9]+)"))

(defn prep-base-url
  [config url]
  (when-some [url (some-str url)]
    (str/replace url config-tag #(get config (keyword (nth % 1)) (nth % 0)))))

(defn prep-twilio
  [{:keys [enabled? prepared?]
    :or   {enabled? true prepared? false}
    :as   config}]
  (if (:prepared? config)
    config
    (-> config
        (assoc  :prepared?    true)
        (assoc  :enabled?     (boolean enabled?))
        (update :account-sid  some-str)
        (update :auth-token   some-str)
        (update :api-sid      some-str)
        (update :api-key      some-str)
        (update :service-sid  some-str)
        (update :service      some-str)
        (assoc  :auth-pub     (or (:api-sid config) (:account-sid config)))
        (assoc  :auth-key     (or (:api-key config) (:auth-token  config)))
        (update :client-opts  #(if (and (map? %) (valuable? %)) % {}))
        (update :parameters   #(when (and (map? %) (valuable? %)) %))
        (update :url (partial prep-base-url config)))))

(defn init-twilio
  [config]
  (if-not (:enabled? config)
    (constantly nil)
    (let [config     (-> config
                         (update-in [:client-opts :authenticator]
                                    #(or % {:user (:auth-pub config)
                                            :pass (:auth-key config)}))
                         (update-in [:client-opts :connect-timeout]
                                    time/parse-duration))
          url        (get config :url)
          cli-opts   (get config :client-opts)
          req-method (or (get cli-opts :request-method) :post)
          req-opts   {:url url :request-method req-method}
          client     (hc/build-http-client (:client-opts config))]
      (if-some [default-params (:parameters config)]
        (fn twilio-request
          ([opts parameters]
           (hc/request
            (update (into req-opts opts) :form-params
                    #(if %
                       (into (into default-params %) parameters)
                       (into default-params parameters)))))
          ([parameters]
           (hc/request
            (assoc req-opts :form-params
                   (into default-params parameters)))))
        (fn twilio-request
          ([opts parameters]
           (hc/request
            (update (into req-opts opts) :form-params
                    #(if % (into % parameters) parameters))))
          ([parameters]
           (hc/request
            (assoc req-opts :form-params parameters))))))))

(system/add-init  ::default [k config] (var/make k (init-twilio (prep-twilio config))))
(system/add-prep  ::default [_ config] (prep-twilio config))
(system/add-halt! ::default [k config] (var/make k nil))

(derive ::sms    ::default)
(derive ::email  ::default)
(derive ::verify ::default)
