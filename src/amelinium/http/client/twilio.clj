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

(def ^:const config-tag (re-pattern ":([a-zA-Z][a-zA-Z0-9_\\-]+)"))

(defn replace-tags
  [config s]
  (when-some [url (some-str s)]
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
        (update :url          (partial replace-tags config))
        (update :parameters   (partial map/map-vals (partial replace-tags config)))
        (update :parameters   (partial map/map-keys some-str)))))

(defn init-twilio
  [config]
  (if-not (:enabled? config)
    (constantly nil)
    (let [cli-opts   (-> (or (:client-opts config) {})
                         (update :authenticator #(or % {:user (:auth-pub config)
                                                        :pass (:auth-key config)}))
                         (map/update-existing :connect-timeout #(when %
                                                                  (time/milliseconds
                                                                   (time/parse-duration % :second)))))
          config     (assoc config :client-opts cli-opts)
          url        (get config :url)
          req-method (or (get cli-opts :request-method) :post)
          client     (hc/build-http-client (:client-opts config))
          req-opts   {:url url :request-method req-method :http-client client}]
      (if-some [default-params (:parameters config)]
        (fn twilio-request
          ([opts parameters]
           (let [parameters (map/map-keys some-str parameters)
                 opts       (-> (into req-opts opts)
                                (update :form-params #(if %
                                                        (into (into default-params %) parameters)
                                                        (into default-params parameters))))]
             (hc/request opts)))
          ([parameters]
           (let [parameters (map/map-keys some-str parameters)
                 opts       (assoc req-opts :form-params (into default-params parameters))]
             (hc/request opts))))
        (fn twilio-request
          ([opts parameters]
           (let [parameters (map/map-keys some-str parameters)
                 opts       (-> (into req-opts opts)
                                (update :form-params #(if % (into % parameters) parameters)))]
             (hc/request opts)))
          ([parameters]
           (let [parameters (map/map-keys some-str parameters)
                 opts       (assoc req-opts :form-params parameters)]
             (hc/request opts))))))))

(system/add-init  ::default [k config] (var/make k (init-twilio (prep-twilio config))))
(system/add-prep  ::default [_ config] (prep-twilio config))
(system/add-halt! ::default [k config] (var/make k nil))

(derive ::sms    ::default)
(derive ::email  ::default)
(derive ::verify ::default)
