(ns

    ^{:doc    "amelinium service, Twilio client."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.client.twilio

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                  :as             set]
            [clojure.string               :as             str]
            [tick.core                    :as               t]
            [hato.client                  :as              hc]
            [amelinium.db                 :as              db]
            [amelinium.logging            :as             log]
            [amelinium.system             :as          system]
            [io.randomseed.utils          :refer         :all]
            [io.randomseed.utils.time     :as            time]
            [io.randomseed.utils.var      :as             var]
            [io.randomseed.utils.map      :as             map]
            [potpuri.core                 :refer [deep-merge]]))

(defonce sms    (constantly nil))
(defonce email  (constantly nil))
(defonce verify (constantly nil))

;; Constants

(def ^:const config-tag (re-pattern ":([a-zA-Z][a-zA-Z0-9_\\-]+)"))
(def ^:const json-types #{:json :application/json "application/json" "json"})

;; Helpers

(defn sendsms
  [to body]
  (sms {:Body (str body) :To (str to)}))

(defn- localize-sendmail-params
  ([lang params]
   (localize-sendmail-params lang params nil))
  ([lang params fallback-template]
   (if lang
     (if-some [template-id (some-str (or (get (get (email :config) :localized-templates)
                                              (some-keyword-simple lang))
                                         fallback-template))]
       (assoc params :template_id template-id)
       params)
     params)))

(defn sendmail-localized-template
  ([lang to]
   (when-some [to (if (map? to) [to] (if (coll? to) (vec to) [{:email (str to)}]))]
     (email (localize-sendmail-params
             lang
             {:personalizations [{:to to}]}
             nil))))
  ([lang to fallback-template-id-or-template-data]
   (when-some [to (if (map? to) [to] (if (coll? to) (vec to) [{:email (str to)}]))]
     (if (map? fallback-template-id-or-template-data)
       (email (localize-sendmail-params
               lang
               {:personalizations
                [{:to                    to
                  :dynamic_template_data fallback-template-id-or-template-data}]}
               nil))
       (email (localize-sendmail-params
               lang
               {:personalizations [{:to to}]}
               fallback-template-id-or-template-data)))))
  ([lang to fallback-template-id template-data]
   (when-some [to (if (map? to) [to] (if (coll? to) (vec to) [{:email (str to)}]))]
     (email (localize-sendmail-params
             lang
             {:personalizations
              [{:to                    to
                :dynamic_template_data template-data}]}
             fallback-template-id)))))

(defn sendmail-template
  ([to]                 (sendmail-localized-template nil to))
  ([to fb-tpl-or-tdata] (sendmail-localized-template nil to fb-tpl-or-tdata))
  ([to fb-tpl tdata]    (sendmail-localized-template nil to fb-tpl tdata)))

;; Initialization helpers

(defn- replace-tags
  [config s]
  (if (string? s)
    (str/replace s config-tag #(get config (keyword (nth % 1)) (nth % 0)))
    s))

(defn is-json?
  [config]
  (if (map? config)
    (or (contains? json-types (get config :accept))
        (contains? json-types (get config :content-type)))
    (contains? json-types config)))

(defn sending-json?
  [opts]
  (contains? json-types (get opts :content-type)))

(defn receiving-json?
  [opts]
  (contains? json-types (get opts :accept)))

(defn- prep-auth
  [{:keys [api-sid api-key api-token account-sid account-key account-token username password]
    :as   config}]
  (cond
    (and username password)         (assoc config :auth-pub username    :auth-key password)
    (and api-sid api-token)         (assoc config :auth-pub api-sid     :auth-tok api-token)
    (and api-sid api-key)           (assoc config :auth-pub api-sid     :auth-key api-key)
    (and account-sid account-token) (assoc config :auth-pub account-sid :auth-tok account-token)
    (and account-sid account-key)   (assoc config :auth-pub account-sid :auth-key account-key)
    api-token                       (assoc config :auth-tok api-token)
    api-key                         (assoc config :auth-key api-key)
    api-sid                         (assoc config :auth-pub api-sid)
    account-token                   (assoc config :auth-tok account-token)
    account-key                     (assoc config :auth-key account-key)
    account-sid                     (assoc config :auth-pub account-sid)))

(defn- prep-params
  [{:keys [parameters]
    :as   config}]
  (if-not (and parameters (map? parameters) (valuable? parameters))
    (dissoc config :parameters)
    (update config :parameters
            (comp (partial map/map-keys some-str)
                  (partial map/map-vals (partial replace-tags config))))))

(defn- prep-client-opts
  [config]
  (let [auth-pub (:auth-pub    config)
        auth-key (:auth-key    config)
        auth-tok (:auth-tok    config)
        opts     (:client-opts config)
        opts     (if (and (map? opts) (valuable? opts)) opts {})
        opts     (if (and auth-pub auth-key) (map/assoc-missing opts :authenticator
                                                                {:user auth-pub
                                                                 :pass auth-key})
                     opts)
        opts     (map/update-existing opts :connect-timeout
                                      #(when %
                                         (time/milliseconds
                                          (time/parse-duration % :second))))]
    (assoc config :client-opts opts)))

(defn- prep-request-opts
  [config]
  (let [url           (:url          config)
        auth-tok      (:auth-tok     config)
        cli-opts      (:client-opts  config)
        opts          (:request-opts config)
        req-method    (or (get cli-opts :request-method) :post)
        accept        (or (get config :accept) :json)
        content-type  (get config :content-type)
        existing-opts (if (and (map? opts) (valuable? opts)) opts {})
        opts          {:url            url
                       :accept         accept
                       :request-method req-method}
        opts          (if (is-json? accept) (assoc opts :as :json) opts)
        opts          (if auth-tok          (assoc opts :oauth-token auth-tok) opts)
        opts          (if content-type      (assoc opts :content-type content-type) opts)
        opts          (into opts existing-opts)]
    (assoc config :request-opts opts)))

;; Initialization

(defn prep-twilio
  [{:keys [enabled? prepared?]
    :or   {enabled? true prepared? false}
    :as   config}]
  (if (:prepared? config)
    config
    (-> config
        (assoc  :prepared?    true)
        (assoc  :enabled?     (boolean enabled?))
        (map/update-existing  :url           some-str)
        (map/update-existing  :account-sid   some-str)
        (map/update-existing  :account-key   some-str)
        (map/update-existing  :account-token some-str)
        (map/update-existing  :api-sid       some-str)
        (map/update-existing  :api-key       some-str)
        (map/update-existing  :api-token     some-str)
        (map/update-existing  :service-sid   some-str)
        (map/update-existing  :service       some-str)
        (map/update-existing  :username      some-str)
        (map/update-existing  :password      some-str)
        prep-auth
        prep-params
        (update :url (partial replace-tags config))
        prep-client-opts
        prep-request-opts)))

(defn- stringify-params
  [p]
  (map/map-keys some-str p))

(defn init-twilio
  [k config]
  (if-not (:enabled? config)
    (constantly nil)
    (let [client   (hc/build-http-client (:client-opts config))
          req-opts (assoc (:request-opts config) :http-client client)]
      (log/msg "Registering Twilio client:" k)
      (if-some [default-params (:parameters config)]
        (fn twilio-request
          ([opts params]
           (let [opts   (into req-opts opts)
                 json?  (sending-json? opts)
                 params (or params {})
                 params (if json? params (stringify-params params))
                 opts   (update opts :form-params
                                #(if %
                                   (deep-merge :into default-params (if json? % (stringify-params %)) params)
                                   (deep-merge :into default-params params)))]
             (if (= :config params)
               config
               (hc/request opts))))
          ([params]
           (let [params (or params {})
                 params (if (sending-json? req-opts) params (stringify-params params))
                 opts   (assoc req-opts :form-params (deep-merge :into default-params params))]
             (if (= :config params)
               config
               (hc/request opts))))
          ([]
           (let [opts (assoc req-opts :form-params default-params)]
             (hc/request opts))))
        (fn twilio-request
          ([opts params]
           (let [opts   (into req-opts opts)
                 json?  (sending-json? opts)
                 params (or params {})
                 params (if json? params (stringify-params params))
                 opts   (update opts :form-params
                                #(if % (deep-merge :into
                                                   (if json? % (stringify-params %))
                                                   params)
                                     params))]
             (if (= :config params)
               config
               (hc/request opts))))
          ([params]
           (let [params (or params {})
                 params (if (sending-json? req-opts) params (stringify-params params))
                 opts   (assoc req-opts :form-params params)]
             (if (= :config params)
               config
               (hc/request opts))))
          ([]
           (hc/request req-opts)))))))

(system/add-init  ::default [k config] (var/make k (init-twilio k (prep-twilio config))))
(system/add-prep  ::default [_ config] (prep-twilio config))
(system/add-halt! ::default [k config] (var/make k nil))

(derive ::sms    ::default)
(derive ::email  ::default)
(derive ::verify ::default)
