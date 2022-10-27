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
            [reitit.impl                  :refer [fast-assoc]]
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

(defn- get-template-id
  [config template-group lang fallback-template]
  (some-str
   (or (-> (get config :localized-templates)
           (get (some-keyword template-group))
           (get (some-keyword-simple lang)))
       fallback-template)))

(defn- localize-sendmail-params
  ([lang params template-group]
   (localize-sendmail-params lang params template-group nil))
  ([lang params template-group fallback-template]
   (if lang
     (if-some [template-id (get-template-id (email :config)
                                            template-group
                                            lang
                                            fallback-template)]
       (fast-assoc params :template_id template-id)
       params)
     params)))

(defn sendmail-l10n-template
  ([lang to template-group]
   (if-some [to (if (map? to) [to] (if (coll? to) (vec to) [{:email (str to)}]))]
     (email (localize-sendmail-params
             lang
             {:personalizations [{:to to}]}
             template-group
             nil))))
  ([lang to template-group fallback-template-id-or-template-data]
   (if-some [to (if (map? to) [to] (if (coll? to) (vec to) [{:email (str to)}]))]
     (if (map? fallback-template-id-or-template-data)
       (email (localize-sendmail-params
               lang
               {:personalizations
                [{:to                    to
                  :dynamic_template_data fallback-template-id-or-template-data}]}
               template-group
               nil))
       (email (localize-sendmail-params
               lang
               {:personalizations [{:to to}]}
               template-group
               fallback-template-id-or-template-data)))))
  ([lang to template-group fallback-template-id template-data]
   (if-some [to (if (map? to) [to] (if (coll? to) (vec to) [{:email (str to)}]))]
     (email (localize-sendmail-params
             lang
             {:personalizations
              [{:to                    to
                :dynamic_template_data template-data}]}
             template-group
             fallback-template-id)))))

(defn sendmail-l10n-template-async
  ([respond raise lang to template-group]
   (if-some [to (if (map? to) [to] (if (coll? to) (vec to) [{:email (str to)}]))]
     (email {:async? true} (localize-sendmail-params
                            lang
                            {:personalizations [{:to to}]}
                            template-group
                            nil)
            respond raise)))
  ([respond raise lang to template-group fallback-template-id-or-template-data]
   (if-some [to (if (map? to) [to] (if (coll? to) (vec to) [{:email (str to)}]))]
     (if (map? fallback-template-id-or-template-data)
       (email {:async? true} (localize-sendmail-params
                              lang
                              {:personalizations
                               [{:to                    to
                                 :dynamic_template_data fallback-template-id-or-template-data}]}
                              template-group
                              nil)
              respond raise)
       (email {:async? true} (localize-sendmail-params
                              lang
                              {:personalizations [{:to to}]}
                              template-group
                              fallback-template-id-or-template-data)
              respond raise))))
  ([respond raise lang to template-group fallback-template-id template-data]
   (if-some [to (if (map? to) [to] (if (coll? to) (vec to) [{:email (str to)}]))]
     (email {:async true} (localize-sendmail-params
                           lang
                           {:personalizations
                            [{:to                    to
                              :dynamic_template_data template-data}]}
                           template-group
                           fallback-template-id)
            respond raise))))

(defn sendmail-template
  ([to tpl-gr]                 (sendmail-l10n-template nil to tpl-gr))
  ([to tpl-gr fb-tpl-or-tdata] (sendmail-l10n-template nil to tpl-gr fb-tpl-or-tdata))
  ([to tpl-gr fb-tpl tdata]    (sendmail-l10n-template nil to tpl-gr fb-tpl tdata)))

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
                                      #(if %
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
        opts          (if (is-json? accept) (fast-assoc opts :as :json) opts)
        opts          (if auth-tok          (fast-assoc opts :oauth-token auth-tok) opts)
        opts          (if content-type      (fast-assoc opts :content-type content-type) opts)
        opts          (into opts existing-opts)]
    (fast-assoc config :request-opts opts)))

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
  (if p (map/map-keys some-str p)))

(defn init-twilio
  [k config]
  (if-not (:enabled? config)
    (constantly nil)
    (let [client   (hc/build-http-client (:client-opts config))
          req-opts (fast-assoc (or (:request-opts config) {}) :http-client client)]
      (log/msg "Registering Twilio client:" k)
      (if-some [default-params (:parameters config)]
        (fn twilio-request
          ([opts params & [respond raise]]
           (if (= :config params)
             config
             (let [opts       (into req-opts opts)
                   json?      (sending-json? opts)
                   params     (if json? params (stringify-params params))
                   fparams    (get opts :form-params)
                   fparams    (if json? fparams (stringify-params fparams))
                   all-params (if params
                                (if fparams
                                  (deep-merge :into default-params fparams params)
                                  (deep-merge :into default-params params))
                                (if fparams
                                  (deep-merge :into default-params fparams)
                                  default-params))]
               (-> (fast-assoc opts :form-params all-params)
                   (hc/request respond raise)))))
          ([params]
           (if (= :config params)
             config
             (let [params     (if (sending-json? req-opts) params (stringify-params params))
                   all-params (if params (deep-merge :into default-params params) default-params)]
               (-> (fast-assoc req-opts :form-params all-params)
                   (hc/request)))))
          ([]
           (-> (fast-assoc req-opts :form-params default-params)
               (hc/request))))
        (fn twilio-request
          ([opts params & [respond raise]]
           (if (= :config params)
             config
             (let [opts       (conj req-opts opts)
                   json?      (sending-json? opts)
                   params     (if json? params (stringify-params params))
                   fparams    (get opts :form-params)
                   fparams    (if json? fparams (stringify-params fparams))
                   all-params (if params
                                (if fparams
                                  (deep-merge :into fparams params)
                                  params)
                                fparams)]
               (-> (fast-assoc opts :form-params (or all-params {}))
                   (hc/request respond raise)))))
          ([params]
           (if (= :config params)
             config
             (let [params (if (sending-json? req-opts) params (stringify-params params))]
               (-> (fast-assoc req-opts :form-params (or params {}))
                   (hc/request)))))
          ([]
           (hc/request req-opts)))))))

(system/add-init  ::default [k config] (var/make k (init-twilio k (prep-twilio config))))
(system/add-prep  ::default [_ config] (prep-twilio config))
(system/add-halt! ::default [k config] (var/make k nil))

(derive ::sms    ::default)
(derive ::email  ::default)
(derive ::verify ::default)
