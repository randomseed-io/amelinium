(ns

    ^{:doc    "Amelinium service, Twilio client."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.client.messaging.twilio

  (:refer-clojure :exclude [uuid random-uuid])

  (:require [clojure.string                  :as                        str]
            [hato.client                     :as                         hc]
            [phone-number.core               :as                      phone]
            [amelinium.logging               :as                        log]
            [amelinium.system                :as                     system]
            [amelinium.http.client.messaging :as                     client]
            [amelinium.proto.messaging       :as                        msg]
            [amelinium.proto.twilio          :as                          p]
            [io.randomseed.utils.time        :as                       time]
            [io.randomseed.utils.var         :as                        var]
            [io.randomseed.utils.map         :as                        map]
            [potpuri.core                    :refer            [deep-merge]]
            [amelinium.types.twilio          :refer [map->TwilioConfig
                                                     map->TwilioEmailMessaging
                                                     map->TwilioSMSMessaging]]
            [io.randomseed.utils             :refer [valuable?
                                                     some-str
                                                     some-keyword
                                                     some-keyword-simple
                                                     ]])

  (:import (clojure.lang              IPersistentMap IPersistentSet)
           (amelinium                 TwilioConfig)
           (amelinium.proto.messaging Messaging)
           (amelinium.proto.twilio    TwilioControl
                                      TwilioEmailMessaging
                                      TwilioSMSMessaging)))

(defonce ^:redef sms    (constantly nil))
(defonce ^:redef email  (constantly nil))
(defonce ^:redef verify (constantly nil))

;; Constants

(def ^:const config-tag (re-pattern ":([a-zA-Z][a-zA-Z0-9_\\-]+)"))
(def ^:const json-types #{:application/json
                          "application/json"
                          "json" "JSON"
                          :json  :JSON})

;; Helpers

(defn- get-template-id-core
  [localized-templates template-group lang fallback-template]
  (some-str
   (or (-> localized-templates
           (get (some-keyword template-group))
           (get (some-keyword-simple lang)))
       fallback-template)))

(defn- localize-sendmail-params
  ([^TwilioControl ctrl lang params template-group]
   (localize-sendmail-params ctrl lang params template-group nil))
  ([^TwilioControl ctrl lang params template-group fallback-template]
   (if lang
     (if-some [template-id (p/get-template-id ctrl
                                              template-group
                                              lang
                                              fallback-template)]
       (map/qassoc params :template_id template-id)
       params)
     params)))

;; E-mail sending

(defn- prep-to
  [to]
  (cond (nil?  to) nil
        (map?  to) [to]
        (coll? to) (vec to)
        :else      [{:email (str to)}]))

(defn sendmail-l10n-template
  ([^TwilioControl ctrl lang to template-group]
   (when-some [to (prep-to to)]
     (p/request ctrl (localize-sendmail-params
                      ctrl
                      lang
                      {:personalizations [{:to to}]}
                      template-group
                      nil))))
  ([^TwilioControl ctrl lang to template-group fallback-template-id-or-template-data]
   (when-some [to (prep-to to)]
     (if (map? fallback-template-id-or-template-data)
       (p/request ctrl (localize-sendmail-params
                        ctrl
                        lang
                        {:personalizations
                         [{:to                    to
                           :dynamic_template_data fallback-template-id-or-template-data}]}
                        template-group
                        nil))
       (p/request ctrl (localize-sendmail-params
                        ctrl
                        lang
                        {:personalizations [{:to to}]}
                        template-group
                        fallback-template-id-or-template-data)))))
  ([^TwilioControl ctrl lang to template-group fallback-template-id template-data]
   (when-some [to (prep-to to)]
     (p/request ctrl (localize-sendmail-params
                      ctrl
                      lang
                      {:personalizations
                       [{:to                    to
                         :dynamic_template_data template-data}]}
                      template-group
                      fallback-template-id)))))

(defn sendmail-l10n-template-async
  {:arglists '([^TwilioControl ctrl respond raise lang to template-group]
               [^TwilioControl ctrl respond raise lang to template-group fallback-template-id]
               [^TwilioControl ctrl respond raise lang to template-group template-data]
               [^TwilioControl ctrl respond raise lang to template-group fallback-template-id template-data])}
  ([^TwilioControl ctrl respond raise lang to template-group]
   (when-some [to (prep-to to)]
     (p/request ctrl {:async? true} (localize-sendmail-params
                                     ctrl
                                     lang
                                     {:personalizations [{:to to}]}
                                     template-group
                                     nil)
                respond raise)))
  ([^TwilioControl ctrl respond raise lang to template-group fallback-template-id-or-template-data]
   (when-some [to (prep-to to)]
     (if (map? fallback-template-id-or-template-data)
       (p/request ctrl {:async? true} (localize-sendmail-params
                                       ctrl
                                       lang
                                       {:personalizations
                                        [{:to                    to
                                          :dynamic_template_data fallback-template-id-or-template-data}]}
                                       template-group
                                       nil)
                  respond raise)
       (p/request ctrl {:async? true} (localize-sendmail-params
                                       ctrl
                                       lang
                                       {:personalizations [{:to to}]}
                                       template-group
                                       fallback-template-id-or-template-data)
                  respond raise))))
  ([^TwilioControl ctrl respond raise lang to template-group fallback-template-id template-data]
   (when-some [to (prep-to to)]
     (p/request ctrl {:async true} (localize-sendmail-params
                                    ctrl
                                    lang
                                    {:personalizations
                                     [{:to                    to
                                       :dynamic_template_data template-data}]}
                                    template-group
                                    fallback-template-id)
                respond raise))))

(defn sendmail-template
  {:arglists '([^TwilioControl ctrl to template-group]
               [^TwilioControl ctrl to template-group fallback-template]
               [^TwilioControl ctrl to template-group template-data]
               [^TwilioControl ctrl to template-group fallback-template template-data])}
  ([^TwilioControl ctrl to tpl-gr]
   (sendmail-l10n-template ctrl nil to tpl-gr))
  ([^TwilioControl ctrl to tpl-gr fb-tpl-or-tdata]
   (sendmail-l10n-template ctrl nil to tpl-gr fb-tpl-or-tdata))
  ([^TwilioControl ctrl to tpl-gr fb-tpl tdata]
   (sendmail-l10n-template ctrl nil to tpl-gr fb-tpl tdata)))

;; SMS sending

(defn- some-phone
  [id]
  (if (phone/native? id)
    (phone/format id :phone-number.format/e164)
    (some-str id)))

(defn sendsms
  [^TwilioControl ctrl to body]
  (p/request ctrl {:Body (str body) :To (some-phone to)}))

(defn sendsms-async
  [^TwilioControl ctrl respond raise to body]
  (p/request ctrl {:async? true}
             {:Body (str body) :To (some-phone to)}
             respond raise))

;; Messaging protocol implementation

(defn- on-ok-adapter
  "Adapter for asynchronous message sending. Takes Twilio response and returns a map
  standardized for messaging subsystem."
  ^IPersistentMap [^Messaging driver ^IPersistentMap response]
  (when-some [headers (:headers response)]
    (when-some [req-id (when (map? headers)
                         (or (get headers "twilio-request-id")
                             (get headers "x-message-id")))]
      (-> (msg/response-stub driver)
          (map/qassoc :provider/msg-id   req-id
                      :provider/response response
                      :status            :accepted)))))

(defn- on-err-adapter
  "Default exception adapter for asynchronous SMS sending. Takes Twilio exception and
  returns a map standardized for messaging subsystem."
  ^IPersistentMap [^Messaging driver exception]
  (-> (msg/response-stub driver)
      (map/qassoc :status             :failed
                  :provider/exception exception)))

(extend-protocol msg/Messaging

  TwilioEmailMessaging

  (provider      ^IPersistentMap [drv] (.provider      ^TwilioEmailMessaging drv))
  (capabilities  ^IPersistentSet [drv] (.capabilities  ^TwilioEmailMessaging drv))
  (response-stub ^IPersistentMap [drv] (.response-stub ^TwilioEmailMessaging drv))
  (send! ^IPersistentMap [drv ^IPersistentMap msg]
    (sendmail-l10n-template (.control ^TwilioEmailMessaging drv)
                            (get msg :lang)
                            (get msg :to)
                            (get msg :template/key)
                            (get msg :template/fallback-key)
                            (get msg :template/data)))
  (send-async! [drv ^IPersistentMap msg ^IPersistentMap opts]
    (sendmail-l10n-template-async (.control ^TwilioEmailMessaging drv)
                                  #((get opts :on-ok)  (on-ok-adapter  drv %))
                                  #((get opts :on-err) (on-err-adapter drv %))
                                  (get msg :lang)
                                  (get msg :to)
                                  (get msg :template/key)
                                  (get msg :template/fallback-key)
                                  (get msg :template/data)))

  TwilioSMSMessaging

  (provider      ^IPersistentMap [drv] (.provider      ^TwilioSMSMessaging drv))
  (capabilities  ^IPersistentSet [drv] (.capabilities  ^TwilioSMSMessaging drv))
  (response-stub ^IPersistentMap [drv] (.response-stub ^TwilioSMSMessaging drv))
  (send! ^IPersistentMap [drv ^IPersistentMap msg]
    (sendsms (.control ^TwilioSMSMessaging drv)
             (get msg :to)
             (get msg :body)))
  (send-async! [drv ^IPersistentMap msg ^IPersistentMap opts]
    (sendsms-async (.control ^TwilioSMSMessaging drv)
                   #((get opts :on-ok)  (on-ok-adapter  drv %))
                   #((get opts :on-err) (on-err-adapter drv %))
                   (get msg :to)
                   (get msg :body))))

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
  (let [auth-pub  (:auth-pub    config)
        auth-key  (:auth-key    config)
        _auth-tok (:auth-tok    config)
        opts      (:client-opts config)
        opts      (if (and (map? opts) (valuable? opts)) opts {})
        opts      (if (and auth-pub auth-key) (map/assoc-missing opts :authenticator
                                                                 {:user auth-pub
                                                                  :pass auth-key})
                     opts)
        opts      (map/update-existing opts :connect-timeout
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
        opts          (if (is-json? accept) (map/qassoc opts :as :json) opts)
        opts          (if auth-tok          (map/qassoc opts :oauth-token auth-tok) opts)
        opts          (if content-type      (map/qassoc opts :content-type content-type) opts)
        opts          (conj opts existing-opts)]
    (map/qassoc config :request-opts opts)))

(defn prep-twilio
  [{:keys [enabled? prepared? url]
    :or   {enabled? true prepared? false}
    :as   config}]
  (if prepared?
    config
    (-> config
        (assoc  :prepared?    true)
        (assoc  :enabled?     (boolean enabled?))
        (assoc  :url          (some-str url))
        (assoc  :raw-url      (some-str url))
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

(defn expand-twilio
  [k config]
  {k (prep-twilio config)})

(defn- stringify-params
  [p]
  (when p (map/map-keys some-str p)))

;; Protocol implementation for nil

(extend-protocol p/TwilioControl
  nil
  (config                [_] nil)
  (get-template-id [_ _ _ _] nil)
  (request
    ([_]                     nil)
    ([_ _]                   nil)
    ([_ _ _]                 nil)
    ([_ _ _ _]               nil)
    ([_ _ _ _ _]             nil)))

;; Initialization

(defn init-twilio
  [k config]
  (if-not (:enabled? config)
    (constantly nil)
    (let [client               (hc/build-http-client (:client-opts config))
          req-opts             (map/qassoc (:request-opts config) :http-client client)
          localized-templates  (:localized-templates config)
          ^TwilioConfig config (map->TwilioConfig config)]
      (log/msg "Registering Twilio client:" k)

      (if-some [default-params (:parameters config)]
        (reify p/TwilioControl
          (config ^TwilioConfig [_] config)
          (get-template-id [_ tg lang fb-tpl]
            (get-template-id-core localized-templates tg lang fb-tpl))
          (request [_ opts params respond raise]
            (let [opts       (conj (or req-opts {}) opts)
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
                                 default-params))
                  opts       (map/qassoc opts :form-params all-params)]
              (if (or respond raise)
                (hc/request opts respond raise)
                (hc/request opts))))
          (request [c opts params respond]
            (p/request c opts params respond nil))
          (request [c opts params]
            (p/request c opts params nil nil))
          (request [_ params]
            (let [params     (if (sending-json? req-opts) params (stringify-params params))
                  all-params (if params (deep-merge :into default-params params) default-params)]
              (-> (map/qassoc req-opts :form-params all-params)
                  (hc/request))))
          (request [_]
            (-> (map/qassoc req-opts :form-params default-params)
                (hc/request))))

        (reify p/TwilioControl
          (config  ^TwilioConfig [_] config)
          (get-template-id [_ tg lang fb-tpl]
            (get-template-id-core localized-templates tg lang fb-tpl))
          (request [_ opts params respond raise]
            (let [opts       (conj (or req-opts {}) opts)
                  json?      (sending-json? opts)
                  params     (if json? params (stringify-params params))
                  fparams    (get opts :form-params)
                  fparams    (if json? fparams (stringify-params fparams))
                  all-params (if params
                               (if fparams
                                 (deep-merge :into fparams params)
                                 params)
                               fparams)
                  opts       (map/qassoc opts :form-params (or all-params {}))]
              (if (or respond raise)
                (hc/request opts respond raise)
                (hc/request opts))))
          (request [c opts params respond]
            (p/request c opts params respond nil))
          (request [c opts params]
            (p/request c opts params nil nil))
          (request [_ params]
            (let [params (if (sending-json? req-opts) params (stringify-params params))]
              (-> (map/qassoc req-opts :form-params (or params {}))
                  (hc/request))))
          (request [_]
            (hc/request req-opts)))))))

(defn init-channel
  [k config constructor]
  (if-not (:enabled? config)
    (constantly nil)
    (if (satisfies? config msg/Messaging)
      config
      (do
        (log/msg "Setting up" (or (some-str (:channel-type config)) " ") "messaging channel:" k)
        (constructor
         (map/qupdate config :messaging/control init-twilio k))))))

(defn prep-channel
  [k config channel-type]
  (when config
    (cond
      (not        config)                 nil
      (satisfies? config msg/Messaging)   config
      (symbol?    config)                 (prep-channel k (var/deref config) channel-type)
      (satisfies? config p/TwilioControl) (prep-channel k {:enabled? true :messaging/control config} (:service (p/config config)))
      :else
      (let [
            channel-type        (or (some-keyword (:messaging/channel-type config))
                                    (some-keyword (:messaging/service      config))
                                    (some-keyword (:channel/type           config))
                                    (some-keyword (:channel                config))
                                    (some-keyword (:service                config))
                                    (some-keyword channel-type)
                                    :email)
            capabilities        (or (some-keyword (:messaging/capabilities config))
                                    (some-keyword (:channel/capabilities   config))
                                    (some-keyword (:capabilities           config))
                                    #{:i18n :async :templates channel-type})
            channel-description (or (some-str (:messaging/description      config))
                                    (some-str (:channel/description        config))
                                    (some-str (:description                config))
                                    (str "Twilio " (name channel-type) " channel"))
            response-stub       {:channel/type channel-type
                                 :channel/id   (some-keyword k)
                                 :provider/id  ::client/twilio}
            provider            (map/qassoc response-stub
                                            :provider/name "Twilio"
                                            :channel/description channel-description)]
        (-> config
            (dissoc            :channel :capabilities :description
                               :channel/type :channel/capabilities :channel/description)
            (map/assoc-missing :service channel-type)
            (map/qassoc        :messaging/capabilities  capabilities
                               :messaging/provider      provider
                               :messaging/response-stub response-stub
                               :messaging/channel-type  channel-type))))))

(system/add-expand ::default [k config] (expand-twilio k config))
(system/add-init   ::default [k config] (var/make k (init-twilio k (prep-twilio config))))
(system/add-halt!  ::default [k      _] (var/make k nil))

(system/add-expand ::email   [k config] {k (prep-channel k config :email)})
(system/add-init   ::email   [k config] (var/make k (init-channel k config map->TwilioEmailMessaging)))
(system/add-halt!  ::email   [k      _] (var/make k nil))

(system/add-expand ::sms     [k config] {k (prep-channel k config :sms)})
(system/add-init   ::sms     [k config] (var/make k (init-channel k config map->TwilioSMSMessaging)))
(system/add-halt!  ::sms     [k      _] (var/make k nil))

(derive ::verify ::default)
(derive ::all    ::default)
