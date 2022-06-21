(ns

    ^{:doc    "Web helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string                       :as          str]
            [clojure.core.memoize                 :as          mem]
            [potemkin.namespaces                  :as            p]
            [tick.core                            :as            t]
            [reitit.core                          :as            r]
            [ring.util.response]
            [ring.util.http-response              :as         resp]
            [ring.util.request                    :as          req]
            [selmer.parser                        :as       selmer]
            [amelinium.common                     :as       common]
            [amelinium.http                       :as         http]
            [amelinium.http.middleware.language   :as     language]
            [amelinium.http.middleware.session    :as      session]
            [amelinium.http.middleware.validators :as   validators]
            [amelinium.logging                    :as          log]
            [io.randomseed.utils.map              :as          map]
            [io.randomseed.utils                  :refer      :all]
            [hiccup.core                          :refer      :all]
            [hiccup.table                         :as        table])

  (:import [reitit.core Match]
           [lazy_map.core LazyMapEntry LazyMap]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request map keys exposed in views

(def ^:const http-keys       [::r/router ::r/match :path-params])
(def ^:const page-keys       [:title :subtitle])
(def ^:const param-keys      [:query-params :form-params :path-params])
(def ^:const validators-keys [:validators/config :validators/params-valid?])
(def ^:const session-keys    [:session])
(def ^:const remote-ip-keys  [:remote-ip :remote-ip/str :remote-ip/by-proxy? :remote-ip/proxy])
(def ^:const language-keys   [:language/id :language/str :accept :language/settings])
(def ^:const roles-keys      [:roles :roles/in-context :roles/context
                              :roles/user-authorized? :roles/user-authenticated?])

(def ^:const common-auth-keys (vec (concat session-keys remote-ip-keys roles-keys)))

;; Request map keys to be always copied to the template system data map
;; Later on we put them under :app/data-required for being used by the injecting function

(def ^:const common-keys (vec (concat common-auth-keys
                                      validators-keys
                                      language-keys
                                      http-keys
                                      param-keys
                                      page-keys)))

;; Database

(p/import-vars [amelinium.common
                auth-config auth-db])

;; Operations logging

(p/import-vars [amelinium.common
                oplog-config oplog-logger oplog-logger-populated oplog])

;; Routing data and settings helpers

(p/import-vars [amelinium.common
                router-match? on-page? lang-param guess-lang-param
                login-page? auth-page? login-auth-state])

;; Path parsing

(p/import-vars [amelinium.common
                path-variants-core path-variants
                path-param path-params has-param?
                path-language
                split-query-params-simple split-query-params req-param-path
                path-template-with-param template-path
                parameterized-page parameterized-page-core
                page localized-page localized-or-regular-page
                current-page current-page-id current-page-id-or-path login-page auth-page
                temporary-redirect localized-temporary-redirect move-to
                see-other localized-see-other go-to])

;; Language

(p/import-vars [amelinium.common
                pick-language pick-language-without-fallback
                pick-language-str pick-language-str-without-fallback])

;; Special redirects

(p/import-vars [amelinium.common
                add-slash slash-redir lang-redir])

;; Accounts

(p/import-vars [amelinium.common
                lock-wait-default lock-wait
                hard-lock-time hard-locked?
                soft-lock-time soft-lock-passed soft-locked? soft-lock-remains])

;; Sessions

(p/import-vars [amelinium.common
                session-key session-variable-get-failed?
                allow-expired allow-soft-expired allow-hard-expired])

;; Context and roles

(p/import-vars [amelinium.common
                has-any-role? has-role?
                role-required! with-role-only!
                roles-for-context roles-for-contexts default-contexts-labeler
                roles-matrix roles-tabler])

;; Data structures

(p/import-vars [amelinium.common
                empty-lazy-map])

;; Filesystem operations

(p/import-vars [amelinium.common
                some-resource])

;; HTML generators and transformers

(defn roles-table
  ([req]
   (let [{:keys [data labels]} (roles-tabler req nil)]
     (when (and data labels)
       (html (table/to-table1d data labels)))))
  ([req opts]
   (let [{:keys [data labels]} (roles-tabler req opts)]
     (when (and data labels)
       (html (table/to-table1d data labels))))))

;; HTML rendering

(defn get-missing-app-data-from-req
  "Associates missing data identified with keys listed in keyz with values taken from
  the request map if the key exists. The resulting map is converted to a lazy map if
  it's not."
  [data req keyz]
  (let [req (map/to-lazy req)]
    (reduce (fn [ret k]
              (if-let [entry (and (not (contains? ret k)) (find req k))]
                (assoc ret k (.val_ ^LazyMapEntry entry))
                ret))
            (map/to-lazy (or data empty-lazy-map))
            (seq keyz))))

(defn no-app-data
  [req]
  (assoc req :app/data false))

(defn prep-app-data
  "Prepares data for the rendering functions by copying the given values associated
  with the given keys from `req` to a lazy map under `:app/data` key of the
  `req`. The list of keys to copy must be as a sequential collection, explicitly
  given as `keyz`, or reside under `:app/data-required` of the `req`. If there
  already is `:app/data` in the request map then it will be used as the initial value
  of the created data map. Data for existing keys will not be copied."
  ([req]
   (prep-app-data req nil nil))
  ([req data]
   (prep-app-data req data nil))
  ([req data keyz]
   (if (false? data)
     req
     (let [req-data (get req :app/data)]
       (if (false? req-data)
         req
         (let [req-data (when req-data (map/to-lazy req-data))
               data     (if req-data (map/merge-lazy req-data data) (map/to-lazy data))
               keyz     (or keyz (concat common-keys (get req :app/data-required)))]
           (if (and data (pos? (count data)))
             (get-missing-app-data-from-req data req keyz)
             (map/select-keys-lazy req keyz))))))))

(defn get-app-data
  "Gets the value of `:app/data` for the current request. If it does not exist, returns
  an empty map."
  [req]
  (when-let [ad (get req :app/data empty-lazy-map)]
    ad))

;; Layouts and views

(defn get-view
  "Gets a view partial path for the current route using `:app/view` route data or
  `:name`. If it cannot be extracted, returns default."
  [req]
  (some-str
   (or (http/req-or-route-param req :app/view)
       (http/req-or-route-param req :name)
       "default")))

(defn get-layout
  "Gets layout partial path for the current route using :app/layout route data. If it
  cannot be extracted, returns default."
  [req]
  (or (some-str (http/req-or-route-param req :app/layout))
      "default"))

(defn resolve-generic
  [uri pre dir lang core]
  (let [pre  (or (some-str pre) "views")
        prep (when pre  (str pre  "/"))
        dir  (when dir  (str dir  "/"))
        lang (when lang (str lang "/"))
        pths (lazy-cat [[prep lang dir core ".html"]]
                       [[prep dir core ".html"]]
                       [[prep lang dir "default.html"]]
                       [[prep dir "default.html"]]
                       [[prep lang "default.html"]]
                       [[prep "default.html"]])]
    (or (first (keep #(apply common/some-resource %) pths))
        (do (log/wrn "Cannot find" pre "for" uri)
            (doseq [path pths] (log/wrn (apply str "Tried: [resources]/" path)))))))

(def ^{:arglists '([uri pre dir lang core])}
  resolve-cached
  (mem/fifo resolve-generic :fifo/threshold 2048))

(defn resolve-layout
  [req lang dir]
  (resolve-cached (get req :uri) "layouts" dir lang (get-layout req)))

(defn resolve-view
  [req lang dir]
  (resolve-cached (get req :uri) "views" dir lang (get-view req)))

(defn render
  "Universal page renderer. Takes a request, a data map to be used in templates, a
  subdirectory for the view file (defaults to `nil`), a subdirectory for the template
  file (defaults to `nil`), a language string (guessed if not given, unless
  explicitly set to `false`) and a session map (used only when the language cannot be
  established otherwise and taken from the request if not given). Template filename
  and view filename are taken from the request map (under the keys `:app/template`
  and `:app/view`)."
  ([]
   (render nil nil nil nil nil nil))
  ([req]
   (render req nil nil nil nil nil))
  ([req data]
   (render req data nil nil nil nil))
  ([req data views-subdir]
   (render req data views-subdir nil nil nil))
  ([req data views-subdir layouts-subdir]
   (render req data views-subdir layouts-subdir nil nil))
  ([req data views-subdir layouts-subdir lang]
   (render req data views-subdir layouts-subdir lang nil))
  ([req data views-subdir layouts-subdir lang sess]
   (let [lang (or lang (when-not (false? lang) (pick-language-str req)))
         layt (resolve-layout req lang layouts-subdir)
         view (resolve-view   req lang views-subdir)]
     (when (and layt view)
       (let [dlng (or lang (get req :language/str))
             data (prep-app-data req data)
             data (map/assoc-missing data :url (delay (req/request-url req)) :lang dlng)
             html (selmer/render-file view data)
             rndr (assoc data :body [:safe html])]
         (selmer/render-file layt rndr))))))

(defn render-response
  "Universal response renderer. Uses the render function to render the response unless
  the req is already a valid response."
  ([]
   (render-response resp/ok nil nil nil nil nil nil))
  ([resp-fn]
   (render-response resp-fn nil nil nil nil nil nil))
  ([resp-fn req]
   (render-response resp-fn req nil nil nil nil nil))
  ([resp-fn req data]
   (render-response resp-fn req data nil nil nil nil))
  ([resp-fn req data views-subdir]
   (render-response resp-fn req data views-subdir nil nil nil))
  ([resp-fn req data views-subdir layouts-subdir]
   (render-response resp-fn req data views-subdir layouts-subdir nil nil))
  ([resp-fn req data views-subdir layouts-subdir lang]
   (render-response resp-fn req data views-subdir layouts-subdir lang nil))
  ([resp-fn req data views-subdir layouts-subdir lang sess]
   (if (resp/response? req)
     req
     (if-some [headers (get req :response/headers)]
       (-> (render req data views-subdir layouts-subdir lang sess) resp-fn (update :headers conj headers))
       (-> (render req data views-subdir layouts-subdir lang sess) resp-fn)))))

(defn render-ok
  ([]
   (render-response resp/ok nil nil nil nil nil nil))
  ([req]
   (render-response resp/ok req nil nil nil nil nil))
  ([req data]
   (render-response resp/ok req data nil nil nil nil))
  ([req data views-subdir]
   (render-response resp/ok req data views-subdir nil nil nil))
  ([req data views-subdir layouts-subdir]
   (render-response resp/ok req data views-subdir layouts-subdir nil nil))
  ([req data views-subdir layouts-subdir lang]
   (render-response resp/ok req data views-subdir layouts-subdir lang nil))
  ([req data views-subdir layouts-subdir lang sess]
   (render-response resp/ok req data views-subdir layouts-subdir lang sess)))

(defn render-bad-params
  ([]
   (render-response resp/unprocessable-entity
                    {:app/view "bad-params"}
                    nil nil nil nil nil))
  ([req]
   (render-response resp/unprocessable-entity
                    (assoc req :app/view "bad-params")
                    nil nil nil nil nil))
  ([req data]
   (render-response resp/unprocessable-entity
                    (assoc req :app/view "bad-params")
                    data nil nil nil nil))
  ([req data lang]
   (render-response resp/unprocessable-entity
                    (assoc req :app/view "bad-params")
                    data nil nil lang nil))
  ([req data lang sess]
   (render-response resp/unprocessable-entity
                    (assoc req :app/view "bad-params")
                    data nil nil lang sess)))

;; Linking helpers

(p/import-vars [amelinium.common
                path localized-path])

;; Anti-spam

(p/import-vars [amelinium.common
                random-uuid-or-empty])

(defn anti-spam-code
  "Generates anti-spam HTML string containing randomly selected fields and values using
  `validators/gen-required`."
  ([config]
   (anti-spam-code config 1 nil))
  ([config num]
   (anti-spam-code config num nil))
  ([config num rng]
   (let [r       (validators/gen-required config num rng)
         k-some  (seq (get r :some))
         k-blank (seq (get r :blank))
         k-any   (seq (get r :any))
         r       (concat
                  (when k-some  (map vector k-some  (repeatedly random-uuid)))
                  (when k-blank (map vector k-blank (repeat "")))
                  (when k-any   (map vector k-any   (repeatedly #(random-uuid-or-empty rng)))))]
     (when (seq r)
       (apply str (map #(str "<input type=\"text\" name=\""   (nth % 0)
                             "\" class=\"subspace\" value=\"" (nth % 1)
                             "\"/>")
                       r))))))

;; Template helpers

(defn lang-url
  ([ctx path-or-name lang localized? params query-params]
   (lang-url ctx path-or-name lang localized? params query-params nil))
  ([ctx path-or-name lang localized? params]
   (lang-url ctx path-or-name lang localized? params nil nil))
  ([ctx path-or-name lang localized?]
   (lang-url ctx path-or-name lang localized? nil nil nil))
  ([ctx path-or-name lang]
   (lang-url ctx path-or-name lang true nil nil nil))
  ([ctx path-or-name]
   (lang-url ctx path-or-name nil true nil nil nil))
  ([ctx]
   (lang-url ctx nil nil true nil nil nil))
  ([ctx path-or-name lang localized? params query-params lang-settings]
   (let [router        (or (get ctx ::r/router) (get ctx :router))
         lang          (or lang (get ctx :language/str) (some-str (get ctx :language)) (some-str (get ctx :lang)))
         lang-settings (or lang-settings (get ctx :language/settings) (get ctx :language-param) (get ctx :param) :lang)
         path-or-name  (or (valuable path-or-name) (current-page ctx))
         path-or-name  (when path-or-name (selmer/render path-or-name ctx {:tag-open \[ :tag-close \]}))
         path-or-name  (if (and path-or-name (str/starts-with? path-or-name ":")) (keyword (subs path-or-name 1)) path-or-name)
         path-fn       (if localized? localized-path path)
         out-path      (path-fn path-or-name lang params query-params router lang-settings)
         out-path      (if out-path out-path (when-not (ident? path-or-name) (some-str path-or-name)))]
     out-path)))

(selmer/add-tag!
 :anti-spam-field
 (fn [args ctx]
   (anti-spam-code (get ctx :validators/config) 2)))

(selmer/add-tag!
 :lang-url
 (fn [args ctx]
   (let [path-or-name    (first args)
         args            (rest args)
         args            (if (map? (first args)) (cons nil args) args)
         [lang params
          query-params
          lang-settings] args]
     (lang-url ctx path-or-name lang true params query-params lang-settings))))

(selmer/add-tag!
 :link
 (fn [args ctx content]
   (let [sid             (get (get ctx :session) :id)
         skey            (session-key ctx)
         path-or-name    (first args)
         args            (rest args)
         args            (if (map? (first args)) (cons nil args) args)
         [lang params
          query-params
          lang-settings] args
         out-path        (lang-url ctx path-or-name lang false params query-params lang-settings)]
     (if (and sid skey)
       (str "<form name=\"sessionLink\" class=\"formlink\" action=\"" out-path "\" method=\"post\">"
            (anti-spam-code (get ctx :validators/config))
            "<button type=\"submit\" class=\"link\" name=\"" skey "\" value=\"" sid "\">"
            (get-in content [:link :content])
            "</button></form>")
       (str "<a href=\"" out-path "\" class=\"link\">" (get-in content [:link :content]) "</a>"))))
 :endlink)

(selmer/add-tag!
 :slink
 (fn [args ctx content]
   (let [url  (selmer/render (first args) ctx {:tag-open \[ :tag-close \]})
         sid  (get (get ctx :session) :id)
         skey (session-key ctx)]
     (if (and sid skey)
       (str "<form name=\"sessionLink\" class=\"formlink\" action=\"" url "\" method=\"post\">"
            (anti-spam-code (get ctx :validators/config))
            "<button type=\"submit\" class=\"link\" name=\"" skey "\" value=\"" sid "\">"
            (get-in content [:slink :content])
            "</button></form>")
       (str "<a href=\"" url  "\" class=\"link\">" (get-in content [:slink :content]) "</a>"))))
 :endslink)

(selmer/add-tag!
 :session-data
 (fn [args ctx]
   (let [skey (session-key ctx)]
     (str (anti-spam-code (get ctx :validators/config))
          "<input type=\"hidden\" name=\"" skey "\" value=\"" (get (get ctx :session) :id) "\" />"))))

;; Language helpers

(p/import-vars [amelinium.common
                lang-id lang-str lang-config lang-from-req])
