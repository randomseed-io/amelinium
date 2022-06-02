(ns

    ^{:doc    "API helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.api

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                          :as          set]
            [clojure.string                       :as          str]
            [clojure.core.memoize                 :as          mem]
            [clojure.java.io                      :as           io]
            [potemkin.namespaces                  :as            p]
            [tick.core                            :as            t]
            [lazy-map.core                        :as     lazy-map]
            [reitit.core                          :as            r]
            [reitit.ring                          :as         ring]
            [ring.util.response]
            [ring.util.http-response              :as         resp]
            [ring.util.request                    :as          req]
            [selmer.parser                        :as       selmer]
            [amelinium.http                       :as         http]
            [amelinium.http.middleware.roles      :as        roles]
            [amelinium.http.middleware.language   :as     language]
            [amelinium.http.middleware.session    :as      session]
            [amelinium.http.middleware.db         :as       mid-db]
            [amelinium.http.middleware.validators :as   validators]
            [amelinium.web.oplog.auth             :as   oplog-auth]
            [amelinium.web.model.user             :as         user]
            [amelinium.logging                    :as          log]
            [amelinium.web                        :as          web]
            [amelinium.db                         :as           db]
            [io.randomseed.utils.time             :as         time]
            [io.randomseed.utils.vec              :as          vec]
            [io.randomseed.utils.map              :as          map]
            [io.randomseed.utils                  :refer      :all]
            [hiccup.core                          :refer      :all]
            [hiccup.table                         :as        table])

  (:import [reitit.core Match]
           [lazy_map.core LazyMapEntry LazyMap]))

;; Database

(p/import-vars [amelinium.web
                auth-config auth-db])

;; Operations logging

(p/import-vars [amelinium.web
                oplog-config oplog-logger oplog-logger-populated oplog])

;; Routing data and settings helpers

(p/import-vars [amelinium.web
                router-match? on-page? lang-param-id guess-lang-param-id
                login-page? auth-page? login-auth-state])

;; Path parsing

(def ^{:arglists '([path lang-id]
                   [path lang suffix])}
  path-variants
  "Generates a list of all possible language variants of a path."
  (mem/fifo web/path-variants-core :fifo/threshold 2048))

(p/import-vars [amelinium.web
                path-param path-params path-language
                split-query-params-simple split-query-params has-param?
                parameterized-page localized-page localized-or-regular-page page
                current-page login-page auth-page
                temporary-redirect localized-temporary-redirect
                move-to see-other localized-see-other])

;; Language

(def ^:const language-pickers-default
  [language/path-lang-id
   language/body-lang-id
   language/form-lang-id
   :language/user
   :language/client
   language/accept-lang-id
   :language/id])

(def ^:const language-pickers-client-preferred
  [language/body-lang-id
   language/form-lang-id
   :language/user
   language/accept-lang-id
   :language/client
   :language/id
   language/path-lang-id])

(def ^:const language-pickers-logged-in
  [language/body-lang-id
   language/form-lang-id
   :language/user
   :language/client
   language/path-lang-id
   language/accept-lang-id
   :language/id])

(defn pick-language-id
  "Tries to pick the best language for a known user or a visitor. To be used (among
  other scenarios) after a successful log-in to show the right language version of a
  welcome page."
  ([req]
   (pick-language-id req language-pickers-default))
  ([req methods]
   (->> (cons (constantly :en) nil)
        (concat methods)
        (map (comp some-keyword #(% req)))
        (filter identity)
        first)))

(defn pick-language-str
  ([req]
   (some-str (pick-language-id req language-pickers-default)))
  ([req methods]
   (some-str (pick-language-id req methods))))

;; Special redirects

(p/import-vars [amelinium.web
                add-slash slash-redir lang-redir])

;; Accounts

(p/import-vars [amelinium.web
                lock-wait-default lock-wait hard-lock-time soft-lock-time
                soft-lock-passed soft-lock-remains hard-locked? soft-locked?])

;; Sessions

(p/import-vars [amelinium.http.middleware.session
                session-key])

(p/import-vars [amelinium.web
                session-variable-get-failed?
                allow-expired allow-soft-expired allow-hard-expired])

;; Context and roles

(p/import-vars [amelinium.web
                has-any-role? has-role? role-required! with-role-only!
                roles-for-context roles-for-contexts
                default-contexts-labeler roles-matrix])

;; API rendering

(p/import-vars [amelinium.web
                empty-lazy-map get-missing-app-data-from-req
                no-app-data prep-app-data get-app-data
                some-resource])

;; Language helpers

(p/import-vars [amelinium.web
                lang-url lang-id lang-str lang-config])

(defn render
  ([]
   (render nil))
  ([req]
   (let [body (get req :response)]
     (if (map? body)
       body
       (if (sequential? body)
         (seq body)
         body)))))

(defn response?
  [req]
  (and (map? req)
       (integer?  (:status req))
       (or (map?  (:headers req))
           (coll? (:body req)))))

(defn render-response
  "Universal response renderer. Uses the render function to render the response body
  unless the `req` is already a valid response (then it is returned as-is)."
  ([]
   (render-response resp/ok nil))
  ([resp-fn]
   (render-response resp-fn nil))
  ([resp-fn req]
   (if (response? req) req (resp-fn (render req)))))

(defn render-ok
  ([]
   (render-response resp/ok nil))
  ([req]
   (render-response resp/ok req)))

(defn render-bad-params
  ([]
   (render-response resp/unprocessable-entity nil))
  ([req]
   (render-response resp/unprocessable-entity req)))

;; Linking helpers

(defn path
  "Creates a URL on a basis of route name or a path."
  ([req name-or-path]
   (page req name-or-path))
  ([req name-or-path lang]
   (localized-page nil name-or-path lang
                   nil nil true false
                   (get req ::r/router)
                   (lang-param-id req)))
  ([name-or-path lang params query-params router language-settings-or-param]
   (localized-page nil name-or-path lang
                   params query-params
                   true false router
                   language-settings-or-param)))

(defn localized-path
  "Creates a URL on a basis of route name or a path. Uses very optimistic matching
  algorithm. Tries to obtain language from user settings and client settings if the
  path does not contain language information."
  ([req name-or-path]
   (let [rtr           (get req ::r/router)
         lang-settings (get req :language/settings)
         lang-param    (guess-lang-param-id lang-settings)
         lang          (pick-language-str req)]
     (localized-page nil name-or-path lang
                     nil nil true true
                     rtr lang-param)))
  ([req name-or-path lang]
   (localized-page nil name-or-path lang
                   nil nil true true
                   (get req ::r/router)
                   (lang-param-id req)))
  ([name-or-path lang params query-params router language-settings-or-param]
   (localized-page nil name-or-path lang
                   params query-params
                   true true
                   router language-settings-or-param)))

;; Anti-spam

(defn- random-uuid-or-empty
  ([]
   (random-uuid-or-empty nil))
  ([rng]
   (if (zero? (get-rand-int 2 rng))
     (random-uuid)
     "")))

(defn anti-spam-code
  "Generates anti-spam value pairs string containing randomly selected fields and
  values using `validators/gen-required`."
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
       (into {} r)))))

;; Template helpers

(selmer/add-tag!
 :lang-url
 (fn [args ctx]
   (let [path-or-name    (first args)
         args            (rest args)
         args            (if (map? (first args)) (cons nil args) args)
         [lang params
          query-params
          lang-settings] args]
     (lang-url true ctx path-or-name lang params query-params lang-settings))))

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
         out-path        (lang-url false ctx path-or-name lang params query-params lang-settings)]
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

