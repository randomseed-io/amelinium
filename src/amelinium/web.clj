(ns

    ^{:doc    "Web helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.java.io                      :as              io]
            [potemkin                             :as               p]
            [ring.util.response]
            [amelinium]
            [amelinium.types.response]
            [amelinium.http.response              :as            resp]
            [ring.util.request                    :as             req]
            [selmer.parser                        :as          selmer]
            [amelinium.db                         :as              db]
            [amelinium.i18n                       :as            i18n]
            [amelinium.common                     :as          common]
            [amelinium.errors                     :as          errors]
            [amelinium.http                       :as            http]
            [amelinium.logging                    :as             log]
            [amelinium.web.html                   :as            html]
            [amelinium.web.htmx                   :as            htmx]
            [amelinium.web.app-data               :as        app-data]
            [io.randomseed.utils.map              :as             map]
            [io.randomseed.utils                  :refer    [or-some
                                                             some-str
                                                             some-keyword
                                                             valuable?]])

  (:import (amelinium     Response)
           (clojure.lang  IFn)
           (java.io       File)))

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
                page localized-page strictly-localized-page
                current-page current-page-id current-page-id-or-path login-page auth-page
                temporary-redirect localized-temporary-redirect
                see-other localized-see-other])

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

;; HTML response

(defmacro response
  "Creates a response block. If the given `req` is already a response then it is simply
  returned. Otherwise the expressions from `body` are evaluated in an implicit `do`."
  [req & body]
  (if (and (seq? body) (> (count body) 1))
    `(let [req# ~req] (if (instance? Response req#) req# (do ~@body)))
    `(let [req# ~req] (if (instance? Response req#) req# ~@body))))

;; Layouts and views

(defn get-view
  "Gets a view partial path for the current route using `:app/view` route data or
  `:name`. If it cannot be extracted, returns default."
  ([req]
   (get-view req (http/get-route-data req)))
  ([req route-data]
   (let [view (get req :app/view)
         view (if (nil? view) (get route-data :app/view) view)]
     (if (false? view)
       false
       (or (some-str view)
           (some-str (http/req-or-route-param req :name))
           "default")))))

(defn get-layout
  "Gets layout partial path for the current route using :app/layout route data. If it
  cannot be extracted, returns default."
  ([req]
   (get-layout req (http/get-route-data req)))
  ([req route-data]
   (let [layout (get req :app/layout)
         layout (if (nil? layout) (get route-data :app/layout) layout)]
     (if (false? layout)
       false
       (or (some-str layout)
           "default")))))

(defn get-view-dir
  "Gets view optional subdirectory for the current route using `:app/layout-dir` route
  data. If it cannot be extracted, returns `nil`."
  ([req view-dir]
   (some-str (or view-dir (http/req-or-route-param req :app/view-dir))))
  ([req view-dir route-data]
   (some-str (or view-dir (get req :app/view-dir) (get route-data :app/view-dir)))))

(defn get-layout-dir
  "Gets layout optional subdirectory for the current route using :app/layout-dir route
  data. If it cannot be extracted, returns `nil`."
  ([req layout-dir]
   (some-str (or layout-dir (http/req-or-route-param req :app/layout-dir))))
  ([req layout-dir route-data]
   (some-str (or layout-dir (get req :app/layout-dir) (get route-data :app/layout-dir)))))

(def ^:const views-str               "views")
(def ^:const layouts-str           "layouts")
(def ^:const dot-html                ".html")
(def ^:const default-html     "default.html")
(def ^:const sl-default-html "/default.html")

(defn resolve-generic
  "Generic view resolver. Takes `uri`, prefix (`pre`), base directory (`dir`),
  language (`lang`) and core name (`core`), and tries different combinations of path
  segments based on these parameters until it finds an existing view file within
  java resource directories.

  If `dir` is `nil` or `false` it will create `auto-dir` by taking a directory part
  of `core` (if it has a directory part). Otherwise `auto-dir` will be same as `dir`.

  Returns a pathname as a string.

  Paths tried:

  - `pre`/`lang`/`dir`/`core`.html
  - `pre`/`lang`/`dir`/`core`/default.html
  - `pre`/`dir`/`core`.html
  - `pre`/`lang`/`auto-dir`/default.html
  - `pre`/`lang`/`auto-dir`.html           (if `auto-dir` is not `nil`)
  - `pre`/`auto-dir`/default.html          (if `auto-dir` is not `nil`)
  - `pre`/`auto-dir`.html                  (if `auto-dir` is not `nil`)
  - `pre`/`lang`/default.html
  - `pre`/default.html

  Example paths tried for `pre`=`\"views\"`, `lang`=`\"pl\"`,
                          `dir`=`nil` and `core`=`:login/prolongate`:

  - `views/pl/login/prolongate.html`
  - `views/pl/login/prolongate/default.html`
  - `views/login/prolongate.html`
  - `views/pl/prolongate/default.html`
  - `views/pl/prolongate.html`
  - `views/prolongate/default.html`
  - `views/prolongate.html`
  - `views/pl/default.html`
  - `views/default.html`

  Results are cached."

  [uri pre dir lang core]
  (let [core        (some-str core)
        pre         (or (some-str pre) "views")
        auto-dir    (or dir (.getName ^File (io/as-file core)))
        auto-dir-sl (when auto-dir (str auto-dir "/"))
        prep-sl     (when pre      (str pre  "/"))
        dir-sl      (when dir      (str dir  "/"))
        lang-sl     (when lang     (str lang "/"))
        pths        (lazy-cat [[prep-sl lang-sl dir-sl core dot-html]]
                              [[prep-sl lang-sl dir-sl core sl-default-html]]
                              [[prep-sl dir-sl core dot-html]]
                              [[prep-sl lang-sl auto-dir-sl default-html]]
                              (when auto-dir [[prep-sl lang-sl auto-dir dot-html]])
                              (when auto-dir [[prep-sl auto-dir-sl default-html]])
                              (when auto-dir [[prep-sl auto-dir dot-html]])
                              [[prep-sl lang-sl default-html]]
                              [[prep-sl default-html]])]
    (or (first (keep #(apply some-resource %) pths))
        (do (when (nil? uri) (log/wrn "[-]: Empty URI while resolving" pre))
            (log/wrn (str "[" uri "]: Cannot find") pre)
            (doseq [path pths] (log/wrn (apply str "[" uri "]: Tried [resources]/" path)))))))

(def ^{:arglists '([uri pre dir lang core])}
  resolve-cached
  (db/memoize+ resolve-generic 2048 256))

(defn resolve-layout
  "Returns a layout file for the given language `lang` and `layout` (a 2-element
  sequence of base directory and a layout name, or just a name as a single
  value). Uses `resolve-generic`.

  If no layout name is given it uses `get-layout` to obtain it from a route
  data (`:app/layout` key) or to fall back to default.

  If layout is set to `false`, explicitly disables using layout by returning
  `false` (may be helpful with partials rendered using AJAX calls)."
  ([req lang layout]
   (resolve-layout req lang layout (http/get-route-data req)))
  ([req lang layout route-data]
   (let [[ldir layout] (if (coll? layout) layout [nil layout])]
     (if (false? layout)
       false
       (let [layout (or layout (get-layout req route-data))]
         (if (false? layout)
           false
           (resolve-cached (get req :uri)
                           layouts-str
                           (get-layout-dir req ldir route-data)
                           lang layout)))))))

(defn resolve-view
  "Returns a view file for the given language `lang` and `view` (a 2-element sequence
  of base directory and a view name, or just a name as a single value). Uses
  `resolve-generic`.

  If no view name is given it uses `get-view` to obtain it from a route
  data (`:app/view` key) or to fall back to default.

  If view is set to `false`, explicitly disables using it by returning `false`."
  ([req lang view]
   (resolve-view req lang view (http/get-route-data req)))
  ([req lang view route-data]
   (let [[vdir view] (if (coll? view) view [nil view])]
     (if (false? view)
       false
       (let [view (or view (get-view req route-data))]
         (if (false? view)
           false
           (resolve-cached (get req :uri)
                           views-str
                           (get-view-dir req vdir route-data)
                           lang view)))))))

;; Response rendering

(p/import-vars [amelinium.common
                add-header add-headers add-status remove-status])

(defn get-for-status
  "If the given `status` is not `nil` and not `false`, it looks for `k` in `db` to
  obtain a map in which `status` is looked up.  If `other-status` is given, it will
  be used as fallback when first lookup within a map will return nil or `status` was
  falsy."
  ([db k status]
   (when status (get (get db k) status)))
  ([db k status other-status]
   (when-some [db (get db k)]
     (if status
       (or-some (get db status) (when other-status (get db other-status)))
       (when other-status (get db other-status))))))

(defn status-ok?
  "Returns `true` when the given `status` is either `:ok/found`, nil, `:ok` or
  `:info/early-hints`. False otherwise."
  [status]
  (if (identical? status :ok/found)
    true
    (or (nil?       status)
        (identical? status :ok)
        (identical? status :info/early-hints))))

(defn status-error?
  "Returns `true` when the given `status` is neither `:ok/found` nor nil nor `:ok` nor
  `:info/early-hints`. False otherwise."
  [status]
  (not (status-ok? status))

(defn- status-lv
  "Sets a different layout and/or view when the given HTTP status (`status`) does not
  equal to `:ok/found` nor `:ok` nor `:info/early-hints` nor `nil`, or when the
  `:response/set-status!` entry of the `req` has a truthy value.

  If `layout` is given and it is not `nil`, it will not be changed but returned as
  is. If `view` is given and it is not `nil`, it will not be changed but returned as
  is.

  To establish a layout the following lookups are made:

  - `:error/layout` is looked up within of `req`,
  - `:app/status` of `req` is looked up within `:status/layouts` of `req`,
  - `status` is looked up within `:status/layouts` of `req`,
  - `:app/status` of `req` is looked up within `:status/layouts` of `route-data`,
  - `status` is looked up within `:status/layouts` of `route-data`,
  - `:error/layout` is looked up within of `route-data`,
  - falls back to \"error\".

  To establish a view the following lookups are made:

  - `:error/view` is looked up within of `req`,
  - `:app/status` of `req` is looked up within `:status/views` of `req`,
  - `status` is looked up within `:status/views` of `req`,
  - `:app/status` of `req` is looked up within `:status/views` of `route-data`,
  - `status` is looked up within `:status/views` of `route-data`,
  - `:error/view` is looked up within of `route-data`,
  - falls back to \"error\".

  Returns a 2-element vector in a form of `[layout view]`."
  ([req]
   req)
  ([req status]
   (status-lv req status nil nil (http/get-route-data req)))
  ([req status layout]
   (status-lv req status layout nil (http/get-route-data req)))
  ([req status layout view]
   (status-lv req status layout view (http/get-route-data req)))
  ([req status layout view route-data]
   (when (or (status-error? status) (get req :response/set-status!))
     (let [no-layout? (nil? layout)
           no-view?   (nil? view)]
       (when (or no-layout? no-view?)
         (let [app-status (get req :app/status)]
           (log/web-dbg req "Getting layout/view for"
                        (when app-status (str "application status " (some-str app-status) " and"))
                        "HTTP response status" status)
           [(if no-layout?
              (or-some (get req :error/layout)
                       (get-for-status req :status/layouts app-status status)
                       (get-for-status route-data :status/layouts app-status status)
                       (get route-data :error/layout)
                       "error")
              layout)
            (if no-view?
              (or-some (get req :error/view)
                       (get-for-status req :status/views app-status status)
                       (get-for-status route-data :status/views app-status status)
                       (get route-data :error/view)
                       "error")
              view)]))))))

(defn render
  "HTML web page renderer. Takes a request, a data map to be used in templates, a name
  of the view file (defaults to `:app/view` from the `req`), a name of the template
  file (defaults to `:app/layout` from the `req`) and a language string (guessed if not
  given, unless explicitly set to `false`).

  Uses values associated with the `:layout/dir` and `:view/dir` keys of the `req` to
  obtain optional subdirectories to be looked up when searching for views and
  layouts.

  It will add `:status`, `:status/title` and `:status/description` entries
  to `:app/data` map (unless it already contains one), using configuration maps
  associated with the `:errors/config` key of a route data.

  In case of a regular response, when the given HTTP status (`http-status`) equals to
  `:ok/found` or `:ok` or `:info/early-hints` or `nil`, and when the
  `:response/set-status!` entry of the `req` has a falsy value a layout is
  established in a following way  (nil causes next lookup to be tried):

  - the given `layout`,
  - value of `:app/layout` (in a route data or a request map).

  In the same case a view is established:

  - the given `view`,
  - value of `:app/view` (in a route data or a request map).

  In case of an error response page, when the given HTTP status (`http-status`) does
  not equal to `:ok/found` nor `:ok` nor `:info/early-hints` nor `nil`, or when the
  `:response/set-status!` entry of the `req` has a truthy value a layout is
  established in a following way (nil causes next lookup to be tried):

  - the given `layout`,
  - `:error/layout` is looked up within of `req`,
  - `:app/status` of `req` is looked up within `:status/layouts` of `req`,
  - `status` is looked up within `:status/layouts` of `req`,
  - `:app/status` of `req` is looked up within `:status/layouts` of `route-data`,
  - `status` is looked up within `:status/layouts` of `route-data`,
  - `:error/layout` is looked up within of `route-data`,
  - plain string \"error\".

  In the same case a view is established:

  - the given `view`,
  - `:error/view` is looked up within of `req`,
  - `:app/status` of `req` is looked up within `:status/views` of `req`,
  - `status` is looked up within `:status/views` of `req`,
  - `:app/status` of `req` is looked up within `:status/views` of `route-data`,
  - `status` is looked up within `:status/views` of `route-data`,
  - `:error/view` is looked up within of `route-data`,
  - plain string \"error\"."
  ([]
   (render nil :ok/found nil nil nil nil))
  ([req]
   (render req :ok/found nil nil nil nil))
  ([req http-status]
   (render req http-status nil nil nil nil))
  ([req http-status data]
   (render req http-status data nil nil nil))
  ([req http-status data view]
   (render req http-status data view nil nil))
  ([req http-status data view layout]
   (render req http-status data view layout nil))
  ([req http-status data view layout lang]
   (let [uri        (get req :uri)
         lang       (when lang (some-str lang))
         lang       (if (false? lang) nil (common/pick-language-str req))
         route-data (http/get-route-data req)
         l-v        (status-lv req http-status layout view route-data)
         layt       (resolve-layout req lang (if l-v (nth l-v 0) layout) route-data)
         view       (resolve-view   req lang (if l-v (nth l-v 1)   view) route-data)]
     (if (or layt view)
       (do (log/web-dbg req "Rendering (layout:" layt "view:" (str view ")"))
           (let [dlng (or lang (get req :language/str))
                 data (app-data/prep req data)
                 data (map/assoc-missing data
                                         :uri                uri
                                         :url                (delay (req/request-url        req))
                                         :character-encoding (delay (req/character-encoding req))
                                         :path               (delay (common/page            req))
                                         :htmx-request?      (delay (htmx/request?          req))
                                         :lang               dlng)
                 data (app-data/update-status data req http-status dlng)
                 html (if view (selmer/render-file view data) "")
                 rndr (map/qassoc data :body [:safe html])
                 resp (if layt (selmer/render-file layt rndr) html)]
             resp))
       (do (log/web-err req "Rendering empty document since no layout nor view was set")
           "")))))

(defn render-response
  "Web response renderer. Uses the `render` function to render a response body (using
  values associated with the `:app/data`, `:app/view`, `:app/layout`, `:app/view-dir`
  and `:app/layout-dir` in the `req` map, or provided as arguments) and response
  headers (using the `:response/headers` value), unless the `req` is already a valid
  response.

  It will add `:status`, `:status/title` and `:status/description` entries
  to `:app/data` map (unless it already contains one), using configuration maps
  associated with the `:errors/config` key of a route data.

   In case of a regular response, when the given HTTP status (`http-status`) equals to
  `:ok/found` or `:ok` or `:info/early-hints` or `nil`, and when the
  `:response/set-status!` entry of the `req` has a falsy value a layout is
  established in a following way  (nil causes next lookup to be tried):

  - the given `layout`,
  - value of `:app/layout` (in a route data or a request map).

  In the same case a view is established:

  - the given `view`,
  - value of `:app/view` (in a route data or a request map).

  In case of an error response page, when the given HTTP status (`http-status`) does
  not equal to `:ok/found` nor `:ok` nor `:info/early-hints` nor `nil`, or when the
  `:response/set-status!` entry of the `req` has a truthy value a layout is
  established in a following way (nil causes next lookup to be tried):

  - the given `layout`,
  - `:error/layout` is looked up within of `req`,
  - `:app/status` of `req` is looked up within `:status/layouts` of `req`,
  - `status` is looked up within `:status/layouts` of `req`,
  - `:app/status` of `req` is looked up within `:status/layouts` of `route-data`,
  - `status` is looked up within `:status/layouts` of `route-data`,
  - `:error/layout` is looked up within of `route-data`,
  - plain string \"error\".

  In the same case a view is established:

  - the given `view`,
  - `:error/view` is looked up within of `req`,
  - `:app/status` of `req` is looked up within `:status/views` of `req`,
  - `status` is looked up within `:status/views` of `req`,
  - `:app/status` of `req` is looked up within `:status/views` of `route-data`,
  - `status` is looked up within `:status/views` of `route-data`,
  - `:error/view` is looked up within of `route-data`,
  - plain string \"error\"."
  (^Response []
   (render-response resp/ok :ok/found nil nil nil nil nil))
  (^Response [resp-fn]
   (render-response resp-fn nil nil nil nil nil nil))
  (^Response [resp-fn req]
   (render-response resp-fn nil req nil nil nil nil))
  (^Response [resp-fn status req]
   (render-response resp-fn status req nil nil nil nil))
  (^Response [resp-fn status req data]
   (render-response resp-fn status req data nil nil nil))
  (^Response [resp-fn status req data view]
   (render-response resp-fn status req data view nil nil))
  (^Response [resp-fn status req data view layout]
   (render-response resp-fn status req data view layout nil))
  (^Response [resp-fn status req data view layout lang]
   (if (resp/response? req)
     req
     (-> (htmx/set-target-header req)
         (render status data view layout lang)
         (resp-fn (or (resp/headers req) {}))))))

(defn render-response-force
  "Web response renderer. Uses the `render` function to render a response body
  (using values associated with the `:app/data`, `:app/view` and `:app/layout` in the
  `req` map, or provided as arguments) and the response headers (using the
  `:response/headers` value), regardless if the `req` is already a valid response or
  not.

  It will add `:status`, `:status/title` and `:status/description` entries
  to `:app/data` map (unless it already contains one), using configuration maps
  associated with the `:errors/config` key of a route data.

   In case of a regular response, when the given HTTP status (`http-status`) equals to
  `:ok/found` or `:ok` or `:info/early-hints` or `nil`, and when the
  `:response/set-status!` entry of the `req` has a falsy value a layout is
  established in a following way  (nil causes next lookup to be tried):

  - the given `layout`,
  - value of `:app/layout` (in a route data or a request map).

  In the same case a view is established:

  - the given `view`,
  - value of `:app/view` (in a route data or a request map).

  In case of an error response page, when the given HTTP status (`http-status`) does
  not equal to `:ok/found` nor `:ok` nor `:info/early-hints` nor `nil`, or when the
  `:response/set-status!` entry of the `req` has a truthy value a layout is
  established in a following way (nil causes next lookup to be tried):

  - the given `layout`,
  - `:error/layout` is looked up within of `req`,
  - `:app/status` of `req` is looked up within `:status/layouts` of `req`,
  - `status` is looked up within `:status/layouts` of `req`,
  - `:app/status` of `req` is looked up within `:status/layouts` of `route-data`,
  - `status` is looked up within `:status/layouts` of `route-data`,
  - `:error/layout` is looked up within of `route-data`,
  - plain string \"error\".

  In the same case a view is established:

  - the given `view`,
  - `:error/view` is looked up within of `req`,
  - `:app/status` of `req` is looked up within `:status/views` of `req`,
  - `status` is looked up within `:status/views` of `req`,
  - `:app/status` of `req` is looked up within `:status/views` of `route-data`,
  - `status` is looked up within `:status/views` of `route-data`,
  - `:error/view` is looked up within of `route-data`,
  - plain string \"error\"."
  ([]
   (render-response-force resp/ok :ok/found nil nil nil nil nil))
  ([resp-fn]
   (render-response-force resp-fn nil nil nil nil nil nil))
  ([resp-fn req]
   (render-response-force resp-fn nil req nil nil nil nil))
  ([resp-fn status req]
   (render-response-force resp-fn status req nil nil nil nil))
  ([resp-fn status req data]
   (render-response-force resp-fn status req data nil nil nil))
  ([resp-fn status req data view]
   (render-response-force resp-fn status req data view nil nil))
  ([resp-fn status req data view layout]
   (render-response-force resp-fn status req data view layout nil))
  ([resp-fn status req data view layout lang]
   (-> (htmx/set-target-header req)
       (render status data view layout lang)
       (resp-fn (or (resp/headers req) {})))))

;; Rendering functions generation

(defmacro def-render
  "Generates a web rendering function."
  {:arglists '([name f]
               [name f status]
               [name doc f]
               [name doc f status])}
  ([name f]
   (#'def-render &form &env name f nil))
  ([name f-or-doc status-or-f]
   (let [[f doc status] (if (string? f-or-doc)
                          [status-or-f f-or-doc nil]
                          [f-or-doc nil status-or-f])
         status         (keyword status)]
     (if doc
       (#'def-render &form &env name doc f status)
       (#'def-render
        &form &env name
        (str "Renders a " (when status (name status " "))
             "response with a possible body generated with views, layouts and data \n  "
             "obtained from a request map (`:app/layout`, `:app/view`, `:app/data` keys).\n  "
             "Uses `" f-or-doc "` to set the response code."
             (when status
               (str " Additionaly, associates `:status` key\n  "
                    "with `" status "` in `:app/data` "
                    "by passing it as an argument to `render-response`\n  "
                    "(which will also set the `:status/title` "
                    "and `:status/description` if possible).")))
        f status))))
  ([name doc f status]
   `(let [^IFn f# ~f
          c#      ~status
          c#      (if c# (keyword c#))]
      (defn ~name ~doc
        (^Response []
         (render-response f# c# nil nil nil nil nil))
        (^Response ~'[req]
         (render-response f# c# ~'req nil nil nil nil))
        (^Response ~'[req data]
         (render-response f# c# ~'req ~'data nil nil nil))
        (^Response ~'[req data view]
         (render-response f# c# ~'req ~'data ~'view nil nil))
        (^Response ~'[req data view layout]
         (render-response f# c# ~'req ~'data ~'view ~'layout nil))
        (^Response ~'[req data view layout lang]
         (render-response f# c# ~'req ~'data ~'view ~'layout ~'lang))))))

;; OK response

(def-render render-ok    resp/ok :ok/found)
(def-render render-page  resp/ok :ok/found)
(def-render render-found resp/ok :ok/found)

;; Success responses with bodies

(def-render render-accepted                        resp/accepted                        :ok/accepted)
(def-render render-non-authoritative-information   resp/non-authoritative-information   :ok/non-authoritative-information)
(def-render render-partial-content                 resp/partial-content                 :ok/partial-content)
(def-render render-multi-status                    resp/multi-status                    :ok/multi-status)
(def-render render-already-reported                resp/already-reported                :ok/already-reported)
(def-render render-im-used                         resp/im-used                         :ok/im-used)

;; Informational responses with bodies

(def-render render-early-hints                     resp/early-hints                     :info/early-hints)

;; Error responses with possible bodies

(def-render render-bad-request                     resp/bad-request                     :error/bad-request)
(def-render render-unauthorized                    resp/unauthorized                    :error/unauthorized)
(def-render render-payment-required                resp/payment-required                :error/payment-required)
(def-render render-forbidden                       resp/forbidden                       :error/forbidden)
(def-render render-not-found                       resp/not-found                       :error/not-found)
(def-render render-method-not-allowed              resp/method-not-allowed              :error/method-not-allowed)
(def-render render-not-acceptable                  resp/not-acceptable                  :error/not-acceptable)
(def-render render-proxy-authentication-required   resp/proxy-authentication-required   :error/proxy-authentication-required)
(def-render render-request-timeout                 resp/request-timeout                 :error/request-timeout)
(def-render render-conflict                        resp/conflict                        :error/conflict)
(def-render render-gone                            resp/gone                            :error/gone)
(def-render render-length-required                 resp/length-required                 :error/length-required)
(def-render render-precondition-failed             resp/precondition-failed             :error/precondition-failed)
(def-render render-request-entity-too-large        resp/request-entity-too-large        :error/request-entity-too-large)
(def-render render-request-uri-too-long            resp/request-uri-too-long            :error/request-uri-too-long)
(def-render render-unsupported-media-type          resp/unsupported-media-type          :error/unsupported-media-type)
(def-render render-requested-range-not-satisfiable resp/requested-range-not-satisfiable :error/requested-range-not-satisfiable)
(def-render render-expectation-failed              resp/expectation-failed              :error/expectation-failed)
(def-render render-im-a-teapot                     resp/im-a-teapot                     :error/im-a-teapot)
(def-render render-enhance-your-calm               resp/enhance-your-calm               :error/enhance-your-calm)
(def-render render-misdirected-request             resp/misdirected-request             :error/misdirected-request)
(def-render render-unprocessable-entity            resp/unprocessable-entity            :error/unprocessable-entity)
(def-render render-bad-params                      resp/unprocessable-entity            :error/bad-parameters)
(def-render render-locked                          resp/locked                          :error/render-locked)
(def-render render-failed-dependency               resp/failed-dependency               :error/failed-dependency)
(def-render render-unordered-collection            resp/unordered-collection            :error/unordered-collection)
(def-render render-too-early                       resp/unordered-collection            :error/too-early)
(def-render render-upgrade-required                resp/upgrade-required                :error/upgrade-required)
(def-render render-precondition-required           resp/precondition-required           :error/precondition-failed)
(def-render render-too-many-requests               resp/too-many-requests               :error/too-many-requests)
(def-render render-request-header-fields-too-large resp/request-header-fields-too-large :error/request-header-fields-too-large)
(def-render render-retry-with                      resp/retry-with                      :error/retry-with)
(def-render render-blocked-by-windows-parental-controls resp/blocked-by-windows-parental-controls :error/blocked-by-windows-parental-controls)
(def-render render-unavailable-for-legal-reasons   resp/unavailable-for-legal-reasons   :error/unavailable-for-legal-reasons)
(def-render render-internal-server-error           resp/internal-server-error           :server-error/internal)
(def-render render-not-implemented                 resp/not-implemented                 :server-error/not-implemented)
(def-render render-bad-gateway                     resp/bad-gateway                     :server-error/bad-gateway)
(def-render render-service-unavailable             resp/service-unavailable             :server-error/service-unavailable)
(def-render render-gateway-timeout                 resp/gateway-timeout                 :server-error/gateway-timeout)
(def-render render-http-version-not-supported      resp/http-version-not-supported      :server-error/http-version-not-supported)
(def-render render-variant-also-negotiates         resp/variant-also-negotiates         :server-error/variant-also-negotiates)
(def-render render-insufficient-storage            resp/insufficient-storage            :server-error/insufficient-storage)
(def-render render-loop-detected                   resp/loop-detected                   :server-error/loop-detected)
(def-render render-bandwidth-limit-exceeded        resp/bandwidth-limit-exceeded        :server-error/bandwidth-limit-exceeded)
(def-render render-not-extended                    resp/not-extended                    :server-error/not-extended)
(def-render render-network-authentication-required resp/network-authentication-required :server-error/network-authentication-required)
(def-render render-network-read-timeout            resp/network-read-timeout            :server-error/read-timeout)
(def-render render-network-connect-timeout         resp/network-connect-timeout         :server-error/connect-timeout)

;; Resource creation success, redirect with a possible body

(defn render-created
  "Renders 201 response with a redirect (possibly localized if a destination path is
  language-parameterized) and a possible body. See `render` documentation to know
  more about body rendering. The destination for a redirect is taken from
  `name-or-path` argument or, if not given, from the `:response/location` key of the
  given request map (`req`)."
  (^Response []
   (resp/created))
  (^Response [req]
   (resp/created (render req :ok/created nil nil nil nil)
                 (or (resp/headers req) {})
                 (resp/location req page)))
  (^Response [req data]
   (resp/created (render req :ok/created data nil nil nil)
                 (or (resp/headers req) {})
                 (resp/location req page)))
  (^Response [req data view]
   (resp/created (render req :ok/created data view nil nil)
                 (or (resp/headers req) {})
                 (resp/location req page)))
  (^Response [req data view layout]
   (resp/created (render req :ok/created data view layout nil)
                 (or (resp/headers req) {})
                 (resp/location req page)))
  (^Response [req data view layout lang]
   (resp/created (render req :ok/created data view layout lang)
                 (or (resp/headers req) {})
                 (resp/location req page lang)))
  (^Response [req data view layout lang name-or-path]
   (resp/created (render req :ok/created data view layout lang)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (page req name-or-path lang))))
  (^Response [req data view layout lang name-or-path params]
   (resp/created (render req :ok/created data view layout lang)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (page req name-or-path lang params))))
  (^Response [req data view layout lang name-or-path params query-params]
   (resp/created (render req :ok/created data view layout lang)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (page req name-or-path lang params query-params)))))

(defn localized-render-created
  "Renders 201 response with a redirect (possibly localized if a destination path is
  language-parameterized) and a possible body. Requires the destination
  URL (specified by arguments or by the `:response/location` key of the given `req`)
  to be language parameterized. See `render` documentation to know more about body
  rendering."
  (^Response []
   (resp/created))
  (^Response [req]
   (resp/created (render req :ok/created nil nil nil nil)
                 (or (resp/headers req) {})
                 (resp/location req localized-page)))
  (^Response [req data]
   (resp/created (render req :ok/created data nil nil nil)
                 (or (resp/headers req) {})
                 (resp/location req localized-page)))
  (^Response [req data view]
   (resp/created (render req :ok/created data view nil nil)
                 (or (resp/headers req) {})
                 (resp/location req localized-page)))
  (^Response [req data view layout]
   (resp/created (render req :ok/created data view layout nil)
                 (or (resp/headers req) {})
                 (resp/location req localized-page)))
  (^Response [req data view layout lang]
   (resp/created (render req :ok/created data view layout lang)
                 (or (resp/headers req) {})
                 (resp/location req localized-page lang)))
  (^Response [req data view layout lang name-or-path]
   (resp/created (render req :ok/created data view layout lang)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (localized-page req name-or-path lang))))
  (^Response [req data view layout lang name-or-path params]
   (resp/created (render req :ok/created data view layout lang)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (localized-page req name-or-path lang params))))
  (^Response [req data view layout lang name-or-path params query-params]
   (resp/created (render req :ok/created data view layout lang)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (localized-page req name-or-path lang params query-params)))))

;; Responses without bodies

(defn render-continue
  "Renders 100 response without a body."
  (^Response []        (resp/continue))
  (^Response [req]     (resp/continue nil (resp/headers req)))
  (^Response [req & _] (resp/continue nil (resp/headers req))))

(defn render-switching-protocols
  "Renders 101 response without a body."
  (^Response []        (resp/switching-protocols))
  (^Response [req]     (resp/switching-protocols nil (resp/headers req)))
  (^Response [req & _] (resp/switching-protocols nil (resp/headers req))))

(defn render-processing
  "Renders 102 response without a body."
  (^Response []        (resp/processing))
  (^Response [req]     (resp/processing nil (resp/headers req)))
  (^Response [req & _] (resp/processing nil (resp/headers req))))

(defn render-no-content
  "Renders 204 response without a body."
  (^Response []        (resp/no-content))
  (^Response [req]     (resp/no-content nil (resp/headers req)))
  (^Response [req & _] (resp/no-content nil (resp/headers req))))

(defn render-reset-content
  "Renders 205 response without a body."
  (^Response []        (resp/reset-content))
  (^Response [req]     (resp/reset-content nil (resp/headers req)))
  (^Response [req & _] (resp/reset-content nil (resp/headers req))))

;; Rendering based on application-logic error

(defn add-missing-sub-status
  ([req sub-status sub-key]
   (let [sub-ns        (name sub-key)
         sub-title-key (keyword sub-ns "title")
         sub-desc-key  (keyword sub-ns "description")]
     (update-status req sub-status nil sub-key sub-title-key sub-desc-key)))
  ([data req sub-status sub-key]
   (let [sub-ns        (name sub-key)
         sub-title-key (keyword sub-ns "title")
         sub-desc-key  (keyword sub-ns "description")]
     (update-status data req sub-status nil sub-key sub-title-key sub-desc-key)))
  ([data req sub-status lang sub-key title-key description-key]
   (update-status data req sub-status lang sub-key title-key description-key))
  ([req sub-status lang sub-key title-key description-key]
   (update-status req sub-status lang sub-key title-key description-key)))

(defn render-error
  "Renders error response on a basis of `app-status` or `app-statuses`, and optional
  `default` rendering function (which is used when no function can be found by
  looking up status in error configuration obtained from a request map). Optional
  `data` should be a data map merged with existing data map (from the request) and
  used during rendering to provide data for templates.

  Optional `layout`, `lang` and other arguments are passed to
  `amelinium.errors/render` function and then are passed to the established rendering
  function.

  When the given status is not mapped to any rendering function and there is no
  default function given, `render-internal-server-error` is used."
  {:arglists '(^Response []
               ^Response [req]
               ^Response [req app-status]
               ^Response [req app-statuses]
               ^Response [req app-status default]
               ^Response [req app-statuses default]
               ^Response [req app-status default data]
               ^Response [req app-statuses default data]
               ^Response [req app-status default data view]
               ^Response [req app-statuses default data view]
               ^Response [req app-status default data view layout]
               ^Response [req app-statuses default data view layout]
               ^Response [req app-status default data view layout lang]
               ^Response [req app-statuses default data view layout lang]
               ^Response [req app-status default data view layout lang & more]
               ^Response [req app-statuses default data view layout lang & more])}
  (^Response []
   (render-internal-server-error))
  (^Response [req]
   (response
    req
    (errors/render req nil render-internal-server-error req)))
  (^Response [req app-status]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          req        (app-data/update-status req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status render-internal-server-error req))))
  (^Response [req app-status default]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          req        (app-data/update-status req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req))))
  (^Response [req app-status default data]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req data))))
  (^Response [req app-status default data view]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req data view))))
  (^Response [req app-status default data view layout]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req data view layout))))
  (^Response [req app-status default data view layout lang]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status lang :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req data view layout lang))))
  (^Response [req app-status default data view layout lang & more]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status lang :app-status :app-status/title :app-status/description)]
      (apply errors/render err-config app-status (or default render-internal-server-error) req data view layout lang more)))))

(defn render-status
  "Renders status response on a basis of `app-status` or `app-statuses`, and optional
  `default` rendering function (which is used when no function can be found by
  looking up status in error configuration obtained from a request map). Optional
  `data` should be a data map merged with existing data map (from the request) and
  used during rendering to provide data for templates.

  Optional `layout`, `lang` and other arguments are passed to
  `amelinium.errors/render` function and then are passed to the established rendering
  function.

  When the given status is not mapped to any rendering function and there is no
  default function given, `render-ok` is be used."
  {:arglists '(^Response []
               ^Response [req]
               ^Response [req app-status]
               ^Response [req app-statuses]
               ^Response [req app-status default]
               ^Response [req app-statuses default]
               ^Response [req app-status default data]
               ^Response [req app-statuses default data]
               ^Response [req app-status default data view]
               ^Response [req app-statuses default data view]
               ^Response [req app-status default data view layout]
               ^Response [req app-statuses default data view layout]
               ^Response [req app-status default data view layout lang]
               ^Response [req app-statuses default data view layout lang]
               ^Response [req app-status default data view layout lang & more]
               ^Response [req app-statuses default data view layout lang & more])}
  (^Response []
   (render-ok))
  (^Response [req]
   (response req (errors/render req nil render-ok req)))
  (^Response [req app-status]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          req        (app-data/update-status req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status render-ok req))))
  (^Response [req app-status default]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          req        (app-data/update-status req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req))))
  (^Response [req app-status default data]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req data))))
  (^Response [req app-status default data view]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req data view))))
  (^Response [req app-status default data view layout]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req data view layout))))
  (^Response [req app-status default data view layout lang]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status lang :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req data view layout lang))))
  (^Response [req app-status default data view layout lang & more]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (app-data/update-status data req app-status lang :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (apply errors/render err-config app-status (or default render-ok) req data view layout lang more)))))

;; Redirect wrappers

(defn- go-to-fn
  ^IFn [req]
  (if (htmx/use? req nil false) htmx/go-to html/go-to))

(defn- move-to-fn
  ^IFn [req]
  (if (htmx/use? req nil false) htmx/move-to html/move-to))

(defn go-to
  "When HTMX is detected with `amelinium.web.htmx/use?` calls
  `amelinium.web.htmx/go-to`, otherwise calls `amelinium.web.html/go-to`."
  {:arglists '(^Response [req]
               ^Response [req url]
               ^Response [req name-or-path]
               ^Response [req name-or-path path-params]
               ^Response [req name-or-path path-params query-params]
               ^Response [req name-or-path lang]
               ^Response [req name-or-path lang path-params]
               ^Response [req name-or-path lang path-params query-params]
               ^Response [req name-or-path lang path-params query-params & more])}
  (^Response [req]
   ((go-to-fn req) req))
  (^Response [req name-or-path]
   ((go-to-fn req) req name-or-path))
  (^Response [req name-or-path lang]
   ((go-to-fn req) req name-or-path lang))
  (^Response [req name-or-path lang params]
   ((go-to-fn req) req name-or-path lang params))
  (^Response [req name-or-path lang params query-params]
   ((go-to-fn req) req name-or-path lang params query-params))
  (^Response [req name-or-path lang params query-params & more]
   (apply (go-to-fn req) req name-or-path lang params query-params more)))

(defn move-to
  "When HTMX is detected with `amelinium.web.htmx/use?` calls
  `amelinium.web.htmx/move-to`, otherwise calls `amelinium.web.html/move-to`."
  {:arglists '(^Response [req]
               ^Response [req url]
               ^Response [req name-or-path]
               ^Response [req name-or-path path-params]
               ^Response [req name-or-path path-params query-params]
               ^Response [req name-or-path lang]
               ^Response [req name-or-path lang path-params]
               ^Response [req name-or-path lang path-params query-params]
               ^Response [req name-or-path lang path-params query-params & more])}
  (^Response [req]
   ((move-to-fn req) req))
  (^Response [req name-or-path]
   ((move-to-fn req) req name-or-path))
  (^Response [req name-or-path lang]
   ((move-to-fn req) req name-or-path lang))
  (^Response [req name-or-path lang params]
   ((move-to-fn req) req name-or-path lang params))
  (^Response [req name-or-path lang params query-params]
   ((move-to-fn req) req name-or-path lang params query-params))
  (^Response [req name-or-path lang params query-params & more]
   (apply (move-to-fn req) req name-or-path lang params query-params more)))

(defn go-to-with-status
  "Uses `amelinium.web.htmx/go-to-with-status` when HTMX is in use (uses
  `amelinium.web.htmx/use?`), `amelinium.web.html/go-to-with-status` otherwise.
  Takes additional `hx-status-flag` as a key to be checked in route data whether it
  is associated with a HTMX-enforcement flag."
  (^Response [req]
   (go-to-with-status req nil :error nil false))
  (^Response [req app-status]
   (go-to-with-status req nil app-status nil false))
  (^Response [req app-status default-page]
   (go-to-with-status req nil app-status default-page false))
  (^Response [req _route-data app-status default-page]
   (go-to-with-status req nil app-status default-page false))
  (^Response [req route-data app-status default-page hx-status-flag]
   (let [route-data (or route-data (http/get-route-data req))
         go-to-fn   (if (htmx/use? req route-data hx-status-flag)
                      htmx/go-to-with-status
                      html/go-to-with-status)]
     (go-to-fn req
               (or (get-in route-data [:error/destinations app-status] default-page)
                   (get route-data :error/destination))))))

;; Business logic errors

(defn handle-error
  "Sets the right response function (which sets HTTP status code) and optional HTTP
  header on a basis of the given `app-status`. If `app-status` is sequential and contains
  multiple statuses, it will be replaced by the most accurate one.

  Returns proper HTMX response when:
  - `amelinium.web.htmx/use?` returns `true` because the request indicated it is HTMX,
  - or `:error/use-htmx?` route data key is set,
  - or generic `:use-htmx?` route data key is set.

  Returns a generic HTTP redirect response (if HTMX is not in use or is disabled),
  as a result of error encountered.

  Additionally, sets an HTTP response header named `header-name` (if set) with error
  status detected (mainly to be used by reverse proxies). If header name is `false`,
  no header is set. If header name is not set or is set to `nil`, the name `Error` is
  used. Returns updated `req`.

  For HTMX uses `amelinium.web.htmx/go-to-with-status`, for regular web uses
  `amelinium.web.html/go-to-with-status`."
  (^Response [req]
   (handle-error req nil :auth/error nil nil))
  (^Response [req app-status]
   (handle-error req nil app-status nil nil))
  (^Response [req app-status default-view]
   (handle-error req nil app-status default-view nil))
  (^Response [req route-data app-status default-view]
   (handle-error req route-data app-status default-view nil))
  (^Response [req route-data app-status default-view header-name]
   (let [route-data    (or route-data (http/get-route-data req))
         app-st-single (when-not (keyword? app-status)
                         (errors/most-significant (get route-data :errors/config) app-status))
         reduced?      (and app-st-single (not (identical? app-status app-st-single)))
         app-status    (if reduced? app-st-single app-status)
         req           (if reduced? (map/qassoc req :app/status app-status) req)]
     (log/web-dbg req "Handling status:" app-status)
     (let [header-name    (cond (nil? header-name)   "Error"
                                (false? header-name) nil
                                :else                (or (some-str header-name) "Error"))
           str-app-status (some-str app-status)
           req            (if (and str-app-status header-name)
                            (add-header req header-name str-app-status)
                            req)]
       (if (htmx/use? req route-data :error/use-htmx?)
         (htmx/go-to-with-status req route-data app-status default-view)
         (html/go-to-with-status req route-data app-status default-view))))))

(defn response!
  "Returns a response on a basis of `app-status` (or `:app/status` key of the
  `req` if status is not given).

  Optional `fallback-view` will be used if there is no assignment of a view to the
  application status in a map under the key `:` of route data.

  Optional `error-header` may be given which will be used to communicate status. If
  it is not given, application status does not exist, or is set to `false` or `nil`,
  then it will not be used. Otherwise the header of the given name will be added to
  a response with a string representation of the status as its value.

  Optional route data map may be given as `route-data` and will be used instead
  of getting it from the request map.

  If there is no application status or the given `req` is already a response, it
  simply returns an unmodified request map."
  (^Response [req]
   (response! req nil nil nil nil))
  (^Response [req app-status]
   (response! req app-status nil nil nil))
  (^Response [req app-status fallback-view]
   (response! req app-status fallback-view nil nil))
  (^Response [req app-status fallback-view error-header]
   (response! req app-status fallback-view error-header nil))
  (^Response [req app-status fallback-view error-header route-data]
   (response
    req
    (if-let [status (some-keyword (or app-status (get req :app/status)))]
      (handle-error req
                    (or route-data (http/get-route-data req))
                    status
                    fallback-view
                    (when error-header error-header))
      req))))

;; Form errors

(defn handle-bad-request-form-params
  "Dispatch function which calls `amelinium.web.htmx/handle-bad-request-form-params`
  when HTMX is in use or `amelinium.web.html/handle-bad-request-form-params` in other
  cases. HTMX mode will be in place when a client request contains `HX-Request`
  header set to a non-falsy and not empty value, or when it was enforced by using
  route data key `:form-errors/use-htmx?` set to a truthy value."
  ([req]
   (handle-bad-request-form-params req nil nil nil nil nil))
  ([req errors]
   (handle-bad-request-form-params req errors nil nil nil nil))
  ([req errors values]
   (handle-bad-request-form-params req errors values nil nil nil))
  ([req errors values explanations]
   (handle-bad-request-form-params req errors values explanations nil nil))
  ([req errors values explanations title]
   (handle-bad-request-form-params req errors values explanations title nil))
  ([req errors values explanations title session-key]
   (let [route-data (http/get-route-data req)]
     (if (htmx/use? req route-data :form-errors/use-htmx?)
       (htmx/handle-bad-request-form-params req route-data errors values explanations title)
       (html/handle-bad-request-form-params req route-data errors values explanations title session-key)))))

(defn- param-current-vals
  [req]
  (when-let [form-params (get req :form-params)]
    (let [good-params (->> (common/get-form-params req)
                           (map/remove-empty-values)
                           (map/map-keys some-str))]
      (->> form-params
           (map/map-vals-by-k #(when (contains? good-params %) (some-str (get form-params %))))
           (map/remove-empty-values)))))

(defn- param-errors-stringify
  [errors]
  (if errors
    (reduce-kv #(map/qassoc %1 (some-str %2) (some-str %3)) errors errors)))

(defn- param-errors-stringify-vals
  [errors]
  (map/map-vals some-str errors))

(defn form-params-error!
  "Called to manually generate a redirect or display a page with a form to be corrected
  because of a parameter error. Takes a request map `req`, a map of erroneous
  parameter identifiers to parameter types `errors`, and optional session key
  `session-key` used when getting a session object from the `req`.

  Parameter type in `errors` map can be `nil`, meaning it is of unknown type.

  This function can work in two modes: HTMX and simple HTTP mode.

  HTMX mode is in place when a client request contains the `HX-Request` header set to
  a non-falsy and not empty value, or it was enforced by using route data key
  `:form-errors/use-htmx?` set to a truthy value.

  In HTMX mode the HTML result is rendered with layout and view obtained from the
  `:form-errors/page` configuration option associated with HTTP route data or the
  destination page established by checking the `Referer` header, unless
  `:form-errors/layout` and/or `:form-errors/view` options are set. Layout can be set
  to `false` to allow injection of HTML fragments. If form errors page is not
  specified one is obtained from the `Referer` request header. See
  `amelinium.web.htmx/handle-bad-request-form-params` for more info.

  If simple HTTP mode is in place the
  `amelinium.web.html/handle-bad-request-form-params` is used, and the following will
  happen:

  If there is a session then the `errors` map is stored in a session variable
  `:form-errors` under the `:errors` key (additionally, there is a `:dest` key
  identifying a path of the current page), unless an HTMX response is to be generated
  by `handle-bad-request-form-params` which is called internally).

  If there is no valid session or a session variable cannot be stored, the result is
  serialized as a query string parameter `form-errors` with erroneous fields
  separated by commas.

  If type name is available for a parameter then a string in a form of
  `parameter:type` is generated.

  If type name is not available, a simple parameter name is generated. So the example
  value (before encoding) may look like `email,secret:password` (`email` being a
  parameter without type information, `secret` being a parameter with type named
  `password`).

  Next, the originating URI is obtained from the `Referer` header and a temporary
  redirect (with HTTP code 307) is generated with this path and a query string
  containing `form-errors` parameter. The value of the parameter is empty if form
  errors were saved in a session variable.

  The destination of the redirect can be overriden by the `:form-errors/page`
  configuration option associated with HTTP route data.

  If the destination URI cannot be established, or if a coercion error happened
  during handling some previous coercion error (so the current page is where the
  browser had been redirected to), then instead of generating a redirect, a regular
  page is rendered with HTTP code of 422. The `:app/data` key of a request map is
  updated with:

  - `:title` set to a translated message of `:parameters/error`,
  - `:form/errors` containing a map:
    - `:errors` mapped to `errors`,
    - `:params` mapped to `values`,
    - `:dest` mapped to destination URI;
  - `:coercion/errors` set to `nil`."
  ([req errors]
   (form-params-error! req errors nil))
  ([req errors session-key]
   (let [translate-sub (i18n/no-default (common/translator-sub req))]
     (if-not (valuable? errors)
       (app-data/assoc req
                       :title (delay (translate-sub :parameters/error))
                       :form/errors nil
                       :coercion/errors nil)
       (render-bad-params
        (handle-bad-request-form-params (delay (param-errors-stringify errors))
                                        (delay (param-current-vals req))
                                        nil
                                        (delay (translate-sub :parameters/error))
                                        session-key))))))

;; Linking helpers

(p/import-vars [amelinium.common
                path localized-path])

;; Language helpers

(p/import-vars [amelinium.common
                lang-id lang-str lang-config lang-from-req])
