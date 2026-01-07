(ns

    ^{:doc    "HTMX helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.htmx

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [ring.util.response]
            [amelinium.types.response]
            [amelinium.http.response              :as            resp]
            [jsonista.core                        :as               j]
            [amelinium                            :refer [->Response]]
            [amelinium.common                     :as          common]
            [amelinium.http                       :as            http]
            [amelinium.http.middleware.session    :as         session]
            [amelinium.logging                    :as             log]
            [amelinium.web.app-data               :as        app-data]
            [io.randomseed.utils.map              :as map  :refer [qassoc]]
            [io.randomseed.utils                  :refer   [strb
                                                            some-str
                                                            parse-url
                                                            valuable?]])

  (:import (amelinium    Response)
           (clojure.lang IFn)))

;; HTMX detection

(defn request?
  "Returns `true` if the client request has `HX-Request` header set to any value but
  a \"false\" or an empty string."
  ^Boolean [req]
  (if-some [hdr (get (get req :headers) "hx-request")]
    (if-some [hdr (some-str hdr)]
      (not= "false" hdr)
      false)
    false))

(defn use?
  "Returns `true` if a response to the current request should be HTMX-compatible,
  `false` if it should not.

  Uses route data as a source of truth. It can be given as a `route-data` argument
  or will be extracted from the `req` map.

  It looks for the key passed as `extra-key` (with fallback to `:use-htmx?`)
  in a route data. The value associated with the key is then used to make a decision:

  * If it is `false`, then `false` is returned.
  * If it is `true` or any non-`nil` value, then `true` is returned.
  * If it is `nil`, or there is no route data nor any of the mentioned
    keys can be found in a route data, it will try to auto-detect HTMX using `request?`
    function which analyzes the `HX-Request` request header.

  So the logic is as follows: HTMX response is advised if there is `:use-htmx?` (or
  some custom key) associated with a truthy value in a route data for the currently
  served request. If the key is not there then `HX-Request` header is analyzed to make
  a decision."
  (^Boolean [req]
   (request? req))
  (^Boolean [req route-data]
   (use? req route-data false))
  (^Boolean [req route-data extra-key]
   (if-some [route-data (or route-data (http/get-route-data req))]
     (if-some [force-hx (or (when extra-key (find route-data extra-key))
                            (find route-data :use-htmx?))]
       (if-some [force-hx? (val force-hx)]
         (boolean force-hx?)
         (request? req))
       (request? req))
     (request? req))))

;; Targets

(defn get-target-header
  "Returns a string from `HX-Target` header set by a client. If the header does not
  exist, it returns `nil`. If the header exists but contains an empty string, it
  returns `nil`."
  [req]
  (some-str (get (get req :headers) "hx-target")))

(defn get-target
  "Gets a target element ID set for the current route using `:app/target` route
  data. If it cannot be extracted, returns `nil`."
  [req]
  (when-let [target (http/req-or-route-param req :app/target)]
    (some-str target)))

(defn set-target-header
  "Sets the `HX-Retarget` header to a string value of the given `target` in response
  headers (under the `:response/headers` key) of the given `req` map. If the target
  is not given, its value is obtained from `:app/target` of the `req` or the route
  data within a request map. Returns updated `req`.

  By default it will not replace existing `HX-Retarget` header, unless the `replace?`
  argument is set to `true`."
  ([req]
   (if-some [target (get-target req)]
     (set-target-header req target false)
     req))
  ([req target]
   (set-target-header req target false))
  ([req target replace?]
   (if-some [target (some-str target)]
     (if-some [headers (resp/headers req)]
       (if (or replace? (not (contains? headers "HX-Retarget")))
         (qassoc req :headers (qassoc headers "HX-Retarget" target))
         req)
       (qassoc req :headers {"HX-Retarget" target}))
     req)))

;; Session

(defmacro kv-json-str
  [k v]
  `(if-let [k# ~k]
     (if-some [k# (some-str k#)]
       (strb "{\"" k# "\":\"" ~v "\"}")) "\"\""))

(defn- inject-json-event-header
  ([headers cur ename hname k v]
   (inject-json-event-header headers cur ename hname k v false))
  ([headers cur ename hname k v replace?]
   (if-some [js (and (string? cur)
                     (not-empty cur)
                     (j/read-value cur j/default-object-mapper))]
     (if (map? js)
       (if (or replace? (not (contains? js ename)))
         (map/qassoc headers hname (j/write-value-as-string (map/qassoc js ename (if k {k v} ""))))
         headers)
       (map/qassoc headers hname (strb "{" cur ":\"\", \"" ename "\":" (kv-json-str k v) "}")))
     (map/qassoc headers hname (strb "{\"" ename "\":" (kv-json-str k v) "}")))))

(defn add-json-event-header
  ([req header-name event-name]
   (add-json-event-header req header-name event-name nil nil false))
  ([req header-name event-name param-key param-value]
   (add-json-event-header req header-name event-name param-key param-value false))
  ([req header-name event-name param-key param-value replace?]
   (let [header-name (some-str header-name)
         event-name  (some-str event-name)
         headers     (get req :response/headers)]
     (map/qassoc
      req :response/headers
      (if (pos? (count headers))
        (if-some [current (get headers header-name)]
          (inject-json-event-header headers current event-name header-name
                                    param-key param-value replace?)
          (map/qassoc headers header-name (strb "{\"" event-name "\":" (kv-json-str param-key param-value) "}")))
        {header-name (strb "{\"" event-name "\":" (kv-json-str param-key param-value) "}")})))))

(defn add-session-hx-header
  "Adds `HX-Trigger` server response header to `:response/headers` map of the given
  `req` map by putting a JSON in the following form:

  `{\"setSession\":{\"session-id\": \"SID\"}}`

  where the `session-id` string is obtained from session's ID field (using
  `amelinium.http.middleware.session/id-field`) and `SID` is replaced with session
  ID (obtained with `amelinium.http.middleware.session/any-id`).

  If the `HX-Trigger` header already exists but it does not contain `setSession`
  trigger name, it will be modified. If it already contains it, it will be left as
  is."
  [req sess]
  (if-some [sid (when sess (session/any-id sess))]
    (add-json-event-header req "HX-Trigger" "setSession" (session/id-field sess) sid false)
    req))

(defn replace-session-hx-header
  "Adds `HX-Trigger` server response header to `:response/headers` map of the given
  `req` map by putting a JSON in the following form:

  `{\"setSession\":{\"session-id\": \"SID\"}}`

  where the `session-id` string is obtained from session's ID field (using
  `amelinium.http.middleware.session/id-field`) and `SID` is replaced with session
  ID (obtained with `amelinium.http.middleware.session/any-id`).

  If the `HX-Trigger` header already exists, it will be modified and any value
  associated with `setSession` key will be modified."
  [req sess]
  (if-some [sid (when sess (session/any-id sess))]
    (add-json-event-header req "HX-Trigger" "setSession" (session/id-field sess) sid true)
    req))

(defn reflect-session-hx-header
  "Tries to obtain session ID from the given `sess` object, and if that fails from the
  request header with name same as session ID field (obtained by calling
  `amelinium.http.middleware.session/id-field` on `sess`) from the `req` map. Then it
  uses `add-json-event-handler` to set `HX-Trigger` response header in a similar way
  the `add-session-hx-header` does."
  [req sess]
  (if sess
    (if-some [id-field (session/id-field sess)]
      (if-some [sid (or (session/any-id sess) (session/get-session-id-header req id-field))]
        (add-json-event-header req
                               "HX-Trigger"
                               "setSession"
                               id-field
                               sid
                               false)
        req)
      req)
    req))

;; Fragments

(defn inject
  "Injects HTML fragment by issuing HTMX response with `HX-Retarget` header set to
  `target` (if given and its value is not `false` and not `nil`), `:app/layout` key
  of the `req` set to `false` and `:app/view` key of the `req` set to `view` (if
  given and not `nil`). Returns updated request map `req`."
  ([req]
   (log/web-dbg req "Setting :app/layout to false")
   (qassoc req :app/layout false))
  ([req target]
   (let [req (qassoc req :app/layout false)]
     (log/web-dbg req "Setting :app/layout to false")
     (if target
       (if-some [t (some-str target)]
         (do (log/web-dbg req "Setting HX-Retarget header to" target)
             (common/add-header req :HX-Retarget t))
         req)
       req)))
  ([req target view]
   (let [req (qassoc req :app/layout false :app/view view)]
     (log/web-dbg req "Setting :app/layout to false and :app/view to" view)
     (if target
       (if-some [t (some-str target)]
         (do (log/web-dbg req "Setting HX-Retarget header to" target)
             (common/add-header req :HX-Retarget t))
         req)
       req))))

;; Redirects

(defn transform-redirect
  "Adds the `HX-Redirect` response header set to a value of existing `Location` header
  and removes the last one from the response map `resp`."
  ^Response [^Response resp]
  (when (instance? Response resp)
    (->Response (.status resp)
                (let [headers (.headers resp)]
                  (-> (qassoc headers "HX-Redirect" (get headers "Location"))
                      (dissoc "Location")))
                (.body resp))))

(defn transform-redirect-200
  "Adds the `HX-Redirect` response header set to a value of existing `Location` header
  and removes the last one from the response map `resp`. Additionally, forces HTTP
  status of the response to be 200."
  ^Response [^Response resp]
  (when (instance? Response resp)
    (->Response 200
                (let [headers (.headers resp)]
                  (-> (qassoc headers "HX-Redirect" (get headers "Location"))
                      (dissoc "Location")))
                (.body resp))))

(defn localized-redirect
  "HTMX redirect wrapper. Uses `HX-Redirect` header to trigger redirect and resets the
  status code to 200. The `f` should be a function which takes a request map and
  returns a response; should take at least one single argument which should be a
  URL. The URL will be parameterized with a language. Works almost the same way as
  the `redirect` but it will generate a localized path using a language obtained from
  a request (under `:language/str` key) and if there will be no
  language-parameterized variant of the path, it will fail. Use this function to make
  sure that localized path will be produced, or `nil`."
  {:arglists '(^Response [^IFn f]
               ^Response [^IFn f req]
               ^Response [^IFn f url]
               ^Response [^IFn f req url]
               ^Response [^IFn f req name-or-path]
               ^Response [^IFn f req name-or-path path-params]
               ^Response [^IFn f req name-or-path path-params query-params]
               ^Response [^IFn f req name-or-path lang]
               ^Response [^IFn f req name-or-path lang path-params]
               ^Response [^IFn f req name-or-path lang path-params query-params]
               ^Response [^IFn f req name-or-path lang path-params query-params & more])}
  (^Response [^IFn f]
   (transform-redirect (common/localized-redirect f)) )
  (^Response [^IFn f req-or-url]
   (transform-redirect (common/localized-redirect f req-or-url)))
  (^Response [^IFn f req name-or-path]
   (transform-redirect (common/localized-redirect f req name-or-path)))
  (^Response [^IFn f req name-or-path lang]
   (transform-redirect (common/localized-redirect f req name-or-path lang)))
  (^Response [^IFn f req name-or-path lang params]
   (transform-redirect (common/localized-redirect f req name-or-path lang params)))
  (^Response [^IFn f req name-or-path lang params query-params]
   (transform-redirect (common/localized-redirect f req name-or-path lang params query-params)))
  (^Response [^IFn f req name-or-path lang params query-params & more]
   (transform-redirect (apply common/localized-redirect f req name-or-path lang params query-params more))))

(defn redirect
  "Generic HTMX redirect wrapper. Uses `HX-Redirect` header to trigger redirect and
  resets the status code to 200. The `f` should be a function which takes a request
  map and returns a response; should take at least one single argument which should
  be a URL. The URL will be parameterized with a language if required. If the
  language is given it uses the `localized-page` function. If there is no language
  given but the page identified by its name requires a language parameter to be set,
  it will be obtained from the given request map (under the key `:language/str`)."
  {:arglists '(^Response [^IFn f]
               ^Response [^IFn f req]
               ^Response [^IFn f url]
               ^Response [^IFn f req url]
               ^Response [^IFn f req name-or-path]
               ^Response [^IFn f req name-or-path path-params]
               ^Response [^IFn f req name-or-path path-params query-params]
               ^Response [^IFn f req name-or-path lang]
               ^Response [^IFn f req name-or-path lang path-params]
               ^Response [^IFn f req name-or-path lang path-params query-params]
               ^Response [^IFn f req name-or-path lang path-params query-params & more])}
  (^Response [^IFn f]
   (transform-redirect (common/redirect f)))
  (^Response [^IFn f req-or-url]
   (transform-redirect (common/redirect f req-or-url)))
  (^Response [^IFn f req name-or-path]
   (transform-redirect (common/redirect f req name-or-path)))
  (^Response [^IFn f req name-or-path lang]
   (transform-redirect (common/redirect f req name-or-path lang)))
  (^Response [^IFn f req name-or-path lang params]
   (transform-redirect (common/redirect f req name-or-path lang params)))
  (^Response [^IFn f req name-or-path lang params query-params]
   (transform-redirect (common/redirect f req name-or-path lang params query-params)))
  (^Response [^IFn f req name-or-path lang params query-params & more]
   (transform-redirect (apply common/redirect f req name-or-path lang params query-params more))))

(defn go-to
  "Same as `amelinium.web/http-go-to` but uses `transform-redirect` internally to generate HTMX
  redirect."
  {:arglists '(^Response []
               ^Response [req]
               ^Response [url]
               ^Response [req url]
               ^Response [req name-or-path]
               ^Response [req name-or-path path-params]
               ^Response [req name-or-path path-params query-params]
               ^Response [req name-or-path lang]
               ^Response [req name-or-path lang path-params]
               ^Response [req name-or-path lang path-params query-params]
               ^Response [req name-or-path lang path-params query-params & more])}
  (^Response []
   (transform-redirect (common/localized-redirect resp/see-other)))
  (^Response [req-or-url]
   (transform-redirect (common/localized-redirect resp/see-other req-or-url)))
  (^Response [req name-or-path]
   (transform-redirect (common/localized-redirect resp/see-other req name-or-path)))
  (^Response [req name-or-path lang]
   (transform-redirect (common/localized-redirect resp/see-other req name-or-path lang)))
  (^Response [req name-or-path lang params]
   (transform-redirect (common/localized-redirect resp/see-other req name-or-path lang params)))
  (^Response [req name-or-path lang params query-params]
   (transform-redirect (common/localized-redirect resp/see-other req name-or-path lang params query-params)))
  (^Response [req name-or-path lang params query-params & more]
   (transform-redirect (apply common/localized-redirect resp/see-other req name-or-path lang params query-params more))))

(defn move-to
  "Same as `amelinium.web/http-move-to` but uses `transform-redirect` internally to generate HTMX
  redirect."
  {:arglists '(^Response []
               ^Response [req]
               ^Response [url]
               ^Response [req url]
               ^Response [req name-or-path]
               ^Response [req name-or-path path-params]
               ^Response [req name-or-path path-params query-params]
               ^Response [req name-or-path lang]
               ^Response [req name-or-path lang path-params]
               ^Response [req name-or-path lang path-params query-params]
               ^Response [req name-or-path lang path-params query-params & more])}
  (^Response []
   (transform-redirect (common/localized-redirect resp/temporary-redirect)))
  (^Response [req-or-url]
   (transform-redirect (common/localized-redirect resp/temporary-redirect req-or-url)))
  (^Response [req name-or-path]
   (transform-redirect (common/localized-redirect resp/temporary-redirect req name-or-path)))
  (^Response [req name-or-path lang]
   (transform-redirect (common/localized-redirect resp/temporary-redirect req name-or-path lang)))
  (^Response [req name-or-path lang params]
   (transform-redirect (common/localized-redirect resp/temporary-redirect req name-or-path lang params)))
  (^Response [req name-or-path lang params query-params]
   (transform-redirect (common/localized-redirect resp/temporary-redirect req name-or-path lang params query-params)))
  (^Response [req name-or-path lang params query-params & more]
   (transform-redirect (apply common/localized-redirect resp/temporary-redirect req name-or-path lang params query-params more))))

(defn go-to-with-status
  "Uses `inject` to set a target (`HX-Retarget` header) on a basis of the given
  application status `app-status` by looking it up in `:status/targets` of a route
  data map with a fallback to `:error/target`.

  Additionally, sets a fallback view to the given `default-view` (if set) and a flag
  `:response/set-status!` in `req` to ensure that application status is processed
  even if an HTTP response status will be `:ok/found` during rendering.

  Returns `req` with added `:app/status` set to the value of `app-status`,
  updated `:response/headers` and `:response/set-status!` flag."
  (^Response [req]
   (go-to-with-status req nil :error/internal nil))
  (^Response [req app-status]
   (go-to-with-status req nil app-status nil))
  (^Response [req app-status default-view]
   (go-to-with-status req nil app-status default-view))
  (^Response [req route-data app-status default-view]
   (let [req        (qassoc req :app/status app-status :response/set-status! true)
         route-data (or route-data (http/get-route-data req))
         target     (or (get-in route-data [:status/targets app-status])
                        (get route-data :error/target))]
     (log/web-dbg req "Injecting HTML fragment with app status" app-status
                  (when target (str "(target: " (some-str target) ")")))
     (if default-view
       (inject req target default-view)
       (inject req target)))))

;; Form errors

(defn handle-bad-request-form-params
  "Called by other functions to render form with feedback about submitted parameter
  errors (induced manually or caused by coercion exception). On fronted, uses HTMX
  and JavaScript Fetch API calls to load HTML.

  Takes a request map `req`, a map of erroneous parameter identifiers to parameter
  types `errors`, a map of current parameter values `values`, a map of error
  explanations (`explanations`), and a page title (`title`).

  The following arguments can be Delay objects and `clojure.core/force` will be
  applied to them before use: `errors`, `values`, `explanations`, `title`.

  Parameter type in `errors` map can be `nil`, meaning it is of unknown type.

  The layout and view are obtained from the `:form-errors/page` configuration option
  associated with HTTP route data or the destination page established by checking
  `Referer` header, unless `:form-errors/layout` and/or `:form-errors/view` options
  are set. Layout can be set to `false` to allow injection of HTML fragments.

  If form errors page is not specified one is obtained from the `Referer` request
  header.

  Sets `HX-Retarget` response header to a value set in route data option
  `:form-errors/target` and returns a response."
  ([req route-data errors]
   (handle-bad-request-form-params req route-data errors nil nil nil))
  ([req route-data errors values]
   (handle-bad-request-form-params req route-data errors values nil nil))
  ([req route-data errors values explanations]
   (handle-bad-request-form-params req route-data errors values explanations nil))
  ([req route-data errors values explanations title _]
   (handle-bad-request-form-params req route-data errors values explanations title))
  ([req route-data errors values explanations title]
   (if-not (valuable? errors)
     req ;; generic error page?
     (let [route-data         (or route-data (http/get-route-data req))
           orig-page          (get route-data :form-errors/page)
           orig-page          (or orig-page (:page (get req :goto)))
           title              (or title (get route-data :form-errors/title))
           referer            (when (nil? orig-page) (some-str (get (get req :headers) "referer")))
           orig-uri           (when referer (some-str (:uri (parse-url referer))))
           src-page           (or orig-page (http/route-name req orig-uri))
           src-route-data     (when src-page (http/route-data req src-page))
           new-view           (get route-data :form-errors/view)
           new-view           (if (nil? new-view) (get src-route-data :app/view) new-view)
           new-view           (if (nil? new-view) (get src-route-data :name) new-view)
           new-layout         (get route-data :form-errors/layout)
           new-layout         (if (nil? new-layout) (get src-route-data :app/layout) new-layout)
           handling-previous? (contains? (get req :query-params) "form-errors")
           hx-targets         (get route-data :form-errors/retargets)
           hx-src-target      (when hx-targets (get-target-header req))
           hx-target          (when hx-src-target (get hx-targets hx-src-target))
           hx-target          (some-str (or hx-target (get route-data :form-errors/target)))
           req                (if hx-target (common/add-header req :HX-Retarget hx-target) req)
           req                (if title (app-data/assoc req :title title) req)
           req                (if (nil? new-view)   req (qassoc req :error/view   new-view))
           req                (if (nil? new-layout) req (qassoc req :error/layout new-layout))]
       (-> req
           (qassoc :response/set-status! true)
           (app-data/assoc :coercion/errors       explanations
                           :form/previous-errors? handling-previous?
                           :form/errors           (delay {:dest   (:uri req)
                                                          :errors (force errors)
                                                          :params (force values)})))))))
