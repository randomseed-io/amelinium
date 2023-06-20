(ns

    ^{:doc    "Web helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string                       :as             str]
            [clojure.core.memoize                 :as             mem]
            [clojure.java.io                      :as              io]
            [potemkin.namespaces                  :as               p]
            [tick.core                            :as               t]
            [reitit.core                          :as               r]
            [ring.util.response]
            [amelinium.http.response              :as            resp]
            [ring.util.request                    :as             req]
            [selmer.parser                        :as          selmer]
            [amelinium.db                         :as              db]
            [amelinium.i18n                       :as            i18n]
            [amelinium.types.response             :refer         :all]
            [amelinium.utils                      :refer         :all]
            [amelinium.common                     :as          common]
            [amelinium.errors                     :as          errors]
            [amelinium.http                       :as            http]
            [amelinium.http.middleware.language   :as        language]
            [amelinium.http.middleware.session    :as         session]
            [amelinium.http.middleware.coercion   :as        coercion]
            [amelinium.http.middleware.validators :as      validators]
            [amelinium.logging                    :as             log]
            [amelinium                            :refer         :all]
            [io.randomseed.utils.map              :as             map]
            [io.randomseed.utils.map              :refer     [qassoc]]
            [io.randomseed.utils                  :refer         :all]
            [hiccup.core                          :refer         :all]
            [hiccup.table                         :as           table]
            [lazy-map.core                        :as        lazy-map])

  (:import (amelinium     Response)
           (clojure.lang  IFn)
           (reitit.core   Match)
           (java.io       File)
           (lazy_map.core LazyMapEntry
                          LazyMap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request map keys exposed in views

(def ^:const page-keys       [:title :subtitle])
(def ^:const param-keys      [:query-params :form-params :path-params :form/errors :coercion/errors])
(def ^:const validators-keys [:validators/config :validators/params-valid?])
(def ^:const session-keys    [:session])
(def ^:const remote-ip-keys  [:remote-ip :remote-ip/str :remote-ip/by-proxy? :remote-ip/proxy])
(def ^:const language-keys   [:language/id :language/str :language/default :accept])
(def ^:const i18n-keys       [:i18n/translator :i18n/translator-sub :i18n/translator-nd :i18n/translator-sub-nd])
(def ^:const roles-keys      [:roles :roles/in-context :roles/context :user/authorized? :user/authenticated? :user/known?])

(def ^:const common-auth-keys (vec (concat session-keys remote-ip-keys roles-keys)))

;; Request map keys to be always copied to the template system data map
;; Later on we put them under :app/data-required for being used by the injecting function

(def ^:const common-keys (vec (concat common-auth-keys
                                      validators-keys
                                      language-keys
                                      i18n-keys
                                      param-keys
                                      page-keys)))

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

;; HTMX

(p/import-vars [amelinium.common
                hx-request? use-hx? hx-target])

;; Special redirects

(p/import-vars [amelinium.common
                add-slash slash-redir lang-redir])

;; Accounts

(p/import-vars [amelinium.common
                lock-wait-default lock-wait
                hard-lock-time hard-locked?
                soft-lock-time soft-lock-passed soft-locked? soft-lock-remains])

;; Context and roles

(p/import-vars [amelinium.common
                has-any-role? has-role?
                role-required! with-role-only!
                roles-for-context roles-for-contexts default-contexts-labeler
                roles-matrix roles-tabler])

;; Lazy map handling

(defn map-to-lazy
  "Ensures that the given argument `m` is a lazy map. If it is not a map, it is
  returned as is. If it is `nil`, empty lazy map is returned."
  [m]
  (if (map? m)
    (map/to-lazy m)
    (if (nil? m)
      empty-lazy-map
      m)))

;; HTML generators and transformers

(defn roles-table
  "Generates roles table as HTML string."
  ([req]
   (let [{:keys [data labels]} (roles-tabler req nil)]
     (if (and data labels)
       (html (table/to-table1d data labels)))))
  ([req opts]
   (let [{:keys [data labels]} (roles-tabler req opts)]
     (if (and data labels)
       (html (table/to-table1d data labels))))))

;; HTML response

(defmacro response
  "Creates a response block. If the given `req` is already a response then it is simply
  returned. Otherwise the expressions from `body` are evaluated in an implicit `do`."
  [req & body]
  (if (and (seq? body) (> (count body) 1))
    `(let [req# ~req] (if (resp/response? req#) req# (do ~@body)))
    `(let [req# ~req] (if (resp/response? req#) req# ~@body))))

;; Handling :app/data

(defn get-missing-app-data-from-req
  "Associates missing data identified with keys listed in `keyz` with values taken from
  the request map if the key exists. The resulting map is converted to a lazy map if
  it's not."
  [data req keyz]
  (let [req (map/to-lazy req)]
    (reduce (fn [ret k]
              (if-let [entry (and (not (contains? ret k)) (find req k))]
                (qassoc ret k (.val_ ^LazyMapEntry entry))
                ret))
            (map/to-lazy (or data empty-lazy-map))
            (seq keyz))))

(defn no-app-data
  "Disables processing of the `:app/data` key for the given request `req` by
  associating it with the `false` value."
  [req]
  (qassoc req :app/data false))

(defn no-app-data?
  "Returns `true` when the value associated with `:app/data` in `req` is `false`."
  [req]
  (false? (get req :app/data)))

(defn app-data
  "Gets the value of `:app/data` for the current request. If it does not exist or it is
  `nil`, returns an empty lazy map. Otherwise it returns the unmodified value. If it
  is a map but not a lazy map, converts it to a lazy map."
  [req]
  (if-some [m (get req :app/data)]
    (map-to-lazy m)
    empty-lazy-map))

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
         (let [req-data (if req-data (map/to-lazy req-data))
               data     (if req-data (map/merge-lazy req-data data) (map/to-lazy data))
               keyz     (or keyz (concat common-keys (get req :app/data-required)))]
           (if (and data (pos? (count data)))
             (get-missing-app-data-from-req data req keyz)
             (map/select-keys-lazy req keyz))))))))

(defmacro add-app-data
  "Adds a lazy map to a request map `req` under its key `:app/data` using
  `qassoc`. Overwrites previous value. The body is a result of evaluating expressions
  passed as additional arguments (`body`). Returns updated `req`. Assumes that `req`
  is always a map. Ensures that the resulting map is lazy. Ensures that the result is
  not `nil` (if it is, empty lazy map is returned).

  Associating `:app/data` with `false` will prevent it from further processing."
  [req & body]
  (if (and (seq? body) (> (count body) 1))
    `(qassoc ~req :app/data (map-to-lazy (do ~@body)))
    `(qassoc ~req :app/data (map-to-lazy ~@body))))

(defn update-app-data
  "Updates the `:app/data` in a request map `req` with a result of calling the function
  `f` on the previous value and optional arguments. Uses
  `io.randomseed.utils.map/qassoc`. Returns updated `req` with a lazy map under
  `:add/data` key. Ensures that the result of calling `f` is a lazy map and if it is
  not, tries to convert it to a lazy map (if it is `nil`). When the current value of
  `:app/data` is `false` it will short-circuit and skip updating."
  ([req]
   (add-app-data req))
  ([req f]
   (let [ad (app-data req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad))))))
  ([req f a]
   (let [ad (app-data req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad a))))))
  ([req f a b]
   (let [ad (app-data req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad a b))))))
  ([req f a b c]
   (let [ad (app-data req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad a b c))))))
  ([req f a b c d]
   (let [ad (app-data req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad a b c d))))))
  ([req f a b c d & more]
   (let [ad (app-data req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (apply f ad a b c d more)))))))

(defmacro assoc-app-data
  "Adds keys with associated values to `:app/data` map of the `req` using `qassoc`. If
  any key argument is a literal keyword, a character, or a literal string, it will be
  converted to a keyword literal and placed as `qassoc` argument. Otherwise it will
  be left as is and wrapped into a call to `io.randomseed.utils/some-keyword` to
  ensure the result is a keyword run-time. Missing last value, if any, will be padded
  with `nil`. If there is no body or the body is empty, it will initialize it with a
  map expression, otherwise it will use `assoc`. Assumes that `req` is always a
  map. If the current value of `:app/data` is `false`, it will skip the processing."
  ([req k v]
   (let [k (if (or (keyword? k) (string? k) (char? k))
             (some-keyword k)
             (cons `some-keyword (cons k nil)))]
     `(let [req# ~req
            apd# (app-data req#)]
        (if (false? apd#) req# (qassoc req# :app/data (qassoc apd# ~k ~v))))))
  ([req k v & more]
   (let [pairs  (cons k (cons v more))
         names  (take-nth 2 pairs)
         values (concat (take-nth 2 (rest pairs)) '(nil))
         pairs  (map #(cons (if (or (keyword?  %1)
                                    (string?   %1)
                                    (char?     %1))
                              (some-keyword %1)
                              (cons `some-keyword (cons %1 nil)))
                            (cons %2 nil))
                     names values)
         pairs  (apply concat pairs)
         names  (take-nth 2 pairs)
         dups?  (not= (count names) (count (distinct names)))]
     (if dups?
       `(let [req# ~req
              apd# (app-data req#)]
          (if (false? apd#) req# (qassoc req# :app/data (qassoc apd# ~@pairs))))
       `(let [req# ~req
              apd# (app-data req#)]
          (if (false? apd#)
            req#
            (qassoc req# :app/data
                    (if (pos? (count apd#))
                      (qassoc apd# ~@pairs)
                      (lazy-map/->LazyMap {~@pairs ~@[]})))))))))

;; Targets

(defn get-target
  "Gets a target element ID set for the current route using `:app/target` route
  data. If it cannot be extracted, returns `nil`."
  [req]
  (if-let [target (http/req-or-route-param req :app/target)]
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
     (if-some [headers (get req :response/headers)]
       (if (or replace? (not (contains? headers "HX-Retarget")))
         (qassoc req :headers (qassoc headers "HX-Retarget" target))
         req)
       (qassoc req :headers {"HX-Retarget" target}))
     req)))

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
        auto-dir-sl (if auto-dir (str auto-dir "/"))
        prep-sl     (if pre      (str pre  "/"))
        dir-sl      (if dir      (str dir  "/"))
        lang-sl     (if lang     (str lang "/"))
        pths        (lazy-cat [[prep-sl lang-sl dir-sl core dot-html]]
                              [[prep-sl lang-sl dir-sl core sl-default-html]]
                              [[prep-sl dir-sl core dot-html]]
                              [[prep-sl lang-sl auto-dir-sl default-html]]
                              (if auto-dir [[prep-sl lang-sl auto-dir dot-html]])
                              (if auto-dir [[prep-sl auto-dir-sl default-html]])
                              (if auto-dir [[prep-sl auto-dir dot-html]])
                              [[prep-sl lang-sl default-html]]
                              [[prep-sl default-html]])]
    (or (first (keep #(apply some-resource %) pths))
        (do (if (nil? uri) (log/wrn "[-]: Empty URI while resolving" pre))
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

(defn response-status?
  "Returns `true` if the response status of the given `req` is equal to `k`."
  [req k]
  (identical? (some-keyword k) (get req :response/status)))

(defn- update-status
  "Updates `:app/data` map of the `req` by setting its status key `status-key` to
  `status`, `title-key` to translated `status` and `description-key` to translated
  message obtained for a translation key made with namespace of `status` and name of
  `status` with `.full` attached. If `status` is untranslatable (is not an ident nor
  a string), it will just associate `status-key` with the given `status`.

  Any existing entries of `:app/data` having the same keys as given (`status-key`,
  `title-key` and `description-key`) will remain unmodified."
  ([req status lang status-key title-key description-key]
   (if status
     (->> (update-status (get req :app/data empty-lazy-map) req status lang status-key title-key description-key)
          (qassoc req :app/data))
     req))
  ([data req status lang status-key title-key description-key]
   (if status
     (if (common/untranslatable? status)
       (map/assoc-missing (or data empty-lazy-map) status-key status)
       (let [translate-sub (delay (i18n/no-default (common/translator-sub req lang)))]
         (map/assoc-missing
          (or data empty-lazy-map)
          status-key      status
          title-key       (delay (@translate-sub status))
          description-key (delay (@translate-sub
                                  (try-namespace status)
                                  (str (try-name status) ".full"))))))
     data))
  ([data req status lang]
   (update-status data req status lang :status :status/title :status/description))
  ([req status lang]
   (update-status req status lang :status :status/title :status/description)))

(defn get-for-status
  "If the given `status` is not `nil` and not `false`, it looks for `k` in `req`,
  and if that returns `nil` tries to look for `k` in `route-data`, and then, if its
  not `nil` gets a value associated with the given `status` in this map. If
  `other-status` is given, it will be used as fallback in the same way as the first,
  when it will return `nil`."
  ([req route-data k status]
   (if status
     (if-some [db (or (get req k) (get route-data k))]
       (get db status))))
  ([req route-data k status other-status]
   (if status
     (if-some [db (or (get req k) (get route-data k))]
       (or-some (get db status)
                (if other-status (get db other-status))))
     (if other-status
       (if-some [db (or (get req k) (get route-data k))]
         (get db other-status))))))

(defn status-lv
  "Sets a different layout and/or view when the given HTTP status (`status`) is not
  `:ok/found` nor `:ok` nor `nil` and the `:response/set-status!` entry of the `req`
  does not have a truthy value.

  If `layout` is given and it is not `nil`, it will not be changed but returned as
  is. If `view` is given and it is not `nil`, it will not be changed but returned as
  is.

  To establish a layout it will check for `:status/layouts` key in `req` and then in
  `route-data` to obtain a mapping of statuses to layouts. It will then look for an
  application status (taken from `:response/status` of the given `req`) and if that
  will return `nil` it will look for an HTTP status `status` in the same map. If that
  will fail, it will try `:error/layout` key of the `req` and the `:error/layout` key
  of the `route-data`. Finally, it will return a string `\"error\"`.

  To establish a view it will check for `:status/views` key in `req` and then in
  `route-data` to obtain a mapping of statuses to views. It will then look for an
  application status (taken from `:response/status` of the given `req`) and if that
  will return `nil` it will look for an HTTP status `status` in the same map. If that
  will fail, it will try `:error/layout` key of the `req` and then `:error/layout`
  key of the `route-data`. Finally, it will return a string `\"error\"`.

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
   (if (and (or (identical? :ok/found status)
                (nil? status)
                (identical? :ok status))
            (not (get req :response/set-status!)))
     [layout view]
     (let [no-layout (nil? layout)
           no-view   (nil? view)]
       (if (not (or no-layout no-view))
         [layout view]
         (let [app-status (get req :response/status)]
           (log/web-dbg req "Getting layout/view for"
                        (if app-status (str "application status " (some-str app-status) " and"))
                        "HTTP response status" status)
           [(if no-layout
              (or-some (get-for-status req route-data :status/layouts app-status status)
                       (get req :error/layout)
                       (get route-data :error/layout)
                       "error")
              layout)
            (if no-view
              (or-some (get-for-status req route-data :status/views app-status status)
                       (get req :error/view)
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

  In case of a regular response page (when the namespace of a `status` keyword is
  \"ok\" or \"info\") the following sources are checked to find a layout path:
  - the given `layout`,
  - value of `:app/layout` (in a route data or a request map).

  In case of a regular response page (when the namespace of a `status` keyword is
  \"ok\" or \"info\") the following sources are checked to find a view path:
  - the given `view`,
  - value of `:app/view` (in a route data or a request map).

  In case of an error response page (when the namespace of a `status` keyword is not
  \"ok\" nor \"info\") the following sources are checked to find a layout path:
  - the given `layout`,
  - value of `status` looked up in a map under `:status/layouts` (in a route data or a request map),
  - value of `:app/error-layout` (in a route data or a request map),
  - \"error\".

  In case of an error response page (when the namespace of a status keyword is not
  \"ok\" nor \"info\") the following sources are checked to find a view path:
  - the given `view`,
  - value of `status` looked up in a map under `:status/views` (in a  route data or a request map),
  - value of `:app/error-view` (in a route data or a request map),
  - \"error\"."
  ([]
   (render nil :ok/found nil nil nil nil))
  ([req]
   (render req :ok/found nil nil nil nil))
  ([req status]
   (render req status nil nil nil nil))
  ([req status data]
   (render req status data nil nil nil))
  ([req status data view]
   (render req status data view nil nil))
  ([req status data view layout]
   (render req status data view layout nil))
  ([req http-status data view layout lang]
   (let [uri         (get req :uri)
         lang        (if lang (some-str lang))
         lang        (if (false? lang) nil (common/pick-language-str req))
         route-data  (http/get-route-data req)
         [layt view] (status-lv req http-status layout view route-data)
         layt        (resolve-layout req lang layt route-data)
         view        (resolve-view   req lang view route-data)]
     (if (or layt view)
       (do (log/web-dbg req "Rendering (layout:" layt "view:" (str view ")"))
           (let [dlng (or lang (get req :language/str))
                 data (prep-app-data req data)
                 data (map/assoc-missing data
                                         :uri                uri
                                         :url                (delay (req/request-url req))
                                         :character-encoding (delay (req/character-encoding req))
                                         :path               (delay (common/page req))
                                         :htmx-request?      (delay (common/hx-request? req))
                                         :lang               dlng)
                 data (update-status data req http-status dlng)
                 html (if view (selmer/render-file view data) "")
                 rndr (qassoc data :body [:safe html])
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

  In case of a regular response page (when the namespace of a `status` keyword is
  \"ok\" or \"info\") the following sources are checked to find a layout path:
  - the given `layout`,
  - value of `:app/layout` (in a route data or a request map).

  In case of a regular response page (when the namespace of a `status` keyword is
  \"ok\" or \"info\") the following sources are checked to find a view path:
  - the given `view`,
  - value of `:app/view` (in a route data or a request map).

  In case of an error response page (when the namespace of a `status` keyword is not
  \"ok\" nor \"info\") the following sources are checked to find a layout path:
  - the given `layout`,
  - value of `status` looked up in a map under `:status/layouts` (in a route data or a request map),
  - value of `:app/error-layout` (in a route data or a request map),
  - \"error\".

  In case of an error response page (when the namespace of a status keyword is not
  \"ok\" nor \"info\") the following sources are checked to find a view path:
  - the given `view`,
  - value of `status` looked up in a map under `:status/views` (in a  route data or a request map),
  - value of `:app/error-view` (in a route data or a request map),
  - \"error\"."
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
     (let [req (set-target-header req)]
       (-> (render req status data view layout lang)
           (resp-fn (or (get req :response/headers) {})))))))

(defn render-response-force
  "Web response renderer. Uses the `render` function to render a response body
  (using values associated with the `:app/data`, `:app/view` and `:app/layout` in the
  `req` map, or provided as arguments) and the response headers (using the
  `:response/headers` value), regardless if the `req` is already a valid response or
  not.

  It will add `:status`, `:status/title` and `:status/description` entries
  to `:app/data` map (unless it already contains one), using configuration maps
  associated with the `:errors/config` key of a route data.

  In case of a regular response page (when the namespace of a `status` keyword is
  \"ok\" or \"info\") the following sources are checked to find a layout path:
  - the given `layout`,
  - value of `:app/layout` (in a route data or a request map).

  In case of a regular response page (when the namespace of a `status` keyword is
  \"ok\" or \"info\") the following sources are checked to find a view path:
  - the given `view`,
  - value of `:app/view` (in a route data or a request map).

  In case of an error response page (when the namespace of a `status` keyword is not
  \"ok\" nor \"info\") the following sources are checked to find a layout path:
  - the given `layout`,
  - value of `status` looked up in a map under `:status/layouts` (in a route data or a request map),
  - value of `:app/error-layout` (in a route data or a request map),
  - \"error\".

  In case of an error response page (when the namespace of a status keyword is not
  \"ok\" nor \"info\") the following sources are checked to find a view path:
  - the given `view`,
  - value of `status` looked up in a map under `:status/views` (in a  route data or a request map),
  - value of `:app/error-view` (in a route data or a request map),
  - \"error\"."
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
   (let [req (set-target-header req)]
     (-> (render req status data view layout lang)
         (resp-fn (or (get req :response/headers) {}))))))

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
        (str "Renders a " (if status (name status " "))
             "response with a possible body generated with views, layouts and data \n  "
             "obtained from a request map (`:app/layout`, `:app/view`, `:app/data` keys).\n  "
             "Uses `" f-or-doc "` to set the response code."
             (if status
               (str " Additionaly, associates `:status` key\n  "
                    "with `" (str status) "` in `:app/data` "
                    "by passing it as an argument to `render-response`\n  "
                    "(which will also set the `:status/title` "
                    "and `:status/description` if possible).")))
        f status))))
  ([name doc f status]
   `(let [^IFn f# ~f
          c# ~status
          c# (if c# (keyword c#))]
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
   (resp/render resp/created))
  (^Response [req]
   (resp/created (render req :ok/created nil nil nil nil)
                 (or (get req :response/headers) {})
                 (common/resolve-location page req)))
  (^Response [req data]
   (resp/created (render req :ok/created data nil nil nil)
                 (or (get req :response/headers) {})
                 (common/resolve-location page req)))
  (^Response [req data view]
   (resp/created (render req :ok/created data view nil nil)
                 (or (get req :response/headers) {})
                 (common/resolve-location page req)))
  (^Response [req data view layout]
   (resp/created (render req :ok/created data view layout nil)
                 (or (get req :response/headers) {})
                 (common/resolve-location page req)))
  (^Response [req data view layout lang]
   (resp/created (render req :ok/created data view layout lang)
                 (or (get req :response/headers) {})
                 (common/resolve-location page req lang)))
  (^Response [req data view layout lang name-or-path]
   (resp/created (render req :ok/created data view layout lang)
                 (or (get req :response/headers) {})
                 (if (common/is-url? name-or-path)
                   name-or-path
                   (page req name-or-path lang))))
  (^Response [req data view layout lang name-or-path params]
   (resp/created (render req :ok/created data view layout lang)
                 (or (get req :response/headers) {})
                 (if (common/is-url? name-or-path)
                   name-or-path
                   (page req name-or-path lang params))))
  (^Response [req data view layout lang name-or-path params query-params]
   (resp/created (render req :ok/created data view layout lang)
                 (or (get req :response/headers) {})
                 (if (common/is-url? name-or-path)
                   name-or-path
                   (page req name-or-path lang params query-params)))))

(defn localized-render-created
  "Renders 201 response with a redirect (possibly localized if a destination path is
  language-parameterized) and a possible body. Requires the destination
  URL (specified by arguments or by the `:response/location` key of the given `req`)
  to be language parameterized. See `render` documentation to know more about body
  rendering."
  (^Response []
   (resp/render resp/created))
  (^Response [req]
   (resp/created (render req :ok/created nil nil nil nil)
                 (or (get req :response/headers) {})
                 (common/resolve-location localized-page req)))
  (^Response [req data]
   (resp/created (render req :ok/created data nil nil nil)
                 (or (get req :response/headers) {})
                 (common/resolve-location localized-page req)))
  (^Response [req data view]
   (resp/created (render req :ok/created data view nil nil)
                 (or (get req :response/headers) {})
                 (common/resolve-location localized-page req)))
  (^Response [req data view layout]
   (resp/created (render req :ok/created data view layout nil)
                 (or (get req :response/headers) {})
                 (common/resolve-location localized-page req)))
  (^Response [req data view layout lang]
   (resp/created (render req :ok/created data view layout lang)
                 (or (get req :response/headers) {})
                 (common/resolve-location localized-page req lang)))
  (^Response [req data view layout lang name-or-path]
   (resp/created (render req :ok/created data view layout lang)
                 (or (get req :response/headers) {})
                 (if (common/is-url? name-or-path)
                   name-or-path
                   (localized-page req name-or-path lang))))
  (^Response [req data view layout lang name-or-path params]
   (resp/created (render req :ok/created data view layout lang)
                 (or (get req :response/headers) {})
                 (if (common/is-url? name-or-path)
                   name-or-path
                   (localized-page req name-or-path lang params))))
  (^Response [req data view layout lang name-or-path params query-params]
   (resp/created (render req :ok/created data view layout lang)
                 (or (get req :response/headers) {})
                 (if (common/is-url? name-or-path)
                   name-or-path
                   (localized-page req name-or-path lang params query-params)))))

;; Responses without bodies

(defn render-continue
  "Renders 100 response without a body."
  (^Response []              (resp/continue))
  (^Response [req]           (resp/render resp/continue req))
  (^Response [req & ignored] (resp/render resp/continue req)))

(defn render-switching-protocols
  "Renders 101 response without a body."
  (^Response []              (resp/switching-protocols))
  (^Response [req]           (resp/render resp/switching-protocols req))
  (^Response [req & ignored] (resp/render resp/switching-protocols req)))

(defn render-processing
  "Renders 102 response without a body."
  (^Response []              (resp/processing))
  (^Response [req]           (resp/render resp/processing req))
  (^Response [req & ignored] (resp/render resp/processing req)))

(defn render-no-content
  "Renders 204 response without a body."
  (^Response []              (resp/no-content))
  (^Response [req]           (resp/render resp/no-content req))
  (^Response [req & ignored] (resp/render resp/no-content req)))

(defn render-reset-content
  "Renders 205 response without a body."
  (^Response []              (resp/reset-content))
  (^Response [req]           (resp/render resp/reset-content req))
  (^Response [req & ignored] (resp/render resp/reset-content req)))

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
          req        (update-status req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status render-internal-server-error req))))
  (^Response [req app-status default]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          req        (update-status req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req))))
  (^Response [req app-status default data]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req data))))
  (^Response [req app-status default data view]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req data view))))
  (^Response [req app-status default data view layout]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req data view layout))))
  (^Response [req app-status default data view layout lang]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status lang :app-status :app-status/title :app-status/description)]
      (errors/render err-config app-status (or default render-internal-server-error) req data view layout lang))))
  (^Response [req app-status default data view layout lang & more]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status lang :app-status :app-status/title :app-status/description)]
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
          req        (update-status req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status render-ok req))))
  (^Response [req app-status default]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          req        (update-status req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req))))
  (^Response [req app-status default data]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req data))))
  (^Response [req app-status default data view]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req data view))))
  (^Response [req app-status default data view layout]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status nil :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req data view layout))))
  (^Response [req app-status default data view layout lang]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status lang :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (errors/render err-config app-status (or default render-ok) req data view layout lang))))
  (^Response [req app-status default data view layout lang & more]
   (response
    req
    (let [err-config (errors/config req)
          app-status (errors/most-significant err-config app-status)
          data       (update-status data req app-status lang :app-status :app-status/title :app-status/description)]
      (log/web-dbg req "Rendering response with application status" app-status)
      (apply errors/render err-config app-status (or default render-ok) req data view layout lang more)))))

;; HTMX

(defn hx-inject
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
             (add-header req :HX-Retarget t))
         req)
       req)))
  ([req target view]
   (let [req (qassoc req :app/layout false :app/view view)]
     (log/web-dbg req "Setting :app/layout to false and :app/view to" view)
     (if target
       (if-some [t (some-str target)]
         (do (log/web-dbg req "Setting HX-Retarget header to" target)
             (add-header req :HX-Retarget t))
         req)
       req))))

(defn hx-transform-redirect
  "Adds the `HX-Redirect` response header set to a value of existing `Location` header
  and removes the last one from the response map `resp`. Additionally forces HTTP
  status of the response to be 200."
  ^Response [^Response resp]
  (if (resp/response? resp)
    (->Response 200
                (let [headers (.headers resp)]
                  (-> (qassoc headers "HX-Redirect" (get headers "Location"))
                      (dissoc "Location")))
                (.body resp))))

(defn hx-localized-redirect
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
   (hx-transform-redirect (common/localized-redirect f)) )
  (^Response [^IFn f req-or-url]
   (hx-transform-redirect (common/localized-redirect f req-or-url)))
  (^Response [^IFn f req name-or-path]
   (hx-transform-redirect (common/localized-redirect f req name-or-path)))
  (^Response [^IFn f req name-or-path lang]
   (hx-transform-redirect (common/localized-redirect f req name-or-path lang)))
  (^Response [^IFn f req name-or-path lang params]
   (hx-transform-redirect (common/localized-redirect f req name-or-path lang params)))
  (^Response [^IFn f req name-or-path lang params query-params]
   (hx-transform-redirect (common/localized-redirect f req name-or-path lang params query-params)))
  (^Response [^IFn f req name-or-path lang params query-params & more]
   (hx-transform-redirect (apply common/localized-redirect f req name-or-path lang params query-params more))))

(defn hx-redirect
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
   (hx-transform-redirect (common/redirect f)))
  (^Response [^IFn f req-or-url]
   (hx-transform-redirect (common/redirect f req-or-url)))
  (^Response [^IFn f req name-or-path]
   (hx-transform-redirect (common/redirect f req name-or-path)))
  (^Response [^IFn f req name-or-path lang]
   (hx-transform-redirect (common/redirect f req name-or-path lang)))
  (^Response [^IFn f req name-or-path lang params]
   (hx-transform-redirect (common/redirect f req name-or-path lang params)))
  (^Response [^IFn f req name-or-path lang params query-params]
   (hx-transform-redirect (common/redirect f req name-or-path lang params query-params)))
  (^Response [^IFn f req name-or-path lang params query-params & more]
   (hx-transform-redirect (apply common/redirect f req name-or-path lang params query-params more))))

(defn http-go-to
  "Uses the `localized-page` function to calculate the destination path on a basis of
  page name (identifier) or a path (a string) and performs a redirect with code 303 to
  it using `resp/see-other`. If the language is given it uses the `localized-page` function.
  If there is no language given but the page identified by its name requires
  a language parameter to be set, it will be obtained from the given request map
  (under the key `:language/str`).

  The difference between this function and its regular counterpart (if defined) is in
  binary variants of them (when a request map and a name or a path are given as
  arguments). The regular function will fail to generate a redirect if there is
  no language parameter and the given path does not point to an existing
  page. On the contrary, this function will generate a localized path using a
  language obtained from a request (under `:language/str` key) and if there will be no
  language-parameterized variant of the path, it will fail. Use this function to make
  sure that a localized path will be produced, or `nil`."
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
   (common/localized-redirect resp/see-other))
  (^Response [req-or-url]
   (common/localized-redirect resp/see-other req-or-url))
  (^Response [req name-or-path]
   (common/localized-redirect resp/see-other req name-or-path))
  (^Response [req name-or-path lang]
   (common/localized-redirect resp/see-other req name-or-path lang))
  (^Response [req name-or-path lang params]
   (common/localized-redirect resp/see-other req name-or-path lang params))
  (^Response [req name-or-path lang params query-params]
   (common/localized-redirect resp/see-other req name-or-path lang params query-params))
  (^Response [req name-or-path lang params query-params & more]
   (apply common/localized-redirect resp/see-other req name-or-path lang params query-params more)))

(defn http-move-to
  "Uses the `localized-page` function to calculate the destination path on a basis of
  page name (identifier) or a path (a string) and performs a redirect with code 307
  to it using `resp/temporary-redirect`. If the language is given it uses the
  `localized-page` function.  If there is no language given but the page identified
  by its name requires a language parameter to be set, it will be obtained from the
  given request map (under the key `:language/str`).

  The difference between this function and its regular counterpart (if defined) is in
  binary variants of them (when a request map and a name or a path are given as
  arguments). The regular function will fail to generate a redirect if there is
  no language parameter and the given path does not point to an existing
  page. On the contrary, this function will generate a localized path using a
  language obtained from a request (under `:language/str` key) and if there will be no
  language-parameterized variant of the path, it will fail. Use this function to make
  sure that a localized path will be produced, or `nil`."
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
   (common/localized-redirect resp/temporary-redirect))
  (^Response [req-or-url]
   (common/localized-redirect resp/temporary-redirect req-or-url))
  (^Response [req name-or-path]
   (common/localized-redirect resp/temporary-redirect req name-or-path))
  (^Response [req name-or-path lang]
   (common/localized-redirect resp/temporary-redirect req name-or-path lang))
  (^Response [req name-or-path lang params]
   (common/localized-redirect resp/temporary-redirect req name-or-path lang params))
  (^Response [req name-or-path lang params query-params]
   (common/localized-redirect resp/temporary-redirect req name-or-path lang params query-params))
  (^Response [req name-or-path lang params query-params & more]
   (apply common/localized-redirect resp/temporary-redirect req name-or-path lang params query-params more)))

(defn hx-go-to
  "Same as `http-go-to` but uses `hx-transform-redirect` internally to generate HTMX
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
   (hx-transform-redirect (common/localized-redirect resp/see-other)))
  (^Response [req-or-url]
   (hx-transform-redirect (common/localized-redirect resp/see-other req-or-url)))
  (^Response [req name-or-path]
   (hx-transform-redirect (common/localized-redirect resp/see-other req name-or-path)))
  (^Response [req name-or-path lang]
   (hx-transform-redirect (common/localized-redirect resp/see-other req name-or-path lang)))
  (^Response [req name-or-path lang params]
   (hx-transform-redirect (common/localized-redirect resp/see-other req name-or-path lang params)))
  (^Response [req name-or-path lang params query-params]
   (hx-transform-redirect (common/localized-redirect resp/see-other req name-or-path lang params query-params)))
  (^Response [req name-or-path lang params query-params & more]
   (hx-transform-redirect (apply common/localized-redirect resp/see-other req name-or-path lang params query-params more))))

(defn hx-move-to
  "Same as `http-move-to` but uses `hx-transform-redirect` internally to generate HTMX
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
   (hx-transform-redirect (common/localized-redirect resp/temporary-redirect)))
  (^Response [req-or-url]
   (hx-transform-redirect (common/localized-redirect resp/temporary-redirect req-or-url)))
  (^Response [req name-or-path]
   (hx-transform-redirect (common/localized-redirect resp/temporary-redirect req name-or-path)))
  (^Response [req name-or-path lang]
   (hx-transform-redirect (common/localized-redirect resp/temporary-redirect req name-or-path lang)))
  (^Response [req name-or-path lang params]
   (hx-transform-redirect (common/localized-redirect resp/temporary-redirect req name-or-path lang params)))
  (^Response [req name-or-path lang params query-params]
   (hx-transform-redirect (common/localized-redirect resp/temporary-redirect req name-or-path lang params query-params)))
  (^Response [req name-or-path lang params query-params & more]
   (hx-transform-redirect (apply common/localized-redirect resp/temporary-redirect req name-or-path lang params query-params more))))

(defn- go-to-fn
  ^IFn [req]
  (if (use-hx? req nil false) hx-go-to http-go-to))

(defn- move-to-fn
  ^IFn [req]
  (if (use-hx? req nil false) hx-move-to http-move-to))

(defn go-to
  "When HTMX is detected with `use-hx?` calls `hx-go-to`, otherwise calls
  `http-go-to`."
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
  "When HTMX is detected with `use-hx?` calls `hx-move-to`, otherwise calls
  `http-move-to`."
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

(defn http-go-to-with-status
  "Uses `http-go-to` to make a redirect on a basis of the given application status
  `app-status` by looking it up in `:error/destinations` of a route data map with
  fallback to a value associated with the `:error/destination` key or to a value of
  the `default-page` argument (if set)."
  (^Response [req]
   (http-go-to-with-status req nil :error nil))
  (^Response [req app-status]
   (http-go-to-with-status req nil app-status nil))
  (^Response [req app-status default-page]
   (http-go-to-with-status req nil app-status default-page))
  (^Response [req route-data app-status default-page]
   (let [route-data (or route-data (http/get-route-data req))]
     (http-go-to req
                 (or (get-in route-data [:error/destinations app-status] default-page)
                     (get route-data :error/destination))))))

(defn hx-go-to-with-status
  "Uses `hx-inject` to set a target (`HX-Retarget` header) on a basis of the given
  application status `app-status` by looking it up in `:status/targets` of a route
  data map with a fallback to `:error/target`.

  Additionally, sets a fallback view to the given `default-view` (if set) and a flag
  `:response/set-status!` in `req` to ensure that application status is processed
  even if an HTTP response status will be `:ok/found` during rendering.

  Returns `req` with added `:response/status` set to the value of `app-status`,
  updated `:response/headers` and `:response/set-status!` flag."
  (^Response [req]
   (hx-go-to-with-status req nil :error/internal nil))
  (^Response [req app-status]
   (hx-go-to-with-status req nil app-status nil))
  (^Response [req app-status default-view]
   (hx-go-to-with-status req nil app-status default-view))
  (^Response [req route-data app-status default-view]
   (let [req        (qassoc req :response/status app-status :response/set-status! true)
         route-data (or route-data (http/get-route-data req))
         target     (or (get-in route-data [:status/targets app-status])
                        (get route-data :error/target))]
     (log/web-dbg req "Injecting HTML fragment with app status" app-status
                  (str "(target:" (some-str target) ")"))
     (if default-view
       (hx-inject req target default-view)
       (hx-inject req target)))))

(defn go-to-with-status
  "Uses `hx-go-to-with-status` when HTMX is in use (uses `common/use-hx?`),
  `http-go-to-with-status` otherwise. Takes additional `hx-status-flag` as a key to
  be checked in route data whether it is associated with a HTMX-enforcement flag."
  (^Response [req]
   (go-to-with-status req nil :error nil false))
  (^Response [req app-status]
   (go-to-with-status req nil app-status nil false))
  (^Response [req app-status default-page]
   (go-to-with-status req nil app-status default-page false))
  (^Response [req route-data app-status default-page]
   (go-to-with-status req nil app-status default-page false))
  (^Response [req route-data app-status default-page hx-status-flag]
   (let [route-data (or route-data (http/get-route-data req))
         go-to-fn   (if (use-hx? req route-data hx-status-flag)
                      hx-go-to-with-status
                      http-go-to-with-status)]
     (go-to-fn req
               (or (get-in route-data [:error/destinations app-status] default-page)
                   (get route-data :error/destination))))))

;; Business logic errors

(defn handle-error
  "Sets the right response function (which sets HTTP status code) and optional HTTP
  header on a basis of the given `app-status`. If `app-status` is sequential and contains
  multiple statuses, it will be replaced by the most accurate one.

  Returns proper HTMX response (when `common/use-hx?` returns `true` because the request
  indicated it is HTMX, or `:error/use-htmx?` route data key is set, or generic
  `:use-htmx?` route data key is set), or a HTTP redirect response (if HTMX is not in
  use or disabled), as a result of error encountered.

  Additionally, sets an HTTP response header named `header-name` (if set) with error
  status detected (mainly to be used by reverse proxies). If header name is `false`,
  no header is set. If header name is not set or is set to `nil`, the name `Error` is
  used. Returns updated `req`.

  For HTMX uses `hx-go-to-with-status`, for regular web uses
  `http-go-to-with-status`."
  (^Response [req]
   (handle-error req nil :auth/error nil nil))
  (^Response [req app-status]
   (handle-error req nil app-status nil nil))
  (^Response [req app-status default-view]
   (handle-error req nil app-status default-view nil))
  (^Response [req route-data app-status default-view]
   (handle-error req route-data app-status default-view nil))
  (^Response [req route-data app-status default-view header-name]
   (log/web-dbg req "Handling status:" app-status)
   (let [route-data     (or route-data (http/get-route-data req))
         header-name    (cond (nil? header-name)   "Error"
                              (false? header-name) nil
                              :else                (or (some-str header-name) "Error"))
         str-app-status (some-str app-status)
         req            (if (and str-app-status header-name)
                          (add-header req header-name str-app-status)
                          req)]
     (if (use-hx? req route-data :error/use-htmx?)
       (hx-go-to-with-status   req route-data app-status default-view)
       (http-go-to-with-status req route-data app-status default-view)))))

(defn response!
  "Returns a response on a basis of `app-status` (or `:response/status` key of the
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
    (if-let [status (some-keyword (or app-status (get req :response/status)))]
      (handle-error req
                    (or route-data (http/get-route-data req))
                    status
                    fallback-view
                    (if error-header error-header))
      req))))

;; Form errors

(defn http-handle-bad-request-form-params
  "Called by other functions to generate a redirect or to display a page with
  a form to be corrected because of a parameter error (induced manually or
  caused by coercion exception).

  Takes a request map `req`, a map of erroneous parameter identifiers to parameter
  types `errors`, a map of current parameter values `values`, a map of error
  explanations (`explanations`), page title (`title`), and optional session key
  `session-key` used when getting a session object from the `req`.

  The following arguments can be Delay objects and `clojure.core/force` will be
  applied to them before use: `errors`, `values`, `explanations`, `title`.

  Parameter type in `errors` map can be `nil`, meaning it is of unknown type.

  If there is a session then the `errors` map is stored in a session variable
  `:form-errors` under the `:errors` key (additionally, there is a `:dest` key
  identifying a path of the current page).

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

  - `:title` set to `title`,
  - `:form/errors` set to a map containing:
    - `:errors` mapped to `errors`,
    - `:params` mapped to `values`,
    - `:dest` mapped to destination URI;
  - `:coercion/errors` set to `explanations` map."
  [req route-data errors values explanations title session-key]
  (if-not (valuable? errors)
    req
    (let [route-data             (or route-data (http/get-route-data req))
          forced-orig-page       (get route-data :form-errors/page)
          orig-page              (or forced-orig-page (:page (get req :goto)))
          referer                (if (nil? orig-page) (some-str (get (get req :headers) "referer")))
          [orig-uri orig-params] (if referer (common/url->uri+params req referer))
          handling-previous?     (contains? (get req :query-params) "form-errors")]

      (if (and (or (valuable? orig-page) (valuable? orig-uri) referer)
               (not handling-previous?))

        ;; redirect to a form-submission page allowing user to correct errors
        ;; transfer form errors using query params or form params (if a session is present)

        (let [orig-uri     (if orig-uri (some-str orig-uri))
              orig-params  (if orig-uri orig-params)
              dest         (or orig-page orig-uri)
              dest-uri     (if (keyword? dest) (common/page req dest) dest)
              dest-uri     (some-str dest-uri)
              skey         (or session-key (get route-data :session-key))
              smap         (session/not-empty-of req skey)
              stored?      (if (and smap (session/valid? smap))
                             (session/put-var!
                              smap :form-errors
                              {:dest   dest-uri
                               :errors (force errors)
                               :params (force values)}))
              joint-params (qassoc orig-params "form-errors" (if stored? "" (coercion/join-errors (force errors))))]
          (if dest-uri
            (common/temporary-redirect req dest-uri nil joint-params)
            (resp/temporary-redirect (str referer
                                          (if (str/includes? referer "?") "&" "?")
                                          (common/query-string-encode req joint-params)))))

        ;; render a separate page describing invalid parameters
        ;; instead of current page

        (-> (assoc-app-data
             req
             :title                 title
             :coercion/errors       explanations
             :form/previous-errors? handling-previous?
             :form/errors           (delay {:dest   (:uri req)
                                            :errors (force errors)
                                            :params (force values)}))
            render-bad-params)))))

(defn hx-handle-bad-request-form-params
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
   (hx-handle-bad-request-form-params req route-data errors nil nil nil))
  ([req route-data errors values]
   (hx-handle-bad-request-form-params req route-data errors values nil nil))
  ([req route-data errors values explanations]
   (hx-handle-bad-request-form-params req route-data errors values explanations nil))
  ([req route-data errors values explanations title _]
   (hx-handle-bad-request-form-params req route-data errors values explanations title))
  ([req route-data errors values explanations title]
   (if-not (valuable? errors)
     req ;; generic error page?
     (let [route-data         (or route-data (http/get-route-data req))
           orig-page          (get route-data :form-errors/page)
           orig-page          (or orig-page (:page (get req :goto)))
           title              (or title (get route-data :form-errors/title))
           referer            (if (nil? orig-page) (some-str (get (get req :headers) "referer")))
           orig-uri           (if referer (some-str (:uri (parse-url referer))))
           src-page           (or orig-page (http/route-name req orig-uri))
           src-route-data     (if src-page (http/route-data req src-page))
           new-view           (get route-data :form-errors/view)
           new-view           (if (nil? new-view) (get src-route-data :app/view) new-view)
           new-view           (if (nil? new-view) (get src-route-data :name) new-view)
           new-layout         (get route-data :form-errors/layout)
           new-layout         (if (nil? new-layout) (get src-route-data :app/layout) new-layout)
           handling-previous? (contains? (get req :query-params) "form-errors")
           hx-targets         (get route-data :form-errors/retargets)
           hx-src-target      (if hx-targets (hx-target req))
           hx-target          (if hx-src-target (get hx-targets hx-src-target))
           hx-target          (some-str (or hx-target (get route-data :form-errors/target)))
           req                (if hx-target (add-header req :HX-Retarget hx-target) req)
           req                (if title (assoc-app-data req :title title) req)
           req                (if (nil? new-view)   req (qassoc req :app/view   new-view))
           req                (if (nil? new-layout) req (qassoc req :app/layout new-layout))]
       (-> req
           (assoc-app-data :coercion/errors       explanations
                           :form/previous-errors? handling-previous?
                           :form/errors           (delay {:dest   (:uri req)
                                                          :errors (force errors)
                                                          :params (force values)}))
           (render-bad-params nil new-view new-layout))))))

(defn handle-bad-request-form-params
  "Dispatch function which calls `hx-handle-bad-request-form-params` when HTMX is in
  use or `http-handle-bad-request-form-params` in other cases. HTMX mode will be in
  place when a client request contains `HX-Request` header set to a non-falsy and not
  empty value, or it was enforced by using route data key `:form-errors/use-htmx?`
  set to a truthy value."
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
     (if (use-hx? req route-data :form-errors/use-htmx?)
       (hx-handle-bad-request-form-params   req route-data errors values explanations title)
       (http-handle-bad-request-form-params req route-data errors values explanations title session-key)))))

(defn- param-current-vals
  [req]
  (if-let [form-params (get req :form-params)]
    (let [good-params (->> (get (get req :parameters) :form)
                           (map/remove-empty-values)
                           (map/map-keys some-str))]
      (->> form-params
           (map/map-vals-by-k #(if (contains? good-params %) (some-str (get form-params %))))
           (map/remove-empty-values)))))

(defn- param-errors-stringify
  [errors]
  (if errors
    (reduce-kv #(qassoc %1 (some-str %2) (some-str %3)) errors errors)))

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
  `hx-handle-bad-request-form-params` for more info.

  If simple HTTP mode is in place the `http-handle-bad-request-form-params` is used,
  and the following will happen:

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
       (assoc-app-data req
                       :title (delay (translate-sub :parameters/error))
                       :form/errors nil
                       :coercion/errors nil)
       (handle-bad-request-form-params req
                                       (delay (param-errors-stringify errors))
                                       (delay (param-current-vals req))
                                       nil
                                       (delay (translate-sub :parameters/error))
                                       session-key)))))

;; Linking helpers

(p/import-vars [amelinium.common
                path localized-path])

;; Language helpers

(p/import-vars [amelinium.common
                lang-id lang-str lang-config lang-from-req])
