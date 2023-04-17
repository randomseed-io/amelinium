(ns

    ^{:doc    "Common helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.common

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                           :as             set]
            [clojure.string                        :as             str]
            [clojure.core.memoize                  :as             mem]
            [clojure.java.io                       :as              io]
            [tick.core                             :as               t]
            [reitit.core                           :as               r]
            [reitit.coercion                       :as        coercion]
            [ring.util.response]
            [ring.util.codec                       :as           codec]
            [ring.util.http-response               :as            resp]
            [ring.util.request                     :as             req]
            [clj-uuid                              :as            uuid]
            [amelinium                             :refer         :all]
            [amelinium.types.session               :refer         :all]
            [amelinium.types.auth                  :refer         :all]
            [amelinium.proto.identity              :as             pid]
            [amelinium.identity                    :as        identity]
            [amelinium.auth                        :as            auth]
            [amelinium.http                        :as            http]
            [amelinium.http.middleware.roles       :as           roles]
            [amelinium.http.middleware.language    :as        language]
            [amelinium.http.middleware.session     :as         session]
            [amelinium.common.oplog.auth           :as      oplog-auth]
            [amelinium.i18n                        :as            i18n]
            [amelinium.logging                     :as             log]
            [phone-number.core                     :as           phone]
            [io.randomseed.utils.time              :as            time]
            [io.randomseed.utils.vec               :as             vec]
            [io.randomseed.utils.map               :as             map]
            [io.randomseed.utils.map               :refer     [qassoc]]
            [io.randomseed.utils.db                :as              db]
            [io.randomseed.utils                   :refer         :all])

  (:import [java.time        Duration]
           [java.time.format DateTimeFormatter]
           [amelinium        Session AuthSettings AuthConfig]
           [lazy_map.core    LazyMapEntry LazyMap]
           [reitit.core      Match]))

;; Operations logging

(defn oplog-config
  "Returns operations logger configuration obtained from a request or a `Match`
  object."
  [req-or-match]
  (http/get-route-data req-or-match :oplog/config))

(defn oplog-logger-from-route-data
  "Retrieves operations logger function from a given route data map (via
  `:oplog/config` key and then the `:fn/reporter` key) and creates a wrapper for
  handling keyword arguments."
  [route-data]
  (if-some [lgr (get (get route-data :oplog/config) :fn/reporter)]
    (fn [& {:as message}] (lgr message))
    (constantly nil)))

(defn oplog-logger
  "Retrieves operations logger function from a current route data (via `:oplog/config`
  key and then the `:fn/reporter` key), and if that fails, tries to retrieve it using
  `:oplog/config` key of the request map (and `:fn/reporter` sub-key). When everything
  fails it will fall back to a global variable `amelinium.common.oplog.auth/log`. The
  given argument can be either a request map or a `Match` object."
  [req-or-match]
  (if-some [lgr (or (get (http/get-route-data req-or-match :oplog/config) :fn/reporter)
                    oplog-auth/log)]
    (fn [& {:as message}] (lgr message))
    (constantly nil)))

(defn oplog-logger-populated
  "Creates operations logging function on a basis of operations logger retrieved by
  getting `:oplog/logger` key of the request (`req`), and if that fails by calling
  `oplog-logger-from-route-data` (when `route-data` is given), and if that fails by
  calling `oplog-logger` function on a `req` (which falls back to
  `amelinium.common.oplog.auth/log`)."
  ([req]
   (or (get req :oplog/logger)
       (oplog-logger req)))
  ([req route-data]
   (or (get req :oplog/logger)
       (oplog-logger-from-route-data route-data)
       (oplog-logger req))))

(defn oplog
  "Logs operation using operations logger. First argument should be a request map or a
  `Match` object containing configuration associated with the current route data
  under the `:oplog/config` key."
  [req-or-match & message]
  (if-some [lgr (oplog-logger-populated req-or-match)] (lgr message)))

;; Routing data and settings helpers

(defn router-match?
  "Returns true if the given argument is Reitit's Match object."
  [v]
  (instance? Match v))

(defn on-page?
  "Checks if a current page matches the given route name (if an identifier is given) or
  the exact path. For multiple page names or paths, it returns true when any of them
  matches."
  ([]
   false)
  ([req]
   true)
  ([req page-id-or-path]
   (if (ident? page-id-or-path)
     (let [rn (http/route-name req)]
       (and (some? rn) (identical? page-id-or-path rn)))
     (let [pn (http/path req)]
       (and (some? pn) (= page-id-or-path pn)))))
  ([req page-id-or-path & more]
   (let [ar (cons page-id-or-path more)
         mt (http/match req)
         rn (http/route-name mt)
         pn (http/path mt)]
     (if (nil? rn)
       (if (nil? pn)
         false
         (boolean (some #{pn} (remove ident? ar))))
       (if (nil? pn)
         (boolean (some #{rn} (filter ident? ar)))
         (boolean (some #(= (if (ident? %) rn pn) %) ar)))))))

(defn lang-param
  "Returns language parameter ID obtained from language settings. Falls back to `:lang`
  when nothing was found."
  [req]
  (or (language/param req) :lang))

(defn guess-lang-param
  "For the given src argument, tries to obtain a language ID. If it's a map it looks
  for `:param` key and for `:language/settings` if that
  fails. If `:language/settings` is found, it will try to get :param, assuming
  it's a map too. If the argument is not a map it will simply convert it into a
  keyword (without a namespace). If all of that fails (e.g. the src is nil) then
  the :lang keyword is returned."
  ([] :lang)
  ([src]
   (or (if (map? src)
         (or (get src :param)
             (some-> (get src :language/settings) :param))
         (some-keyword-simple src))
       :lang)))

(defn login-page?
  "Returns true if the current (or given as a match) page is a login page (has :login-page?
  route data set to a truthy value)."
  ([req]            (boolean (http/get-route-data req :login-page?)))
  ([req ring-match] (boolean (http/get-route-data ring-match req :login-page?))))

(defn auth-page?
  "Returns true if the current (or given as a match) page is an authentication
  page (has :auth-page? route data set to a truthy value)."
  ([req]            (boolean (http/get-route-data req :auth-page?)))
  ([req ring-match] (boolean (http/get-route-data ring-match req :auth-page?))))

(defn login-auth-state
  "Helper which returns 2-element vector telling if the current (or given as a match)
  page is a login page (1st element) and/or an auth page (2nd element)."
  ([req]
   (let [rd     (http/get-route-data req)
         login? (boolean (get rd :login-page?))
         auth?  (boolean (get rd :auth-page?))]
     [login? auth?]))
  ([req ring-match]
   (let [rd     (http/get-route-data req)
         login? (boolean (get rd :login-page?))
         auth?  (boolean (get rd :auth-page?))]
     (cons login? (cons auth? nil))))
  ([req login-page-data auth-page-data]
   (let [rd     (http/get-route-data req)
         login? (boolean (get rd (or login-page-data :login-page?)))
         auth?  (boolean (get rd (or auth-page-data  :auth-page?)))]
     [login? auth?]))
  ([req ring-match login-page-data auth-page-data]
   (let [rd     (http/get-route-data req)
         login? (boolean (get rd (or login-page-data :login-page?)))
         auth?  (boolean (get rd (or auth-page-data  :auth-page?)))]
     [login? auth?])))

;; Path parsing

(def ^:const max-url-len      8192)
(def ^:const page-cache-len   4096)
(def ^:const fast-url-matcher (re-pattern "^[a-zA-Z0-9\\+\\.\\-]+\\:"))
(def ^:const path-splitter    (re-pattern "([^\\?\\#]+)(\\#[^\\?]+)?(\\?.*)?"))
(def ^:const split-qparams    (re-pattern "[^\\?\\#]+|[\\?\\#].*"))
(def ^:const on-slash         (re-pattern "/"))
(def ^:const slash-break      (re-pattern "[^/]+|/"))

(defn is-url?
  [s]
  (if (and s (string? s))
    (let [^String s s]
      (and (not-empty-string? s)
           (not= \/ (.charAt s 0))
           (some? (re-find fast-url-matcher s))))))

(defn path-variants-core
  "Generates a list of all possible language variants of a path."
  {:no-doc true}
  ([path lang-id]
   (if-some [path (some-str path)]
     (if-some [lang (some-str lang-id)]
       (let [[p s] (re-seq split-qparams path)]
         (path-variants-core p lang s)))))
  ([path lang suffix]
   (let [pathc (count path)]
     (if (and (= 1 pathc) (= path "/"))
       (cons (str "/" lang "/") (cons (str "/" lang) nil))
       (let [abs?   (= \/ (.charAt ^String path (unchecked-int 0)))
             trail? (= \/ (.charAt ^String path (unchecked-dec-int pathc)))
             segs   (str/split path on-slash)
             paths  (map-indexed
                     (if trail?
                       (fn [i _] (str (str/join "/" (insert-at (unchecked-inc i) segs lang)) "/"))
                       (fn [i _] (str/join "/" (insert-at (unchecked-inc i) segs lang))))
                     segs)
             paths  (if abs?
                      paths
                      (->> paths
                           (cons (str "/" lang "/" path)) lazy-seq
                           (cons (str lang "/" path))     lazy-seq))
             paths  (concat paths
                            (lazy-seq
                             (cons (if trail? (str path lang) (str path "/" lang "/"))
                                   nil)))]
         (if suffix
           (map #(str % suffix) paths)
           paths))))))

(def ^{:arglists '([path lang-id]
                   [path lang suffix])}
  path-variants
  "Generates a list of all possible language variants of a path."
  (db/memoize+ path-variants-core 2048 256))

(defn path-param
  "Returns a parameter if the given path contains it and it is set. Otherwise it
  returns nil."
  ([req-or-match param]
   (get (or (get req-or-match :path-params)
            (get (http/match req-or-match) :path-params))
        param))
  ([req path param]
   (get (some->> path (r/match-by-path (http/router req)) :path-params) param))
  ([_ path param router]
   (get (some->> path (r/match-by-path router) :path-params) param)))

(defn path-params
  "Returns a map of parameters if the given path contains it. Otherwise it returns nil."
  ([req-or-match]
   (or (get req-or-match :path-params)
       (get (http/match req-or-match) :path-params)))
  ([req path]
   (some->> path (r/match-by-path (http/router req)) :path-params))
  ([_ path router]
   (some->> path (r/match-by-path router) :path-params)))

(defn path-language
  "Returns a language string if the given path contains a language parameter. Otherwise
  it returns nil."
  ([req]
   (path-param req (lang-param req)))
  ([req-or-match path-or-lang-settings]
   (if (router-match? req-or-match)
     (path-param req-or-match (guess-lang-param path-or-lang-settings))
     (path-param req-or-match path-or-lang-settings (lang-param req-or-match))))
  ([req path router]
   (path-param nil path (lang-param req) (or router (http/router req))))
  ([_ path router language-settings-or-param]
   (path-param nil path (guess-lang-param language-settings-or-param) router)))

(defn split-query-params-simple
  "Splits path into 2 components: path string and location / query params
  string. Returns a sequence."
  [path]
  (if path (re-seq split-qparams path)))

(defn split-query-params
  "Splits path into 3 string components: path, location and query params. Returns a
  vector."
  [path]
  (if path
    (if-some [segs (first (re-seq path-splitter path))]
      (if (and (= 4 (count segs)) (some? (nth segs 1)))
        (subvec segs 1)
        [path nil nil])
      [path nil nil])))

(defn- req-param-path
  "Checks if the match has a parameter set to the given value. Used to re-check after a
  route was found."
  ([router match-or-path param pvalue]
   (req-param-path router match-or-path param pvalue nil))
  ([router match-or-path param pvalue query-params]
   (if (map? match-or-path)
     (let [path      (some-> match-or-path (r/match->path query-params))
           [path
            location
            qparams] (split-query-params path)]
       (if (some->> path
                    (r/match-by-path router)
                    :path-params param #{pvalue})
         (str path location qparams)))
     (if match-or-path
       (let [[path location qparams] (split-query-params match-or-path)
             qparams                 (if-not (not-empty query-params) qparams)
             m                       (r/match-by-path router path)]
         (if (some-> m :path-params param #{pvalue})
           (some-> (r/match->path m query-params)
                   (str location qparams))))))))

(defn path-slash-variants
  "Returns a 2-element vector of a path containing its two variants: with and without a
  trailing slash. The original path is always placed first.

  If the path is empty, vector of an empty string and a slash is returned. If the
  path is a slash, vector of a slash and an empty string is returned.

  If the path is `nil`, it returns `nil`."
  [uri]
  (if uri
    (let [c (unchecked-int (count uri))]
      (if (pos? c)
        (if (= \/ (.charAt ^String uri (unchecked-dec-int c)))
          [uri (subs uri 0 (dec c))]
          [uri (str uri "/")])
        ["" "/"]))))

(defn has-param?
  "Checks if the given route match can be parameterized with a parameter of the given
  id."
  [match param]
  (if-some [param (some-keyword-simple param)]
    (or (contains? (get match :required) param)
        (if-some [t (get match :template)] (some? (some #{(str param)} (re-seq slash-break t)))))))

(defn template-path
  "Replaces parameters in the given path using a template."
  ([match params]
   (template-path match params nil))
  ([match params query-params]
   (if match
     (template-path (r/match->path match query-params)
                    (get match :template)
                    params nil)))
  ([path template params _]
   (if-some [template (some-str template)]
     (->> (map (map/map-keys str params)
               (concat (re-seq slash-break template) (repeat nil))
               (re-seq slash-break (str path)))
          (apply str)))))

(defn path-template-with-param
  "Returns a path template for the given match if the route supports the given
  parameter."
  ([match required-param]
   (path-template-with-param match required-param nil))
  ([match required-param short-circuit]
   (if-some [required-param (some-keyword-simple required-param)]
     (if-some [t (get match :template)]
       (if (or (some? short-circuit)
               (contains? (get match :required) required-param)
               (some #{(str required-param)} (re-seq slash-break t)))
         t)))))

(defn parameterized-page-core
  {:no-doc true}
  [param rtr id pvalue params query-params require-param? name-path-fallback?]
  (let [pvalue (some-str pvalue)
        param  (some-keyword-simple param)]
    (if (ident? id)

      ;; identifier given (route name)

      (if-some [m (r/match-by-name rtr id (qassoc params param pvalue))]
        (if require-param?

          ;; path must be parameterized with our parameter

          (or (req-param-path rtr m param pvalue query-params)
              (if name-path-fallback?
                (if-some [path (some-str (r/match->path m))]
                  (parameterized-page-core param rtr path
                                           pvalue params query-params
                                           true name-path-fallback?))))

          ;; we do not require path to be parameterized with our parameter

          (r/match->path m query-params)))

      ;; path given

      (if id
        (let [[id location qparams] (split-query-params id)
              qparams               (if-not (not-empty query-params) qparams)
              m                     (r/match-by-path rtr id)
              cur-pvalue            (get (get m :path-params) param)]
          (if (= cur-pvalue pvalue)

            ;; path is parameterized and the parameter value is the same

            (some-> (r/match->path m query-params) (str location qparams))

            ;; path is not parameterized or the parameter value is different

            (if-some [template (path-template-with-param m param cur-pvalue)]

              ;; path is parameterized with our parameter
              ;; we can re-parameterize the path by calling template-path

              (if-some [p (template-path m {param pvalue})]
                (if require-param?
                  (some-> (req-param-path  rtr p param pvalue query-params)    (str location qparams))
                  (some-> (r/match-by-path rtr p) (r/match->path query-params) (str location qparams))))

              ;; path is not parameterized with our parameter

              (if require-param?

                ;; parameter is required and path is not parameterized with it

                (some-> (some #(req-param-path rtr % param pvalue query-params)
                              (path-variants id pvalue))
                        (str location qparams))

                ;; parameter is not required and path is not parameterized with it

                (if (identical? :brute-force name-path-fallback?)

                  ;; brute-force parameter injection was requested

                  (some-> (some #(some-> (r/match-by-path rtr %) (r/match->path query-params))
                                (path-variants id pvalue))
                          (str location qparams))

                  ;; no parameter injection, return the path

                  (some-> (some #(some-> (r/match-by-path rtr %) (r/match->path query-params))
                                (path-slash-variants id))
                          (str location qparams)))))))))))

(def ^{:private  true
       :tag      String
       :arglists '(^String [param rtr id param-value params query-params require-param? name-path-fallback?])}
  parameterized-page-mem
  (mem/lu parameterized-page-core :lu/threshold page-cache-len))

(defn parameterized-page
  "Generates a path for the given page identifier (which may be a name expressed with
  an identifier, preferably a keyword, or a path expressed as a string) and a
  parameter with the given value.

  Optional path parameters may be given. They will be used to match a page by name if
  it requires additional parameters to be present.

  Examples:

  `(parameterized-page req)`
  `(parameterized-page req :login-page)`
  `(parameterized-page req :login-page :lang :pl)`
  `(parameterized-page req :login-page :lang :pl {:client \"wow-corp\"})`
  `(parameterized-page req \"/login/page/\")`
  `(parameterized-page req \"/login/page/\" :lang :pl)`
  `(parameterized-page req \"/en/login/page/\" :lang :pl)`

  When called with just a request map, returns a path of the current page if the page
  exists. When called with a page name or path, it returns a path if the page
  exists.

  The optional `name-path-fallback?` argument, when set to a truthy value (default is
  `false`), causes non-matching route identifier to be retried by extracting its path
  and calling this function for that path given as a string.

  The optional `require-param?` argument (set to `false` when not given) enables
  extra check eliminating pages which do not support the given parameter, yet would
  be matched. Example:

  `(parameterized-page req :login-page :lang :pl true)`

   will fail if there is no parameter `:lang` handled by the route named
  `:login-page`.

  When the given path is already parameterized then re-parameterized path is
  generated and checked if it exists, unless the value of the parameter is the same
  as the existing one. In such case the path is returned after a quick existence
  check.

  Additional path parameters (`path-params`) can be given to be used when matching by
  name. Giving extra (unknown to route) parameters does not affect lookup. Giving
  `path-params` when matching by path causes them to be silently ignored.

  Additional query parameters (`query-params`) can be given. They will be used when
  generating path. If the path was given and it already contains query parameters,
  they will be replaced.

  If the path is given instead of a route identifier, it must exist (after being
  equipped with any extra parameters, if needed, using `path-params`).

  When the `require-param?` is set to `false` (default) and a path is given but it
  does not require this parameter, a path will be generated by testing whether it
  exists for regular and for slash-stripped or slash-added variant (the original
  variant goes first).

  When the `require-param?` is set to `false` (default) and a path is given but it
  does not require this parameter plus the `name-path-fallback?` argument is set to
  `:brute-force`, all possible path variants (with parameter injected within its
  succesive segments) will be tried in hope that one will exist, regardless of
  parameter name that matches. Use it with caution as it may give weird matches; if
  for example, there is a route path `/users/:id` defined, and the given path is
  `/users/` with `:lang` parameter set to `:pl`, then it will match `/users/pl` even
  though the parameter name is different.

  When the `require-param?` is set to `true` and a path is given but it does not
  require this parameter, all possible path variants (with parameter injected within
  its succesive segments) will be tried in hope that one will match, being a path
  that requires this parameter."
  {:tag      String
   :arglists '(^String []
               ^String [req]
               ^String [req name-or-path]
               ^String [req name-or-path param param-value]
               ^String [req name-or-path param param-value path-params]
               ^String [req name-or-path param param-value path-params query-params]
               ^String [req name-or-path param param-value require-param?]
               ^String [req name-or-path param param-value path-params require-param?]
               ^String [req name-or-path param param-value path-params query-params require-param?]
               ^String [req name-or-path param param-value path-params query-params require-param? name-path-fallback?]
               ^String [_   name-or-path param param-value path-params query-params require-param? name-path-fallback? router])}
  (^String [] "/")
  (^String [req]
   (r/match->path (get req ::r/match) (get req :query-params)))
  (^String [req id-or-path]
   (if-some [rtr (get req ::r/router)]
     (if (ident? id-or-path)
       (some-> (r/match-by-name rtr id-or-path) r/match->path)
       (if-some [path (some-str id-or-path)]
         (let [[path location qparams] (split-query-params path)]
           (some-> (r/match-by-path rtr path) r/match->path (str location qparams)))))))
  (^String [req id-or-path param param-value]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem param rtr id-or-path param-value nil nil false false)))
  (^String [req id-or-path param param-value params-or-require-param?]
   (if-some [rtr (get req ::r/router)]
     (if (boolean? params-or-require-param?)
       (parameterized-page-mem param rtr id-or-path param-value nil nil params-or-require-param? false)
       (parameterized-page-mem param rtr id-or-path param-value params-or-require-param? nil false false))))
  (^String [req id-or-path param param-value params query-params-or-require-param?]
   (if-some [rtr (get req ::r/router)]
     (if (boolean? query-params-or-require-param?)
       (parameterized-page-mem param rtr id-or-path param-value params nil query-params-or-require-param? false)
       (parameterized-page-mem param rtr id-or-path param-value params query-params-or-require-param? false false))))
  (^String [req id-or-path param param-value params query-params require-param?]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem param rtr id-or-path param-value params query-params require-param? false)))
  (^String [req id-or-path param param-value params query-params require-param? name-path-fallback?]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem param rtr id-or-path param-value params query-params require-param? name-path-fallback?)))
  (^String [_ id-or-path param param-value params query-params require-param? name-path-fallback? router]
   (if (some? router)
     (parameterized-page-mem param router id-or-path param-value params query-params require-param? name-path-fallback?))))

(defn localized-page
  "Generates a page path for the given page identifier and language identifier. When
  called with just a request map, returns a path of the current page but re-generated
  to support current language in use (taken from `:language/str` key of the request
  map).

  The optional `name-path-fallback?` argument, when set to a truthy value (`true` by
  default), causes non-matching route identifier to be retried by extracting its path
  and calling this function for that path given as a string.

  The optional `lang-required?` argument (set to `false` when not given) enables
  extra check eliminating pages which do not support the given language
  parameter. Example:

  `(localized-page req :login-page :pl true)`

   will fail if there is no parameter `:lang` handled by the route named
  `:login-page`.

  When the given path is already parameterized with language then re-parameterized
  path is generated and checked if it exists, unless the value of the parameter is
  the same as the existing one. In such case the path is returned after a quick
  existence check.

  Additional path parameters (`path-params`) can be given to be used when matching by
  name. Giving extra (unknown to route) parameters does not affect lookup. Giving
  `path-params` when matching by path causes them to be silently ignored.

  Additional query parameters (`query-params`) can be given. They will be used when
  generating path. If the path was given and it already contains query parameters,
  they will be replaced.

  If the path is given instead of a route identifier, it must exist (after being
  equipped with any extra parameters, if needed, using `path-params`).

  When the `lang-required?` is set to `false` (default) and a path is given but it
  does not require language parameter, a path will be generated by testing whether it
  exists for regular and for slash-stripped or slash-added variant (the original
  variant goes first).

  When the `lang-required?` is set to `false` (default) and a path is given but it
  does not require language parameter plus the `name-path-fallback?` argument is set
  to `:brute-force`, all possible path variants (with language parameter injected
  within its succesive segments) will be tried in hope that one will exist,
  regardless of parameter name that matches. Use it with caution as it may give weird
  matches; if for example, there is a route path `/users/:id` defined, and the given
  path is `/users/` with language set to `:pl`, then it will match `/users/pl` even
  though the parameter is not related to a language.

  When the `lang-required?` is set to `true` and a path is given but it does not
  require a language parameter, all possible path variants (with parameter injected
  within its succesive segments) will be tried in hope that one will match, being a
  path that requires language parameter."
  {:tag String
   :arglists '(^String [req]
               ^String [req name-or-path]
               ^String [req name-or-path path-params]
               ^String [req name-or-path path-params query-params]
               ^String [req name-or-path lang]
               ^String [req name-or-path lang path-params]
               ^String [req name-or-path lang path-params query-params]
               ^String [req name-or-path lang lang-required?]
               ^String [req name-or-path lang path-params lang-required?]
               ^String [req name-or-path lang path-params query-params lang-required?]
               ^String [req name-or-path lang path-params query-params lang-required? name-path-fallback?]
               ^String [req name-or-path lang path-params query-params lang-required? name-path-fallback? router]
               ^String [_   name-or-path lang path-params query-params lang-required? name-path-fallback? router language-settings-or-param])}
  (^String [] "/")
  (^String [req]
   (let [m (get req ::r/match)]
     (localized-page req
                     (r/match->path m)
                     (get req :language/str)
                     (or (get req :path-params) (get m :path-params))
                     (get req :query-params)
                     false true)))
  (^String [req name-or-path]
   (localized-page req name-or-path
                   (get req :language/str)
                   nil nil false true))
  (^String [req name-or-path lang]
   (localized-page req name-or-path
                   (or lang (get req :language/str))
                   nil nil false true))
  (^String [req name-or-path lang params-or-lang-required?]
   (if-some [rtr (get req ::r/router)]
     (if (boolean? params-or-lang-required?)
       (parameterized-page-mem (lang-param req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               nil nil
                               params-or-lang-required?
                               true)
       (parameterized-page-mem (lang-param req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               params-or-lang-required?
                               nil false true))))
  (^String [req name-or-path lang params query-params-or-lang-required?]
   (if-some [rtr (get req ::r/router)]
     (if (boolean? query-params-or-lang-required?)
       (parameterized-page-mem (lang-param req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               params nil
                               query-params-or-lang-required?
                               true)
       (parameterized-page-mem (lang-param req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               params query-params-or-lang-required?
                               false true))))
  (^String [req name-or-path lang params query-params lang-required?]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             lang-required? true)))
  (^String [req name-or-path lang params query-params lang-required? name-path-fallback?]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             lang-required?
                             name-path-fallback?)))
  (^String [req name-or-path lang params query-params lang-required? name-path-fallback? router]
   (if (some? router)
     (parameterized-page-mem (lang-param req)
                             router name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             lang-required?
                             name-path-fallback?)))
  (^String [_ name-or-path lang params query-params lang-required? name-path-fallback? router language-settings-or-param]
   (if (some? router)
     (parameterized-page-mem (guess-lang-param language-settings-or-param)
                             router name-or-path lang
                             params query-params
                             lang-required?
                             name-path-fallback?))))

(defn strictly-localized-page
  "Same as `localized-page` with `lang-required?` always set to `true`,
  `name-path-fallback?` set to `false`, and with less arities supported. When the
  language version of a page identified by its name is not present it will return
  `nil`."
  (^String [req]
   (let [m (get req ::r/match)]
     (localized-page req
                     (r/match->path m)
                     (get req :language/str)
                     (or (get req :path-params) (get m :path-params))
                     (get req :query-params)
                     true false)))
  (^String [req name-or-path]
   (localized-page req
                   name-or-path
                   (get req :language/str)
                   nil nil true false))
  (^String [req name-or-path lang]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             nil nil true false)))
  (^String [req name-or-path lang params]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params nil true false)))
  (^String [req name-or-path lang params query-params]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             true false)))
  (^String [name-or-path lang params query-params router lang-param]
   (parameterized-page-mem lang-param
                           router name-or-path
                           lang params query-params
                           true false)))

(defn- ^{:tag String} page-core
  (^String [rtr id]
   (page-core rtr id nil nil nil))
  (^String [rtr id params]
   (page-core rtr id params nil nil))
  (^String [rtr id params query-params]
   (page-core rtr id params query-params nil))
  (^String [rtr id params query-params fb-lang-settings]
   (if rtr
     (if (ident? id)
       ;; identifier given (route name)
       (let [params (if fb-lang-settings (apply assoc params fb-lang-settings) params)]
         (some-> (r/match-by-name rtr id params)
                 (r/match->path query-params)))
       ;; path given
       (if id
         (let [[id location qparams] (split-query-params id)
               qparams               (if-not (not-empty query-params) qparams)]
           (some-> (r/match-by-path rtr id)
                   (r/match->path query-params)
                   (str location qparams))))))))

(defn lang-from-req
  [req]
  (if-some [lang-param (lang-param req)]
    (if-some [lang-str (get req :language/str)]
      [lang-param lang-str])))

(defn page
  "Generates a page path for the given page identifier (a route name) or a path, and
  optional language identifier. When called with just a request map, returns a path
  of the current page.

  It tries to be optimistic. When called for a page identified by its name (expressed
  as an identifier, usually a keyword) and with a language parameter to be found (so
  it cannot be looked up using just a name alone), it will use currently detected
  language obtained from the given request map (key `:language/str`), and will use
  it.

  Additional query parameters (`query-params`) can be given. They will be used when
  generating path with `localized-page`. If the path was given and it already
  contains query parameters, they will be replaced.

  Additional path parameters (`path-params`) may be given. Giving extra (unknown to
  route) parameters does not affect lookup. Giving `path-params` when matching by
  path causes them to be silently ignored.

  When invoked with a language parameter, calls `localized-page` internally.

  When invoked with a language parameter, the `lang-required?` argument may be
  used (by default set to `false`) to check if a matching route is parameterized with
  a language parameter. This is to ensure that only a localized route is used.

  When invoked with a language parameter, the optional `name-path-fallback?`
  argument may be used (set to `true` by default) to cause non-matching route
  identifier to be retried by extracting its path and calling `localized-page`
  function for that path expressed as a string.

  If the path is given instead of a route identifier, it must exist (after being
  equipped with any extra parameters, if needed, using `path-params`).

  When invoked with a language parameter, the `lang-required?` is set to
  `false` (default), and a path is given but it does not require language parameter,
  a path will be generated by testing whether it exists for regular and for
  slash-stripped or slash-added variant (the original variant goes first).

  When invoked with a language parameter, the `lang-required?` is set to
  `false` (default), and a path is given but it does not require language parameter
  plus the `name-path-fallback?` argument is set to `:brute-force`, all possible path
  variants (with language parameter injected within its succesive segments) will be
  tried in hope that one will exist, regardless of parameter name that matches. Use
  it with caution as it may give weird matches; if for example, there is a route path
  `/users/:id` defined, and the given path is `/users/` with language set to `:pl`,
  then it will match `/users/pl` even though the parameter is not related to a
  language.

  When invoked with a language parameter, the `lang-required?` is set to `true`, and
  a path is given but it does not require a language parameter, all possible path
  variants (with language parameter injected within its succesive segments) will be
  tried in hope that one will match, being a path that requires language parameter."
  {:tag String
   :arglists '(^String [req]
               ^String [req name-or-path]
               ^String [req name-or-path path-params]
               ^String [req name-or-path path-params query-params]
               ^String [req name-or-path lang]
               ^String [req name-or-path lang path-params]
               ^String [req name-or-path lang path-params query-params]
               ^String [req name-or-path lang lang-required?]
               ^String [req name-or-path lang path-params lang-required?]
               ^String [req name-or-path lang path-params query-params lang-required?]
               ^String [req name-or-path lang path-params query-params lang-required? name-path-fallback?]
               ^String [_ _ name-or-path path-params query-params router hint-lang lang-settings-or-param]
               ^String [_   name-or-path lang path-params query-params lang-required? name-path-fallback? router lang-settings-or-param])}
  (^String [req]
   (r/match->path (get req ::r/match) (get req :query-params)))
  (^String [req name-or-path]
   (if (ident? name-or-path)
     ;; route name
     (page-core (get req ::r/router)
                name-or-path
                nil nil
                (lang-from-req req))
     ;; path
     (page-core (get req ::r/router)
                name-or-path
                nil nil nil)))
  (^String [req name-or-path lang-or-params]
   (if (or (nil? lang-or-params) (map? lang-or-params))
     ;; no language specified
     (if (ident? name-or-path)
       ;; route name
       (page-core (get req ::r/router)
                  name-or-path lang-or-params
                  nil
                  (lang-from-req req))
       ;; path
       (page-core (get req ::r/router)
                  name-or-path
                  lang-or-params
                  nil nil))
     ;; language specified
     (localized-page req name-or-path lang-or-params nil nil false false)))
  (^String [req name-or-path lang-or-params params-or-query-params-or-required?]
   (if (or (nil? lang-or-params) (map? lang-or-params))
     ;; no language specified
     (if (ident? name-or-path)
       ;; route name
       (page-core (get req ::r/router)
                  name-or-path
                  lang-or-params
                  params-or-query-params-or-required?
                  (lang-from-req req))
       ;; path
       (page-core (get req ::r/router)
                  name-or-path
                  lang-or-params
                  params-or-query-params-or-required?
                  nil))
     ;; language specified
     (if (boolean? params-or-query-params-or-required?)
       (localized-page req
                       name-or-path
                       lang-or-params
                       nil nil
                       params-or-query-params-or-required?
                       true)
       (localized-page req
                       name-or-path
                       lang-or-params
                       params-or-query-params-or-required?
                       nil
                       false
                       true))))
  (^String [req name-or-path lang-or-params params-or-query-params query-params-or-require-param?]
   (if (or (nil? lang-or-params) (map? lang-or-params))
     ;; no language specified
     (if (ident? name-or-path)
       ;; route name
       (page-core (get req ::r/router)
                  name-or-path
                  lang-or-params
                  params-or-query-params
                  (lang-from-req req))
       ;; path
       (page-core (get req ::r/router)
                  name-or-path
                  lang-or-params
                  params-or-query-params
                  nil))
     ;; language specified
     (if (boolean? query-params-or-require-param?)
       (localized-page req
                       name-or-path
                       lang-or-params
                       nil nil
                       query-params-or-require-param?
                       true)
       (localized-page req
                       name-or-path
                       lang-or-params
                       nil
                       query-params-or-require-param?
                       false
                       true))))
  (^String [req name-or-path lang params query-params require-param?]
   ;; language specified
   (localized-page req
                   name-or-path
                   lang
                   params
                   query-params
                   require-param?
                   true))
  (^String [req name-or-path lang params query-params require-param? name-path-fallback?]
   ;; language specified
   (localized-page req
                   name-or-path
                   lang
                   params
                   query-params
                   require-param?
                   name-path-fallback?))
  (^String [_ _ name-or-path params query-params router hint-lang lang-settings-or-param]
   (if (ident? name-or-path)
     ;; route name
     (page-core router name-or-path params query-params
                [(guess-lang-param lang-settings-or-param) hint-lang])
     ;; path
     (page-core router name-or-path params query-params nil)))
  (^String [_ name-or-path lang params query-params require-param? name-path-fallback? router lang-settings-or-param]
   ;; language specified
   (localized-page nil
                   name-or-path
                   lang
                   params
                   query-params
                   require-param?
                   name-path-fallback?
                   router
                   lang-settings-or-param)))

(defn current-page
  "Returns a path of the current page."
  ^String [req]
  (page req))

(defn current-page-id
  "Returns an identifier of a current page if it is defined for a HTTP route."
  ([req-or-match]
   (http/route-name req-or-match)))

(defn current-page-id-or-path
  "Returns an identifier of a current page if it is defined for a HTTP route or a path
  if the page name is not defined."
  [req]
  (or (http/route-name req) (page req)))

(defn login-page
  "Returns a path for the login page. The page must have ID of `:login`."
  (^String [req]         (page req :login))
  (^String [req lang-id] (page req :login lang-id)))

(defn auth-page
  "Returns a path for the authentication page. The page must have ID of `:welcome`."
  (^String [req]         (page req :welcome))
  (^String [req lang-id] (page req :welcome lang-id)))

;; Additional responses

(defn im-a-teapot
  "418 I'm a teapot
  The server cannot brew coffee because it is, permanently, a teapot."
  ([] (im-a-teapot nil))
  ([body]
   {:status  418
    :headers {}
    :body    body}))

(defn misdirected-request
  "421 Misdirected Request
  The request was directed at a server that is not able to produce a response
  (e.g. network balancer forwarded traffic to a wrong server)."
  ([] (misdirected-request nil))
  ([body]
   {:status  421
    :headers {}
    :body    body}))

(defn early-hints
  "103 Early Hints
   The server sends some response headers (e.g. HTML resource links) before final HTTP message."
  ([] (early-hints nil))
  ([body]
   {:status  103
    :headers {}
    :body    body}))

;; Rendering

(defn render
  "Universal response renderer. Returns the result of calling the `resp-fn` with
  headers attached (from `:response/headers` key of the `req`) unless the req is
  already a valid response. Arguments from the third are passed to `resp-fn`
  function."
  ([resp-fn]
   (resp-fn))
  ([resp-fn req]
   (if (nil? req)
     (resp-fn)
     (if (resp/response? req)
       req
       (if-some [headers (get req :response/headers)]
         (let [r (resp-fn)] (qassoc r :headers (conj (get r :headers) headers)))
         (resp-fn)))))
  ([resp-fn req a]
   (if (nil? req)
     (resp-fn a)
     (if (resp/response? req)
       req
       (if-some [headers (get req :response/headers)]
         (let [r (resp-fn a)] (qassoc r :headers (conj (get r :headers) headers)))
         (resp-fn a)))))
  ([resp-fn req a b]
   (if (nil? req)
     (resp-fn a b)
     (if (resp/response? req)
       req
       (if-some [headers (get req :response/headers)]
         (let [r (resp-fn a b)] (qassoc r :headers (conj (get r :headers) headers)))
         (resp-fn a b)))))
  ([resp-fn req a b c]
   (if (nil? req)
     (apply resp-fn a b c)
     (if (resp/response? req)
       req
       (if-some [headers (get req :response/headers)]
         (let [r (resp-fn a b c)] (qassoc r :headers (conj (get r :headers) headers)))
         (resp-fn a b c)))))
  ([resp-fn req a b c & more]
   (if (nil? req)
     (apply resp-fn a b c more)
     (if (resp/response? req)
       req
       (if-some [headers (get req :response/headers)]
         (let [r (apply resp-fn a b c more)] (qassoc r :headers (conj (get r :headers) headers)))
         (apply resp-fn a b c more))))))

(defn render-force
  "Universal body-less response renderer. Returns the result of calling the `resp-fn`
  with headers attached (from `:response/headers` key of the `req`). Arguments from
  the third are passed to `resp-fn` function."
  ([resp-fn]
   (resp-fn))
  ([resp-fn req]
   (if (nil? req)
     (resp-fn)
     (if-some [headers (get req :response/headers)]
       (let [r (resp-fn)] (qassoc r :headers (conj (get r :headers) headers)))
       (resp-fn))))
  ([resp-fn req a]
   (if (nil? req)
     (resp-fn a)
     (if-some [headers (get req :response/headers)]
       (let [r (resp-fn a)] (qassoc r :headers (conj (get r :headers) headers)))
       (resp-fn a))))
  ([resp-fn req a b]
   (if (nil? req)
     (resp-fn a b)
     (if-some [headers (get req :response/headers)]
       (let [r (resp-fn a b)] (qassoc r :headers (conj (get r :headers) headers)))
       (resp-fn a b))))
  ([resp-fn req a b c]
   (if (nil? req)
     (resp-fn a b c)
     (if-some [headers (get req :response/headers)]
       (let [r (resp-fn a b c)] (qassoc r :headers (conj (get r :headers) headers)))
       (resp-fn a b c))))
  ([resp-fn req a b c & more]
   (if (nil? req)
     (apply resp-fn a b c more)
     (if-some [headers (get req :response/headers)]
       (let [r (apply resp-fn a b c more)] (qassoc r :headers (conj (get r :headers) headers)))
       (apply resp-fn a b c more)))))

;; Redirects

(defn redirect
  "Generic redirect wrapper. The `f` should be a function which takes a request map and
  returns a response; should take at least one single argument which should be a
  URL. The URL will be parameterized with a language if required. If the language is
  given it uses the `localized-page` function. If there is no language given but the
  page identified by its name requires a language parameter to be set, it will be
  obtained from the given request map (under the key `:language/str`)."
  {:arglists '([f]
               [f req]
               [f url]
               [f req url]
               [f req name-or-path]
               [f req name-or-path path-params]
               [f req name-or-path path-params query-params]
               [f req name-or-path lang]
               [f req name-or-path lang path-params]
               [f req name-or-path lang path-params query-params]
               [f req name-or-path lang path-params query-params & more])}
  ([f]
   (f "/"))
  ([f req-or-url]
   (if (map? req-or-url)
     (render-force f req-or-url (page req-or-url))
     (render-force f nil req-or-url)))
  ([f req name-or-path]
   (if (is-url? name-or-path)
     (render-force f req name-or-path)
     (render-force f req (page req name-or-path))))
  ([f req name-or-path lang]
   (render-force f req (page req name-or-path lang)))
  ([f req name-or-path lang params]
   (render-force f req (page req name-or-path lang params)))
  ([f req name-or-path lang params query-params]
   (render-force f req (page req name-or-path lang params query-params)))
  ([f req name-or-path lang params query-params & more]
   (render-force f req (apply page req name-or-path lang params query-params more))))

(defn localized-redirect
  "Generic redirect wrapper. The `f` should be a function which takes a request map and
  returns a response; should take at least one single argument which should be a
  URL. The URL will be parameterized with a language. Works almost the same way as
  the `redirect` but it will generate a localized path using a language obtained from
  a request (under `:language/str` key) and if there will be no
  language-parameterized variant of the path, it will fail. Use this function to make
  sure that localized path will be produced, or `nil`."
  {:arglists '([f]
               [f req]
               [f url]
               [f req url]
               [f req name-or-path]
               [f req name-or-path path-params]
               [f req name-or-path path-params query-params]
               [f req name-or-path lang]
               [f req name-or-path lang path-params]
               [f req name-or-path lang path-params query-params]
               [f req name-or-path lang path-params query-params & more])}
  ([f]
   (f "/"))
  ([f req-or-url]
   (if (map? req-or-url)
     (render-force f req-or-url (localized-page req-or-url))
     (render-force f nil req-or-url)))
  ([f req name-or-path]
   (if (is-url? name-or-path)
     (render-force f req name-or-path)
     (render-force f req (localized-page req name-or-path))))
  ([f req name-or-path lang]
   (render-force f req (localized-page req name-or-path lang)))
  ([f req name-or-path lang params]
   (render-force f req (localized-page req name-or-path lang params)))
  ([f req name-or-path lang params query-params]
   (render-force f req (localized-page req name-or-path lang params query-params)))
  ([f req name-or-path lang params query-params & more]
   (render-force f req (apply localized-page req name-or-path lang params query-params more))))

(defmacro def-redirect
  "Generates a language-parameterized redirect function which acts like `redirect`."
  {:arglists '([name f]
               [name f http-code]
               [name doc f])}
  ([name f]
   (#'def-redirect &form &env name f nil nil))
  ([name f code _]
   (#'def-redirect &form &env name
                   (str "Uses the page function to calculate the destination path on a basis of page
  name (identifier) or a path (a string) and performs a redirect"
                        (if code (str " with code " code)) " to it using
  `" f "`. If the language is given it uses the `localized-page`
  function. If there is no language given but the page identified by its name
  requires a language parameter to be set, it will be obtained from the given request
  map (under the key `:language/str`).") f))
  ([name doc-or-f f-or-code]
   (if (pos-int? f-or-code)
     (#'def-redirect &form &env name doc-or-f f-or-code nil)
     `(let [f# ~f-or-code]
        (defn ~name ~doc-or-f
          {:arglists '([]
                       ~'[req]
                       ~'[url]
                       ~'[req url]
                       ~'[req name-or-path]
                       ~'[req name-or-path path-params]
                       ~'[req name-or-path path-params query-params]
                       ~'[req name-or-path lang]
                       ~'[req name-or-path lang path-params]
                       ~'[req name-or-path lang path-params query-params]
                       ~'[req name-or-path lang path-params query-params & more])}
          ([]
           (f# "/"))
          (~'[req-or-url]
           (if (map? ~'req-or-url)
             (render-force f# ~'req-or-url (page ~'req-or-url))
             (render-force f# nil ~'req-or-url)))
          (~'[req name-or-path]
           (if (is-url? ~'name-or-path)
             (render-force f# ~'req ~'name-or-path)
             (render-force f# ~'req (page ~'req ~'name-or-path))))
          (~'[req name-or-path lang]
           (render-force f# ~'req (page ~'req ~'name-or-path ~'lang)))
          (~'[req name-or-path lang params]
           (render-force f# ~'req (page ~'req ~'name-or-path ~'lang ~'params)))
          (~'[req name-or-path lang params query-params]
           (render-force f# ~'req (page ~'req ~'name-or-path ~'lang ~'params ~'query-params)))
          (~'[req name-or-path lang params query-params & more]
           (render-force f# ~'req (apply page ~'req ~'name-or-path ~'lang ~'params ~'query-params ~'more))))))))

(defmacro def-localized-redirect
  "Generates a language-parameterized redirect function which acts like
  `localized-redirect`."
  {:arglists '([name f]
               [name f http-code]
               [name doc f])}
  ([name f]
   (#'def-localized-redirect &form &env name f nil nil))
  ([name f code _]
   (#'def-localized-redirect &form &env name
                             (str "Uses the `localized-page` function to calculate the destination path on a basis of
  page name (identifier) or a path (a string) and performs a redirect"
                                  (if code (str " with code " code)) " to
  it using `" f "`. If the language is given it uses the `localized-page` function.
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
  sure that a localized path will be produced, or `nil`.") f))
  ([name doc-or-f f-or-code]
   (if (pos-int? f-or-code)
     (#'def-localized-redirect &form &env name doc-or-f f-or-code nil)
     `(let [f# ~f-or-code]
        (defn ~name ~doc-or-f
          {:arglists '([]
                       ~'[req]
                       ~'[url]
                       ~'[req url]
                       ~'[req name-or-path]
                       ~'[req name-or-path path-params]
                       ~'[req name-or-path path-params query-params]
                       ~'[req name-or-path lang]
                       ~'[req name-or-path lang path-params]
                       ~'[req name-or-path lang path-params query-params]
                       ~'[req name-or-path lang path-params query-params & more])}
          ([]
           (f# "/"))
          (~'[req-or-url]
           (if (map? ~'req-or-url)
             (render-force f# ~'req-or-url (localized-page ~'req-or-url))
             (render-force f# nil ~'req-or-url)))
          (~'[req name-or-path]
           (if (is-url? ~'name-or-path)
             (render-force f# ~'req ~'name-or-path)
             (render-force f# ~'req (localized-page ~'req ~'name-or-path))))
          (~'[req name-or-path lang]
           (render-force f# ~'req (localized-page ~'req ~'name-or-path ~'lang)))
          (~'[req name-or-path lang params]
           (render-force f# ~'req (localized-page ~'req ~'name-or-path ~'lang ~'params)))
          (~'[req name-or-path lang params query-params]
           (render-force f# ~'req (localized-page ~'req ~'name-or-path ~'lang ~'params ~'query-params)))
          (~'[req name-or-path lang params query-params & more]
           (render-force f# ~'req (apply localized-page ~'req ~'name-or-path ~'lang ~'params ~'query-params ~'more))))))))

(def-redirect           created                      resp/created             201)
(def-redirect           multiple-choices             resp/multiple-choices    300)
(def-redirect           moved-permanently            resp/moved-permanently   301)
(def-redirect           found                        resp/found               302)
(def-redirect           see-other                    resp/see-other           303)
(def-redirect           use-proxy                    resp/use-proxy           305)
(def-redirect           temporary-redirect           resp/temporary-redirect  307)
(def-redirect           permanent-redirect           resp/permanent-redirect  308)

(def-localized-redirect localized-created            resp/created             201)
(def-localized-redirect localized-multiple-choices   resp/multiple-choices    300)
(def-localized-redirect localized-moved-permanently  resp/moved-permanently   301)
(def-localized-redirect localized-found              resp/found               302)
(def-localized-redirect localized-see-other          resp/see-other           303)
(def-localized-redirect go-to                        resp/see-other           303)
(def-localized-redirect localized-use-proxy          resp/use-proxy           305)
(def-localized-redirect localized-temporary-redirect resp/temporary-redirect  307)
(def-localized-redirect move-to                      resp/temporary-redirect  307)
(def-localized-redirect localized-permanent-redirect resp/permanent-redirect  308)

(defn not-modified
  ([]           (resp/not-modified))
  ([req]        (if (nil? req) (resp/not-modified) (render-force resp/not-modified req)))
  ([req & more] (if (nil? req) (resp/not-modified) (render-force resp/not-modified req))))

(defn localized-not-modified
  ([]           (resp/not-modified))
  ([req]        (if (nil? req) (resp/not-modified) (render-force resp/not-modified req)))
  ([req & more] (if (nil? req) (resp/not-modified) (render-force resp/not-modified req))))

;; Language

(def ^{:tag      clojure.lang.Keyword
       :arglists '([req]
                   [req pickers]
                   [req picker-id]
                   [req pickers picker-id])}
  pick-language
  "Tries to pick the best language for a known user or a visitor. To be used (among
  other scenarios) after a successful log-in to show the right language version of a
  welcome page. Does not use pre-calculated values from a request map, instead
  triggers configured pickers from a default or given chain. Returns a keyword."
  language/pick)

(def ^{:tag      String
       :arglists '([req]
                   [req pickers]
                   [req picker-id]
                   [req pickers picker-id])}
  pick-language-str
  "Tries to pick the best language for a known user or a visitor. To be used (among
  other scenarios) after a successful log-in to show the right language version of a
  welcome page. Does not use pre-calculated values from a request map, instead
  triggers configured pickers from a default or given chain. Returns a string."
  (comp some-str language/pick))

(def ^{:tag      clojure.lang.Keyword
       :arglists '([req]
                   [req pickers]
                   [req picker-id]
                   [req pickers picker-id])}
  pick-language-without-fallback
  "Tries to pick the best language for a known user or a visitor. To be used (among
  other scenarios) after a successful log-in to show the right language version of a
  welcome page. Does not use pre-calculated values from a request map, instead
  triggers configured pickers from a default or given chain. When a language cannot
  be found it simply returns `nil` instead of a default language. Returns a keyword."
  language/pick-without-fallback)

(def ^{:tag      String
       :arglists '([req]
                   [req pickers]
                   [req picker-id]
                   [req pickers picker-id])}
  pick-language-str-without-fallback
  "Tries to pick the best language for a known user or a visitor. To be used (among
  other scenarios) after a successful log-in to show the right language version of a
  welcome page. Does not use pre-calculated values from a request map, instead
  triggers configured pickers from a default or given chain. When a language cannot
  be found it simply returns `nil` instead of a default language. Returns a string."
  (comp some-str language/pick-without-fallback))

;; Special redirects

(defn add-slash
  "Adds trailing slash to a path unless it already exists."
  ^String [^String uri]
  (if uri
    (let [c (unchecked-int (count uri))]
      (if (pos? c)
        (if (= \/ (.charAt ^String uri (unchecked-dec-int c))) uri (str uri "/"))
        "/"))
    "/"))

(defn slash-redir
  "Redirects to a slash-trailed version of the same URI. If the URI already has a
  slash, it returns a req."
  [req]
  (temporary-redirect req (add-slash (get req :uri))))

(defn lang-redir
  "Redirects to a best-suited language version of the URI. Uses `:browser` pickers
  chain to get the right language if the path is language-parameterized."
  [req]
  (move-to req
           (or (http/get-route-data req :destination) "/")
           (pick-language-str req :browser)))

;; Accounts

(def ^:const lock-wait-default (t/new-duration 10 :minutes))

(defn hard-lock-time
  "Gets a hard-lock time for a given user specified by a map having the :locked key."
  [user]
  (and user (get user :locked)))

(defn soft-lock-time
  "Gets a soft lock time for the given user specified by a map having :soft-locked
  key."
  [user]
  (and user (get user :soft-locked)))

(defn soft-lock-passed
  "Returns the time duration between soft lock and the given moment. If the duration is
  zero or negative, it returns nil."
  [user time]
  (if-some [lock-time (soft-lock-time user)]
    (let [d (t/between lock-time time)]
      (if (time/pos-duration? d) d))))

(defn lock-wait
  "Returns lock-wait configuration option taken from the authentication configuration
  map or given as a time duration. Does not connect to a database."
  [auth-config-or-lock-wait]
  (or (if (map? auth-config-or-lock-wait)
        (get (get auth-config-or-lock-wait :locking) :lock-wait)
        auth-config-or-lock-wait)
      lock-wait-default))

(defn hard-locked?
  "Returns true if the given user map contains the :locked key and a value associated
  with it is not nil. Does not connect to a database."
  ^Boolean [user]
  (some? (hard-lock-time user)))

(defn soft-locked?
  "Returns true if the given user account is soft-locked (the time amount which passed
  from the lock till the given time is lesser than the soft lock wait configuration
  option). Does not connect to a database."
  (^Boolean [lock-passed auth-config-or-lw]
   (if lock-passed
     (if-some [lock-wait (lock-wait auth-config-or-lw)]
       (t/< lock-passed lock-wait) false) false))
  (^Boolean [user auth-config-or-lw time]
   (if auth-config-or-lw
     (if-some [lock-passed (soft-lock-passed user time)]
       (if-some [lock-wait (lock-wait auth-config-or-lw)]
         (t/< lock-passed lock-wait) false) false) false)))

(defn soft-lock-remains
  "Returns the amount of time left before reaching lock-wait. If the amount is negative
  or zero, it returns nil. Does not connect to a database."
  ([lock-passed auth-config-or-lw]
   (if lock-passed
     (if-some [lock-wait (lock-wait auth-config-or-lw)]
       (t/- lock-wait lock-passed))))
  ([user auth-config-or-lw time]
   (if-some [lock-passed (soft-lock-passed user time)]
     (if-some [lock-wait (lock-wait auth-config-or-lw)]
       (let [d (t/- lock-wait lock-passed)]
         (if (time/pos-duration? d) d))))))

;; Sessions

(defn session
  "Gets a session map from the given request map."
  ([req]
   (session/of req))
  ([req session-key]
   (session/of req session-key)))

(defn session-config
  "Gets a session config map from the given request map."
  ([req]
   (session/config req))
  ([req session-key]
   (session/config req session-key)))

(defn config+session
  "Gets a session map and a session config map from the given request map. Returns a
  two-element vector."
  ([req]
   (config+session req :session))
  ([req session-key]
   (if-some [^Session s (session/of req session-key)]
     [(or (session/config s) (session/config req session-key)) s]
     [nil nil])))

(defn session-inject
  "Adds session data to a request map. Session key is obtained from the `config` field
  of `smap` or (if given) from a `session-key` argument."
  ([req smap]
   (session/inject req smap))
  ([req smap session-key]
   (session/inject req smap session-key)))

(defn session-variable-get-failed?
  [v]
  (session/get-variable-failed? v))

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
  [req ^Session sess]
  (if-some [sid (if sess (session/any-id sess))]
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
  [req ^Session sess]
  (if-some [sid (if sess (session/any-id sess))]
    (add-json-event-header req "HX-Trigger" "setSession" (session/id-field sess) sid true)
    req))

;; Context and roles

(defn roles-refresh
  [req]
  (roles/refresh req))

(defn has-any-role?
  [req role]
  (contains?
   (set (vals (get req :roles)))
   (some-keyword role)))

(defn has-role?
  ([req role]
   (contains? (get req :roles/in-context)
              (some-keyword role)))
  ([req role context]
   (contains? (roles/filter-in-context context (get req :roles) (get req :roles/config))
              (some-keyword role))))

(defn role-required!
  [req role]
  (if (has-role? req role)
    req
    (localized-temporary-redirect req :unauthorized)))

(defmacro with-role-only!
  [req role & body]
  `(do (role-required! req role)
       ~@body))

(defn roles-for-context
  ([req user-id context]
   (let [config (get req :roles/config)
         roles  (roles/get-roles-for-user-id config user-id)]
     (sort (get roles (some-keyword context)))))
  ([req context]
   (sort (get (get req :roles) (some-keyword context))))
  ([req]
   (sort (get req :roles/in-context))))

(defn roles-for-contexts
  ([req user-id]
   (let [config (get req :roles/config)
         roles  (roles/get-roles-for-user-id config user-id)]
     (sort-by first
              (map (comp (partial apply cons)
                         (juxt-seq first (comp sort second)))
                   roles))))
  ([req]
   (sort-by first
            (map (comp (partial apply cons)
                       (juxt-seq first (comp sort second)))
                 (get req :roles)))))

(defn default-contexts-labeler
  [_ ids]
  (map (juxt-seq some-keyword-simple some-str) ids))

(defn roles-matrix
  ([req]
   (roles-matrix req nil))
  ([req opts]
   (let [user-id        (or (get opts :user-id) (get opts :user/id))
         effective?     (get opts :effective?      false)
         inc-g?         (get opts :include-global? false)
         inc-s?         (get opts :include-self?   false)
         config         (get req :roles/config)
         gctx           (get config :global-context :!)
         known          (get config :roles)
         translation-fn (or (get opts :translation-fn) (get config :translation-fn))
         self-role      (get config :self-role)
         dynamic-roles  [:anonymous-role :logged-in-role :known-user-role]
         dynamic-roles  (set (filter identity (cons self-role (map config dynamic-roles))))
         translate-role (if translation-fn #(or (translation-fn %) (get known % %))  #(get known % %))
         sorter         (comp str/lower-case translate-role)
         all-roles-m    (if user-id (roles/get-roles-for-user-id config user-id) (get req :roles))
         roles-m        (dissoc all-roles-m gctx)
         groles         (get all-roles-m gctx #{})
         dyn-roles      (set/select groles dynamic-roles)
         reg-roles      (vals (map/qupdate all-roles-m gctx #(apply disj % dyn-roles)))
         reg-roles      (dedupe (sort-by sorter (apply concat reg-roles)))
         dyn-roles      (if (or inc-s? (not self-role)) dyn-roles (disj dyn-roles self-role))
         all-roles      (concat reg-roles (sort-by sorter dyn-roles))
         all-contexts   (keys roles-m)
         all-contexts   (if (and inc-g? gctx) (cons gctx all-contexts) all-contexts)
         header         (map translate-role all-roles)]
     (seq
      (cons header
            (for [context-id all-contexts]
              (let [croles (get all-roles-m context-id #{})]
                (cons context-id
                      (map
                       (if effective?
                         #(or (contains? croles %) (and (contains? groles %) :!))
                         (partial contains? croles))
                       all-roles)))))))))

(defn- calc-roles
  [ctx-labeler roles-labeler missing-label [ctx & roles]]
  (into [(or (some-str (ctx-labeler ctx)) (str ctx))]
        (mapv (comp (fnil identity missing-label) roles-labeler) roles)))

(defn roles-tabler
  ([req]
   (roles-tabler req nil))
  ([{{:keys [global-context] :or {global-context :!}} :roles/config :as req}
    {:keys [user-id effective? include-global? include-self?
            present-label missing-label global-label
            global-marker global-present-label context-label contexts-labeler]
     :or   {present-label    "YES"
            missing-label    "—"
            global-label     "global"
            context-label    "Context"
            contexts-labeler default-contexts-labeler
            include-global?  (not effective?)
            include-self?    false}
     :as   opts}]
   (let [global-marker        (or global-marker (str " (" global-label ")"))
         global-present-label (or global-present-label (str present-label global-marker))
         opts                 (qassoc opts :include-global? include-global? :include-self? include-self?)
         [l & d]              (roles-matrix req opts)
         gctx-line            (first d)
         have-gctx?           (and include-global? (= global-context (first gctx-line)))
         labels               (vec (interleave (range) (cons context-label (map str l))))
         roles-labeler        {true present-label, false missing-label, :! global-present-label}
         gctx-labeler         (if have-gctx? (qassoc roles-labeler :! present-label))
         ctx-labeler          (contexts-labeler req (map first d))
         data                 (->> (if have-gctx? (next d) d)
                                   (map (partial calc-roles
                                                 ctx-labeler
                                                 roles-labeler
                                                 missing-label))
                                   (sort-by (comp str/lower-case first)))
         data                 (if have-gctx?
                                (cons (calc-roles identity
                                                  gctx-labeler
                                                  missing-label
                                                  (cons global-label (rest gctx-line)))
                                      data)
                                data)]
     {:data (seq data) :labels labels})))

;; Data structures

(def empty-lazy-map
  (map/lazy))

;; Filesystem operations

(defn some-resource
  "Returns the given path if there is a resource it points to. Otherwise it returns
  nil. Multiple arguments are joined using str."
  ([path]
   (if-some [path (str path)] (and (io/resource path) path)))
  ([path & more]
   (if-some [path (apply str path more)] (and (io/resource path) path))))

;; Linking helpers

(defn path
  "Creates a URL on a basis of route name or a path.

  If a page found does not have a language parameter it is accepted too.

  When the given path is already parameterized with language then re-parameterized
  path is generated and checked if it exists, unless the value of the parameter is
  the same as the existing one. In such case the path is returned after a quick
  existence check.

  Additional path parameters (`params`) can be given to be used when matching by
  name. Giving extra (unknown to route) parameters does not affect lookup. Giving
  `params` when matching by path causes them to be silently ignored.

  Additional query parameters (`query-params`) can be given. They will be used when
  generating path. If the path was given and it already contains query parameters,
  they will be replaced.

  If the path is given instead of a route identifier, it must exist (after being
  equipped with any extra parameters, if needed, using `params`).

  When a path is given but it does not require language parameter, a path will be
  generated by testing whether it exists for regular and for slash-stripped or
  slash-added variant (the original variant goes first)."
  ([]
   nil)
  (^String [req]
   (page req (current-page req)))
  (^String [req name-or-path]
   (page req name-or-path))
  (^String [req name-or-path lang]
   (localized-page nil name-or-path lang
                   nil nil false false
                   (get req ::r/router)
                   (lang-param req)))
  ([req name-or-path lang params]
   (localized-page nil name-or-path lang
                   params nil false false
                   (get req ::r/router)
                   (lang-param req)))
  ([req name-or-path lang params query-params]
   (localized-page nil name-or-path lang
                   params query-params false false
                   (get req ::r/router)
                   (lang-param req)))
  ([name-or-path lang params query-params router language-settings-or-param]
   (localized-page nil name-or-path lang
                   params query-params
                   false false router
                   language-settings-or-param)))

(defn localized-path
  "Creates a URL on a basis of route name or a path.

  Uses very optimistic matching algorithm. Tries to obtain language from user
  settings and client settings if the path does not contain language
  information. Uses the `:default` language picker.

  Non-matching route identifier is retried by extracting its path and calling the
  `localized-page` function for that path given as a string.

  If a page found does not have a language parameter it is accepted too.

  When the given path is already parameterized with language then re-parameterized
  path is generated and checked if it exists, unless the value of the parameter is
  the same as the existing one. In such case the path is returned after a quick
  existence check.

  Additional path parameters (`params`) can be given to be used when matching by
  name. Giving extra (unknown to route) parameters does not affect lookup. Giving
  `params` when matching by path causes them to be silently ignored.

  Additional query parameters (`query-params`) can be given. They will be used when
  generating path. If the path was given and it already contains query parameters,
  they will be replaced.

  If the path is given instead of a route identifier, it must exist (after being
  equipped with any extra parameters, if needed, using `params`).

  When a path is given but it does not require language parameter, a path will be
  generated by testing whether it exists for regular and for slash-stripped or
  slash-added variant (the original variant goes first)."
  ([]
   nil)
  (^String[req]
   (localized-path req (current-page req)))
  (^String [req name-or-path]
   (localized-page nil name-or-path
                   (or (get req :language/id) (pick-language-str req :default))
                   nil nil false true
                   (get req ::r/router)
                   (lang-param req)))
  (^String [req name-or-path lang]
   (localized-page nil name-or-path lang
                   nil nil false true
                   (get req ::r/router)
                   (lang-param req)))
  (^String [req name-or-path lang params]
   (localized-page nil name-or-path lang
                   params nil false true
                   (get req ::r/router)
                   (lang-param req)))
  (^String [req name-or-path lang params query-params]
   (localized-page nil name-or-path lang
                   params query-params false true
                   (get req ::r/router)
                   (lang-param req)))
  (^String [name-or-path lang params query-params router language-settings-or-param]
   (localized-page nil name-or-path lang
                   params query-params
                   false true
                   router language-settings-or-param)))

;; Anti-spam

(defn random-uuid-or-empty
  ([]
   (random-uuid-or-empty nil))
  ([rng]
   (if (zero? (get-rand-int 2 rng))
     (random-uuid)
     "")))

;; Parameters

(defn string-from-param
  ^String [s]
  (if-some [^String s (some-str s)]
    (if (= \: (.charAt s 0))
      (let [^String s (subs s 1)] (if (not-empty-string? s) s))
      s)))

(defn keyword-from-param
  ^clojure.lang.Keyword [s]
  (if (keyword? s)
    s
    (if-some [^String s (some-str s)]
      (if (= \: (.charAt s 0))
        (let [^String s (subs s 1)] (if (not-empty-string? s) (keyword s)))
        (keyword s)))))

(defn try-kw-from-param
  [s]
  (if (keyword? s)
    s
    (if-some [^String s (some-str s)]
      (if (= \: (.charAt s 0))
        (let [^String s (subs s 1)] (if (not-empty-string? s) (keyword s)))
        s))))

(defn parse-query-params
  "Parses query params string `qstr` using Ring's `ring.util.codec/form-decode`. The
  given request map `req` is used internally to get the current character
  encoding (with fallback to UTF-8)."
  [req qstr]
  (if req
    (if-some [qstr (some-str qstr)]
      (codec/form-decode qstr (or (req/character-encoding req)
                                  (get req :character-encoding)
                                  "UTF-8")))))

(defn url->uri+params
  "Takes a request map `req` and URI (`u`) and tries to decompose it into 2-element
  vector with first element being a URI and second a query params map. If there is a
  processing exception during the operation, string-converted `u` is returned
  unmodified and query params slot is set to be `nil`. The request map is used
  internally to get the current character encoding (with fallback to UTF-8)."
  [req u]
  (try (let [{:keys [uri query-string]} (parse-url u)]
         [(some-str uri) (parse-query-params req query-string)])
       (catch Exception _ [(some-str u) nil])))

(defn query-string-encode
  ([params]
   (if params (codec/form-encode params)))
  ([req params]
   (if params (codec/form-encode params (or (req/character-encoding req)
                                            (get req :character-encoding)
                                            "UTF-8")))))

(defn remove-params
  "Removes the given parameter or parameters from a request map locations:
  - from a map associated with `parameters-sub-key` key
    within a map associated with `:parameters` key in `m`,
  - from a map associated with `params-key` key in `m`,
  - from a map associated with `:params` key in `m` (if `combined?` is truthy).
  All parameters must be either keywords or strings, they cannot be mixed.
  Parameter type `params-key` and `parameters-sub-key` should be keywords.
  If `params-key` or `parameters-sub-key` is `nil` then the removal within a specific
  location(s) will be skipped."
  ([m params-key parameters-sub-key combined? param]
   (let [strings?     (string? param)
         param-str    (if strings? param (some-str param))
         param-kw     (if strings? (keyword param) param)
         m-parameters (if parameters-sub-key (get m :parameters))
         m-req-params (if params-key (get m params-key))
         m-params     (if combined? (get m :params))
         m            (if (nil? m-parameters) m
                          (if-some [p (get m-parameters parameters-sub-key)]
                            (qassoc m :parameters
                                    (qassoc m-parameters parameters-sub-key
                                            (dissoc p param-kw)))
                            m))
         m            (if (nil? m-req-params) m
                          (qassoc m params-key
                                  (dissoc m-req-params param-str)))
         m            (if (nil? m-params) m
                          (qassoc m :params
                                  (dissoc m-params param-kw param-str)))]
     m))
  ([m params-key parameters-sub-key combined? param & more]
   (let [params       (cons param more)
         strings?     (string? param)
         params-str   (if strings? params (map some-str params))
         params-kw    (if strings? (map keyword params) params)
         m-parameters (if parameters-sub-key (get m :parameters))
         m-req-params (if params-key (get m params-key))
         m-params     (if combined? (get m :params))
         m            (if (nil? m-parameters) m
                          (if-some [p (get m-parameters parameters-sub-key)]
                            (qassoc m :parameters
                                    (qassoc m-parameters parameters-sub-key
                                            (apply dissoc p params-kw)))
                            m))
         m            (if (nil? m-req-params) m
                          (qassoc m params-key
                                  (apply dissoc m-req-params params-str)))
         m            (if (nil? m-params) m
                          (qassoc m :params
                                  (apply dissoc m-params (concat params-kw params-str))))]
     m)))

(defn remove-path-params
  "Removes the given parameter or parameters from a request map locations:
  `[:parameters :path]`, `:path-params` and `:params`. All parameters must be
  either keywords or strings, they cannot be mixed."
  ([m param]
   (remove-params m :path-params :path true param))
  ([m param & more]
   (apply remove-params m :path-params :path true param more)))

(defn remove-query-params
  "Removes the given parameter or parameters from a request map locations:
  `[:parameters :query]`, `:query-params` and `:params`. All parameters must be
  either keywords or strings, they cannot be mixed."
  ([m param]
   (remove-params m :query-params :query true param))
  ([m param & more]
   (apply remove-params m :query-params :query true param more)))

(defn remove-form-params
  "Removes the given parameter or parameters from a request map locations:
  `[:parameters :form]`, `:form-params` and `:params`. All parameters must be either
  keywords or strings, they cannot be mixed."
  ([m param]
   (remove-params m :form-params :form true param))
  ([m param & more]
   (apply remove-params m :form-params :form true param more)))

(defn remove-body-params
  "Removes the given parameter or parameters from a request map locations:
  `[:parameters :body]`, `:body-params` and `:params`. All parameters must be either
  keywords or strings, they cannot be mixed."
  ([m param]
   (remove-params m :body-params :body true param))
  ([m param & more]
   (apply remove-params m :body-params :body true param more)))

(defn pick-params
  "Removes namespaces from keys in `params` or renames keys according to the given
  `key-map`.

  The `keys` argument should be a sequence of keys to be found in `params`. Only the
  entries identified by these keys will be preserved, with keys stripped from their
  namespaces. When `new-ns` is present then the keys in generated map will have their
  namespace set to this value (which should be a string or any object convertible to
  a string).

  The `key-map` argument should be a map of keys to be found in `params` mapped to
  their new names. Only the entries from `params` identified by the keys from
  `key-map` will be preserved and renamed to corresponding values.

  The `ns` argument should be a string or an object which can be converted to a
  string.  It will be used to select the entries from `params` by the given namespace
  and to remove this namespace when producing the result. When `new-ns` is present
  then the keys in generated map will have their namespace set to this value (which
  should be a string or any object convertible to a string).

  When only `params` argument is given, it will simply rename all keys from
  namespaced to simple keywords.

  When `params` argument is given, then `ns` is set to `nil`, and `new-ns` is set, it
  will strip all keys from their namespaces replacing it with the given namespace in
  a resulting map."
  {:arglists '([params]
               [params ns]
               [params keys]
               [params key-map]
               [params new-ns ns]
               [params new-ns keys])}
  ([params new-ns keys-or-ns]
   (if (coll? keys-or-ns)
     (let [new-ns (some-str new-ns)]
       (reduce (fn [^clojure.lang.Associative m k]
                 (if (contains? params k)
                   (qassoc m (keyword new-ns (name k)) (get params k))
                   m))
               {} keys-or-ns))
     (if (nil? keys-or-ns)
       (map/map-keys (comp (partial keyword (some-str new-ns)) name) params)
       (let [ns     (some-str keys-or-ns)
             new-ns (some-str new-ns)]
         (reduce (fn [^clojure.lang.Associative m ^clojure.lang.MapEntry e]
                   (let [k (.key ^clojure.lang.MapEntry e)]
                     (if (= ns (namespace k))
                       (qassoc m (keyword new-ns (name k)) (.val ^clojure.lang.MapEntry e))
                       m)))
                 {} params)))))
  ([params keys-or-ns]
   (if (coll? keys-or-ns)
     (if (map? keys-or-ns)
       (reduce (fn [^clojure.lang.Associative m ^clojure.lang.MapEntry e]
                 (let [k (.key ^clojure.lang.MapEntry e)]
                   (if (contains? params k)
                     (qassoc m (.val ^clojure.lang.MapEntry e) (get params k))
                     m)))
               {} keys-or-ns)
       (reduce (fn [^clojure.lang.Associative m k]
                 (if (contains? params k)
                   (qassoc m (keyword (name k)) (get params k))
                   m))
               {} keys-or-ns))
     (let [ns (some-str keys-or-ns)]
       (reduce (fn [^clojure.lang.Associative m ^clojure.lang.MapEntry e]
                 (let [k (.key ^clojure.lang.MapEntry e)]
                   (if (= ns (namespace k))
                     (qassoc m (keyword (name k)) (.val ^clojure.lang.MapEntry e))
                     m)))
               {} params))))
  ([params]
   (map/map-keys (comp keyword name) params)))

;; Language helpers

(defn lang-url
  (^String [router req path-or-name lang localized? path-params]
   (lang-url router req path-or-name lang localized? path-params nil nil))
  (^String [router req path-or-name lang localized?]
   (lang-url router req path-or-name lang localized? nil nil nil))
  (^String [router req path-or-name lang]
   (lang-url router req path-or-name lang true nil nil nil))
  (^String [router req path-or-name]
   (lang-url router req path-or-name nil true nil nil nil))
  (^String [router req]
   (lang-url router req nil nil true nil nil nil))
  (^String [req]
   (lang-url nil req nil nil true nil nil nil))
  (^String [req path-or-name lang localized? path-params query-params lang-param]
   (lang-url nil req path-or-name lang localized? path-params query-params lang-param))
  (^String [router req path-or-name lang localized? path-params query-params lang-param]
   (let [router       (or router (get req ::r/router) (get req :router))
         lang         (or lang (get req :language/str) (some-str (get req :language/id)) (some-str (get req :lang)))
         lang-param   (or lang-param (get req :language/settings) :lang)
         path-or-name (or (valuable path-or-name) (current-page req))
         path-or-name (if path-or-name (try-kw-from-param path-or-name))
         path-fn      (if localized? localized-path path)
         out-path     (path-fn path-or-name lang path-params query-params router lang-param)]
     (or out-path (if-not (ident? path-or-name) (some-str path-or-name))))))

(defn lang-param
  [req]
  (or (get (get req :language/settings) :param) :lang))

(defn lang-id
  ^clojure.lang.Keyword [req]
  (or (get req :language/id)
      (get req :language/default)))

(defn lang-id-or-nil
  ^clojure.lang.Keyword [req]
  (get req :language/id))

(defn lang-str
  ^String [req]
  (or (get req :language/str)
      (str (get req :language/default))))

(defn lang-str-or-nil
  ^String [req]
  (get req :language/str))

(defn lang-query-string
  [req]
  (query-string-encode req {"lang" (lang-str req)}))

(defn lang-config
  [req]
  (get req :language/settings))

(defn add-missing-lang
  "For the given `body` map it adds a language under the `:lang` key if it does not
  exists yet. The language is obtained from the request map `req` by reading a value
  associated with the `:language/id` key."
  [body req translation-keys]
  (if (contains? body :lang)
    body
    (if (some #(contains? body %) translation-keys)
      (if-some [l (lang-id req)]
        (qassoc body :lang l)
        body)
      body)))

;; I18n

(defn translator
  "Generates a translation function using populated values from `:i18n/translator` or
  `:i18n/translator-nd` (variant which returns `nil` for missing keys, used when
  `i18n/*handle-missing-keys*` is set to a falsy value).

  Falls back to `i18n/translator` if predefined functions are not populated in the
  request map or if a language was specified as an optional `lang` argument and it
  differs from the language stored in `req` under `:language/id` (or
  `:language/default`)."
  ([req]
   (or (get req (if i18n/*handle-missing-keys* :i18n/translator :i18n/translator-nd))
       (i18n/translator req)))
  ([req lang-id]
   (if (and lang-id (not= lang-id (i18n/lang req)))
     (i18n/translator req lang-id)
     (or (get req (if i18n/*handle-missing-keys* :i18n/translator :i18n/translator-nd))
         (i18n/translator req)))))

(defn translator-sub
  ([req]
   (or (get req (if i18n/*handle-missing-keys* :i18n/translator-sub :i18n/translator-sub-nd))
       (i18n/translator-sub req)))
  ([req lang-id]
   (if (and lang-id (not= lang-id (i18n/lang req)))
     (i18n/translator-sub req lang-id)
     (or (get req (if i18n/*handle-missing-keys* :i18n/translator-sub :i18n/translator-sub-nd))
         (i18n/translator-sub req)))))

(defn try-namespace
  [v]
  (if (ident? v) (namespace v) v))

(defn try-name
  [v]
  (if (ident? v) (name v) v))

(defn add-missing-translation
  "For the given `body` map, a new key `new-k`, a key `k` and a translation function
  `sub-translation-fn`, tries to add a translation of the key `k` as a value under
  the given key `new-k`, if it does not exist yet in `body`. Optional `suffix`
  argument is used to add suffix to a key name."
  ([body new-k k sub-translation-fn]
   (if (contains? body new-k)
     body
     (if-some [t (and k (sub-translation-fn k))]
       (qassoc body new-k t)
       body)))
  ([body new-k k suffix sub-translation-fn]
   (if (contains? body new-k)
     body
     (if-some [t (and k (sub-translation-fn (try-namespace k) (str (try-name k) suffix)))]
       (qassoc body new-k t)
       body))))

(defn untranslatable?
  "Returns true if the given argument cannot be used as a translation key."
  ^Boolean [v]
  (not (or (ident? v) (string? v))))

;; Headers

(defn mobile-agent?
  ^Boolean [req]
  (if-some [ua (get (get req :headers) "user-agent")]
    (some? (re-find #"\b(iPhone|iPad|iPod|Android|Windows Phone|webOS|IEMobile|BlackBerry)\b" ua))))

(defmacro add-header
  "Adds a header `header` to `:response/headers` map of the `req` using built-in
  function `qassoc`. If a header name argument is a literal identifier (keyword or
  symbol), a character, a number, or a literal string, it will be converted to a
  string literal and placed as `qassoc` argument. Otherwise it will be left as is and
  wrapped into a call to `io.randomseed.utils/some-str` to ensure the result is a
  string run-time. All arguments of the body are used to calculate a value of the
  header. Assumes that `req` is always a map."
  [req header-name & body]
  (let [header-name (if (or (ident?  header-name)
                            (string? header-name)
                            (char?   header-name)
                            (number? header-name))
                      (some-str header-name)
                      (cons `some-str (cons header-name nil)))]
    `(let [req# ~req
           hdr# (get req# :response/headers)]
       (qassoc req# :response/headers
               (if (pos? (count hdr#))
                 (qassoc hdr# ~header-name (do ~@body))
                 {~header-name (do ~@body)})))))

(defmacro add-headers
  "Adds headers with associated values to `:response/headers` map of the `req` using
  built-in function `qassoc`. If any header name argument is a literal identifier
  (a keyword or a symbol), a character, a number, or a literal string, it will be
  converted to a string literal and placed as an argument passed to `qassoc`.
  Otherwise it will be left as is and wrapped in a call to `io.randomseed.utils/some-str`
  to ensure at run-time that the result will be a string. Missing header value, if any,
  will be padded with `nil`."
  ([req header-name header-value]
   (let [header-name (if (or (ident?  header-name)
                             (string? header-name)
                             (char?   header-name)
                             (number? header-name))
                       (some-str header-name)
                       (cons `some-str (cons header-name nil)))]
     `(let [req# ~req
            hdr# (get req# :response/headers)]
        (qassoc req# :response/headers
                (if (pos? (count hdr#))
                  (qassoc hdr# ~header-name ~header-value)
                  {~header-name ~header-value})))))
  ([req header-name header-value & more]
   (let [pairs  (cons header-name (cons header-value more))
         names  (take-nth 2 pairs)
         values (concat (take-nth 2 (rest pairs)) '(nil))
         pairs  (map #(cons (if (or (ident?  %1)
                                    (string? %1)
                                    (char?   %1)
                                    (number? %1))
                              (some-str %1)
                              (cons `some-str (cons %1 nil)))
                            (cons %2 nil))
                     names values)
         pairs  (apply concat pairs)
         names  (take-nth 2 pairs)
         dups?  (not= (count names) (count (distinct names)))]
     (if dups?
       `(let [req# ~req]
          (qassoc req# :response/headers (qassoc (get req# :response/headers) ~@pairs)))
       `(let [req# ~req
              hdr# (get req# :response/headers)]
          (qassoc req# :response/headers
                  (if (pos? (count hdr#))
                    (qassoc hdr# ~@pairs)
                    {~@pairs ~@[]})))))))

;; Identities

(pid/add-acceptable-type! ::identity/standard :email :phone :uid)

(defn id-type->url-type
  "Returns URL type identifier which matches the given identity type and confirmation
  reason. Used to generate confirmation link which is to be sent in verification message.

  If `id-type` is `:user/email` or `:email` and `reason` is `:change` or \"change\",
  the result is `:url/update-email.`

  If `id-type` is `:user/email` or `:email` and `reason` is `:recovery` or \"recovery\",
  the result is `:url/update-password.`

  If `id-type` is `:user/email` or `:email` and `reason` is not of any above,
  the result is `:url/create.`

  If `id-type` is `:user/phone` or `:phone` and `reason` is `:recovery` or \"recovery\",
  the result is `:url/update-password.`

  If `id-type` is `:user/phone` or `:phone`, and `reason` is not of any above,
  the result is `:url/update-phone.`

  In all other cases `:url/update-email` is returned."
  [id-type reason]
  (case id-type
    (:email :user/email) (case reason
                           ("recovery" :recovery) :url/update-password
                           ("change"   :change)   :url/update-email
                           :url/create)
    (:phone :user/phone) (case reason
                           ("recovery" :recovery) :url/update-password
                           :url/update-phone)
    :url/update-email))

(defn guess-identity-type
  "Detects the identity type and checks if it is assigned to a tag
  `:amelinium.identity/standard` or to a tag passed as `acceptable-tag` argument. To
  accept any valid identity type the `:amelinium.identity/valid` must be explicitly
  given.

  The `src` may be a map or a single value. If it is a map, the keys `:identity`,
  `:user/identity` and `:id` are looked up to get the identity of a user, keys
  `:id-type` and `:identity/type` are used to look up to get already established
  identity type. If it is a single value then it should contain the identity
  source (a string, a phone number, a number, etc).

  When the optional `id` argument is given, it is tried before any lookup in a source
  map and even before getting it from `src` directly (in case it is a single value).

  If the identity type was obtained (from the optional `identity-type` argument or a
  map entry, if any), it is checked whether it is acceptable. If the identity type
  was not obtained, it is guessed by analyzing the identity value (and also checked
  against the acceptable types).

  If the identity type cannot be established or it is not acceptable, `nil` is
  returned."
  ([src]
   (guess-identity-type src nil nil nil))
  ([src identity-type]
   (guess-identity-type src nil identity-type nil))
  ([src id identity-type]
   (guess-identity-type src id identity-type nil))
  ([src id identity-type acceptable-tag]
   (cond
     (and (nil? src)
          (nil? id)) nil
     (map? src)      (identity/acceptable-type
                      (or id (identity/value (or identity-type ::identity/any) src))
                      (or (if identity-type (some-keyword identity-type))
                          (get src :id-type)
                          (get src :identity/type)
                          (get src :identity-type))
                      (or (if acceptable-tag (some-keyword acceptable-tag))
                          ::identity/standard))
     :else           (identity/acceptable-type
                      (if id id src)
                      identity-type
                      (or (if acceptable-tag (some-keyword acceptable-tag))
                          ::identity/standard)))))

(defn acceptable-identity-type
  ([identity-type]
   (acceptable-identity-type identity-type nil))
  ([identity-type acceptable-tag]
   (if-some [id-type (some-keyword identity-type)]
     (if (identity/type? id-type
                         (or (if acceptable-tag (some-keyword acceptable-tag))
                             ::identity/standard))
       id-type))))

(defn identity-and-type
  "Detects an identity type and checks if it is assigned to a tag
  `:amelinium.identity/standard` or to a tag passed as `acceptable-tag` argument.

  To accept any valid identity type the `:amelinium.identity/valid` must be
  explicitly given as `acceptable-tag` argument.

  If `identity-type` is not given, is set to `nil` or to `:amelinium.identity/any`,
  it is guessed by analyzing `user-identity`.

  Returns a 2-element vector containing original value of `user-identity` argument
  and detected identity type.

  If `user-identity` is `nil` or `false` then only the given `identity-type` is
  checked whether it is acceptable. If it is not, `nil` is returned instead of
  vector.

  If `user-identity` is given but it is not matching the given identity type or its
  identity type is not acceptable, `nil` is returned instead of vector.

  If `user-identity` is given but `identity-type` is `nil` or
  `:amelinium.identity/any`, the type of user identity is extracted and checked
  whether it is acceptable.

  User identity is only tested, never transformed, even if it is not an `Identity`
  object. Identity type is extracted from identity object (given or ad-hoc created)."
  ([user-identity]
   (if-some [id-type (identity/acceptable-type user-identity nil ::identity/standard)]
     [user-identity id-type]))
  ([user-identity identity-type]
   (identity-and-type user-identity identity-type ::identity/standard))
  ([user-identity identity-type acceptable-tag]
   (if user-identity
     (if-some [id-type (identity/acceptable-type
                        user-identity
                        identity-type
                        (or (some-keyword acceptable-tag) ::identity/standard))]
       [user-identity id-type])
     (if identity-type
       (if-some [id-type (acceptable-identity-type identity-type acceptable-tag)]
         [nil id-type])))))

;; Date and time

(defn rfc1123-date-time
  "Returns a date and time formatted according to the RFC 1123."
  [t]
  (when t
    (some-str
     (t/format DateTimeFormatter/RFC_1123_DATE_TIME (t/zoned-date-time t)))))

(defn duration-nanos
  "Calculates the duration in nanoseconds between the time of calling the function (or
  the given time `begin`) till the given time `end`."
  ([end]       (when end (.withNanos ^Duration (t/between (t/now) end) 0)))
  ([begin end] (when end (.withNanos ^Duration (t/between (or begin (t/now)) end) 0))))

(defn retry-in-mins
  "Calculates minutes of duration on a basis of `Duration` object. Returns an integer
  number (or `nil` when there was no duration given) which is never less than 1 and
  always incremented by one. Used to calculate retry times to report them to a user."
  [duration]
  (when duration
    (let [mins (inc (t/minutes duration))]
      (if (pos-int? mins) mins 1))))

;; Response status

(defmacro add-status
  "Adds response status to a request map `req` under its key `:response/status` using
  `qassoc`. The status is a result of evaluating expressions passed as additional
  arguments. Returns updated `req`. Assumes that `req` is always a map."
  [req & body]
  (if (and (seq? body) (> (count body) 1))
    `(qassoc ~req :response/status (do ~@body))
    `(qassoc ~req :response/status ~@body)))

(defmacro remove-status
  "Removes `:response/status` from `req` using `clojure.core/dissoc`."
  [req]
  `(dissoc ~req :response/status))
