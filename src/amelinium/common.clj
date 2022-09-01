(ns

    ^{:doc    "Common helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.common

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                          :as          set]
            [clojure.string                       :as          str]
            [clojure.core.memoize                 :as          mem]
            [clojure.java.io                      :as           io]
            [potemkin.namespaces                  :as            p]
            [tick.core                            :as            t]
            [reitit.core                          :as            r]
            [reitit.coercion                      :as     coercion]
            [ring.util.response]
            [ring.util.codec                      :as        codec]
            [ring.util.http-response              :as         resp]
            [ring.util.request                    :as          req]
            [amelinium.http                       :as         http]
            [amelinium.http.middleware.roles      :as        roles]
            [amelinium.http.middleware.language   :as     language]
            [amelinium.http.middleware.session    :as      session]
            [amelinium.http.middleware.db         :as       mid-db]
            [amelinium.http.middleware.validators :as   validators]
            [amelinium.common.oplog.auth          :as   oplog-auth]
            [amelinium.i18n                       :as         i18n]
            [amelinium.model.user                 :as         user]
            [amelinium.logging                    :as          log]
            [amelinium.db                         :as           db]
            [io.randomseed.utils.time             :as         time]
            [io.randomseed.utils.vec              :as          vec]
            [io.randomseed.utils.map              :as          map]
            [io.randomseed.utils                  :refer      :all])

  (:import [amelinium.auth AuthConfig AuthSettings AccountTypes]
           [reitit.core Match]
           [lazy_map.core LazyMapEntry LazyMap]))

;; Data sources

(defn auth-config
  "Gets authentication configuration for the given account type or a global
  authentication settings if the account type was not given. If `auth-type` is
  explicitly set to `nil`, it will be changed into `:default`."
  ([req-or-match auth-type]
   (if-some [auth-settings (http/get-route-data req-or-match :auth/config)]
     (get (.types ^AuthSettings auth-settings)
          (cond (keyword? auth-type) auth-type
                (some? auth-type)    (keyword auth-type)
                :other               :default))))
  ([req-or-match]
   (http/get-route-data req-or-match :auth/config)))

(defn auth-db
  "Returns an authentication database connection object for the given authentication
  type or, if the type is not given, for a common authentication database (top-level,
  not assigned to any particular authentication type)."
  ([req-or-match]
   (if-some [auth-settings (auth-config req-or-match)]
     (.db ^AuthSettings auth-settings))))

;; Operations logging

(defn oplog-config
  "Returns operations logger configuration obtained from a request or a `Match`
  object."
  [req-or-match]
  (or (http/get-route-data req-or-match :oplog/config)
      oplog-auth/log))

(defn oplog-logger
  "Retrieves operations logger function from a current route data (via `:oplog/config`
  key and then the `:fn/reporter` key), and if that fails, tries to retrieve it using
  `:oplog/config` key of the request map (and `:fn/reporter sub-key). When everything
  fails it will fall back to a global variable `amelinium.common.oplog.auth/log`. The
  given argument can be either a request map or a `Match` object. In its binary
  variant the second argument is tested first and it should be an operations logger
  configuration map containing the `:fn/reporter` key."
  ([req-or-match]
   (if-some [lgr (or (get (http/get-route-data req-or-match :oplog/config) :fn/reporter)
                     oplog-auth/log)]
     (fn [& {:as message}] (lgr message))
     (constantly nil))))

(defn oplog-logger-populated
  "Creates operations logging function on a basis of operations logger retrieved by
  getting `:oplog/logger` key of the request (`req`) or by calling `oplog-logger`
  function."
  ([req]
   (or (get req :oplog/logger)
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
       (and (some? rn) (= page-id-or-path rn)))
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
  "Helper which returns 2-element sequence telling if the current (or given as a match)
  page is a login page (1st element) and/or an auth page (2nd element)."
  ([req]
   (let [rd     (http/get-route-data req)
         login? (boolean (get rd :login-page?))
         auth?  (boolean (get rd :auth-page?))]
     (cons login? (cons auth? nil))))
  ([req ring-match]
   (let [rd     (http/get-route-data req)
         login? (boolean (get rd :login-page?))
         auth?  (boolean (get rd :auth-page?))]
     (cons login? (cons auth? nil))))
  ([req login-page-data auth-page-data]
   (let [rd     (http/get-route-data req)
         login? (boolean (get rd (or login-page-data :login-page?)))
         auth?  (boolean (get rd (or auth-page-data  :auth-page?)))]
     (cons login? (cons auth? nil))))
  ([req ring-match login-page-data auth-page-data]
   (let [rd     (http/get-route-data req)
         login? (boolean (get rd (or login-page-data :login-page?)))
         auth?  (boolean (get rd (or auth-page-data  :auth-page?)))]
     (cons login? (cons auth? nil)))))

;; Path parsing

(def ^:const max-url-len      8192)
(def ^:const fast-url-matcher (re-pattern "^[a-zA-Z0-9\\+\\.\\-]+\\:"))
(def ^:const path-splitter    (re-pattern "([^\\?\\#]+)(\\#[^\\?]+)?(\\?.*)?"))
(def ^:const split-qparams    (re-pattern "[^\\?\\#]+|[\\?\\#].*"))
(def ^:const on-slash         (re-pattern "/"))
(def ^:const slash-break      (re-pattern "[^/]+|/"))

(defn is-url?
  [s]
  (and s (string? s) (pos? (count ^String s))
       (not= \/ (.charAt ^String s 0))
       (some? (re-find fast-url-matcher ^String s))))

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
  (mem/fifo path-variants-core :fifo/threshold 2048))

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
     (let [path                    (some-> match-or-path (r/match->path query-params))
           [path location qparams] (split-query-params path)]
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
      (if-some [m (r/match-by-name rtr id (assoc params param pvalue))]
        (if require-param?
          (or (req-param-path rtr m param pvalue query-params)
              (if name-path-fallback?
                (if-some [path (some-str (r/match->path m))]
                  (parameterized-page-core param rtr path
                                           pvalue params query-params
                                           require-param? false))))
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
              ;; route is not parameterized with our parameter
              ;; we have to go brute-force by trying different path variants
              (some-> (some
                       (if require-param?
                         #(req-param-path rtr % param pvalue query-params)
                         #(some-> (r/match-by-path rtr %) (r/match->path query-params)))
                       (path-variants id pvalue))
                      (str location qparams)))))))))

(def ^{:private  true
       :arglists '([param rtr id param-value params query-params require-param? name-path-fallback?])}
  parameterized-page-mem
  (mem/lu parameterized-page-core :lu/threshold 4096))

(defn parameterized-page
  "Generates a path for the given page identifier (which may be a name expressed with
  an identifier, preferably a keyword, or a path expressed as a string) and a
  parameter with the given value. Optional parameters may be given as the argument
  called params; they will be used to match a page by name if it requires additional
  parameters to be present.

  Examples:

  (parameterized-page req)
  (parameterized-page req :login-page)
  (parameterized-page req :login-page :lang :pl)
  (parameterized-page req :login-page :lang :pl {:client \"wow-corp\"})
  (parameterized-page req \"/login/page/\")
  (parameterized-page req \"/login/page/\" :lang :pl)
  (parameterized-page req \"/en/login/page/\" :lang :pl)

  When called with just a request map, returns a path of the current page if the page
  exists. When called with a page name or path, it returns a path if the page
  exists.

  The optional require-param? argument (the last one in a quaternary variant,
  set to true when not given) enables extra check eliminating pages which do not
  support the given parameter, yet were matched by their names. Example:
  (parameterized-page req :login-page :lang :pl) will fail if there is no parameter
  :lang handled by the route named :login-page and require-param? was set to
  true.

  When the given path is already parameterized then re-parameterized path is
  generated and checked if it exists, unless the value of the parameter is the same
  as the existing one. In such case the path is returned after a quick existence
  check.

  If the path is given, it must exist, unless a parameter name and value are passed
  as argument values too. In such case the page identified by the path does not have
  to exist but the resulting page has to."
  ([] "/")
  ([req]
   (r/match->path (get req ::r/match) (get req :query-params)))
  ([req id-or-path]
   (if-some [rtr (get req ::r/router)]
     (if (ident? id-or-path)
       (some-> (r/match-by-name rtr id-or-path) r/match->path)
       (if-some [path (some-str id-or-path)]
         (let [[path location qparams] (split-query-params path)]
           (some-> (r/match-by-path rtr path) r/match->path (str location qparams)))))))
  ([req id-or-path param param-value]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem param rtr id-or-path param-value nil nil true false)))
  ([req id-or-path param param-value params-or-require-param?]
   (if-some [rtr (get req ::r/router)]
     (if (boolean? params-or-require-param?)
       (parameterized-page-mem param rtr id-or-path param-value nil nil params-or-require-param? false)
       (parameterized-page-mem param rtr id-or-path param-value params-or-require-param? nil true false))))
  ([req id-or-path param param-value params query-params-or-require-param?]
   (if-some [rtr (get req ::r/router)]
     (if (boolean? query-params-or-require-param?)
       (parameterized-page-mem param rtr id-or-path param-value params nil query-params-or-require-param? false)
       (parameterized-page-mem param rtr id-or-path param-value params query-params-or-require-param? true false))))
  ([req id-or-path param param-value params query-params require-param?]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem param rtr id-or-path param-value params query-params require-param? false)))
  ([req id-or-path param param-value params query-params require-param? name-path-fallback?]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem param rtr id-or-path param-value params query-params require-param? name-path-fallback?)))
  ([_ id-or-path param param-value params query-params require-param? name-path-fallback? router]
   (if (some? router)
     (parameterized-page-mem param router id-or-path param-value params query-params require-param? name-path-fallback?))))

(defn localized-page
  "Generates a page path for the given page identifier and language identifier. When
  called with just a request map, returns a path of the current page but re-generated
  to support current language in use (taken from `:language/str` key of the request
  map).

  The optional lang-required? argument (the last one in a quaternary variant, set to
  `true` when not given) enables extra check which eliminates the resulting pages not
  supporting the language parameter (yet were matched by their names).

  When the given path is already localized then re-localized path is generated and
  checked if it exists, unless its language is the same as the existing one. In such
  case the path is returned after a quick existence check.

  If the path is given it does not have to exist but the resulting page (identified
  by the localized path) has to.

  When having a path given, the failed matching causes it to fall back into
  brute-force mode where the given language parameter is injected into every possible
  segment of a path to check if it exists."
  {:arglists '([req]
               [req name-or-path]
               [req name-or-path path-params]
               [req name-or-path path-params query-params]
               [req name-or-path lang]
               [req name-or-path lang path-params]
               [req name-or-path lang path-params query-params]
               [req name-or-path lang lang-required?]
               [req name-or-path lang path-params lang-required?]
               [req name-or-path lang path-params query-params lang-required?]
               [req name-or-path lang path-params query-params lang-required? name-path-fallback?]
               [req name-or-path lang path-params query-params lang-required? name-path-fallback? router]
               [_   name-or-path lang path-params query-params lang-required? name-path-fallback? router language-settings-or-param])}
  ([] "/")
  ([req]
   (let [m (get req ::r/match)]
     (localized-page req
                     (r/match->path m)
                     (get req :language/str)
                     (or (get req :path-params) (get m :path-params))
                     (get req :query-params)
                     true false)))
  ([req name-or-path]
   (localized-page req name-or-path
                   (get req :language/str)
                   nil nil true false))
  ([req name-or-path lang]
   (localized-page req name-or-path
                   (or lang (get req :language/str))
                   nil nil true false))
  ([req name-or-path lang params-or-lang-required?]
   (if-some [rtr (get req ::r/router)]
     (if (boolean? params-or-lang-required?)
       (parameterized-page-mem (lang-param req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               nil nil
                               params-or-lang-required?
                               false)
       (parameterized-page-mem (lang-param req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               params-or-lang-required?
                               nil true false))))
  ([req name-or-path lang params query-params-or-lang-required?]
   (if-some [rtr (get req ::r/router)]
     (if (boolean? query-params-or-lang-required?)
       (parameterized-page-mem (lang-param req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               params nil
                               query-params-or-lang-required?
                               false)
       (parameterized-page-mem (lang-param req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               params query-params-or-lang-required?
                               true false))))
  ([req name-or-path lang params query-params lang-required?]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             lang-required? false)))
  ([req name-or-path lang params query-params lang-required? name-path-fallback?]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             lang-required?
                             name-path-fallback?)))
  ([req name-or-path lang params query-params lang-required? name-path-fallback? router]
   (if (some? router)
     (parameterized-page-mem (lang-param req)
                             router name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             lang-required?
                             name-path-fallback?)))
  ([_ name-or-path lang params query-params lang-required? name-path-fallback? router language-settings-or-param]
   (if (some? router)
     (parameterized-page-mem (guess-lang-param language-settings-or-param)
                             router name-or-path lang
                             params query-params
                             lang-required?
                             name-path-fallback?))))

(defn localized-or-regular-page
  "Same as localized-page with lang-required? always set to false and with less arities
  supported. When the language version of a page identified by its name is not
  present it will fallback to a regular version, without using language
  parameter. The regular page must exist too. If the path is given, it does not have
  to exist but the resulting page (identified by a localized path) has to."
  ([req]
   (let [m (get req ::r/match)]
     (localized-page req
                     (r/match->path m)
                     (get req :language/str)
                     (or (get req :path-params) (get m :path-params))
                     (get req :query-params)
                     false false)))
  ([req name-or-path]
   (localized-page req
                   name-or-path
                   (get req :language/str)
                   nil nil false false))
  ([req name-or-path lang]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             nil nil false false)))
  ([req name-or-path lang params]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params nil false false)))
  ([req name-or-path lang params query-params]
   (if-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             false false))))

(defn- page-core
  ([rtr id]
   (page-core rtr id nil nil nil))
  ([rtr id params]
   (page-core rtr id params nil nil))
  ([rtr id params query-params]
   (page-core rtr id params query-params nil))
  ([rtr id params query-params fb-lang-settings]
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
  "Generates page path for the given page identifier (a name) or a path and optional
  language identifier. When called with just a request map, returns a path of the
  current page.

  It tries to be optimistic. When called for a page identified by its name (expressed
  as an identifier, usually a keyword) and requiring a language parameter to be
  found (so it cannot be looked up using just a name alone) then it will use
  currently detected language obtained from the given request
  map (key :language/str), and use it.

  When invoked with a language parameter, it calls localized-page to handle it.  The
  lang-required? parameter is used when localized-page is called to check if the
  route which was matched is parameterized with the language parameter. This is to
  ensure that a localized route is used.

  If the path is given, it must exist, unless the language argument is given. In such
  case the path does not have to exist but the resulting page (identified by the
  localized path) has to.

  Additional path parameters (path-params) can be given to be used when matching by
  name. Giving extra (unknown to route) parameters does not affect lookup. Giving
  path-params when matching by path causes them to be silently ignored.

  Additional query parameters (query-params) can be given. They will be used when
  generating path. If the path was given and it already contains query parameters,
  they will be replaced.

  When having a language and a path given, the failed matching causes internally
  called localized-page to fall back into brute-force mode where the given language
  parameter is injected into every possible segment of a path to check if it exists."
  {:arglists '([req]
               [req name-or-path]
               [req name-or-path path-params]
               [req name-or-path path-params query-params]
               [req name-or-path lang]
               [req name-or-path lang path-params]
               [req name-or-path lang path-params query-params]
               [req name-or-path lang lang-required?]
               [req name-or-path lang path-params lang-required?]
               [req name-or-path lang path-params query-params lang-required?]
               [req name-or-path lang path-params query-params lang-required? name-path-fallback?]
               [_ _ name-or-path path-params query-params router hint-lang lang-settings-or-param]
               [_   name-or-path lang path-params query-params lang-required? name-path-fallback? router lang-settings-or-param])}
  ([req]
   (r/match->path (get req ::r/match) (get req :query-params)))
  ([req name-or-path]
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
  ([req name-or-path lang-or-params]
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
     (localized-page req name-or-path lang-or-params nil nil true false)))
  ([req name-or-path lang-or-params params-or-query-params-or-required?]
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
                       false)
       (localized-page req
                       name-or-path
                       lang-or-params
                       params-or-query-params-or-required?
                       nil
                       true
                       false))))
  ([req name-or-path lang-or-params params-or-query-params query-params-or-require-param?]
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
                       false)
       (localized-page req
                       name-or-path
                       lang-or-params
                       nil
                       query-params-or-require-param?
                       true
                       false))))
  ([req name-or-path lang params query-params require-param?]
   ;; language specified
   (localized-page req
                   name-or-path
                   lang
                   params
                   query-params
                   require-param?
                   false))
  ([req name-or-path lang params query-params require-param? name-path-fallback?]
   ;; language specified
   (localized-page req
                   name-or-path
                   lang
                   params
                   query-params
                   require-param?
                   name-path-fallback?))
  ([_ _ name-or-path params query-params router hint-lang lang-settings-or-param]
   (if (ident? name-or-path)
     ;; route name
     (page-core router name-or-path params query-params
                [(guess-lang-param lang-settings-or-param) hint-lang])
     ;; path
     (page-core router name-or-path params query-params nil)))
  ([_ name-or-path lang params query-params require-param? name-path-fallback? router lang-settings-or-param]
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
  [req]
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
  ([req]         (page req :login))
  ([req lang-id] (page req :login lang-id)))

(defn auth-page
  "Returns a path for the authentication page. The page must have ID of `:welcome`."
  ([req]         (page req :welcome))
  ([req lang-id] (page req :welcome lang-id)))

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

;; Rendering

(defn render
  "Universal, response renderer. Returns the result of calling the `resp-fn` with
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
         (update (resp-fn) :headers conj headers)
         (resp-fn)))))
  ([resp-fn req a]
   (if (nil? req)
     (resp-fn a)
     (if (resp/response? req)
       req
       (if-some [headers (get req :response/headers)]
         (update (resp-fn a) :headers conj headers)
         (resp-fn a)))))
  ([resp-fn req a b]
   (if (nil? req)
     (resp-fn a b)
     (if (resp/response? req)
       req
       (if-some [headers (get req :response/headers)]
         (update (resp-fn a b) :headers conj headers)
         (resp-fn a b)))))
  ([resp-fn req a b & more]
   (if (nil? req)
     (apply resp-fn a b more)
     (if (resp/response? req)
       req
       (if-some [headers (get req :response/headers)]
         (update (resp-fn a b more) :headers conj headers)
         (apply resp-fn a b more))))))

(defn render-force
  "Universal, body-less response renderer. Returns the result of calling the `resp-fn`
  with headers attached (from `:response/headers` key of the `req`). Arguments from
  the third are passed to `resp-fn` function."
  ([resp-fn]
   (resp-fn))
  ([resp-fn req]
   (if (nil? req)
     (resp-fn)
     (if-some [headers (get req :response/headers)]
       (update (resp-fn) :headers conj headers)
       (resp-fn))))
  ([resp-fn req a]
   (if (nil? req)
     (resp-fn a)
     (if-some [headers (get req :response/headers)]
       (update (resp-fn a) :headers conj headers)
       (resp-fn a))))
  ([resp-fn req a b]
   (if (nil? req)
     (resp-fn a b)
     (if-some [headers (get req :response/headers)]
       (update (resp-fn a b) :headers conj headers)
       (resp-fn a b))))
  ([resp-fn req a b & more]
   ([resp-fn req a b]
    (if (nil? req)
      (apply resp-fn a b more)
      (if-some [headers (get req :response/headers)]
        (update (apply resp-fn a b more) :headers conj headers)
        (apply resp-fn a b more))))))

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
                             (str "Uses the localized-page function to calculate the destination path on a basis of
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

(def ^{:arglists '([req]
                   [req pickers]
                   [req picker-id]
                   [req pickers picker-id])}
  pick-language
  "Tries to pick the best language for a known user or a visitor. To be used (among
  other scenarios) after a successful log-in to show the right language version of a
  welcome page. Does not use pre-calculated values from a request map, instead
  triggers configured pickers from a default or given chain. Returns a keyword."
  language/pick)

(def ^{:arglists '([req]
                   [req pickers]
                   [req picker-id]
                   [req pickers picker-id])}
  pick-language-str
  "Tries to pick the best language for a known user or a visitor. To be used (among
  other scenarios) after a successful log-in to show the right language version of a
  welcome page. Does not use pre-calculated values from a request map, instead
  triggers configured pickers from a default or given chain. Returns a string."
  (comp some-str language/pick))

(def ^{:arglists '([req]
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

(def ^{:arglists '([req]
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
  [uri]
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
        (get auth-config-or-lock-wait :locking/lock-wait)
        auth-config-or-lock-wait)
      lock-wait-default))

(defn hard-locked?
  "Returns true if the given user map contains the :locked key and a value associated
  with it is not nil. Does not connect to a database."
  [user]
  (some? (hard-lock-time user)))

(defn soft-locked?
  "Returns true if the given user account is soft-locked (the time amount which passed
  from the lock till the given time is lesser than the soft lock wait configuration
  option). Does not connect to a database."
  ([lock-passed auth-config-or-lw]
   (if lock-passed
     (if-some [lock-wait (lock-wait auth-config-or-lw)]
       (t/< lock-passed lock-wait))))
  ([user auth-config-or-lw time]
   (if auth-config-or-lw
     (if-some [lock-passed (soft-lock-passed user time)]
       (if-some [lock-wait (lock-wait auth-config-or-lw)]
         (t/< lock-passed lock-wait))))))

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

(p/import-vars [amelinium.http.middleware.session
                session-field])

(defn session
  "Gets a session map from the given request map."
  ([req]
   (get req (or (get (get req :session/config) :session-key) :session)))
  ([req config-key]
   (get req (or (get (get req config-key) :session-key) :session))))

(defn session-config
  "Gets a session config map from the given request map."
  ([req]
   (get req :session/config))
  ([req config-key]
   (get req config-key)))

(defn config+session
  "Gets a session map and a session config map from the given request map. Returns a
  two-element vector."
  ([req]
   (config+session req :session/config))
  ([req config-key]
   (if-some [cfg (get req config-key)]
     [cfg (get req (or (get cfg :session-key) :session))]
     [nil nil])))

(defn session-variable-get-failed?
  [v]
  (session/get-variable-failed? v))

(defn allow-expired
  "Temporarily marks expired session as valid."
  [smap]
  (if (and (get smap :expired?       )
           (not   (get smap :valid? ))
           (nil?  (get smap :id     ))
           (some? (get smap :err/id )))
    (assoc smap :valid? true :id (get smap :err/id))
    smap))

(defn allow-soft-expired
  "Temporarily mark soft-expired session as valid."
  [smap]
  (if (get smap :hard-expired?)
    smap
    (allow-expired smap)))

(defn allow-hard-expired
  "Temporarily mark hard-expired session as valid."
  [smap]
  (if (get smap :hard-expired?)
    (allow-expired smap)
    smap))

;; Context and roles

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
         reg-roles      (vals (update all-roles-m gctx #(apply disj % dyn-roles)))
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
         opts                 (assoc opts :include-global? include-global? :include-self? include-self?)
         [l & d]              (roles-matrix req opts)
         gctx-line            (first d)
         have-gctx?           (and include-global? (= global-context (first gctx-line)))
         labels               (vec (interleave (range) (cons context-label (map str l))))
         roles-labeler        {true present-label, false missing-label, :! global-present-label}
         gctx-labeler         (if have-gctx? (assoc roles-labeler :! present-label))
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
  "Creates a URL path on a basis of route name or a path."
  ([]
   nil)
  ([req]
   (page req (current-page req)))
  ([req name-or-path]
   (page req name-or-path))
  ([req name-or-path lang]
   (localized-page nil name-or-path lang
                   nil nil true false
                   (get req ::r/router)
                   (lang-param req)))
  ([req name-or-path lang params]
   (localized-page nil name-or-path lang
                   params nil true false
                   (get req ::r/router)
                   (lang-param req)))
  ([req name-or-path lang params query-params]
   (localized-page nil name-or-path lang
                   params query-params true false
                   (get req ::r/router)
                   (lang-param req)))
  ([name-or-path lang params query-params router language-settings-or-param]
   (localized-page nil name-or-path lang
                   params query-params
                   true false router
                   language-settings-or-param)))

(defn localized-path
  "Creates a URL on a basis of route name or a path. Uses very optimistic matching
  algorithm. Tries to obtain language from user settings and client settings if the
  path does not contain language information. Uses the `:default` language picker. "
  ([]
   nil)
  ([req]
   (localized-path req (current-page req)))
  ([req name-or-path]
   (localized-page nil name-or-path
                   (or (get req :language/id) (pick-language-str req :default))
                   nil nil true true
                   (get req ::r/router)
                   (lang-param req)))
  ([req name-or-path lang]
   (localized-page nil name-or-path lang
                   nil nil true true
                   (get req ::r/router)
                   (lang-param req)))
  ([req name-or-path lang params]
   (localized-page nil name-or-path lang
                   params nil true true
                   (get req ::r/router)
                   (lang-param req)))
  ([req name-or-path lang params query-params]
   (localized-page nil name-or-path lang
                   params query-params true true
                   (get req ::r/router)
                   (lang-param req)))
  ([name-or-path lang params query-params router language-settings-or-param]
   (localized-page nil name-or-path lang
                   params query-params
                   true true
                   router language-settings-or-param)))

;; Anti-spam

(defn random-uuid-or-empty
  ([]
   (random-uuid-or-empty nil))
  ([rng]
   (if (zero? (get-rand-int 2 rng))
     (random-uuid)
     "")))

;; Language helpers

(defn lang-url
  [req path-or-name lang localized? params query-params lang-settings]
  (let [router        (or (get req ::r/router) (get req :router))
        lang          (or lang (get req :language/str) (some-str (get req :language)) (some-str (get req :lang)))
        lang-settings (or (valuable lang-settings) (get req :language/settings) (get req :language-param) (get req :param) :lang)
        path-or-name  (or path-or-name (current-page req))
        path-or-name  (if (and path-or-name (str/starts-with? path-or-name ":")) (keyword (subs path-or-name 1)) path-or-name)
        path-fn       (if localized? localized-path path)
        out-path      (path-fn path-or-name lang params query-params router lang-settings)
        out-path      (or out-path (if-not (ident? path-or-name) (some-str path-or-name)))]
    out-path))

(defn lang-param
  [req]
  (or (get (get req :language/settings) :param) :lang))

(defn lang-id
  [req]
  (or (get req :language/id)
      (get req :language/default)))

(defn lang-id-or-nil
  [req]
  (get req :language/id))

(defn lang-str
  [req]
  (or (get req :language/str)
      (str (get req :language/default))))

(defn lang-str-or-nil
  [req]
  (get req :language/str))

(defn lang-config
  [req]
  (get req :language/settings))

;; I18n

(defn translator
  ([req]
   (or (get req :i18n/translator) (i18n/translator req)))
  ([req lang]
   (if lang
     (i18n/translator req lang)
     (or (get req :i18n/translator) (i18n/translator req)))))

(defn translator-sub
  ([req]
   (or (get req :i18n/translator-sub) (i18n/translator-sub req)))
  ([req lang]
   (if lang
     (i18n/translator-sub req lang)
     (or (get req :i18n/translator-sub) (i18n/translator-sub req)))))

;; Parameters

(defn string-from-param
  [s]
  (if-some [s (some-str s)]
    (if (= \: (.charAt ^String s 0)) (subs s 1) s)))

(defn keyword-from-param
  [s]
  (if (keyword? s)
    s
    (if-some [^String s (some-str s)]
      (keyword
       (if (= \: (.charAt ^String s 0)) (subs s 1) s)))))

(defn parse-query-params
  [req qstr]
  (if req
    (if-some [qstr (some-str qstr)]
      (codec/form-decode qstr (or (req/character-encoding req) "UTF-8")))))

(defn url->uri+params
  [req u]
  (try (let [{:keys [uri query-string]} (parse-url u)]
         [(some-str uri) (parse-query-params req query-string)])
       (catch Exception _ [(some-str u) nil])))

(defn query-string-encode
  ([params]
   (if params (codec/form-encode params)))
  ([params enc]
   (if params (codec/form-encode params enc))))

;; Headers

(defn mobile-agent?
  [req]
  (if-some [ua (get (get req :headers) "user-agent")]
    (some? (re-find #"\b(iPhone|iPad|iPod|Android|Windows Phone|webOS|IEMobile|BlackBerry)\b" ua))))
