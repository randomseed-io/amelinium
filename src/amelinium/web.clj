(ns

    ^{:doc    "Web helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web

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
            [amelinium.db                         :as           db]
            [io.randomseed.utils.time             :as         time]
            [io.randomseed.utils.vec              :as          vec]
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

(defn auth-config
  [req-or-match]
  (http/get-route-data req-or-match :auth/config))

(defn auth-db
  "Retrieves authentication database from a current route data (via `:auth/config` key
  and then the `:db` key), and if that fails, tries to retrieve it using
  `:amelinium.middleware.db/auth` key of the request map and later using :auth and
  `:db` key path of the request map. When everything fails it will fall back to a
  global variable `amelinium.db/auth`. The given argument can be either a request map
  or a `Match` object. In its binary variant the second argument is tested first and
  it should be an authentication configuration map containing the `:db` key."
  ([req-or-match]
   (or (get req-or-match :auth/db)
       (get (http/get-route-data req-or-match :auth/config) :db)
       (when-not (instance? Match req-or-match)
         (or (get req-or-match ::mid-db/auth)
             (get (get req-or-match :auth/config) :db)))))
  ([req-or-match auth-config-arg]
   (or (get auth-config-arg :db)
       (auth-db req-or-match))))

;; Operations logging

(defn oplog-config
  "Returns operations logger configuration obtained from a request or a `Match`
  object."
  [req-or-match]
  (or (http/get-route-data req-or-match :oplog/config)
      (when-not (instance? Match req-or-match)
        (get req-or-match :oplog/config))
      oplog-auth/log))

(defn oplog-logger
  "Retrieves operations logger function from a current route data (via `:oplog/config`
  key and then the `:fn/reporter` key), and if that fails, tries to retrieve it using
  `:oplog/config` key of the request map (and `:fn/reporter sub-key). When everything
  fails it will fall back to a global variable `amelinium.web.oplog.auth/log`. The
  given argument can be either a request map or a `Match` object. In its binary
  variant the second argument is tested first and it should be an operations logger
  configuration map containing the `:fn/reporter` key."
  ([req-or-match]
   (if-some [lgr (or (get (http/get-route-data req-or-match :oplog/config) :fn/reporter)
                     (when-not (instance? Match req-or-match)
                       (get (get req-or-match :oplog/config) :fn/reporter))
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
  (when-some [lgr (oplog-logger-populated req-or-match)]
    (lgr message)))

;; Routing data and settings helpers

(defn router-match?
  "Returns true if the given argument is Reitit's Match object."
  [v]
  (instance? Match v))

(defn on-page?
  "Checks if a current page matches the given route name (if an identifier is given) or
  the exact path. For multiple page names or paths, it returns true when any of them
  matches."
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

(defn lang-param-id
  "Returns language parameter ID obtained from language settings. Falls back to :lang
  when nothing was found."
  [req]
  (or (get (get req :language/settings) :language-param) :lang))

(defn guess-lang-param-id
  "For the given src argument, tries to obtain language ID. If it's a map it looks
  for :language-param key and for :language/settings if that
  fails. If :language/settings is found, it will try to get :language-param, assuming
  it's a map too. If the argument is not a map it will simply convert it into a
  keyword (without a namespace). If all of that fails (e.g. the src is nil) then
  the :lang keyword is returned."
  ([] :lang)
  ([src]
   (or (if (map? src)
         (or (get src :language-param)
             (some-> (get src :language/settings) :language-param))
         (some-keyword-simple src))
       :lang)))

(defn login-page?
  "Returns true if the current (or given as a match) page is a login page (has :login-page?
  route data set to a truthy value)."
  ([req]            (boolean (http/get-route-data req :login-page?)))
  ([req ring-match] (boolean (http/get-route-data req ring-match :login-page?))))

(defn auth-page?
  "Returns true if the current (or given as a match) page is an authentication
  page (has :auth-page? route data set to a truthy value)."
  ([req]            (boolean (http/get-route-data req :auth-page?)))
  ([req ring-match] (boolean (http/get-route-data req ring-match :auth-page?))))

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

(def ^:const path-splitter (re-pattern "([^\\?\\#]+)(\\#[^\\?]+)?(\\?.*)?"))
(def ^:const split-qparams (re-pattern "[^\\?\\#]+|[\\?\\#].*"))
(def ^:const on-slash      (re-pattern "/"))
(def ^:const slash-break   (re-pattern "[^/]+|/"))

(defn- path-variants-core
  "Generates a list of all possible language variants of a path."
  ([path lang-id]
   (when-some [path (some-str path)]
     (when-some [lang (some-str lang-id)]
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
   (path-param req (lang-param-id req)))
  ([req-or-match path-or-lang-settings]
   (if (router-match? req-or-match)
     (path-param req-or-match (guess-lang-param-id path-or-lang-settings))
     (path-param req-or-match path-or-lang-settings (lang-param-id req-or-match))))
  ([req path router]
   (path-param nil path (lang-param-id req) (or router (http/router req))))
  ([_ path router language-settings-or-param]
   (path-param nil path (guess-lang-param-id language-settings-or-param) router)))

(defn template-path
  "Replaces parameters in the given path using a template."
  ([match params]
   (template-path match params nil))
  ([match params query-params]
   (when match
     (template-path (r/match->path match query-params)
                    (get match :template)
                    params nil)))
  ([path template params _]
   (when-some [template (some-str template)]
     (->> (map (map/map-keys str params)
               (concat (re-seq slash-break template) (repeat nil))
               (re-seq slash-break (str path)))
          (apply str)))))

(defn split-query-params-simple
  "Splits path into 2 components: path string and location / query params
  string. Returns a sequence."
  [path]
  (when path (re-seq split-qparams path)))

(defn split-query-params
  "Splits path into 3 string components: path, location and query params. Returns a
  vector."
  [path]
  (when path
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
       (when (some->> path
                      (r/match-by-path router)
                      :path-params param #{pvalue})
         (str path location qparams)))
     (when match-or-path
       (let [[path location qparams] (split-query-params match-or-path)
             qparams                 (when-not (not-empty query-params) qparams)
             m                       (r/match-by-path router path)]
         (when (some-> m :path-params param #{pvalue})
           (some-> (r/match->path m query-params)
                   (str location qparams))))))))

(defn path-template-with-param
  "Returns a path template for the given match if the route supports the given
  parameter."
  ([match required-param]
   (path-template-with-param match required-param nil))
  ([match required-param short-circuit]
   (when-some [required-param (some-keyword-simple required-param)]
     (when-some [t (get match :template)]
       (when (or (some? short-circuit)
                 (contains? (get match :required) required-param)
                 (some #{(str required-param)} (re-seq slash-break t)))
         t)))))

(defn has-param?
  "Checks if the given route match can be parameterized with a parameter of the given
  id."
  [match param]
  (when-some [param (some-keyword-simple param)]
    (or (contains? (get match :required) param)
        (when-some [t (get match :template)]
          (some? (some #{(str param)} (re-seq slash-break t)))))))

(defn- parameterized-page-core
  [param rtr id pvalue params query-params require-param? name-path-fallback?]
  (let [pvalue (some-str pvalue)
        param  (some-keyword-simple param)]
    (if (ident? id)
      ;; identifier given (route name)
      (when-some [m (r/match-by-name rtr id (assoc params param pvalue))]
        (if require-param?
          (or (req-param-path rtr m param pvalue query-params)
              (when name-path-fallback?
                (when-some [path (some-str (r/match->path m))]
                  (parameterized-page-core param rtr path
                                           pvalue params query-params
                                           require-param? false))))
          (r/match->path m query-params)))
      ;; path given
      (when id
        (let [[id location qparams] (split-query-params id)
              qparams               (when-not (not-empty query-params) qparams)
              m                     (r/match-by-path rtr id)
              cur-pvalue            (get (get m :path-params) param)]
          (if (= cur-pvalue pvalue)
            ;; path is parameterized and the parameter value is the same
            (some-> (r/match->path m query-params) (str location qparams))
            ;; path is not parameterized or the parameter value is different
            (if-some [template (path-template-with-param m param cur-pvalue)]
              ;; path is parameterized with our parameter
              ;; we can re-parameterize the path by calling template-path
              (when-some [p (template-path m {param pvalue})]
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
  ([] nil)
  ([req]
   (r/match->path (get req ::r/match) (get req :query-params)))
  ([req id-or-path]
   (when-some [rtr (get req ::r/router)]
     (if (ident? id-or-path)
       (some-> (r/match-by-name rtr id-or-path) r/match->path)
       (when-some [path (some-str id-or-path)]
         (let [[path location qparams] (split-query-params path)]
           (some-> (r/match-by-path rtr path) r/match->path (str location qparams)))))))
  ([req id-or-path param param-value]
   (when-some [rtr (get req ::r/router)]
     (parameterized-page-mem param rtr id-or-path param-value nil nil true false)))
  ([req id-or-path param param-value params-or-require-param?]
   (when-some [rtr (get req ::r/router)]
     (if (boolean? params-or-require-param?)
       (parameterized-page-mem param rtr id-or-path param-value nil nil params-or-require-param? false)
       (parameterized-page-mem param rtr id-or-path param-value params-or-require-param? nil true false))))
  ([req id-or-path param param-value params query-params-or-require-param?]
   (when-some [rtr (get req ::r/router)]
     (if (boolean? query-params-or-require-param?)
       (parameterized-page-mem param rtr id-or-path param-value params nil query-params-or-require-param? false)
       (parameterized-page-mem param rtr id-or-path param-value params query-params-or-require-param? true false))))
  ([req id-or-path param param-value params query-params require-param?]
   (when-some [rtr (get req ::r/router)]
     (parameterized-page-mem param rtr id-or-path param-value params query-params require-param? false)))
  ([req id-or-path param param-value params query-params require-param? name-path-fallback?]
   (when-some [rtr (get req ::r/router)]
     (parameterized-page-mem param rtr id-or-path param-value params query-params require-param? name-path-fallback?)))
  ([_ id-or-path param param-value params query-params require-param? name-path-fallback? router]
   (when (some? router)
     (parameterized-page-mem param router id-or-path param-value params query-params require-param? name-path-fallback?))))

(defn localized-page
  "Generates a page path for the given page identifier and language identifier. When
  called with just a request map, returns a path of the current page but re-generated
  to support current language in use (taken from :language/str key of the request
  map).

  The optional lang-required? argument (the last one in a quaternary variant, set to
  true when not given) enables extra check which eliminates the resulting pages not
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
  ([] nil)
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
   (when-some [rtr (get req ::r/router)]
     (if (boolean? params-or-lang-required?)
       (parameterized-page-mem (lang-param-id req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               nil nil
                               params-or-lang-required?
                               false)
       (parameterized-page-mem (lang-param-id req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               params-or-lang-required?
                               nil true false))))
  ([req name-or-path lang params query-params-or-lang-required?]
   (when-some [rtr (get req ::r/router)]
     (if (boolean? query-params-or-lang-required?)
       (parameterized-page-mem (lang-param-id req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               params nil
                               query-params-or-lang-required?
                               false)
       (parameterized-page-mem (lang-param-id req)
                               rtr name-or-path
                               (or lang (get req :language/str))
                               params query-params-or-lang-required?
                               true false))))
  ([req name-or-path lang params query-params lang-required?]
   (when-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param-id req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             lang-required? false)))
  ([req name-or-path lang params query-params lang-required? name-path-fallback?]
   (when-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param-id req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             lang-required?
                             name-path-fallback?)))
  ([req name-or-path lang params query-params lang-required? name-path-fallback? router]
   (when (some? router)
     (parameterized-page-mem (lang-param-id req)
                             router name-or-path
                             (or lang (get req :language/str))
                             params query-params
                             lang-required?
                             name-path-fallback?)))
  ([_ name-or-path lang params query-params lang-required? name-path-fallback? router language-settings-or-param]
   (when (some? router)
     (parameterized-page-mem (guess-lang-param-id language-settings-or-param)
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
   (when-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param-id req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             nil nil false false)))
  ([req name-or-path lang params]
   (when-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param-id req)
                             rtr name-or-path
                             (or lang (get req :language/str))
                             params nil false false)))
  ([req name-or-path lang params query-params]
   (when-some [rtr (get req ::r/router)]
     (parameterized-page-mem (lang-param-id req)
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
   (when rtr
     (if (ident? id)
       ;; identifier given (route name)
       (let [params (if fb-lang-settings (apply assoc params fb-lang-settings) params)]
         (some-> (r/match-by-name rtr id params)
                 (r/match->path query-params)))
       ;; path given
       (when id
         (let [[id location qparams] (split-query-params id)
               qparams               (when-not (not-empty query-params) qparams)]
           (some-> (r/match-by-path rtr id)
                   (r/match->path query-params)
                   (str location qparams))))))))

(defn- lang-from-req
  [req]
  (when-some [lang-param-id (lang-param-id req)]
    (when-some [lang-str (get req :language/str)]
      [lang-param-id lang-str])))

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
                (list (guess-lang-param-id lang-settings-or-param) hint-lang))
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
  "Returns a path for current page."
  [req]
  (page req))

(defn login-page
  "Returns path for a login page."
  ([req]         (page :manager/login))
  ([req lang-id] (page :manager/login lang-id)))

(defn auth-page
  "Returns path for an authentication page."
  ([req]         (page :manager/welcome))
  ([req lang-id] (page :manager/welcome lang-id)))

(defn temporary-redirect
  "Uses the page function to calculate the destination path on a basis of page
  name (identifier) or a path (a string) and performs temporary redirect on it. If
  the language is given it uses the localized-page function. If there is no language
  given but the page identified by its name requires a language parameter to be set,
  it will be obtained from the given request map (under the key :language/str)."
  ([]
   (resp/temporary-redirect "/"))
  ([req]
   (resp/temporary-redirect (page req)))
  ([req name-or-path]
   (resp/temporary-redirect (page req name-or-path)))
  ([req name-or-path lang]
   (resp/temporary-redirect (page req name-or-path lang)))
  ([req name-or-path lang params]
   (resp/temporary-redirect (page req name-or-path lang params)))
  ([req name-or-path lang params query-params]
   (resp/temporary-redirect (page req name-or-path lang params query-params)))
  ([req name-or-path lang params query-params & more]
   (resp/temporary-redirect
    (apply page req name-or-path lang params query-params more))))

(defn localized-temporary-redirect
  "Uses the localized-page function to calculate the destination path on a basis of
  page name (identifier) or a path (a string) and performs temporary redirect on
  it. If the language is not given it will use a value from the given request
  map (under the key :language/str).

  The difference between this function and the temporary-redirect function is in
  binary variants of them (when a request map and a name or a path are given as
  arguments). The temporary-redirect function will fail to generate a redirect if
  there is no language parameter and the given path does not point to an existing
  page. On the contrary, this function will generate a localized path using a
  language obtained from a request (under :language/str key) and if there will be no
  language-parameterized variant of the path, it will fail. Use this function to make
  sure that localized path will be produced, or nil."
  ([]
   (resp/temporary-redirect "/"))
  ([req]
   (resp/temporary-redirect (localized-page req)))
  ([req name-or-path]
   (resp/temporary-redirect (localized-page req name-or-path)))
  ([req name-or-path lang]
   (resp/temporary-redirect (localized-page req name-or-path lang)))
  ([req name-or-path lang params]
   (resp/temporary-redirect (localized-page req name-or-path lang params)))
  ([req name-or-path lang params query-params]
   (resp/temporary-redirect (localized-page req name-or-path lang params query-params)))
  ([req name-or-path lang params query-params & more]
   (resp/temporary-redirect
    (apply localized-page req name-or-path lang params query-params more))))

(def ^{:arglists '([req]
                   [req name-or-path]
                   [req name-or-path lang]
                   [req name-or-path lang params]
                   [req name-or-path lang params query-params]
                   [req name-or-path lang params query-params & more])}
  move-to
  localized-temporary-redirect)

(defn see-other
  "Uses the page function to calculate the destination path on a basis of page
  name (identifier) or a path (a string) and performs a 303 (see other) redirect on
  it. If the language is given it uses the localized-page function. If there is no
  language given but the page identified by its name requires a language parameter to
  be set, it will be obtained from the given request map (under the
  key :language/str)."
  ([]
   (resp/see-other "/"))
  ([req]
   (resp/see-other (page req)))
  ([req name-or-path]
   (resp/see-other (page req name-or-path)))
  ([req name-or-path lang]
   (resp/see-other (page req name-or-path lang)))
  ([req name-or-path lang params]
   (resp/see-other (page req name-or-path lang params)))
  ([req name-or-path lang params query-params]
   (resp/see-other (page req name-or-path lang params query-params)))
  ([req name-or-path lang params query-params & more]
   (resp/see-other
    (apply page req name-or-path lang params query-params more))))

(defn localized-see-other
  "Uses the localized-page function to calculate the destination path on a basis of
  page name (identifier) or a path (a string) and performs a 303 (see other) redirect
  on it. If the language is not given it will use a value from the given request
  map (under the key :language/str).

  The difference between this function and the see-other function is in
  binary variants of them (when a request map and a name or a path are given as
  arguments). The see-other function will fail to generate a redirect if
  there is no language parameter and the given path does not point to an existing
  page. On the contrary, this function will generate a localized path using a
  language obtained from a request (under :language/str key) and if there will be no
  language-parameterized variant of the path, it will fail. Use this function to make
  sure that localized path will be produced, or nil."
  ([]
   (resp/see-other "/"))
  ([req]
   (resp/see-other (localized-page req)))
  ([req name-or-path]
   (resp/see-other (localized-page req name-or-path)))
  ([req name-or-path lang]
   (resp/see-other (localized-page req name-or-path lang)))
  ([req name-or-path lang params]
   (resp/see-other (localized-page req name-or-path lang params)))
  ([req name-or-path lang params query-params]
   (resp/see-other (localized-page req name-or-path lang params query-params)))
  ([req name-or-path lang params query-params & more]
   (resp/see-other
    (apply localized-page req name-or-path lang params query-params more))))

(def ^{:arglists '([req]
                   [req name-or-path]
                   [req name-or-path lang]
                   [req name-or-path lang params]
                   [req name-or-path lang params query-params]
                   [req name-or-path lang params query-params & more])}
  go-to
  localized-see-other)

;; Language

(def ^:const language-pickers-default
  [language/path-lang-id
   language/form-lang-id
   :language/user
   :language/client
   language/accept-lang-id
   :language/id])

(def ^:const language-pickers-client-preferred
  [language/form-lang-id
   :language/user
   language/accept-lang-id
   :language/client
   :language/id
   language/path-lang-id])

(def ^:const language-pickers-logged-in
  [language/form-lang-id
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

(defn add-slash
  "Adds trailing slash to a path unless it already exists."
  [uri]
  (if uri
    (let [c (unchecked-int (count uri))]
      (if (pos? c)
        (if (= \/ (.charAt uri (unchecked-dec-int c))) uri (str uri "/"))
        "/"))
    "/"))

(defn slash-redir
  "Redirects to a slash-trailed version of the same URI. If the URI already has a
  slash, it returns a req."
  [req]
  (resp/temporary-redirect (add-slash (get req :uri))))

(defn lang-redir
  "Redirects to a best-suited language version of the URI."
  [req]
  (move-to req
           (or (http/get-route-data req :destination) "/")
           (pick-language-str req language-pickers-client-preferred)))

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
  (when-some [lock-time (soft-lock-time user)]
    (let [d (t/between lock-time time)]
      (when (time/pos-duration? d) d))))

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
   (when lock-passed
     (when-some [lock-wait (lock-wait auth-config-or-lw)]
       (t/< lock-passed lock-wait))))
  ([user auth-config-or-lw time]
   (when auth-config-or-lw
     (when-some [lock-passed (soft-lock-passed user time)]
       (when-some [lock-wait (lock-wait auth-config-or-lw)]
         (t/< lock-passed lock-wait))))))

(defn soft-lock-remains
  "Returns the amount of time left before reaching lock-wait. If the amount is negative
  or zero, it returns nil. Does not connect to a database."
  ([lock-passed auth-config-or-lw]
   (when lock-passed
     (when-some [lock-wait (lock-wait auth-config-or-lw)]
       (t/- lock-wait lock-passed))))
  ([user auth-config-or-lw time]
   (when-some [lock-passed (soft-lock-passed user time)]
     (when-some [lock-wait (lock-wait auth-config-or-lw)]
       (let [d (t/- lock-wait lock-passed)]
         (when (time/pos-duration? d) d))))))

;; Sessions

(p/import-vars [amelinium.http.middleware.session
                session-key])

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

;; HTML generators and transformers

(defn- calc-roles
  [ctx-labeler roles-labeler missing-label [ctx & roles]]
  (into [(or (some-str (ctx-labeler ctx)) (str ctx))]
        (mapv (comp (fnil identity missing-label) roles-labeler) roles)))

(defn roles-table
  ([req]
   (roles-table req nil))
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
         gctx-labeler         (when have-gctx? (assoc roles-labeler :! present-label))
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
     (when (and (seq data) labels)
       (html (table/to-table1d data labels))))))

;; HTML rendering

(def empty-lazy-map
  (map/lazy))

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

(defn some-resource
  "Returns the given path if there is a resource it points to. Otherwise it returns
  nil. Multiple arguments are joined using str."
  ([path]
   (when-some [path (str path)]
     (and (io/resource path) path)))
  ([path & more]
   (when-some [path (apply str path more)]
     (and (io/resource path) path))))

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
    (or (first (keep #(apply some-resource %) pths))
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
  explicitly set to `false`) and ` session map (used only when the language cannot be
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
   (let [lang (or lang
                  (when-not (false? lang)
                    (or (get req :language/str)
                        (pick-language-str req))))
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
     (-> (render req data views-subdir layouts-subdir lang sess)
         resp-fn
         (resp/content-type "text/html")))))

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

(selmer/add-tag!
 :anti-spam-field
 (fn [args ctx]
   (anti-spam-code (get ctx :validators/config) 2)))

(defn lang-url
  [localized? ctx path-or-name lang params query-params lang-settings]
  (let [router        (or (get ctx ::r/router) (get ctx :router))
        lang          (or lang (get ctx :language/str) (some-str (get ctx :language)) (some-str (get ctx :lang)))
        lang-settings (or lang-settings (get ctx :language/settings) (get ctx :language-param) :lang)
        path-or-name  (when path-or-name (selmer/render path-or-name ctx {:tag-open \[ :tag-close \]}))
        path-or-name  (if (and path-or-name (str/starts-with? path-or-name ":")) (keyword (subs path-or-name 1)) path-or-name)
        path-fn       (if localized? localized-path path)
        out-path      (path-fn path-or-name lang params query-params router lang-settings)
        out-path      (if out-path out-path (when-not (ident? path-or-name) (some-str path-or-name)))]
    out-path))

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

;; Language helpers

(defn lang-id
  [req]
  (get req :language/id))

(defn lang-str
  [req]
  (get req :language/str))

(defn lang-config
  [req]
  (get req :language/settings))