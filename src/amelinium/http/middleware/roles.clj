(ns

    ^{:doc    "amelinium service, role-based access control middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.roles

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                       :as        set]
            [clojure.string                    :as        str]
            [tick.core                         :as          t]
            [buddy.core.hash                   :as       hash]
            [buddy.core.codecs                 :as     codecs]
            [taoensso.nippy                    :as      nippy]
            [next.jdbc.sql                     :as        sql]
            [next.jdbc                         :as       jdbc]
            [reitit.ring                       :as       ring]
            [ring.util.http-response           :as       resp]
            [amelinium.db                      :as         db]
            [amelinium.logging                 :as        log]
            [amelinium.system                  :as     system]
            [amelinium.http.middleware.session :as    session]
            [amelinium.types.session           :refer    :all]
            [amelinium                         :refer    :all]
            [io.randomseed.utils               :refer    :all]
            [io.randomseed.utils.time          :as       time]
            [io.randomseed.utils.var           :as        var]
            [io.randomseed.utils.map           :as        map]
            [io.randomseed.utils.ip            :as         ip])

  (:import [amelinium Session]))

(defn known?
  [config role]
  (and role config (contains? (get config :roles) role)))

(defn unknown?
  [config role]
  (not (known? config role)))

(defn description
  [config role]
  (some-str (get (get config :roles) role)))

(defn filter-in-context
  "Filters roles map by the given context, merging-in global roles when needed. Returns
  a set of roles matching the context or nil."
  ([req]
   (filter-in-context (get req :roles/context) (get req :roles) (get req :roles/config)))
  ([context roles config]
   (if (valuable? roles)
     (let [context        (some-keyword context)
           global-context (get config :global-context)
           context-roles  (if context (get roles context))
           global-roles   (if global-context (get roles global-context))]
       (set/union context-roles global-roles)))))

(defn user-authenticated?
  "Returns true if user is authenticated, false otherwise."
  [session]
  (boolean
   (if (session/session? session)
     (let [^Session session session]
       (and (.valid?         ^Session session)
            (some? (.id      ^Session session))
            (some? (.user-id ^Session session)))))))

(defn user-authorized?
  "Checks if user is authorized in the specified context. Takes a request map and a set
  of roles which are tested to be true in the detected context. Uses :data entry of
  the current route to get local configuration in which it looks for
  keys: :roles/forbidden, :roles/any and :roles/all (in that order).
  The :roles/forbidden should contain a set of roles which make access unauthorized
  if at least one of the current roles is matching. The :roles/any authorizes
  operation if at least one of the current roles is matching. The :roles/all, if
  present, matches if all the specified roles are effectively present. The default
  strategy (when there are no rules specified or just the :roles/forbidden) is to
  allow but it can be changed in the middleware configuration, under the
  key :authorize-default?. Returns nil if the access is forbidden, true if granted,
  false if there were rules but none matched."
  ([req]
   (user-authorized? req
                     (get req :roles/in-context)
                     (get req :roles/config req)))
  ([req in-context]
   (user-authorized? req in-context
                     (get req :roles/config)))
  ([req in-context config]
   (user-authorized? req in-context nil
                     (get config :authorize-default? true)))
  ([req in-context data auth-default?]
   (if-some [data (or data (not-empty (get (ring/get-match req) :data)))]
     (if-not (some-> (get data :roles/forbidden) (contains-some? in-context))
       (if-some [roles-any (not-empty (get data :roles/any))]
         (or  (contains-some? roles-any in-context)
              (some-> (get data :roles/all) (set/subset? in-context)) false)
         (if-some [roles-all (get data :roles/all)]
           (set/subset? roles-all in-context)
           auth-default?)))
     auth-default?))
  ([req in-context auth-default? roles-forbidden roles-any roles-all]
   (if-not (some-> roles-forbidden (contains-some? in-context))
     (if roles-any
       (or (contains-some? roles-any in-context)
           (some-> roles-all (set/subset? in-context)) false)
       (if roles-all
         (set/subset? roles-all in-context)
         auth-default?))
     auth-default?)))

(defn- rename-context-column
  "For each result (which should be a map) of the given `query-results` renames a key
  reflecting the context column to `:context` when its keyword identifier (passed as
  `ctx-col`) is different than `:context`."
  [ctx-col query-results]
  (seq
   (if (or (nil? ctx-col) (= ctx-col :context))
     query-results
     (map (fn [m]
            (if (contains? m ctx-col)
              (dissoc (assoc m :context (get m ctx-col)) ctx-col)
              m))
          query-results))))

(defn query-roles
  "Gets roles for the given user ID from a database. Returns a map of roles as a
  sequence of maps."
  ([user-id config db]
   (query-roles db user-id))
  ([db user-id]
   (if (and db user-id)
     (sql/find-by-keys db :roles {:user-id user-id} db/opts-simple-map))))

(defn parse-roles
  "Parses a sequence of maps expressing roles and generates a single map with roles
  grouped by context."
  ([roles user-id config]
   (parse-roles roles user-id config
                (get config :roles)
                (get config :global-context)
                (get config :context-column)
                (get config :keep-unknown?)))
  ([roles user-id config known-roles global-context context-column keep-unknown?]
   (let [remove-unknown (if (or keep-unknown? (empty? known-roles))
                          identity
                          (partial filter (comp (partial contains? known-roles) :role)))]
     (if (seq roles)
       (->> roles
            (filter identity)
            (map #(update % :role keyword))
            remove-unknown
            (rename-context-column context-column)
            (group-by :context)
            (map (juxt-seq first (comp (partial map :role) second)))
            (filter (every-pred first (comp seq second)))
            (map (juxt (comp keyword first) (comp set second)))
            (into {}) not-empty)))))

;; todo: add role (with context) to a user
;;       del role (with context) from a user

(defn unauthorized
  "Generates unauthorized redirect."
  [config]
  (resp/see-other (str (get config :unauthorized-redirect "/unauthorized"))))

(defn invalidate-cache!
  [req-or-config user-id]
  (let [config (:roles/config req-or-config req-or-config)]
    ((get config :invalidator) config user-id)))

(defn handler
  "Processes RBAC information by taking a user-id and configuration options."
  ([user-id config]
   (handler user-id config
            (get config :query-roles-fn)
            (get config :roles)
            (get config :logged-in-role)
            (get config :global-context)
            (get config :context-column)
            (get config :keep-unknown?)))
  ([user-id config
    query-roles-fn known-roles login-role
    global-context context-column keep-unknown?]
   (let [roles (-> user-id
                   query-roles-fn
                   (parse-roles user-id config known-roles
                                global-context context-column keep-unknown?))]
     (if login-role
       (update roles global-context (fnil #(conj % login-role) #{}))
       roles))))

(defn get-roles-for-user-id
  "Retrieves all roles for the given user ID and returns them as a map where keys are
  contexts (expressed as keywords) and values are sets of roles assigned to those
  contexts. If the user ID is `nil` or `false`, it returns a map with a global
  context and just one, anonymous role assigned to it within a set. If there is no
  anonymous role passed as an argument, returns `nil` in such case. Self-role is not
  included, even if it configured, since it is highly conditioned and may depend on
  data from the request or an external data source."
  ([config user-id]
   (get-roles-for-user-id config user-id (get config :processor)
                          (get config :global-context)
                          (get config :anonymous-role)))
  ([config user-id handler-fn]
   (get-roles-for-user-id config user-id handler-fn
                          (get config :global-context)
                          (get config :anonymous-role)))
  ([config user-id handler-fn global-context anonymous-role]
   (if (valuable? user-id)
     (handler-fn user-id)
     (if anonymous-role {global-context #{anonymous-role}}))))

(defn get-roles-from-session
  "Uses session map (`session`) to obtain current user's ID and then calls `handler-fn`
  with user ID to obtain user's roles."
  ([config ^Session session]
   (get-roles-from-session config session (get config :processor)
                           (get config :global-context)
                           (get config :anonymous-role)
                           (get config :known-user-role)))
  ([config ^Session session handler-fn]
   (get-roles-from-session config session handler-fn
                           (get config :global-context)
                           (get config :anonymous-role)
                           (get config :known-user-role)))
  ([config ^Session session handler-fn
    global-context anonymous-role known-user-role]
   (let [user-id (valuable (session/user-id session))
         roles   (get-roles-for-user-id config user-id handler-fn global-context anonymous-role)]
     (if (or (not user-id) (session/valid? ^Session session) )
       roles
       (if known-user-role {global-context #{known-user-role}})))))

(defn get-req-context
  "Gets context from a request using a key path."
  ([req]
   (get-req-context req (get req :roles/config)))
  ([req config]
   (get-req-context req config (get config :req-context-path)))
  ([req config req-context-path]
   (some-keyword (get-in req req-context-path))))

(defn get-req-self
  "Gets a value from the given request map (`req`) located under a (possibly nested)
  key(s) specified by a sequential collection `self-path` and compares it with a
  value obtained from the same `req` map identified by `self-check-path`. If the
  values are equal, it returns `self-role`. Otherwise it returns `nil`. If the first
  obtained value is truthy (not `nil` and not `false`) but the second
  path (`self-check-path`) is not specified (is `nil` or `false`), then the value of
  `self-role` is also returned."
  ([req]
   (get-req-self req (get req :roles/config)))
  ([req config]
   (get-req-self req config
                 (get config :self-role)
                 (get config :req-self-path)
                 (get config :req-self-check-path)))
  ([req config self-role self-path self-check-path]
   (if self-path
     (if-some [req-self-value (get-in req self-path)]
       (if self-check-path
         (if (= req-self-value (get-in req self-check-path)) self-role)
         self-role)))))

(defn force-context
  "Forces different context by setting `:roles/context` and
  recalculating `:roles/in-context` in the given request map `req`."
  ([req context]
   (force-context req context false))
  ([req context self-role?]
   (let [context (some-keyword context)]
     (assoc req
            :roles/context    context
            :roles/in-context (delay (let [config (get req :roles/config)
                                           in-ctx (filter-in-context context
                                                                     (get req :roles)
                                                                     config)]
                                       (if self-role?
                                         (if-some [sr (get config :self-role)]
                                           (conj (or in-ctx #{}) sr)
                                           in-ctx)
                                         in-ctx)))))))

(defn inject-roles
  "Main handler for roles. Takes a request map and updates it with role
  information. Returns updated map which may be a response if redirects for
  unauthorized access are enabled.

  Internally it uses the `process` function, passing it a processor as the first
  argument. The `processor` should be a function which takes a configuration of roles
  as a map and the current user identifier. The default one is called `handler` but
  its memoized variant is used in the middleware wrapper.

  This function is wrapped and exposed in a configuration map (`:roles/config`) under
  the key `:handler`. It takes a single argument (a request map) and performs
  injection using enclosed configuration and a default, memoized processor."
  ([req]
   (inject-roles req (get req :roles/config)))
  ([req {:keys [config processor
                req-context-fn req-self-role-fn
                anonymous-role known-user-role self-role
                global-context authorize-default? session-key]
         :or   {authorize-default? true}
         :as   config}]
   (inject-roles req config processor
                 req-context-fn req-self-role-fn
                 anonymous-role known-user-role self-role
                 global-context authorize-default? session-key))
  ([req config processor rcfn srfn
    anonymous-role known-user-role self-role
    global-context authorize-default? session-key]
   (let [data        (not-empty (get (ring/get-match req) :data))
         [roles-forbidden
          roles-any
          roles-all] (if data [(not-empty (get data :roles/forbidden))
                               (not-empty (get data :roles/any))
                               (not-empty (get data :roles/all))])]
     (inject-roles req config processor rcfn srfn
                   anonymous-role known-user-role self-role
                   global-context authorize-default? session-key
                   roles-forbidden roles-any roles-all)))
  ([req config processor rcfn srfn
    anonymous-role known-user-role self-role
    global-context authorize-default? session-key
    roles-forbidden roles-any roles-all]
   (let [^Session session (session/of req session-key)
         authenticated?   (delay (user-authenticated? session))
         roles            (delay (let [sr (if (and self-role @authenticated?) (srfn req))]
                                   (cond-> (get-roles-from-session config session
                                                                   processor
                                                                   global-context
                                                                   anonymous-role
                                                                   known-user-role)
                                     sr (update global-context (fnil #(conj % sr) #{})))))
         context          (delay (rcfn req))
         in-context       (delay (filter-in-context @context @roles config))
         authorized?      (delay (user-authorized? req @in-context authorize-default?
                                                   roles-forbidden roles-any roles-all))
         unauth-redir     (get config :unauthorized-redirect)
         req              (-> req
                              (map/assoc-missing :roles/config config)
                              (assoc :roles               roles
                                     :roles/context       context
                                     :roles/in-context    in-context
                                     :user/authorized?    authorized?
                                     :user/authenticated? authenticated?))]
     (if (and unauth-redir (not @authorized?))
       (resp/see-other unauth-redir)
       req))))

(defn refresh
  "Recalculates roles by calling configured handler on a request map."
  [req]
  (if-some [cfg (get req :roles/config)]
    (if-some [hdl (get cfg :handler)]
      (hdl req)
      req)
    req))

;; Initialization

(defn- update-built-in-role
  "Updates built-in role if it does not exist in :roles of a config and according
  setting is present. The setting argument is a configuration key under which a role
  identifier should reside. If this is nil or false then this role will be disabled
  and removed from known roles of the configuration even if it exists there. If this
  is not nil and not false then it should contain an identifier of a role associated
  with th purpose explained by this setting name. If such role exists in known roles
  map of a configuration then nothing is done. If it does not exist it will be added
  with the given description. Original role (given as role argument) will not be
  removed if exists in known roles, even if we are using different role for the
  particular setting. If the added role exists in known roles it won't be replaced.
  If there is no setting present then role name and role description are added unless
  they already exist."
  [config setting role description]
  (let [known-roles (get config :roles {})]
    (assoc config :roles
           (if (contains? config setting)
             (if-let [setting-role (config setting)]
               (map/assoc-missing known-roles setting-role description)
               (dissoc known-roles role))
             (map/assoc-missing known-roles role description)))))

(defn- update-built-in-roles
  [config]
  (-> config
      (update-built-in-role :anonymous-role  :anonymous "Anonymous User")
      (update-built-in-role :logged-in-role  :user      "Logged-in User")
      (update-built-in-role :known-user-role :known     "Known User")
      (update-built-in-role :self-role       :self      "Resource Owner")))

(defn- setup-invalidator
  [processor mem-processor]
  (if (or (not mem-processor)
          (= mem-processor processor))
    (constantly nil)
    (db/invalidator mem-processor)))

(defn- setup-req-path
  [v]
  (if v
    (if (coll? v)
      (mapv some-keyword v)
      [(some-keyword v)])))

(defn- setup-req-fn
  [v]
  (if v
    (if (fn? v) v (var/deref-symbol v))))

(defn- setup-session-key
  [config]
  (if-some [sk (get config :session-key)]
    config
    (assoc config :session-key
           (or (get (get (or (get config :session/config) (get config :session)) :config) :session-key)
               (get (or (get config :session/config) (get config :session)) :session-key)
               :session))))

(defn prep-config
  [config]
  (-> config
      (update            :handler            (fnil identity handler))
      (update            :cache-ttl          time/parse-duration)
      (update            :cache-size         safe-parse-long)
      (update            :context-column     (fnil some-keyword-simple :context))
      (update            :global-context     (fnil some-keyword-simple :!))
      (update            :req-context-path   setup-req-path)
      (update            :req-self-path      setup-req-path)
      (update            :self-equality-path setup-req-path)
      (update            :req-context-fn     setup-req-fn)
      (update            :req-self-role-fn   setup-req-fn)
      (update            :query-roles-fn     setup-req-fn)
      (update            :req-context-fn     (fnil identity get-req-context))
      (update            :req-self-role-fn   (fnil identity get-req-self))
      (update            :query-roles-fn     (fnil identity query-roles))
      (update            :session-key        some-keyword)
      (map/assoc-missing :keep-unknown?      true)
      (update            :keep-unknown?      boolean)
      (map/assoc-missing :authorize-default? true)
      (update            :authorize-default? boolean)
      update-built-in-roles))

(defn wrap-roles
  "Role-based access maintaining middleware. Uses the function associated with
  `:handler` configuration key (`handler` by default) to process roles information.
  This handler is wrapped in memoizer function to cache the results and passed as a
  first argument to `inject-roles` responsible for putting role information into
  request map.

  So the workflow is:
  - For each request `inject-roles` is called.
  - `inject-roles` extracts session object from the request and calls `get-roles-from-session`.
  - `get-roles-from-session` calls handler on user ID (obtained from the session).
  - `get-roles-from-session` adds anonymous role if the user ID could not be obtained.
  - If there is user ID the handler is called from within `get-roles-from-session`.
  - Handler gets user ID and calls a function under configuration key `:query-roles-fn`.
  - The result of querying the database is a sequence of maps.
  - The maps are parsed with `parse-roles`.
  - The result of `parse-roles` is a map with (context -> sets of roles).
  - The result is returned to `inject-roles` and injected into a request map.

  Additionally, configuration option `:req-context-fn` can specify an alternative
  function which will be called within `inject-roles` to get context from the request.

  One can also use `:req-self-role-fn` to provide a function used to get an identifier
  of a self-role (only when user is authenticated) on a basis of the request map."
  [{:keys [db req-context-path req-self-path req-self-check-path
           req-self-role-fn req-context-fn query-roles-fn
           global-context context-column
           self-role logged-in-role anonymous-role known-user-role roles
           authorize-default? keep-unknown?]
    :as   config}]
  (if-some [processor (var/deref-symbol (:handler config))]
    (let [handler-name     (:handler config)
          dbname           (db/db-name db)
          config           (-> config (dissoc :handler) (update :db db/ds) prep-config setup-session-key)
          db               (get config :db)
          session-key      (get config :session-key)
          config           (dissoc config :req-context-fn :req-self-role-fn)
          req-context-fn   #(req-context-fn   % config req-context-path)
          req-self-role-fn #(req-self-role-fn % config self-role req-self-path req-self-check-path)
          query-roles-fn   #(query-roles-fn   % config db)
          processor        #(processor        % config query-roles-fn roles logged-in-role
                                              global-context context-column keep-unknown?)
          mem-processor    (db/memoizer processor config)
          invalidator      (setup-invalidator processor mem-processor)
          config           (assoc config
                                  :getter  #(get-roles-for-user-id mem-processor %1 %2)
                                  :handler #(inject-roles %1 config mem-processor
                                                          req-context-fn req-self-role-fn
                                                          anonymous-role known-user-role self-role
                                                          global-context authorize-default?
                                                          session-key)
                                  :query-roles-fn query-roles-fn
                                  :processor      mem-processor
                                  :invalidator    invalidator)]
      (log/msg "Installing role-based access control handler:" handler-name)
      (log/msg "Using database" dbname "for permissions")
      {:name    ::roles
       :compile (fn [{:keys [no-roles?] :as data} opts]
                  (if (and (not no-roles?) db)
                    (let [roles-forbidden (not-empty (get data :roles/forbidden))
                          roles-any       (not-empty (get data :roles/any))
                          roles-all       (not-empty (get data :roles/all))]
                      (fn [h]
                        (fn [req]
                          (h
                           (inject-roles req config mem-processor
                                         req-context-fn req-self-role-fn
                                         anonymous-role known-user-role self-role
                                         global-context authorize-default? session-key
                                         roles-forbidden roles-any roles-all)))))))})))

(system/add-prep  ::default [_ config] (prep-config config))
(system/add-init  ::default [_ config] (wrap-roles  config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
