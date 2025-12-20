(ns

    ^{:doc    "amelinium service, application."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.app

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [puget.printer            :refer [cprint pprint]]
            [ns-tracker.core          :as         ns-tracker]
            [amelinium.system         :as             system]
            [amelinium.logging        :as                log]
            [tick.core                :as                  t]
            [io.randomseed.utils.map  :as                map]
            [io.randomseed.utils.var  :as                var]
            [io.randomseed.utils      :refer            :all]))

(set! *warn-on-reflection* true)

;;
;; defaults
;;

(def ^:dynamic *ns-reload-watch-dirs*             ["src" "test"])
(def ^:dynamic *local-config*                                nil)

(def ^:dynamic *resource-config-dirs* ["translations/amelinium"
                                       "config/amelinium"])

(def ^:dynamic *resource-dev-dirs*    ["translations/amelinium"
                                       "config/amelinium"
                                       "config/amelinium-dev"])

(def ^:dynamic *resource-admin-dirs*  ["translations/amelinium"
                                       "config/amelinium"
                                       "config/amelinium-admin"])

(defmacro with-config-dirs
  "Sets the dynamic variable `amelinium.app/*resource-config-dirs*` and executes body."
  [dirs & body]
  `(binding [*resource-config-dirs* ~dirs]
     ~@body))

(defmacro with-local-config
  "Sets the dynamic variable `amelinium.app/*local-config*` and executes body."
  [local-file & body]
  `(binding [*local-config* ~local-file]
     ~@body))

(defmacro with-configs
  "Sets the dynamic variables `amelinium.app/*resource-config-dirs*`,
  `amelinium.app/*resource-dev-dirs*`, `amelinium.app/*resource-admin-dirs*`
  and `amelinium.app/*local-config*` to the given values and executes body.
  May be helpful in overriding defaults when instantiating `app.clj`
  and creating custom wrappers around common management functions
  (like `amelinium.app/start!`, `amelinium.app/stop!`, `amelinium.app/reload!`
  and others)."
  [local-file dirs dev-dirs admin-dirs & body]
  `(binding [*resource-config-dirs* ~dirs
             *resource-dev-dirs*    ~dev-dirs
             *resource-admin-dirs*  ~admin-dirs
             *local-config*         ~local-file]
     ~@body))

(defmacro with-watch-dirs
  "Sets the dynamic variable `amelinium.app/*ns-reload-watch-dirs*` to the given value and executes
  body. May be helpful when creating custom namespace tracker (and setting
  `amelinium.app/*ns-tracker*` value) in instantiated `app.clj`."
  [watch-dirs & body]
  `(binding [*ns-reload-watch-dirs* ~watch-dirs]
     ~@body))

(defmacro with-ns-tracker
  "Sets `amelinium.app/*ns-tracker*` to the given value and executes body. Helpful when
  creating custom `amelinium.app/reload!` function in instantiated `app.clj`."
  [ns-tracker & body]
  `(binding [*ns-tracker* ~ns-tracker]
     ~@body))

;;
;; application init key (used to force deps, e.g. with logger)
;;

(derive ::init ::system/nil)

;;
;; property names
;;

(system/add-init
 ::properties
 [_ config]
 (let [config (-> config
                  (update :name        normalize-name "unnamed system")
                  (update :title       normalize-name "unnamed system")
                  (update :author      normalize-name "unknown author")
                  (update :profile     some-keyword)
                  (update :node        some-keyword)
                  (update :version     normalize-name "1.0.0")
                  (update :license     normalize-name "Copyright")
                  (update :description normalize-name ""))]
   config))

;;
;; time zone reference
;;

(derive ::timezone ::system/value)

;;
;; hot reloading
;;

(defn make-ns-tracker
  "Creates `ns-tracker` instance for tracking code changes in directories specified
  with `amelinium.app/*ns-reload-watch-dirs*`."
  []
  (if-some [wdirs *ns-reload-watch-dirs*]
    (if-some [wdirs (and (sequential? wdirs) (seq wdirs))]
      (ns-tracker/ns-tracker wdirs))))

(def ^:dynamic *ns-tracker*
  (make-ns-tracker))

(defn reload-namespaces
  "Reloads code (using `clojure.core/require` with `:reload` flag) in namespaces found
  in files listed in `amelinium.app/*ns-reload-watch-dirs*`."
  []
  (if-some [nstracker *ns-tracker*]
    (doseq [ns-sym (nstracker)]
      (require ns-sym :reload))))

;;
;; direct references
;;

(derive ::db                ::system/var-make)
(derive ::auth-db           ::system/var-make)
(derive ::logger            ::system/var-make)
(derive ::http-server       ::system/var-make)
(derive ::http-handler      ::system/var-make)
(derive ::http-router       ::system/var-make)
(derive ::http-routes       ::system/var-make)
(derive ::http-middleware   ::system/var-make)

;;
;; url
;;

(system/add-init  ::url [k url] (var/make k (some-str url)))
(system/add-halt! ::url [k url] (var/make k nil))

;;
;; application state management
;;

(defonce ^:private ^:redef lock 'lock)

(defonce ^:redef config            nil)  ;; configuration which was read from files
(defonce ^:redef post-config       nil)  ;; configuration prepared by parser
(defonce ^:redef state             nil)  ;; current state of this application
(defonce ^:redef exception         nil)  ;; unhandled exception
(defonce ^:redef start-args         [])  ;; arguments used to start this application
(defonce ^:redef phase        :stopped)  ;; phase flag

(defn starting?    [] (locking lock (identical? :starting   phase)))
(defn failed?      [] (locking lock (identical? :failed     phase)))
(defn running?     [] (locking lock (identical? :running    phase)))
(defn stopping?    [] (locking lock (identical? :stopping   phase)))
(defn stopped?     [] (locking lock (identical? :stopped    phase)))
(defn suspended?   [] (locking lock (identical? :suspended  phase)))
(defn resuming?    [] (locking lock (identical? :resuming   phase)))
(defn suspending?  [] (locking lock (identical? :suspending phase)))
(defn configured?  [] (locking lock (some? post-config)))
(defn start-args?  [] (locking lock (not (empty? start-args))))

;;
;; application management helpers
;;

(declare start-app)
(declare restart-app)
(declare resume-app)

(defn state-from-exception
  "Takes an exception object and sets the global variable `amelinium.app/state` to
  contain the exception data extracted from it. Additionally sets the current value
  of global variable `amelinium.app/phase` to `:failed` and uses logging to notify
  about this event (with the log level set to error)."
  [ex]
  (locking lock
    (var/reset exception ex)
    (var/reset state (:system (ex-data ex)))
    (log/err "Exception during " (normalize-name phase) ": " (ex-message ex) ": " (ex-cause ex))
    (var/reset phase :failed)))

(defn configure-app
  "Configures the application using `local-config-file` and `rc-dirs` (list of relative
  paths to be scanned for EDN files with configuration maps to be merged with
  `meta-merge`).

  Uses `amelinium.system/read-configs` to load EDN files and merge them, and then
  sets the global variable `amelinium.app/config` to contain it. The next step is to
  call `amelinium.system/prep` and update the global variable
  `amelinium.app/post-config` with its result.

  If `keys` are given then `amelinium.system/config` is only updated when it does not
  yet have a truthy value, and after that the `amelinium.system/prep` is called with
  `keys` passed to only prepare values for the specified keys. The result of this
  call is stored in `amelinium.app/post-config`.

  The function returns `:configured` keyword. See also `amelinium.app/configure!`."
  ([]
   (configure-app nil nil))
  ([local-config-file rc-dirs & keys]
   (let [rc-dirs (if (coll? rc-dirs) rc-dirs (cons rc-dirs nil))]
     (locking lock
       (if-some [keys (seq keys)]
         (do (if-not config
               (var/reset config (apply system/read-configs local-config-file rc-dirs)))
             (var/reset post-config (system/prep config keys)))
         (do (if (and (nil? local-config-file) (nil? rc-dirs))
               (var/reset config (apply system/read-configs config))
               (var/reset config (apply system/read-configs local-config-file rc-dirs)))
             (var/reset post-config (system/prep config))))))
   :configured))

(defn start-app
  ""
  ([]
   (start-app nil nil))
  ([local-config-file rc-dirs & keys]
   (locking lock
     (if (suspended?)
       (apply resume-app keys)
       (try
         (if-not (configured?) (apply configure-app local-config-file rc-dirs keys))
         (var/reset start-args [local-config-file rc-dirs keys])
         (if-some [keys (seq keys)]
           (do
             (var/reset phase    :starting)
             ;;(apply configure-app local-config-file rc-dirs keys)
             (var/reset state    (system/init post-config keys))
             (var/reset phase    :running)
             (var/reset exception nil))
           (when (stopped?)
             (var/reset phase     :starting)
             (var/reset state     (system/init post-config))
             (var/reset phase     :running)
             (var/reset exception nil)))
         (catch Throwable e (state-from-exception e))))
     phase)))

(defn stop-app
  [& keys]
  (locking lock
    (if-not (stopped?)
      (try
        (var/reset phase :stopping)
        (if-some [keys (seq keys)]
          (do (if-some [s state] (system/halt! s keys))
              (var/reset state         (map/nil-existing-keys state keys))
              (var/reset exception     nil))
          (do (if-some [s state] (system/halt! s))
              (var/reset state         nil)
              (var/reset post-config   nil)
              (var/reset config        nil)
              (var/reset phase    :stopped)
              (var/reset exception     nil)))
        (catch Throwable e (state-from-exception e))))
    phase))

(defn restart-app
 [& keys]
 (locking lock
   (let [new-keys (or (seq keys) (seq (peek start-args)))]
     (apply stop-app new-keys)
     (if-some [new-args (when (seq start-args) (pop start-args))]
         (apply start-app (concat new-args new-keys))
         (apply start-app *local-config* *resource-config-dirs* new-keys))))

(defn suspend-app
  [& keys]
  (locking lock
    (if (running?)
      (try
        (var/reset phase :suspending)
        (if (seq keys) (system/suspend! state keys) (system/suspend! state))
        (var/reset phase :suspended)
        (var/reset exception nil)
        (catch Throwable e (state-from-exception e))))
    phase))

(defn resume-app
  [& keys]
  (locking lock
    (if (suspended?)
      (try
        (var/reset phase :resuming)
        (if (seq keys) (system/resume post-config state keys) (system/resume post-config state))
        (var/reset phase :running)
        (catch Throwable e (state-from-exception e)))
      (if (stopped?)
        (apply restart-app keys)))
    phase))

(defn expand-app
  [& keys]
  (locking lock
    (if (seq keys)
      (system/expand state keys)
      (system/expand state))))

;;
;; application control
;;

(defn configure!         [   ] (configure-app *local-config* *resource-config-dirs*))
(defn configure-dev!     [   ] (configure-app *local-config* *resource-dev-dirs*))
(defn configure-admin!   [   ] (configure-app *local-config* *resource-admin-dirs*))

(defn start!             [& k] (apply start-app *local-config* *resource-config-dirs* k))
(defn stop!              [& k] (apply stop-app    k))
(defn restart!           [& k] (apply restart-app k))
(defn suspend!           [& k] (apply suspend-app k))
(defn resume!            [& k] (apply resume-app  k))

(defn start-dev!         [& k] (apply start-app  *local-config* *resource-dev-dirs*    k))
(defn start-admin!       [& k] (apply start-app  *local-config* *resource-admin-dirs*  k))

(defn reload!
  "When the application is stopped, reloads code (using
  `amelinium.app/reload-namespaces`). Otherwise stops the application, reloads
  namespaces and starts it again."
  [& k]
  (if (stopped?)
    (reload-namespaces)
    (do (apply stop-app k)
        (reload-namespaces)
        (apply start-app *local-config* *resource-config-dirs* k))))

(defn print-state        [ ] (pprint state))
(defn print-config       [ ] (pprint config))
(defn print-post-config  [ ] (pprint post-config))
(defn print-start-args   [ ] (pprint start-args))

(defn cprint-state       [ ] (cprint state))
(defn cprint-config      [ ] (cprint config))
(defn cprint-post-config [ ] (cprint post-config))
(defn cprint-start-args  [ ] (cprint start-args))

;;
;; main function
;;

(defn -main []
  (start!))

;; documentation

(defdoc! config      "A nested map containing application configuration which was read from files.")
(defdoc! post-config "A nested map containing application configuration which was pre-parsed.")
(defdoc! state       "A nested map containing current state of application when it is running.")
(defdoc! exception   "Unhandled exception object thrown during starting, stopping or suspending.")

(defdoc! starting?   "Returns `true` when application is in starting phase.")
(defdoc! failed?     "Returns `true` when application is in failed phase.")
(defdoc! running?    "Returns `true` when application is in running phase.")
(defdoc! stopping?   "Returns `true` when application is in stopping phase.")
(defdoc! stopped?    "Returns `true` when application is in stopped phase.")
(defdoc! suspended?  "Returns `true` when application is in suspended phase.")
(defdoc! resuming?   "Returns `true` when application is in resuming phase.")
(defdoc! suspending? "Returns `true` when application is in suspending phase.")
(defdoc! configured? "Returns `true` when application is configured.")

(defdoc! phase
  "A keyword describing current phase (`:stopping`, `:stoppped`, `:starting`,
`:running`, `:suspended`, `:suspending`, `:resuming`, `:failed`).")

(defdoc! print-state        "Prints current state of application.")
(defdoc! print-config       "Prints current configuration (not parsed) of application.")
(defdoc! print-post-config  "Prints current, pre-parsed configuration of application.")

(defdoc! cprint-state       "Prints current state of application using `cprint`.")
(defdoc! cprint-config      "Prints current configuration (not parsed) of application using `cprint`.")
(defdoc! cprint-post-config "Prints current, pre-parsed configuration of application using `cprint`.")

(defdoc! *ns-reload-watch-dirs*
  "A sequence of directories to be watched when reloading code. Used by
  `amelinium.app/reload-namespaces` and (indirectly) by `amelinium.app/reload!` and
  `amelinium.app/make-ns-tracker`. Can also be set using
  `amelinium.app/with-watch-dirs`.")

(defdoc! *local-config*
  "A local configuration file in EDN format which will be loaded after all other
  configuration files so its entries will replace any existing entries during
  merge. Be aware that `meta-merge` is used in the process so values of nested maps
  are replaced not the whole branches. Used when `amelinium.app/configure!` is
  called. Please be aware that using this setting to override settings in certain
  environments may be considered less elastic than creating a separate, local folder
  and putting local configuration files there.")

(defdoc! *resource-config-dirs*
  "A sequence of paths (relative to the `resources` directory) to be scanned for EDN
  configuration files in regular mode of operation. Loaded in the same order as they
  appear and used by `amelinium.app/configure!`.

  Please note that when building your own instance of application you still may refer
  to the original Amelinium configs since resource directories are shared across
  loaded libraries. Therefore, it is possible to load original files (for instance
  some basic translations) and override some of them in your i18n configuration. See
  also `amelinium.app/start!` and `amelinium.app/configure!`.")

(defdoc! *resource-admin-dirs*
  "The same as `amelinium.app/*resource-config-dirs*` but loaded when application is
  run in administrative mode (and configured with `amelinium.app/configure-admin!`).

   See also `amelinium.app/start-admin!` and `amelinium.app/configure-admin!`.")

(defdoc! *resource-dev-dirs*
  "The same as `amelinium.app/*resource-config-dirs*` but loaded when application is
  run in development mode (and configured with `amelinium.app/configure-dev!`).

  See also `amelinium.app/start-dev!` and `amelinium.app/configure-dev!`.")

(defdoc! *ns-tracker*
  "Instance of `ns-tracker` used to track directories for code changes. By default it
  is initialized by calling `ns-tracker.core/ns-tracker` (from
  `amelinium.app/make-ns-tracker`) with a sequence of directories from
  `amelinium.app/*ns-reload-watch-dirs*`.")

(defdoc! configure!
  "Configures the application. Calls `amelinium.app/configure-app` passing values of
  `amelinium.app/*local-config*` and `amelinium.app/*resource-config-dirs*`. See also
  `amelinium.app/start!`.")

(defdoc! configure-dev!
  "Configures the application in development mode. Calls `amelinium.app/configure-app`
  passing it values of `amelinium.app/*local-config*` and
  `amelinium.app/*resource-dev-dirs*`.")

(defdoc! configure-admin!
  "Configures the application in administrative mode. Calls
  `amelinium.app/configure-app` passing it values of `amelinium.app/*local-config*`
  and `amelinium.app/*resource-admin-dirs*`. See also `amelinium.app/start-admin!`.")

(defdoc! start!
  "Starts or resumes the application. Calls `amelinium.app/start-app` passing values of
  `amelinium.app/*local-config*`, `amelinium.app/*resource-config-dirs*` and optional
  keys identifying components which should be configured and started. If no
  components are given, all are started. If the application is in a suspended state,
  it is resumed (see `amelinium.app/resume!`).")

(defdoc! restart!
  "Restarts the application. Calls `amelinium.app/stop-app` (passing optional keys
  identifying components which should be stopped and then started) and then calls
  `amelinium.app/start-app` passing parameters used during previous start or (if this
  is a first call to it) passing `amelinium.app/*local-config*`,
  `amelinium.app/*resource-config-dirs*` and optional keys identifying components
  to be started. If no components are given then only those which were previously
  started are stopped and then started. Application configuration is re-initialized
  during the process.")

(defdoc! stop!
  "Stops the application. Calls `amelinium.app/stop-app` passing it optional keys
  identifying components which should be stopped. If no components are given then
  only those which were previously started are stopped. After successful stop,
  application configuration is removed.")

(defdoc! suspend!
  "Suspends the application. Calls `amelinium.app/suspend-app` passing it optional keys
  identifying components which should be suspended. If no components are given then
  only those which were previously started are suspended.")

(defdoc! resume!
  "Resumes the application which was previously suspended. Calls
  `amelinium.app/resume-app` passing it optional keys identifying components which
  should be resumed. If no components are given then only those which were previously
  suspended are resumed.")

(defdoc! start-dev!
  "Starts or resumes the application using development configuration. Calls
  `amelinium.app/start-app` passing values of `amelinium.app/*local-config*`,
  `amelinium.app/*resource-dev-dirs*` and optional keys identifying components
  which should be configured and started. If no components are given, all are
  started. If the application is in suspended state, it is resumed (see
  `amelinium.app/resume!`).")

(defdoc! start-admin!
  "Starts or resumes the application using administrative configuration. Calls
  `amelinium.app/start-app` passing values of `amelinium.app/*local-config*`,
  `amelinium.app/*resource-admin-dirs*` and optional keys identifying components
  which should be configured and started. If no components are given, all are
  started. If the application is in suspended state, it is resumed (see
  `amelinium.app/resume!`).

  This mode is intended to be used when performing administrative tasks (like
  database migrations) requiring different or additional setup (e.g. specially
  configured data sources with different credentials used to create database
  connections).")
