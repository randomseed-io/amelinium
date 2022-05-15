(ns

    ^{:doc    "amelinium service, application."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

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
(def ^:dynamic *local-dev-config*                   "config.edn")
(def ^:dynamic *resource-config-dirs* ["config/amelinium"
                                       "translations/amelinium"])
(def ^:dynamic *resource-admin-dirs*  ["config/amelinium"
                                       "config/amelinium-admin"])

(defmacro with-config-dirs
  [dirs & body]
  `(binding [*resource-config-dirs* ~dirs]
     ~@body))

(defmacro with-local-config
  [local-file & body]
  `(binding [*local-config* ~local-file]
     ~@body))

(defmacro with-configs
  [local-file dirs & body]
  `(binding [*resource-config-dirs* ~dirs
             *local-config*         ~local-file]
     ~@body))

(defmacro with-watch-dirs
  [watch-dirs & body]
  `(binding [*ns-reload-watch-dirs* ~watch-dirs]
     ~@body))

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
                  (update :profile     normalize-name "unknown")
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

(def modified-namespaces
  (ns-tracker/ns-tracker *ns-reload-watch-dirs*))

(defn check-modified-namespaces
  []
  (doseq [ns-sym (modified-namespaces)]
    (require ns-sym :reload)))

(def ns-reload check-modified-namespaces)

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
;; application state management
;;

(defonce ^:private lock 'lock)

(defonce config            nil)  ;; configuration which was read from files
(defonce post-config       nil)  ;; configuration prepared by parser
(defonce state             nil)  ;; current state of this application
(defonce exception         nil)  ;; unhandled exception
(defonce phase        :stopped)  ;; phase flag

(defn starting?    [] (locking lock (= :starting   phase)))
(defn failed?      [] (locking lock (= :failed     phase)))
(defn running?     [] (locking lock (= :running    phase)))
(defn stopping?    [] (locking lock (= :stopping   phase)))
(defn stopped?     [] (locking lock (= :stopped    phase)))
(defn suspended?   [] (locking lock (= :suspended  phase)))
(defn resuming?    [] (locking lock (= :resuming   phase)))
(defn suspending?  [] (locking lock (= :suspending phase)))
(defn configured?  [] (locking lock (some? post-config)))

;;
;; application management helpers
;;

(declare start-app)
(declare resume-app)

(defn state-from-exception
  [ex]
  (locking lock
    (var/reset exception ex)
    (var/reset state (:system (ex-data ex)))
    (log/err "Exception during " (normalize-name phase) ": " (ex-message ex) ": " (ex-cause ex))
    (var/reset phase :failed)))

(defn configure-app
  ([]
   (configure-app nil nil))
  ([local-config-file rc-dirs & keys]
   (let [rc-dirs (if (coll? rc-dirs) rc-dirs (cons rc-dirs nil))]
     (locking lock
       (if-some [keys (seq keys)]
         (do (when-not config
               (var/reset config (apply system/read-configs local-config-file rc-dirs)))
             (var/reset post-config (system/prep config keys)))
         (do (if (and (nil? local-config-file) (nil? rc-dirs))
               (var/reset config (apply system/read-configs config))
               (var/reset config (apply system/read-configs local-config-file rc-dirs)))
             (var/reset post-config (system/prep config))))))
   :configured))

(defn start-app
  ([]
   (start-app nil nil))
  ([local-config-file rc-dirs & keys]
   (locking lock
     (if (suspended?)
       (apply resume-app keys)
       (try
         (when-not (configured?)
           (apply configure-app local-config-file rc-dirs keys))
         (if-some [keys (seq keys)]
           (do
             (var/reset phase :starting)
             ;;(apply configure-app local-config-file rc-dirs keys)
             (var/reset state (system/init post-config keys))
             (var/reset phase :running)
             (var/reset exception  nil))
           (when (stopped?)
             (var/reset phase :starting)
             (var/reset state (system/init post-config))
             (var/reset phase :running)
             (var/reset exception  nil)))
         (catch Throwable e (state-from-exception e))))
     phase)))

(defn stop-app
  [& keys]
  (locking lock
    (when-not (stopped?)
      (try
        (var/reset phase :stopping)
        (if-some [keys (seq keys)]
          (do (when-some [s state] (system/halt! s keys))
              (var/reset state         (map/nil-existing-keys state keys))
              (var/reset exception     nil))
          (do (when-some [s state] (system/halt! s))
              (var/reset state         nil)
              (var/reset post-config   nil)
              (var/reset config        nil)
              (var/reset phase    :stopped)
              (var/reset exception     nil)))
        (catch Throwable e (state-from-exception e))))
    phase))

(defn suspend-app
  [& keys]
  (locking lock
    (when (running?)
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
      (when (stopped?)
        (apply start-app nil nil keys)))
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
(defn configure-dev!     [   ] (configure-app *local-dev-config* *resource-config-dirs*))
(defn configure-admin!   [   ] (configure-app *local-config* *resource-admin-dirs*))

(defn start!             [& k] (apply start-app   *local-config* *resource-config-dirs* k))
(defn restart!           [& k] (apply stop-app    k) (apply start-app *local-config* *resource-config-dirs* k))
(defn stop!              [& k] (apply stop-app    k))
(defn suspend!           [& k] (apply suspend-app k))
(defn resume!            [& k] (apply resume-app  *local-config* *resource-config-dirs* k))
(defn start-dev!         [& k] (apply start-app *local-dev-config* *resource-config-dirs* k))
(defn start-admin!       [& k] (apply start-app *local-config* *resource-admin-dirs* k))

(defn reload!
  [& k]
  (if (stopped?)
    (ns-reload)
    (do (apply stop-app k)
        (ns-reload)
        (apply start-app *local-config* *resource-config-dirs* k))))

(defn print-state        [ ] (pprint (state)))
(defn print-config       [ ] (pprint (config)))
(defn print-post-config  [ ] (pprint (post-config)))

(defn cprint-state       [ ] (cprint (state)))
(defn cprint-config      [ ] (cprint (config)))
(defn cprint-post-config [ ] (cprint (post-config)))

;;
;; main function
;;

(defn -main []
  (start!))
