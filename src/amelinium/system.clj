(ns

    ^{:doc    "amelinium system."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.system

  (:refer-clojure :exclude [ref])

  (:require [integrant.core           :as         ig]
            [maailma.core             :as       conf]
            [cambium.core             :as        log]
            [amelinium                :as  amelinium]
            [amelinium.readers        :as    readers]
            [amelinium.env.file       :as    envfile]
            [tick.core                :as          t]
            [clojure.java.io          :as         io]
            [clojure.string           :as        str]
            [io.randomseed.utils      :as      utils]
            [io.randomseed.utils.var  :as        var]
            [io.randomseed.utils.fs   :as         fs])

  (:import (java.util TimeZone)))

;; integrant wrappers

(defmacro add-init        [& more] `(defmethod ig/init-key     ~@more))
(defmacro add-expand      [& more] `(defmethod ig/expand-key   ~@more))
(defmacro add-suspend!    [& more] `(defmethod ig/suspend-key! ~@more))
(defmacro add-resume      [& more] `(defmethod ig/resume-key   ~@more))
(defmacro add-resolve     [& more] `(defmethod ig/resolve-key  ~@more))
(defmacro add-halt!       [& more] `(defmethod ig/halt-key!    ~@more))

(defmacro init-key        [& more] `(ig/init-key     ~@more))
(defmacro expand-key      [& more] `(ig/expand-key   ~@more))
(defmacro suspend-key!    [& more] `(ig/suspend-key! ~@more))
(defmacro resume-key      [& more] `(ig/resume-key   ~@more))
(defmacro resolve-key     [& more] `(ig/resolve-key  ~@more))
(defmacro halt-key!       [& more] `(ig/halt-key!    ~@more))

(defmacro ref             [& more] `(ig/ref          ~@more))
(defmacro refset          [& more] `(ig/refset       ~@more))

(defmacro add-prep
  [dispatch-value argvec & body]
  (let [k#    (gensym "k")
        v#    (gensym "v")
        kform (nth argvec 0 '_)
        vform (nth argvec 1 '_)]
    `(defmethod ig/expand-key ~dispatch-value [~k# ~v#]
       (let [~kform ~k#
             ~vform ~v#
             prepared# (do ~@body)]
         {~k# prepared#}))))

(defmacro prep-key
  [k v]
  `(let [k# ~k
         m# (ig/expand-key k# ~v)]
     (when-not (map? m#)
       (throw (ex-info "ig/expand-key must return a map" {:k k# :returned m#})))
     (when-not (contains? m# k#)
       (throw (ex-info "ig/expand-key did not produce value for key" {:k k# :returned m#})))
     (get m# k#)))

(defn expand
  ([cfg]
   (expand cfg identity nil))
  ([cfg innerf]
   (expand cfg innerf nil))
  ([cfg innerf ks]
   (let [csrc     (get cfg ::config-sources)
         ksrc     (get cfg ::keys)
         pure-cfg (dissoc cfg ::keys ::config-sources)
         ks       (or (seq ks) (keys cfg))]
       (assoc (ig/expand pure-cfg innerf ks) ::keys ksrc ::config-sources csrc))))

(defn prep
  ([cfg]
   (expand cfg))
  ([cfg keys]
   (expand cfg identity keys)))

(defn init
  ([cfg]
   (init cfg nil))
  ([cfg ks]
   (let [csrc     (get cfg ::config-sources)
         ksrc     (get cfg ::keys)
         pure-cfg (dissoc cfg ::keys ::config-sources)
         ks       (or (seq ks) (keys cfg))]
     (assoc (ig/init pure-cfg ks) ::keys ksrc ::config-sources csrc))))

(defn suspend!
  ([cfg]
   (suspend! cfg nil))
  ([cfg ks]
   (let [pure-cfg (dissoc cfg ::keys ::config-sources)]
     (if-some [ks (seq ks)]
       (ig/suspend! pure-cfg ks)
       (ig/suspend! pure-cfg)))))

(defn resume
  ([cfg system]
   (resume cfg system nil))
  ([cfg system ks]
   (let [csrc     (get cfg ::config-sources)
         ksrc     (get cfg ::keys)
         pure-cfg (dissoc cfg    ::keys ::config-sources)
         pure-sys (dissoc system ::keys ::config-sources)
         new-sys  (if (seq ks) (ig/resume pure-cfg pure-sys ks) (ig/resume pure-cfg pure-sys))]
     (assoc new-sys ::config-sources csrc ::keys ksrc))))

(defn halt!
  ([cfg]
   (halt! cfg nil))
  ([cfg ks]
   (let [pure-cfg (dissoc cfg ::keys ::config-sources)]
     (if-some [ks (seq ks)]
       (ig/halt! pure-cfg keys)
       (ig/halt! pure-cfg)))))

(defn ref?
  [v]
  (ig/ref? v))

;; var-object pre-processing (allows to dereference Vars by symbols or keywords);
;; functions (symbols/keywords in lists) will be called in the init phase

(defn expand-var-process [k v] {k (var/resolve v)})
(defn prep-var-process   [  v]    (var/resolve v))
(defn init-var-process   [  v]    (var/deref   v))

;;
;; configuration loading
;;

;; readers for a custom tags #ref and #refset
;; that can be placed in configuration file to reference
;; other keys or sets of keys

(defn- validate-ref
  [ref]
  (if-not (qualified-keyword? ref)
    (ex-info (str "Invalid reference: " ref ". Must be a qualified keyword.")
             {:reason ::invalid-ref, :ref ref})))

(def ^:private integrant-readers
  {:readers {'ref    ig/ref
             'refset ig/refset
             're     readers/regex}})

;; parsing configuration files and returning a merged map
;; for the given profile

(defn conf-resource
  ([r]
   (if r (conf/resource r integrant-readers)))
  ([r & more]
   (->> (cons r more)
        (map conf-resource)
        (filter identity)
        seq)))

(defn conf-file
  [f]
  (if f (conf/file f integrant-readers)))

(defn slurp-resource-or-file
  [p]
  (when-some [p (not-empty (str p))]
    (if-let [r (io/resource p)]
      (slurp r)
      (slurp p))))

(defn edn-path?
  [p]
  (and p (str/ends-with? (str p) ".edn")))

(defn env-path?
  [p]
  (and p (str/ends-with? (str p) ".env")))

(defn conf-dirs->resource-names
  ([d]
   (some->> d
            fs/resource-file
            file-seq
            (map fs/basename)
            (filter (some-fn edn-path? env-path?))
            (sort)
            (map (comp utils/some-str (partial io/file (str d))))))
  ([d & more]
   (some->> (cons d more)
            (map conf-dirs->resource-names)
            (map seq)
            (filter identity)
            (apply concat)
            seq)))

;; loading namespaces required by fully-qualified configuration keys

(defn load-with-namespaces
  "Returns the given config, loading any detected namespaces with
  `integrant.core/load-namespaces`."
  [config]
  (ig/load-namespaces config)
  config)

;; selecting subsystem(s) from a global configuration map

(defn subsystems
  "Selects subsystems from the given configuration map `config` by selecting 1st-level
  branch keys. Returns a map."
  ([config]      config)
  ([config keys] (select-keys config keys)))

;; getting system configuration from file(s)

(defn read-configs
  "Reads configuration files in EDN or ENV format. For 2 or more arguments
  it loads `local-file` from a filesystem (unless it's `nil`) and scans all resource
  directories specified as other arguments. For each directory it tries to find
  filenames ending with `.edn` or `.env` and loads them all in order. The local
  file is being loaded last.

  The function returns a single configuration map merged from all loaded maps (EDN-sourced)
  with a special key ::config-. The
  configuration sources are preserved in this map under a key
  `:amelinium.app/config-sources`, containing the following keys: `:resource-dirs`,
  `:resource-files` and `:local-file`.

  When there is only 1 argument given and it is a map then it should be a valid
  config with `:amelinium.app/config-sources` key present. The associated map will be
  then used as a sources list of loaded configuration.

  When there is only 1 argument given and it is not a map then it should be a
  sequential collection of resource directories to scan and load configuration
  from. In this case the local configuration file is considered to be `nil`."
  ([resource-config-dir-or-map]
   (if-not (map? resource-config-dir-or-map)
     (read-configs nil resource-config-dir-or-map)
     (let [config-sources   (::config-sources resource-config-dir-or-map)
           local-file       (:local-file config-sources)
           resource-dirs    (seq (filter identity (:resource-dirs  config-sources)))
           resource-files   (seq (filter identity (:resource-files config-sources)))
           resource-files   (or resource-files
                                (apply conf-dirs->resource-names resource-dirs))
           config-sources   (assoc config-sources
                                   :resource-dirs resource-dirs
                                   :resource-files resource-files)
           edn-resources    (seq (filter edn-path? resource-files))
           env-resources    (seq (filter env-path? resource-files))
           edn-file         (when (edn-path? local-file) local-file)
           env-file         (when (and (nil? edn-file) (env-path? local-file)) local-file)
           edn-res-confs    (when edn-resources (apply conf-resource edn-resources))
           edn-file-confs   (some-> edn-file not-empty conf-file (cons '()))
           env-file-confs   (some-> env-file not-empty (cons '()))
           edn-configs      (concat edn-res-confs edn-file-confs)
           env-configs      (concat env-resources env-file-confs)
           edn-config-data  (some->> edn-configs seq
                                     (apply conf/build-config)
                                     load-with-namespaces)
           env-config-data  (some->> env-configs seq (apply envfile/parse))]
       (envfile/derive-keys! env-config-data)
       (-> (merge edn-config-data env-config-data)
           (assoc ::config-sources config-sources)))))
  ([local-file resource-config-dir & more-dirs]
   (read-configs
    {::config-sources {:local-file    local-file
                       :resource-dirs (cons resource-config-dir more-dirs)}})))

;; initialization shortcuts

(add-init   ::key         [k v] k)
(add-init   ::function    [k v] (v k))
(add-init   ::nil         [_ _] nil)
(add-init   ::var         [_ v] (init-var-process v))
(add-expand ::prepped-var [k v] (expand-var-process k v))
(add-init   ::prepped-var [_ v] (init-var-process v))
(add-init   ::value       [_ v] v)
(add-halt!  ::value       [_ v] v)
(add-init   ::var-make    [k v] (var/make k v))
(add-halt!  ::var-make    [k _] (var/make k nil))

;; properties shortcut

(derive ::properties ::value)

;; time zone

(add-init
 ::timezone
 [_ tz]
 (let [tz (utils/valuable tz)
       tz (or (:timezone-id tz) tz)
       tz (if (or (string? tz) (ident? tz)) (utils/some-str tz) tz)]
   (if tz
     (let [^TimeZone tz (if (true? tz)
                          (TimeZone/getDefault)
                          (let [^TimeZone tznew (TimeZone/getTimeZone ^String (str tz))]
                            (TimeZone/setDefault ^TimeZone tznew) tznew))
           tz-id        (utils/some-str (.getID ^TimeZone tz))]
       (log/info (str "Setting default time zone to " tz-id))
       {:timezone    tz
        :timezone-id tz-id
        :zone-region (t/zone tz-id)}))))
