(ns

    ^{:doc    "amelinium system."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.system

  (:refer-clojure :exclude [ref])

  (:require [integrant.core           :as         ig]
            [maailma.core             :as       conf]
            [cambium.core             :as        log]
            [amelinium                :as  amelinium]
            [tick.core                :as          t]
            [clojure.java.io          :as         io]
            [clojure.string           :as        str]
            [io.randomseed.utils      :as      utils]
            [io.randomseed.utils.var  :as        var]
            [io.randomseed.utils.fs   :as         fs])

  (:import [java.util TimeZone]))

;; integrant wrappers

(defmacro add-init        [& more] `(defmethod ig/init-key     ~@more))
(defmacro add-prep        [& more] `(defmethod ig/prep-key     ~@more))
(defmacro add-suspend!    [& more] `(defmethod ig/suspend-key! ~@more))
(defmacro add-resume      [& more] `(defmethod ig/resume-key   ~@more))
(defmacro add-resolve     [& more] `(defmethod ig/resolve-key  ~@more))
(defmacro add-halt!       [& more] `(defmethod ig/halt-key!    ~@more))

(defmacro init-key        [& more] `(ig/init-key     ~@more))
(defmacro prep-key        [& more] `(ig/prep-key     ~@more))
(defmacro suspend-key!    [& more] `(ig/suspend-key! ~@more))
(defmacro resume-key      [& more] `(ig/resume-key   ~@more))
(defmacro resolve-key     [& more] `(ig/resolve-key  ~@more))
(defmacro halt-key!       [& more] `(ig/halt-key!    ~@more))

(defmacro ref             [& more] `(ig/ref          ~@more))
(defmacro refset          [& more] `(ig/refset       ~@more))

(defn prep
  ([cfg]
   (prep cfg nil))
  ([cfg keys]
   (if-some [keys (or (seq keys) (seq (::keys cfg)))]
     (assoc (ig/prep (dissoc cfg ::keys ::config-sources) keys) ::keys keys)
     (ig/prep cfg))))

(defn init
  ([cfg]
   (init cfg nil))
  ([cfg keys]
   (if-some [keys (or (seq keys) (seq (::keys cfg)))]
     (assoc (ig/init (dissoc cfg ::keys ::config-sources) keys) ::keys keys)
     (ig/init cfg))))

(defn suspend!
  ([cfg]
   (suspend! cfg nil))
  ([cfg keys]
   (if-some [keys (seq keys)]
     (ig/suspend! (dissoc cfg ::keys) keys)
     (ig/suspend! (dissoc cfg ::keys)))))

(defn resume
  ([cfg system]
   (resume cfg system nil))
  ([cfg system keys]
   (if-some [keys (seq keys)]
     (ig/resume (dissoc cfg ::keys) (dissoc system ::keys) keys)
     (ig/resume (dissoc cfg ::keys) (dissoc system ::keys)))))

(defn halt!
  ([cfg]
   (halt! cfg nil))
  ([cfg keys]
   (if-some [keys (seq keys)]
     (ig/halt! (dissoc cfg ::keys) keys)
     (ig/halt! (dissoc cfg ::keys)))))

(defn expand
  [cfg]
  (ig/expand (dissoc cfg ::keys)))

(defn ref?
  [v]
  (ig/ref? v))

;; var-object pre-processing (allows to dereference Vars by symbols or keywords);
;; functions (symbols/keywords in lists) will be called in the init phase

(defn prep-var-process [v] (var/resolve v))
(defn init-var-process [v] (var/deref   v))

;;
;; configuration loading
;;

;; readers for a custom tags #ref and #refset
;; that can be placed in configuration file to reference
;; other keys or sets of keys

(defn- validate-ref
  [ref]
  (when-not (qualified-keyword? ref)
    (ex-info (str "Invalid reference: " ref ". Must be a qualified keyword.")
             {:reason ::invalid-ref, :ref ref})))

(defn- regex-reader
  [rgx]
  (re-pattern rgx))

(def ^:private integrant-readers
  {:readers {'ref    ig/ref
             'refset ig/refset
             're     regex-reader}})

;; parsing configuration files and returning a merged map
;; for the given profile

(defn conf-resource
  ([r]
   (when r
     (conf/resource r integrant-readers)))
  ([r & more]
   (->> (cons r more)
        (map conf-resource)
        (filter identity)
        seq)))

(defn conf-file
  [f]
  (when f
    (conf/file f integrant-readers)))

(defn conf-dirs->resource-names
  ([d]
   (some->> d
            fs/resource-file
            file-seq
            (map fs/basename)
            (filter #(str/ends-with? % ".edn"))
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
  ([config]      config)
  ([config keys] (select-keys config keys)))

;; getting system configuration from file(s)

(defn read-configs
  "Reads configuration files in EDN format. For 2 or more arguments it loads
  `local-file` from a filesystem (unless it's `nil`) and scans all resource
  directories specified as other arguments. For each directory it tries to find
  filenames ending with `.edn` and loads them all in order. The local file is being
  loaded last.

  The function returns a single configuration map merged from all loaded maps. The
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
           file-conf        (when local-file (conf-file local-file))
           file-confs       (when file-conf (cons file-conf nil))
           resource-dirs    (seq (filter identity (:resource-dirs  config-sources)))
           resource-files   (seq (filter identity (:resource-files config-sources)))
           resource-files   (or resource-files (apply conf-dirs->resource-names resource-dirs))
           config-sources   (assoc config-sources :resource-dirs resource-dirs :resource-files resource-files)
           resource-confs   (apply conf-resource resource-files)
           configs-to-build (concat resource-confs file-confs)]
       (-> (apply conf/build-config configs-to-build)
           load-with-namespaces
           (assoc ::config-sources config-sources)))))
  ([local-file resource-config-dir & more-dirs]
   (read-configs
    {::config-sources {:local-file    local-file
                       :resource-dirs (cons resource-config-dir more-dirs)}})))

;; initialization shortcuts

(add-init  ::key         [k v] k)
(add-init  ::function    [k v] (v k))
(add-init  ::nil         [_ _] nil)
(add-init  ::var         [_ v] (init-var-process v))
(add-prep  ::prepped-var [_ v] (prep-var-process v))
(add-init  ::prepped-var [_ v] (init-var-process v))
(add-init  ::value       [_ v] v)
(add-halt! ::value       [_ v] v)
(add-init  ::var-make    [k v] (var/make k v))
(add-halt! ::var-make    [k _] (var/make k nil))

;; properties shortcut

(derive ::properties ::value)

;; time zone

(add-init
 ::timezone
 [_ tz]
 (let [tz (utils/valuable tz)
       tz (or (:timezone-id tz) tz)
       tz (if (or (string? tz) (ident? tz)) (utils/some-str tz) tz)]
   (when tz
     (let [^TimeZone tz (if (true? tz)
                          (TimeZone/getDefault)
                          (let [^TimeZone tznew (TimeZone/getTimeZone ^String (str tz))]
                            (TimeZone/setDefault ^TimeZone tznew) tznew))
           tz-id        (utils/some-str (.getID ^TimeZone tz))]
       (log/info (str "Setting default time zone to " tz-id))
       {:timezone    tz
        :timezone-id tz-id
        :zone-region (t/zone tz-id)}))))
