(ns

    ^{:doc    "App/data processing for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.app-data

  (:refer-clojure :exclude [parse-long uuid random-uuid]
                  :rename  {get get-core update update-core assoc assoc-core})

  (:require [amelinium.i18n                       :as                       i18n]
            [amelinium.common                     :as                     common]
            [io.randomseed.utils.map              :as map :refer        [qassoc]]
            [io.randomseed.utils                  :refer          [some-keyword]]
            [io.randomseed.lazy-map               :as                   lazy-map]
            [amelinium.utils                      :refer         [try-name
                                                                  try-namespace
                                                                  empty-lazy-map
                                                                  map-to-lazy]])

  (:import (io.randomseed.lazy_map LazyMapEntry)))

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

;; Handling :app/data

(defn get-missing-from-req
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

(defn disable
  "Disables processing of the `:app/data` key for the given request `req` by
  associating it with the `false` value."
  [req]
  (qassoc req :app/data false))

(defn disabled?
  "Returns `true` when the value associated with `:app/data` in `req` is `false`."
  [req]
  (false? (get-core req :app/data)))

(defn get
  "Gets the value of `:app/data` for the current request. If it does not exist or it is
  `nil`, returns an empty lazy map. Otherwise it returns the unmodified value. If it
  is a map but not a lazy map, converts it to a lazy map."
  [req]
  (if-some [m (get-core req :app/data)]
    (map-to-lazy m)
    empty-lazy-map))

(defn prep
  "Prepares data for the rendering functions by copying the given values associated
  with the given keys from `req` to a lazy map under `:app/data` key of the
  `req`. The list of keys to copy must be as a sequential collection, explicitly
  given as `keyz`, or reside under `:app/data-required` of the `req`. If there
  already is `:app/data` in the request map then it will be used as the initial value
  of the created data map. Data for existing keys will not be copied."
  ([req]
   (prep req nil nil))
  ([req data]
   (prep req data nil))
  ([req data keyz]
   (if (false? data)
     req
     (let [req-data (get-core req :app/data)]
       (if (false? req-data)
         req
         (let [req-data (when req-data (map/to-lazy req-data))
               data     (if req-data (map/merge-lazy req-data data) (map/to-lazy data))
               keyz     (or keyz (concat common-keys (get-core req :app/data-required)))]
           (if (and data (pos? (count data)))
             (get-missing-from-req data req keyz)
             (map/select-keys-lazy req keyz))))))))

(defmacro add
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

(defn update
  "Updates the `:app/data` in a request map `req` with a result of calling the function
  `f` on the previous value and optional arguments. Uses
  `io.randomseed.utils.map/qassoc`.

  Returns updated `req` with a lazy map under `:add/data` key. Ensures that the first
  argument of calling `f` is a map from `req` (which, by convenience, should be a
  lazy map, `nil` or `false`), and a result of calling `f` is also a lazy map (and if
  it is not, tries to convert it to a lazy map).

  When the current value of `:app/data` is `nil` it will create an empty lazy map.
  When the current value of `:app/data` is `false` it will short-circuit and skip any
  updating."
  ([req]
   (add req))
  ([req f]
   (let [ad (get req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad))))))
  ([req f a]
   (let [ad (get req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad a))))))
  ([req f a b]
   (let [ad (get req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad a b))))))
  ([req f a b c]
   (let [ad (get req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad a b c))))))
  ([req f a b c d]
   (let [ad (get req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (f ad a b c d))))))
  ([req f a b c d & more]
   (let [ad (get req)]
     (if (false? ad) req (qassoc req :app/data (map-to-lazy (apply f ad a b c d more)))))))

(defmacro assoc
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
            apd# (get req#)]
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
              apd# (get req#)]
          (if (false? apd#) req# (qassoc req# :app/data (qassoc apd# ~@pairs))))
       `(let [req# ~req
              apd# (get req#)]
          (if (false? apd#)
            req#
            (qassoc req# :app/data
                    (if (pos? (count apd#))
                      (qassoc apd# ~@pairs)
                      (lazy-map/->LazyMap {~@pairs ~@[]})))))))))

(defn update-status
  "Updates `:app/data` map of the `req` by setting its status key `status-key` to
  `status`, `title-key` to translated `status` and `description-key` to translated
  message obtained for a translation key made with namespace of `status` and name of
  `status` with `.full` attached. If `status` is untranslatable (is not an ident nor
  a string), it will just associate `status-key` with the given `status`.

  Any existing entries of `:app/data` having the same keys as given (`status-key`,
  `title-key` and `description-key`) will remain unmodified."
  ([req status lang status-key title-key description-key]
   (if status
     (->> (update-status (get-core req :app/data empty-lazy-map) req status lang status-key title-key description-key)
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
