(ns

    ^{:doc    "amelinium service, Selmer taggers."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.taggers

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string                       :as        str]
            [tick.core                            :as          t]
            [reitit.core                          :as          r]
            [selmer.parser                        :as     selmer]
            [amelinium.i18n                       :as       i18n]
            [amelinium.common                     :as     common]
            [amelinium.http.middleware.language   :as   language]
            [amelinium.http.middleware.validators :as validators]
            [amelinium.logging                    :as        log]
            [amelinium.system                     :as     system]
            [io.randomseed.utils                  :refer    :all]))

;; Template helpers

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
                  (if k-some  (map vector k-some  (repeatedly random-uuid)))
                  (if k-blank (map vector k-blank (repeat "")))
                  (if k-any   (map vector k-any   (repeatedly #(common/random-uuid-or-empty rng)))))]
     (if (seq r)
       (apply str (map #(str "<input type=\"text\" name=\""   (nth % 0)
                             "\" class=\"subspace\" value=\"" (nth % 1)
                             "\"/>")
                       r))))))

(defn get-lang
  [ctx]
  (or (get ctx :language/str)
      (some-str (get ctx :lang))
      (some-str (get ctx :language))
      (get ctx :language/default)))

(defn lang-url
  ([router ctx path-or-name lang localized? params query-params]
   (lang-url router ctx path-or-name lang localized? params query-params nil))
  ([router ctx path-or-name lang localized? params]
   (lang-url router ctx path-or-name lang localized? params nil nil))
  ([router ctx path-or-name lang localized?]
   (lang-url router ctx path-or-name lang localized? nil nil nil))
  ([router ctx path-or-name lang]
   (lang-url router ctx path-or-name lang true nil nil nil))
  ([router ctx path-or-name]
   (lang-url router ctx path-or-name nil true nil nil nil))
  ([router ctx]
   (lang-url router ctx nil nil true nil nil nil))
  ([router ctx path-or-name lang localized? params query-params lang-param]
   (let [router       (or router (get ctx ::r/router) (get ctx :router))
         lang         (or lang (get-lang ctx))
         lang-param   (or lang-param (get ctx :language/settings) (get ctx :language-param) (get ctx :param) :lang)
         path-or-name (or (valuable path-or-name) (get ctx :current-path) (common/current-page ctx))
         path-or-name (if path-or-name (selmer/render path-or-name ctx {:tag-open \[ :tag-close \]}))
         path-or-name (if (and path-or-name (str/starts-with? path-or-name ":")) (keyword (subs path-or-name 1)) path-or-name)
         path-fn      (if localized? common/localized-path common/path)
         out-path     (path-fn path-or-name lang params query-params router lang-param)
         out-path     (or out-path (if-not (ident? path-or-name) (some-str path-or-name)))]
     out-path)))

(defn add-taggers
  [router language translator validators]

  (let [lang-settings (or (get language :config) language)
        lang-param    (language/param nil lang-settings)
        validators    (or (get validators :config) validators)]

    (selmer/add-tag!
     :anti-spam-field
     (fn [args ctx]
       (anti-spam-code validators 2)))

    (selmer/add-tag!
     :lang-url
     (fn [args ctx]
       (let [path-or-name   (first args)
             args           (rest args)
             args           (if (map? (first args)) (cons nil args) args)
             [lang params
              query-params] args]
         (lang-url router ctx path-or-name lang true params query-params lang-param))))

    (selmer/add-tag!
     :link
     (fn [args ctx content]
       (let [smap            (common/session ctx)
             sid             (get smap :id)
             sfld            (get smap :session-id-field)
             path-or-name    (first args)
             args            (rest args)
             args            (if (map? (first args)) (cons nil args) args)
             [lang params
              query-params
              lang-settings] args
             out-path        (lang-url router ctx path-or-name lang false params query-params lang-param)]
         (if (and sid sfld)
           (str "<form name=\"sessionLink\" class=\"formlink\" action=\"" out-path "\" method=\"post\">"
                (anti-spam-code validators)
                "<button type=\"submit\" class=\"link\" name=\"" sfld "\" value=\"" sid "\">"
                (get-in content [:link :content])
                "</button></form>")
           (str "<a href=\"" out-path "\" class=\"link\">" (get-in content [:link :content]) "</a>"))))
     :endlink)

    (selmer/add-tag!
     :slink
     (fn [args ctx content]
       (let [url  (selmer/render (first args) ctx {:tag-open \[ :tag-close \]})
             smap (common/session ctx)
             sid  (get smap :id)
             sfld (get smap :session-id-field)]
         (if (and sid sfld)
           (str "<form name=\"sessionLink\" class=\"formlink\" action=\"" url "\" method=\"post\">"
                (anti-spam-code validators)
                "<button type=\"submit\" class=\"link\" name=\"" sfld "\" value=\"" sid "\">"
                (get-in content [:slink :content])
                "</button></form>")
           (str "<a href=\"" url  "\" class=\"link\">" (get-in content [:slink :content]) "</a>"))))
     :endslink)

    (selmer/add-tag!
     :session-data
     (fn [args ctx]
       (let [smap (common/session ctx)
             sfld (get smap :session-id-field)]
         (str (anti-spam-code validators)
              "<input type=\"hidden\" name=\"" sfld "\" value=\"" (get smap :id) "\" />"))))
    nil))

;; Configuration initializers

(defn init
  "Initializes Selmer taggers."
  [{:keys [enabled? router language translations validators]
    :or   {enabled? true}}]
  (when enabled?
    (log/msg "Initializing Selmer taggers")
    (add-taggers router language translations validators)))

(system/add-init  ::default [_ config] (init config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::all ::default)
