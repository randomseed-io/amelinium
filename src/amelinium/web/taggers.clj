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
            [selmer.util                          :as      sutil]
            [selmer.filter-parser                 :as         fp]
            [amelinium.i18n                       :as       i18n]
            [amelinium.common                     :as     common]
            [amelinium.http.middleware.session    :as    session]
            [amelinium.http.middleware.language   :as   language]
            [amelinium.http.middleware.validators :as validators]
            [amelinium.http.middleware.coercion   :as   coercion]
            [amelinium.logging                    :as        log]
            [amelinium.system                     :as     system]
            [io.randomseed.utils.map              :as        map]
            [io.randomseed.utils                  :refer    :all])

  (:import [java.net URLEncoder]))

;; Template helpers

(defn url-enc
  ^String [^String s]
  (URLEncoder/encode s))

(defn url-esc
  ^String [^String s]
  (let [slen              (unchecked-int (count s))
        ^StringBuilder sb (StringBuilder. slen)]
    (loop [idx (unchecked-int 0)]
      (if (>= idx slen)
        (.toString sb)
        (let [c (char (.charAt s idx))]
          (case c
            \< (.append sb "%3C")
            \> (.append sb "%3E")
            \" (.append sb "%22")
            \' (.append sb "%27")
            (.append sb c))
          (recur (inc idx))))))
  s)

(defn html-esc
  [v]
  (fp/escape-html* v))

(defn assignments->map
  [^String s]
  (if (and s (seq s))
    (->> (str/split s #"\,")
         (mapcat #(map str/trim (str/split (str/trim %) #"\=")))
         (apply array-map)
         (not-empty))))

(defn assignments->kw-map
  [^String s]
  (if (and s (seq s))
    (->> (str/split s #"\,")
         (mapcat #(map str/trim (str/split (str/trim %) #"\=")))
         (map #(%1 %2) (cycle [common/keyword-from-param identity]))
         (apply array-map)
         (not-empty))))

(defn parse-args
  [args]
  (fp/fix-filter-args args))

(defn args->map
  [args]
  (if (seq args)
    (apply array-map (map #(%1 %2) (cycle [common/keyword-from-param identity]) args))))

(defn strb
  (^String [^Object a]
   (if a (.toString ^Object a) ""))
  (^String [^String a ^String b]
   (.toString ^StringBuilder (.append ^StringBuilder (StringBuilder. (or a "")) (or b ""))))
  (^String [^String a ^String b ^String c]
   (.toString
    ^StringBuilder (doto (StringBuilder. (or a ""))
                     (.append (or b ""))
                     (.append (or c "")))))
  (^String [^String a ^String b ^String c ^String d]
   (.toString
    ^StringBuilder (doto (StringBuilder. (or a ""))
                     (.append (or b ""))
                     (.append (or c ""))
                     (.append (or d "")))))
  (^String [^String a ^String b ^String c ^String d ^String e]
   (.toString
    ^StringBuilder (doto (StringBuilder. (or a ""))
                     (.append (or b ""))
                     (.append (or c ""))
                     (.append (or d ""))
                     (.append (or e "")))))
  (^String [^String a ^String b ^String c ^String d ^String e & more]
   (.toString
    ^StringBuilder (doto (StringBuilder. (or a ""))
                     (.append (or b ""))
                     (.append (or c ""))
                     (.append (or d ""))
                     (.append (or e ""))
                     (.append (apply strb more))))))

(defmacro strs
  ([]
   "")
  ([a]
   (if (string? a) `~a `(strb ~a)))
  ([a & more]
   `(strb ~@(->> (cons a more)
                 (partition-by string?)
                 (mapcat #(if (string? (first %)) (cons (apply strb %) nil) %))))))

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
       (apply strb (map #(strb "<input type=\"text\" name=\""   (str (nth % 0))
                               "\" class=\"subspace\" value=\"" (str (nth % 1))
                               "\"/>\n")
                        r))))))

(defn get-lang
  [ctx]
  (or (get ctx :language/str)
      (some-str (get ctx :lang))
      (some-str (get ctx :language))
      (get ctx :language/default)))

(defn get-lang-id
  [ctx]
  (or (get ctx :language/id)
      (some-keyword-simple (get ctx :lang))
      (some-keyword-simple (get ctx :language))
      (get ctx :language/default)))

(defn lang-url
  ([router ctx path-or-name lang localized? path-params query-params]
   (lang-url router ctx path-or-name lang localized? path-params query-params nil))
  ([router ctx path-or-name lang localized? path-params]
   (lang-url router ctx path-or-name lang localized? path-params nil nil))
  ([router ctx path-or-name lang localized?]
   (lang-url router ctx path-or-name lang localized? nil nil nil))
  ([router ctx path-or-name lang]
   (lang-url router ctx path-or-name lang true nil nil nil))
  ([router ctx path-or-name]
   (lang-url router ctx path-or-name nil true nil nil nil))
  ([router ctx]
   (lang-url router ctx nil nil true nil nil nil))
  ([router ctx path-or-name lang localized? path-params query-params lang-param]
   (let [lang         (or lang (get-lang ctx))
         lang-param   (or lang-param (get ctx :language/settings) (get ctx :language-param) (get ctx :param) :lang)
         path-or-name (or (valuable path-or-name) (get ctx :current-path) (common/current-page ctx))]
     (if-some [out-path (common/lang-url router ctx path-or-name lang localized? path-params query-params lang-param)]
       (url-esc out-path)))))

(defn translator
  ([ctx]
   (translator ctx nil))
  ([ctx translations-fn]
   (or (get ctx (if i18n/*handle-missing-keys* :i18n/translator :i18n/translator-nd))
       (let [tf   (or translations-fn i18n/translations)
             lang (get-lang-id ctx)]
         (fn
           ([k]       (i18n/translate-with tf lang k))
           ([k a]     (i18n/translate-with tf lang k a))
           ([k a b]   (i18n/translate-with tf lang k a b))
           ([k a b c] (i18n/translate-with tf lang k a b c))
           ([k a b c & more] (apply i18n/translate-with tf lang k a b c more)))))))

(defn translator-sub
  ([ctx]
   (translator-sub ctx nil))
  ([ctx translations-fn]
   (or (get ctx (if i18n/*handle-missing-keys* :i18n/translator-sub :i18n/translator-sub-nd))
       (let [tf   (or translations-fn i18n/translations)
             lang (get-lang-id ctx)]
         (fn
           ([k]         (i18n/translate-sub-with tf lang k))
           ([k a]       (i18n/translate-sub-with tf lang k a))
           ([k a b]     (i18n/translate-sub-with tf lang k a b))
           ([k a b c]   (i18n/translate-sub-with tf lang k a b c))
           ([k a b c d] (i18n/translate-sub-with tf lang k a b c d))
           ([k a b c d & more] (apply i18n/translate-sub-with tf lang k a b c d more)))))))

(defn tr
  ([args ctx]
   (tr args ctx nil))
  ([args ctx translations-fn]
   (if-some [translator (translator ctx translations-fn)]
     (apply translator (common/keyword-from-param (first args)) (next args)))))

(defn tr-sub
  ([args ctx]
   (tr-sub args ctx nil))
  ([args ctx translations-fn]
   (if-some [translator-sub (translator-sub ctx translations-fn)]
     (apply translator-sub
            (common/keyword-from-param (first  args))
            (common/keyword-from-param (second args))
            (nnext args)))))

(defn kw-param?
  [v]
  (or (keyword? v)
      (and (string? v)
           (pos? (count ^String v))
           (= \: (.charAt ^String v 0)))))

(defn param-try-tr
  ([tr-sub v]
   (if v
     (if (kw-param? v)
       (if-some [v (common/keyword-from-param v)]
         ((force tr-sub) v))
       (not-empty (str v)))))
  ([tr-sub k v]
   (if v
     (if (kw-param? v)
       (if-some [v (common/string-from-param v)]
         ((force tr-sub) (some-str k) v))
       (not-empty (str v)))))
  ([tr-sub k v a]
   (if v
     (if (kw-param? v)
       (if-some [v (common/string-from-param v)]
         ((force tr-sub) (some-str k) v a))
       (not-empty (str v)))))
  ([tr-sub k v a b]
   (if v
     (if (kw-param? v)
       (if-some [v (common/string-from-param v)]
         ((force tr-sub) (some-str k) v a b))
       (not-empty (str v)))))
  ([tr-sub k v a b & more]
   (if v
     (if (kw-param? v)
       (if-some [v (common/string-from-param v)]
         (apply (force tr-sub) (some-str k) v a b more))
       (not-empty (str v))))))

(defn form-field
  [args tr-sub errors params]
  (let [field (args->map args)
        id    (get field :id)]
    (if-some [id-kw (common/keyword-from-param id)]
      (let [{:keys
             [name
              label
              type
              value
              placeholder
              input-type]} field
            value?         (contains? params id-kw)
            error?         (contains? errors id-kw)
            id-str         (common/string-from-param id)
            name           (common/string-from-param name)
            type           (common/string-from-param type)
            itype          (common/string-from-param input-type)
            label          (param-try-tr tr-sub :forms label id)
            phold          (param-try-tr tr-sub :forms placeholder id)
            value          (valuable (if value? (get params id-kw) value))
            value          (if value (param-try-tr tr-sub :forms value id))
            type           (or type (if error? (get errors id-kw)))
            err-msgs       (if error?   (coercion/translate-error @tr-sub id-kw type))
            err-summ       (if err-msgs (some-str (get err-msgs :error/summary)))
            err-desc       (if err-msgs (some-str (get err-msgs :error/description)))
            error?         (boolean (or err-summ err-desc))
            html-id        (html-esc id-str)
            html-label     (if label    (html-esc label))
            html-name      (if name     (html-esc name)  html-id)
            html-itype     (if itype    (html-esc itype) "text")
            html-ptcls     (if type     (strb " param-type-"    (html-esc type)))
            html-value     (if value    (strb " value=\""       (html-esc value) "\""))
            html-phold     (if phold    (strb " placeholder=\"" (html-esc phold) "\""))
            html-esumm     (if err-summ (strb "      <p class=\"error-summary\">"     (html-esc err-summ) "</p>\n"))
            html-edesc     (if err-desc (strb "      <p class=\"error-description\">" (html-esc err-desc) "</p>\n"))
            html-error     (if error?   (strb "    <div class=\"form-error\">\n" html-esumm html-edesc "</div>\n"))
            html-label     (if label    (strb "    <label for=\"" id-str "\" class=\"label\">" html-label "</label>\n"))]
        (strs "<div class=\"field param-" html-id html-ptcls "\">\n"
              html-label
              "    <input type=\"" html-itype "\" name=\"" html-name "\" id=\"" html-id "\""
              html-phold html-value " />\n"
              html-error
              "  </div>\n")))))
(defn form-fields
  [args tr-sub errors params]
  (str/join
   "\n"
   (for [field (->> args (partition-by #{"|"}) (take-nth 2))]
     (form-field field tr-sub errors params))))

(defn add-taggers
  [router language translations-fn validators]

  (let [lang-settings (or (get language :config) language)
        lang-param    (language/param nil lang-settings)
        validators    (or (get validators :config) validators)]

    (selmer/add-tag! :tr     #(tr     %1 %2 translations-fn))
    (selmer/add-tag! :tr-sub #(tr-sub %1 %2 translations-fn))

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
       (let [smap            (session/of ctx)
             sid             (session/id smap)
             sfld            (session/id-field smap)
             path-or-name    (first args)
             args            (rest args)
             args            (if (map? (first args)) (cons nil args) args)
             [lang params
              query-params
              lang-settings] args
             out-path        (lang-url router ctx path-or-name lang false params query-params lang-param)]
         (if (and sid sfld)
           (strb "<form name=\"sessionLink\" class=\"formlink\" action=\"" out-path "\" method=\"post\">"
                 (anti-spam-code validators)
                 "<button type=\"submit\" class=\"link\" name=\"" sfld "\" value=\"" sid "\">"
                 (get-in content [:link :content])
                 "</button></form>")
           (strb "<a href=\"" out-path "\" class=\"link\">" (get-in content [:link :content]) "</a>"))))
     :endlink)

    (selmer/add-tag!
     :slink
     (fn [args ctx content]
       (let [url  (selmer/render (first args) ctx {:tag-open \[ :tag-close \]})
             smap (session/of ctx)
             sid  (session/id smap)
             sfld (session/id-field smap)]
         (if (and sid sfld)
           (strb "<form name=\"sessionLink\" class=\"formlink\" action=\"" url "\" method=\"post\">"
                 (anti-spam-code validators)
                 "<button type=\"submit\" class=\"link\" name=\"" sfld "\" value=\"" sid "\">"
                 (get-in content [:slink :content])
                 "</button></form>")
           (strb "<a href=\"" url  "\" class=\"link\">" (get-in content [:slink :content]) "</a>"))))
     :endslink)

    (selmer/add-tag!
     :session-data
     (fn [args ctx]
       (let [smap (session/of ctx)
             sfld (session/id-field smap)]
         (strb (anti-spam-code validators)
               "<input type=\"hidden\" name=\"" sfld "\" value=\"" (session/id smap) "\" />"))))

    (selmer/add-tag!
     :explain-form-error
     (fn [args ctx]
       (if-some [fe (get ctx :form/errors)]
         (let [fe         (get fe :errors)
               param-id   (common/string-from-param (first args))
               param-type (common/string-from-param (second args))]
           (if (and param-id (contains? fe (keyword param-id)))
             (let [translator-sub (i18n/no-default (translator-sub ctx translations-fn))
                   param-type     (or param-type (get fe param-id))
                   ptype-class    (if param-type (strb " param-type-" param-type))
                   messages       (coercion/translate-error translator-sub param-id param-type)
                   summary        (some-str (get messages :error/summary))
                   description    (some-str (get messages :error/description))
                   summary        (if summary (strb "<p class=\"error-summary\">" summary "</p>"))
                   description    (if description (strb "<p class=\"error-description\">" description "</p>"))]
               (if (or summary description)
                 (strb "<div class=\"form-error param-" param-id ptype-class "\">"
                       summary description "</div>"))))))))

    (selmer/add-tag!
     :prefill-form-field
     (fn [args ctx]
       (if-some [fe (not-empty (get ctx :form/errors))]
         (if-some [pa (not-empty (get fe :params))]
           (if-let [param-id (common/keyword-from-param (first args))]
             (if-some [param (some-str (get pa param-id))]
               (binding [sutil/*escape-variables* true]
                 (html-esc param))))))))

    (selmer/add-tag!
     :form-fields
     (fn [args ctx]
       (binding [sutil/*escape-variables* true]
         (let [tr-sub      (delay (i18n/no-default (translator-sub ctx translations-fn)))
               form-errors (not-empty (get ctx :form/errors))
               errors      (if form-errors (get form-errors :errors))
               params      (if form-errors (get form-errors :params))
               args        (parse-args args)]
           (form-fields args tr-sub errors params)))))
    (selmer/add-tag!
     :form-field
     (fn [args ctx]
       (binding [sutil/*escape-variables* true]
         (let [props       (get ctx :form-props)
               tr-sub      (if props (get props :tr-sub))
               tr-sub      (or tr-sub (delay (i18n/no-default (translator-sub ctx translations-fn))))
               form-errors (not-empty (get ctx :form/errors))
               errors      (if form-errors (get form-errors :errors))
               params      (if form-errors (get form-errors :params))
               args        (parse-args args)]
           (form-field args tr-sub errors params)))))
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
