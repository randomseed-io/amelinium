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
  "Encodes URL given as a string `s` using Java's `URLEncoder/encode`."
  ^String [^String s]
  (URLEncoder/encode s))

(defn url-esc
  "Escapes certain characters (`<`, `>`, `\"` and `'`) in the given string `s` by
  replacing them with character codes prefixed with `%` symbol."
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
  "Escapes string `s` to be used in HTML using Selmer's
  `selmer.filter-parser/escape-html*` function."
  [s]
  (fp/escape-html* s))

(defn render-assignment-value
  [ctx v]
  (if (nil? ctx)
    v
    (if (seq v)
      (if-some [r (nth (re-find #"^\s*\[\[\s*([^\]]+)\s*\]\]\s*$" v) 1 nil)]
        (some->> (str/trimr r) some-keyword (get ctx))
        v)
      "")))

(defn parse-assigments
  [fk fv coll]
  (map #(%1 %2) (cycle [fk fv]) coll))

(defn assignments->map
  "Parses a string `s` with key=value assignments and returns a map with keys as
  strings. Variables are resolved if they have `[[` and `]]` around."
  ([^String s ctx]
   (if (and s (seq s))
     (let [tr-fn (if ctx
                   (partial parse-assigments identity #(render-assignment-value ctx %))
                   identity)]
       (->> (str/split s #"\,")
            (mapcat #(map str/trim (str/split (str/trim %) #"\=")))
            (tr-fn)
            (apply array-map)
            (not-empty)))))
  ([^String s]
   (assignments->map s nil)))

(defn assignments->kw-map
  "Parses a string `s` with key=value assignments and returns a map with keys as
  keywords. Variables are resolved if they have `[[` and `]]` around."
  ([^String s ctx]
   (if (and s (seq s))
     (let [v-fn (if ctx #(render-assignment-value ctx %) identity)]
       (->> (str/split s #"\,")
            (mapcat #(map str/trim (str/split (str/trim %) #"\=")))
            (parse-assigments common/keyword-from-param v-fn)
            (apply array-map)
            (not-empty)))))
  ([^String s]
   (assignments->kw-map s nil)))

(defn parse-args
  "Parses arguments using `selmer.filter-parser/fix-filter-args` function."
  [args]
  (fp/fix-filter-args args))

(defn args->map
  "Transforms the given sequence of arguments `args` to a map by taking each
  consecutive pair and changing its first element into a keyword (using
  `amelinium.common/keyword-from-param`) to become a key associated with its paired
  value."
  [args]
  (if (seq args)
    (apply array-map (map #(%1 %2) (cycle [common/keyword-from-param identity]) args))))

(defn strb
  "Like `clojure.core/str` but faster. Be aware that all arguments except first must be
  of type `String` or `nil`."
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
  "Converts all arguments to strings and concatenates them. Neighbouring literal
  strings will be concatenated at compile time."
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

(defn- pos-str
  "Calls Java's `Integer/toUnsignedString` on an integer number."
  ^String [^Integer n]
  (Integer/toUnsignedString n))

(defn ad-hoc-id
  "Generates ad-hoc ID (a numeric string) on a basis of values passed as arguments."
  (^String [a]         (pos-str (hash a)))
  (^String [a b]       (pos-str (hash-ordered-coll [a b])))
  (^String [a b c]     (pos-str (hash-ordered-coll [a b c])))
  (^String [a b c d]   (pos-str (hash-ordered-coll [a b c d])))
  (^String [a b c d e] (pos-str (hash-ordered-coll [a b c d e])))
  (^String [a b c d e & more]
   (pos-str (hash-ordered-coll (list* a b c d e more)))))

(defn get-lang
  "Obtains a language string from a context map `ctx` by trying the following keys:
  `:language/str`, `:lang`, `:language` and `:language/default`."
  [ctx]
  (or (get ctx :language/str)
      (some-str (get ctx :lang))
      (some-str (get ctx :language))
      (get ctx :language/default)))

(defn get-lang-id
  "Obtains a language identifier (as keyword) from a context map `ctx` by trying the
  following keys: `:language/str`, `:lang`, `:language` and `:language/default`."
  [ctx]
  (or (get ctx :language/id)
      (some-keyword-simple (get ctx :lang))
      (some-keyword-simple (get ctx :language))
      (get ctx :language/default)))

(defn lang-url
  "Transforms the given URI (a path expressed as a string) or a route name (a keyword)
  to a localized path using the detected (or given) language. Calls
  `amelinium.common/lang-url`. Tries to be very optimistic: if a path was given and
  there was no success in transforming it into localized variant, it will return it."
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
  "For the given context map `ctx` and optional translation function `translations-fn`
  it returns a translation function with predefined language, taking a translation
  key `k` and optional arguments.

  The value of `translations-fn` argument is used as fallback when there is no
  `:i18n/translator` nor `:i18n/translator-nd` key found in a context map. It should
  be a function returned by the `amelinium.i18n/translation-fn`. If `translations-fn`
  is `nil` or `false`, it will fall back to a generic, globally initialized
  `amelinium.i18n/translations`."
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
  "For the given context map `ctx` and optional translation function `translations-fn`
  it returns a translation function with predefined language, taking a translation
  key `k` and optional arguments. If the first optional argument is present and it is
  not `nil` nor `false` then its value will become a name and the value of `k` will
  become a namespace for a translation key.

  The value of `translations-fn` argument is used as fallback when there is no
  `:i18n/translator-sub` nor `:i18n/translator-sub-nd` key found in a context map. It
  should be a function returned by the `amelinium.i18n/translation-fn`. If
  `translations-fn` is `nil` or `false`, it will fall back to a generic, globally
  initialized `amelinium.i18n/translations`."
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
  "Translation function. Creates a translator by calling `translator` and prepares
  arguments from a template tag to be passed to it (the first being converted to a
  keyword). Returns a string or `nil`."
  ([args ctx]
   (tr args ctx nil))
  ([args ctx translations-fn]
   (if-some [translator (translator ctx translations-fn)]
     (apply translator (common/keyword-from-param (first args)) (next args)))))

(defn tr-sub
  "Translation function. Creates a translator by calling `translator-sub` and prepares
  arguments from a template tag to be passed to it (the first and second being
  converted to keywords). Returns a string or `nil`."
  ([args ctx]
   (tr-sub args ctx nil))
  ([args ctx translations-fn]
   (if-some [translator-sub (translator-sub ctx translations-fn)]
     (apply translator-sub
            (common/keyword-from-param (first  args))
            (common/keyword-from-param (second args))
            (nnext args)))))

(defn kw-param?
  "Returns `true` if the given value is a keyword or a string expressing a
  keyword (with `:` symbol as its first character)."
  [v]
  (or (keyword? v)
      (and (string? v)
           (not-empty-string? v)
           (= \: (.charAt ^String v 0)))))

(defn param-try-tr
  "Tries to translate tag parameters `k` and `v` using the given `tr-sub-fn`
  function (which should be a result of calling `translator-sub`). If the value of
  `v` is not a keyword or keyworded string (`kw-param?` applied to `v` does not
  return `true`) then a string of `v` is returned without calling a translation
  function. Any additional arguments are passed to a translation function as
  additional arguments."
  ([tr-sub-fn v]
   (if v
     (if (kw-param? v)
       (if-some [v (common/keyword-from-param v)]
         ((force tr-sub-fn) v))
       (not-empty (str v)))))
  ([tr-sub-fn k v]
   (if v
     (if (kw-param? v)
       (if-some [v (common/string-from-param v)]
         ((force tr-sub-fn) (some-str k) v))
       (not-empty (str v)))))
  ([tr-sub-fn k v a]
   (if v
     (if (kw-param? v)
       (if-some [v (common/string-from-param v)]
         ((force tr-sub-fn) (some-str k) v a))
       (not-empty (str v)))))
  ([tr-sub-fn k v a b]
   (if v
     (if (kw-param? v)
       (if-some [v (common/string-from-param v)]
         ((force tr-sub-fn) (some-str k) v a b))
       (not-empty (str v)))))
  ([tr-sub-fn k v a b & more]
   (if v
     (if (kw-param? v)
       (if-some [v (common/string-from-param v)]
         (apply (force tr-sub-fn) (some-str k) v a b more))
       (not-empty (str v))))))

(defn form-field
  "Helper to generate HTML for the `form-field` tag."
  [args tr-sub errors params]
  (let [field (args->map args)
        id    (get field :id)]
    (if-some [id-str (common/string-from-param id)]
      (let [{:keys
             [name
              label
              parameter-type
              value
              placeholder
              autocomplete
              input-type
              type]}   field
            value?     (contains? params id-str)
            error?     (contains? errors id-str)
            id-str     (common/string-from-param id)
            name       (common/string-from-param name)
            autoc      (common/string-from-param autocomplete)
            ptype      (common/string-from-param parameter-type)
            itype      (common/string-from-param input-type)
            itype      (or itype (common/string-from-param type))
            label      (param-try-tr tr-sub :forms label id)
            phold      (param-try-tr tr-sub :forms placeholder id)
            value      (valuable (if value? (get params id-str) value))
            value      (if value (param-try-tr tr-sub :forms value id))
            ptype      (or ptype (if error? (get errors id-str)))
            err-msgs   (if error?   (coercion/translate-error @tr-sub id-str ptype))
            err-summ   (if err-msgs (some-str (get err-msgs :error/summary)))
            err-desc   (if err-msgs (some-str (get err-msgs :error/description)))
            error?     (boolean (or err-summ err-desc))
            hidden?    (and itype (= "hidden" itype))
            html-id    (html-esc id-str)
            html-label (if label    (html-esc label))
            html-name  (if name     (html-esc name)  html-id)
            html-itype (if itype    (html-esc itype) "text")
            html-ptcls (if ptype    (strb " param-type-"     (html-esc ptype)))
            html-value (if value    (strb " value=\""        (html-esc value) "\""))
            html-phold (if phold    (strb " placeholder=\""  (html-esc phold) "\""))
            html-autoc (if autoc    (strb " autocomplete=\"" (html-esc autoc) "\""))
            html-esumm (if err-summ (strb "      <p class=\"error-summary\">"     (html-esc err-summ) "</p>\n"))
            html-edesc (if err-desc (strb "      <p class=\"error-description\">" (html-esc err-desc) "</p>\n"))
            html-error (if error?   (strb "    <div class=\"form-error\">\n" html-esumm html-edesc "</div>\n"))
            html-label (if label    (strb "    <label for=\"" id-str "\" class=\"label\">" html-label "</label>\n"))]
        (if hidden?
          (strs html-label
                "  <input type=\"" html-itype "\" name=\"" html-name "\" id=\"" html-id "\""
                html-phold html-value html-autoc " />\n"
                html-error)
          (strs "<div class=\"field param-" html-id html-ptcls "\">\n"
                html-label
                "    <input type=\"" html-itype "\" name=\"" html-name "\" id=\"" html-id "\""
                html-phold html-value html-autoc " />\n"
                html-error
                "  </div>\n"))))))

(defn form-fields
  "Helper to generate HTML for the `form-fields` tag."
  [args tr-sub errors params]
  (str/join
   "\n"
   (for [field (->> args (partition-by #{"|"}) (take-nth 2))]
     (form-field field tr-sub errors params))))

(defn form-submit
  "Helper to generate HTML for the `form-submit` tag."
  [label tr-sub session-field session-id validators]
  (let [label (param-try-tr tr-sub :forms (or label :submit))
        label (if label (html-esc label) "OK!")
        sdata (if (and session-field session-id) (strb " name=\"" session-field "\" value=\"" session-id "\""))]
    (strs (anti-spam-code validators)
          "  <div class=\"control\">\n"
          "    <button type=\"submit\"" sdata ">" label "</button>\n"
          "  </div>\n")))

(defn get-form-action
  "Prepares default form action attribute by removing `form-errors` from a query string
  for current page if the `:action` in `args` is set to `nil`. If the `:action` is
  not `nil`, it is returned as is. If there is no query params, `nil` is returned. If
  there is no `form-errors` in query params, `nil` is returned.

  This function takes care about a corner case where leaving form action empty on a
  rendered form would cause current form errors encoded in a query parameter string
  to be sent again to the same page, giving possibly misleading information about
  form errors."
  [args ctx]
  (if (contains? args :action)
    (get args :action)
    (if-some [qp (get ctx :query-params)]
      (if (and (contains? qp "form-errors") (contains? ctx :uri))
        (let [qp (dissoc qp "form-errors")]
          (if (pos? (count qp))
            (str (get ctx :uri) "?" (common/query-string-encode ctx qp))
            (get ctx :uri)))
        nil))))

(defn add-taggers
  "Registers taggers in a global repository. To be changed to a pure fn some day."
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
             [lang
              path-params
              query-params] args
             lang           (or lang (get-lang ctx))
             path-params    (if path-params  (assignments->kw-map path-params ctx))
             query-params   (if query-params (assignments->map query-params ctx))]
         (lang-url router ctx path-or-name lang true path-params query-params lang-param))))

    (selmer/add-tag!
     :link
     (fn [args ctx content]
       (let [smap           (session/of ctx)
             sid            (session/id smap)
             sfld           (session/id-field smap)
             lcontent       (get (get content :link) :content)
             sdata          (if (and sid sfld) (strb " name=\"" sfld "\" value=\"" sid "\""))
             path-or-name   (first args)
             args           (rest args)
             args           (if (map? (first args)) (cons nil args) args)
             [lang
              path-params
              query-params] args
             lang           (or lang (get-lang ctx))
             path-params    (if path-params  (assignments->kw-map path-params ctx))
             query-params   (if query-params (assignments->map   query-params ctx))
             out-path       (lang-url router ctx path-or-name lang false path-params query-params lang-param)]
         (if sdata
           (strs "<form name=\"sessionLink\" class=\"formlink\" action=\"" out-path "\" method=\"post\">"
                 (anti-spam-code validators)
                 "<button type=\"submit\" class=\"link\"" sdata ">" lcontent "</button></form>")
           (strs "<a href=\"" out-path "\" class=\"link\">" lcontent "</a>"))))
     :end-link)

    (selmer/add-tag!
     :slink
     (fn [args ctx content]
       (let [url      (first args)
             ;;url      (selmer/render (first args) ctx {:tag-open \[ :tag-close \]})
             smap     (session/of ctx)
             sid      (session/id smap)
             sfld     (session/id-field smap)
             sdata    (if (and sid sfld) (strb " name=\"" sfld "\" value=\"" sid "\""))
             lcontent (get (get content :slink) :content)]
         (if sdata
           (strs "<form name=\"sessionLink\" class=\"formlink\" action=\"" url "\" method=\"post\">"
                 (anti-spam-code validators)
                 "<button type=\"submit\" class=\"link\"" sdata ">" lcontent "</button></form>")
           (strs "<a href=\"" url  "\" class=\"link\">" lcontent "</a>"))))
     :end-slink)

    (selmer/add-tag!
     :session-data
     (fn [args ctx]
       (let [smap (session/of ctx)
             sid  (session/id smap)
             sfld (session/id-field smap)]
         (if (and sid sfld)
           (strs (anti-spam-code validators)
                 "<input type=\"hidden\" name=\"" sfld "\" value=\"" sid "\" />")))))

    (selmer/add-tag!
     :explain-form-error
     (fn [args ctx]
       (if-some [fe (get ctx :form/errors)]
         (let [fe         (get fe :errors)
               param-id   (common/string-from-param (first args))
               param-type (common/string-from-param (second args))]
           (if (and param-id (contains? fe param-id))
             (let [translator-sub (i18n/no-default (translator-sub ctx translations-fn))
                   param-type     (or param-type (get fe param-id))
                   ptype-class    (if param-type (strb " param-type-" param-type))
                   messages       (coercion/translate-error translator-sub param-id param-type)
                   summary        (some-str (get messages :error/summary))
                   description    (some-str (get messages :error/description))
                   summary        (if summary (strb "<p class=\"error-summary\">" summary "</p>"))
                   description    (if description (strb "<p class=\"error-description\">" description "</p>"))]
               (if (or summary description)
                 (strs "<div class=\"form-error param-" param-id ptype-class "\">"
                       summary description "</div>"))))))))

    (selmer/add-tag!
     :prefill-form-field
     (fn [args ctx]
       (if-some [fe (not-empty (get ctx :form/errors))]
         (if-some [pa (not-empty (get fe :params))]
           (if-let [param-id (common/string-from-param (first args))]
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

    (selmer/add-tag!
     :form-submit
     (fn [args ctx]
       (let [args   (parse-args args)
             props  (get ctx :form-props)
             smap   (if-not props (session/of ctx))
             sid    (or (get props :session-id) (session/id smap))
             sfld   (or (get props :session-id-field) (session/id-field smap))
             tr-sub (or (get props :tr-sub) (i18n/no-default (translator-sub ctx translations-fn)))]
         (form-submit (first args) tr-sub sfld sid validators))))

    (selmer/add-tag!
     :form
     (fn [args ctx content]
       (let [args         (args->map (parse-args args))
             smap         (session/of ctx)
             sfld         (session/id-field smap)
             sid          (session/id smap)
             lang         (get args :lang)
             method       (get args :method)
             label        (get args :label)
             action       (get-form-action args ctx)
             id-str       (common/string-from-param (get args :id))
             tr-sub-fn    (delay (i18n/no-default (translator-sub ctx translations-fn)))
             label        (if label (param-try-tr tr-sub-fn :forms label id-str))
             label?       (some? label)
             method       (if method (common/string-from-param method))
             lang         (if lang   (common/string-from-param lang))
             action?      (some? action)
             action-lang  (if action? (or (common/string-from-param (get args :action-lang)) lang))
             path-params  (if action? (assignments->kw-map (get args :path-params) ctx))
             query-params (if action? (assignments->map (get args :query-params) ctx))
             id-str       (or id-str (if label? (ad-hoc-id action method label path-params query-params)))
             id-str?      (some? id-str)
             action       (if action?
                            (lang-url router ctx action action-lang false
                                      path-params query-params lang-param))
             form-props   (if id-str?
                            {:tr-sub tr-sub-fn :session-id sid :session-id-field sfld :id id-str}
                            {:tr-sub tr-sub-fn :session-id sid :session-id-field sfld})
             html-method  (strb " method=\"" (if method (html-esc method) "post") "\"")
             html-lang    (if lang    (strb " lang=\"" (html-esc lang) "\""))
             html-action  (if action  (strb " action=\"" action "\""))
             html-id      (if id-str? (strb " id=\"" id-str "\""))
             html-label   (if label?  (html-esc label))
             html-label   (if label?  (strb "<label for=\"" id-str "\""
                                            html-lang
                                            " class=\"label\">"
                                            html-label "</label>\n"))]
         (strs
          html-label
          "<form" html-id " class=\"familiar medium\"" html-lang html-method html-action ">"
          (selmer/render (get (get content :form) :content) (map/qassoc ctx :form-props form-props) {:tag-second \-})
          "</form>")))
     :end-form)

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
