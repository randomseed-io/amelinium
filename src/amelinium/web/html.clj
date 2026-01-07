(ns

    ^{:doc    "HTML web helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.html

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string                       :as                 str]
            [ring.util.response]
            [amelinium.http.response              :as                resp]
            [amelinium.types.response             :refer             :all]
            [amelinium.utils                      :refer             :all]
            [amelinium.common                     :as              common]
            [amelinium.http                       :as                http]
            [amelinium.http.middleware.session    :as             session]
            [amelinium.http.middleware.coercion   :as            coercion]
            [amelinium.web.app-data               :as            app-data]
            [amelinium                            :refer             :all]
            [io.randomseed.utils.map              :as map :refer [qassoc]]
            [io.randomseed.utils                  :refer      [some-str
                                                               juxt-seq
                                                               some-keyword-simple
                                                               valuable?]]
            [hiccup.core                          :refer         :all]
            [hiccup.table                         :as           table])

  (:import (amelinium     Response)))

;; HTML generators and transformers

(defn- calc-roles
  [ctx-labeler roles-labeler missing-label [ctx & roles]]
  (into [(or (some-str (ctx-labeler ctx)) (str ctx))]
        (mapv (comp (fnil identity missing-label) roles-labeler) roles)))

(defn default-contexts-labeler
  [_ ids]
  (map (juxt-seq some-keyword-simple some-str) ids))

(defn roles-tabler
  ([req]
   (roles-tabler req nil))
  ([{{:keys [global-context] :or {global-context :!}} :roles/config :as req}
    {:keys [user-id effective? include-global? include-self?
            present-label missing-label global-label
            global-marker global-present-label context-label contexts-labeler]
     :or   {present-label    "YES"
            missing-label    "—"
            global-label     "global"
            context-label    "Context"
            contexts-labeler default-contexts-labeler
            include-self?    false}
     :as   opts}]
   (let [include-global?      (if (contains? opts :include-global?) include-global? (not effective?))
         user-id              (or user-id (get opts :user/id))
         opts                 (qassoc opts
                                      :user-id         user-id
                                      :include-global? include-global?
                                      :include-self?   include-self?)
         global-marker        (or global-marker (utils/strb " (" global-label ")"))
         global-present-label (or global-present-label (utils/strb present-label global-marker))
         [l & d]              (roles-matrix req opts)
         gctx-line            (first d)
         have-gctx?           (and include-global? (= global-context (first gctx-line)))
         labels               (vec (interleave (range) (cons context-label (map str l))))
         roles-labeler        {true present-label, false missing-label, :! global-present-label}
         gctx-labeler         (when have-gctx? (qassoc roles-labeler :! present-label))
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
     {:data (seq data) :labels labels})))

(defn roles-table
  "Generates roles table as HTML string."
  ([req]
   (let [{:keys [data labels]} (roles-tabler req nil)]
     (when (and data labels)
       (html (table/to-table1d data labels)))))
  ([req opts]
   (let [{:keys [data labels]} (roles-tabler req opts)]
     (when (and data labels)
       (html (table/to-table1d data labels))))))

;; Redirect wrappers

(defn go-to
  "Uses the `localized-page` function to calculate the destination path on a basis of
  page name (identifier) or a path (a string) and performs a redirect with code 303 to
  it using `resp/see-other`. If the language is given it uses the `localized-page` function.
  If there is no language given but the page identified by its name requires
  a language parameter to be set, it will be obtained from the given request map
  (under the key `:language/str`).

  The difference between this function and its regular counterpart (if defined) is in
  binary variants of them (when a request map and a name or a path are given as
  arguments). The regular function will fail to generate a redirect if there is
  no language parameter and the given path does not point to an existing
  page. On the contrary, this function will generate a localized path using a
  language obtained from a request (under `:language/str` key) and if there will be no
  language-parameterized variant of the path, it will fail. Use this function to make
  sure that a localized path will be produced, or `nil`."
  {:arglists '(^Response []
               ^Response [req]
               ^Response [url]
               ^Response [req url]
               ^Response [req name-or-path]
               ^Response [req name-or-path path-params]
               ^Response [req name-or-path path-params query-params]
               ^Response [req name-or-path lang]
               ^Response [req name-or-path lang path-params]
               ^Response [req name-or-path lang path-params query-params]
               ^Response [req name-or-path lang path-params query-params & more])}
  (^Response []
   (common/localized-redirect resp/see-other))
  (^Response [req-or-url]
   (common/localized-redirect resp/see-other req-or-url))
  (^Response [req name-or-path]
   (common/localized-redirect resp/see-other req name-or-path))
  (^Response [req name-or-path lang]
   (common/localized-redirect resp/see-other req name-or-path lang))
  (^Response [req name-or-path lang params]
   (common/localized-redirect resp/see-other req name-or-path lang params))
  (^Response [req name-or-path lang params query-params]
   (common/localized-redirect resp/see-other req name-or-path lang params query-params))
  (^Response [req name-or-path lang params query-params & more]
   (apply common/localized-redirect resp/see-other req name-or-path lang params query-params more)))

(defn move-to
  "Uses the `localized-page` function to calculate the destination path on a basis of
  page name (identifier) or a path (a string) and performs a redirect with code 307
  to it using `resp/temporary-redirect`. If the language is given it uses the
  `localized-page` function.  If there is no language given but the page identified
  by its name requires a language parameter to be set, it will be obtained from the
  given request map (under the key `:language/str`).

  The difference between this function and its regular counterpart (if defined) is in
  binary variants of them (when a request map and a name or a path are given as
  arguments). The regular function will fail to generate a redirect if there is
  no language parameter and the given path does not point to an existing
  page. On the contrary, this function will generate a localized path using a
  language obtained from a request (under `:language/str` key) and if there will be no
  language-parameterized variant of the path, it will fail. Use this function to make
  sure that a localized path will be produced, or `nil`."
  {:arglists '(^Response []
               ^Response [req]
               ^Response [url]
               ^Response [req url]
               ^Response [req name-or-path]
               ^Response [req name-or-path path-params]
               ^Response [req name-or-path path-params query-params]
               ^Response [req name-or-path lang]
               ^Response [req name-or-path lang path-params]
               ^Response [req name-or-path lang path-params query-params]
               ^Response [req name-or-path lang path-params query-params & more])}
  (^Response []
   (common/localized-redirect resp/temporary-redirect))
  (^Response [req-or-url]
   (common/localized-redirect resp/temporary-redirect req-or-url))
  (^Response [req name-or-path]
   (common/localized-redirect resp/temporary-redirect req name-or-path))
  (^Response [req name-or-path lang]
   (common/localized-redirect resp/temporary-redirect req name-or-path lang))
  (^Response [req name-or-path lang params]
   (common/localized-redirect resp/temporary-redirect req name-or-path lang params))
  (^Response [req name-or-path lang params query-params]
   (common/localized-redirect resp/temporary-redirect req name-or-path lang params query-params))
  (^Response [req name-or-path lang params query-params & more]
   (apply common/localized-redirect resp/temporary-redirect req name-or-path lang params query-params more)))

(defn go-to-with-status
  "Uses `go-to` to make a redirect on a basis of the given application status
  `app-status` by looking it up in `:error/destinations` of a route data map with
  fallback to a value associated with the `:error/destination` key or to a value of
  the `default-page` argument (if set)."
  (^Response [req]
   (go-to-with-status req nil :error nil))
  (^Response [req app-status]
   (go-to-with-status req nil app-status nil))
  (^Response [req app-status default-page]
   (go-to-with-status req nil app-status default-page))
  (^Response [req route-data app-status default-page]
   (let [route-data (or route-data (http/get-route-data req))]
     (go-to req
                 (or (get-in route-data [:error/destinations app-status] default-page)
                     (get route-data :error/destination))))))

;; Form errors

(defn handle-bad-request-form-params
  "Called by other functions to generate a redirect or to display a page with
  a form to be corrected because of a parameter error (induced manually or
  caused by coercion exception).

  Takes a request map `req`, a map of erroneous parameter identifiers to parameter
  types `errors`, a map of current parameter values `values`, a map of error
  explanations (`explanations`), page title (`title`), and optional session key
  `session-key` used when getting a session object from the `req`.

  The following arguments can be Delay objects and `clojure.core/force` will be
  applied to them before use: `errors`, `values`, `explanations`, `title`.

  Parameter type in `errors` map can be `nil`, meaning it is of unknown type.

  If there is a session then the `errors` map is stored in a session variable
  `:form-errors` under the `:errors` key (additionally, there is a `:dest` key
  identifying a path of the current page).

  If there is no valid session or a session variable cannot be stored, the result is
  serialized as a query string parameter `form-errors` with erroneous fields
  separated by commas.

  If type name is available for a parameter then a string in a form of
  `parameter:type` is generated.

  If type name is not available, a simple parameter name is generated. So the example
  value (before encoding) may look like `email,secret:password` (`email` being a
  parameter without type information, `secret` being a parameter with type named
  `password`).

  Next, the originating URI is obtained from the `Referer` header and a temporary
  redirect (with HTTP code 307) is generated with this path and a query string
  containing `form-errors` parameter. The value of the parameter is empty if form
  errors were saved in a session variable.

  The destination of the redirect can be overriden by the `:form-errors/page`
  configuration option associated with HTTP route data.

  If the destination URI cannot be established, or if a coercion error happened
  during handling some previous coercion error (so the current page is where the
  browser had been redirected to), then instead of generating a redirect, a regular
  page is rendered with HTTP code of 422. The `:app/data` key of a request map is
  updated with:

  - `:title` set to `title`,
  - `:form/errors` set to a map containing:
    - `:errors` mapped to `errors`,
    - `:params` mapped to `values`,
    - `:dest` mapped to destination URI;
  - `:coercion/errors` set to `explanations` map."
  [req route-data errors values explanations title session-key]
  (if-not (valuable? errors)
    req
    (let [route-data             (or route-data (http/get-route-data req))
          forced-orig-page       (get route-data :form-errors/page)
          orig-page              (or forced-orig-page (:page (get req :goto)))
          referer                (when (nil? orig-page) (some-str (get (get req :headers) "referer")))
          [orig-uri orig-params] (when referer (common/url->uri+params req referer))
          handling-previous?     (contains? (get req :query-params) "form-errors")]

      (if (and (or (valuable? orig-page) (valuable? orig-uri) referer)
               (not handling-previous?))

        ;; redirect to a form-submission page allowing user to correct errors
        ;; transfer form errors using query params or form params (if a session is present)

        (let [orig-uri     (when orig-uri (some-str orig-uri))
              orig-params  (when orig-uri orig-params)
              dest         (or orig-page orig-uri)
              dest-uri     (if (keyword? dest) (common/page req dest) dest)
              dest-uri     (some-str dest-uri)
              skey         (or session-key (get route-data :session-key))
              smap         (session/not-empty-of req skey)
              stored?      (when (and smap (session/valid? smap))
                             (session/put-var!
                              smap :form-errors
                              {:dest   dest-uri
                               :errors (force errors)
                               :params (force values)}))
              joint-params (qassoc orig-params "form-errors" (if stored? "" (coercion/join-errors (force errors))))]
          (if dest-uri
            (common/temporary-redirect req dest-uri nil joint-params)
            (resp/temporary-redirect (str referer
                                          (if (str/includes? referer "?") "&" "?")
                                          (common/query-string-encode req joint-params)))))

        ;; render a separate page describing invalid parameters
        ;; instead of current page

        (app-data/assoc
         req
         :title                 title
         :coercion/errors       explanations
         :form/previous-errors? handling-previous?
         :form/errors           (delay {:dest   (:uri req)
                                        :errors (force errors)
                                        :params (force values)}))))))
