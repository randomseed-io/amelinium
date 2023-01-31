(ns

    ^{:doc    "amelinium service, common web URLs."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.web.url

  (:refer-clojure :exclude [update])
  (:require [amelinium.app :as app]))

(derive ::base                  ::app/url)
(derive ::host                  ::app/url)
(derive ::confirm               ::app/url)
(derive ::create                ::app/url)
(derive ::update                ::app/url)
(derive ::update-email          ::app/url)
(derive ::update-phone          ::app/url)
(derive ::update-password       ::app/url)
(derive ::recover               ::app/url)
