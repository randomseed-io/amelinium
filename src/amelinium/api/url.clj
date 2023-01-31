(ns

    ^{:doc    "amelinium service, common API URLs."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.api.url

  (:require [amelinium.app :as app]))

(derive ::base         ::app/url)
(derive ::host         ::app/url)
(derive ::swagger.json ::app/url)
