(ns

    ^{:doc    "Public specs of amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    amelinium.spec

  (:require [amelinium                 :as             GW]
            [amelinium.core            :as      amelinium]
            [io.randomseed.utils       :as          utils]
            [amelinium.locale          :as         locale]
            [clojure.spec.alpha        :as              s]
            [orchestra.spec.test       :as             st]
            [clojure.spec.gen.alpha    :as            gen]))

;;
;; Namespaces for easy use of keywords
;;

(alias 'arg   (create-ns 'amelinium.arg))   ;; for internal arg specs
(alias 'args  (create-ns 'amelinium.args))  ;; for internal args specs
(alias 'prop  (create-ns 'amelinium.prop))  ;; for additional properties

(alias 'input (create-ns 'amelinium.input)) ;; in for public input specs

