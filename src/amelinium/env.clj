(ns

    ^{:doc    "Environment configuration branch of Amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.env

  (:require [amelinium           :refer    :all]
            [amelinium.env.file  :as    envfile]
            [amelinium.system    :as     system]
            [io.randomseed.utils :as      utils]))

(system/add-expand ::default [k config] {k config})
(system/add-init   ::default [_ config]     config)
(system/add-halt!  ::default [_ _]             nil)
