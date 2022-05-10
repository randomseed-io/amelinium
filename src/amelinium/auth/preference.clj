(ns

    ^{:doc    "amelinium service, authentication preferences."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.auth.preference

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [amelinium.logging          :as       log]
            [amelinium.auth.pwd         :as       pwd]
            [amelinium.system           :as    system]
            [io.randomseed.utils        :refer   :all]
            [io.randomseed.utils.db     :as        db]
            [io.randomseed.utils.time   :as      time]
            [io.randomseed.utils.var    :as       var]
            [io.randomseed.utils.map    :as       map]))

;; Mapping of account type to preferred authentication configuration

(defonce default nil)
(defonce by-type nil)

(defmacro with-lock
  [& body]
  `(locking (var by-type)
     ~@body))

(defn update-by-account-type
  "Updates global map by-type associating account type (a keyword) with authentication
  settings (a map) if the key is not yet present."
  [account-type settings]
  (when-some [at (some-keyword account-type)]
    (when (:passwords/id settings)
      (var/alter by-type map/assoc-missing at settings))))

(defn by-account-type-or-default
  "Returns an authentication configuration for the given account type. If it cannot be
  found, returns a default one."
  [account-type]
  (or (get by-type (some-keyword account-type)) default))

(defn by-account-type
  "Returns an authentication configuration for the given account type."
  [account-type]
  (get by-type (some-keyword account-type)))

(defn init-by-type
  "Prepares static authentication preference map."
  [config]
  (->> config (map/map-keys some-keyword-simple) map/remove-empty-values))

(system/add-init  ::default [_ config] (var/reset default (when (seq config) config)))
(system/add-halt! ::default [_ config] (var/reset default nil))

(system/add-init  ::by-type [_ config] (var/alter by-type merge (init-by-type config)))
(system/add-halt! ::by-type [_ config] (var/reset by-type nil))
