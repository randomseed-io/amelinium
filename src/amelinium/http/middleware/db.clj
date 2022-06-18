(ns

    ^{:doc    "amelinium service, database middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.middleware.db

  (:require [amelinium.db      :as     db]
            [amelinium.logging :as    log]
            [amelinium.system  :as system]))

(defn wrap-db
  "Database wrapping middleware."
  [k db-name db-obj]
  (let [ds-obj  (db/ds db-obj)
        db-name (db/db-name db-name db-obj)]
    (log/msg "Installing database handler for" db-name)
    {:name    k
     :compile (fn [_ _]
                (fn [handler]
                  (fn [req]
                    (handler (assoc req k ds-obj)))))}))

(system/add-init  ::db [k config] (wrap-db k (:name config) (:db config)))
(system/add-halt! ::db [_ config] nil)

(derive ::default ::db)

(derive ::web ::db)
(derive ::api ::db)
(derive ::all ::db)
