(ns hooks.cambium.core
  (:require [clj-kondo.hooks-api :as api]))

(defn deflevel [{:keys [node]}]
  ;; (deflevel info) -> (defn info [& _] nil)
  (let [[_ level-name & _more] (:children node)]
    {:node (api/list-node
            (list
             (api/token-node 'clojure.core/defn)
             level-name
             (api/vector-node [(api/token-node '&) (api/token-node '_args)])
             (api/token-node 'nil)))}))
