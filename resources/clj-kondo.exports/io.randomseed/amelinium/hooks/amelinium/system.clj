(ns hooks.amelinium.system
  (:require [clj-kondo.hooks-api :as api]))

(defn- rewrite-to-defmethod [{:keys [node]}]
  ;; children = (add-init ::key [k v] k)
  (let [[_macro dispatch args & body] (:children node)]
    {:node (api/list-node
            (list* (api/token-node 'defmethod)      ;; niekwalifikowane
                   (api/token-node 'print-method)   ;; cokolwiek istniejącego
                   dispatch
                   args
                   body))}))

(defn add-init     [ctx] (rewrite-to-defmethod ctx))
(defn add-expand   [ctx] (rewrite-to-defmethod ctx))
(defn add-suspend! [ctx] (rewrite-to-defmethod ctx))
(defn add-resume   [ctx] (rewrite-to-defmethod ctx))
(defn add-resolve  [ctx] (rewrite-to-defmethod ctx))
(defn add-halt!    [ctx] (rewrite-to-defmethod ctx))
