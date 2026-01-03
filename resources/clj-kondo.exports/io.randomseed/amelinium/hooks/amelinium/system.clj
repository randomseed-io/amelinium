(ns hooks.amelinium.system
  (:require [clj-kondo.hooks-api :as api]))

(defn- rewrite-to-defmethod [multifn {:keys [node]}]
  ;; node = (add-init ::key [k v] k)
  (let [[dispatch args & body] (rest (:children node))]
    {:node (api/list-node
            (list* (api/token-node 'defmethod)
                   (api/token-node multifn)
                   dispatch
                   args
                   body))}))

(defn add-init     [ctx] (rewrite-to-defmethod 'integrant.core/init-key     ctx))
(defn add-expand   [ctx] (rewrite-to-defmethod 'integrant.core/expand-key   ctx))
(defn add-suspend! [ctx] (rewrite-to-defmethod 'integrant.core/suspend-key! ctx))
(defn add-resume   [ctx] (rewrite-to-defmethod 'integrant.core/resume-key   ctx))
(defn add-resolve  [ctx] (rewrite-to-defmethod 'integrant.core/resolve-key  ctx))
(defn add-halt!    [ctx] (rewrite-to-defmethod 'integrant.core/halt-key!    ctx))
