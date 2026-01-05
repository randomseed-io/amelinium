(ns hooks.amelinium.web
  (:require [clj-kondo.hooks-api :as api]))

(def ^:private render-arities
  [[]                                 ; ()
   ['req]                             ; (req)
   ['req 'data]                       ; (req data)
   ['req 'data 'view]                 ; (req data view)
   ['req 'data 'view 'layout]         ; (req data view layout)
   ['req 'data 'view 'layout 'lang]]) ; (req data view layout lang)

(defn- do-body-node
  "Builds a body that 'uses' all syms to silence unused-binding warnings:
   (do sym1 sym2 ... nil)
   For zero-arity returns nil."
  [syms]
  (if (seq syms)
    (api/list-node
      (into [(api/token-node 'do)]
            (concat (map api/token-node syms)
                    [(api/token-node nil)])))
    (api/token-node nil)))

(defn- arity-node
  "Builds a single arity: ([args] (do args... nil))"
  [syms]
  (api/list-node
    [(api/vector-node (mapv api/token-node syms))
     (do-body-node syms)]))

(defn def-render
  "clj-kondo hook for amelinium.web/def-render (or similar)."
  [{:keys [node]}]
  (let [children  (:children node)
        args      (rest children)
        name-node (nth args 0 nil)
        a-node    (nth args 1 nil)
        doc-node  (when (and a-node (string? (api/sexpr a-node)))
                    a-node)
        arities   (mapv arity-node render-arities)
        head      (cond-> [(api/token-node 'defn) name-node]
                    doc-node (conj doc-node))]
    {:node (api/list-node (into head arities))}))
