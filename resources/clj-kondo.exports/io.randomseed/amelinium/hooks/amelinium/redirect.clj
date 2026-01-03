(ns hooks.amelinium.redirect
  (:require [clj-kondo.hooks-api :as api]))

(defn- string-node? [n]
  (string? (api/sexpr n)))

(defn- pos-int-node? [n]
  (let [x (api/sexpr n)]
    (and (int? x) (pos? x))))

(defn- wrapper-defn-node [name-node]
  ;; (defn <name> [& _args] nil)
  (api/list-node
   [(api/token-node 'clojure.core/defn)
    name-node
    (api/vector-node [(api/token-node '&) (api/token-node '_args)])
    (api/token-node nil)]))

(defn- maybe-call-node [f-node]
  ;; (resp/moved-permanently) — tylko po to, by kondo widział "użycie" symbolu
  (when f-node
    (api/list-node [f-node])))

(defn- rewrite-def-redirect [{:keys [node]}]
  ;; node = (def-redirect name f 301) albo (def-redirect name "doc" f) albo (def-redirect name "doc" f 301)
  (let [args     (rest (:children node))
        name     (nth args 0 nil)
        a1       (nth args 1 nil)
        a2       (nth args 2 nil)
        a3       (nth args 3 nil)

        ;; rozpoznaj warianty:
        ;; - [name f] / [name f code]
        ;; - [name doc f] / [name doc f code]
        [f _code] (cond
                   (string-node? a1) [a2 (when (pos-int-node? a3) a3)]
                   :else             [a1 (when (pos-int-node? a2) a2)])]
    {:node (api/list-node
            (cond-> [(api/token-node 'do)
                     (wrapper-defn-node name)]
              f (conj (maybe-call-node f))))}))

(defn def-redirect           [ctx] (rewrite-def-redirect ctx))
(defn def-localized-redirect [ctx] (rewrite-def-redirect ctx))
