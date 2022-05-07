(ns amelinium.rebel.main
  (:require
   rebel-readline.clojure.main
   rebel-readline.core
   io.aviso.ansi
   puget.printer))

(defn -main
  [& args]
  (rebel-readline.core/ensure-terminal
   (rebel-readline.clojure.main/repl
    :init (fn []
            (try
              (println "[amelinium] Loading Clojure code, please wait...")
              (require 'user)
              (in-ns 'user)
              (catch Exception e
                (.printStackTrace e)
                (println "[amelinium] Failed to require user, this usually means there was a syntax error. See exception above.")))))))
