(ns amelinium.build.main

  (:require [clojure.java.io         :as       io]
            [clojure.data.xml        :as      xml]
            [clojure.tools.build.api :as        b]
            [juxt.pack.api           :as     pack]))

(defn jar
  [_]
  (pack/library
   {:basis (b/create-basis)
    :path  "amelinium.jar"
    :lib   'io.randomseed/amelinium
    :pom   (java.io.ByteArrayInputStream.
            (.getBytes
             (xml/emit-str
              (xml/parse (io/reader "pom.xml")))
             "UTF-8"))}))

(defn -main
  [& args]
  (jar nil))
