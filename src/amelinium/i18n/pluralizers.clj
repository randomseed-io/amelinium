(ns

    ^{:doc    "I18N pluralizing functions"
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.i18n.pluralizers

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [tongue.core             :as    tongue]
            [io.randomseed.utils     :refer   :all]
            [io.randomseed.utils.var :as       var]
            [io.randomseed.utils.map :as       map]
            [amelinium.locale        :as         l]))

;; en

(defn en
  ([n]
   (str n))
  ([n any]
   (if (identical? :parse-args n)
     (let [zero   (or (:zero any) (:z any) (:none any) (get any 0))
           one    (or (:one  any) (:o any) (:singular any) (:single any) (get any 1))
           plural (or (:many any) (:plural any) (:p any) (:more any) (:multiple any))
           all    (or (:all any) (:other any) (:any any) (:else any))]
       (cond
         (and zero one plural) [zero one plural]
         (and one plural)      [one plural]
         (and zero one)        [zero one zero]
         (and zero plural)     [zero plural plural]
         :else                 [(or plural all)]))
     any))
  ([n singular plural]
   (if (= 1 n) singular plural))
  ([n zero singular plural]
   (if (= 1 n)
     singular
     (if (or (= 0 n) (not n)) zero plural))))

;; pl

(defn- pl-nominative?
  [n]
  (and (>= 4 (mod n 10) 2)
       (let [mh (mod n 100)] (or (< mh 10) (>= mh 20)))))

(defn pl
  ([n]
   (str n))
  ([n any]
   (if (identical? :parse-args n)
     (let [zero       (or (:zero any) (:z any) (:none any) (get any 0))
           one        (or (:one  any) (:singular any ) (:single any) (:o any) (get any 1))
           genitive   (or (:genitive any) (:g any))
           nominative (or (:nominative any) (:n any))
           plural     (or (:plural any) (:p any) (:many any) (:more any) genitive nominative)
           all        (or (:all any) (:other any) (:any any) (:else any))]
       (cond
         (and zero one genitive nominative) [zero one genitive nominative]
         (and zero one plural)              [zero one plural]
         (and one genitive nominative)      [genitive one genitive nominative]
         (and one plural)                   [one plural]
         (and zero genitive nominative)     [zero nominative genitive nominative]
         (and zero one)                     [zero one zero]
         (and zero genitive)                [zero genitive genitive]
         (and zero nominative)              [zero nominative nominative]
         (and zero plural)                  [zero plural plural]
         :else                              [(or plural all)]))
     any))
  ([n singular plural]
   (if (= 1 n) singular plural))
  ([n zero singular plural]
   (case (int n) 1 singular 0 zero plural))
  ([n zero singular plural-genitive plural-nominative]
   (cond
     (= 1 n)            singular
     (= 0 n)            zero
     (not n)            zero
     (pl-nominative? n) plural-nominative
     :else              plural-genitive)))
