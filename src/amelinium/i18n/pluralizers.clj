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
     (let [none   (or (:none any) (:nil any) (:null any) (get any nil) (get any false))
           zero   (or (:zero any) (:z any) (get any 0))
           one    (or (:one  any) (:o any) (:singular any) (:single any) (get any 1))
           plural (or (:many any) (:plural any) (:p any) (:more any) (:multiple any))
           all    (or (:all  any) (:other  any) (:any any) (:else any))]
       (cond
         (and none zero one plural) [none zero one plural]
         (and zero one plural)      [zero one plural]
         (and none one plural)      [none one plural]
         (and one plural)           [one plural]
         (and none zero one)        [none zero one zero]
         (and zero one)             [zero one zero]
         (and none one)             [none one one]
         (and none zero plural)     [none zero plural plural]
         (and zero plural)          [zero plural plural]
         (and none plural)          [none plural plural]
         :else                      [(or plural all)]))
     any))
  ([n singular plural]
   (if (= 1 n) singular plural))
  ([n zero singular plural]
   (if (= 1 n)
     singular
     (if (or (= 0 n) (not n)) zero plural)))
  ([n none zero singular plural]
   (if (= 1 n)
     singular
     (if (= 0 n)
       zero
       (if (not n) none plural)))))

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
     (let [none       (or (:none any) (:nil any) (:null any) (get any nil) (get any false))
           zero       (or (:zero any) (:z any) (get any 0))
           one        (or (:one  any) (:singular any ) (:single any) (:o any) (get any 1))
           genitive   (or (:genitive any) (:g any))
           nominative (or (:nominative any) (:n any))
           plural     (or (:plural any) (:p any) (:many any) (:more any) genitive nominative)
           all        (or (:all any) (:other any) (:any any) (:else any))]
       (cond
         (and none zero one genitive nominative) [none zero one genitive nominative]
         (and zero one genitive nominative)      [zero one genitive nominative]
         (and none one genitive nominative)      [none one genitive nominative]
         (and none zero one plural)              [none zero one plural plural]
         (and zero one plural)                   [zero one plural]
         (and none one plural)                   [none one plural]
         (and one genitive nominative)           [genitive one genitive nominative]
         (and none one plural)                   [none one plural plural plural]
         (and one plural)                        [one plural]
         (and none zero genitive nominative)     [none zero nominative genitive nominative]
         (and zero genitive nominative)          [zero nominative genitive nominative]
         (and none genitive nominative)          [none nominative genitive nominative]
         (and none zero one)                     [none zero one zero zero]
         (and zero one)                          [zero one zero]
         (and none one)                          [none one one]
         (and none zero genitive)                [none zero genitive genitive genitive]
         (and zero genitive)                     [zero genitive genitive]
         (and none genitive)                     [none genitive genitive]
         (and none zero nominative)              [none zero nominative nominative nominative]
         (and zero nominative)                   [zero nominative nominative]
         (and none nominative)                   [none nominative nominative]
         (and none zero plural)                  [none zero plural plural plural]
         (and zero plural)                       [zero plural plural]
         (and none plural)                       [none plural plural]
         :else                                   [(or plural all)]))
     any))
  ([n singular plural]
   (if (= 1 n) singular plural))
  ([n zero singular plural]
   (if n (case (int n) 1 singular 0 zero plural) zero))
  ([n zero singular plural-genitive plural-nominative]
   (cond
     (= 1 n)            singular
     (= 0 n)            zero
     (not n)            zero
     (pl-nominative? n) plural-nominative
     :else              plural-genitive))
  ([n none zero singular plural-genitive plural-nominative]
   (cond
     (= 1 n)            singular
     (= 0 n)            zero
     (not n)            none
     (pl-nominative? n) plural-nominative
     :else              plural-genitive)))
