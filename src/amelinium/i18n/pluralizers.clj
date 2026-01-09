(ns

    ^{:doc    "I18N pluralizing functions"
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.i18n.pluralizers

  (:require [io.randomseed.utils.map :as map]))

(defn integer-valued?
  "Returns true if x is a number representing an integer value
   (e.g. 2.0, 4M, 6/3). For NaN/Inf returns false."
  [x]
  (cond

    (integer? x)
    true

    (ratio? x)
    (== 1 (denominator x))

    (decimal? x)
    (let [^java.math.BigDecimal bd x]
      (zero? (.scale (.stripTrailingZeros bd))))

    (double? x)
    (let [d ^double x]
      (and (Double/isFinite d)
           (== d (Math/rint d))))

    (float? x)
    (let [d (double ^float x)]
      (and (Double/isFinite d)
           (== d (Math/rint d))))

    (number? x)
    (try
      (zero? (mod x 1))
      (catch ArithmeticException _ false))

    :else
    false))

;; en

(defn en-from-map
  [spec]
  (let [none   (or (:none spec) (:nil spec) (:null spec) (:noar spec) (get spec nil) (get spec false))
        zero   (or (:zero spec) (:z spec) (:zer spec) (:nullar spec))
        one    (or (:one  spec) (:singular spec) (:singular/n spec) (:single spec) (:s spec) (:singular spec) (:s/nominative spec) (:s/n spec))
        plural (or (:plural spec) (:p spec) (:many spec) (:more spec) zero)
        zero   (or zero plural)
        none   (or none zero)
        all    (or (:all spec) (:other spec) (:any spec) (:else spec) (:multar spec) (:mular spec))
        any    (or all plural one zero)]
    {:none         (or none      any)
     :zero         (or plural    any)
     :s/nominative (or one       any)
     :plural       (or plural    any)
     :all          (or all       any)
     :any          any}))

(defn en-from-vec
  [spec]
  (en-from-map
   (case (unchecked-long (count spec))
     0 {}
     1 {:all (nth spec 0)}
     2 {:one (nth spec 0), :plural (nth spec 1)}
     3 {:one (nth spec 1), :plural (nth spec 2), :zero (nth spec 0)}
     4 {:one (nth spec 2), :plural (nth spec 3), :zero (nth spec 1), :none (nth spec 0)}
     {:one (nth spec 2), :plural (nth spec 3), :zero (nth spec 1), :none (nth spec 0), :all (nth spec 4)})))

(defn en-compile
  [spec]
  (let [spec    (if (map? spec) (map/remove-if-value spec nil?) spec)
        src-map (map/remove-if-value ((if (map? spec) en-from-map en-from-vec) spec) nil?)
        {:keys [:none :zero :s/nominative :plural :all :any]}
        src-map]
    (cond
      (nil? (seq spec))
      (fn pluralize [_] nil)

      (== 1 (count spec))
      (fn pluralize [_] any)

      :else
      (fn pluralize [n]
        (or (cond
              (integer-valued? n) (cond
                                    (== n  1) nominative
                                    (== n  0) zero
                                    (== n -1) nominative
                                    :else     plural)
              (not n)             none
              (number? n)         plural
              :else               all)
            any)))))

(defn en
  ([n]
   (str n))
  ([n spec]
   (if (identical? :amelinium.i18n/parse n)
     (en-compile spec)
     spec)))

;; pl

(defn- pl-nominative-long
  [^long n]
  (let [n    (abs n)
        n10  (rem n 10)
        n100 (rem n 100)]
    (and (<= 2 n10 4)
         (or (< n100 10) (>= n100 20)))))

(defn pl-nominative?
  [n]
  (if (or (int? n) (instance? java.lang.Long n))
    (pl-nominative-long n)
    (let [n (if (decimal? n) (.abs ^BigDecimal n) (abs n))]
      (and (>= 4 (rem n 10) 2)
           (let [mh (rem n 100)] (or (< mh 10) (>= mh 20)))))))

(defn pl-from-map
  [spec]
  (let [none        (or (:none spec) (:nil spec) (:null spec) (:noar spec) (get spec nil) (get spec false))
        zero        (or (:zero spec) (:z spec) (:zer spec) (:nullar spec))
        one         (or (:one  spec) (:singular spec) (:singular/n spec) (:single spec) (:s spec) (:singular spec) (:s/nominative spec) (:s/n spec))
        pgenitive   (or (:plural/genitive spec) (:plural/g spec) (:p/genitive spec) (:p/g spec))
        pnominative (or (:plural/nominative spec) (:plural/n spec) (:p/nominative spec) (:p/n spec))
        plural      (or (:plural spec) (:p spec) (:many spec) (:more spec) pgenitive pnominative zero)
        pgenitive   (or pgenitive plural zero)
        pnominative (or pnominative plural)
        zero        (or zero pgenitive)
        none        (or none zero)
        genitive    (or (:genitive spec) (:s/genitive spec) (:fraction spec) (:s/g spec) (:g spec) pgenitive pnominative plural)
        all         (or (:all spec) (:other spec) (:any spec) (:else spec) (:multar spec) (:mular spec))
        any         (or all pgenitive one plural zero genitive)]
    {:none         (or none           any)
     :zero         (or zero           any)
     :s/nominative (or one            any)
     :p/genitive   (or pgenitive      any)
     :p/nominative (or pnominative    any)
     :plural       (or plural         any)
     :s/genitive   (or genitive       any)
     :all          (or all            any)
     :any          any}))

(defn pl-from-vec
  [spec]
  (pl-from-map
   (case (unchecked-long (count spec))
     0 {}
     1 {:all (nth spec 0)}
     2 {:one (nth spec 0), :plural (nth spec 1), :all (nth spec 1)}
     3 {:one (nth spec 1), :plural (nth spec 2), :zero (nth spec 0), :all (nth spec 2)}
     4 {:one (nth spec 1), :p/g (nth spec 2), :p/n (nth spec 3), :zero (nth spec 0)}
     5 {:one (nth spec 1), :p/g (nth spec 2), :p/n (nth spec 3), :zero (nth spec 0), :g (nth spec 4)}
     6 {:one (nth spec 1), :p/g (nth spec 2), :p/n (nth spec 3), :zero (nth spec 0), :g (nth spec 4), :none (nth spec 5)}
     {:one  (nth spec 1), :p/g (nth spec 2), :p/n (nth spec 3), :zero (nth spec 0), :g (nth spec 4),
      :none (nth spec 5), :all (nth spec 6)})))

(defn pl-compile
  [spec]
  (let [spec    (if (map? spec) (map/remove-if-value spec nil?) spec)
        src-map (map/remove-if-value ((if (map? spec) pl-from-map pl-from-vec) spec) nil?)
        {:keys             [:none :zero :s/genitive :s/nominative :plural :all :any]
         plural-genitive   :p/genitive
         plural-nominative :p/nominative}
        src-map]
    (cond
      (nil? (seq spec))
      (fn pluralize [_] nil)

      (== 1 (count spec))
      (fn pluralize [_] any)

      :else
      (fn pluralize [n]
        (or (cond
              (integer-valued? n) (cond
                                    (== n  1)          nominative
                                    (== n  0)          zero
                                    (== n -1)          nominative
                                    (pl-nominative? n) plural-nominative
                                    :else              (or plural-genitive plural))
              (not n)             none
              (number? n)         genitive
              :else               all)
            any)))))

(defn pl
  ([n]
   (str n))
  ([n spec]
   (if (identical? :amelinium.i18n/parse n)
     (pl-compile spec)
     spec)))
