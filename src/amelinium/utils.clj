(ns

 ^{:doc    "amelinium service, utility functions."
   :author "Paweł Wilk"
   :added  "1.0.0"}

 amelinium.utils

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [amelinium]
            [clojure.java.io           :as               io]
            [tick.core                 :as                t]
            [io.randomseed.utils       :refer [get-rand-int
                                               random-uuid
                                               not-empty-string?
                                               some-str]]
            [io.randomseed.utils.map   :as              map])

  (:import (java.time        Duration)
           (java.time.format DateTimeFormatter)
           (clojure.lang     Keyword)))

;; Data structures and control flow expressions

(def empty-lazy-map
  "An empty lazy map."
  (map/lazy))

(defn map-to-lazy
  "Ensures that the given argument `m` is a lazy map. If it is not a map, it is
  returned as is. If it is `nil`, empty lazy map is returned."
  [m]
  (if (map? m)
    (map/to-lazy m)
    (if (nil? m)
      empty-lazy-map
      m)))

;; File system operations

(defn some-resource
  "Returns the given path if there is a resource it points to. Otherwise it returns
  nil. Multiple arguments are joined using str."
  ([path]
   (when-some [path (str path)] (and (io/resource path) path)))
  ([path & more]
   (when-some [path (apply str path more)] (and (io/resource path) path))))

;; UUIDs

(defn random-uuid-or-empty
  "Returns random UUID object or an empty string (50/50 chance)."
  ([]
   (random-uuid-or-empty nil))
  ([rng]
   (if (zero? (get-rand-int 2 rng))
     (random-uuid)
     "")))

;; Parameters

(defn string-from-param
  "Takes a value `s` (which should be convertable to a string) and returns a string
  with first colon character removed. Returns `nil` for falsy values or empty
  string (including empty strings after trimming the `:`)."
  ^String [s]
  (when-some [^String s (some-str s)]
    (if (= \: (.charAt s 0))
      (let [^String s (subs s 1)] (when (not-empty-string? s) s))
      s)))

(defn keyword-from-param
  "Takes a value `s` (which should be convertable to a keyword) and returns a keyword
  with a name having first colon character removed in pre-parsing phase. Returns
  `nil` for falsy values or empty strings (including empty strings after trimming the
  `:`)."
  ^Keyword [s]
  (if (keyword? s)
    s
    (when-some [^String s (some-str s)]
      (if (= \: (.charAt s 0))
        (let [^String s (subs s 1)] (when (not-empty-string? s) (keyword s)))
        (keyword s)))))

(defn try-namespace
  "If the given `v` is an ident, it returns its namespace. Otherwise it returns `v`."
  [v]
  (if (ident? v) (namespace v) v))

(defn try-name
  "If the given `v` is an ident, it returns its name. Otherwise it returns `v`."
  [v]
  (if (ident? v) (name v) v))

;; Date and time

(defn rfc1123-date-time
  "Returns a date and time formatted according to the RFC 1123."
  [t]
  (when t
    (some-str
     (t/format DateTimeFormatter/RFC_1123_DATE_TIME (t/zoned-date-time t)))))

(defn timeout?
  "Returns `true` if the given duration is negative or 0, `false` otherwise. Uses
  milliseconds. Returns `false` when `nil` or `false` is given."
  [duration]
  (if duration
    (if-some [millis (t/millis duration)]
      (not (pos? millis))
      false)
    false))

(defn simple-duration
  "Calculates the duration between the time of calling the function (or the given
  time `begin`) till the given time `end`, with nanoseconds set to 0."
  ([end]       (when end (.withNanos ^Duration (t/between (t/now) end) 0)))
  ([begin end] (when end (.withNanos ^Duration (t/between (or begin (t/now)) end) 0))))

(defn retry-in-mins
  "Calculates minutes of duration on a basis of `Duration` object. Returns an integer
  number of minutes left or `nil` when there was no duration given or the duration is
  negative. May return 0 if there is less than 1 minute left. Used to calculate retry
  timeouts to report them to a user."
  [duration]
  (when duration
    (when-some [millis (t/millis duration)]
      (if (neg? millis) nil (t/minutes duration)))))

;; URLs

(defn is-url?
  "Returns `true` if the given argument `s` is a non-empty string that begins like an
  URL. Returns `false` otherwise."
  ^Boolean [s]
  (when (string? s)
    (let [^String s s
          len (.length s)]
      (when (pos? len)
        (let [c0 (.charAt s 0)]
          (when (not= c0 \/)
            (loop [i 0]
              (let [c (.charAt s i)
                    x (int c)]
                (cond
                  ;; colon ends scheme-like
                  (= c \:) true

                  ;; A-Z a-z 0-9 + . -
                  (or (<= 48 x 57)     ; 0-9
                      (<= 65 x 90)     ; A-Z
                      (<= 97 x 122)    ; a-z
                      (= 43 x)         ; +
                      (= 46 x)         ; .
                      (= 45 x))        ; -
                  (let [i' (inc i)]
                    (and (< i' len) (recur i')))

                  :else false)))))))))
