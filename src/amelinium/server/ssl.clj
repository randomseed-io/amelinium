(ns

    ^{:doc    "amelinium service, SSL configuration."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.server.ssl

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string             :as        str]
            [clojure.java.io            :as         io]
            [amelinium.system           :as     system]
            [io.randomseed.utils        :refer    :all]
            [io.randomseed.utils.fs     :as         fs]
            [io.randomseed.utils.map    :as        map]
            [io.randomseed.utils.var    :as        var]
            [io.randomseed.utils.crypto :as     crypto])

  (:import [java.io File FileInputStream IOException]
           [java.security KeyStore]
           [java.security.cert Certificate]
           [javax.security.cert X509Certificate]
           [javax.security.auth.x500 X500Principal]
           [io.undertow.server HttpServerExchange ConnectionSSLSessionInfo]
           [io.undertow.server.protocol.http HttpServerConnection]))

;; Keystore and truststore handling

(def ^:const keystore-types
  {"p12"  "PKCS12"
   "pkcs" "PKCS12"})

(defonce keystore-password   (atom nil))
(defonce truststore-password (atom nil))

(defn reset-password!
  [pwd-atom]
  (reset! pwd-atom nil))

(defn get-pass-once
  (^String [^clojure.lang.Atom pwd-atom]
   (get-pass-once pwd-atom nil))
  (^String [^clojure.lang.Atom pwd-atom prompt]
   (if-some [p @pwd-atom] p (reset! pwd-atom (crypto/read-pwd (or prompt "Enter passphrase:"))))))

(defn ask-pass
  (^String [path pwd-atom]
   (ask-pass path pwd-atom nil))
  (^String [path pwd-atom ready]
   (if path
     (if (some? ready)
       (reset! pwd-atom (str ready))
       (get-pass-once pwd-atom (str-spc "Enter" (fs/basename path) "passphrase:"))))))

(defn ask-pass-keystore
  (^String [path]
   (ask-pass path keystore-password))
  (^String [path ready]
   (ask-pass path keystore-password ready)))

(defn ask-pass-truststore
  (^String [path]
   (ask-pass path truststore-password))
  (^String [path ready]
   (ask-pass path truststore-password ready)))

(defn reset-keystore-pass!   [] (reset-password!   keystore-password))
(defn reset-truststore-pass! [] (reset-password! truststore-password))

(defn reset-pass!
  []
  (reset-keystore-pass!)
  (reset-truststore-pass!))

(defn keystore-type
  [path]
  (if-some [ext (fs/extension path)]
    (get (str/upper-case ext) keystore-types "PKCS12")))

(defn new-store
  (^KeyStore []
   (KeyStore/getInstance "PKCS12"))
  (^KeyStore [path pwd-atom prompt ks-type]
   (try
     (new-store path (get-pass-once pwd-atom) ks-type)
     (catch IOException e
       (reset-password! pwd-atom)
       (throw e))))
  (^KeyStore [path password ks-type]
   (if (atom? password)
     (try
       (new-store path @password ks-type)
       (catch IOException e
         (reset-password! password)
         (throw e)))
     (if-some [^FileInputStream fis (some-> path fs/resource-file io/input-stream)]
       (let [ks-type (or (some-str ks-type) (keystore-type path))]
         (if-some [p (some-str password)]
           (doto ^KeyStore (new-store) (.load ^FileInputStream fis ^chars (char-array p)))
           (doto ^KeyStore (new-store) (.load ^FileInputStream fis))))))))

(defn kprompt [n] (str-spc "Enter" (fs/basename n) "passphrase:"))

(defn keystore
  (^KeyStore []                      (new-store))
  (^KeyStore [path]                  (new-store path keystore-password (kprompt path) nil))
  (^KeyStore [path password]         (new-store path password nil))
  (^KeyStore [path password ks-type] (new-store path password ks-type)))

(defn truststore
  (^KeyStore []                      (new-store))
  (^KeyStore [path]                  (new-store path truststore-password (kprompt path) nil))
  (^KeyStore [path password]         (new-store path password nil))
  (^KeyStore [path password ks-type] (new-store path password ks-type)))

;; SSL session information

(defn session-info-core
  [req]
  (if-some [ex (get req :server-exchange)]
    (.getSslSessionInfo ^HttpServerConnection (.getConnection ^HttpServerExchange ex))))

(defn session-info
  [req]
  (if-some [si (session-info-core req)]
    {:cipher-suite       ^String          (.getCipherSuite          ^ConnectionSSLSessionInfo si)
     :certificate        ^X509Certificate (.getPeerCertificateChain ^ConnectionSSLSessionInfo si)
     :client-certificate ^Certificate     (.getPeerCertificates     ^ConnectionSSLSessionInfo si)
     :id                 ^bytes           (.getSessionId            ^ConnectionSSLSessionInfo si)}))

(defn client-certificates
  [req]
  (if-some [si (session-info-core req)]
    (.getPeerCertificates ^ConnectionSSLSessionInfo si)))

(defn client-certificate
  [req]
  (or (get req :ssl-client-cert)
      (nth (client-certificates req) 0 nil)))

(defn client-certificate-subject
  [req]
  (if-some [cc (client-certificate req)]
    (.getSubjectX500Principal ^X509Certificate cc)))

(defn client-certificate-subject-canonical
  [req]
  (if-some [cc (client-certificate req)]
    (some-str
     (.getName ^X500Principal (.getSubjectX500Principal ^X509Certificate cc)
               X500Principal/CANONICAL))))

(defn client-certificate-subject-rfc1779
  [req]
  (if-some [cc (client-certificate req)]
    (some-str
     (.getName ^X500Principal (.getSubjectX500Principal ^X509Certificate cc)
               X500Principal/RFC1779))))

(defn client-certificate-subject-rfc2253
  [req]
  (if-some [cc (client-certificate req)]
    (some-str
     (.getName ^X500Principal (.getSubjectX500Principal ^X509Certificate cc)
               X500Principal/RFC2253))))
