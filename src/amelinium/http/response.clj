(ns

    ^{:doc    "Common HTTP responses for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.response

  (:require [clojure.set                 :as             set]
            [clojure.string              :as             str]
            [potemkin.namespaces         :as               p]
            [ring.util.response]
            [ring.util.http-response     :as            resp]
            [ring.util.request           :as             req]
            [amelinium                   :refer         :all]
            [amelinium.types.response    :refer         :all]
            [io.randomseed.utils.map     :as             map]
            [io.randomseed.utils.map     :refer     [qassoc]])

  (:import [amelinium Response]))

;; Imports

(p/import-vars [ring.util.http-response
                throw! status header file-response content-type
                find-header get-header update-header
                charset get-charset set-cookie resource-data
                url-response resource-response])

;; Response testing

(defn response?
  [req]
  (instance? Response req))

;; HTTP responses

(defn continue
  "100 Continue (Informational). The server has received the request headers and the
  client should proceed to send the request body."
  ([]                 (->Response 100 {} ""))
  ([_]                (->Response 100 {} ""))
  ([_ headers]        (->Response 100 headers "")))

(defn switching-protocols
  "101 Switching Protocols (Informational). The server is switching protocols because
  the client requested the switch."
  ([]                 (->Response 101 {} ""))
  ([_]                (->Response 101 {} ""))
  ([_ headers]        (->Response 101 headers "")))

(defn processing
  "102 Processing (Informational). The server is processing the request but no response
  is available yet."
  ([]                 (->Response 102 {} ""))
  ([_]                (->Response 102 {} ""))
  ([_ headers]        (->Response 102 headers "")))

(defn early-hints
  "103 Early Hints (Informational). The server sends some response headers (e.g. HTML
  resource links) before final HTTP message."
  ([]                 (->Response 103 {} ""))
  ([body]             (->Response 103 {} body))
  ([body headers]     (->Response 103 headers body)))

(defn ok
  "200 OK (Success). OK, resource found."
  ([]                 (->Response 200 {} nil))
  ([body]             (->Response 200 {} body))
  ([body headers]     (->Response 200 headers body)))

(defn created
  "201 Created (Success). The request has been fulfilled and resulted in a new resource
  being created."
  ([]                 (->Response 201 nil nil))
  ([body]             (->Response 201 {} body))
  ([body headers]     (->Response 201 headers body))
  ([body headers url] (->Response 201 (if headers (qassoc headers "Location" url) {"Location" url}) body)))

(defn accepted
  "202 Accepted (Success). The request has been accepted for processing but the
  processing has not been completed."
  ([]                 (->Response 202 {} nil))
  ([body]             (->Response 202 {} body))
  ([body headers]     (->Response 202 headers body)))

(defn non-authoritative-information
  "203 Non-Authoritative Information (Success). The server successfully processed the
  request but is returning information that may be from another source."
  ([]                 (->Response 203 {} nil))
  ([body]             (->Response 203 {} body))
  ([body headers]     (->Response 203 headers body)))

(defn no-content
  "204 No Content (Success). The server successfully processed the request, but is not
  returning any content. Usually used as a response to a successful delete request."
  ([]                 (->Response 204 {} ""))
  ([_]                (->Response 204 {} ""))
  ([_ headers]        (->Response 204 headers "")))

(defn reset-content
  "205 Reset Content (Success). The server successfully processed the request but is
  not returning any content. Unlike a 204 response, this response requires that the
  requester reset the document view."
  ([]                 (->Response 205 {} ""))
  ([_]                (->Response 205 {} ""))
  ([_ headers]        (->Response 205 headers "")))

(defn partial-content
  "206 Partial Content (Success). The server is delivering only part of the resource
  due to a range header sent by the client."
  ([]                 (->Response 206 {} nil))
  ([body]             (->Response 206 {} body))
  ([body headers]     (->Response 206 headers body)))

(defn multi-status
  "207 Multi-Status (Success). The message body that follows is an XML message and can
  contain a number of separate response codes depending on how many sub-requests were
  made."
  ([]                 (->Response 207 {} nil))
  ([body]             (->Response 207 {} body))
  ([body headers]     (->Response 207 headers body)))

(defn already-reported
  "208 Already Reported (Success). The members of a DAV binding have already been
  enumerated in a previous reply to this request and are not being included again."
  ([]                 (->Response 208 {} nil))
  ([body]             (->Response 208 {} body))
  ([body headers]     (->Response 208 headers body)))

(defn im-used
  "226 IM Used (Success). The server has fulfilled a GET request for the resource and
  the response is a representation of the result of one or more
  instance-manipulations applied to the current instance."
  ([]                 (->Response 226 {} nil))
  ([body]             (->Response 226 {} body))
  ([body headers]     (->Response 226 headers body)))

(defn multiple-choices
  "300 Multiple Choices (Redirection). There are multiple options for the resource that
  the client may follow."
  ([url]               (->Response 300 {"Location" url} ""))
  ([url _]             (->Response 300 {"Location" url} ""))
  ([url _ headers]     (->Response 300 (qassoc headers "Location" url) "")))

(defn moved-permanently
  "301 Moved Permanently (Redirection). This and all future requests should be directed
  to the given URI."
  ([url]               (->Response 301 {"Location" url} ""))
  ([url _]             (->Response 301 {"Location" url} ""))
  ([url _ headers]     (->Response 301 (qassoc headers "Location" url) "")))

(defn found
  "302 Found (Redirection). The resource was found but at a different URI."
  ([url]               (->Response 302 {"Location" url} ""))
  ([url _]             (->Response 302 {"Location" url} ""))
  ([url _ headers]     (->Response 302 (qassoc headers "Location" url) "")))

(defn see-other
  "303 See Other (Redirection). The response to the request can be found under another
  URI using a GET method."
  ([url]               (->Response 303 {"Location" url} ""))
  ([url _]             (->Response 303 {"Location" url} ""))
  ([url _ headers]     (->Response 303 (qassoc headers "Location" url) "")))

(defn not-modified
  "304 Not Modified (Redirection). The resource has not been modified since last
  requested."
  ([]                  (->Response 304 {} ""))
  ([_]                 (->Response 304 {} ""))
  ([_ headers]         (->Response 304 headers "")))

(defn use-proxy
  "305 Use Proxy (Redirection). This single request is to be repeated via the proxy
  given by the Location field."
  ([url]               (->Response 305 {"Location" url} ""))
  ([url _]             (->Response 305 {"Location" url} ""))
  ([url _ headers]     (->Response 305 (qassoc headers "Location" url) "")))

(defn temporary-redirect
  "307 Temporary Redirect (Redirection). The request should be repeated with another
  URI but future requests can still use the original URI."
  ([url]               (->Response 307 {"Location" url} ""))
  ([url _]             (->Response 307 {"Location" url} ""))
  ([url _ headers]     (->Response 307 (qassoc headers "Location" url) "")))

(defn permanent-redirect
  "308 Permanent Redirect (Redirection). The request and all future requests should be
  repeated using another URI."
  ([url]               (->Response 308 {"Location" url} ""))
  ([url _]             (->Response 308 {"Location" url} ""))
  ([url _ headers]     (->Response 308 (qassoc headers "Location" url) "")))

(defn bad-request
  "400 Bad Request (Client Error). The request contains bad syntax or cannot be
  fulfilled."
  ([]                 (->Response 400 {} nil))
  ([body]             (->Response 400 {} body))
  ([body headers]     (->Response 400 headers body)))

(defn bad-request!
  "400 Bad Request (Client Error). The request contains bad syntax or cannot be
  fulfilled. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 400 {} nil)))
  ([body]             (throw! (->Response 400 {} body)))
  ([body headers]     (throw! (->Response 400 headers body))))

(defn unauthorized
  "401 Unauthorized (Client Error). Authentication is possible but has failed or not
  yet been provided."
  ([]                 (->Response 401 {} nil))
  ([body]             (->Response 401 {} body))
  ([body headers]     (->Response 401 headers body)))

(defn unauthorized!
  "401 Unauthorized (Client Error). Authentication is possible but has failed or not
  yet been provided. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 401 {} nil)))
  ([body]             (throw! (->Response 401 {} body)))
  ([body headers]     (throw! (->Response 401 headers body))))

(defn payment-required
  "402 Payment Required (Client Error). Reserved for future use."
  ([]                 (->Response 402 {} nil))
  ([body]             (->Response 402 {} body))
  ([body headers]     (->Response 402 headers body)))

(defn payment-required!
  "402 Payment Required (Client Error). Reserved for future use.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 402 {} nil)))
  ([body]             (throw! (->Response 402 {} body)))
  ([body headers]     (throw! (->Response 402 headers body))))

(defn forbidden
  "403 Forbidden (Client Error). The request was a legal request but the server is
  refusing to respond to it."
  ([]                 (->Response 403 {} nil))
  ([body]             (->Response 403 {} body))
  ([body headers]     (->Response 403 headers body)))

(defn forbidden!
  "403 Forbidden (Client Error)
  The request was a legal request but the server is refusing to respond to it.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 403 {} nil)))
  ([body]             (throw! (->Response 403 {} body)))
  ([body headers]     (throw! (->Response 403 headers body))))

(defn not-found
  "404 Not Found (Client Error). The requested resource could not be found but may be
  available again in the future."
  ([]                 (->Response 404 {} nil))
  ([body]             (->Response 404 {} body))
  ([body headers]     (->Response 404 headers body)))

(defn not-found!
  "404 Not Found (Client Error) The requested resource could not be found but may be
  available again in the future. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 404 {} nil)))
  ([body]             (throw! (->Response 404 {} body)))
  ([body headers]     (throw! (->Response 404 headers body))))

(defn method-not-allowed
  "405 Method Not Allowed (Client Error). A request was made of a resource using a
  request method not supported by that resource;"
  ([]                 (->Response 405 {} nil))
  ([body]             (->Response 405 {} body))
  ([body headers]     (->Response 405 headers body)))

(defn method-not-allowed!
  "405 Method Not Allowed (Client Error)
  A request was made of a resource using a request method not supported by that resource;
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 405 {} nil)))
  ([body]             (throw! (->Response 405 {} body)))
  ([body headers]     (throw! (->Response 405 headers body))))

(defn not-acceptable
  "406 Not Acceptable (Client Error). The requested resource is only capable of
  generating content not acceptable according to the Accept headers sent in the
  request."
  ([]                 (->Response 406 {} nil))
  ([body]             (->Response 406 {} body))
  ([body headers]     (->Response 406 headers body)))

(defn not-acceptable!
  "406 Not Acceptable (Client Error). The requested resource is only capable of
  generating content not acceptable according to the Accept headers sent in the
  request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 406 {} nil)))
  ([body]             (throw! (->Response 406 {} body)))
  ([body headers]     (throw! (->Response 406 headers body))))

(defn proxy-authentication-required
  "407 Proxy Authentication Required (Client Error). Proxy authentication is required
  to access the requested resource."
  ([]                 (->Response 407 {} nil))
  ([body]             (->Response 407 {} body))
  ([body headers]     (->Response 407 headers body)))

(defn proxy-authentication-required!
  "407 Proxy Authentication Required (Client Error). Proxy authentication is required
  to access the requested resource. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 407 {} nil)))
  ([body]             (throw! (->Response 407 {} body)))
  ([body headers]     (throw! (->Response 407 headers body))))

(defn request-timeout
  "408 Request Timeout (Client Error). The server timed out waiting for the request."
  ([]                 (->Response 408 {} nil))
  ([body]             (->Response 408 {} body))
  ([body headers]     (->Response 408 headers body)))

(defn request-timeout!
  "408 Request Timeout (Client Error). The server timed out waiting for the request.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 408 {} nil)))
  ([body]             (throw! (->Response 408 {} body)))
  ([body headers]     (throw! (->Response 408 headers body))))

(defn conflict
  "409 Conflict (Client Error). The request could not be processed because of conflict
  in the request such as an edit conflict."
  ([]                 (->Response 409 {} nil))
  ([body]             (->Response 409 {} body))
  ([body headers]     (->Response 409 headers body)))

(defn conflict!
  "409 Conflict (Client Error). The request could not be processed because of conflict
  in the request such as an edit conflict. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 409 {} nil)))
  ([body]             (throw! (->Response 409 {} body)))
  ([body headers]     (throw! (->Response 409 headers body))))

(defn gone
  "410 Gone (Client Error). The resource requested is no longer available and will not
  be available again."
  ([]                 (->Response 410 {} nil))
  ([body]             (->Response 410 {} body))
  ([body headers]     (->Response 410 headers body)))

(defn gone!
  "410 Gone (Client Error). The resource requested is no longer available and will not
  be available again. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 410 {} nil)))
  ([body]             (throw! (->Response 410 {} body)))
  ([body headers]     (throw! (->Response 410 headers body))))

(defn length-required
  "411 Length Required (Client Error). The request did not specify the length of its
  content which is required by the requested resource."
  ([]                 (->Response 411 {} nil))
  ([body]             (->Response 411 {} body))
  ([body headers]     (->Response 411 headers body)))

(defn length-required!
  "411 Length Required (Client Error). The request did not specify the length of its
  content which is required by the requested resource. Throws an exception with
  ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 411 {} nil)))
  ([body]             (throw! (->Response 411 {} body)))
  ([body headers]     (throw! (->Response 411 headers body))))

(defn precondition-failed
  "412 Precondition Failed (Client Error). The server does not meet one of the
  preconditions that the requester put on the request."
  ([]                 (->Response 412 {} nil))
  ([body]             (->Response 412 {} body))
  ([body headers]     (->Response 412 headers body)))

(defn precondition-failed!
  "412 Precondition Failed (Client Error). The server does not meet one of the
  preconditions that the requester put on the request. Throws an exception with
  ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 412 {} nil)))
  ([body]             (throw! (->Response 412 {} body)))
  ([body headers]     (throw! (->Response 412 headers body))))

(defn request-entity-too-large
  "413 Request Entity Too Large (Client Error). The request is larger than the server
  is willing or able to process."
  ([]                 (->Response 413 {} nil))
  ([body]             (->Response 413 {} body))
  ([body headers]     (->Response 413 headers body)))

(defn request-entity-too-large!
  "413 Request Entity Too Large (Client Error). The request is larger than the server
  is willing or able to process. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 413 {} nil)))
  ([body]             (throw! (->Response 413 {} body)))
  ([body headers]     (throw! (->Response 413 headers body))))

(defn request-uri-too-long
  "414 Request-URI Too Long (Client Error). The URI provided was too long for the
  server to process."
  ([]                 (->Response 414 {} nil))
  ([body]             (->Response 414 {} body))
  ([body headers]     (->Response 414 headers body)))

(defn request-uri-too-long!
  "414 Request-URI Too Long (Client Error). The URI provided was too long for the server to process.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 414 {} nil)))
  ([body]             (throw! (->Response 414 {} body)))
  ([body headers]     (throw! (->Response 414 headers body))))

(defn unsupported-media-type
  "415 Unsupported Media Type (Client Error). The request entity has a media type which
  the server or resource does not support."
  ([]                 (->Response 415 {} nil))
  ([body]             (->Response 415 {} body))
  ([body headers]     (->Response 415 headers body)))

(defn unsupported-media-type!
  "415 Unsupported Media Type (Client Error). The request entity has a media type
  which the server or resource does not support.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 415 {} nil)))
  ([body]             (throw! (->Response 415 {} body)))
  ([body headers]     (throw! (->Response 415 headers body))))

(defn requested-range-not-satisfiable
  "416 Requested Range Not Satisfiable (Client Error). The client has asked for a
  portion of the file but the server cannot supply that portion."
  ([]                 (->Response 416 {} nil))
  ([body]             (->Response 416 {} body))
  ([body headers]     (->Response 416 headers body)))

(defn requested-range-not-satisfiable!
  "416 Requested Range Not Satisfiable (Client Error). The client has asked for a
  portion of the file but the server cannot supply that portion. Throws an exception
  with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 416 {} nil)))
  ([body]             (throw! (->Response 416 {} body)))
  ([body headers]     (throw! (->Response 416 headers body))))

(defn expectation-failed
  "417 Expectation Failed (Client Error). The server cannot meet the requirements of
  the Expect request-header field."
  ([]                 (->Response 417 {} nil))
  ([body]             (->Response 417 {} body))
  ([body headers]     (->Response 417 headers body)))

(defn expectation-failed!
  "417 Expectation Failed (Client Error). The server cannot meet the requirements of
  the Expect request-header field. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 417 {} nil)))
  ([body]             (throw! (->Response 417 {} body)))
  ([body headers]     (throw! (->Response 417 headers body))))

(defn im-a-teapot
  "418 I'm a teapot (Client Error). The server cannot brew coffee because it is,
  permanently, a teapot."
  ([]                 (->Response 418 {} nil))
  ([body]             (->Response 418 {} body))
  ([body headers]     (->Response 418 headers body)))

(defn im-a-teapot!
  "418 I'm a teapot (Client Error). The server cannot brew coffee because it is,
  permanently, a teapot. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 418 {} nil)))
  ([body]             (throw! (->Response 418 {} body)))
  ([body headers]     (throw! (->Response 418 headers body))))

(defn enhance-your-calm
  "420 Enhance Your Calm (Client Error). You are being rate-limited."
  ([]                 (->Response 420 {} nil))
  ([body]             (->Response 420 {} body))
  ([body headers]     (->Response 420 headers body)))

(defn enhance-your-calm!
  "420 Enhance Your Calm (Client Error). You are being rate-limited.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 420 {} nil)))
  ([body]             (throw! (->Response 420 {} body)))
  ([body headers]     (throw! (->Response 420 headers body))))

(defn misdirected-request
  "421 Misdirected Request (Client Error). The request was directed at a server that is
  not able to produce a response (e.g. network balancer forwarded traffic to a wrong
  server)."
  ([]                 (->Response 421 {} nil))
  ([body]             (->Response 421 {} body))
  ([body headers]     (->Response 421 headers body)))

(defn misdirected-request!
  "421 Misdirected Request (Client Error). The request was directed at a server that is
  not able to produce a response (e.g. network balancer forwarded traffic to a wrong
  server). Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 421 {} nil)))
  ([body]             (throw! (->Response 421 {} body)))
  ([body headers]     (throw! (->Response 421 headers body))))

(defn unprocessable-entity
  "422 Unprocessable Entity (Client Error). The request was well-formed but was unable
  to be followed due to semantic errors."
  ([]                 (->Response 422 {} nil))
  ([body]             (->Response 422 {} body))
  ([body headers]     (->Response 422 headers body)))

(defn unprocessable-entity!
  "422 Unprocessable Entity (Client Error). The request was well-formed but was
  unable to be followed due to semantic errors. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 422 {} nil)))
  ([body]             (throw! (->Response 422 {} body)))
  ([body headers]     (throw! (->Response 422 headers body))))

(defn locked
  "423 Locked (Client Error). The resource that is being accessed is locked."
  ([]                 (->Response 423 {} nil))
  ([body]             (->Response 423 {} body))
  ([body headers]     (->Response 423 headers body)))

(defn locked!
  "423 Locked (Client Error). The resource that is being accessed is locked.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 423 {} nil)))
  ([body]             (throw! (->Response 423 {} body)))
  ([body headers]     (throw! (->Response 423 headers body))))

(defn failed-dependency
  "424 Failed Dependency (Client Error). The request failed due to failure of a previous
  request."
  ([]                 (->Response 424 {} nil))
  ([body]             (->Response 424 {} body))
  ([body headers]     (->Response 424 headers body)))

(defn failed-dependency!
  "424 Failed Dependency (Client Error). The request failed due to failure of
  a previous request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 424 {} nil)))
  ([body]             (throw! (->Response 424 {} body)))
  ([body headers]     (throw! (->Response 424 headers body))))

(defn unordered-collection
  "425 Unordered Collection (Client Error). The collection is unordered."
  ([]                 (->Response 425 {} nil))
  ([body]             (->Response 425 {} body))
  ([body headers]     (->Response 425 headers body)))

(defn unordered-collection!
  "425 Unordered Collection (Client Error). The collection is unordered.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 425 {} nil)))
  ([body]             (throw! (->Response 425 {} body)))
  ([body headers]     (throw! (->Response 425 headers body))))

(defn upgrade-required
  "426 Upgrade Required (Client Error). The client should switch to a different
  protocol."
  ([]                 (->Response 426 {} nil))
  ([body]             (->Response 426 {} body))
  ([body headers]     (->Response 426 headers body)))

(defn upgrade-required!
  "426 Upgrade Required (Client Error). The client should switch to a different
  protocol. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 426 {} nil)))
  ([body]             (throw! (->Response 426 {} body)))
  ([body headers]     (throw! (->Response 426 headers body))))

(defn precondition-required
  "428 Precondition Required (Client Error). The server requires the request to be
  conditional."
  ([]                 (->Response 428 {} nil))
  ([body]             (->Response 428 {} body))
  ([body headers]     (->Response 428 headers body)))

(defn precondition-required!
  "428 Precondition Required (Client Error). The server requires the request to be
  conditional. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 428 {} nil)))
  ([body]             (throw! (->Response 428 {} body)))
  ([body headers]     (throw! (->Response 428 headers body))))

(defn too-many-requests
  "429 Too Many Requests (Client Error). The user has sent too many requests in a given
  amount of time."
  ([]                 (->Response 429 {} nil))
  ([body]             (->Response 429 {} body))
  ([body headers]     (->Response 429 headers body)))

(defn too-many-requests!
  "429 Too Many Requests (Client Error). The user has sent too many requests in a
  given amount of time. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 429 {} nil)))
  ([body]             (throw! (->Response 429 {} body)))
  ([body headers]     (throw! (->Response 429 headers body))))

(defn request-header-fields-too-large
  "431 Request Header Fields Too Large (Client Error). The server is unwilling to
  process the request because either an individual header field or all the header
  fields collectively are too large."
  ([]                 (->Response 431 {} nil))
  ([body]             (->Response 431 {} body))
  ([body headers]     (->Response 431 headers body)))

(defn request-header-fields-too-large!
  "431 Request Header Fields Too Large (Client Error). The server is unwilling to
  process the request because either an individual header field or all the header
  fields collectively are too large. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 431 {} nil)))
  ([body]             (throw! (->Response 431 {} body)))
  ([body headers]     (throw! (->Response 431 headers body))))

(defn retry-with
  "449 Retry With (Client Error). The request should be retried after doing the
  appropriate action."
  ([]                 (->Response 449 {} nil))
  ([body]             (->Response 449 {} body))
  ([body headers]     (->Response 449 headers body)))

(defn retry-with!
  "449 Retry With (Client Error). The request should be retried after doing
  the appropriate action. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 449 {} nil)))
  ([body]             (throw! (->Response 449 {} body)))
  ([body headers]     (throw! (->Response 449 headers body))))

(defn blocked-by-windows-parental-controls
  "450 Blocked by Windows Parental Controls (Client Error). Windows Parental Controls
  are turned on and are blocking access to the given web page."
  ([]                 (->Response 450 {} nil))
  ([body]             (->Response 450 {} body))
  ([body headers]     (->Response 450 headers body)))

(defn blocked-by-windows-parental-controls!
  "450 Blocked by Windows Parental Controls (Client Error). Windows Parental Controls
  are turned on and are blocking access to the given web page. Throws an exception
  with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 450 {} nil)))
  ([body]             (throw! (->Response 450 {} body)))
  ([body headers]     (throw! (->Response 450 headers body))))

(defn unavailable-for-legal-reasons
  "451 Unavailable For Legal Reasons (Client Error). Resource access is denied for
  legal reasons."
  ([]                 (->Response 451 {} nil))
  ([body]             (->Response 451 {} body))
  ([body headers]     (->Response 451 headers body)))

(defn unavailable-for-legal-reasons!
  "451 Unavailable For Legal Reasons (Client Error). Resource access is denied for
  legal reasons. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 451 {} nil)))
  ([body]             (throw! (->Response 451 {} body)))
  ([body headers]     (throw! (->Response 451 headers body))))

(defn internal-server-error
  "500 Internal Server Error (Server Error). There was an internal server error."
  ([]                 (->Response 500 {} nil))
  ([body]             (->Response 500 {} body))
  ([body headers]     (->Response 500 headers body)))

(defn internal-server-error!
  "500 Internal Server Error (Server Error). There was an internal server error.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 500 {} nil)))
  ([body]             (throw! (->Response 500 {} body)))
  ([body headers]     (throw! (->Response 500 headers body))))

(defn not-implemented
  "501 Not Implemented (Server Error). The server either does not recognize the request
  method or it lacks the ability to fulfill the request."
  ([]                 (->Response 501 {} nil))
  ([body]             (->Response 501 {} body))
  ([body headers]     (->Response 501 headers body)))

(defn not-implemented!
  "501 Not Implemented (Server Error). The server either does not recognize the request
  method or it lacks the ability to fulfill the request.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 501 {} nil)))
  ([body]             (throw! (->Response 501 {} body)))
  ([body headers]     (throw! (->Response 501 headers body))))

(defn bad-gateway
  "502 Bad Gateway (Server Error). The server was acting as a gateway or proxy and
  received an invalid response from the upstream server."
  ([]                 (->Response 502 {} nil))
  ([body]             (->Response 502 {} body))
  ([body headers]     (->Response 502 headers body)))

(defn bad-gateway!
  "502 Bad Gateway (Server Error). The server was acting as a gateway or proxy and
  received an invalid response from the upstream server. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 502 {} nil)))
  ([body]             (throw! (->Response 502 {} body)))
  ([body headers]     (throw! (->Response 502 headers body))))

(defn service-unavailable
  "503 Service Unavailable (Server Error). The server is currently unavailable (because
  it is overloaded or down for maintenance)."
  ([]                 (->Response 503 {} nil))
  ([body]             (->Response 503 {} body))
  ([body headers]     (->Response 503 headers body)))

(defn service-unavailable!
  "503 Service Unavailable (Server Error). The server is currently unavailable (because
  it is overloaded or down for maintenance). Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 503 {} nil)))
  ([body]             (throw! (->Response 503 {} body)))
  ([body headers]     (throw! (->Response 503 headers body))))

(defn gateway-timeout
  "504 Gateway Timeout (Server Error). The server was acting as a gateway or proxy
  and did not receive a timely request from the upstream server."
  ([]                 (->Response 504 {} nil))
  ([body]             (->Response 504 {} body))
  ([body headers]     (->Response 504 headers body)))

(defn gateway-timeout!
  "504 Gateway Timeout (Server Error). The server was acting as a gateway or proxy
  and did not receive a timely request from the upstream server.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 504 {} nil)))
  ([body]             (throw! (->Response 504 {} body)))
  ([body headers]     (throw! (->Response 504 headers body))))

(defn http-version-not-supported
  "505 HTTP Version Not Supported (Server Error). The server does not support the
  HTTP protocol version used in the request."
  ([]                 (->Response 505 {} nil))
  ([body]             (->Response 505 {} body))
  ([body headers]     (->Response 505 headers body)))

(defn http-version-not-supported!
  "505 HTTP Version Not Supported (Server Error). The server does not support the
  HTTP protocol version used in the request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 505 {} nil)))
  ([body]             (throw! (->Response 505 {} body)))
  ([body headers]     (throw! (->Response 505 headers body))))

(defn variant-also-negotiates
  "506 Variant Also Negotiates (Server Error). Transparent content negotiation for
  the request results in a circular reference."
  ([]                 (->Response 506 {} nil))
  ([body]             (->Response 506 {} body))
  ([body headers]     (->Response 506 headers body)))

(defn variant-also-negotiates!
  "506 Variant Also Negotiates (Server Error). Transparent content negotiation for
  the request results in a circular reference. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 506 {} nil)))
  ([body]             (throw! (->Response 506 {} body)))
  ([body headers]     (throw! (->Response 506 headers body))))

(defn insufficient-storage
  "507 Insufficient Storage (Server Error). Insufficient storage to complete the
  request."
  ([]                 (->Response 507 {} nil))
  ([body]             (->Response 507 {} body))
  ([body headers]     (->Response 507 headers body)))

(defn insufficient-storage!
  "507 Insufficient Storage (Server Error). Insufficient storage to complete the
  request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 507 {} nil)))
  ([body]             (throw! (->Response 507 {} body)))
  ([body headers]     (throw! (->Response 507 headers body))))

(defn loop-detected
  "508 Loop Detected (Server Error). The server detected an infinite loop while
  processing the request."
  ([]                 (->Response 508 {} nil))
  ([body]             (->Response 508 {} body))
  ([body headers]     (->Response 508 headers body)))

(defn loop-detected!
  "508 Loop Detected (Server Error). The server detected an infinite loop while
  processing the request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 508 {} nil)))
  ([body]             (throw! (->Response 508 {} body)))
  ([body headers]     (throw! (->Response 508 headers body))))

(defn bandwidth-limit-exceeded
  "509 Bandwidth Limit Exceeded (Server Error). Bandwidth limit has been exceeded."
  ([]                 (->Response 509 {} nil))
  ([body]             (->Response 509 {} body))
  ([body headers]     (->Response 509 headers body)))

(defn bandwidth-limit-exceeded!
  "509 Bandwidth Limit Exceeded (Server Error). Bandwidth limit has been exceeded.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 509 {} nil)))
  ([body]             (throw! (->Response 509 {} body)))
  ([body headers]     (throw! (->Response 509 headers body))))

(defn not-extended
  "510 Not Extended (Server Error). Further extensions to the request are required for
  the server to fulfill it."
  ([]                 (->Response 510 {} nil))
  ([body]             (->Response 510 {} body))
  ([body headers]     (->Response 510 headers body)))

(defn not-extended!
  "510 Not Extended (Server Error). Further extensions to the request are required for
  the server to fulfill it. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 510 {} nil)))
  ([body]             (throw! (->Response 510 {} body)))
  ([body headers]     (throw! (->Response 510 headers body))))

(defn network-authentication-required
  "511 Network Authentication Required (Server Error). The client needs to
  authenticate to gain network access."
  ([]                 (->Response 511 {} nil))
  ([body]             (->Response 511 {} body))
  ([body headers]     (->Response 511 headers body)))

(defn network-authentication-required!
  "511 Network Authentication Required (Server Error). The client needs to
  authenticate to gain network access. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 511 {} nil)))
  ([body]             (throw! (->Response 511 {} body)))
  ([body headers]     (throw! (->Response 511 headers body))))

(defn network-read-timeout
  "598 Network read timeout (Server Error)."
  ([]                 (->Response 598 {} nil))
  ([body]             (->Response 598 {} body))
  ([body headers]     (->Response 598 headers body)))

(defn network-read-timeout!
  "598 Network read timeout (Server Error). Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 598 {} nil)))
  ([body]             (throw! (->Response 598 {} body)))
  ([body headers]     (throw! (->Response 598 headers body))))

(defn network-connect-timeout
  "599 Network connect timeout (Server Error). Network connect timeout behind the
  proxy."
  ([]                 (->Response 599 {} nil))
  ([body]             (->Response 599 {} body))
  ([body headers]     (->Response 599 headers body)))

(defn network-connect-timeout!
  "599 Network connect timeout (Server Error). Network connect timeout behind the
  proxy. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                 (throw! (->Response 599 {} nil)))
  ([body]             (throw! (->Response 599 {} body)))
  ([body headers]     (throw! (->Response 599 headers body))))

;; Rendering

(defn render
  "Universal response renderer. Returns the result of calling the `resp-fn` with
  headers attached as the second argument (obtained from `:response/headers` key of
  the `req`) unless the `req` is already a valid response.

  Third argument is passed as a first argument to the response function. Further
  arguments are passed beginning from the third argument to the response function."
  ([resp-fn]
   (resp-fn))
  ([resp-fn req]
   (if (nil? req)
     (resp-fn)
     (if (response? req)
       req
       (if-some [headers (get req :response/headers)]
         (resp-fn nil headers)
         (resp-fn)))))
  ([resp-fn req a]
   (if (nil? req)
     (resp-fn a)
     (if (response? req)
       req
       (resp-fn a (or (get req :response/headers) {})))))
  ([resp-fn req a b]
   (if (nil? req)
     (resp-fn a {} b)
     (if (response? req)
       req
       (resp-fn a (or (get req :response/headers) {}) b))))
  ([resp-fn req a b c]
   (if (nil? req)
     (resp-fn a {} b c)
     (if (resp/response? req)
       req
       (resp-fn a (or (get req :response/headers) {}) b c))))
  ([resp-fn req a b c & more]
   (if (nil? req)
     (apply resp-fn a {} b c more)
     (if (resp/response? req)
       req
       (apply resp-fn a (or (get req :response/headers) {}) b c more)))))

(defn render-force
  "Universal body-less response renderer. Returns the result of calling the `resp-fn`
  with headers attached (from `:response/headers` key of the `req`). Arguments from
  the third are passed to `resp-fn` function. Will not skip response generation even
  if `req` is already a response."
  ([resp-fn]
   (resp-fn))
  ([resp-fn req]
   (if (nil? req)
     (resp-fn)
     (if-some [headers (get req :response/headers)]
       (resp-fn nil headers)
       (resp-fn))))
  ([resp-fn req a]
   (if (nil? req)
     (resp-fn a)
     (resp-fn a (or (get req :response/headers) {}))))
  ([resp-fn req a b]
   (if (nil? req)
     (resp-fn a {} b)
     (resp-fn a (or (get req :response/headers) {}) b)))
  ([resp-fn req a b c]
   (if (nil? req)
     (resp-fn a {} b c)
     (resp-fn a (or (get req :response/headers) {}) b c)))
  ([resp-fn req a b c & more]
   (if (nil? req)
     (apply resp-fn a {} b c more)
     (apply resp-fn a (or (get req :response/headers) {}) b c more))))
