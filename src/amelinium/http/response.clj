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
            [amelinium.types.response    :refer         :all]
            [amelinium.proto.http        :as            http]
            [amelinium                   :refer         :all]
            [io.randomseed.utils.map     :as             map]
            [io.randomseed.utils.map     :refer     [qassoc]])

  (:import (clojure.lang         IPersistentMap)
           (amelinium            Response)
           (amelinium.proto.http HTTP)))

;; Imports

(p/import-vars [ring.util.http-response
                throw! status header file-response content-type
                find-header get-header update-header
                charset get-charset set-cookie resource-data
                url-response resource-response])

;; Response testing

(defn response?
  "Returns `true` if the given value is a kind of `amelinium.Response`, `false`
  otherwise."
  ^Boolean [obj]
  (instance? Response obj))

;; HTTP protocol

(extend-protocol http/HTTP

  Response

  (request?           ^Boolean        [resp] false)
  (response?          ^Boolean        [resp] true)
  (app-status                         [resp] nil)
  (response-status                    [resp] (.status  ^Response resp))
  (response-headers   ^IPersistentMap [resp] (.headers ^Response resp))
  (response-body                      [resp] (.body    ^Response resp))
  (response-location
    ([resp]     (get (.headers ^Response resp) "Location"))
    ([resp _]   (get (.headers ^Response resp) "Location"))
    ([resp _ _] (get (.headers ^Response resp) "Location"))))

;; Response components

(defn headers
  "Returns response headers map from the response record or a request map."
  [src]
  (http/response-headers src))

(defn body
  "Returns response body from the response record or a request map."
  [src]
  (http/response-body src))

(defn status
  "Returns response status code from the response record. For a request map given as
  `src` it returns `nil`."
  [src]
  (http/response-status src))

(defn app-status?
  "Returns `true` if the given application status is not `nil` and equal to the
  application status obtained from the request's key `:app/status`. If the obtained
  application status is a set, it will check if for the existence of `app-status`
  within that set. If the values or not equal and there is not element in a set (in
  case of set), returns `false`."
  [src app-status]
  (if-some [st (http/response-status src)]
    (or (identical? st app-status)
        (and (set? st) (contains? st app-status)))
    false))

(defn location
  "Returns response location header. For a request map given as `src` returns a value
  associated with its `:response/location` key. In this case the following is applied:
  If it is found and it is a URL, it is returned. If it is found and it is a path or
  page identifier it is parsed with the function `f`. Optional `lang`, `params`,
  `qparams` and any other additional arguments are passed as arguments during the
  call to `f`. If there is no response location, `nil` is returned."
  ([src] (http/response-location src))
  ([src f] (http/response-location src f))
  ([src f lang] (http/response-location src f [lang]))
  ([src f lang params] (http/response-location src f [lang params]))
  ([src f lang params query-params] (http/response-location src f [lang params query-params]))
  ([src f lang params query-params & more] (http/response-location src f (list* lang params query-params more))))

;; HTTP responses

(defn continue
  "100 Continue (Informational). The server has received the request headers and the
  client should proceed to send the request body."
  (^Response []                 (->Response 100 {} ""))
  (^Response [_]                (->Response 100 {} ""))
  (^Response [_ headers]        (->Response 100 headers "")))

(defn switching-protocols
  "101 Switching Protocols (Informational). The server is switching protocols because
  the client requested the switch."
  (^Response []                 (->Response 101 {} ""))
  (^Response [_]                (->Response 101 {} ""))
  (^Response [_ headers]        (->Response 101 headers "")))

(defn processing
  "102 Processing (Informational). The server is processing the request but no response
  is available yet."
  (^Response []                 (->Response 102 {} ""))
  (^Response [_]                (->Response 102 {} ""))
  (^Response [_ headers]        (->Response 102 headers "")))

(defn early-hints
  "103 Early Hints (Informational). The server sends some response headers (e.g. HTML
  resource links) before final HTTP message."
  (^Response []                 (->Response 103 {} ""))
  (^Response [body]             (->Response 103 {} body))
  (^Response [body headers]     (->Response 103 headers body)))

(defn ok
  "200 OK (Success). OK, resource found."
  (^Response []                 (->Response 200 {} nil))
  (^Response [body]             (->Response 200 {} body))
  (^Response [body headers]     (->Response 200 headers body)))

(defn created
  "201 Created (Success). The request has been fulfilled and resulted in a new resource
  being created."
  (^Response []                 (->Response 201 nil nil))
  (^Response [body]             (->Response 201 {} body))
  (^Response [body headers]     (->Response 201 headers body))
  (^Response [body headers url] (->Response 201 (if headers (qassoc headers "Location" url) {"Location" url}) body)))

(defn accepted
  "202 Accepted (Success). The request has been accepted for processing but the
  processing has not been completed."
  (^Response []                 (->Response 202 {} nil))
  (^Response [body]             (->Response 202 {} body))
  (^Response [body headers]     (->Response 202 headers body)))

(defn non-authoritative-information
  "203 Non-Authoritative Information (Success). The server successfully processed the
  request but is returning information that may be from another source."
  (^Response []                 (->Response 203 {} nil))
  (^Response [body]             (->Response 203 {} body))
  (^Response [body headers]     (->Response 203 headers body)))

(defn no-content
  "204 No Content (Success). The server successfully processed the request, but is not
  returning any content. Usually used as a response to a successful delete request."
  (^Response []                 (->Response 204 {} ""))
  (^Response [_]                (->Response 204 {} ""))
  (^Response [_ headers]        (->Response 204 headers "")))

(defn reset-content
  "205 Reset Content (Success). The server successfully processed the request but is
  not returning any content. Unlike a 204 response, this response requires that the
  requester reset the document view."
  (^Response []                 (->Response 205 {} ""))
  (^Response [_]                (->Response 205 {} ""))
  (^Response [_ headers]        (->Response 205 headers "")))

(defn partial-content
  "206 Partial Content (Success). The server is delivering only part of the resource
  due to a range header sent by the client."
  (^Response []                 (->Response 206 {} nil))
  (^Response [body]             (->Response 206 {} body))
  (^Response [body headers]     (->Response 206 headers body)))

(defn multi-status
  "207 Multi-Status (Success). The message body that follows is an XML message and can
  contain a number of separate response codes depending on how many sub-requests were
  made."
  (^Response []                 (->Response 207 {} nil))
  (^Response [body]             (->Response 207 {} body))
  (^Response [body headers]     (->Response 207 headers body)))

(defn already-reported
  "208 Already Reported (Success). The members of a DAV binding have already been
  enumerated in a previous reply to this request and are not being included again."
  (^Response []                 (->Response 208 {} nil))
  (^Response [body]             (->Response 208 {} body))
  (^Response [body headers]     (->Response 208 headers body)))

(defn im-used
  "226 IM Used (Success). The server has fulfilled a GET request for the resource and
  the response is a representation of the result of one or more
  instance-manipulations applied to the current instance."
  (^Response []                 (->Response 226 {} nil))
  (^Response [body]             (->Response 226 {} body))
  (^Response [body headers]     (->Response 226 headers body)))

(defn multiple-choices
  "300 Multiple Choices (Redirection). There are multiple options for the resource that
  the client may follow."
  (^Response [url]               (->Response 300 {"Location" url} ""))
  (^Response [url _]             (->Response 300 {"Location" url} ""))
  (^Response [url _ headers]     (->Response 300 (qassoc headers "Location" url) "")))

(defn moved-permanently
  "301 Moved Permanently (Redirection). This and all future requests should be directed
  to the given URI."
  (^Response [url]               (->Response 301 {"Location" url} ""))
  (^Response [url _]             (->Response 301 {"Location" url} ""))
  (^Response [url _ headers]     (->Response 301 (qassoc headers "Location" url) "")))

(defn found
  "302 Found (Redirection). The resource was found but at a different URI."
  (^Response [url]               (->Response 302 {"Location" url} ""))
  (^Response [url _]             (->Response 302 {"Location" url} ""))
  (^Response [url _ headers]     (->Response 302 (qassoc headers "Location" url) "")))

(defn see-other
  "303 See Other (Redirection). The response to the request can be found under another
  URI using a GET method."
  (^Response [url]               (->Response 303 {"Location" url} ""))
  (^Response [url _]             (->Response 303 {"Location" url} ""))
  (^Response [url _ headers]     (->Response 303 (qassoc headers "Location" url) "")))

(defn not-modified
  "304 Not Modified (Redirection). The resource has not been modified since last
  requested."
  (^Response []                  (->Response 304 {} ""))
  (^Response [-]                 (->Response 304 {} ""))
  (^Response [_ headers]         (->Response 304 headers "")))

(defn use-proxy
  "305 Use Proxy (Redirection). This single request is to be repeated via the proxy
  given by the Location field."
  (^Response [url]               (->Response 305 {"Location" url} ""))
  (^Response [url _]             (->Response 305 {"Location" url} ""))
  (^Response [url _ headers]     (->Response 305 (qassoc headers "Location" url) "")))

(defn temporary-redirect
  "307 Temporary Redirect (Redirection). The request should be repeated with another
  URI but future requests can still use the original URI."
  (^Response [url]               (->Response 307 {"Location" url} ""))
  (^Response [url _]             (->Response 307 {"Location" url} ""))
  (^Response [url _ headers]     (->Response 307 (qassoc headers "Location" url) "")))

(defn permanent-redirect
  "308 Permanent Redirect (Redirection). The request and all future requests should be
  repeated using another URI."
  (^Response [url]               (->Response 308 {"Location" url} ""))
  (^Response [url _]             (->Response 308 {"Location" url} ""))
  (^Response [url _ headers]     (->Response 308 (qassoc headers "Location" url) "")))

(defn bad-request
  "400 Bad Request (Client Error). The request contains bad syntax or cannot be
  fulfilled."
  (^Response []                 (->Response 400 {} nil))
  (^Response [body]             (->Response 400 {} body))
  (^Response [body headers]     (->Response 400 headers body)))

(defn bad-request!
  "400 Bad Request (Client Error). The request contains bad syntax or cannot be
  fulfilled. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 400 {} nil)))
  ([body]               (throw! (->Response 400 {} body)))
  ([body headers]       (throw! (->Response 400 headers body))))

(defn unauthorized
  "401 Unauthorized (Client Error). Authentication is possible but has failed or not
  yet been provided."
  (^Response []                 (->Response 401 {} nil))
  (^Response [body]             (->Response 401 {} body))
  (^Response [body headers]     (->Response 401 headers body)))

(defn unauthorized!
  "401 Unauthorized (Client Error). Authentication is possible but has failed or not
  yet been provided. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 401 {} nil)))
  ([body]               (throw! (->Response 401 {} body)))
  ([body headers]       (throw! (->Response 401 headers body))))

(defn payment-required
  "402 Payment Required (Client Error). Reserved for future use."
  (^Response []                 (->Response 402 {} nil))
  (^Response [body]             (->Response 402 {} body))
  (^Response [body headers]     (->Response 402 headers body)))

(defn payment-required!
  "402 Payment Required (Client Error). Reserved for future use.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 402 {} nil)))
  ([body]               (throw! (->Response 402 {} body)))
  ([body headers]       (throw! (->Response 402 headers body))))

(defn forbidden
  "403 Forbidden (Client Error). The request was a legal request but the server is
  refusing to respond to it."
  (^Response []                 (->Response 403 {} nil))
  (^Response [body]             (->Response 403 {} body))
  (^Response [body headers]     (->Response 403 headers body)))

(defn forbidden!
  "403 Forbidden (Client Error)
  The request was a legal request but the server is refusing to respond to it.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 403 {} nil)))
  ([body]               (throw! (->Response 403 {} body)))
  ([body headers]       (throw! (->Response 403 headers body))))

(defn not-found
  "404 Not Found (Client Error). The requested resource could not be found but may be
  available again in the future."
  (^Response []                 (->Response 404 {} nil))
  (^Response [body]             (->Response 404 {} body))
  (^Response [body headers]     (->Response 404 headers body)))

(defn not-found!
  "404 Not Found (Client Error) The requested resource could not be found but may be
  available again in the future. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 404 {} nil)))
  ([body]               (throw! (->Response 404 {} body)))
  ([body headers]       (throw! (->Response 404 headers body))))

(defn method-not-allowed
  "405 Method Not Allowed (Client Error). A request was made of a resource using a
  request method not supported by that resource;"
  (^Response []                 (->Response 405 {} nil))
  (^Response [body]             (->Response 405 {} body))
  (^Response [body headers]     (->Response 405 headers body)))

(defn method-not-allowed!
  "405 Method Not Allowed (Client Error)
  A request was made of a resource using a request method not supported by that resource;
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 405 {} nil)))
  ([body]               (throw! (->Response 405 {} body)))
  ([body headers]       (throw! (->Response 405 headers body))))

(defn not-acceptable
  "406 Not Acceptable (Client Error). The requested resource is only capable of
  generating content not acceptable according to the Accept headers sent in the
  request."
  (^Response []                 (->Response 406 {} nil))
  (^Response [body]             (->Response 406 {} body))
  (^Response [body headers]     (->Response 406 headers body)))

(defn not-acceptable!
  "406 Not Acceptable (Client Error). The requested resource is only capable of
  generating content not acceptable according to the Accept headers sent in the
  request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 406 {} nil)))
  ([body]               (throw! (->Response 406 {} body)))
  ([body headers]       (throw! (->Response 406 headers body))))

(defn proxy-authentication-required
  "407 Proxy Authentication Required (Client Error). Proxy authentication is required
  to access the requested resource."
  (^Response []                 (->Response 407 {} nil))
  (^Response [body]             (->Response 407 {} body))
  (^Response [body headers]     (->Response 407 headers body)))

(defn proxy-authentication-required!
  "407 Proxy Authentication Required (Client Error). Proxy authentication is required
  to access the requested resource. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 407 {} nil)))
  ([body]               (throw! (->Response 407 {} body)))
  ([body headers]       (throw! (->Response 407 headers body))))

(defn request-timeout
  "408 Request Timeout (Client Error). The server timed out waiting for the request."
  (^Response []                 (->Response 408 {} nil))
  (^Response [body]             (->Response 408 {} body))
  (^Response [body headers]     (->Response 408 headers body)))

(defn request-timeout!
  "408 Request Timeout (Client Error). The server timed out waiting for the request.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 408 {} nil)))
  ([body]               (throw! (->Response 408 {} body)))
  ([body headers]       (throw! (->Response 408 headers body))))

(defn conflict
  "409 Conflict (Client Error). The request could not be processed because of conflict
  in the request such as an edit conflict."
  (^Response []                 (->Response 409 {} nil))
  (^Response [body]             (->Response 409 {} body))
  (^Response [body headers]     (->Response 409 headers body)))

(defn conflict!
  "409 Conflict (Client Error). The request could not be processed because of conflict
  in the request such as an edit conflict. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 409 {} nil)))
  ([body]               (throw! (->Response 409 {} body)))
  ([body headers]       (throw! (->Response 409 headers body))))

(defn gone
  "410 Gone (Client Error). The resource requested is no longer available and will not
  be available again."
  (^Response []                 (->Response 410 {} nil))
  (^Response [body]             (->Response 410 {} body))
  (^Response [body headers]     (->Response 410 headers body)))

(defn gone!
  "410 Gone (Client Error). The resource requested is no longer available and will not
  be available again. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 410 {} nil)))
  ([body]               (throw! (->Response 410 {} body)))
  ([body headers]       (throw! (->Response 410 headers body))))

(defn length-required
  "411 Length Required (Client Error). The request did not specify the length of its
  content which is required by the requested resource."
  (^Response []                 (->Response 411 {} nil))
  (^Response [body]             (->Response 411 {} body))
  (^Response [body headers]     (->Response 411 headers body)))

(defn length-required!
  "411 Length Required (Client Error). The request did not specify the length of its
  content which is required by the requested resource. Throws an exception with
  ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 411 {} nil)))
  ([body]               (throw! (->Response 411 {} body)))
  ([body headers]       (throw! (->Response 411 headers body))))

(defn precondition-failed
  "412 Precondition Failed (Client Error). The server does not meet one of the
  preconditions that the requester put on the request."
  (^Response []                 (->Response 412 {} nil))
  (^Response [body]             (->Response 412 {} body))
  (^Response [body headers]     (->Response 412 headers body)))

(defn precondition-failed!
  "412 Precondition Failed (Client Error). The server does not meet one of the
  preconditions that the requester put on the request. Throws an exception with
  ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 412 {} nil)))
  ([body]               (throw! (->Response 412 {} body)))
  ([body headers]       (throw! (->Response 412 headers body))))

(defn request-entity-too-large
  "413 Request Entity Too Large (Client Error). The request is larger than the server
  is willing or able to process."
  (^Response []                 (->Response 413 {} nil))
  (^Response [body]             (->Response 413 {} body))
  (^Response [body headers]     (->Response 413 headers body)))

(defn request-entity-too-large!
  "413 Request Entity Too Large (Client Error). The request is larger than the server
  is willing or able to process. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 413 {} nil)))
  ([body]               (throw! (->Response 413 {} body)))
  ([body headers]       (throw! (->Response 413 headers body))))

(defn request-uri-too-long
  "414 Request-URI Too Long (Client Error). The URI provided was too long for the
  server to process."
  (^Response []                 (->Response 414 {} nil))
  (^Response [body]             (->Response 414 {} body))
  (^Response [body headers]     (->Response 414 headers body)))

(defn request-uri-too-long!
  "414 Request-URI Too Long (Client Error). The URI provided was too long for the server to process.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 414 {} nil)))
  ([body]               (throw! (->Response 414 {} body)))
  ([body headers]       (throw! (->Response 414 headers body))))

(defn unsupported-media-type
  "415 Unsupported Media Type (Client Error). The request entity has a media type which
  the server or resource does not support."
  (^Response []                 (->Response 415 {} nil))
  (^Response [body]             (->Response 415 {} body))
  (^Response [body headers]     (->Response 415 headers body)))

(defn unsupported-media-type!
  "415 Unsupported Media Type (Client Error). The request entity has a media type
  which the server or resource does not support.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 415 {} nil)))
  ([body]               (throw! (->Response 415 {} body)))
  ([body headers]       (throw! (->Response 415 headers body))))

(defn requested-range-not-satisfiable
  "416 Requested Range Not Satisfiable (Client Error). The client has asked for a
  portion of the file but the server cannot supply that portion."
  (^Response []                 (->Response 416 {} nil))
  (^Response [body]             (->Response 416 {} body))
  (^Response [body headers]     (->Response 416 headers body)))

(defn requested-range-not-satisfiable!
  "416 Requested Range Not Satisfiable (Client Error). The client has asked for a
  portion of the file but the server cannot supply that portion. Throws an exception
  with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 416 {} nil)))
  ([body]               (throw! (->Response 416 {} body)))
  ([body headers]       (throw! (->Response 416 headers body))))

(defn expectation-failed
  "417 Expectation Failed (Client Error). The server cannot meet the requirements of
  the Expect request-header field."
  (^Response []                 (->Response 417 {} nil))
  (^Response [body]             (->Response 417 {} body))
  (^Response [body headers]     (->Response 417 headers body)))

(defn expectation-failed!
  "417 Expectation Failed (Client Error). The server cannot meet the requirements of
  the Expect request-header field. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 417 {} nil)))
  ([body]               (throw! (->Response 417 {} body)))
  ([body headers]       (throw! (->Response 417 headers body))))

(defn im-a-teapot
  "418 I'm a teapot (Client Error). The server cannot brew coffee because it is,
  permanently, a teapot."
  (^Response []                 (->Response 418 {} nil))
  (^Response [body]             (->Response 418 {} body))
  (^Response [body headers]     (->Response 418 headers body)))

(defn im-a-teapot!
  "418 I'm a teapot (Client Error). The server cannot brew coffee because it is,
  permanently, a teapot. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 418 {} nil)))
  ([body]               (throw! (->Response 418 {} body)))
  ([body headers]       (throw! (->Response 418 headers body))))

(defn enhance-your-calm
  "420 Enhance Your Calm (Client Error). You are being rate-limited."
  (^Response []                 (->Response 420 {} nil))
  (^Response [body]             (->Response 420 {} body))
  (^Response [body headers]     (->Response 420 headers body)))

(defn enhance-your-calm!
  "420 Enhance Your Calm (Client Error). You are being rate-limited.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 420 {} nil)))
  ([body]               (throw! (->Response 420 {} body)))
  ([body headers]       (throw! (->Response 420 headers body))))

(defn misdirected-request
  "421 Misdirected Request (Client Error). The request was directed at a server that is
  not able to produce a response (e.g. network balancer forwarded traffic to a wrong
  server)."
  (^Response []                 (->Response 421 {} nil))
  (^Response [body]             (->Response 421 {} body))
  (^Response [body headers]     (->Response 421 headers body)))

(defn misdirected-request!
  "421 Misdirected Request (Client Error). The request was directed at a server that is
  not able to produce a response (e.g. network balancer forwarded traffic to a wrong
  server). Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 421 {} nil)))
  ([body]               (throw! (->Response 421 {} body)))
  ([body headers]       (throw! (->Response 421 headers body))))

(defn unprocessable-entity
  "422 Unprocessable Entity (Client Error). The request was well-formed but was unable
  to be followed due to semantic errors."
  (^Response []                 (->Response 422 {} nil))
  (^Response [body]             (->Response 422 {} body))
  (^Response [body headers]     (->Response 422 headers body)))

(defn unprocessable-entity!
  "422 Unprocessable Entity (Client Error). The request was well-formed but was
  unable to be followed due to semantic errors. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 422 {} nil)))
  ([body]               (throw! (->Response 422 {} body)))
  ([body headers]       (throw! (->Response 422 headers body))))

(defn locked
  "423 Locked (Client Error). The resource that is being accessed is locked."
  (^Response []                 (->Response 423 {} nil))
  (^Response [body]             (->Response 423 {} body))
  (^Response [body headers]     (->Response 423 headers body)))

(defn locked!
  "423 Locked (Client Error). The resource that is being accessed is locked.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 423 {} nil)))
  ([body]               (throw! (->Response 423 {} body)))
  ([body headers]       (throw! (->Response 423 headers body))))

(defn failed-dependency
  "424 Failed Dependency (Client Error). The request failed due to failure of a previous
  request."
  (^Response []                 (->Response 424 {} nil))
  (^Response [body]             (->Response 424 {} body))
  (^Response [body headers]     (->Response 424 headers body)))

(defn failed-dependency!
  "424 Failed Dependency (Client Error). The request failed due to failure of
  a previous request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 424 {} nil)))
  ([body]               (throw! (->Response 424 {} body)))
  ([body headers]       (throw! (->Response 424 headers body))))

(defn unordered-collection
  "425 Unordered Collection (Client Error). The collection is unordered."
  (^Response []                 (->Response 425 {} nil))
  (^Response [body]             (->Response 425 {} body))
  (^Response [body headers]     (->Response 425 headers body)))

(defn unordered-collection!
  "425 Unordered Collection (Client Error). The collection is unordered.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 425 {} nil)))
  ([body]               (throw! (->Response 425 {} body)))
  ([body headers]       (throw! (->Response 425 headers body))))

(defn upgrade-required
  "426 Upgrade Required (Client Error). The client should switch to a different
  protocol."
  (^Response []                 (->Response 426 {} nil))
  (^Response [body]             (->Response 426 {} body))
  (^Response [body headers]     (->Response 426 headers body)))

(defn upgrade-required!
  "426 Upgrade Required (Client Error). The client should switch to a different
  protocol. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 426 {} nil)))
  ([body]               (throw! (->Response 426 {} body)))
  ([body headers]       (throw! (->Response 426 headers body))))

(defn precondition-required
  "428 Precondition Required (Client Error). The server requires the request to be
  conditional."
  (^Response []                 (->Response 428 {} nil))
  (^Response [body]             (->Response 428 {} body))
  (^Response [body headers]     (->Response 428 headers body)))

(defn precondition-required!
  "428 Precondition Required (Client Error). The server requires the request to be
  conditional. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 428 {} nil)))
  ([body]               (throw! (->Response 428 {} body)))
  ([body headers]       (throw! (->Response 428 headers body))))

(defn too-many-requests
  "429 Too Many Requests (Client Error). The user has sent too many requests in a given
  amount of time."
  (^Response []                 (->Response 429 {} nil))
  (^Response [body]             (->Response 429 {} body))
  (^Response [body headers]     (->Response 429 headers body)))

(defn too-many-requests!
  "429 Too Many Requests (Client Error). The user has sent too many requests in a
  given amount of time. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 429 {} nil)))
  ([body]               (throw! (->Response 429 {} body)))
  ([body headers]       (throw! (->Response 429 headers body))))

(defn request-header-fields-too-large
  "431 Request Header Fields Too Large (Client Error). The server is unwilling to
  process the request because either an individual header field or all the header
  fields collectively are too large."
  (^Response []                 (->Response 431 {} nil))
  (^Response [body]             (->Response 431 {} body))
  (^Response [body headers]     (->Response 431 headers body)))

(defn request-header-fields-too-large!
  "431 Request Header Fields Too Large (Client Error). The server is unwilling to
  process the request because either an individual header field or all the header
  fields collectively are too large. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 431 {} nil)))
  ([body]               (throw! (->Response 431 {} body)))
  ([body headers]       (throw! (->Response 431 headers body))))

(defn retry-with
  "449 Retry With (Client Error). The request should be retried after doing the
  appropriate action."
  (^Response []                 (->Response 449 {} nil))
  (^Response [body]             (->Response 449 {} body))
  (^Response [body headers]     (->Response 449 headers body)))

(defn retry-with!
  "449 Retry With (Client Error). The request should be retried after doing
  the appropriate action. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 449 {} nil)))
  ([body]               (throw! (->Response 449 {} body)))
  ([body headers]       (throw! (->Response 449 headers body))))

(defn blocked-by-windows-parental-controls
  "450 Blocked by Windows Parental Controls (Client Error). Windows Parental Controls
  are turned on and are blocking access to the given web page."
  (^Response []                 (->Response 450 {} nil))
  (^Response [body]             (->Response 450 {} body))
  (^Response [body headers]     (->Response 450 headers body)))

(defn blocked-by-windows-parental-controls!
  "450 Blocked by Windows Parental Controls (Client Error). Windows Parental Controls
  are turned on and are blocking access to the given web page. Throws an exception
  with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 450 {} nil)))
  ([body]               (throw! (->Response 450 {} body)))
  ([body headers]       (throw! (->Response 450 headers body))))

(defn unavailable-for-legal-reasons
  "451 Unavailable For Legal Reasons (Client Error). Resource access is denied for
  legal reasons."
  (^Response []                 (->Response 451 {} nil))
  (^Response [body]             (->Response 451 {} body))
  (^Response [body headers]     (->Response 451 headers body)))

(defn unavailable-for-legal-reasons!
  "451 Unavailable For Legal Reasons (Client Error). Resource access is denied for
  legal reasons. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 451 {} nil)))
  ([body]               (throw! (->Response 451 {} body)))
  ([body headers]       (throw! (->Response 451 headers body))))

(defn internal-server-error
  "500 Internal Server Error (Server Error). There was an internal server error."
  (^Response []                 (->Response 500 {} nil))
  (^Response [body]             (->Response 500 {} body))
  (^Response [body headers]     (->Response 500 headers body)))

(defn internal-server-error!
  "500 Internal Server Error (Server Error). There was an internal server error.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 500 {} nil)))
  ([body]               (throw! (->Response 500 {} body)))
  ([body headers]       (throw! (->Response 500 headers body))))

(defn not-implemented
  "501 Not Implemented (Server Error). The server either does not recognize the request
  method or it lacks the ability to fulfill the request."
  (^Response []                 (->Response 501 {} nil))
  (^Response [body]             (->Response 501 {} body))
  (^Response [body headers]     (->Response 501 headers body)))

(defn not-implemented!
  "501 Not Implemented (Server Error). The server either does not recognize the request
  method or it lacks the ability to fulfill the request.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 501 {} nil)))
  ([body]               (throw! (->Response 501 {} body)))
  ([body headers]       (throw! (->Response 501 headers body))))

(defn bad-gateway
  "502 Bad Gateway (Server Error). The server was acting as a gateway or proxy and
  received an invalid response from the upstream server."
  (^Response []                 (->Response 502 {} nil))
  (^Response [body]             (->Response 502 {} body))
  (^Response [body headers]     (->Response 502 headers body)))

(defn bad-gateway!
  "502 Bad Gateway (Server Error). The server was acting as a gateway or proxy and
  received an invalid response from the upstream server. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 502 {} nil)))
  ([body]               (throw! (->Response 502 {} body)))
  ([body headers]       (throw! (->Response 502 headers body))))

(defn service-unavailable
  "503 Service Unavailable (Server Error). The server is currently unavailable (because
  it is overloaded or down for maintenance)."
  (^Response []                 (->Response 503 {} nil))
  (^Response [body]             (->Response 503 {} body))
  (^Response [body headers]     (->Response 503 headers body)))

(defn service-unavailable!
  "503 Service Unavailable (Server Error). The server is currently unavailable (because
  it is overloaded or down for maintenance). Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 503 {} nil)))
  ([body]               (throw! (->Response 503 {} body)))
  ([body headers]       (throw! (->Response 503 headers body))))

(defn gateway-timeout
  "504 Gateway Timeout (Server Error). The server was acting as a gateway or proxy
  and did not receive a timely request from the upstream server."
  (^Response []                 (->Response 504 {} nil))
  (^Response [body]             (->Response 504 {} body))
  (^Response [body headers]     (->Response 504 headers body)))

(defn gateway-timeout!
  "504 Gateway Timeout (Server Error). The server was acting as a gateway or proxy
  and did not receive a timely request from the upstream server.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 504 {} nil)))
  ([body]               (throw! (->Response 504 {} body)))
  ([body headers]       (throw! (->Response 504 headers body))))

(defn http-version-not-supported
  "505 HTTP Version Not Supported (Server Error). The server does not support the
  HTTP protocol version used in the request."
  (^Response []                 (->Response 505 {} nil))
  (^Response [body]             (->Response 505 {} body))
  (^Response [body headers]     (->Response 505 headers body)))

(defn http-version-not-supported!
  "505 HTTP Version Not Supported (Server Error). The server does not support the
  HTTP protocol version used in the request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 505 {} nil)))
  ([body]               (throw! (->Response 505 {} body)))
  ([body headers]       (throw! (->Response 505 headers body))))

(defn variant-also-negotiates
  "506 Variant Also Negotiates (Server Error). Transparent content negotiation for
  the request results in a circular reference."
  (^Response []                 (->Response 506 {} nil))
  (^Response [body]             (->Response 506 {} body))
  (^Response [body headers]     (->Response 506 headers body)))

(defn variant-also-negotiates!
  "506 Variant Also Negotiates (Server Error). Transparent content negotiation for
  the request results in a circular reference. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 506 {} nil)))
  ([body]               (throw! (->Response 506 {} body)))
  ([body headers]       (throw! (->Response 506 headers body))))

(defn insufficient-storage
  "507 Insufficient Storage (Server Error). Insufficient storage to complete the
  request."
  (^Response []                 (->Response 507 {} nil))
  (^Response [body]             (->Response 507 {} body))
  (^Response [body headers]     (->Response 507 headers body)))

(defn insufficient-storage!
  "507 Insufficient Storage (Server Error). Insufficient storage to complete the
  request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 507 {} nil)))
  ([body]               (throw! (->Response 507 {} body)))
  ([body headers]       (throw! (->Response 507 headers body))))

(defn loop-detected
  "508 Loop Detected (Server Error). The server detected an infinite loop while
  processing the request."
  (^Response []                 (->Response 508 {} nil))
  (^Response [body]             (->Response 508 {} body))
  (^Response [body headers]     (->Response 508 headers body)))

(defn loop-detected!
  "508 Loop Detected (Server Error). The server detected an infinite loop while
  processing the request. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 508 {} nil)))
  ([body]               (throw! (->Response 508 {} body)))
  ([body headers]       (throw! (->Response 508 headers body))))

(defn bandwidth-limit-exceeded
  "509 Bandwidth Limit Exceeded (Server Error). Bandwidth limit has been exceeded."
  (^Response []                 (->Response 509 {} nil))
  (^Response [body]             (->Response 509 {} body))
  (^Response [body headers]     (->Response 509 headers body)))

(defn bandwidth-limit-exceeded!
  "509 Bandwidth Limit Exceeded (Server Error). Bandwidth limit has been exceeded.
  Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 509 {} nil)))
  ([body]               (throw! (->Response 509 {} body)))
  ([body headers]       (throw! (->Response 509 headers body))))

(defn not-extended
  "510 Not Extended (Server Error). Further extensions to the request are required for
  the server to fulfill it."
  (^Response []                 (->Response 510 {} nil))
  (^Response [body]             (->Response 510 {} body))
  (^Response [body headers]     (->Response 510 headers body)))

(defn not-extended!
  "510 Not Extended (Server Error). Further extensions to the request are required for
  the server to fulfill it. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 510 {} nil)))
  ([body]               (throw! (->Response 510 {} body)))
  ([body headers]       (throw! (->Response 510 headers body))))

(defn network-authentication-required
  "511 Network Authentication Required (Server Error). The client needs to
  authenticate to gain network access."
  (^Response []                 (->Response 511 {} nil))
  (^Response [body]             (->Response 511 {} body))
  (^Response [body headers]     (->Response 511 headers body)))

(defn network-authentication-required!
  "511 Network Authentication Required (Server Error). The client needs to
  authenticate to gain network access. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 511 {} nil)))
  ([body]               (throw! (->Response 511 {} body)))
  ([body headers]       (throw! (->Response 511 headers body))))

(defn network-read-timeout
  "598 Network read timeout (Server Error)."
  (^Response []                 (->Response 598 {} nil))
  (^Response [body]             (->Response 598 {} body))
  (^Response [body headers]     (->Response 598 headers body)))

(defn network-read-timeout!
  "598 Network read timeout (Server Error). Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 598 {} nil)))
  ([body]               (throw! (->Response 598 {} body)))
  ([body headers]       (throw! (->Response 598 headers body))))

(defn network-connect-timeout
  "599 Network connect timeout (Server Error). Network connect timeout behind the
  proxy."
  (^Response []                 (->Response 599 {} nil))
  (^Response [body]             (->Response 599 {} body))
  (^Response [body headers]     (->Response 599 headers body)))

(defn network-connect-timeout!
  "599 Network connect timeout (Server Error). Network connect timeout behind the
  proxy. Throws an exception with ex-info:
  `{:type :ring.util.http-response/response
   :response response}`"
  ([]                   (throw! (->Response 599 {} nil)))
  ([body]               (throw! (->Response 599 {} body)))
  ([body headers]       (throw! (->Response 599 headers body))))

;; Rendering

(defn render
  "Universal response renderer. Returns the result of calling the `resp-fn` with
  headers attached as the second argument (obtained from `:response/headers` key of
  the `req`) unless the `req` is already a valid response.

  When only `req` is given and there are no headers associated with
  `:response/headers` then `resp-fn` will be called without any argument.

  When only `req` is given and there are headers present, `resp-fn` is called with
  two arguments: `nil` and a map of headers.

  Third argument is passed as a first argument to the response function. Further
  arguments are passed beginning from the third argument to the response function."
  (^Response [resp-fn]
   (resp-fn))
  (^Response [resp-fn req]
   (if (nil? req)
     (resp-fn)
     (if (instance? Response req)
       req
       (if-some [headers (get req :response/headers)]
         (resp-fn nil headers)
         (resp-fn)))))
  (^Response [resp-fn req a]
   (if (nil? req)
     (resp-fn a)
     (if (instance? Response req)
       req
       (resp-fn a (or (get req :response/headers) {})))))
  (^Response [resp-fn req a b]
   (if (nil? req)
     (resp-fn a {} b)
     (if (instance? Response req)
       req
       (resp-fn a (or (get req :response/headers) {}) b))))
  (^Response [resp-fn req a b c]
   (if (nil? req)
     (resp-fn a {} b c)
     (if (instance? Response req)
       req
       (resp-fn a (or (get req :response/headers) {}) b c))))
  (^Response [resp-fn req a b c & more]
   (if (nil? req)
     (apply resp-fn a {} b c more)
     (if (instance? Response req)
       req
       (apply resp-fn a (or (get req :response/headers) {}) b c more)))))

(defn render-force
  "Universal response renderer. Returns the result of calling the `resp-fn` with
  headers attached (from `:response/headers` key of the `req`). Will not skip
  response generation even if `req` is already a response. Used mainly to force
  generation of redirects.

  When only `req` is given and there are no headers associated with
  `:response/headers` then `resp-fn` will be called without any argument.

  When only `req` is given and there are headers present, `resp-fn` is called with
  two arguments: `nil` and a map of headers.

  Third argument is passed as a first argument to the response function. Further
  arguments are passed beginning from the third argument to the response function."
  (^Response [resp-fn]
   (resp-fn))
  (^Response [resp-fn req]
   (if (nil? req)
     (resp-fn)
     (if-some [headers (get req :response/headers)]
       (resp-fn nil headers)
       (resp-fn))))
  (^Response [resp-fn req a]
   (if (nil? req)
     (resp-fn a)
     (resp-fn a (or (get req :response/headers) {}))))
  (^Response [resp-fn req a b]
   (if (nil? req)
     (resp-fn a {} b)
     (resp-fn a (or (get req :response/headers) {}) b)))
  (^Response [resp-fn req a b c]
   (if (nil? req)
     (resp-fn a {} b c)
     (resp-fn a (or (get req :response/headers) {}) b c)))
  (^Response [resp-fn req a b c & more]
   (if (nil? req)
     (apply resp-fn a {} b c more)
     (apply resp-fn a (or (get req :response/headers) {}) b c more))))
