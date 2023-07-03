(ns

    ^{:doc    "amelinium service, HTTP protocol."
      :author "Paweł Wilk"
      :added  "1.0.1"}

    amelinium.proto.http

  (:require [amelinium]
            [amelinium.types.errors]))

(defprotocol HTTP
  "The `amelinium.HTTP` protocol is used to get basic properties of request and
  response data structures used in Amelinium, like quickly checking if the given
  object is a request or a response, getting response headers (prepared in a request
  map or rendered in a response), and so on."

  (^{:tag Boolean}
   request?
   [src]
   "Returns `true` if the given object `src` is a request map, `false` otherwise.")

  (^{:tag Boolean}
   response?
   [src]
   "Returns `true` if the given object `src` is a response data structure.")

  (^{:tag IPersistentMap}
   response-headers
   [src]
   "Returns response headers found in the given object `src` which may be a request
  or a response data structure. In case of a request, the key `:response/headers` is
  looked up. In case of a response the key `:headers` is looked up. Returns `nil` if
  headers cannot not be found.")

  (^{:tag Long}
   response-status
   [src]
   "Returns a response status code (as an integer value) found in the response object
  `src` under the key `:status`. Returns `nil` if status cannot be found or the
  given object is not a response.")

  (response-body
    [src]
    "Returns a response body found in the given object `src` which may be a request
  or a response data structure. In case of a request, the key `:response/body` is
  looked up. In case of a response the key `:body` is looked up. Returns `nil` if
  a body cannot be found.")

  (app-status
    [src]
    "Returns application status found under the key `:app/status` of the given
  request map `src`. For a response, or if the status cannot be found, it returns
  `nil`.")

  (response-location
    [src] [src f] [src f args]
    "Returns a response location header.

  For a response object given as `src`, it simply returns its `Location` header.

  For a request map given as `src`, it returns a value associated with its
  `:response/location` key. In this case the following is applied:

  - If it is found and it is a URL, it is returned.
  - If it is found and it is a path or page identifier it is parsed using the
    given function `f`.
  - Optional `lang`, `params`, `qparams` and other additional arguments are passed
    as arguments during the call to `f`.

  If there is no response location, `nil` is returned."))
