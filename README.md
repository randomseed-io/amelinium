# Amelinium

**Opinionated Clojure Web Engine.**

Welcome to Amelinium, yet another set of libraries and helper functions to serve
a dynamic web content. It is quite opinionated since its primary purpose is to be the
web and API engine for a bunch of projects run by *random:seed*, the author and
associates.

Important features:

* Configured with a bunch of **EDN files** loaded from specified directories.

* Management functions, including **inspection** of current state,
  **starting/stopping** and **suspending**.

* **JSP model-2 architecture** with **models**, **controllers**, **views** and **layouts**.

* Database **connection pooling**.

* **Abstract in-memory caches** around database operations (including incremental
  updates of collections) and time consuming functions (with adjustable TTL and queue
  size parameters).

* **Internationalization** (i18n) with or without default messages and
  **pluralization rules** for translations of countable items in different languages.

* **Session handling** with configurable persistent storage accessors, polymorphic
  interface, secure tokens and prolongation support.

* **Cookie-less** session and state management.

* **Role-based access control** middleware with optional context detection.

* **HTTP headers manipulation** middleware.

* **Language detection** middleware with configurable detection chains.

* **Generic populating functions** to enrich request map with dynamic data.

* **Remote IP** middleware with proxy detection and proxy IP whitelisting.

* **Lazy maps** to pass request data between middleware handlers and contextual data
  to template rendering functions.

* **URI** builders for **localized paths** with automatic detection and/or injection
  of language parameter.

* **URI** builders for other **parameterized paths**.

* **Parameter coercion** and **error reporting** for both API and web channels.

* **Twilio API** client (for SMS and e-mail messaging, including internationalized templates).

* **API response building** macros and functions (including standardized translatable
  statuses, sub-statuses, language parameters and errors).

* **Web rendering** handlers and predefined template tags for translations,
  language-parameterized path generation, session links (for cookie-less sessions),
  form building and session data access.

* **Flexible authentication engine** with pluggable encryption modules and ability to
  build parameterized encryption chains stored in a database (and re-used where
  possible).

* **Authentication functions**.

* **Identity management functions** for e-mail and phone numbers.

* **Buffered events logging** to a database with async channels.

* **Swagger** support for API and web.

## Installation

[![Amelinium on Clojars](https://img.shields.io/clojars/v/io.randomseed/amelinium.svg)](https://clojars.org/io.randomseed/amelinium)
[![Amelinium on cljdoc](https://cljdoc.org/badge/io.randomseed/amelinium)](https://cljdoc.org/d/io.randomseed/amelinium/CURRENT)
[![CircleCI](https://circleci.com/gh/randomseed-io/amelinium.svg?style=svg)](https://circleci.com/gh/randomseed-io/amelinium)

To use Amelinium in your project, add the following to dependencies section of
`project.clj` or `build.boot`:

```clojure
[io.randomseed/amelinium "1.0.1"]
```

For `deps.edn` add the following as an element of a map under `:deps` or
`:extra-deps` key:

```clojure
io.randomseed/amelinium {:mvn/version "1.0.1"}
```

Additionally, if you want to utilize specs and generators provided by the Amelinium
you can use (in your development profile):

```clojure
org.clojure/spec.alpha {:mvn/version "0.2.194"}
org.clojure/test.check {:mvn/version "1.1.0"}
```

You can also download JAR from [Clojars](https://clojars.org/io.randomseed/amelinium).

## Sneak peeks

TBW

And more…

## Documentation

Full documentation including usage examples is available at:

* https://randomseed.io/software/amelinium/

## License

Copyright © 2022 Paweł Wilk

May contain works from earlier free software projects, copyright © 2019-2022 Paweł Wilk

Amelinium is copyrighted software owned by Paweł Wilk (pw@gnu.org). You may
redistribute and/or modify this software as long as you comply with the terms of
the [GNU Lesser General Public License][LICENSE] (version 3).

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Development

### Building docs

```bash
make docs
```

### Building JAR

```bash
make jar
```

### Rebuilding POM

```bash
make pom
```

### Signing POM

```bash
make sig
```

### Deploying to Clojars

```bash
make deploy
```

### Interactive development

```bash
bin/repl
```

Starts REPL and nREPL server (port number is stored in `.nrepl-port`).

[LICENSE]:    https://github.com/randomseed-io/amelinium/blob/master/LICENSE
