# Introduction

**Opinionated Clojure Web Engine.**

[![Amelinium on Clojars](https://img.shields.io/clojars/v/io.randomseed/amelinium.svg)](https://clojars.org/io.randomseed/amelinium)
[![Amelinium on cljdoc](https://cljdoc.org/badge/io.randomseed/amelinium)](https://cljdoc.org/d/io.randomseed/amelinium/CURRENT)
[![CircleCI](https://circleci.com/gh/randomseed-io/amelinium.svg?style=svg)](https://circleci.com/gh/randomseed-io/amelinium)

Clojure library with helpful functions and macros.

## Features

TBW

## Installation

To use Amelinium in your project, add the following to dependencies section of
`project.clj` or `build.boot`:

```clojure
[io.randomseed/amelinium "1.0.0"]
```

For `deps.edn` add the following as an element of a map under `:deps` or
`:extra-deps` key:

```clojure
io.randomseed/amelinium {:mvn/version "1.0.0"}
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
