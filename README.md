# guest

[![Build Status](https://travis-ci.com/Petelliott/guest.svg?branch=master)](https://travis-ci.com/Petelliott/guest)
[![Coverage Status](https://coveralls.io/repos/github/Petelliott/guest/badge.svg?branch=master)](https://coveralls.io/github/Petelliott/guest?branch=master)

guest is a testing framework for GNU guile. It is licensed under the LGPLv3

## installation

put this directory into somewhere on your guile %load-path.
put the `guest` script somewhere on your path.

TODO: add a makefile or make a package manager

## example

put the following in a file named `test/test.scm` directory (or really anywhere).

```scheme
(use-modules (guest test))

(define-test (a b c)
  (assert #t)
  (assert-equal? 5 6)
  (assert #t)

(define-test (a b d)
  (assert #t)
  (assert #t)
  (assert #t)

(define-test (a b e)
  (assert #t)
  (car '()))
```

Then run `guest test/` or `guest test/test.scm`. You should see something like
this:

```
Testing a:
    Testing b:
        test c: fail: (equal? 5 6)
        test d: pass
        test e: fail: (car (quote ())): error in car: Wrong type argument in position 1 (expecting pair): ()
    suite b: ran 3, passed 1 (33.33%): fail
suite a: ran 3, passed 1 (33.33%): fail
```

### travis ci/coveralls example

`.travis.yml`:

```yaml
dist: bionic
language: generic

install:
  - curl https://raw.githubusercontent.com/Petelliott/guest/master/install.sh | bash

script:
  - GUILE_LOAD_PATH=$(realpath ..) guest test/ --cover "$(printf "your-package/%s " *.scm)" --cover-out coverage.info


after_success:
  - coveralls.sh coverage.info
```

in your travis settings, be sure to set the `COVERALLS_TOKEN` environment variable.

## how it works

The `define-test` macro takes a hierarchical test name, and a bunch of
expressions. all of these expressions must evaluate to true for the test to
pass. when one expressions fails the rest will not be executed.
