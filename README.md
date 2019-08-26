# guest

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
  #t
  (equal? 5 6)
  #t)

(define-test (a b d)
  #t
  #t
  #t)

(define-test (a b e)
  #t
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

## how it works

The `define-test` macro takes a hierarchical test name, and a bunch of
expressions. all of these expressions must evaluate to true for the test to
pass. when one expressions fails the rest will not be executed.
