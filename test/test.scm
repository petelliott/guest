(use-modules (guest test)
             (guest atree))

;; run the statements of body* with and empty *guest-tests*
;; and then restores afterwards.
;; this is necessary for guest's tests to be self-hosted.
;; we are temporarily saving test-cont because paramertize breaks
(define-syntax-rule (with-fresh-guest body* ...)
  (let ((saved-tests (@@ (guest test) *guest-tests*))
        (saved-cont  (@@ (guest test) test-cont)))
    (set! (@@ (guest test) *guest-tests*) '())
    (let ((ret (begin body* ...)))
      (set! (@@ (guest test) *guest-tests*) saved-tests)
      (set! (@@ (guest test) test-cont) saved-cont)
      ret)))

(define-test (guest test define-test)
  (assert (with-fresh-guest
           (define-test (a b c) (assert-equal? 1 2))
           ((atree-ref (@@ (guest test) *guest-tests*) '(a b c)))))
  (assert-equal? #f (with-fresh-guest
                     (define-test (a b d) (assert-equal? 1 1))
                     ((atree-ref (@@ (guest test) *guest-tests*) '(a b d))))))

;; this test is of questionable utility, and is a lot of work to port
;; to the new system.
#;(define-test (guest test return-fail)
  (equal? ((@@ (guest test) return-fail) (equal? #f #t)) '(#f . (equal? #f #t)))
  (equal? ((@@ (guest test) return-fail) (equal? #t #t)) #f)
  (equal? (cdr ((@@ (guest test) return-fail) (car '()))) '(car '())))

(define-test (guest test run-guest)
  (assert
   (with-fresh-guest
    (equal? (run-guest) '(0 . 0))))
  (assert
   (with-fresh-guest
    (define-test (a b c) #t)
    (equal? (run-guest) '(1 . 1))))
  (assert
   (with-fresh-guest
    (define-test (a b c) (assert #f))
    (equal? (run-guest) '(1 . 0))))
  (assert
   (with-fresh-guest
    (define-test (a b c) (assert #f))
    (define-test (a b d) #t)
    (define-test (a c c) (assert #f))
    (define-test (a c d) #t)
    (equal? (run-guest) '(4 . 2)))))

(define-test (guest test transform-single-test)
  (assert-equal? ((@@ (guest test) transform-single-test) '(#:pass . 0)) '(1 . 1))
  (assert-equal? ((@@ (guest test) transform-single-test) '(#:fail . 0)) '(1 . 0)))

(define-test (guest test suite-fail)
  (assert-equal? ((@@ (guest test) suite-fail) 1 0) #:fail)
  (assert-equal? ((@@ (guest test) suite-fail) 2 2) #:pass))

(define-test (guest test run-guest-test)
  (let* ((pargs '())
         (printer (lambda (key . args)
                    (set! pargs (cons key args)))))
    (run-guest-test 'c '(a b c) (lambda () #f) printer)
    (assert-equal? pargs
           '(#:leaf c (a b c) 0 #:pass #f #f)))
  (assert-equal? (run-guest-test 'c '(a b c) (lambda() #f))
          '(#:pass . #f))
  (assert-equal? (run-guest-test 'c '(a b c) (lambda() '(#f . (car (quote ())))))
          '(#:fail . (#f . (car (quote ()))))))
