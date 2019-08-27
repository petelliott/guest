(use-modules (guest test))

;; run the statements of body* with and empty *guest-tests*
;; and then restores afterwards.
;; this is necessary for guest's tests to be self-hosted.
(define-syntax-rule (with-fresh-guest body* ...)
  (let ((saved (@@ (guest test) *guest-tests*)))
    (set! (@@ (guest test) *guest-tests*) '())
    (let ((ret (begin body* ...)))
      (set! (@@ (guest test) *guest-tests*) saved)
      ret)))

(define-test (guest test define-test)
  (with-fresh-guest
    (define-test (a b c) #t)
    (define-test (a b d) #f)
    (and (equal? ((atree-ref (@@ (guest test) *guest-tests*)
                             '(a b c)))
                 #f)
         (equal? ((atree-ref (@@ (guest test) *guest-tests*)
                             '(a b d)))
                 '(#f . #f)))))

(define-test (guest test return-fail)
  (equal? ((@@ (guest test) return-fail) (equal? #f #t)) '(#f . (equal? #f #t)))
  (equal? ((@@ (guest test) return-fail) (equal? #t #t)) #f)
  (equal? (cdr ((@@ (guest test) return-fail) (car '()))) '(car '())))

(define-test (guest test run-guest)
  (with-fresh-guest
    (equal? (run-guest) '(0 . 0)))
  (with-fresh-guest
    (define-test (a b c) #t)
    (equal? (run-guest) '(1 . 1)))
  (with-fresh-guest
    (define-test (a b c) #f)
    (equal? (run-guest) '(1 . 0)))
  (with-fresh-guest
    (define-test (a b c) #f)
    (define-test (a b d) #t)
    (define-test (a c c) #f)
    (define-test (a c d) #t)
    (equal? (run-guest) '(4 . 2))))

(define-test (guest test transform-single-test)
  (equal? ((@@ (guest test) transform-single-test) '(#:pass . 0)) '(1 . 1))
  (equal? ((@@ (guest test) transform-single-test) '(#:fail . 0)) '(1 . 0)))

(define-test (guest test suite-fail)
  (equal? ((@@ (guest test) suite-fail) 1 0) #:fail)
  (equal? ((@@ (guest test) suite-fail) 2 2) #:pass))

(define-test (guest test run-guest-test)
  (let* ((pargs '())
         (printer (lambda (key . args)
                    (set! pargs (cons key args)))))
    (run-guest-test 'c '(a b c) (lambda () #f) printer)
    (equal? pargs
           '(#:leaf c (a b c) 0 #:pass #f #f)))
  (equal? (run-guest-test 'c '(a b c) (lambda() #f))
          '(#:pass . #f))
  (equal? (run-guest-test 'c '(a b c) (lambda() '(#f . (car (quote ())))))
          '(#:fail . (#f . (car (quote ()))))))




