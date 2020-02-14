(define-module (guest test)
  #:use-module (guest atree)
  #:use-module (srfi srfi-1)
  #:export (define-test
            define-suite
            assert-equal?
            run-guest
            run-guest-test))

(define *guest-tests* '())

(define test-cont '())
(define suite-prefix '())


(define (assert-equal-internal? left right mleft mright cl)
 (unless (equal? left right)
   (test-cont (cons #f (format #f (string-append
                                   "Left: ~A -> ~A != Right ~A -> ~A~%"
                                   "On line ~A of ~A~%")
                               mleft left mright right
                               (cdr (assoc 'line cl))
                               (cdr (assoc 'filename cl)))))))
(define-syntax-rule
  (define-suite name rules* ...)
  (let ((copy suite-prefix))
    (set! suite-prefix (append (quote name) suite-prefix))
    rules* ...
    (set! suite-prefix copy)))


(define-syntax-rule
  (assert-equal? left right)
  (assert-equal-internal? left right 'left 'right (current-source-location)))

(define-syntax-rule
  (define-test name tcase* ...)
  (set! *guest-tests*
    (atree-insert *guest-tests* (append suite-prefix (quote name))
                  (λ ()
                    (call/cc
                      (lambda (cont)
                        (set! test-cont cont)
                        (return-fail tcase*)
                        ...
                        #f))))))

(define-syntax-rule
  (return-fail tcase)
  (let ((errorm #f))
    (catch #t
      (lambda ()
        tcase)
      ; post-unwind handler
      (lambda (key . args)
        (test-cont (cons args (quote tcase)))))))


(define* (run-guest #:optional (printer null-printer))
  "run all gest tests in alphabetical order calling printer\
  #:before #:after and #:leaf. the default printer does nothing\
  returns a pair of (tests . passed)"
  (run-guest-sorted (atree-sort *guest-tests*) printer))

(define* (run-guest-sorted tests printer #:optional (prefix '()))
  (if (list? tests)
    (reduce
      (lambda (pair1 pair2)
        (cons
          (+ (car pair1) (car pair2))
          (+ (cdr pair1) (cdr pair2))))

      '(0 . 0)

      (map (lambda (test)
             (let ((newpre (append prefix (list (car test)))))
               (when (list? (cdr test)) (printer #:before (car test) newpre))
               (let ((ret (run-guest-sorted (cdr test) printer newpre)))
                 (when (list? (cdr test))
                   (printer #:after (car test) newpre
                            0 (suite-fail (car ret) (cdr ret))
                            (car ret) (cdr ret)))
                 ret)))
           tests))
    (transform-single-test
      (run-guest-test (last prefix) prefix
                      tests printer))))

(define (transform-single-test ret)
  (cons
    1
    (case (car ret)
      ((#:pass) 1)
      ((#:fail) 0))))

(define (suite-fail ntests passed)
  (if (= ntests passed)
    #:pass
    #:fail))

(define* (run-guest-test name fullname test #:optional (printer null-printer))
  "run and test and print printer output returns (#:pass/#:fail . msg)"
  (let* ((msg (test))
         (pf (if msg #:fail #:pass)))
    (printer #:leaf name fullname 0 pf (if msg (cdr msg) msg) (if msg (car msg) #f))
    (cons pf msg)))

;;; printers take a key, whigh is either #:before #:after of #:leaf
;;; with the following args:
;;; #:before: (name fullname)
;;; #:after: (name fullname time status ntests passed)
;;; #:leaf:  (name fullname time status msg error)
;;;
;;; status can be #:pass/#:fail
;;; in the future there might be #:stillfail

(define (null-printer key . args)
  "the guest printer that does nothing"
  #f)
