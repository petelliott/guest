(define-module (guest test)
  #:use-module (guest atree)
  #:use-module (srfi srfi-1)
  #:export (define-test
            run-guest
            run-guest-test))

(define *guest-tests* '())

(define-syntax-rule
  (define-test name tcase* ...)
  (set! *guest-tests*
    (atree-insert *guest-tests* (quote name)
                  (lambda ()
                    (or (return-fail tcase*) ...)))))

(define-syntax-rule
  (return-fail tcase)
  (catch #t
    (lambda ()
      (if tcase
        #f
        (cons #:fail (quote tcase))))
    ; post-unwind handler
    (lambda (key . args)
      (quote tcase))
    ; pre-unwind handler
    (lambda (key . args)
      (display-error #f (current-output-port)
                     (car args) (cadr args) (caddr args) #f))))


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
          (+ (cdr pair1) (car pair2))))

      '(0 . 0)

      (map (lambda (test)
             (let ((newpre (append prefix (list (car test)))))
               (printer #:before (car test) newpre)
               (let ((ret (run-guest-sorted (cdr test) printer newpre)))
                 (printer #:after (car test) (append prefix (car test))
                          0 (car ret) (cdr ret))
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

(define* (run-guest-test name fullname test #:optional printer)
  "run and test and print printer output returns (#:pass/#:fail . msg)"
  (let* ((msg (test))
         (pf (if msg #:fail #:pass)))
    (printer #:leaf name fullname 0 pf msg)
    (cons pf msg)))

;;; printers take a key, whigh is either #:before #:after of #:leaf
;;; with the following args:
;;; #:before: (name fullname)
;;; #:after: (name fullname time status percent)
;;; #:leaf:  (name fullname time status msg)
;;;
;;; status can be #:pass/#:fail
;;; in the future there might be #:stillfail

(define (null-printer key . args)
  "the guest printer that does nothing"
  #f)

