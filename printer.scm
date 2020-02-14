;;; the defualt guest printer
(define-module (guest printer)
  #:use-module (guest color)
  #:use-module (ice-9 format)
  #:export (default-printer))

(define (default-printer key . args)
  "the defualt guest printer. compatible with run-guest"
  (case key
    ((#:before) (apply before-printer args))
    ((#:leaf)   (apply leaf-printer args))
    ((#:after)  (apply after-printer args))))

(define (before-printer name fullname)
  (format #t "~v_Testing ~a:\n"
          (* 4 (1- (length fullname)))
          name))

(define (leaf-printer name fullname time status msg err)
  (format #t "~v_test ~a: ~a\n"
          (* 4 (1- (length fullname)))
          name
          (cond
            ((equal? status #:pass)
             (bold (foreground #:green "pass")))
            (err (format #f "~a: ~s: error in ~a: ~a"
                         (bold (foreground #:red "fail"))
                         msg
                         (car err)
                         (apply format #f (cadr err) (caddr err))))
            (#t (format #f "~a: ~a"
                        (bold (foreground #:red "fail"))
                        msg)))))

(define (after-printer name fullname time status ntests passed)
  (format #t "~v_suite ~a: ran ~a, passed ~a (~$%): ~a\n"
          (* 4 (1- (length fullname)))
          name
          ntests
          passed
          (* 100 (/ passed ntests))
          (if (equal? status #:pass)
            (bold (foreground #:green "pass"))
            (bold (foreground #:red "fail")))))
