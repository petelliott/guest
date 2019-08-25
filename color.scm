;;; procedures for colored output
(define-module (guest color)
  #:export (ansi-format
            foreground
            background
            bold
            underlined
            blinking))

(define (ansi-format code . rest)
  "takes any number of ansi format codes and emits a sequence to\
  produce those"
  (format #f "~a[~a~{;~a~}m"
          #\esc code rest))

(define color-offset
  '((#:black . 0)
    (#:red . 1)
    (#:green . 2)
    (#:yellow . 3)
    (#:blue . 4)
    (#:magenta . 5)
    (#:cyan . 6)
    (#:white . 7)))

;TODO some kind of color stack

(define (foreground color text)
  "return a string that when printed in an ansi terminal will have\
  forground color #:black #:red #:green #:yellow #:blue #:magenta\
  #:cyan or #:white"
  (format #f "~a~a~a"
          (ansi-format
            (+ 30 (assoc-ref color-offset color)))
          text
          (ansi-format 39)))

(define (background color text)
  "return a string that when printed in an ansi terminal will have\
  background color #:black #:red #:green #:yellow #:blue #:magenta\
  #:cyan or #:white"
  (format #f "~a~a~a"
          (ansi-format
            (+ 40 (assoc-ref color-offset color)))
          text
          (ansi-format 49)))

(define (bold text)
  "return a bolded string"
  (format #f "~a~a~a"
          (ansi-format 1)
          text
          (ansi-format 22)))

(define (underlined text)
  "return an underlined string"
  (format #f "~a~a~a"
          (ansi-format 4)
          text
          (ansi-format 24)))

(define (blinking text)
  "return an underlined string"
  (format #f "~a~a~a"
          (ansi-format 5)
          text
          (ansi-format 25)))
