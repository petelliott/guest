;;; atree is an association list keyed by a list of elements
;;; its main use is sorting by key elements from highest to lowest
(define-module (guest atree)
  #:export (atree-insert
            atree-ref
            atree-sort))

(define (atree-insert atree key value)
  "insert a value into an atree, producing a new atree"
  (cond
    ((null? key) value)
    ((assoc (car key) atree)
     (map (lambda (pair)
            (if (equal? (car pair) (car key))
              (cons (car pair)
                    (atree-insert (cdr pair) (cdr key) value))
              pair))
          atree))
    (#t (acons (car key) (atree-insert '() (cdr key) value) atree))))

(define* (atree-ref atree key #:optional dflt)
  "find the value pointed to by key, with optional default"
  (cond
    ((null? key) atree)
    ((not atree) dflt)
    (#t (atree-ref (assoc-ref atree (car key))
                   (cdr key) dflt))))

(define* (atree-sort atree #:optional (less symbol-alist-less))
  "stably sort an atree. default comparison is for symbols"
  (if (list? atree)
    (stable-sort
      (map (lambda (pair)
             (cons (car pair)
                   (atree-sort (cdr pair) less)))
           atree)
      less)
    atree))

(define (symbol-alist-less a b)
  (string< (symbol->string (car a))
           (symbol->string (car b))))





