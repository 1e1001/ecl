#lang racket/base
; Run fully-expanded ecl in racket, with enough standard library to run RDEXP.RKT
(require racket/file
         (for-syntax racket/base
                     racket/string))

(provide
 #%module-begin #%app #%top #%datum
 (rename-out
  [-%read-file %read-file]
  [-%hash %hash]
  [-%ref %ref]
  [define /core/define]
  [lambda /core/fn]
  [letrec /core/let]
  [list %list]
  [vector %tup]))

{define-syntax-rule (-println fmt . args)
  (-print (string-append fmt "\n") . args)}

{define-syntax-rule (-print fmt . args)
  (printf (transform-fmt fmt) . args)}

{define (-%ref a . b)
  (cond
    [(null? b) a]
    [(list? a) (-%ref (list-ref a (car b)) (cdr b))]
    [(hash? a) (-%ref (hash-ref a (car b)) (cdr b))]
    [(string? a) (-%ref (string-ref a (car b)) (cdr b))]
    [(vector? a) (-%ref (vector-ref a (car b)) (cdr b))])}

{define (-%hash . items)
  (define res (make-hash))
  {define (inner items)
    (cond
      [(null? items)
       res]
      [(null? (cdr items))
       (error "odd arg count")]
      [else
       (hash-set! res (car items) (cadr items))
       (inner (cddr items))])}
  (inner items)}

{define-syntax (transform-fmt str)
  {define (inner str rules)
    (if (null? rules)
      str
      (inner (string-replace str (caar rules) (cdar rules)) (cdr rules)))}
  #`#,(inner (syntax-e (cadr (syntax-e str)))
             '(("~" . "~~")
               ("{}" . "~a")
               ("{:?}" . "~v")
               ("{{" . "{")
               ("}}" . "}")))}

{define (-%read-file file)
  (file->string file)}
