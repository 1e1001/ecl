#lang racket/base
;; reader
;;
;; phases
;; - delimitization - splits into ident, delimiter, string, comment
;; - parsing - convert into a syntax object

;;; Imports
(require racket/match
         racket/generic)

;;; Exports

;;; Types
;; source for reading from, can be constructed with file-from-bytes
{define-generics SFile
  ;; Fetch the next n bytes without moving the head, can return less than n bytes of output if there's an end of file
  ;; n: UnsignedInt
  ;; -> String
  (SFile-peekn SFile n)
  ;; Move the head n bytes forward
  ;; n: UnsignedInt
  (SFile-move! SFile n)}

;; A file constructed with file-from-bytes
;; pos: UnsignedInt, buf: String
(struct SFileFromBytes (p b)
  #:mutable
  #:methods gen:SFile
  [{define (SFile-peekn self n)
     (define pos (SFileFromBytes-p self))
     (define buf (SFileFromBytes-b self))
     (subbytes buf pos (min (bytes-length buf) (+ pos n)))}
   {define (SFile-move! self n)
     (set-SFileFromBytes-p! (+ (SFileFromBytes-p self) n))}])

;; type: (or 'id 'del 'str 'com), value: (or String Byte String String)
(struct SToken (t v))

(struct STagged ())

;; value: String
(struct SId STagged (v))
;; value: String
(struct SString STagged (v))
;; value: Symbol | Number | Bool
(struct SLiteral STagged (v))
;; value: List[STagged]
(struct SList STagged (v))
(struct SEof ())

{define (print-expr expr)
  {define (inner expr indent)
    (display (build-string (* 2 indent) {lambda _ #\ }))
    (match expr
      [(SToken t v)
       (printf "SToken<~a> ~v\n" t v)]
      [(SId v)
       (printf "SId ~v\n" v)]
      [(SString v)
       (printf "SString ~v\n" v)]
      [(SLiteral v)
       (printf "SLiteral ~v\n" v)]
      [(SList v)
       (printf "SList\n")
       (define inner-indent (add1 indent))
       {for ([i v])
         (inner i inner-indent)}])}
  (inner expr 0)}

;;; Functions
;; Construct a new file from a racket bytes
;; bytes: String
;; -> SFile
{define (file-from-bytes bytes)
  (SFileFromBytes 0 bytes)}

{define (read-token file)
  (define ((n-range low high) v)
    (<= low v high))
  {let loop ()
    (match (bytes->list (SFile-peekn file 3))
      [(list (? (n-range #x00 #x20)) _ ...)
       (SFile-move! file 1)
       (loop)]
      [(or
        (list #xC2 #x85 _ ...)
        (list #xC2 #xA0 _ ...))
       (SFile-move! file 2)
       (loop)]
      [(or
        (list #xE1 #x9A #x80)
        (list #xE1 #xA0 #x8E)
        (list #xE2 #x80 (? (n-range #x80 #x8B)))
        (list #xE2 #x80 #xA8)
        (list #xE2 #x80 #xA9)
        (list #xE2 #x80 #xAF)
        (list #xE2 #x81 #x9F)
        (list #xE3 #x80 #x80)
        (list #xEF #xBB #xBF))
       (SFile-move! file 3)
       (loop)]
      )}}

;; Read all items from a file
{define (read-all file)
  {let loop ([res null])
    (define new (read-item file))
    (if (SEof? new)
        (reverse res)
        (loop (cons new res)))}}

{define (read-item file)
  (error "todo: read-item")}

;;; Test
(require racket/file)
(print-expr (SList (list (SId #"define")
                         (SList (list (SId #"factorial")
                                      (SId #"n")))
                         (SList (list (SId #"error")
                                      (SString #"todo: factorial"))))))
; (writeln (read-all (file-from-bytes (file->bytes "INPUT.ECL"))))
