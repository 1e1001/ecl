#lang racket/base
;; reader
;;
;; phases
;; - delimitization - splits into ident, delimiter, string, comment
;; - parsing - convert into a syntax object

;;; Imports
(require racket/generic)

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

;;; Functions
;; Construct a new file from a racket bytes
;; bytes: String
;; -> SFile
{define (file-from-bytes bytes)
  (SFileFromBytes 0 bytes)}

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
(writeln (read-all (file-from-bytes (file->bytes "INPUT.ECL"))))
