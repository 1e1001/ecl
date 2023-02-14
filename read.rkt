#lang racket/base
(require racket/match
         racket/file)

(struct Parser (cursor data) #:mutable #:transparent)

(define (Parser-peekn parser n)
  (define data (Parser-data parser))
  (define cursor (Parser-cursor parser))
  (subbytes data cursor (min (+ cursor n) (bytes-length data))))
(define (Parser-peek parser)
  (define data (Parser-data parser))
  (define cursor (Parser-cursor parser))
  (if (>= cursor (bytes-length data))
      #f
      (bytes-ref data cursor)))
(define (Parser-move! parser n)
  (set-Parser-cursor! parser (+ (Parser-cursor parser) n)))
(define (Parser-next! parser)
  (Parser-move! parser 1))
(define (Parser-next-utf8! parser)
  (define v (Parser-peek parser))
  (Parser-move! parser
                (cond
                  [(not v) 0]
                  [(<= #x00 v #x7F) 1]
                  [(<= #xC0 v #xDF) 2]
                  [(<= #xE0 v #xEF) 3]
                  [(<= #xF0 v #xF7) 4]
                  [(<= #xF0 v #xF7) 5]
                  [(<= #xF8 v #xFB) 6]
                  [(<= #xFE v #xFF) 7]
                  [else (error "next-utf8 called at invalid utf8")])))
(define (Parser-space? parser)
  (define ((n-range low high) v)
    (<= low v high))
  (match (bytes->list (Parser-peekn parser 3))
    [(list (? (n-range #x00 #x20)) _ ...) #t]
    [(list #xC2 #x85 _ ...) #t]
    [(list #xC2 #xA0 _ ...) #t]
    [(list #xE1 #x9A #x80) #t]
    [(list #xE1 #xA0 #x8E) #t]
    [(list #xE2 #x80 (? (n-range #x80 #x8B))) #t]
    [(list #xE2 #x80 #xA8) #t]
    [(list #xE2 #x80 #xA9) #t]
    [(list #xE2 #x80 #xAF) #t]
    [(list #xE2 #x81 #x9F) #t]
    [(list #xE3 #x80 #x80) #t]
    [(list #xEF #xBB #xBF) #t]
    [_ #f]))

(define indent 0)

(define (get-indent)
  (build-string (* indent 2) (lambda _ #\ )))

(define-syntax-rule (define-read (name parser . args) body ...)
  (define (name parser . args)
    (printf "~a~a\n" (get-indent) (list 'name . args))
    (set! indent (add1 indent))
    (define res (let () body ...))
    (set! indent (sub1 indent))
    (unless (void? res)
      (printf "~a-> ~a\n" (get-indent) res))
    res))

; syntax objects
(struct SSExpr (s v))
(struct SMExpr (s v))
(struct SHExpr (s v))
(struct SLExpr (s v))
(struct SRef (s p k))
(struct SId (s v))
(struct SBool (s v))
(struct SSpan (s e))

(define RESERVED_CHARS (cons #f (bytes->list #"[]{}()\\#;\"'")))

(define-read (read-file parser)
  (let loop ([result null])
    (read-spaces parser)
    (if (Parser-peek parser)
        (loop (cons (read-item parser) result))
        (reverse result))))

(define-read (read-spaces parser)
  (let loop ()
    ; todo: expression comments
    (when (Parser-space? parser)
      (Parser-next-utf8! parser)
      (loop))))

(define-read (read-item parser)
  (define start (Parser-cursor parser))
  (define res (read-quotable parser))
  (read-spaces parser)
  (let loop ([res res])
    (match (Parser-peek parser)
      [#x5B
       (Parser-next! parser)
       (define val (read-list parser read-item #x5D))
       (loop (SMExpr (SSpan start
                            (Parser-cursor parser))
                     (cons res val)))]
      [#x5C
       (Parser-next! parser)
       (if (equal? (Parser-peek parser) #x5C)
           (begin
             (Parser-next! parser)
             (loop (SLExpr (SSpan (- (Parser-cursor parser) 2)
                                  (Parser-cursor parser))
                           (list res (read-quotable parser)))))
           (loop (SRef (SSpan (sub1 (Parser-cursor parser))
                              (Parser-cursor parser))
                       res
                       (list (read-quotable parser)))))]
      [_ res])))

(define-read (read-list parser fn close)
  (let loop ([res null])
    (read-spaces parser)
    (if (equal? (Parser-peek parser) close)
        (begin
          (Parser-next! parser)
          (reverse res))
        (loop (cons (fn parser) res)))))

(define-read (read-path parser)
  (let loop ([res (list (read-ident parser))])
    (read-spaces parser)
    (if (equal? (Parser-peek parser) #x5C)
        (begin (Parser-next! parser)
               (if (equal? (Parser-peek parser) #x5C)
                   (begin
                     (Parser-move! parser -1)
                     (reverse res))
                   (begin
                     (read-spaces parser)
                     (loop (cons (read-ident parser) res)))))
        (reverse res))))

(define-read (read-quotable parser)
  (match (Parser-peek parser)
    [#x23
     (Parser-next! parser)
     (read-hash parser)]
    [#x28
     (define start (Parser-cursor parser))
     (Parser-next! parser)
     (define res (read-list parser read-item #x29))
     (SSExpr (SSpan start (Parser-cursor parser)) res)]
    [#x7B
     (define start (Parser-cursor parser))
     (Parser-next! parser)
     (define res (read-list parser read-item #x7D))
     (SHExpr (SSpan start (Parser-cursor parser)) res)]
    [#x22 (error "string quote")]
    [#x27 (error "string marked")]
    [_
     (define res (read-path parser))
     (define first (car res))
     (define rest (cdr res))
     (if (zero? (length rest))
         first
         (SRef (SId-s first) first rest))]))

(define-read (read-hash parser)
  (match (Parser-peek parser)
    [#x5C
     (Parser-next! parser)
     (let/cc break
       (define val (Parser-peek parser))
       (match val
         [(or #x54 #x74 #x46 #x66)
          (Parser-next! parser)
          (when (or (Parser-space? parser)
                    (memv (Parser-peek parser) RESERVED_CHARS))
            (break (match val
                     [(or #x54 #x74) (SBool #t)]
                     [(or #x46 #x66) (SBool #f)]
                     [_ (error "expected # or =, found space")])))]
         [_ (void)])
       (define id (read-ident parser))
       (match (Parser-peek parser)
         [#x23 (error "#\\id#")]
         [#x3D (error "#\\id\\")]
         [_ (error "expected # or \\")]))]
    [_ (error "quote")]))

(define-read (read-ident parser)
  (define start (Parser-cursor parser))
  (let loop ([res null])
    (define val (Parser-peek parser))
    (if (or (Parser-space? parser) (memv val RESERVED_CHARS))
        (if (zero? (length res))
            (error "zero-length identifier")
            (SId (SSpan start (Parser-cursor parser)) (apply bytes (reverse res))))
        (begin
          (Parser-next! parser)
          (loop (cons val res))))))

(define-read (read-todo parser)
  (error "todo: read-todo"))

(writeln (read-file (Parser 0 (file->bytes "input.ecl"))))
