#lang racket
(require "gd.rkt")
(provide string->gd)

(struct accept (value) #:transparent)
(struct reject () #:transparent)
(struct end () #:transparent)

(struct get-str (cat) #:transparent)

(define (white? c)
  (or (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\newline)))

(define not-white? (compose not white?))

(define (loc->range l)
  (match l
    [(location r c) (text-range l (location r (+ c 1)))]))

(define (range-increase r l)
  (match* (r l)
    [((text-range start _) (location r c)) (text-range start (location r (+ c 1)))]))

(define strnil
  (let ()
    (define (loop rl range)
      (λ (x)
        (match x
          [(char c loc) (loop (rl (add c)) (range-increase range loc))]
          [(get-str sym) (str sym (list->string (rl (get))) range)])))
    (λ (x)
      (match x
        [(char c loc) (loop (rnil (add c)) (loc->range loc))]
        [(get-str sym) (str sym "" #f)]))))

(define-syntax-rule (define-readers ((from-state cat) [rule to-state] ...) ...)
  (begin
    (define ((from-state str) c)
      (match c
        [(char rule _) (accept (to-state (str c)))]
        ...
        [(char _ _) (reject)]
        [(get) (str (get-str 'cat))]))
    ...))

(match-define (list empty-r text-r)
  (let ()
    (define-readers
      ((empty empty) [#\newline lf]
                     [(? white?) white]
                     [(? not-white?) text])
      ((text text) [(? not-white?) text])
      ((white space) [#\newline lf]
             [(? white?) white])
      ((lf space) [#\newline lf2]
          [(? white?) lf])
      ((lf2 empty-line) [(? white?) lf2]))
    (list (empty strnil) (text strnil))))

(define docify
  (let ()
    (define (loop doc par-top par-rest)
      (λ (x)
        (match x
          [(str sym _ _) (match sym
                           [(or 'text 'space) (loop doc (par-top (add x)) par-rest)]
                           ['start (start doc (cons par-top par-rest))]
                           ['stop (match par-rest
                                    ['() (loop doc (par-top (add (err "unopen"))) par-rest)]
                                    [(list next rest ...) (loop doc (next (add (par-top (get)))) rest)])]
                           ['empty-line (loop (end-para doc par-top par-rest) rnil '())])]
          [(end) ((end-para doc par-top par-rest) (get))])))
    (define (start doc par)
      (λ (x)
        (match x
          [(str 'text _ _) (loop doc (rnil (add x)) par)])))
    (define (end-para doc par-top par-rest)
      (match par-rest
        ['() (doc (add (par-top (get))))]
        [(list first rest ...) (end-para doc (first (add ((par-top (add (err "unclosed"))) (get)))) rest)]))
    (loop rnil rnil '())))

(define (read-doc start-char stop-char escape-char)
  
  (define c=? (curry char=?))
  (define (loop doc r)
    (define (flush)
      (match (r (get))
        [(str 'empty _ _) doc]
        [res (doc res)]))
    (λ (x)
      (match x
        [(end) ((flush) (end))]
        [(char c _)
         (match c
           [(? (c=? start-char)) (loop ((flush) ((strnil x) (get-str 'start))) text-r)]
           [(? (c=? stop-char)) (loop ((flush) ((strnil x) (get-str 'stop))) empty-r)]
           [(? (c=? escape-char)) (escape doc r)]
           [_ (match (r x)
                [(accept new-r) (loop doc new-r)]
                [(reject) ((loop (flush) empty-r) x)])])])))
  (define (escape doc r)
    (define (flush)
      (match (r (get))
        [(str 'empty _ _) doc]
        [res (doc res)]))
    (λ (x)
      (match x
        [(end) ((flush) (get))]
        [(char _ _)
         (match (r x)
           [(accept new-r) (loop doc new-r)]
           [(reject) (match (empty-r x)
                       [(accept new-r) (loop (flush) new-r)])])])))
  (loop docify empty-r))

(define read-gd
  (let ()
    (define error (list (list (err "too short"))))
    (define-syntax halp
      (syntax-rules ()
        [(_ () body) body]
        [(_ (first rest ...) body)
         (λ (x)
           (match x
             [(end) error]
             [(char first _) (halp (rest ...) body)]))]))
    (halp
     (start stop escape)
     (read-doc start stop escape))))

(define (read-chars [start (location 0 0)])
  (define (loop r loc)
    (match loc
      [(location row col)
       (λ (c)
         (match c
           [(end) (r (end))]
           [#\newline (loop (r (char #\newline loc)) (location (+ row 1) 0))]
           [#\return (cr (r (char #\newline loc)) (location (+ row 1) 0))]
           [_ (loop (r (char c loc)) (location row (+ col 1)))]))]))
  (define (cr r loc)
    (λ (c)
         (match c
           [#\newline (loop r loc)]
           [_ ((loop r loc) c)])))
  (loop read-gd start))

(define (string->gd s)
  ((for/fold ([r (read-chars)]) ([c s]) (r c)) (end)))


