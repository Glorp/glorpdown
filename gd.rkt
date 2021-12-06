#lang racket

(provide (struct-out location)
         (struct-out text-range)
         (struct-out char)
         (struct-out str)
         (struct-out err)
         (struct-out add)
         (struct-out get)
         rnil)

(struct char (value location) #:transparent)
(struct str (cat value range) #:transparent)
(struct err (str) #:transparent)
(struct location (row column) #:transparent)
(struct text-range (start stop) #:transparent)

(struct add (value) #:transparent)
(struct get () #:transparent)

(define rnil
  (let loop ([l '()])
    (λ (x)
      (match x
        [(add v) (loop (cons v l))]
        [(get) (reverse l)]))))

