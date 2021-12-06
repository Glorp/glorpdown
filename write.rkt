#lang racket

(define (write-gdexpr start-char stop-char escape-char line-length gdexpr out #:lf (λ (o) (write-char #\newline o)))
  (define (write-elem x idx)
    (match x
      [()]
  
  (write-char start-char out)
  (write-char stop-char out)
  (write-char escape-char out)
  (lf out)

  (match gdexpr
    [`(doc (head . ,head-list) . ,rest)
     (write-head )]))
  