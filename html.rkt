#lang racket
(require "gd.rkt")

(provide gd->xexpr)

(define (er x)
  `((span ((class "error")) ,(~a x))))

(define ((elem [recur elem]) x)
  (match x
    [(err s) (er s)]
    [(str 'text s _) (list s)]
    [(str 'space _ _) (list " ")]
    [(list (str text tag _) rest ...)
     (define (halp) (apply append (map (recur) rest)))
     (match tag
       ["`" `((code () . ,(halp)))]
       ["\"" `("“" ,@(halp) "”")]
       ["_" `((em () . ,(halp)))]
       ["" '()]
       ["ol" `((ol () . ,(listy rest)))]
       ["ul" `((ul () . ,(listy rest)))]
       [_ (er (format "unkown tag: ~a" tag))])]
    [_ (er x)]))

(define (split l pred?)
  (let loop ([cur rnil] [res rnil] [l l])
    (match l
      ['() ((res (add (cur (get)))) (get))]
      [(list first rest ...) (if (pred? first)
                                 (loop rnil (res (add (cur (get)))) rest)
                                 (loop (cur (add first)) res rest))])))

(define ((pre-elem) x)
  (match x
    [(str 'space s _) (list s)]
    [_ ((elem pre-elem) x)]))
  
(define (listy l)
  (match l
    [(list (str 'space _ _) (str 'text li _) rest ...)
     (define ls (split rest (λ (x) (match x [(str 'text (? (λ (x) (string=? x li))) _) #t] [_ #f]))))
     (apply append (map (λ (x) `((li () . ,(apply append (map (elem) x))))) ls))]
    [_ (er "expected first list element")]))

(define (quoty l)
  (define (halp l)
    (define ls (split l (λ (x) (match x [(list (str 'text "" _) _ ...) #t] [_ #f]))))
    (apply append (map (λ (x) `((p () . ,(apply append (map (elem) x))))) ls)))
      
  (match (split l (λ (x) (match x [(list (str 'text "cite" _) _ ...) #t] [_ #f])))
    [(list before after) `((blockquote () ,@(halp before) (cite () ,@(apply append (map (elem) after)))))]
    [(list before) `((blockquote () ,@(halp before)))]
    [_ (er "strange quote-stuff")]))

(define (head-el x)
  (match x
    [(list (str 'text s _) rest ...) `(,s ": " ,@(apply append (map (elem) rest)) (br ()))]
    [(str 'space s _) '()]
    [(str 'text s _) (er (format "text in header: ~a" s))]))

(define (para x)
  (match x
    [(list (list (str 'text s _) tag-rest ...) rest ...)
     (match s
       ["code" `((pre () . ,(apply append (map (pre-elem) rest))))]
       ["quote" (quoty rest)]
       [_ `((p () . ,(apply append (map (elem) x))))])]
    [(list (and el (list (str 'text (or "ol" "ul") _) rest ...))) (elem el)]
    [_ `((p () . ,(apply append (map (elem) x))))]))

(define (gd->xexpr g)
  (match g
    [(list hd rest ...)
     `(div ()
           ,@(apply append (map head-el hd))
           ,@(apply append (map para rest)))]))


