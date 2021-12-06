#lang racket
(require "mime.rkt"
         "html.rkt"
         "read.rkt"
         web-server/http
         net/url
         (only-in web-server/servlet-dispatch dispatch/servlet)
         web-server/web-server
         web-server/dispatch
         web-server/dispatchers/filesystem-map
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in files: web-server/dispatchers/dispatch-files))

(define base-dir (current-directory))
(define gd-dir (build-path base-dir "gd"))

(define (gd-files)
  (filter (λ (x) (equal? #".gd" (path-get-extension x))) (directory-list gd-dir)))


(define (debug x)
  (printf "~s~n=======~n~n" x)
  x)

(define-values (app _)
  (dispatch-rules
   [("test") (λ (req)
               (response/jsexpr (map path->string '#hasheq((hei . "hopp")))))]
   [("preview") #:method "post"
                (λ (req)
                  (response/xexpr (gd->xexpr (string->gd (bytes->string/utf-8 (request-post-data/raw req))))))]
   [("notes") (λ (req)
                (response/jsexpr (map ~a (gd-files))))]
   [("notes" (string-arg)) (λ (req name)
                             (response/jsexpr `#hasheq((filename . ,name) (content . ,(file->string (build-path gd-dir name))))))]))

(define static-dispatcher
  (files:make #:url->path (make-url->path "static")
              #:path->mime-type (λ (p) (extension->mime-type (path-get-extension p)))))

(define (not-found req)
  (response/xexpr
   '(html () (head () (title () "404 Not Found")) (body () "404 Not Found"))
   #:code 404))

(define stop
  (serve
   #:dispatch (sequencer:make (dispatch/servlet app)
                              static-dispatcher
                              (dispatch/servlet not-found))
   #:listen-ip "127.0.0.1"
   #:port 8000))

(require net/sendurl)
;(send-url "http://localhost:8000/mlep.html")
(with-handlers ([exn:break? (lambda (e)
                              (stop))])
  (sync/enable-break never-evt))
