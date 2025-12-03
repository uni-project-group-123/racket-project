#lang racket

(require web-server/servlet-env
         web-server/dispatch
         web-server/dispatchers/dispatch
         web-server/dispatchers/filesystem-map
         web-server/http
         racket/port
         racket/runtime-path

         "database/db.rkt"
         "controllers/home.rkt"
         "controllers/auth.rkt"
         "controllers/fan-dashboard.rkt"
         "controllers/creator-dashboard.rkt"
         "controllers/concerts.rkt"
         "controllers/creator-settings.rkt")

;; ============================
;;      ROUTING
;; ============================

(define-values (dispatch dispatcher)
  (dispatch-rules
   [("static" "images" (string-arg)) #:method "get" serve-png]
   [("") home-page]

   [("register") #:method "get" register-page]
   [("register") #:method "post" handle-register]

   [("login") #:method "get" login-page]
   [("login") #:method "post" handle-login]

   [("fan-dashboard") #:method "get" fan-dashboard]

   [("creator-dashboard") #:method "get" creator-dashboard]
   [("creator-settings") #:method "get" creator-settings]
   [("logout") #:method "get" handle-logout]

   [("create-concert") #:method "get" create-concert-form]
   [("create-concert") #:method "post" handle-create-concert]

   [("edit-concert" (string-arg)) #:method "get" edit-concert-form]
   [("edit-concert" (string-arg)) #:method "post" handle-edit-concert]

   [("cancel-concert" (string-arg)) #:method "get" handle-cancel-concert]
   [("restore-concert" (string-arg)) #:method "get" handle-restore-concert]
   [("delete-concert" (string-arg)) #:method "post" handle-delete-concert]


   [("concert" (string-arg)) #:method "get" view-concert]
   [("buy" (string-arg)) #:method "post" handle-buy]



   [else not-found-page]))

;; ============================
;;      SERVER START
;; ============================

(init-db)

(define (start req)
  (dispatch req))

(define-runtime-path STATIC-DIR "static")

;; Serve PNGs under /static/images/<name>.png directly as files
(define (serve-png req fname)
  (define path (build-path STATIC-DIR "images" fname))
  (cond
    [(and (regexp-match? #rx"\\.png$" fname)
          (file-exists? path))
     (response/output
      #:code 200
      #:message #"OK"
      #:mime-type #"image/png"
      (lambda (out)
        (call-with-input-file path
          (lambda (in)
            (copy-port in out)))))]
    [else (not-found-page req)]))

(serve/servlet
 start
 #:port 8080
 #:servlet-regexp #rx""
 #:launch-browser? #f
 #:extra-files-paths (list STATIC-DIR))