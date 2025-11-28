#lang racket

(require web-server/servlet-env
         web-server/dispatch
         web-server/dispatchers/dispatch
         web-server/dispatchers/filesystem-map
         racket/runtime-path

         "database/db.rkt"
         "controllers/home.rkt"
         "controllers/auth.rkt"
         "controllers/fan-dashboard.rkt"
         "controllers/creator-dashboard.rkt"
         "controllers/browse.rkt"
         "controllers/concerts.rkt")

;; ============================
;;      ROUTING
;; ============================

(define-values (dispatch dispatcher)
  (dispatch-rules
   [("") home-page]

   [("register") #:method "get" register-page]
   [("register") #:method "post" handle-register]

   [("login") #:method "get" login-page]
   [("login") #:method "post" handle-login]

   [("fan-dashboard") #:method "get" fan-dashboard]

   [("creator-dashboard") #:method "get" creator-dashboard]
  [("logout") #:method "get" handle-logout]

   [("create-concert") #:method "get" create-concert-form]
   [("create-concert") #:method "post" handle-create-concert]
   
   [("edit-concert" (string-arg)) #:method "get" edit-concert-form]
   [("edit-concert" (string-arg)) #:method "post" handle-edit-concert]
   
   [("cancel-concert" (string-arg)) #:method "get" handle-cancel-concert]
   [("restore-concert" (string-arg)) #:method "get" handle-restore-concert]
   [("delete-concert" (string-arg)) #:method "post" handle-delete-concert]

   [("browse") #:method "get" browse]



   [else not-found-page]))

;; ============================
;;      SERVER START
;; ============================

(init-db)

(define (start req)
  (dispatch req))

(define-runtime-path STATIC-DIR "static")

(serve/servlet
 start
 #:port 8080
 #:servlet-regexp #rx""
 #:launch-browser? #f
 #:extra-files-paths (list STATIC-DIR))