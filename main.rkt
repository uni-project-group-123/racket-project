#lang racket

(require web-server/servlet-env
         web-server/dispatch

         "database/db.rkt"
         "controllers/home.rkt"
         "controllers/auth.rkt"
         "controllers/fan-dashboard.rkt"
         "controllers/creator-dashboard.rkt"
         "controllers/browse.rkt")

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

   [("browse") #:method "get" browse]

   [else not-found-page]))

;; ============================
;;      SERVER START
;; ============================

(init-db)

(define (start req)
  (dispatch req))

(serve/servlet
 start
 #:port 8080
 #:servlet-regexp #rx""
 #:launch-browser? #f)