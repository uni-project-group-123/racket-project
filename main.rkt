#lang racket

(require web-server/servlet-env
         web-server/dispatch
         web-server/dispatchers/dispatch
         web-server/dispatchers/filesystem-map
         web-server/http
<<<<<<< Updated upstream
=======
         web-server/safety-limits
>>>>>>> Stashed changes
         racket/port
         racket/runtime-path

         "database/db.rkt"
         "controllers/home.rkt"
         "controllers/auth.rkt"
         "controllers/fan-dashboard.rkt"
         "controllers/creator-dashboard.rkt"
         "controllers/concerts.rkt"
         "controllers/creator-settings.rkt"
         "utils/security.rkt")

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
   [("fan-dashboard" "selected-concerts") #:method "get" selected-concerts-page]
   [("toggle-selected-concert") #:method "post" handle-toggle-selected-concert]

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
  (with-handlers ([exn:fail?
                   (λ (e)
                     (if (regexp-match? #rx"read-mime-multipart: file exceeds max length" (exn-message e))
                         (response/xexpr
                          `(html
                            (head (title "Upload Error"))
                            (body ((style "font-family: sans-serif; padding: 50px; text-align: center;"))
                                  (h1 "File Too Large")
                                  (p "The image you uploaded is too large. Please try a smaller file.")
                                  (p (a ((href "javascript:history.back()")) "Go Back")))))
                         (raise e)))])
    (define access-result (check-access req))
    (if (eq? access-result #t)
        (dispatch req)
        access-result)))

(define-runtime-path STATIC-DIR "static")

<<<<<<< Updated upstream
;; Serve PNGs under /static/images/<name>.png directly as files
=======
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
=======
(define limits
  (make-safety-limits
   #:max-form-data-file-length 209715200
   #:max-request-body-length 209715200))

>>>>>>> Stashed changes
(serve/servlet
 start
 #:port 8080
 #:servlet-regexp #rx""
 #:launch-browser? #f
 #:extra-files-paths (list STATIC-DIR)
 #:safety-limits limits)