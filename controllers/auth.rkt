#lang racket

(provide register-page handle-register
         login-page handle-login
         handle-logout)

(require "../utils/web-utils.rkt"
         "../models/users.rkt"
         "../utils/crypto-utils.rkt"
         "./fan-dashboard.rkt"
         "./creator-dashboard.rkt"
         web-server/http
         racket/date)

;; =====================
;;     REGISTER PAGE
;; =====================

(define (register-page req)
  (if (get-cookie req "uid")
      (redirect-303 "/")
      (render-page
       #:request req
       `(div
         (h1 "Create an account")
         (p ((class "lead")) "Join Music Portal — create an account as a fan or creator.")
         (form ((action "/register") (method "post"))
               (p
                (label "Username:")
                (input ((name "name"))))
               (p
                (label "Password:")
                (input ((name "password") (type "password"))))
               (p
                (label "Account type:")
                (select ((name "type"))
                        (option ((value "fan")) "Fan")
                        (option ((value "creator")) "Creator")))
               (div ((class "actions"))
                    (button ((type "submit") (class "btn btn-primary")) "Register")
                    (a ((href "/login") (class "btn btn-outline")) "Have an account? Log in")))))))

(define (handle-register req)
  (define name (get-param req 'name))
  (define password (get-param req 'password))
  (define type (get-param req 'type))

  (cond
    [(or (not name) (not password) (not type)
         (string=? name "") (string=? password "") (string=? type ""))
     (render-page
      #:request req
      `(div
        (h1 "Error: fill in all fields")
        (div ((class "actions"))
             (a ((href "/register") (class "btn btn-outline")) "Back to register"))))]

    [else
     (with-handlers ([exn:fail?
                      (λ(e)
                        (render-page
                         #:request req
                         `(div
                           (h1 "Error: user already exists?")
                           (div ((class "actions"))
                                (a ((href "/register") (class "btn btn-outline")) "Back to register")))) )])
      (db-create-user! name password type)
      (define created (db-find-user-by-name name))
      ;; Persistent cookie (~30 days)
      (define user-cookie (make-cookie "uid" (number->string (user-id created))
                  #:path "/" #:http-only? #t #:max-age (* 60 60 24 30)))
       (cond
           [(string=? type "creator")
            (redirect-303 "/creator-dashboard" #:cookies (list user-cookie))]
           [else
            (redirect-303 "/fan-dashboard" #:cookies (list user-cookie))]))]))

;; =====================
;;       LOGIN PAGE
;; =====================

(define (login-page req)
  (if (get-cookie req "uid")
      (redirect-303 "/")
      (render-page
       #:request req
       `(div
         (h1 "Log in")
         (p ((class "lead")) "Welcome back — enter your credentials to continue.")
         (form ((action "/login") (method "post"))
               (p
                (label "Username:")
                (input ((name "name"))))
               (p
                (label "Password:")
                (input ((name "password") (type "password"))))
               (div ((class "actions"))
                    (button ((type "submit") (class "btn btn-primary")) "Log in")
                    (a ((href "/register") (class "btn btn-outline")) "Create account")))))))

(define (handle-login req)
  (define name (get-param req 'name))
  (define password (get-param req 'password))

  (define u (db-find-user-by-name name))

  (cond
    [(not u)
     (render-page
      #:request req
      `(div
        (h1 "User not found")
        (div ((class "actions"))
             (a ((href "/login") (class "btn btn-outline")) "Back to login")) ))]

    [(not (string=? (hash-password password) (user-password u)))
     (render-page
      #:request req
      `(div
        (h1 "Incorrect password")
        (div ((class "actions"))
             (a ((href "/login") (class "btn btn-outline")) "Try again")) ))]

        [else
         ;; Persistent cookie (~30 days)
         (define user-cookie (make-cookie "uid" (number->string (user-id u))
                 #:path "/" #:http-only? #t #:max-age (* 60 60 24 30)))
         (cond
       [(string=? (user-type u) "creator")
        (redirect-303 "/creator-dashboard" #:cookies (list user-cookie))]
       [else
        (redirect-303 "/fan-dashboard" #:cookies (list user-cookie))])]))

;; =====================
;;        LOGOUT
;; =====================

(define (handle-logout req)
  ;; Clear cookie by expiring it in the past
  (define cleared (make-cookie "uid" "" #:path "/" #:http-only? #t #:expires (seconds->date 0)))
  (redirect-303 "/login" #:cookies (list cleared)))
