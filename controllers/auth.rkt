#lang racket

(provide register-page handle-register
         login-page handle-login)

(require "../utils/web-utils.rkt"
         "../models/users.rkt"
         "./fan-dashboard.rkt"
         "./creator-dashboard.rkt")

;; =====================
;;     REGISTER PAGE
;; =====================

(define (register-page req)
  (render-page
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
                (a ((href "/login") (class "btn btn-outline")) "Have an account? Log in"))))))

(define (handle-register req)
  (define name (get-param req 'name))
  (define password (get-param req 'password))
  (define type (get-param req 'type))

  (cond
    [(or (not name) (not password) (not type)
         (string=? name "") (string=? password "") (string=? type ""))
     (render-page
      `(div
        (h1 "Error: fill in all fields")
        (div ((class "actions"))
             (a ((href "/register") (class "btn btn-outline")) "Back to register"))))]

    [else
     (with-handlers ([exn:fail?
                      (λ(e)
                        (render-page
                         `(div
                           (h1 "Error: user already exists?")
                           (div ((class "actions"))
                                (a ((href "/register") (class "btn btn-outline")) "Back to register")))) )])

       (db-create-user! name password type)
       (render-page
        `(div
          (h1 "Account created!")
          (p ((class "lead")) "Your account has been created — you can now log in.")
          (div ((class "actions"))
               (a ((href "/login") (class "btn btn-primary")) "Go to login")))))]))

;; =====================
;;       LOGIN PAGE
;; =====================

(define (login-page req)
  (render-page
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
                (a ((href "/register") (class "btn btn-outline")) "Create account"))))))

(define (handle-login req)
  (define name (get-param req 'name))
  (define password (get-param req 'password))

  (define u (db-find-user-by-name name))

  (cond
    [(not u)
     (render-page
      `(div
        (h1 "User not found")
        (div ((class "actions"))
             (a ((href "/login") (class "btn btn-outline")) "Back to login")) ))]

    [(not (string=? password (user-password u)))
     (render-page
      `(div
        (h1 "Incorrect password")
        (div ((class "actions"))
             (a ((href "/login") (class "btn btn-outline")) "Try again")) ))]

    [else
     (cond
       [(string=? (user-type u) "creator")
        (render-page
         `(div (h1 "Redirecting to Creator Dashboard...")
               (meta ((http-equiv "refresh") (content "0;url=/creator-dashboard")))))]
       [else
        (render-page
         `(div (h1 "Redirecting to Fan Dashboard...")
               (meta ((http-equiv "refresh") (content "0;url=/fan-dashboard")))))])]))
