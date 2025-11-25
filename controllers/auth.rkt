#lang racket

(provide register-page handle-register
         login-page handle-login)

(require "../utils/web-utils.rkt"
         "../models/users.rkt")

;; =====================
;;     REGISTER PAGE
;; =====================

(define (register-page req)
  (render-page
   `(div
      (h1 "Create an account")
      (form ((action "/register") (method "post"))
        (p "Username:"
           (input ((name "name"))))
        (p "Password:"
           (input ((name "password") (type "password"))))
        (p "Account type:"
           (select ((name "type"))
             (option ((value "fan")) "Fan")
             (option ((value "creator")) "Creator")))
        (p (input ((type "submit") (value "Register"))))))))

(define (handle-register req)
  (define name (get-param req 'name))
  (define password (get-param req 'password))
  (define type (get-param req 'type))

  (cond
    [(or (not name) (not password) (not type)
         (string=? name "") (string=? password "") (string=? type ""))
     (render-page
      `(div (h1 "Error: fill in all fields")
            (a ((href "/register")) "Back")))]

    [else
     (with-handlers ([exn:fail? 
                      (λ(e)
                        (render-page
                         `(div
                            (h1 "Error: user already exists?")
                            (a ((href "/register")) "Back"))))])

       (db-create-user! name password type)
       (render-page
        `(div
           (h1 "Account created!")
           (a ((href "/login")) "Go to login"))))]))

;; =====================
;;       LOGIN PAGE
;; =====================

(define (login-page req)
  (render-page
   `(div
      (h1 "Log in")
      (form ((action "/login") (method "post"))
        (p "Username:"
           (input ((name "name"))))
        (p "Password:"
           (input ((name "password") (type "password"))))
        (p (input ((type "submit") (value "Log in"))))))))

(define (handle-login req)
  (define name (get-param req 'name))
  (define password (get-param req 'password))

  (define u (db-find-user-by-name name))

  (cond
    [(not u)
     (render-page
      `(div (h1 "User not found")
            (a ((href "/login")) "Back")))]

    [(not (string=? password (user-password u)))
     (render-page
      `(div (h1 "Incorrect password")
            (a ((href "/login")) "Try again")))]

    [else
     (render-page
      `(div
         (h1 ,(format "Welcome, ~a!" (user-name u)))
         (p ,(format "You are logged in as: ~a" (user-type u)))

         ,(cond
            [(string=? (user-type u) "creator")
             `(a ((href "/creator/dashboard")) "Creator Dashboard")]
            [else
             `(a ((href "/fan/dashboard")) "Fan Dashboard")])))]))