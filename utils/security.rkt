#lang racket

(provide check-access)

(require "../models/users.rkt"
         "../utils/web-utils.rkt"
         web-server/http/request-structs
         net/url
         racket/string)

;; Format: (cons regex-pattern required-role)
;; Roles: "creator", "fan", "guest" (only for non-logged in)
(define security-rules
  (list
   ;; Creator routes
   (cons #rx"^/creator-dashboard" "creator")
   (cons #rx"^/creator-settings" "creator")
   (cons #rx"^/create-concert" "creator")
   (cons #rx"^/edit-concert" "creator")
   (cons #rx"^/cancel-concert" "creator")
   (cons #rx"^/restore-concert" "creator")
   (cons #rx"^/delete-concert" "creator")
   
   ;; Fan routes
   (cons #rx"^/fan-dashboard" "fan")
   (cons #rx"^/toggle-selected-concert" "fan")
   (cons #rx"^/buy" "fan")
   
   ;; Guest routes (only accessible if NOT logged in)
   (cons #rx"^/login" "guest")
   (cons #rx"^/register" "guest")
   ))

(define (req->path req)
  (define u (request-uri req))
  (define p (url-path u))
  ;; Reconstruct path from path/param list
  (string-join (map (lambda (pp) (path/param-path pp)) p) "/" #:before-first "/"))

;; Returns #t if allowed, or a response (redirect) if denied
(define (check-access req)
  (define path (req->path req))
  
  ;; Find the first matching rule
  (define required-role
    (for/or ([rule security-rules])
      (match-define (cons pattern role) rule)
      (if (regexp-match? pattern path) role #f)))
  
  (define uid (get-cookie req "uid"))
  (define current-user (and uid (db-find-user-by-id (string->number uid))))

  (cond
    ;; No rule matched -> Public access
    [(not required-role) #t]
    
    ;; Guest only routes (login/register)
    [(string=? required-role "guest")
     (if current-user
         (redirect-303 "/") ;; Already logged in, go home
         #t)] ;; Not logged in, allowed
    
    ;; Protected routes (creator/fan)
    [else
     (if (not current-user)
         (redirect-303 "/login") ;; Not logged in, go to login
         (if (string=? (user-type current-user) required-role)
             #t ;; Role matches, allowed
             (redirect-303 "/"))) ;; Wrong role, go home
     ]))
