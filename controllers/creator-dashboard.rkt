#lang racket
(provide creator-dashboard)

(require "../utils/web-utils.rkt")

;; =======================
;; CREATOR DASHBOARD PAGE
;; =======================

(define (creator-dashboard req)
  (render-page
   `(div
     (h1 "Your Creator Dashboard")
     (p ((class "lead")) "Manage your concerts, listings and profile — tools for creators.")
     (div ((class "nav"))
          (a ((href "#" ) (class "btn btn-primary")) "Create Concert Listing")
          (a ((href "#" ) (class "btn btn-outline")) "Manage Concert Listings")
          (a ((href "#" ) (class "btn btn-outline")) "Profile settings")
          (a ((href "/" ) (class "btn btn-outline")) "Sign Out")))))