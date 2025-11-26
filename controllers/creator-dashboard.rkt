#lang racket
(provide creator-dashboard)

(require "../utils/web-utils.rkt")

;; =======================
;; CREATOR DASHBOARD PAGE
;; =======================

(define (creator-dashboard req)
  (render-page
   '(div
     (h1 "Your Creator Dashboard")
     (ul
      (li (a ((href "#")) "Create Concert Listing")) ; Post to concert db
      (li (a ((href "#")) "Manage Concert Listings")) ; Get and post to concert db
      (li (a ((href "#")) "Profile settings")) ; can sort this out if we have time
      (li (a ((href "/")) "Sign Out")) ; back to home
      )
     )
   )
  )