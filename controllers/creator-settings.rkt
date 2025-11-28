#lang racket

(provide creator-settings)

(require "../utils/web-utils.rkt")

(define (creator-settings req)
  (render-page
   '(div
     (div ((class "nav")))
     (h1 "Account Settings")
     (div ((class "nav"))
          (a ((href "#" ) (class "btn btn-outline")) "Selected Concerts")
          (a ((href "/browse" ) (class "btn btn-outline")) "Browse Concerts")
          (a ((href "#" ) (class "btn btn-outline")) "Profile settings")
          (a ((href "/logout" ) (class "btn btn-primary")) "Sign Out"))
     )
   )
  )