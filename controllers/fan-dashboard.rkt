#lang racket
(provide fan-dashboard)

(require "../utils/web-utils.rkt")

;; =====================
;;  FAN DASHBOARD PAGE
;; =====================

(define (fan-dashboard req)
  (render-page
   '(div
     (h1 "Your Fan Dashboard")
     (ul
      (li (a ((href "#")) "Selected Concerts")) ; will need to
      (li (a ((href "#")) "Browse Concerts")) ; will route to browse-concert
      (li (a ((href "#")) "Profile settings")) ; can sort this out if we have time
      (li (a ((href "/")) "Sign Out")) ; back to home
      )
     )
   )
  )