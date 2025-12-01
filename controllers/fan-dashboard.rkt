#lang racket
(provide fan-dashboard)

(require "../utils/web-utils.rkt")

;; =====================
;;  FAN DASHBOARD PAGE
;; =====================

(define (fan-dashboard req)
  (render-page
   `(div
     (h1 "Your Fan Dashboard")
     (p ((class "lead")) "Quick access to your upcoming concerts, saved events and profile settings.")
     (div ((class "nav"))
          (a ((href "#" ) (class "btn btn-outline")) "Selected Concerts")
          (a ((href "/" ) (class "btn btn-outline")) "Browse Concerts")
          (a ((href "#" ) (class "btn btn-outline")) "Profile settings")
          (a ((href "/logout" ) (class "btn btn-primary")) "Sign Out")))))