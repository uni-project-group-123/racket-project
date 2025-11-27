#lang racket

(provide browse)

(require "../utils/web-utils.rkt"
         "../models/concerts.rkt")

(define (browse req)
  (define all-concerts (db-get-all-concerts))
  
  (render-page
   `(div
     (h1 "Browse All Concerts")
     (p ((class "lead")) "Discover upcoming concerts and events.")
     
     (div ((class "nav"))
          (a ((href "/") (class "btn btn-outline")) "← Back to Home"))
     
     (div ((class "concert-grid"))
          ,@(map browse-concert->card all-concerts)))))

;; Helper function for browse view (without edit/delete buttons)
(define (browse-concert->card concert)
  `(div ((class "concert-card"))
        (div ((class "concert-image"))
             (img ((src ,(if (concert-image-path concert) 
                             (concert-image-path concert) 
                             "/static/images/default-concert.png"))
                   (alt ,(concert-name concert)))))
        (div ((class "concert-info"))
             (h3 ,(concert-name concert))
             (p ((class "location")) "📍 " ,(concert-location concert))
             (p ((class "date")) "🗓 " ,(concert-date-time concert))
             (p ((class "tickets")) "🎫 " ,(number->string (concert-max-tickets-to-sell concert)) " available")
             (p ((class "price")) "💰 $" ,(number->string (concert-ticket-price concert))))))