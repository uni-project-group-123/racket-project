#lang racket
(provide fan-dashboard selected-concerts-page handle-toggle-selected-concert tickets-bought-page)

(require "../utils/web-utils.rkt"
         "../models/concerts.rkt"
         "../models/users.rkt"
         "../utils/image-utils.rkt"
         web-server/http
         net/url
         xml)

;; =====================
;;  FAN DASHBOARD PAGE
;; =====================

(define (fan-dashboard req)
  (render-page
   #:request req
   `(div
     (h1 "Your Fan Dashboard")
     (p ((class "lead")) "Quick access to your upcoming concerts, saved events and profile settings.")
     (div ((class "nav"))
          (a ((href "/fan-dashboard/selected-concerts" ) (class "btn btn-outline")) "Selected Concerts")
          (a ((href "/fan-dashboard/tickets-bought" ) (class "btn btn-outline")) "Tickets Bought")
          (a ((href "/" ) (class "btn btn-outline")) "Browse Concerts")
          (a ((href "#" ) (class "btn btn-outline")) "Profile settings")
          (a ((href "/logout" ) (class "btn btn-primary")) "Sign Out")))))

(define (selected-concerts-page req)
  (define raw-id (get-cookie req "uid"))
  (define user-id (string->number raw-id))
  
  (define selected (db-get-selected-concerts user-id))
  
  (render-page
   #:request req
   `(div
     (h1 "Selected Concerts")
     (p ((class "lead")) "Your wishlist of concerts.")
     (div ((class "concert-grid"))
          ,@(map (lambda (c) (selected-concert-card c user-id)) selected))
     (p (a ((href "/fan-dashboard") (class "btn btn-outline")) "Back to Dashboard")))))

(define (selected-concert-card concert user-id)
  (define img-url (normalize-concert-image (concert-image-path concert) (concert-id concert)))
  (define fallback-url (concert-image-url (concert-id concert)))
  (define sold (db-count-tickets-sold (concert-id concert)))
  (define cap (concert-max-tickets-to-sell concert))
  (define remaining (- cap sold))
  (define is-sold-out (<= remaining 0))
  (define is-cancelled (equal? (concert-status concert) "cancelled"))

  (define minus-btn
    `(form ((method "post") (action "/toggle-selected-concert") (style "margin:0;"))
           (input ((type "hidden") (name "concert_id") (value ,(number->string (concert-id concert)))))
           (input ((type "hidden") (name "action") (value "remove")))
           (input ((type "hidden") (name "redirect") (value "/fan-dashboard/selected-concerts")))
           (button ((type "submit") (class "heart-btn active") (title "Remove from list")) ,(string->xexpr svg-minus))))

  (define card
    `(div ((class ,(if is-cancelled "concert-card cancelled" "concert-card")))
          ,minus-btn
          (a ((href ,(format "/concert/~a?source=selected" (concert-id concert))) (class "card-link"))
             (div ((class "concert-image"))
                  (img ((src ,img-url)
                        (alt ,(concert-name concert))
                        (class "concert-img")
                        (loading "lazy")
                        (onerror ,(format "this.onerror=null;this.src='~a';" fallback-url)))))
             (div ((class "concert-info"))
                  (h3 ,(concert-name concert))
                  (p ((class "artist-name")) "🎤 " ,(concert-artist-name concert))
                  (p ((class "location")) "📍 " ,(concert-location concert))
                  (p ((class "date")) "🗓 " ,(format-datetime (concert-date-time concert)))
                  ,(cond
                     [is-cancelled `(div (span ((class "called-off-badge")) "Called Off"))]
                     [is-sold-out `(div (span ((class "sold-out-badge")) "Sold Out"))]
                     [else `(div
                             (p ((class "tickets")) "🎫 " ,(number->string remaining) " available")
                             (p ((class "price")) "💰 $" ,(number->string (concert-ticket-price concert))))])))))
  
  card)

(define (handle-toggle-selected-concert req)
  (define raw-id (get-cookie req "uid"))
  (define user-id (string->number raw-id))
  
  (define concert-id (string->number (get-param req 'concert_id)))
  (define action (get-param req 'action)) ;; "add" or "remove"
  (define redirect-url (or (get-param req 'redirect) "/"))

  (cond
    [(string=? action "add")
     (db-add-selected-concert! user-id concert-id)]
    [(string=? action "remove")
     (db-remove-selected-concert! user-id concert-id)])
  
    (redirect-303 redirect-url))

(define (tickets-bought-page req)
  (define raw-id (get-cookie req "uid"))
  (define user-id (string->number raw-id))
  
  (define bought (db-get-bought-concerts user-id))
  
  (render-page
   #:request req
   `(div
     (h1 "Tickets Bought")
     (p ((class "lead")) "Concerts you have purchased tickets for.")
     (div ((class "concert-grid"))
          ,@(map (lambda (c) (bought-concert-card c)) bought))
     (p (a ((href "/fan-dashboard") (class "btn btn-outline")) "Back to Dashboard")))))

(define (bought-concert-card concert)
  (define img-url (normalize-concert-image (concert-image-path concert) (concert-id concert)))
  (define fallback-url (concert-image-url (concert-id concert)))

  `(div ((class "concert-card"))
        (div ((class "concert-image"))
             (img ((src ,img-url)
                   (alt ,(concert-name concert))
                   (class "concert-img")
                   (loading "lazy")
                   (onerror ,(format "this.onerror=null;this.src='~a';" fallback-url)))))
        (div ((class "concert-info"))
             (h3 ,(concert-name concert))
             (p ((class "artist-name")) "🎤 " ,(concert-artist-name concert))
             (p ((class "location")) "📍 " ,(concert-location concert))
             (p ((class "date")) "🗓 " ,(format-datetime (concert-date-time concert))))
        (div ((class "concert-actions"))
             (a ((href ,(format "/concert/~a?source=bought" (concert-id concert))) (class "btn btn-primary")) "View Details"))))