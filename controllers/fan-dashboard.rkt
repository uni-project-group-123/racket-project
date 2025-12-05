#lang racket
(provide fan-dashboard selected-concerts-page handle-toggle-selected-concert)

(require "../utils/web-utils.rkt"
         "../models/concerts.rkt"
         "../models/users.rkt"
         "../utils/image-utils.rkt"
         web-server/http
         net/url)

;; =====================
;;  FAN DASHBOARD PAGE
;; =====================

(define (fan-dashboard req)
  (render-page
   `(div
     (h1 "Your Fan Dashboard")
     (p ((class "lead")) "Quick access to your upcoming concerts, saved events and profile settings.")
     (div ((class "nav"))
          (a ((href "/fan-dashboard/selected-concerts" ) (class "btn btn-outline")) "Selected Concerts")
          (a ((href "/" ) (class "btn btn-outline")) "Browse Concerts")
          (a ((href "#" ) (class "btn btn-outline")) "Profile settings")
          (a ((href "/logout" ) (class "btn btn-primary")) "Sign Out")))))

(define (selected-concerts-page req)
  (define raw-id (get-cookie req "uid"))
  (define user-id (string->number raw-id))
  
  (define selected (db-get-selected-concerts user-id))
  
  (render-page
   `(div
     (h1 "Selected Concerts")
     (p ((class "lead")) "Your wishlist of concerts.")
     (div ((class "concert-grid"))
          ,@(map (lambda (c) (selected-concert-card c user-id)) selected))
     (p (a ((href "/fan-dashboard") (class "btn btn-outline")) "Back to Dashboard")))))

(define (selected-concert-card concert user-id)
  (define img-url (concert-image-path concert))
  (define fallback-url (concert-image-url (concert-id concert)))
  (define final-img-url 
    (if (and img-url (not (string=? img-url ""))
             (or (string-prefix? img-url "/static/") (string-prefix? img-url "data:")))
        img-url
        fallback-url))

  `(div ((class "concert-card"))
        (div ((class "concert-image"))
             (img ((src ,final-img-url)
                   (alt ,(concert-name concert))
                   (class "concert-img")
                   (loading "lazy")
                   (onerror ,(format "this.onerror=null;this.src='~a';" fallback-url)))))
        (div ((class "concert-info"))
             (h3 ,(concert-name concert))
             (p ((class "location")) "📍 " ,(concert-location concert))
             (p ((class "date")) "🗓 " ,(concert-date-time concert)))
        (div ((class "concert-actions"))
             (form ((method "post") (action "/toggle-selected-concert") (style "margin:0;"))
                   (input ((type "hidden") (name "concert_id") (value ,(number->string (concert-id concert)))))
                   (input ((type "hidden") (name "action") (value "remove")))
                   (input ((type "hidden") (name "redirect") (value "/fan-dashboard/selected-concerts")))
                   (button ((type "submit") (class "action-btn delete-btn") (title "Remove from list")) "−")))))

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