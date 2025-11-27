#lang racket
(provide creator-dashboard)

(require "../utils/web-utils.rkt"
         "../models/concerts.rkt"
         "../models/users.rkt"
         "../utils/image-utils.rkt")

;; =======================
;; CREATOR DASHBOARD PAGE
;; =======================

(define (creator-dashboard req)
     ;; Determine current creator from cookie; fallback to id 1.
     (define raw-id (get-cookie req "uid"))
     (define creator-id (if raw-id (string->number raw-id) 1))
     (define user-concerts (db-find-concerts-by-creator creator-id))
     (define current-user (db-find-user-by-id creator-id))
  
  (render-page
   `(div
     ;; Header with user info and logout
     (div ((class "dashboard-header"))
          (div ((class "user-info"))
               (h2 "🎶 Music Portal")
               (span ,(format "Welcome, ~a!" (or (and current-user (user-name current-user)) "Creator"))))
          (a ((href "/logout") (class "btn btn-outline logout-btn")) "Logout"))
     
     ;; Main content
     (h1 "My Concert Listings:")
     
     ;; Concert grid
     (div ((class "concert-grid"))
          ,@(map concert->card user-concerts)
          ;; Add new concert card
          (div ((class "concert-card add-new-card"))
               (a ((href "/create-concert") (class "add-new-link"))
                  (div ((class "add-icon")) "+")
                  (span "Add New Concert")))))))

;; Helper function to convert concert to card HTML
(define (concert->card concert)
  `(div ((class ,(format "concert-card ~a" (if (string=? (concert-status concert) "cancelled") "cancelled" ""))))
       (div ((class "concert-image")
            (style ,(format "background-image:url('~a');" (concert-image-url (concert-id concert))))))
        ,(if (string=? (concert-status concert) "cancelled")
             `(div ((class "cancelled-overlay")) "CANCELLED")
             "")
        (div ((class "concert-info"))
             (h3 ,(concert-name concert))
             (p ((class "location")) "📍 " ,(concert-location concert))
             (p ((class "date")) "🗓 " ,(concert-date-time concert))
             (p ((class "tickets")) "🎫 " ,(number->string (concert-max-tickets-to-sell concert)) " sold")
             (p ((class "price")) "💰 $" ,(number->string (concert-ticket-price concert))))
          (div ((class "concert-actions"))
                (a ((href ,(format "/edit-concert/~a" (concert-id concert))) 
                     (class "action-btn edit-btn")) "✏️")
                ,(if (string=? (concert-status concert) "cancelled")
                      `(form ((action ,(format "/delete-concert/~a" (concert-id concert))) (method "post") (style "display:inline"))
                               (button ((type "submit") (class "action-btn delete-btn") (onclick "return confirm('Delete permanently?')")) "🗑"))
                      ""))))