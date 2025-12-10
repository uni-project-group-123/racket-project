#lang racket

(provide create-concert-form
         handle-create-concert
         edit-concert-form
         handle-edit-concert
         handle-cancel-concert
         handle-restore-concert
         handle-delete-concert
         view-concert
         handle-buy)

(require "../utils/web-utils.rkt"
         "../models/concerts.rkt"
         "../utils/image-utils.rkt"
         "../database/db.rkt"
         db
         web-server/http
         net/uri-codec
         "home.rkt")

;; ---------- Helpers ----------
(define (current-creator-id req)
  (define cid (get-cookie req "uid"))
  (if cid (string->number cid) 1))

;; ---------- Create Form ----------
(define (create-concert-form req)
  (render-page
   #:request req
   `(div
     ,(home-top-bar req)
     (h1 "Create Concert")
     (p ((class "lead")) "Fill in the details to list your concert.")
     (form ((action "/create-concert") (method "post") (enctype "multipart/form-data"))
           (p (label "Concert Name:") (input ((name "name") (type "text") (required "required"))))
           (p (label "Location:") (input ((name "location") (type "text") (required "required"))))
           (p (label "Max Tickets to Sell:") (input ((name "max_tickets") (type "number") (min "1") (required "required"))))
           (p (label "Ticket Price ($):") (input ((name "ticket_price") (type "number") (step "0.01") (min "0") (required "required"))))
           (p (label "Date & Time:") (input ((name "date_time") (type "datetime-local") (required "required"))))
           (p (label "Concert Image (PNG):") (input ((name "image") (type "file") (accept "image/png"))))
           (div ((class "actions"))
                (button ((type "submit") (class "btn btn-primary")) "✅ Create")
                (a ((href "/creator-dashboard") (class "btn btn-outline")) "❌ Cancel"))))))

;; ---------- Handle Create ----------
(define (handle-create-concert req)
  (define creator-id (current-creator-id req))
  (define name (get-param req 'name))
  (define location (get-param req 'location))
  (define max-tickets-str (get-param req 'max_tickets))
  (define ticket-price-str (get-param req 'ticket_price))
  (define date-time (get-param req 'date_time))
  (define max-tickets (and max-tickets-str (string->number max-tickets-str)))
  (define ticket-price (and ticket-price-str (string->number ticket-price-str)))
  (if (or (not name) (string=? name "")
          (not location) (string=? location "")
          (not date-time) (string=? date-time "")
          (not max-tickets) (<= max-tickets 0)
          (not ticket-price) (< ticket-price 0))
      (render-page
       #:request req
       `(div ,(home-top-bar req)
             (h1 "Error: Invalid input")
             (p "Please fill in all fields with valid values.")
             (div ((class "actions")) (a ((href "/create-concert") (class "btn btn-outline")) "Back to form"))))
      (with-handlers ([exn:fail?
                       (λ (e)
                         (render-page
                          #:request req
                          `(div ,(home-top-bar req)
                                (h1 "Error creating concert") (p ,(format "~a" e))
                                (div ((class "actions")) (a ((href "/create-concert") (class "btn btn-outline")) "Back to form")))) )])
        (define dbc (get-db))
        (db-create-concert! creator-id name max-tickets ticket-price location "" date-time)
        (define new-id (query-value dbc "SELECT last_insert_rowid();"))
        (define img-binding (bindings-assq #"image" (request-bindings/raw req)))
        (when (and img-binding (binding:file? img-binding) (> (bytes-length (binding:file-content img-binding)) 0))
          (define headers (binding:file-headers img-binding))
          (define content-type (for/or ([h headers])
                                 (and (header? h)
                                      (string-ci=? (bytes->string/utf-8 (header-field h)) "Content-Type")
                                      (bytes->string/utf-8 (header-value h)))))
          (define saved-path (save-concert-image-with-id (binding:file-content img-binding) content-type new-id))
          (db-set-concert-image-path! new-id saved-path))
        (render-page
         #:request req
         `(div ,(home-top-bar req)
               (h1 "Concert Created!")
               (p ((class "lead")) "Your concert has been listed.")
               (div ((class "actions"))
                    (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard")
                    (a ((href ,(format "/concert/~a" new-id)) (class "btn btn-outline")) "View Listing")))))))

;; ---------- Edit Form ----------
(define (edit-concert-form req concert-id-str)
  (define cid (string->number concert-id-str))
  (define c (db-find-concert-by-id cid))
  (if (not c)
      (render-page #:request req `(div ,(home-top-bar req) (h1 "Concert not found") (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-outline")) "Back to Dashboard"))))
      (render-page
       #:request req
       `(div
         ,(home-top-bar req)
         (h1 "Edit Concert")
         (p ((class "lead")) "Update your concert details.")
         (form ((action ,(format "/edit-concert/~a" cid)) (method "post") (enctype "multipart/form-data"))
               (p (label "Concert Name:") (input ((name "name") (type "text") (required "required") (value ,(concert-name c)))))
               (p (label "Location:") (input ((name "location") (type "text") (required "required") (value ,(concert-location c)))))
               (p (label "Max Tickets to Sell:") (input ((name "max_tickets") (type "number") (required "required") (min "1") (value ,(number->string (concert-max-tickets-to-sell c))))))
               (p (label "Ticket Price ($):") (input ((name "ticket_price") (type "number") (step "0.01") (required "required") (min "0") (value ,(number->string (concert-ticket-price c))))))
               (p (label "Date & Time:") (input ((name "date_time") (type "datetime-local") (required "required") (value ,(concert-date-time c)))))
               (p (label "Update Concert Image:") (input ((name "image") (type "file") (accept "image/png"))))
              (div ((class "actions"))
                (button ((type "submit") (class "btn btn-primary")) "✅ Save")
                ,(if (string=? (concert-status c) "active")
                  `(a ((href ,(format "/cancel-concert/~a" cid)) (class "btn btn-outline btn-warning")) "⚠️ Call Off")
                  `(a ((href ,(format "/restore-concert/~a" cid)) (class "btn btn-outline restore-btn")) "↩ Restore"))
                (a ((href "/creator-dashboard") (class "btn btn-outline")) "❌ Cancel")))))))

;; ---------- Handle Edit ----------
(define (handle-edit-concert req concert-id-str)
  (define cid (string->number concert-id-str))
  (define existing (db-find-concert-by-id cid))
  (define name (get-param req 'name))
  (define location (get-param req 'location))
  (define max-tickets-str (get-param req 'max_tickets))
  (define ticket-price-str (get-param req 'ticket_price))
  (define date-time (get-param req 'date_time))
  (define max-tickets (and max-tickets-str (string->number max-tickets-str)))
  (define ticket-price (and ticket-price-str (string->number ticket-price-str)))
  (define img-binding (bindings-assq #"image" (request-bindings/raw req)))
  (when (and img-binding (binding:file? img-binding) (> (bytes-length (binding:file-content img-binding)) 0))
    (define headers (binding:file-headers img-binding))
    (define content-type (for/or ([h headers])
                           (and (header? h)
                                (string-ci=? (bytes->string/utf-8 (header-field h)) "Content-Type")
                                (bytes->string/utf-8 (header-value h)))))
    (define saved-path (save-concert-image-with-id (binding:file-content img-binding) content-type cid))
    (db-set-concert-image-path! cid saved-path))
  (if (or (not name) (not location) (not max-tickets) (not ticket-price) (not date-time)
          (string=? name "") (string=? location "") (string=? date-time "")
          (<= max-tickets 0) (< ticket-price 0))
      (render-page
       #:request req
       `(div ,(home-top-bar req) (h1 "Error: Invalid input") (p "Please fill in all fields with valid values.")
             (div ((class "actions")) (a ((href ,(format "/edit-concert/~a" cid)) (class "btn btn-outline")) "Back to form"))))
      (with-handlers ([exn:fail?
                       (λ (e)
                         (render-page
                          #:request req
                          `(div ,(home-top-bar req) (h1 "Error updating concert") (p ,(format "~a" e))
                                (div ((class "actions")) (a ((href ,(format "/edit-concert/~a" cid)) (class "btn btn-outline")) "Back to form")))) )])
        (define status (if existing (concert-status existing) "active"))
        (define kept (let ([c (db-find-concert-by-id cid)]) (if c (concert-image-path c) "")))
        (db-update-concert! cid name max-tickets ticket-price location kept date-time status)
        (render-page #:request req `(div ,(home-top-bar req) (h1 "Concert Updated!") (p ((class "lead")) "Your concert listing has been updated successfully.")
                           (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard")))))))

;; ---------- Cancel ----------
(define (handle-cancel-concert req concert-id-str)
  (define cid (string->number concert-id-str))
  (with-handlers ([exn:fail?
                   (λ (e)
                     (render-page
                      #:request req
                      `(div ,(home-top-bar req) (h1 "Error cancelling concert") (p ,(format "~a" e))
                            (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-outline")) "Back to Dashboard")))) )])
    (db-cancel-concert! cid)
    (render-page
     #:request req
     `(div ,(home-top-bar req) (h1 "Concert Cancelled") (p ((class "lead")) "The concert has been marked as cancelled.")
           (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard"))))))

;; ---------- Restore ----------
(define (handle-restore-concert req concert-id-str)
  (define cid (string->number concert-id-str))
  (with-handlers ([exn:fail?
                   (λ (e)
                     (render-page
                      #:request req
                      `(div ,(home-top-bar req) (h1 "Error restoring concert") (p ,(format "~a" e))
                            (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-outline")) "Back to Dashboard")))) )])
    (db-restore-concert! cid)
    (render-page
     #:request req
     `(div ,(home-top-bar req) (h1 "Concert Restored") (p ((class "lead")) "The concert has been marked as active again.")
           (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard"))))))

;; ---------- Delete ----------
(define (handle-delete-concert req concert-id-str)
  (define cid (string->number concert-id-str))
  (with-handlers ([exn:fail?
                   (λ (e)
                     (render-page
                      #:request req
                      `(div ,(home-top-bar req) (h1 "Error deleting concert") (p ,(format "~a" e))
                            (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-outline")) "Back to Dashboard")))) )])
    (db-delete-concert! cid)
    (render-page
     #:request req
     `(div ,(home-top-bar req) (h1 "Concert Deleted")
           (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard"))))))

;; ---------- View (fan) ----------
(define (view-concert req concert-id-str)
  (define cid (string->number concert-id-str))
  (define c (db-find-concert-by-id cid))
  (define body-xpr
    (if (not c)
     `(div ,(home-top-bar req)
        (h1 "Concert not found")
        (div ((class "actions")) (a ((href "/") (class "btn btn-outline")) "Back")))
     (let* ([sold (db-count-tickets-sold cid)]
      [cap (concert-max-tickets-to-sell c)]
      [remaining (- cap sold)]
      [img-url (concert-image-url cid)]
      [source (or (get-param req 'source) "home")]
      [back-link (cond
                   [(string=? source "bought") "/fan-dashboard/tickets-bought"]
                   [(string=? source "selected") "/fan-dashboard/selected-concerts"]
                   [else "/"])]
      [back-text (cond
                   [(string=? source "bought") "← Back to Tickets"]
                   [(string=? source "selected") "← Back to Selected"]
                   [else "← Back to Concerts"])])
    (define action-x
      (cond
        [(string=? (concert-status c) "cancelled")
      `(p ((class "notice")) "⚠️ This concert has been cancelled.")]
        [(<= remaining 0)
      `(p ((class "notice")) "Sold out.")]
        [else
      `(form ((action ,(format "/buy/~a" cid)) (method "post"))
          (label "Quantity: ")
          (input ((type "number") (name "qty") (min "1") (max ,(number->string remaining)) (value "1")))
          (button ((type "submit") (class "btn btn-primary")) "Buy"))]))
    `(div
      ,(home-top-bar req)
      (a ((href ,back-link) (class "back-btn")) ,back-text)
      (div ((class "concert-detail"))
        (div ((class "media"))
          (img ((src ,img-url)
             (alt ,(string-append (concert-name c) " image"))
             (class "concert-img"))))
        (div ((class "info"))
          (h1 ,(concert-name c))
          (p ((class "artist-name")) "🎤 " ,(concert-artist-name c))
          (p ((class "location")) "📍 " ,(concert-location c))
          (p ((class "date")) "🗓 " ,(format-datetime (concert-date-time c)))
          (p ((class "price")) "💰 $" ,(number->string (concert-ticket-price c)))
          ,action-x))))))
  (render-page #:request req body-xpr))

;; ---------- Buy ----------
(define (handle-buy req concert-id-str)
  (define uid (get-cookie req "uid"))
  (if (not uid)
  (redirect-303 "/login")
  (let* ([cid (string->number concert-id-str)]
     [qty-str (get-param req 'qty)]
     [qty (if qty-str (string->number qty-str) 1)]
     [dbc (get-db)]
     [c (db-find-concert-by-id cid)])
    (cond
      [(not c)
       (render-page
        #:request req
    `(div ,(home-top-bar req)
      (h1 "Concert not found")
      (div ((class "actions")) (a ((href "/") (class "btn btn-outline")) "Back"))))]
      [else
       (let* ([sold (db-count-tickets-sold cid)]
      [cap (concert-max-tickets-to-sell c)]
      [remaining (- cap sold)])
     (cond
       [(string=? (concert-status c) "cancelled")
        (render-page
         #:request req
         `(div ,(home-top-bar req)
           (h1 "Concert is cancelled")
           (div ((class "actions")) (a ((href ,(format "/concert/~a" cid)) (class "btn btn-outline")) "Back"))))]
       [(<= remaining 0)
        (render-page
         #:request req
         `(div ,(home-top-bar req)
           (h1 "Sold out")
           (div ((class "actions")) (a ((href ,(format "/concert/~a" cid)) (class "btn btn-outline")) "Back"))))]
       [(< remaining qty)
        (begin
          (query-exec dbc
              "INSERT INTO tickets (concert_id, buyer_id, qty, purchased_at) VALUES (?, ?, ?, datetime('now'));"
              cid (string->number uid) remaining)
          (render-page
           #:request req
           `(div ,(home-top-bar req)
             (h1 "Partial Purchase")
             (p ((class "lead")) ,(format "You requested ~a tickets, but only ~a were available. You have purchased ~a tickets." qty remaining remaining))
             (div ((class "actions"))
              (a ((href "/fan-dashboard") (class "btn btn-primary")) "Go to Dashboard")
              (a ((href ,(format "/concert/~a" cid)) (class "btn btn-outline")) "Back to Concert")))))]
       [else
        (begin
      (query-exec dbc
          "INSERT INTO tickets (concert_id, buyer_id, qty, purchased_at) VALUES (?, ?, ?, datetime('now'));"
          cid (string->number uid) qty)
      (render-page
       #:request req
       `(div ,(home-top-bar req)
         (h1 "✅ Purchase complete")
         (p ((class "lead")) "Your ticket has been added to your account.")
         (div ((class "actions"))
          (a ((href "/fan-dashboard") (class "btn btn-primary")) "Go to Dashboard")
          (a ((href ,(format "/concert/~a" cid)) (class "btn btn-outline")) "Back to Concert")))))]))]))))