#lang racket

(provide create-concert-form
         handle-create-concert
         edit-concert-form
         handle-edit-concert
         handle-cancel-concert
         handle-restore-concert
         handle-delete-concert)

(require "../utils/web-utils.rkt"
         "../models/concerts.rkt"
         "../utils/image-utils.rkt"
         "../database/db.rkt"
         db
         web-server/http)

;; ---------- Helpers ----------
(define (current-creator-id req)
  (define cid (get-cookie req "uid"))
  (if cid (string->number cid) 1))

(define (extract-image-path req existing)
  (define b (bindings-assq #"image" (request-bindings/raw req)))
  (cond
    [(and b (binding:file? b) (> (bytes-length (binding:file-content b)) 0))
     (define file-bytes (binding:file-content b))
     (define filename-bytes (binding:file-filename b))
     (define filename (and filename-bytes (bytes->string/utf-8 filename-bytes)))
     (define headers (binding:file-headers b))
     (define content-type (for/or ([h headers])
                            (and (header? h)
                                 (string-ci=? (bytes->string/utf-8 (header-field h)) "Content-Type")
                                 (bytes->string/utf-8 (header-value h)))) )
     (define ct (or content-type "image/jpeg"))
     (with-handlers ([exn:fail? (λ(e) (or existing ""))])
       (save-uploaded-image file-bytes (or filename "upload.jpg") ct))]
    [else (or existing "")]))

;; ---------- Create Form ----------
(define (create-concert-form req)
  (render-page
   `(div
     (h1 "Create New Concert")
     (p ((class "lead")) "Fill out the details for your new concert listing.")
     (form ((action "/create-concert") (method "post") (enctype "multipart/form-data"))
           (p (label "Concert Name:") (input ((name "name") (type "text") (required "required"))))
           (p (label "Location:") (input ((name "location") (type "text") (required "required") (placeholder "e.g. The O2, London"))))
           (p (label "Max Tickets to Sell:") (input ((name "max_tickets") (type "number") (required "required") (min "1"))))
           (p (label "Ticket Price ($):") (input ((name "ticket_price") (type "number") (step "0.01") (required "required") (min "0"))))
           (p (label "Date & Time:") (input ((name "date_time") (type "datetime-local") (required "required"))))
           (p (label "Concert Image:") (input ((name "image") (type "file") (accept "image/png"))))
           (div ((class "actions"))
                (button ((type "submit") (class "btn btn-primary")) "✅ Save")
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
  ;; We'll save the image after we know the new concert ID
  (define img-binding (bindings-assq #"image" (request-bindings/raw req)))
  (if (or (not name) (not location) (not max-tickets) (not ticket-price) (not date-time)
          (string=? name "") (string=? location "") (string=? date-time "")
          (<= max-tickets 0) (< ticket-price 0))
      (render-page
       `(div (h1 "Error: Invalid input")
             (p "Please fill in all fields with valid values.")
             (div ((class "actions"))
                  (a ((href "/create-concert") (class "btn btn-outline")) "Back to form"))))
      (with-handlers ([exn:fail?
                       (λ(e)
                         (render-page
                          `(div (h1 "Error creating concert")
                                (p ,(format "~a" e))
                                (div ((class "actions"))
                                     (a ((href "/create-concert") (class "btn btn-outline")) "Back to form")))) )])
        ;; 1) Insert without image_path
        (db-create-concert! creator-id name max-tickets ticket-price location "" date-time)
        ;; 2) Get new id and save file if provided
        (define new-id (query-value (get-db) "SELECT last_insert_rowid();"))
        (when (and img-binding (binding:file? img-binding) (> (bytes-length (binding:file-content img-binding)) 0))
          (define headers (binding:file-headers img-binding))
          (define content-type (for/or ([h headers])
                                 (and (header? h)
                                      (string-ci=? (bytes->string/utf-8 (header-field h)) "Content-Type")
                                      (bytes->string/utf-8 (header-value h)))))
          (define saved-path (save-concert-image-with-id (binding:file-content img-binding) content-type new-id))
          (db-set-concert-image-path! new-id saved-path))
        (render-page
         `(div (h1 "Concert Created!")
               (p ((class "lead")) "Your concert listing has been created successfully.")
               (div ((class "actions"))
                    (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard")))))))

;; ---------- Edit Form ----------
(define (edit-concert-form req concert-id-str)
  (define cid (string->number concert-id-str))
  (define c (db-find-concert-by-id cid))
  (if (not c)
      (render-page `(div (h1 "Concert not found") (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-outline")) "Back to Dashboard"))))
      (render-page
       `(div
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
  ;; Save new image if uploaded
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
       `(div (h1 "Error: Invalid input") (p "Please fill in all fields with valid values.")
             (div ((class "actions")) (a ((href ,(format "/edit-concert/~a" cid)) (class "btn btn-outline")) "Back to form"))))
      (with-handlers ([exn:fail?
                       (λ(e)
                         (render-page
                          `(div (h1 "Error updating concert") (p ,(format "~a" e))
                                (div ((class "actions")) (a ((href ,(format "/edit-concert/~a" cid)) (class "btn btn-outline")) "Back to form")))) )])
          (define status (if existing (concert-status existing) "active"))
          ;; Keep current image_path as stored (possibly updated above)
          (define kept (let ([c (db-find-concert-by-id cid)]) (if c (concert-image-path c) "")))
          (db-update-concert! cid name max-tickets ticket-price location kept date-time status)
        (render-page
         `(div (h1 "Concert Updated!") (p ((class "lead")) "Your concert listing has been updated successfully.")
               (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard")))))))

;; ---------- Cancel ----------
(define (handle-cancel-concert req concert-id-str)
  (define cid (string->number concert-id-str))
  (with-handlers ([exn:fail?
                   (λ(e)
                     (render-page
                      `(div (h1 "Error cancelling concert") (p ,(format "~a" e))
                            (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-outline")) "Back to Dashboard")))) )])
    (db-cancel-concert! cid)
    (render-page
     `(div (h1 "Concert Cancelled") (p ((class "lead")) "The concert has been marked as cancelled.")
           (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard"))))))

;; ---------- Restore ----------
(define (handle-restore-concert req concert-id-str)
  (define cid (string->number concert-id-str))
  (with-handlers ([exn:fail?
                   (λ(e)
                     (render-page
                      `(div (h1 "Error restoring concert") (p ,(format "~a" e))
                            (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-outline")) "Back to Dashboard")))) )])
    (db-restore-concert! cid)
    (render-page
     `(div (h1 "Concert Restored") (p ((class "lead")) "The concert has been marked as active again.")
           (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard"))))))

;; ---------- Delete ----------
(define (handle-delete-concert req concert-id-str)
  (define cid (string->number concert-id-str))
  (with-handlers ([exn:fail?
                   (λ(e)
                     (render-page
                      `(div (h1 "Error deleting concert") (p ,(format "~a" e))
                            (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-outline")) "Back to Dashboard")))) )])
    ;; Remove the image file first
    (delete-concert-image! cid)
    (db-delete-concert! cid)
    (render-page
     `(div (h1 "Concert Deleted") (p ((class "lead")) "The concert has been removed from your listings.")
           (div ((class "actions")) (a ((href "/creator-dashboard") (class "btn btn-primary")) "Back to Dashboard"))))))