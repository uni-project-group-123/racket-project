#lang racket

(provide home-page not-found-page home-top-bar)

(require "../utils/web-utils.rkt"
         "../models/users.rkt"
         "../models/concerts.rkt"
         "../utils/image-utils.rkt"
         racket/string
         web-server/http
         net/url)

;; Reusable top bar which adapts to logged-in state via cookie
(define (home-top-bar req)
  (define raw-id (get-cookie req "uid"))
  (define current-user (and raw-id (db-find-user-by-id (string->number raw-id))))
  (define current-name (and current-user (user-name current-user)))
  (define current-type (and current-user (user-type current-user)))
  (define dashboard-link (if (and current-type (string=? current-type "creator"))
                             "/creator-dashboard" "/fan-dashboard"))
  `(div ((class "topbar"))
        (div ((class "brand"))
             (a ((href "/") (class "brand-link")) "Music Portal"))
        ,(if current-user
             `(div ((class "user-menu"))
                   (span ((class "user-name")) ,current-name)
                   (div ((class "dropdown"))
                        (a ((href ,dashboard-link)) "Dashboard")
                        (a ((href "/")) "Browse")
                        (a ((href "/logout")) "Logout")))
             `(div ((class "nav"))
                   (a ((href "/") (class "btn btn-outline")) "Browse")
                   (a ((href "/register") (class "btn btn-outline")) "Register")
                   (a ((href "/login") (class "btn btn-primary")) "Log in")))))

(define (normalize-concert-image concert)
  (define raw (concert-image-path concert))
  (cond
    [(and raw (not (string=? raw ""))
          (or (string-prefix? raw "/static/") (string-prefix? raw "data:"))) raw]
    ;; If raw path appears to be a filesystem path (contains backslash or drive letter), ignore it.
    [else (concert-image-url (concert-id concert))]))

<<<<<<< Updated upstream
(define (home-browse-concert->card concert)
  (define img-url (normalize-concert-image concert))
  (define fallback-url (concert-image-url (concert-id concert)))
=======
(define (home-browse-concert->card concert current-user)
  (define img-url (normalize-concert-image concert))
  (define fallback-url (concert-image-url (concert-id concert)))
  (define creator (db-find-user-by-id (concert-creator-id concert)))
  (define creator-name (if creator (user-name creator) "Unknown"))
  
  (define is-fan (and current-user (string=? (user-type current-user) "fan")))
  (define is-selected (and is-fan (db-user-has-selected-concert? (user-id current-user) (concert-id concert))))
  
  (define action-btn
    (if is-fan
        `(div ((class "concert-actions"))
              (form ((method "post") (action "/toggle-selected-concert") (style "margin:0;"))
                    (input ((type "hidden") (name "concert_id") (value ,(number->string (concert-id concert)))))
                    (input ((type "hidden") (name "action") (value ,(if is-selected "remove" "add"))))
                    (input ((type "hidden") (name "redirect") (value "/")))
                    (button ((type "submit") 
                             (class ,(if is-selected "action-btn delete-btn" "action-btn edit-btn")) 
                             (title ,(if is-selected "Remove from list" "Add to list"))) 
                            ,(if is-selected "−" "+"))))
        ""))

>>>>>>> Stashed changes
  (define card
    `(div ((class "concert-card"))
          (div ((class "concert-image"))
               (img ((src ,img-url)
                     (alt ,(string-append (concert-name concert) " image"))
                     (class "concert-img")
                     (loading "lazy")
                     (onerror ,(format "this.onerror=null;this.src='~a';" fallback-url)))))
          (div ((class "concert-info"))
<<<<<<< Updated upstream
               (h3 ,(concert-name concert))
               (p ((class "location")) "📍 " ,(concert-location concert))
               (p ((class "date")) "🗓 " ,(concert-date-time concert))
               (p ((class "tickets")) "🎫 " ,(number->string (concert-max-tickets-to-sell concert)) " available")
               (p ((class "price")) "💰 $" ,(number->string (concert-ticket-price concert))))))
=======
               (h3 ,(string-append (concert-name concert) " - " creator-name))
               (p ((class "location")) "📍 " ,(concert-location concert))
               (p ((class "date")) "🗓 " ,(concert-date-time concert))
               (p ((class "tickets")) "🎫 " ,(number->string (concert-max-tickets-to-sell concert)) " available")
               (p ((class "price")) "💰 $" ,(number->string (concert-ticket-price concert))))
          ,action-btn))
>>>>>>> Stashed changes
  `(a ((href ,(format "/concert/~a" (concert-id concert))) (class "card-link"))
      ,card))

(define (home-page req)
  ;; Extract location from query string
  (define selected-location
    (let ([uri (request-uri req)])
      (if (url-query uri)
          (let* ([query (url-query uri)]
                 ;; query is a list of (name . value) pairs where name is a symbol
                 [pair (findf (λ (p)
                                (eq? (car p) 'location))
                              query)])
            (if pair
                (cdr pair)
                #f))
          #f)))

<<<<<<< Updated upstream
  (define all-concerts
    (if (and selected-location (not (string=? selected-location "")))
        (db-find-concerts-by-location selected-location)
        (db-get-all-concerts)))
=======
  (define raw-id (get-cookie req "uid"))
  (define current-user (and raw-id (db-find-user-by-id (string->number raw-id))))

  (define all-concerts
    (let ([concerts (if (and selected-location (not (string=? selected-location "")))
                        (db-find-concerts-by-location selected-location)
                        (db-get-all-concerts))])
      (filter (λ (c) (not (string=? (concert-status c) "cancelled"))) concerts)))
>>>>>>> Stashed changes
  (define locations (db-get-all-locations))

  (render-page
   `(div
     ,(home-top-bar req)
     (div ((class "home-hero"))
          (h1 "Discover, share and enjoy music")
          (p ((class "lead")) "For fans and creators alike — jump in and explore."))
     (h1 "Browse All Concerts")
     (p ((class "lead")) "Discover upcoming concerts and events.")
     (p "Filter by:")
     (form ((method "get") (action "/"))
           (div (label "Location:")
                (select ((name "location") (onchange "this.form.submit()"))
                        (option ((value "")) "All")
                        ,@(map (λ (loc)
                                 `(option ((value ,loc) ,@(if (string=? loc (or selected-location ""))
                                                              '((selected "selected"))'())) ,loc))
                               locations))))
     (div ((class "concert-grid"))
          ,@(if (null? all-concerts)
                `((p "No concerts found for this location."))
<<<<<<< Updated upstream
                (map home-browse-concert->card all-concerts))))))
=======
                (map (λ (c) (home-browse-concert->card c current-user)) all-concerts))))))
>>>>>>> Stashed changes


(define (not-found-page req)
  (render-page
   `(div
     ,(home-top-bar req)
     (div
      (h1 "404 — Page not found")
      (p ((class "lead")) "We couldn't find that page.")
      (div ((class "nav"))
           (a ((href "/") (class "btn btn-primary")) "Back to home"))))))