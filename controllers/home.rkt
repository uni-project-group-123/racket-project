#lang racket

(provide home-page not-found-page home-top-bar)

(require "../utils/web-utils.rkt"
         "../models/users.rkt"
         "../models/concerts.rkt"
         "../utils/image-utils.rkt"
         racket/string)

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

(define (home-browse-concert->card concert)
  (define img-url (normalize-concert-image concert))
  (define fallback-url (concert-image-url (concert-id concert)))
  (define card
    `(div ((class "concert-card"))
          (div ((class "concert-image"))
               (img ((src ,img-url)
                     (alt ,(string-append (concert-name concert) " image"))
                     (class "concert-img")
                     (loading "lazy")
                     (onerror ,(format "this.onerror=null;this.src='~a';" fallback-url)))))
          (div ((class "concert-info"))
               (h3 ,(concert-name concert))
               (p ((class "location")) "📍 " ,(concert-location concert))
               (p ((class "date")) "🗓 " ,(concert-date-time concert))
               (p ((class "tickets")) "🎫 " ,(number->string (concert-max-tickets-to-sell concert)) " available")
               (p ((class "price")) "💰 $" ,(number->string (concert-ticket-price concert))))))
  `(a ((href ,(format "/concert/~a" (concert-id concert))) (class "card-link"))
      ,card))

(define (home-page req)
  (define all-concerts (db-get-all-concerts))
  (render-page
   `(div
     ,(home-top-bar req)
     (div ((class "home-hero"))
          (h1 "Discover, share and enjoy music")
          (p ((class "lead")) "For fans and creators alike — jump in and explore."))
     (h1 "Browse All Concerts")
     (p ((class "lead")) "Discover upcoming concerts and events.")
     (div ((class "concert-grid"))
          ,@(map home-browse-concert->card all-concerts)))))

(define (not-found-page req)
  (render-page
   `(div
     ,(home-top-bar req)
     (div
      (h1 "404 — Page not found")
      (p ((class "lead")) "We couldn't find that page.")
      (div ((class "nav"))
           (a ((href "/") (class "btn btn-primary")) "Back to home"))))))