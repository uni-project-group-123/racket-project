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
  ;; Extract filters from query string
  (define selected-location
    (let ([uri (request-uri req)])
      (if (url-query uri)
          (let ([pair (findf (λ (p) (eq? (car p) 'location)) (url-query uri))])
            (if pair (cdr pair) #f))
          #f)))

  (define selected-band-name
    (let ([uri (request-uri req)])
      (if (url-query uri)
          (let ([pair (findf (λ (p) (eq? (car p) 'band)) (url-query uri))])
            (if pair (cdr pair) #f))
          #f)))

  (define selected-price-range
    (let ([uri (request-uri req)])
      (if (url-query uri)
          (let ([pair (findf (λ (p) (eq? (car p) 'price)) (url-query uri))])
            (if pair (cdr pair) #f))
          #f)))

  ;; Start with all concerts and apply filters
  (define base-concerts (db-get-all-concerts))
  (define by-location
    (if (and selected-location (not (string=? selected-location "")))
        (filter (λ (c) (string=? (concert-location c) selected-location)) base-concerts)
        base-concerts))

  (define by-band
    (if (and selected-band-name (not (string=? selected-band-name "")))
        (filter (λ (c) (string-contains? (string-downcase (concert-name c))
                                         (string-downcase selected-band-name)))
                by-location)
        by-location))

  (define all-concerts
    (if (and selected-price-range (not (string=? selected-price-range "")))
        (let ([range-parts (string-split selected-price-range "-")])
          (if (= (length range-parts) 2)
              (let ([min-price (string->number (first range-parts))]
                    [max-price (string->number (second range-parts))])
                (filter (λ (c) (and (>= (concert-ticket-price c) min-price)
                                    (<= (concert-ticket-price c) max-price)))
                        by-band))
              by-band))
        by-band))

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
                                 `(option ((value ,loc) ,@(if (and selected-location (string=? loc selected-location))
                                                              '((selected "selected"))
                                                              '()))
                                          ,loc))
                               locations)))
           (div (label "Band Name:")
                (input ((type "text") (name "band") (placeholder "Search band name...")
                                      (value ,(or selected-band-name ""))
                                      (onchange "this.form.submit()"))))
           (div (label "Price Range:")
                (select ((name "price") (onchange "this.form.submit()"))
                        (option ((value "")) "All")
                        (option ((value "0-50") ,@(if (and selected-price-range (string=? selected-price-range "0-50"))
                                                      '((selected "selected"))
                                                      '())) "$0 - $50")
                        (option ((value "50-100") ,@(if (and selected-price-range (string=? selected-price-range "50-100"))
                                                        '((selected "selected"))
                                                        '())) "$50 - $100")
                        (option ((value "100-200") ,@(if (and selected-price-range (string=? selected-price-range "100-200"))
                                                         '((selected "selected"))
                                                         '())) "$100 - $200")
                        (option ((value "200-500") ,@(if (and selected-price-range (string=? selected-price-range "200-500"))
                                                         '((selected "selected"))
                                                         '())) "$200 - $500")
                        (option ((value "500-10000") ,@(if (and selected-price-range (string=? selected-price-range "500-10000"))
                                                           '((selected "selected"))
                                                           '())) "$500+")))

           (div ((class "concert-grid"))
                ,@(if (null? all-concerts)
                      `((p "No concerts found matching your filters."))
                      (map home-browse-concert->card all-concerts))))
     )))


(define (not-found-page req)
  (render-page
   `(div
     ,(home-top-bar req)
     (div
      (h1 "404 — Page not found")
      (p ((class "lead")) "We couldn't find that page.")
      (div ((class "nav"))
           (a ((href "/") (class "btn btn-primary")) "Back to home"))))))