#lang racket

(provide home-page not-found-page)

(require "../utils/web-utils.rkt")

(define (home-page req)
  (render-page
   `(div
      (div
       (h1 "Music Portal")
       (p ((class "lead")) "Discover, share and enjoy music — for fans and creators.")
       (div ((class "nav"))
            (a ((href "/register") (class "btn btn-primary")) "Get started")
            (a ((href "/login") (class "btn btn-outline")) "Log in")
            (a ((href "/fan-dashboard") (class "btn btn-outline")) "Fan dashboard")
            (a ((href "/creator-dashboard") (class "btn btn-outline")) "Creator dashboard"))))))

(define (not-found-page req)
  (render-page
   `(div
      (div
       (h1 "404 — Page not found")
       (p ((class "lead")) "We couldn't find that page.")
       (div ((class "nav"))
            (a ((href "/") (class "btn btn-primary")) "Back to home"))))))