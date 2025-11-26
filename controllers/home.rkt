#lang racket

(provide home-page not-found-page)

(require "../utils/web-utils.rkt")

(define (home-page req)
  (render-page
   `(div
      (h1 "Music Portal")
      (ul
        (li (a ((href "/register")) "Register"))
        (li (a ((href "/login")) "Log in"))))))

(define (not-found-page req)
  (render-page
   `(div
      (h1 "404 - Page not found")
      (a ((href "/")) "Back to home"))))