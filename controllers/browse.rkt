#lang racket

(provide browse)

(require "../utils/web-utils.rkt")

(define (browse req)
  (render-page
   '(div
     (h1 "Test")
     (p "Waiting for concert db to pull from it and create cards.")
     )))