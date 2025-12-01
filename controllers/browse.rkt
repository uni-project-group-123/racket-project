#lang racket

(provide browse)

(require "../utils/web-utils.rkt")

(define (browse req)
     (redirect-303 "/"))