#lang racket

(provide hash-password)
(require file/sha1)

(define (hash-password raw-password)
  (call-with-input-string raw-password sha1))
