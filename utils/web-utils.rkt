#lang racket

(provide render-page
         get-param)

(require web-server/http)

(define (render-page body-xexpr)
  (response/xexpr
   `(html
      (head
        (meta ((charset "utf-8")))
        (title "Music Portal"))
      (body
        ,body-xexpr))))

(define (get-param req sym)
  (define b (bindings-assq
             (string->bytes/utf-8 (symbol->string sym))
             (request-bindings/raw req)))
  (and b (bytes->string/utf-8 (binding:form-value b))))