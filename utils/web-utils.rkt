#lang racket

(provide render-page
         get-param)

(require web-server/http)

#|
This helper builds an HTML response using Racket "xexpr" syntax and
injects CSS and body content via quasiquote/unquote. Notes on how it works:

- X-expressions (xexprs) are Racket's s-expression representation of HTML/XML
  nodes. See: https://docs.racket-lang.org/reference/xexpr.html

- We construct an xexpr using quasiquote (the backtick `). Inside the quoted
  template we use unquote (comma ,) to inject Racket values (strings or
  sub-xexprs) into the generated xexpr. Example:

    `(html (head (style ,(string-append "body{...}")))
           (body ,body-xexpr))

  The expression above inserts the result of `string-append` as the contents
  of the `style` element, and inserts the `body-xexpr` value into the `body`.

- If you need to splice a list of sibling xexpr nodes into a parent, you can
  use unquote-splice `,@` inside the quasiquote. Example:

    ;; where `body-nodes` is a list of xexprs: (list `(div ...) `(section ...))
    `(body ,@body-nodes)

  Our `render-page` currently expects a single xexpr value for the body (for
  example `(div ...)`). If you decide to pass a list of nodes, switch to
  using `,@` in the body template.

- We inject CSS by building a string and placing it inside a `style` element.
  This is convenient for small, embedded styles. For larger stylesheets
  extract them to a static file and link with a `link` element instead.

- Security: do NOT inject untrusted user input directly into xexprs or style
  strings without proper escaping — this can cause XSS vulnerabilities.
  Prefer escaping functions or constructing content as xexpr nodes rather
  than raw HTML strings. See the web-server docs for safe response helpers:
  https://docs.racket-lang.org/web-server/response__xexpr.html

Examples and further reading:
- Quasiquote / unquote in Racket: https://docs.racket-lang.org/reference/notation.html
- X-expressions: https://docs.racket-lang.org/reference/xexpr.html
- web-server response/xexpr: https://docs.racket-lang.org/web-server/response__xexpr.html
|#

(define (render-page body-xexpr)
  (response/xexpr
   `(html
      (head
        (meta ((charset "utf-8")))
        (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
        (title "Music Portal")
        (style ,(string-append
                 "/* Minimal site styles */\n"
                 "html,body{height:100%;margin:0}\n"
                 "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,\n"
                 "  system-ui; color:#111;background:linear-gradient(135deg,#f8fafc,#eef2f7);display:flex;align-items:center;justify-content:center;padding:24px}\n"
                 ".container{max-width:900px;width:100%;background:white;border-radius:12px;box-shadow:0 10px 30px rgba(20,20,50,0.08);padding:36px}\n"
                 "h1{margin:0 0 8px;font-size:28px}\n"
                 "p.lead{margin:0 0 20px;color:#4b5563}\n"
                 ".nav{display:flex;gap:12px;flex-wrap:wrap}\n"
                 ".btn{display:inline-block;padding:10px 16px;border-radius:8px;text-decoration:none;font-weight:600;border:1px solid transparent}\n"
                 ".btn-primary{background:#6b46ff;color:white}\n"
                 ".btn-outline{background:transparent;border:1px solid #e6e8eb;color:#374151}\n"
                 "/* Forms */\n"
                 "form p{margin:0 0 12px;display:flex;flex-direction:column;gap:8px}\n"
                 "input,select,textarea{padding:10px 12px;border:1px solid #e6e8eb;border-radius:8px;font-size:14px}\n"
                 "input[type=submit],button[type=submit]{cursor:pointer;border-radius:8px;padding:10px 14px}\n"
                 "label{font-size:13px;color:#374151}\n"
                 " .actions{margin-top:12px;display:flex;gap:8px}\n"
                 "@media (max-width:600px){.container{padding:20px}}")) )
      (body
        (div ((class "container"))
             ,body-xexpr)))))

(define (get-param req sym)
  (define b (bindings-assq
             (string->bytes/utf-8 (symbol->string sym))
             (request-bindings/raw req)))
  (and b (bytes->string/utf-8 (binding:form-value b))))