#lang racket

(provide render-page
         render-page/cookies
         redirect-303
         get-param
         get-cookie)

(require web-server/http)

#|
Utility helpers: rendering pages (with optional cookies) and extracting
form parameters / cookies in a safe, minimal way.
|#

;; ---- Cookie helpers ----
(define (safe-bytes->string v)
  (cond
    [(bytes? v) (bytes->string/utf-8 v)]
    [(string? v) v]
    [else #f]))

(define (get-cookie req name)
  (for/or ([c (request-cookies req)])
    (define n (safe-bytes->string (client-cookie-name c)))
    (and n (string=? n name)
         (let ([v (safe-bytes->string (client-cookie-value c))])
           (and v (not (string=? v "")) v)))))

;; Centralised CSS so multiple render functions can reuse.
(define base-styles
  (string-append
   "/* Minimal site styles */\n"
   "html,body{height:100%;margin:0}\n"
   "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,system-ui;"
   " color:#111;background:linear-gradient(135deg,#f8fafc,#eef2f7);min-height:100vh;display:flex;align-items:center;justify-content:center;padding:24px}\n"
   ".container{max-width:1200px;width:100%;background:white;border-radius:12px;box-shadow:0 10px 30px rgba(20,20,50,0.08);padding:36px}\n"
   "h1{margin:0 0 8px;font-size:28px}\n"
   "h2{margin:0;font-size:18px}\n"
   "p.lead{margin:0 0 20px;color:#4b5563}\n"
   ".nav{display:flex;gap:12px;flex-wrap:wrap}\n"
   ".btn{display:inline-block;padding:10px 16px;border-radius:8px;text-decoration:none;font-weight:600;border:1px solid transparent;cursor:pointer}\n"
   ".btn-primary{background:#6b46ff;color:white}\n"
   ".btn-outline{background:transparent;border:1px solid #e6e8eb;color:#374151}\n"
   ".dashboard-header{display:flex;justify-content:space-between;align-items:center;margin-bottom:30px;padding-bottom:15px;border-bottom:1px solid #e6e8eb}\n"
   ".user-info{display:flex;align-items:center;gap:15px}\n"
   ".logout-btn{padding:8px 16px}\n"
   ".concert-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(280px,1fr));gap:20px;margin-top:20px}\n"
   ".concert-card{background:white;border:2px solid #e6e8eb;border-radius:12px;overflow:hidden;transition:all .3s ease;position:relative}\n"
   ".concert-card:hover{border-color:#6b46ff;box-shadow:0 4px 20px rgba(107,70,255,0.15)}\n"
  ".concert-image{width:100%;height:180px;position:relative;overflow:hidden;background-color:#f1f5f9;background-size:cover;background-position:center}\n"
   ".concert-info{padding:15px}\n"
   ".concert-info h3{margin:0 0 10px;font-size:16px;color:#111}\n"
   ".concert-info p{margin:5px 0;font-size:14px;color:#666}\n"
   ".concert-actions{position:absolute;top:10px;right:10px;display:flex;gap:8px}\n"
  ".action-btn{width:36px;height:36px;display:inline-flex;align-items:center;justify-content:center;border-radius:8px;font-size:16px;border:2px solid;text-decoration:none;background:rgba(255,255,255,0.9);transition:all .2s ease;backdrop-filter:blur(4px);padding:0;line-height:1;box-sizing:border-box}\n"
   ".edit-btn{border-color:#6b46ff;color:#6b46ff}\n"
   ".edit-btn:hover{background:#6b46ff;color:white;transform:scale(1.05)}\n"
   ".delete-btn{border-color:#ef4444;color:#ef4444}\n"
   ".delete-btn:hover{background:#ef4444;color:white;transform:scale(1.05)}\n"
   ".restore-btn{border-color:#16a34a;color:#16a34a}\n"
   ".restore-btn:hover{background:#16a34a;color:white;transform:scale(1.05)}\n"
   ".btn-warning{background:rgba(255,193,7,0.1);border-color:#ffc107;color:#856404}\n"
   ".concert-card.cancelled{opacity:0.85;position:relative}\n"
   ".cancelled-overlay{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);background:rgba(239,68,68,0.9);color:white;padding:8px 16px;border-radius:20px;font-weight:bold;font-size:12px;z-index:10}\n"
   ".cancelled-status{color:#ef4444;font-weight:bold;font-size:12px}\n"
   ".add-new-card{display:flex;flex-direction:column;align-items:center;justify-content:center;min-height:280px;border:2px dashed #cbd5e1;background:rgba(107,70,255,0.02)}\n"
   ".add-new-link{text-decoration:none;display:flex;flex-direction:column;align-items:center;gap:10px;color:#6b46ff}\n"
   ".add-icon{width:50px;height:50px;border:2px solid #6b46ff;border-radius:50%;display:flex;align-items:center;justify-content:center;font-size:24px;font-weight:bold}\n"
   "form p{margin:0 0 12px;display:flex;flex-direction:column;gap:8px}\n"
   "input,select,textarea{padding:10px 12px;border:1px solid #e6e8eb;border-radius:8px;font-size:14px}\n"
   "input[type=submit],button[type=submit]{cursor:pointer;border-radius:8px;padding:10px 14px}\n"
   "label{font-size:13px;color:#374151;font-weight:600}\n"
   ".actions{margin-top:12px;display:flex;gap:8px}\n"
   "@media (max-width:600px){.container{padding:20px}.concert-grid{grid-template-columns:1fr}.dashboard-header{flex-direction:column;gap:15px;align-items:flex-start}}"))

;; ---- Rendering ----
(define (render-page body-xpr)
  (response/xexpr
   `(html
     (head (meta ((charset "utf-8")))
           (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
           (title "Music Portal")
           (style ,base-styles))
     (body (div ((class "container")) ,body-xpr)))))

(define (render-page/cookies body-xpr cookies)
  (response/xexpr
   `(html
     (head (meta ((charset "utf-8")))
           (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
           (title "Music Portal")
           (style ,base-styles))
     (body (div ((class "container")) ,body-xpr)))
   #:cookies cookies))

;; ---- Redirect helper (proper 303 See Other) ----
(define (redirect-303 url #:cookies [cookies '()])
  (response/xexpr
   `(html (head (meta ((charset "utf-8")))
                (meta ((http-equiv "refresh") (content ,(string-append "0;url=" url))))
                (title "Redirecting...")))
   #:code 303
   #:message #"See Other"
   #:headers (list (header #"Location" (string->bytes/utf-8 url))
                   (header #"Connection" #"close"))
   #:cookies cookies))

;; ---- Form param helper ----
(define (get-param req sym)
  (define b (bindings-assq (string->bytes/utf-8 (symbol->string sym))
                           (request-bindings/raw req)))
  (and b (bytes->string/utf-8 (binding:form-value b))))