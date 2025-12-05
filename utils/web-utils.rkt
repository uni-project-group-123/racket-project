#lang racket

(provide render-page
         render-page/cookies
         redirect-303
         get-param
         get-cookie)

(require web-server/http)

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

;; ---- Styles ----
(define base-styles
  (string-append
   "/* Unified responsive styles */\n"
   "html,body{height:100%;margin:0}body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,system-ui;color:#111;background:linear-gradient(135deg,#f8fafc,#eef2f7);min-height:100vh;padding:24px;box-sizing:border-box;}\n"
   ".container{max-width:1200px;margin:0 auto;width:100%;background:white;border-radius:12px;box-shadow:0 10px 30px rgba(20,20,50,0.08);padding:36px}\n"
  ".topbar{display:flex;align-items:center;justify-content:space-between;gap:16px;margin-bottom:24px;padding-bottom:12px;border-bottom:1px solid #e6e8eb;flex-wrap:wrap}.brand-link{font-weight:800;font-size:20px;color:#111;text-decoration:none}.user-menu{position:relative;display:inline-flex;align-items:center;gap:10px;font-weight:600;color:#374151;cursor:pointer}.user-name{padding:8px 12px;border-radius:8px;background:#f3f4f6;user-select:none}.dropdown{position:absolute;top:100%;right:0;margin-top:6px;background:white;border:1px solid #e6e8eb;border-radius:10px;box-shadow:0 10px 20px rgba(20,20,50,0.08);display:none;min-width:190px;overflow:hidden;z-index:200}.dropdown a{display:block;padding:12px 14px;color:#111;text-decoration:none;font-weight:500;font-size:14px}.dropdown a:hover{background:#f8fafc}.user-menu.open .dropdown{display:block}\n"
   "h1{margin:0 0 8px;font-size:28px}h2{margin:0;font-size:18px}p.lead{margin:0 0 20px;color:#4b5563}\n"
   ".nav{display:flex;gap:12px;flex-wrap:wrap}.btn{display:inline-block;padding:10px 16px;border-radius:8px;text-decoration:none;font-weight:600;border:1px solid transparent;cursor:pointer}.btn-primary{background:#6b46ff;color:white}.btn-outline{background:transparent;border:1px solid #e6e8eb;color:#374151}\n"
  ".card-link{text-decoration:none;color:inherit;display:block}\n"
   ".dashboard-header{display:flex;justify-content:space-between;align-items:center;margin-bottom:30px;padding-bottom:15px;border-bottom:1px solid #e6e8eb;flex-wrap:wrap}.user-info{display:flex;align-items:center;gap:15px}.logout-btn{padding:8px 16px}\n"
   ".concert-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(260px,1fr));gap:20px;margin-top:20px}.concert-card{background:white;border:2px solid #e6e8eb;border-radius:12px;overflow:hidden;transition:all .3s ease;position:relative;display:flex;flex-direction:column}.concert-card:hover{border-color:#6b46ff;box-shadow:0 4px 20px rgba(107,70,255,0.15)}.concert-image{width:100%;height:180px;position:relative;overflow:hidden;background:#f1f5f9;background-size:cover;background-position:center}.concert-img{width:100%;height:100%;object-fit:cover;display:block}\n"
   ".concert-info{padding:15px;flex:1;display:flex;flex-direction:column}.concert-info h3{margin:0 0 10px;font-size:16px;color:#111}.concert-info p{margin:5px 0;font-size:14px;color:#666}\n"
   ".concert-actions{position:absolute;top:10px;right:10px;display:flex;gap:8px}.action-btn{width:36px;height:36px;display:inline-flex;align-items:center;justify-content:center;border-radius:8px;font-size:16px;border:2px solid;text-decoration:none;background:rgba(255,255,255,0.9);transition:all .2s ease;backdrop-filter:blur(4px);padding:0;line-height:1;box-sizing:border-box}.edit-btn{border-color:#6b46ff;color:#6b46ff}.edit-btn:hover{background:#6b46ff;color:white;transform:scale(1.05)}.delete-btn{border-color:#ef4444;color:#ef4444}.delete-btn:hover{background:#ef4444;color:white;transform:scale(1.05)}.restore-btn{border-color:#16a34a;color:#16a34a}.restore-btn:hover{background:#16a34a;color:white;transform:scale(1.05)}.btn-warning{background:rgba(255,193,7,0.1);border-color:#ffc107;color:#856404}\n"
   ".concert-card.cancelled{opacity:.85;position:relative}.cancelled-overlay{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);background:rgba(239,68,68,0.9);color:white;padding:8px 16px;border-radius:20px;font-weight:bold;font-size:12px;z-index:10}.cancelled-status{color:#ef4444;font-weight:bold;font-size:12px}.add-new-card{display:flex;flex-direction:column;min-height:280px;border:2px dashed #cbd5e1;background:rgba(107,70,255,0.02)}.add-new-link{text-decoration:none;display:flex;flex-direction:column;align-items:center;justify-content:center;gap:10px;color:#6b46ff;flex:1;width:100%}.add-icon{width:50px;height:50px;border:2px solid #6b46ff;border-radius:50%;display:flex;align-items:center;justify-content:center;font-size:24px;font-weight:bold}\n"
   "form p{margin:0 0 12px;display:flex;flex-direction:column;gap:8px}input,select,textarea{padding:10px 12px;border:1px solid #e6e8eb;border-radius:8px;font-size:14px}input[type=submit],button[type=submit]{cursor:pointer;border-radius:8px;padding:10px 14px}label{font-size:13px;color:#374151;font-weight:600}.actions{margin-top:12px;display:flex;gap:8px}\n"
   "@media (max-width:900px){.container{padding:28px}.topbar{flex-direction:column;align-items:flex-start}.concert-image{height:160px}}@media (max-width:600px){.container{padding:20px}.concert-grid{grid-template-columns:1fr}.dashboard-header{flex-direction:column;gap:15px;align-items:flex-start}.concert-image{height:150px}.user-name{padding:6px 10px}}"))

;; ---- Rendering ----
(define dropdown-script "document.addEventListener('DOMContentLoaded',function(){var menus=document.querySelectorAll('.user-menu');function closeAll(){menus.forEach(function(m){m.classList.remove('open');});}menus.forEach(function(m){var toggle=m.querySelector('.user-name');var dd=m.querySelector('.dropdown');if(!toggle||!dd)return;toggle.addEventListener('click',function(e){e.stopPropagation();var already=m.classList.contains('open');closeAll();if(!already){m.classList.add('open');}});dd.addEventListener('click',function(){closeAll();});});document.addEventListener('click',function(){closeAll();});});")

(define (render-page body-xpr)
  (response/xexpr
   `(html
     (head (meta ((charset "utf-8")))
           (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
           (title "Music Portal")
           (style ,base-styles)
           (script ,dropdown-script))
     (body (div ((class "container")) ,body-xpr)))) )

(define (render-page/cookies body-xpr cookies)
  (response/xexpr
   `(html
     (head (meta ((charset "utf-8")))
           (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
           (title "Music Portal")
           (style ,base-styles)
           (script ,dropdown-script))
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