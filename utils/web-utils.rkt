#lang racket

(provide render-page
         render-page/cookies
         redirect-303
         get-param
         get-cookie
         format-datetime
         svg-heart-outline
         svg-heart-filled
         svg-minus
         svg-sun
         svg-moon)

(require web-server/http
         racket/string
         net/url
         net/uri-codec
         xml)

;; ---- Icons ----
(define svg-sun
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"5\"></circle><line x1=\"12\" y1=\"1\" x2=\"12\" y2=\"3\"></line><line x1=\"12\" y1=\"21\" x2=\"12\" y2=\"23\"></line><line x1=\"4.22\" y1=\"4.22\" x2=\"5.64\" y2=\"5.64\"></line><line x1=\"18.36\" y1=\"18.36\" x2=\"19.78\" y2=\"19.78\"></line><line x1=\"1\" y1=\"12\" x2=\"3\" y2=\"12\"></line><line x1=\"21\" y1=\"12\" x2=\"23\" y2=\"12\"></line><line x1=\"4.22\" y1=\"19.78\" x2=\"5.64\" y2=\"18.36\"></line><line x1=\"18.36\" y1=\"5.64\" x2=\"19.78\" y2=\"4.22\"></line></svg>")

(define svg-moon
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z\"></path></svg>")

(define svg-heart-outline
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"28\" height=\"28\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M20.84 4.61a5.5 5.5 0 0 0-7.78 0L12 5.67l-1.06-1.06a5.5 5.5 0 0 0-7.78 7.78l1.06 1.06L12 21.23l7.78-7.78 1.06-1.06a5.5 5.5 0 0 0 0-7.78z\"></path></svg>")

(define svg-heart-filled
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"28\" height=\"28\" viewBox=\"0 0 24 24\" fill=\"currentColor\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" style=\"color:#ef4444\"><path d=\"M20.84 4.61a5.5 5.5 0 0 0-7.78 0L12 5.67l-1.06-1.06a5.5 5.5 0 0 0-7.78 7.78l1.06 1.06L12 21.23l7.78-7.78 1.06-1.06a5.5 5.5 0 0 0 0-7.78z\"></path></svg>")

(define svg-minus
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"28\" height=\"28\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><line x1=\"5\" y1=\"12\" x2=\"19\" y2=\"12\"></line></svg>")

;; ---- Date helper ----
(define (format-datetime str)
  (if (and (string? str) (non-empty-string? str))
      (string-replace str "T" " ")
      str))

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
   ":root { --bg-color: #f8fafc; --text-color: #111; --card-bg: #fff; --border-color: #e6e8eb; --primary-color: #6b46ff; --secondary-text: #666; --shadow: rgba(20,20,50,0.08); }\n"
   "body.dark-mode { --bg-color: #0f172a; --text-color: #f1f5f9; --card-bg: #1e293b; --border-color: #334155; --primary-color: #818cf8; --secondary-text: #94a3b8; --shadow: rgba(0,0,0,0.3); }\n"
   "html,body{height:100%;margin:0}body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,system-ui;color:var(--text-color);background:var(--bg-color);min-height:100vh;padding:24px;box-sizing:border-box;transition:background .3s, color .3s}\n"
   ".container{max-width:1200px;margin:0 auto;width:100%;background:var(--card-bg);border-radius:12px;box-shadow:0 10px 30px var(--shadow);padding:36px;border:1px solid var(--border-color)}\n"
   ".topbar{display:flex;align-items:center;justify-content:space-between;gap:16px;margin-bottom:24px;padding-bottom:12px;border-bottom:1px solid var(--border-color);flex-wrap:wrap}.brand-link{font-weight:800;font-size:20px;color:var(--text-color);text-decoration:none}.user-menu{position:relative;display:inline-flex;align-items:center;gap:10px;font-weight:600;color:var(--text-color);cursor:pointer}.user-name{padding:8px 12px;border-radius:8px;background:var(--bg-color);user-select:none}.dropdown{position:absolute;top:100%;right:0;margin-top:6px;background:var(--card-bg);border:1px solid var(--border-color);border-radius:10px;box-shadow:0 10px 20px var(--shadow);display:none;min-width:190px;overflow:hidden;z-index:200}.dropdown a{display:block;padding:12px 14px;color:var(--text-color);text-decoration:none;font-weight:500;font-size:14px}.dropdown a:hover{background:var(--bg-color)}.user-menu.open .dropdown{display:block}\n"
   "h1{margin:0 0 8px;font-size:28px}h2{margin:0;font-size:18px}p.lead{margin:0 0 20px;color:var(--secondary-text)}\n"
   ".nav{display:flex;gap:12px;flex-wrap:wrap}.btn{display:inline-block;padding:10px 16px;border-radius:8px;text-decoration:none;font-weight:600;border:1px solid transparent;cursor:pointer}.btn-primary{background:var(--primary-color);color:white}.btn-outline{background:transparent;border:1px solid var(--border-color);color:var(--text-color)}\n"
   ".card-link{text-decoration:none;color:inherit;display:block}\n"
   ".dashboard-header{display:flex;justify-content:space-between;align-items:center;margin-bottom:30px;padding-bottom:15px;border-bottom:1px solid var(--border-color);flex-wrap:wrap}.user-info{display:flex;align-items:center;gap:15px}.logout-btn{padding:8px 16px}\n"
   ".concert-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(260px,1fr));gap:20px;margin-top:40px}.concert-card{background:var(--card-bg);border:2px solid var(--border-color);border-radius:12px;overflow:hidden;transition:all .3s ease;position:relative;display:flex;flex-direction:column;height:100%}.concert-card:hover{border-color:var(--primary-color);box-shadow:0 4px 20px var(--shadow)}.concert-image{width:100%;height:180px;position:relative;overflow:hidden;background:var(--bg-color);background-size:cover;background-position:center}.concert-img{width:100%;height:100%;object-fit:cover;display:block}\n"
   ".concert-info{padding:15px;flex:1;display:flex;flex-direction:column}.concert-info h3{margin:0 0 10px;font-size:16px;color:var(--text-color)}.concert-info p{margin:5px 0;font-size:14px;color:var(--secondary-text)}\n"
   ".card-link{text-decoration:none;color:inherit;display:block;height:100%}\n"
   ".concert-actions{position:absolute;top:10px;right:10px;display:flex;gap:8px}.action-btn{width:36px;height:36px;display:inline-flex;align-items:center;justify-content:center;border-radius:8px;font-size:16px;border:2px solid;text-decoration:none;background:rgba(255,255,255,0.9);transition:all .2s ease;backdrop-filter:blur(4px);padding:0;line-height:1;box-sizing:border-box}.edit-btn{border-color:var(--primary-color);color:var(--primary-color)}.edit-btn:hover{background:var(--primary-color);color:white;transform:scale(1.05)}.delete-btn{border-color:#ef4444;color:#ef4444}.delete-btn:hover{background:#ef4444;color:white;transform:scale(1.05)}.restore-btn{border-color:#16a34a;color:#16a34a}.restore-btn:hover{background:#16a34a;color:white;transform:scale(1.05)}.btn-warning{background:rgba(255,193,7,0.1);border-color:#ffc107;color:#856404}\n"
   ".concert-card.cancelled{opacity:.85;position:relative}.cancelled-overlay{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);background:rgba(239,68,68,0.9);color:white;padding:8px 16px;border-radius:20px;font-weight:bold;font-size:12px;z-index:10}.cancelled-status{color:#ef4444;font-weight:bold;font-size:12px}.add-new-card{display:flex;flex-direction:column;min-height:280px;border:2px dashed var(--border-color);background:rgba(107,70,255,0.02)}.add-new-link{text-decoration:none;display:flex;flex-direction:column;align-items:center;justify-content:center;gap:10px;color:var(--primary-color);flex:1;width:100%}.add-icon{width:50px;height:50px;border:2px solid var(--primary-color);border-radius:50%;display:flex;align-items:center;justify-content:center;font-size:24px;font-weight:bold}\n"
   ".concert-detail{display:flex;gap:40px;margin-top:20px;align-items:flex-start}.concert-detail .media{flex:0 0 400px;max-width:100%}.concert-detail .concert-img{width:100%;border-radius:12px;box-shadow:0 4px 12px var(--shadow);display:block}.concert-detail .info{flex:1}.concert-detail h1{font-size:32px;margin-bottom:10px;line-height:1.2}.concert-detail .location{font-size:18px;color:var(--secondary-text);margin-bottom:20px;display:flex;align-items:center;gap:8px}.concert-detail .date{font-size:16px;color:var(--text-color);margin-bottom:20px;font-weight:500;display:flex;align-items:center;gap:8px}.concert-detail .price{font-size:28px;font-weight:800;color:var(--primary-color);margin-bottom:30px}.back-btn{display:inline-flex;align-items:center;gap:6px;margin-bottom:20px;color:var(--secondary-text);text-decoration:none;font-weight:600;font-size:14px;transition:color .2s}.back-btn:hover{color:var(--text-color)}.sold-out-badge{display:inline-block;background:#fee2e2;color:#991b1b;padding:8px 16px;border-radius:8px;font-size:14px;font-weight:800;text-transform:uppercase;letter-spacing:1px;margin-top:10px;border:1px solid #fecaca;width:100%;text-align:center;box-sizing:border-box}.called-off-badge{display:inline-block;background:var(--bg-color);color:var(--secondary-text);padding:8px 16px;border-radius:8px;font-size:14px;font-weight:800;text-transform:uppercase;letter-spacing:1px;margin-top:10px;border:1px solid var(--border-color);width:100%;text-align:center;box-sizing:border-box}\n"
   ".browse-header{display:flex;justify-content:space-between;align-items:center;margin-bottom:20px;flex-wrap:wrap;gap:20px}.browse-title h1{margin-bottom:4px}.browse-title p{margin:0}.filter-form{display:flex;gap:12px;align-items:flex-end;flex-wrap:wrap}.filter-group{display:flex;flex-direction:column;gap:4px}.filter-group label{font-size:12px;font-weight:600;color:var(--secondary-text)}.filter-group select,.filter-group input{padding:8px 12px;border:1px solid var(--border-color);border-radius:6px;font-size:14px;min-width:140px;background:var(--card-bg);color:var(--text-color)}\n"
   ".heart-btn{position:absolute;top:10px;right:10px;width:44px;height:44px;border-radius:50%;background:rgba(255,255,255,0.9);border:none;display:flex;align-items:center;justify-content:center;cursor:pointer;font-size:20px;color:#ccc;transition:all .2s;box-shadow:0 2px 8px rgba(0,0,0,0.1);z-index:10}.heart-btn:hover{transform:scale(1.1);color:#ef4444}.heart-btn.active{color:#ef4444}\n"
   ".concert-card > .card-link {display:flex;flex-direction:column;flex:1;height:100%;width:100%}\n"
   "form p{margin:0 0 12px;display:flex;flex-direction:column;gap:8px}input,select,textarea{padding:10px 12px;border:1px solid var(--border-color);border-radius:8px;font-size:14px;background:var(--card-bg);color:var(--text-color)}input[type=submit],button[type=submit]{cursor:pointer;border-radius:8px;padding:10px 14px}label{font-size:13px;color:var(--text-color);font-weight:600}.actions{margin-top:12px;display:flex;gap:8px}\n"
   ".theme-toggle{position:fixed;bottom:20px;right:20px;width:50px;height:50px;border-radius:50%;background:var(--card-bg);border:2px solid var(--border-color);color:var(--text-color);display:flex;align-items:center;justify-content:center;cursor:pointer;box-shadow:0 4px 12px var(--shadow);z-index:1000;transition:all .2s}\n"
   ".theme-toggle:hover{transform:scale(1.1);border-color:var(--primary-color)}\n"
   "@media (max-width:900px){.container{padding:28px}.topbar{flex-direction:column;align-items:flex-start}.concert-image{height:160px}.concert-detail{flex-direction:column;gap:24px}.concert-detail .media{flex:1;width:100%}}@media (max-width:600px){.container{padding:20px}.concert-grid{grid-template-columns:1fr}.dashboard-header{flex-direction:column;gap:15px;align-items:flex-start}.concert-image{height:150px}.user-name{padding:6px 10px}}"))

;; ---- Rendering ----
(define dropdown-script "document.addEventListener('DOMContentLoaded',function(){var menus=document.querySelectorAll('.user-menu');function closeAll(){menus.forEach(function(m){m.classList.remove('open');});}menus.forEach(function(m){var toggle=m.querySelector('.user-name');var dd=m.querySelector('.dropdown');if(!toggle||!dd)return;toggle.addEventListener('click',function(e){e.stopPropagation();var already=m.classList.contains('open');closeAll();if(!already){m.classList.add('open');}});dd.addEventListener('click',function(){closeAll();});});document.addEventListener('click',function(){closeAll();});});")

(define (render-page body-xpr #:request [req #f] #:cookies [extra-cookies '()])
  (define theme (if req (or (get-cookie req "theme") "light") "light"))
  (define is-dark (string=? theme "dark"))
  (define theme-icon (if is-dark svg-sun svg-moon))
  
  (response/xexpr
   `(html
     (head (meta ((charset "utf-8")))
           (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
           (title "Music Portal")
           (style ,base-styles)
           (script ,dropdown-script))
     (body ((class ,(if is-dark "dark-mode" "")))
           (div ((class "container")) ,body-xpr)
           (a ((href ,(string-append "/toggle-theme?redirect=" 
                                     (uri-encode (if req (url->string (request-uri req)) "/"))))
               (class "theme-toggle")
               (title "Toggle Theme"))
              ,(string->xexpr theme-icon))))
   #:cookies extra-cookies))

(define (render-page/cookies body-xpr cookies)
  (render-page body-xpr #:cookies cookies))

;; ---- Redirect helper (proper 303 See Other) ----
(define (redirect-303 url #:cookies [cookies '()])
  (define cookie-headers (map cookie->header cookies))
  (response/full
   303
   #"See Other"
   (current-seconds)
   #"text/html; charset=utf-8"
   (append (list (header #"Location" (string->bytes/utf-8 url)))
           cookie-headers)
   (list)
   ))

;; ---- Form param helper ----
(define (get-param req sym)
  (define b (bindings-assq (string->bytes/utf-8 (symbol->string sym))
                           (request-bindings/raw req)))
  (and b (bytes->string/utf-8 (binding:form-value b))))