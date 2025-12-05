#lang racket

(provide save-uploaded-image
         save-concert-image-with-id
         concert-image-url
         delete-concert-image!
         get-image-url
         validate-image-type)

(require web-server/http
         file/sha1
         racket/runtime-path)

(define-runtime-path IMAGES-DIR "../static/images")
(define UPLOAD-URL-PREFIX "/static/images/")
;; We enforce PNG only throughout the app.
;; Any non-PNG upload will be rejected by validate-image-type.
;; All saved files use the .png extension and URLs resolve only .png.

(define (concert-filename* concert-id ext)
  (format "concert_~a~a" concert-id ext))

(define (concert-filename concert-id)
  (concert-filename* concert-id ".png"))
(define (concert-fs-path* concert-id ext)
  (build-path IMAGES-DIR (concert-filename* concert-id ext)))

(define (concert-fs-path concert-id)
  (concert-fs-path* concert-id ".png"))
(define ALLOWED-TYPES '("image/png"))

;; Sprawdza czy typ pliku jest dozwolony
(define (validate-image-type content-type)
  (member content-type ALLOWED-TYPES))

;; Zapisuje przesłany plik obrazu i zwraca ścieżkę
(define (save-uploaded-image file-bytes filename content-type)
  (unless (validate-image-type content-type)
    (error "Nieprawidłowy typ pliku. Dozwolone: JPG, PNG"))
  
  ;; Generuj unikalną nazwę pliku na podstawie hash
  (define hash-name (bytes->hex-string (sha1-bytes file-bytes)))
  (define extension (cond
                      [(string-contains? content-type "jpeg") ".jpg"]
                      [(string-contains? content-type "jpg") ".jpg"]
                      [(string-contains? content-type "png") ".png"]
                      [else ".jpg"]))
  (define new-filename (string-append hash-name extension))
  (define full-path (build-path IMAGES-DIR new-filename))
  
  ;; Tworzenie katalogu jeśli nie istnieje
  (make-directory* IMAGES-DIR)
  
  ;; Zapisanie pliku
  (call-with-output-file full-path
    (lambda (out)
      (write-bytes file-bytes out))
    #:exists 'replace)
  
  ;; Zwróć ścieżkę względną
  (string-append UPLOAD-URL-PREFIX new-filename))

;; Save using deterministic name: static/images/concert_<id>.png (PNG only)
(define (save-concert-image-with-id file-bytes content-type concert-id)
  (unless (validate-image-type content-type)
    (error "Nieprawidłowy typ pliku. Wymagany PNG (image/png)"))
  (define ext ".png")
  (define filename (concert-filename* concert-id ext))
  (define full-path (concert-fs-path* concert-id ext))
  (make-directory* IMAGES-DIR)
  (call-with-output-file full-path
    (lambda (out)
      (write-bytes file-bytes out))
    #:exists 'replace)
  (string-append UPLOAD-URL-PREFIX filename))

;; Return URL for card background based on file existence
(define (concert-image-url concert-id)
  (define p-png (concert-fs-path* concert-id ".png"))
  (if (file-exists? p-png)
      (string-append UPLOAD-URL-PREFIX (concert-filename* concert-id ".png"))
      ;; tiny 1x1 transparent PNG as a safe default
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGMAAQAABQABDQottQAAAABJRU5ErkJggg=="))

;; Remove image file for concert if it exists
(define (delete-concert-image! concert-id)
  (define path (concert-fs-path* concert-id ".png"))
  (when (file-exists? path)
    (delete-file path)))

;; Generuje URL do obrazu
(define (get-image-url image-path)
  (if (and image-path (not (string=? image-path "")))
      image-path
      ;; Properly URL-encoded inline SVG placeholder (avoids angle bracket parsing issues)
      "data:image/svg+xml,%3Csvg%20xmlns='http://www.w3.org/2000/svg'%20width='600'%20height='400'%3E%3Cdefs%3E%3ClinearGradient%20id='g'%20x1='0'%20y1='0'%20x2='1'%20y2='1'%3E%3Cstop%20stop-color='%23f1f5f9'%20offset='0'/%3E%3Cstop%20stop-color='%23e2e8f0'%20offset='1'/%3E%3C/linearGradient%3E%3C/defs%3E%3Crect%20width='600'%20height='400'%20fill='url(%23g)'/%3E%3Ctext%20x='50%'%20y='50%'%20dominant-baseline='middle'%20text-anchor='middle'%20font-family='Segoe%20UI,%20Arial'%20font-size='28'%20fill='%236b46ff'%3EConcert%3C/text%3E%3C/svg%3E")) ; default inline placeholder

;; Pomocnicza funkcja do wyciągnięcia danych z multipart form
(define (extract-file-from-binding binding)
  (cond
    [(binding:file? binding)
     (values (binding:file-content binding)
             (binding:file-filename binding)
             (bytes->string/utf-8 (binding:file-headers binding)))]
    [else
     (values #f #f #f)]))