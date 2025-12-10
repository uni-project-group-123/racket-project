#lang racket

(provide creator-settings
         handle-update-username
         handle-update-password)

(require "../utils/web-utils.rkt"
         "../models/users.rkt"
         "../utils/crypto-utils.rkt"
         web-server/http)

(define (creator-settings req)
  (define raw-id (get-cookie req "uid"))
  (define creator-id (string->number raw-id))
  (define current-user (db-find-user-by-id creator-id))
  
  (render-page
   #:request req
   `(div
     ;; Back button
     (a ((href "/creator-dashboard") (class "back-btn")) "← Back to Dashboard")
     
     (h1 "Account Settings")
     (p ((class "lead")) ,(format "Manage settings for ~a" (user-name current-user)))
     
     ;; Change Username Section
     (div ((style "margin-top: 30px; padding: 20px; border: 1px solid var(--border-color); border-radius: 8px;"))
          (h2 "Change Username")
          (form ((action "/update-username") (method "post"))
                (p
                 (label "New Username:")
                 (input ((name "new-username") (type "text") (required "required") (value ,(user-name current-user)))))
                (div ((class "actions"))
                     (button ((type "submit") (class "btn btn-primary")) "Update Username"))))
     
     ;; Change Password Section
     (div ((style "margin-top: 30px; padding: 20px; border: 1px solid var(--border-color); border-radius: 8px;"))
          (h2 "Change Password")
          (form ((action "/update-password") (method "post"))
                (p
                 (label "Current Password:")
                 (input ((name "current-password") (type "password") (required "required"))))
                (p
                 (label "New Password:")
                 (input ((name "new-password") (type "password") (required "required"))))
                (p
                 (label "Confirm New Password:")
                 (input ((name "confirm-password") (type "password") (required "required"))))
                (div ((class "actions"))
                     (button ((type "submit") (class "btn btn-primary")) "Update Password")))))))

(define (handle-update-username req)
  (define raw-id (get-cookie req "uid"))
  (define user-id (string->number raw-id))
  (define current-user (db-find-user-by-id user-id))
  (define new-username (get-param req 'new-username))
  
  (cond
    [(or (not new-username) (string=? new-username ""))
     (render-page
      #:request req
      `(div
        (h1 "Error")
        (p "Username cannot be empty.")
        (div ((class "actions"))
             (a ((href "/creator-settings") (class "btn btn-outline")) "Back to Settings"))))]
    
    [(string=? new-username (user-name current-user))
     (redirect-303 "/creator-settings")]
    
    [else
     (with-handlers ([exn:fail?
                      (λ (e)
                        (render-page
                         #:request req
                         `(div
                           (h1 "Error")
                           (p "Username already taken. Please choose another.")
                           (div ((class "actions"))
                                (a ((href "/creator-settings") (class "btn btn-outline")) "Back to Settings")))))])
       (db-update-username! user-id new-username)
       (render-page
        #:request req
        `(div
          (h1 "Success!")
          (p ,(format "Your username has been updated to \"~a\"." new-username))
          (div ((class "actions"))
               (a ((href "/creator-settings") (class "btn btn-primary")) "Back to Settings")))))]))

(define (handle-update-password req)
  (define raw-id (get-cookie req "uid"))
  (define user-id (string->number raw-id))
  (define current-user (db-find-user-by-id user-id))
  
  (define current-password (get-param req 'current-password))
  (define new-password (get-param req 'new-password))
  (define confirm-password (get-param req 'confirm-password))
  
  (cond
    [(or (not current-password) (not new-password) (not confirm-password)
         (string=? current-password "") (string=? new-password "") (string=? confirm-password ""))
     (render-page
      #:request req
      `(div
        (h1 "Error")
        (p "All password fields are required.")
        (div ((class "actions"))
             (a ((href "/creator-settings") (class "btn btn-outline")) "Back to Settings"))))]
    
    [(not (string=? (hash-password current-password) (user-password current-user)))
     (render-page
      #:request req
      `(div
        (h1 "Error")
        (p "Current password is incorrect.")
        (div ((class "actions"))
             (a ((href "/creator-settings") (class "btn btn-outline")) "Back to Settings"))))]
    
    [(not (string=? new-password confirm-password))
     (render-page
      #:request req
      `(div
        (h1 "Error")
        (p "New passwords do not match.")
        (div ((class "actions"))
             (a ((href "/creator-settings") (class "btn btn-outline")) "Back to Settings"))))]
    
    [(string=? new-password current-password)
     (render-page
      #:request req
      `(div
        (h1 "Error")
        (p "New password must be different from current password.")
        (div ((class "actions"))
             (a ((href "/creator-settings") (class "btn btn-outline")) "Back to Settings"))))]
    
    [else
     (db-update-user-password! user-id (hash-password new-password))
     (render-page
      #:request req
      `(div
        (h1 "Success!")
        (p "Your password has been updated successfully.")
        (div ((class "actions"))
             (a ((href "/creator-settings") (class "btn btn-primary")) "Back to Settings"))))]))