#lang racket

(provide concert?
         concert-id concert-creator-id concert-name 
         concert-max-tickets-to-sell concert-ticket-price 
         concert-location concert-image-path concert-date-time concert-status
         row->concert
         db-create-concert!
         db-find-concerts-by-creator
         db-find-concert-by-id
         db-update-concert!
         db-cancel-concert!
         db-restore-concert!
         db-delete-concert!
         db-get-all-concerts
         db-count-tickets-sold
         db-user-has-ticket?
         db-set-concert-image-path!)

(require "../database/db.rkt"
         db)

(struct concert (id creator-id name max-tickets-to-sell 
                 ticket-price location image-path date-time status) #:transparent)

(define (row->concert row)
  (match row
    [(vector id creator-id name max-tickets ticket-price location image-path date-time status)
     (concert id creator-id name max-tickets ticket-price location image-path date-time status)]))

(define (db-create-concert! creator-id name max-tickets ticket-price location image-path date-time)
  (define db (get-db))
  (query-exec db
    "INSERT INTO concerts (creator_id, name, max_tickets_to_sell, ticket_price, location, image_path, date_time, status)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?);"
    creator-id name max-tickets ticket-price location image-path date-time "active"))

(define (db-find-concerts-by-creator creator-id)
  (define db (get-db))
  (define rows
    (query-rows db
      "SELECT id, creator_id, name, max_tickets_to_sell, ticket_price, location, image_path, date_time, status
       FROM concerts WHERE creator_id = ?;"
      creator-id))
  (map row->concert rows))

(define (db-find-concert-by-id concert-id)
  (define db (get-db))
  (define rows
    (query-rows db
      "SELECT id, creator_id, name, max_tickets_to_sell, ticket_price, location, image_path, date_time, status
       FROM concerts WHERE id = ?;"
      concert-id))
  (if (null? rows)
      #f
      (row->concert (first rows))))

(define (db-update-concert! concert-id name max-tickets ticket-price location image-path date-time status)
  (define db (get-db))
  (query-exec db
    "UPDATE concerts SET name = ?, max_tickets_to_sell = ?, ticket_price = ?, 
     location = ?, image_path = ?, date_time = ?, status = ? WHERE id = ?;"
    name max-tickets ticket-price location image-path date-time status concert-id))

(define (db-cancel-concert! concert-id)
  (define db (get-db))
  (query-exec db
    "UPDATE concerts SET status = 'cancelled' WHERE id = ?;"
    concert-id))

(define (db-restore-concert! concert-id)
  (define db (get-db))
  (query-exec db
    "UPDATE concerts SET status = 'active' WHERE id = ?;"
    concert-id))

(define (db-delete-concert! concert-id)
  (define db (get-db))
  (query-exec db
    "DELETE FROM concerts WHERE id = ?;"
    concert-id))

(define (db-get-all-concerts)
  (define db (get-db))
  (define rows
    (query-rows db
      "SELECT id, creator_id, name, max_tickets_to_sell, ticket_price, location, image_path, date_time, status
       FROM concerts ORDER BY date_time ASC;"))
  (map row->concert rows))

;; ---------- Ticket helpers ----------
(define (db-count-tickets-sold concert-id)
  (define db (get-db))
  (define rows (query-rows db "SELECT COALESCE(SUM(qty),0) FROM tickets WHERE concert_id = ?;" concert-id))
  (if (null? rows) 0 (vector-ref (first rows) 0)))

(define (db-user-has-ticket? concert-id user-id)
  (define db (get-db))
  (define rows (query-rows db "SELECT id FROM tickets WHERE concert_id = ? AND buyer_id = ? LIMIT 1;" concert-id user-id))
  (not (null? rows)))

(define (db-set-concert-image-path! concert-id image-path)
  (define db (get-db))
  (query-exec db "UPDATE concerts SET image_path = ? WHERE id = ?;" image-path concert-id))