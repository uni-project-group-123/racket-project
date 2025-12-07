#lang racket

(provide user?
         user-id user-name user-password user-type
         row->user
         db-create-user!
         db-find-user-by-name
         db-find-user-by-id
         db-update-user-password!)

(require "../database/db.rkt"
         "../utils/crypto-utils.rkt"
         db)

(struct user (id name password type) #:transparent)

(define (row->user row)
  (match row
    [(vector id name password type)
     (user id name password type)]))

(define (db-create-user! name password type)
  (define db (get-db))
  (query-exec db
    "INSERT INTO users (name, password, type)
     VALUES (?, ?, ?);"
    name (hash-password password) type))

(define (db-update-user-password! id new-password)
  (define db (get-db))
  (query-exec db
    "UPDATE users SET password = ? WHERE id = ?;"
    new-password id))

(define (db-find-user-by-name name)
  (define db (get-db))
  (define rows
    (query-rows db
      "SELECT id, name, password, type
       FROM users WHERE name = ?;"
      name))
  (if (null? rows)
      #f
      (row->user (first rows))))

(define (db-find-user-by-id id)
  (define db (get-db))
  (define rows
    (query-rows db
      "SELECT id, name, password, type
       FROM users WHERE id = ?;"
      id))
  (if (null? rows)
      #f
      (row->user (first rows))))