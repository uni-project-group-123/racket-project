#lang racket

(provide get-db init-db)

(require db)

(define db-conn
  (sqlite3-connect
   #:database "portal.db"
   #:mode 'create))

(define (get-db)
  db-conn)

(define (init-db)
  (query-exec db-conn
    "CREATE TABLE IF NOT EXISTS users (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       name TEXT UNIQUE NOT NULL,
       password TEXT NOT NULL,
       type TEXT NOT NULL CHECK(type IN ('fan','creator'))
     )"))