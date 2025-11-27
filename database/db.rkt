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
     )")
  (query-exec db-conn
    "CREATE TABLE IF NOT EXISTS concerts (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       creator_id INTEGER NOT NULL,
       name TEXT NOT NULL,
       max_tickets_to_sell INTEGER NOT NULL,
       ticket_price REAL NOT NULL,
       location TEXT NOT NULL,
       image_path TEXT,
       date_time TEXT,
       status TEXT NOT NULL DEFAULT 'active' CHECK(status IN ('active','cancelled')),
       FOREIGN KEY(creator_id) REFERENCES users(id)
     )")

  ;; Ticket purchases: who bought how many for which concert
  (query-exec db-conn
    "CREATE TABLE IF NOT EXISTS tickets (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       concert_id INTEGER NOT NULL,
       buyer_id INTEGER NOT NULL,
       qty INTEGER NOT NULL CHECK(qty > 0),
       purchased_at TEXT NOT NULL,
       FOREIGN KEY(concert_id) REFERENCES concerts(id),
       FOREIGN KEY(buyer_id) REFERENCES users(id)
     )")
  
  (with-handlers ([exn:fail? (λ (e) #f)])
    (query-exec db-conn
      "ALTER TABLE concerts ADD COLUMN status TEXT NOT NULL DEFAULT 'active' CHECK(status IN ('active','cancelled'));"))

  ;; Seed creator if missing
  
  (with-handlers ([exn:fail? (λ (e) #f)])
    (query-exec db-conn
      "INSERT INTO users (id, name, password, type) VALUES (1, 'TestCreator', 'password123', 'creator');")))