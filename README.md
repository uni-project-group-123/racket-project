# Racket Project — Concert Platform

A Racket-based web application for a concert/fan platform. It includes controllers for routes, models for users and concerts, database helpers, and utilities for crypto, images, security, and web responses.

## Quick Start

- **Prerequisites:** Install Racket (v8.9+ recommended) and ensure `raco` is available.
- **Run the app:**

```pwsh
# From the repo root
racket .\main.rkt
```

If a web server is started, it will log the port or URL in the terminal.

## Project Structure

- `main.rkt`: Entry point. Wires controllers, models, and utils.
- `controllers/`: Route handlers and view logic.
	- `home.rkt`: Landing and general routes.
	- `auth.rkt`: Authentication flows.
	- `concerts.rkt`: Concert listing, creation, and details.
	- `creator-dashboard.rkt`: Creator portal views.
	- `creator-settings.rkt`: Creator account/settings.
	- `fan-dashboard.rkt`: Fan portal views.
- `models/`: Domain models and data schemas.
	- `users.rkt`: User data structures and helpers.
	- `concerts.rkt`: Concert entities and operations.
- `database/`: Persistence and DB utilities.
	- `db.rkt`: Connection and query helpers (file/SQLite/other).
- `utils/`: Cross-cutting utilities.
	- `crypto-utils.rkt`: Hashing, tokens, and signatures.
	- `image-utils.rkt`: Image processing helpers.
	- `security.rkt`: Auth guards, permissions, CSRF helpers.
	- `web-utils.rkt`: HTTP responses, templating, and helpers.
- `static/`: Static assets served by the app.
	- `images/`: Image resources.
		- `default-concert.txt`: Placeholder asset.
- `structure_sketch.txt`: High-level architecture notes.
- `LICENSE`: Project license.

## Configuration

- Environment variables or constants may be defined in `main.rkt` or `utils/security.rkt`.
- Database location/driver is configured in `database/db.rkt`.
- Static files are served from `static/`.

## License

See `LICENSE` for details.