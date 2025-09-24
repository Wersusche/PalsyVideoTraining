#!/usr/bin/env bash
set -euo pipefail

# Wait for the database to be ready before running migrations
if [ -n "${DATABASE_URL:-}" ]; then
  python <<'PYTHON'
import os
import time

import psycopg

database_url = os.environ["DATABASE_URL"]
max_attempts = 30

for attempt in range(1, max_attempts + 1):
    try:
        with psycopg.connect(database_url, connect_timeout=5):
            break
    except Exception as exc:  # pragma: no cover - diagnostic output only
        print(f"Waiting for database (attempt {attempt}/{max_attempts}): {exc}")
        time.sleep(1)
else:
    raise SystemExit("Database is not ready")
PYTHON
fi

# Apply database migrations
if command -v alembic >/dev/null 2>&1; then
  alembic upgrade head
fi

# Start the FastAPI application
exec uvicorn app.main:app --host 0.0.0.0 --port 8000
