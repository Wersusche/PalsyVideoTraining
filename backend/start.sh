#!/usr/bin/env bash
set -euo pipefail

# Apply database migrations
if command -v alembic >/dev/null 2>&1; then
  alembic upgrade head
fi

# Start the FastAPI application
exec uvicorn app.main:app --host 0.0.0.0 --port 8000
