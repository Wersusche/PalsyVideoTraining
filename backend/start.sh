#!/bin/bash
set -euo pipefail

MAX_RETRIES=${ALEMBIC_MAX_RETRIES:-10}
SLEEP_SECONDS=${ALEMBIC_RETRY_DELAY:-3}

attempt=1
while true; do
    if alembic upgrade head; then
        break
    fi

    if [[ ${attempt} -ge ${MAX_RETRIES} ]]; then
        echo "Alembic migrations failed after ${MAX_RETRIES} attempts" >&2
        exit 1
    fi

    echo "Alembic migration attempt ${attempt} failed, retrying in ${SLEEP_SECONDS}s..." >&2
    attempt=$((attempt + 1))
    sleep "${SLEEP_SECONDS}"
done

echo "Starting Uvicorn..."
exec uvicorn app.main:app --host 0.0.0.0 --port 8000
