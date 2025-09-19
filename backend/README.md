# Palsy Video Training Backend

## Prerequisites
- Python 3.10+
- Running PostgreSQL instance accessible via `PVT_DATABASE_URL`
- Optional: [`pg_cron`](https://github.com/citusdata/pg_cron) extension installed on the target database (used to schedule daily maintenance jobs). If it is unavailable, schedule the maintenance SQL statements manually via an external cron job.

## Setup
```bash
cd backend
python -m venv .venv
source .venv/bin/activate
pip install -e .[dev]
cp .env.example .env  # adjust credentials
alembic upgrade head
uvicorn app.main:app --reload
```

## Testing
```bash
pytest
```
