# Palsy Video Training Backend

## Prerequisites
- Python 3.10+
- Running MySQL instance accessible via `PVT_DATABASE_URL`

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
