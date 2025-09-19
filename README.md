# PalsyVideoTraining

Приложение для тренировки детей с ДЦП. Репозиторий теперь содержит:

- существующий код на Delphi (не изменён);
- **backend/** — окружение FastAPI + SQLAlchemy/Alembic для работы с базой данных `palsy_db`;
- **frontend/** — заготовка интерфейса на React + Vite + Tailwind/shadcn/ui;
- GitHub Actions workflow для проверки Python/Node проектов.

## Быстрый старт

### Backend
1. `cd backend`
2. `python -m venv .venv && source .venv/bin/activate`
3. `pip install -e .[dev]`
4. Скопируйте `.env.example` в `.env` и укажите доступ к PostgreSQL.
5. Примените миграции: `alembic upgrade head`.
6. Запустите API: `uvicorn app.main:app --reload`

### Frontend
1. `cd frontend`
2. `npm install`
3. `npm run dev`

## CI/CD
Работает GitHub Actions workflow `.github/workflows/ci.yml`, который проверяет backend (pytest) и frontend (npm build). Это позволит добавить шаги деплоя после настройки инфраструктуры.
