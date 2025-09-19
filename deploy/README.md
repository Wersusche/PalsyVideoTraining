# Container-based deployment

Новая схема развёртывания приложения основана на Docker Compose и включает три контейнера: Caddy для статики и реверс-прокси, backend на FastAPI и PostgreSQL в качестве базы данных.

## Архитектура

### Frontend

- Изображение `deploy/caddy/Dockerfile` собирает фронтенд в отдельном Node-слое и копирует артефакты сборки `frontend/dist` в итоговый образ.
- Контейнер `caddy` поднимает порты 80/443 и публикует директорию `/srv` как корень сайта. Через `file_server` фронтенд раздаётся напрямую.
- Обработчик `handle_path /media/*` подключает общий том `media` и отдаёт пользовательские файлы из `/media`.

### Backend

- Контейнер `backend` строится из `python:3.12-slim`, устанавливает зависимости из `backend/pyproject.toml` и использует скрипт `start.sh` в качестве точки входа.
- При запуске `start.sh` повторно пытается применить миграции Alembic до успеха и стартует Uvicorn (`app.main:app`) на порту 8000.

### Сетевое взаимодействие

- Caddy служит реверс-прокси: `handle_path /api/*` перенаправляет трафик на сервис `backend:8000`, где работает FastAPI.
- PostgreSQL запускается в отдельном контейнере `postgres:15`. Backend подключается к базе через строку `postgresql+psycopg://palsy:palsy@db:5432/palsy`, переданную через переменную окружения `PVT_DATABASE_URL`.
- Общий том `media` примонтирован в `backend` по пути `/app/media` и в Caddy по пути `/media`.

## Запуск

```bash
cd deploy
docker compose up --build
```

Команда создаст необходимые образы, выполнит миграции и поднимет все сервисы. После старта:

- Фронтенд доступен по `http://localhost/`.
- Бэкенд доступен по `http://localhost/api` (через Caddy) или по внутреннему адресу `backend:8000` внутри сети Compose.

Для очистки контейнеров и томов выполните:

```bash
docker compose down -v
```
