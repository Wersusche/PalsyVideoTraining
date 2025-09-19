from fastapi import Depends, FastAPI
from sqlalchemy import text
from sqlalchemy.orm import Session

from .database import get_db


def create_app() -> FastAPI:
    app = FastAPI(title="Palsy Video Training API", version="0.1.0")

    @app.get("/healthz", tags=["Health"], summary="Service healthcheck")
    def healthcheck(db: Session = Depends(get_db)) -> dict[str, str]:
        db.execute(text("SELECT 1"))
        return {"status": "ok"}

    return app


app = create_app()
