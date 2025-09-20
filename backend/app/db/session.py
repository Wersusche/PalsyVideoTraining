from collections.abc import AsyncGenerator

from fastapi import HTTPException
from pydantic import ValidationError
from sqlalchemy.ext.asyncio import AsyncEngine, AsyncSession, async_sessionmaker, create_async_engine

from app.core.config import get_settings

_engine: AsyncEngine | None = None
_session_factory: async_sessionmaker[AsyncSession] | None = None


def _get_session_factory() -> async_sessionmaker[AsyncSession]:
    global _engine, _session_factory

    if _session_factory is None:
        try:
            settings = get_settings()
        except ValidationError as exc:  # pragma: no cover - defensive configuration guard
            raise RuntimeError("DATABASE_URL is not configured") from exc

        if settings.database_url is None:
            raise RuntimeError("DATABASE_URL is not configured")

        _engine = create_async_engine(str(settings.database_url), echo=False, future=True)
        _session_factory = async_sessionmaker(bind=_engine, expire_on_commit=False, class_=AsyncSession)

    assert _session_factory is not None  # for type checkers
    return _session_factory


async def get_session() -> AsyncGenerator[AsyncSession, None]:
    try:
        session_factory = _get_session_factory()
    except RuntimeError as exc:
        raise HTTPException(status_code=503, detail=str(exc)) from exc

    async with session_factory() as session:
        yield session
