from functools import lru_cache

from pydantic import PostgresDsn
from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    project_name: str = "myapp"
    database_url: PostgresDsn
    media_root: str = "/media"
    media_url: str = "/media"

    model_config = {
        "env_file": ".env",
        "env_file_encoding": "utf-8",
    }

    @property
    def async_database_url(self) -> str:
        """Return database URL compatible with SQLAlchemy async engines."""

        url = str(self.database_url)

        scheme = url.split("://", 1)[0]

        if "+" in scheme:
            return url

        if url.startswith("postgresql://"):
            return url.replace("postgresql://", "postgresql+asyncpg://", 1)

        if url.startswith("postgres://"):
            return url.replace("postgres://", "postgresql+asyncpg://", 1)

        return url

    @property
    def sync_database_url(self) -> str:
        """Return database URL for sync SQLAlchemy engines (psycopg driver)."""

        url = str(self.database_url)

        scheme = url.split("://", 1)[0]

        if "+" in scheme:
            if scheme.endswith("+psycopg"):
                return url

            if scheme.endswith("+asyncpg"):
                return url.replace("+asyncpg", "+psycopg", 1)

            return url

        if url.startswith("postgresql://"):
            return url.replace("postgresql://", "postgresql+psycopg://", 1)

        if url.startswith("postgres://"):
            return url.replace("postgres://", "postgresql+psycopg://", 1)

        return url


@lru_cache(maxsize=1)
def get_settings() -> Settings:
    return Settings()
