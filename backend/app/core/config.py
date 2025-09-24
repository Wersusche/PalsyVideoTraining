from functools import lru_cache

from pydantic import PostgresDsn
from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    project_name: str = "myapp"
    database_url: PostgresDsn

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


@lru_cache(maxsize=1)
def get_settings() -> Settings:
    return Settings()
