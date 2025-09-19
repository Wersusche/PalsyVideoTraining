from functools import lru_cache

from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    model_config = SettingsConfigDict(env_file=".env", env_prefix="PVT_", case_sensitive=False)

    database_url: str = "postgresql+psycopg://postgres:postgres@localhost:5432/postgres"


@lru_cache(maxsize=1)
def get_settings() -> Settings:
    return Settings()
