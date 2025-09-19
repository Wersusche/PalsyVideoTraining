from sqlalchemy import Integer
from sqlalchemy.orm import Mapped, mapped_column

from .base import Base


class PushCount(Base):
    __tablename__ = "push_count"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    count: Mapped[int | None] = mapped_column(Integer, nullable=True)
