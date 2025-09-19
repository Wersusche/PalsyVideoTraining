from sqlalchemy import Integer, String
from sqlalchemy.orm import Mapped, mapped_column

from .base import Base


class Rule(Base):
    __tablename__ = "rules"

    doc_rules: Mapped[int] = mapped_column(Integer, primary_key=True)
    rule_description: Mapped[str | None] = mapped_column(String(45), nullable=True)
