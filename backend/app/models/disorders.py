from sqlalchemy import Integer, String
from sqlalchemy.orm import Mapped, mapped_column, relationship

from .base import Base


class Disorder(Base):
    __tablename__ = "disorders"

    idDisorders: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    Disorder_name: Mapped[str | None] = mapped_column(String(45), nullable=True)
    Disorder_type: Mapped[str | None] = mapped_column(String(45), nullable=True)

    videos = relationship("DisorderVideo", back_populates="disorder", cascade="all, delete-orphan")
    patients = relationship("PatientDisorder", back_populates="disorder", cascade="all, delete-orphan")
