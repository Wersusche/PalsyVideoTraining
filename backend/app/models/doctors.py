from sqlalchemy import Integer, String
from sqlalchemy.orm import Mapped, mapped_column, relationship

from .base import Base


class Doctor(Base):
    __tablename__ = "doctors"

    idDoctors: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    doc_name: Mapped[str | None] = mapped_column(String(45), nullable=True)
    doc_secname: Mapped[str | None] = mapped_column(String(45), nullable=True)
    doc_surname: Mapped[str | None] = mapped_column(String(45), nullable=True)
    doc_rules: Mapped[int | None] = mapped_column(Integer, nullable=True)

    appointments = relationship("Appointment", back_populates="doctor")
