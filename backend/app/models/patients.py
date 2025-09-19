from datetime import date

from sqlalchemy import Date, Integer, String
from sqlalchemy.orm import Mapped, mapped_column, relationship

from .base import Base


class Patient(Base):
    __tablename__ = "patients"

    idPatients: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    Name: Mapped[str] = mapped_column(String(145), nullable=False)
    Secname: Mapped[str | None] = mapped_column(String(145), nullable=True)
    Surname: Mapped[str] = mapped_column(String(145), nullable=False)
    Birthdate: Mapped[date | None] = mapped_column(Date, nullable=True)
    Username: Mapped[str] = mapped_column(String(145), nullable=False)
    Password: Mapped[str] = mapped_column(String(45), nullable=False)

    appointments = relationship("Appointment", back_populates="patient")
    disorders = relationship("PatientDisorder", back_populates="patient", cascade="all, delete-orphan")
