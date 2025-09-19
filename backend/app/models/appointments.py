from datetime import date, datetime, time

from sqlalchemy import Date, DateTime, Float, ForeignKey, Integer, Text, Time
from sqlalchemy.orm import Mapped, mapped_column, relationship

from .base import Base


class Appointment(Base):
    __tablename__ = "appointments"

    idAppointments: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    Starttime: Mapped[date | None] = mapped_column(Date, nullable=True)
    Endtime: Mapped[date | None] = mapped_column(Date, nullable=True)
    Comments: Mapped[str | None] = mapped_column(Text, nullable=True)
    idPatients: Mapped[int] = mapped_column(ForeignKey("patients.idPatients"), primary_key=True)
    idvideos: Mapped[int] = mapped_column(ForeignKey("videos.idvideos"), nullable=False)
    idDoctors: Mapped[int] = mapped_column(ForeignKey("doctors.idDoctors"), primary_key=True, default=1)
    kolvden: Mapped[int] = mapped_column(Integer, nullable=False, default=1)
    dlitelnost: Mapped[time | None] = mapped_column(Time, nullable=True, default=time(hour=0, minute=0, second=50))
    sdelanovden: Mapped[int] = mapped_column(Integer, nullable=False, default=0)
    sdelanovsego: Mapped[int] = mapped_column(Integer, nullable=False, default=0)
    Lastsession: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    done_percent: Mapped[int] = mapped_column(Integer, nullable=False, default=0)
    CumulativeTimeSpent: Mapped[float | None] = mapped_column(Float, nullable=True, default=0.0)

    patient = relationship("Patient", back_populates="appointments")
    video = relationship("Video", back_populates="appointments")
    doctor = relationship("Doctor", back_populates="appointments")
