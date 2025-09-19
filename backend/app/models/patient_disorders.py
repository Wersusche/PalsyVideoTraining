from sqlalchemy import ForeignKey, Integer
from sqlalchemy.orm import Mapped, mapped_column, relationship

from .base import Base


class PatientDisorder(Base):
    __tablename__ = "patient_disorders"

    patient_id: Mapped[int] = mapped_column(ForeignKey("patients.idPatients"), primary_key=True)
    disorder_id: Mapped[int] = mapped_column(ForeignKey("disorders.idDisorders"), primary_key=True)

    patient = relationship("Patient", back_populates="disorders")
    disorder = relationship("Disorder", back_populates="patients")
