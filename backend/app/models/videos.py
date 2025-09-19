from sqlalchemy import Integer, String
from sqlalchemy.orm import Mapped, mapped_column, relationship

from .base import Base


class Video(Base):
    __tablename__ = "videos"

    idvideos: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    video_name: Mapped[str | None] = mapped_column(String(45), nullable=True)
    filename: Mapped[str | None] = mapped_column(String(45), nullable=True)
    ex_type: Mapped[str] = mapped_column(String(50), nullable=False, default="для всех")

    appointments = relationship("Appointment", back_populates="video")
    disorders = relationship("DisorderVideo", back_populates="video", cascade="all, delete-orphan")
