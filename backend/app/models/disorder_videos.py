from sqlalchemy import ForeignKey, Integer
from sqlalchemy.orm import Mapped, mapped_column, relationship

from .base import Base


class DisorderVideo(Base):
    __tablename__ = "disorder_videos"

    disorder_id: Mapped[int] = mapped_column(ForeignKey("disorders.idDisorders", ondelete="CASCADE", onupdate="CASCADE"), primary_key=True)
    videos_id: Mapped[int] = mapped_column(ForeignKey("videos.idvideos"), primary_key=True)

    disorder = relationship("Disorder", back_populates="videos")
    video = relationship("Video", back_populates="disorders")
