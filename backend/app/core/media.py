"""Utilities for storing uploaded media files."""

from __future__ import annotations

import mimetypes
import secrets
import shutil
from pathlib import Path
from typing import Final

from fastapi import UploadFile

from app.core.config import get_settings

__all__ = [
    "ALLOWED_VIDEO_EXTENSIONS",
    "build_public_url",
    "delete_media_file",
    "generate_unique_filename",
    "import_video_file",
    "resolve_extension",
    "save_video_upload",
]


CHUNK_SIZE: Final[int] = 1024 * 1024
ALLOWED_VIDEO_EXTENSIONS: Final[set[str]] = {
    ".mp4",
    ".mov",
    ".mkv",
    ".webm",
}

_MIME_BY_EXTENSION: Final[dict[str, str]] = {
    ".mp4": "video/mp4",
    ".mov": "video/quicktime",
    ".mkv": "video/x-matroska",
    ".webm": "video/webm",
}
_EXTENSION_BY_MIME: Final[dict[str, str]] = {
    mime: extension for extension, mime in _MIME_BY_EXTENSION.items()
}


def _media_root() -> Path:
    settings = get_settings()
    return Path(settings.media_root)


def _video_directory() -> Path:
    directory = _media_root() / "videos"
    directory.mkdir(parents=True, exist_ok=True)
    return directory


def resolve_extension(filename: str | None, content_type: str | None) -> str:
    """Return a safe, allowed file extension for an uploaded file."""

    if filename:
        ext = Path(filename).suffix.lower()
        if ext in ALLOWED_VIDEO_EXTENSIONS:
            return ext

    if content_type:
        normalized = content_type.split(";", 1)[0].strip().lower()
        if normalized in _EXTENSION_BY_MIME:
            return _EXTENSION_BY_MIME[normalized]
        guessed = mimetypes.guess_extension(normalized)
        if guessed and guessed.lower() in ALLOWED_VIDEO_EXTENSIONS:
            return guessed.lower()

    allowed = ", ".join(sorted(ALLOWED_VIDEO_EXTENSIONS))
    raise ValueError(f"Недопустимое расширение файла. Разрешены: {allowed}.")


def generate_unique_filename(extension: str) -> str:
    """Generate a random filename with the provided extension."""

    safe_extension = extension if extension.startswith(".") else f".{extension}"
    token = secrets.token_urlsafe(16)
    return f"{token}{safe_extension.lower()}"


async def save_video_upload(upload: UploadFile) -> tuple[str, str]:
    """Persist an uploaded video and return its relative path and MIME type."""

    try:
        extension = resolve_extension(upload.filename, upload.content_type)
        filename = generate_unique_filename(extension)
        directory = _video_directory()
        destination = directory / filename

        with destination.open("wb") as buffer:
            while True:
                chunk = await upload.read(CHUNK_SIZE)
                if not chunk:
                    break
                buffer.write(chunk)
    finally:
        await upload.close()

    relative_path = str(Path("videos") / filename)
    content_type = (upload.content_type or "").split(";", 1)[0].strip().lower()
    mime_type = _MIME_BY_EXTENSION.get(extension, None)
    if content_type:
        if content_type in _EXTENSION_BY_MIME:
            mime_type = content_type
        else:
            guess = mimetypes.guess_extension(content_type)
            if guess and guess.lower() == extension:
                mime_type = content_type
    if not mime_type:
        mime_type = _MIME_BY_EXTENSION.get(extension, "application/octet-stream")

    return relative_path, mime_type


def import_video_file(source: Path | str) -> tuple[str, str]:
    """Copy a video file from ``source`` into the media storage."""

    candidate = Path(source)
    if not candidate.is_file():
        raise ValueError("Указанный путь не является файлом.")

    extension = candidate.suffix.lower()
    if extension not in ALLOWED_VIDEO_EXTENSIONS:
        allowed = ", ".join(sorted(ALLOWED_VIDEO_EXTENSIONS))
        raise ValueError(f"Недопустимое расширение файла. Разрешены: {allowed}.")

    destination_dir = _video_directory()
    filename = generate_unique_filename(extension)
    destination = destination_dir / filename
    shutil.copy2(candidate, destination)

    relative_path = str(Path("videos") / filename)
    mime_type = _MIME_BY_EXTENSION.get(extension)
    if not mime_type:
        guessed, _ = mimetypes.guess_type(str(candidate))
        mime_type = guessed or "application/octet-stream"

    return relative_path, mime_type


def delete_media_file(relative_path: str | None) -> None:
    """Remove a media file stored under the configured media root."""

    if not relative_path:
        return
    media_root = _media_root().resolve()
    candidate = (media_root / Path(relative_path)).resolve()
    try:
        candidate.relative_to(media_root)
    except ValueError:
        # Prevent path traversal outside the media root.
        return
    candidate.unlink(missing_ok=True)


def build_public_url(relative_path: str | None) -> str | None:
    """Compose a publicly accessible URL for a stored media file."""

    if not relative_path:
        return None
    settings = get_settings()
    base = settings.media_url.rstrip("/")
    suffix = str(relative_path).lstrip("/")
    if not base:
        return f"/{suffix}"
    return f"{base}/{suffix}"
