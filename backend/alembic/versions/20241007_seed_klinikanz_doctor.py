"""Ensure default doctor account and hashed passwords."""

from __future__ import annotations

import bcrypt
from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = "20241007_seed_klinikanz_doctor"
down_revision = "20241005_add_doc_credentials"
branch_labels = None
depends_on = None


DEFAULT_DOCTOR_LOGIN = "klinikanz"
DEFAULT_DOCTOR_PASSWORD = "welove23041987"


def _hash_password(password: str) -> str:
    return bcrypt.hashpw(password.encode("utf-8"), bcrypt.gensalt()).decode("utf-8")


def _is_bcrypt_hash(value: str) -> bool:
    return value.startswith("$2a$") or value.startswith("$2b$") or value.startswith("$2y$")


def upgrade() -> None:
    bind = op.get_bind()

    doctors = bind.execute(
        sa.text('SELECT "idDoctors", "Password" FROM "doctors" WHERE "Password" IS NOT NULL')
    ).mappings()

    for doctor in doctors:
        stored_password = doctor.get("Password")
        if stored_password is None:
            continue

        if isinstance(stored_password, memoryview):
            stored_password = stored_password.tobytes().decode("utf-8", "ignore")
        else:
            stored_password = str(stored_password)

        if not stored_password:
            continue

        if _is_bcrypt_hash(stored_password):
            continue

        hashed_password = _hash_password(stored_password)
        bind.execute(
            sa.text('UPDATE "doctors" SET "Password" = :password WHERE "idDoctors" = :doctor_id'),
            {"password": hashed_password, "doctor_id": doctor["idDoctors"]},
        )

    existing_default = bind.execute(
        sa.text('SELECT 1 FROM "doctors" WHERE "Login" = :login'),
        {"login": DEFAULT_DOCTOR_LOGIN},
    ).scalar()

    if existing_default is None:
        hashed_default_password = _hash_password(DEFAULT_DOCTOR_PASSWORD)
        bind.execute(
            sa.text(
                'INSERT INTO "doctors" ("doc_name", "Login", "Password") '
                'VALUES (:name, :login, :password)'
            ),
            {
                "name": "Klinikanz",
                "login": DEFAULT_DOCTOR_LOGIN,
                "password": hashed_default_password,
            },
        )


def downgrade() -> None:
    bind = op.get_bind()
    bind.execute(
        sa.text('DELETE FROM "doctors" WHERE "Login" = :login'),
        {"login": DEFAULT_DOCTOR_LOGIN},
    )
