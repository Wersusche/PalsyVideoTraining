"""Initial PostgreSQL schema migrated from MySQL dump."""

from __future__ import annotations

from pathlib import Path

from alembic import op

# revision identifiers, used by Alembic.
revision = "20240919_01_initial_schema"
down_revision = None
branch_labels = None
depends_on = None


def upgrade() -> None:
    sql_path = Path(__file__).with_suffix(".sql")
    bind = op.get_bind()
    bind.exec_driver_sql(sql_path.read_text())


def downgrade() -> None:
    raise RuntimeError("Downgrade is not supported for the initial schema import.")
