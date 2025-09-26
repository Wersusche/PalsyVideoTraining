"""Add storage metadata for videos"""

from __future__ import annotations

from alembic import op
import sqlalchemy as sa


revision = "20241010_add_video_storage_columns"
down_revision = "20241009_add_video_metadata_fields"
branch_labels = None
depends_on = None


TABLE_NAME = "videos"
FILE_PATH_COLUMN = "file_path"
MIME_TYPE_COLUMN = "mime_type"
FILENAME_COLUMN = "filename"


def upgrade() -> None:
    with op.batch_alter_table(TABLE_NAME) as batch_op:
        batch_op.alter_column(
            FILENAME_COLUMN,
            existing_type=sa.String(length=45),
            type_=sa.Text(),
        )
        batch_op.add_column(sa.Column(FILE_PATH_COLUMN, sa.Text(), nullable=True))
        batch_op.add_column(sa.Column(MIME_TYPE_COLUMN, sa.String(length=255), nullable=True))


def downgrade() -> None:
    with op.batch_alter_table(TABLE_NAME) as batch_op:
        batch_op.drop_column(MIME_TYPE_COLUMN)
        batch_op.drop_column(FILE_PATH_COLUMN)
        batch_op.alter_column(
            FILENAME_COLUMN,
            existing_type=sa.Text(),
            type_=sa.String(length=45),
        )
