"""Add body_part and type_of_activity columns to videos"""

from __future__ import annotations

from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = "20241009_add_video_metadata_fields"
down_revision = "20241007_seed_klinikanz_doctor"
branch_labels = None
depends_on = None


TABLE_NAME = "videos"
BODY_PART_COLUMN = "body_part"
TYPE_OF_ACTIVITY_COLUMN = "type_of_activity"


def upgrade() -> None:
    with op.batch_alter_table(TABLE_NAME) as batch_op:
        batch_op.add_column(sa.Column(BODY_PART_COLUMN, sa.Text(), nullable=True))
        batch_op.add_column(sa.Column(TYPE_OF_ACTIVITY_COLUMN, sa.Text(), nullable=True))


def downgrade() -> None:
    with op.batch_alter_table(TABLE_NAME) as batch_op:
        batch_op.drop_column(TYPE_OF_ACTIVITY_COLUMN)
        batch_op.drop_column(BODY_PART_COLUMN)
