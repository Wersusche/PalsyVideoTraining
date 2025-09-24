"""Add login and password columns to doctors"""

from alembic import op
import sqlalchemy as sa

# revision identifiers, used by Alembic.
revision = "20241005_add_doc_credentials"
down_revision = "20240919_01_initial_schema"
branch_labels = None
depends_on = None


TABLE_NAME = "doctors"
LOGIN_COLUMN = "Login"
PASSWORD_COLUMN = "Password"


def upgrade() -> None:
    with op.batch_alter_table(TABLE_NAME) as batch_op:
        batch_op.add_column(sa.Column(LOGIN_COLUMN, sa.String(length=145), nullable=True))
        batch_op.add_column(sa.Column(PASSWORD_COLUMN, sa.String(length=255), nullable=True))

    op.execute(
        sa.text(
            "UPDATE \"doctors\" SET \"Login\" = :login, \"Password\" = :password "
            "WHERE \"Login\" IS NULL AND \"idDoctors\" = 1"
        ).bindparams(login="doctor", password="123")
    )


def downgrade() -> None:
    with op.batch_alter_table(TABLE_NAME) as batch_op:
        batch_op.drop_column(PASSWORD_COLUMN)
        batch_op.drop_column(LOGIN_COLUMN)
