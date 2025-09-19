"""Initial database schema

Revision ID: 20240919_000001
Revises: 
Create Date: 2024-04-01 00:00:00.000000
"""

from __future__ import annotations

from alembic import op
import sqlalchemy as sa


revision = "20240919_000001"
down_revision = None
branch_labels = None
depends_on = None


def upgrade() -> None:
    op.create_table(
        "doctors",
        sa.Column("idDoctors", sa.Integer(), primary_key=True, autoincrement=True),
        sa.Column("doc_name", sa.String(length=45), nullable=True),
        sa.Column("doc_secname", sa.String(length=45), nullable=True),
        sa.Column("doc_surname", sa.String(length=45), nullable=True),
        sa.Column("doc_rules", sa.Integer(), nullable=True),
    )

    op.create_table(
        "patients",
        sa.Column("idPatients", sa.Integer(), primary_key=True, autoincrement=True),
        sa.Column("Name", sa.String(length=145), nullable=False),
        sa.Column("Secname", sa.String(length=145), nullable=True),
        sa.Column("Surname", sa.String(length=145), nullable=False),
        sa.Column("Birthdate", sa.Date(), nullable=True),
        sa.Column("Username", sa.String(length=145), nullable=False),
        sa.Column("Password", sa.String(length=45), nullable=False),
    )

    op.create_table(
        "videos",
        sa.Column("idvideos", sa.Integer(), primary_key=True, autoincrement=True),
        sa.Column("video_name", sa.String(length=45), nullable=True),
        sa.Column("filename", sa.String(length=45), nullable=True),
        sa.Column("ex_type", sa.String(length=50), nullable=False, server_default=sa.text("'для всех'")),
    )

    op.create_table(
        "appointments",
        sa.Column("idAppointments", sa.Integer(), primary_key=True, autoincrement=True),
        sa.Column("Starttime", sa.Date(), nullable=True),
        sa.Column("Endtime", sa.Date(), nullable=True),
        sa.Column("Comments", sa.Text(), nullable=True),
        sa.Column("idPatients", sa.Integer(), nullable=False, primary_key=True),
        sa.Column("idvideos", sa.Integer(), nullable=False),
        sa.Column("idDoctors", sa.Integer(), nullable=False, primary_key=True, server_default=sa.text("1")),
        sa.Column("kolvden", sa.Integer(), nullable=False, server_default=sa.text("1")),
        sa.Column("dlitelnost", sa.Time(), nullable=True, server_default=sa.text("'00:00:50'")),
        sa.Column("sdelanovden", sa.Integer(), nullable=False, server_default=sa.text("0")),
        sa.Column("sdelanovsego", sa.Integer(), nullable=False, server_default=sa.text("0")),
        sa.Column("Lastsession", sa.DateTime(), nullable=True),
        sa.Column("done_percent", sa.Integer(), nullable=False, server_default=sa.text("0")),
        sa.Column("CumulativeTimeSpent", sa.Float(), nullable=True, server_default=sa.text("0")),
        sa.ForeignKeyConstraint(["idDoctors"], ["doctors.idDoctors"]),
        sa.ForeignKeyConstraint(["idPatients"], ["patients.idPatients"]),
        sa.ForeignKeyConstraint(["idvideos"], ["videos.idvideos"]),
    )

    op.create_table(
        "disorders",
        sa.Column("idDisorders", sa.Integer(), primary_key=True, autoincrement=True),
        sa.Column("Disorder_name", sa.String(length=45), nullable=True),
        sa.Column("Disorder_type", sa.String(length=45), nullable=True),
    )

    op.create_table(
        "push_count",
        sa.Column("id", sa.Integer(), primary_key=True, autoincrement=True),
        sa.Column("count", sa.Integer(), nullable=True),
    )

    op.create_table(
        "rules",
        sa.Column("doc_rules", sa.Integer(), primary_key=True),
        sa.Column("rule_description", sa.String(length=45), nullable=True),
    )

    op.create_table(
        "disorder_videos",
        sa.Column("disorder_id", sa.Integer(), nullable=False),
        sa.Column("videos_id", sa.Integer(), nullable=False),
        sa.ForeignKeyConstraint(["disorder_id"], ["disorders.idDisorders"], ondelete="CASCADE", onupdate="CASCADE"),
        sa.ForeignKeyConstraint(["videos_id"], ["videos.idvideos"]),
        sa.PrimaryKeyConstraint("disorder_id", "videos_id"),
    )
    op.create_index("disorder_videos_ibfk_2", "disorder_videos", ["videos_id"])

    op.create_index("fk_appointments_patients_idx", "appointments", ["idPatients"] )
    op.create_index("fk_appointments_videos1_idx", "appointments", ["idvideos"] )
    op.create_index("fk_appointments_doctors1_idx", "appointments", ["idDoctors"] )

    op.create_table(
        "patient_disorders",
        sa.Column("patient_id", sa.Integer(), nullable=False),
        sa.Column("disorder_id", sa.Integer(), nullable=False),
        sa.ForeignKeyConstraint(["disorder_id"], ["disorders.idDisorders"]),
        sa.ForeignKeyConstraint(["patient_id"], ["patients.idPatients"]),
        sa.PrimaryKeyConstraint("patient_id", "disorder_id"),
    )
    op.create_index("patient_disorders_disorder_id_idx", "patient_disorders", ["disorder_id"])

    op.execute(
        sa.text(
            """
            CREATE EVENT IF NOT EXISTS reset_field_event
            ON SCHEDULE EVERY 1 DAY
            STARTS '2023-08-15 00:00:00'
            DO
            BEGIN
                UPDATE appointments SET sdelanovden = 0;
                UPDATE appointments SET CumulativeTimeSpent = 0;
            END
            """
        )
    )

    op.execute(
        sa.text(
            """
            CREATE EVENT IF NOT EXISTS update_done_percent_event
            ON SCHEDULE EVERY 1 DAY
            STARTS '2023-08-31 09:17:29'
            DO
            BEGIN
                UPDATE appointments
                SET done_percent = CASE
                    WHEN (DATEDIFF(NOW(), starttime) * kolvden) > 0 THEN
                        (sdelanovsego * 100) / (DATEDIFF(NOW(), starttime) * kolvden)
                    ELSE 0
                END
                WHERE NOW() BETWEEN starttime AND endtime;
            END
            """
        )
    )


def downgrade() -> None:
    op.execute(sa.text("DROP EVENT IF EXISTS update_done_percent_event"))
    op.execute(sa.text("DROP EVENT IF EXISTS reset_field_event"))

    op.drop_table("patient_disorders")
    op.drop_table("disorder_videos")
    op.drop_table("rules")
    op.drop_table("push_count")
    op.drop_table("disorders")
    op.drop_table("appointments")
    op.drop_table("videos")
    op.drop_table("patients")
    op.drop_table("doctors")
