"""Initial PostgreSQL schema migrated from MySQL dump."""

from __future__ import annotations

from pathlib import Path

from alembic import op

# revision identifiers, used by Alembic.
revision = "20240919_01_initial_schema"
down_revision = None
branch_labels = None
depends_on = None


def _split_sql_statements(sql: str) -> list[str]:
    """Split a SQL script into individual statements.

    Psycopg (used under SQLAlchemy) cannot execute multiple statements in a
    single ``execute`` call when the script contains procedural blocks such as
    ``CREATE FUNCTION``.  We therefore need to feed statements one by one while
    keeping the content within quotes and dollar-quoted blocks intact.
    """

    # Remove single-line comments – PostgreSQL ignores the rest of the line
    # after ``--``.  This keeps the parser below simpler without affecting
    # statements inside dollar-quoted blocks (which do not contain comments in
    # the converted dump).
    lines = []
    for line in sql.splitlines():
        stripped = line.strip()
        if stripped.startswith("--"):
            continue
        lines.append(line)
    sql = "\n".join(lines)

    statements: list[str] = []
    buffer: list[str] = []
    in_single_quote = False
    in_double_quote = False
    dollar_quote_delimiter: str | None = None
    i = 0

    while i < len(sql):
        char = sql[i]

        if dollar_quote_delimiter is not None:
            if sql.startswith(dollar_quote_delimiter, i):
                buffer.append(dollar_quote_delimiter)
                i += len(dollar_quote_delimiter)
                dollar_quote_delimiter = None
                continue
            buffer.append(char)
            i += 1
            continue

        if char == "'":
            buffer.append(char)
            if in_single_quote:
                # handle escaped single quote inside a string literal
                if i + 1 < len(sql) and sql[i + 1] == "'":
                    buffer.append("'")
                    i += 2
                    continue
                in_single_quote = False
            else:
                in_single_quote = True
            i += 1
            continue

        if char == '"':
            buffer.append(char)
            in_double_quote = not in_double_quote
            i += 1
            continue

        if char == "$":
            # Detect dollar-quoted blocks ($tag$ ... $tag$)
            j = i + 1
            while j < len(sql) and (sql[j].isalnum() or sql[j] == "_"):
                j += 1
            if j < len(sql) and sql[j] == "$":
                delimiter = sql[i : j + 1]
                buffer.append(delimiter)
                if dollar_quote_delimiter is None:
                    dollar_quote_delimiter = delimiter
                else:
                    dollar_quote_delimiter = None
                i = j + 1
                continue

        if (
            char == ";"
            and not in_single_quote
            and not in_double_quote
            and dollar_quote_delimiter is None
        ):
            statement = "".join(buffer).strip()
            if statement:
                statements.append(statement)
            buffer.clear()
            i += 1
            # Skip any trailing whitespace/newlines between statements
            while i < len(sql) and sql[i] in "\r\n\t ":
                i += 1
            continue

        buffer.append(char)
        i += 1

    tail = "".join(buffer).strip()
    if tail:
        statements.append(tail)

    return statements


def upgrade() -> None:
    sql_path = Path(__file__).with_suffix(".sql")
    sql_statements = _split_sql_statements(sql_path.read_text())
    bind = op.get_bind()
    for statement in sql_statements:
        bind.exec_driver_sql(statement, execution_options={"prepared": False})


def downgrade() -> None:
    raise RuntimeError("Downgrade is not supported for the initial schema import.")
