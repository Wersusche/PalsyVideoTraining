from collections import defaultdict
import json
from datetime import date, datetime, time
from decimal import Decimal
import random
import re
from typing import Any
from uuid import UUID

import bcrypt
from fastapi import Depends, FastAPI, HTTPException
from sqlalchemy import text
from sqlalchemy.ext.asyncio import AsyncSession

from app.core.config import get_settings
from app.db.session import get_session

from pydantic import BaseModel, Field

settings = get_settings()
app = FastAPI(title=settings.project_name)


class DisorderSchema(BaseModel):
    id: int
    name: str
    category: str


class ExerciseSchema(BaseModel):
    id: int
    name: str
    type: str
    description: str | None = None
    bodyPart: str | None = None
    typeOfActivity: str | None = None


class AppointmentSchema(BaseModel):
    id: str
    exerciseId: int
    start: str
    end: str
    perDay: int
    totalCompleted: int
    donePercent: int
    durationSeconds: int


class PatientSchema(BaseModel):
    id: int
    firstName: str
    lastName: str
    middleName: str
    birthDate: str
    username: str
    password: str
    disorders: list[int]
    appointments: list[AppointmentSchema]


class PatientCreateRequest(BaseModel):
    firstName: str
    lastName: str
    middleName: str | None = None
    birthDate: date
    generateCredentials: bool = False
    username: str | None = None
    password: str | None = None


class DoctorLoginRequest(BaseModel):
    login: str
    password: str


class DoctorLoginResponse(BaseModel):
    status: str = "ok"


class DoctorDashboardResponse(BaseModel):
    patients: list[PatientSchema]
    disorders: list[DisorderSchema]
    exercises: list[ExerciseSchema]
    disorderExerciseMap: dict[int, list[int]]


class DatabaseTablesResponse(BaseModel):
    tables: list[str]


class TableDataResponse(BaseModel):
    columns: list[str]
    rows: list[dict[str, Any]]
    totalRows: int
    pkColumns: list[str]


class InsertedRowSchema(BaseModel):
    tempId: Any | None = None
    primaryKey: dict[str, Any]


class TableMutationRequest(BaseModel):
    new_rows: list[dict[str, Any]] = Field(default_factory=list)
    updated_rows: list[dict[str, Any]] = Field(default_factory=list)
    deleted_rows: list[dict[str, Any]] = Field(default_factory=list)


class TableMutationResponse(BaseModel):
    inserted: list[InsertedRowSchema] = []


@app.get("/api/healthz")
async def healthcheck(db: AsyncSession = Depends(get_session)) -> dict[str, str]:
    await db.execute(text("SELECT 1"))
    return {"status": "ok"}


def _to_iso_date(value: Any) -> str:
    if value is None:
        return ""
    try:
        return value.isoformat()
    except AttributeError:
        return str(value)


_TRANSLIT_MAP: dict[str, str] = {
    "а": "a",
    "б": "b",
    "в": "v",
    "г": "g",
    "д": "d",
    "е": "e",
    "ё": "yo",
    "ж": "zh",
    "з": "z",
    "и": "i",
    "й": "y",
    "к": "k",
    "л": "l",
    "м": "m",
    "н": "n",
    "о": "o",
    "п": "p",
    "р": "r",
    "с": "s",
    "т": "t",
    "у": "u",
    "ф": "f",
    "х": "h",
    "ц": "ts",
    "ч": "ch",
    "ш": "sh",
    "щ": "sch",
    "ъ": "",
    "ы": "y",
    "ь": "",
    "э": "e",
    "ю": "yu",
    "я": "ya",
}


def _transliterate(value: str) -> str:
    return "".join(_TRANSLIT_MAP.get(char, char) for char in value.lower())


def _generate_random_password() -> str:
    letters = "abcdefghijkmnopqrstuvwxyz"
    digits = "0123456789"
    digits_at_start = random.random() < 0.5
    result = ""

    def random_letter() -> str:
        return random.choice(letters)

    def random_digit() -> str:
        return random.choice(digits)

    if digits_at_start:
        result += random_digit()
        result += random_digit()

    for _ in range(4):
        result += random_letter()

    if not digits_at_start:
        result += random_digit()
        result += random_digit()

    return result


def _filter_alpha_numeric(value: str) -> str:
    return re.sub(r"[^A-Za-z0-9]", "", value)


async def _username_exists(db: AsyncSession, username: str) -> bool:
    if not username:
        return False
    result = await db.execute(
        text('SELECT 1 FROM "patients" WHERE "Username" = :username'),
        {"username": username},
    )
    return result.scalar() is not None


def _coerce_to_str(value: Any) -> str:
    if value is None:
        return ""
    if isinstance(value, memoryview):
        return value.tobytes().decode("utf-8", "ignore")
    return str(value)


def _normalize_optional_str(value: Any) -> str | None:
    if value is None:
        return None
    if isinstance(value, memoryview):
        value = value.tobytes().decode("utf-8", "ignore")
    value = str(value).strip()
    return value or None


def _serialize_value(value: Any) -> Any:
    if value is None:
        return None
    if isinstance(value, memoryview):
        return value.tobytes().decode("utf-8", "ignore")
    if isinstance(value, (bytes, bytearray)):
        return value.decode("utf-8", "ignore")
    if isinstance(value, (date, datetime, time)):
        return value.isoformat()
    if isinstance(value, Decimal):
        return str(value)
    if isinstance(value, UUID):
        return str(value)
    return value


def _verify_password(plain_password: str, hashed_password: str) -> bool:
    if not plain_password or not hashed_password:
        return False
    try:
        return bcrypt.checkpw(
            plain_password.encode("utf-8"), hashed_password.encode("utf-8")
        )
    except ValueError:
        return False


@app.post("/api/doctor-login", response_model=DoctorLoginResponse)
async def doctor_login(
    payload: DoctorLoginRequest, db: AsyncSession = Depends(get_session)
) -> DoctorLoginResponse:
    login = payload.login.strip()
    if not login:
        raise HTTPException(status_code=401, detail="Неверный логин или пароль.")

    doctor_row = (
        await db.execute(
            text('SELECT "Password" FROM "doctors" WHERE "Login" = :login'),
            {"login": login},
        )
    ).mappings().first()

    if not doctor_row:
        raise HTTPException(status_code=401, detail="Неверный логин или пароль.")

    stored_password = _coerce_to_str(doctor_row.get("Password"))

    if not stored_password or not _verify_password(payload.password, stored_password):
        raise HTTPException(status_code=401, detail="Неверный логин или пароль.")

    return DoctorLoginResponse()


async def _generate_unique_username(
    db: AsyncSession, first_name: str, last_name: str
) -> str:
    first = _transliterate(first_name)
    last = _transliterate(last_name)
    base = f"{first}.{last[:1]}"
    candidate = base or "user"

    if not await _username_exists(db, candidate):
        return candidate

    counter = 2
    while True:
        new_candidate = f"{base}{counter}" if base else f"user{counter}"
        if not await _username_exists(db, new_candidate):
            return new_candidate
        counter += 1

@app.get("/api/doctor-dashboard", response_model=DoctorDashboardResponse)
async def get_doctor_dashboard(
    db: AsyncSession = Depends(get_session),
) -> DoctorDashboardResponse:
    patient_rows = (
        await db.execute(
            text(
                """
                SELECT "idPatients", "Name", "Surname", "Secname", "Birthdate", "Username", "Password"
                FROM "patients"
                ORDER BY "Surname", "Name", "Secname"
                """
            )
        )
    ).mappings()

    patients_data: dict[int, dict[str, Any]] = {}
    for row in patient_rows:
        patient_id = int(row["idPatients"])
        patients_data[patient_id] = {
            "id": patient_id,
            "firstName": (row["Name"] or "").strip(),
            "lastName": (row["Surname"] or "").strip(),
            "middleName": (row["Secname"] or "").strip(),
            "birthDate": _to_iso_date(row["Birthdate"]),
            "username": (row["Username"] or "").strip(),
            "password": (row["Password"] or "").strip(),
            "disorders": [],
            "appointments": [],
        }

    disorder_rows = (
        await db.execute(
            text(
                """
                SELECT "idDisorders", "Disorder_name", "Disorder_type"
                FROM "disorders"
                ORDER BY "Disorder_type", "Disorder_name"
                """
            )
        )
    ).mappings()
    disorders = [
        DisorderSchema(
            id=int(row["idDisorders"]),
            name=(row["Disorder_name"] or "").strip(),
            category=(row["Disorder_type"] or "").strip(),
        )
        for row in disorder_rows
    ]

    exercises_rows = (
        await db.execute(
            text(
                """
                SELECT "idvideos", "video_name", "ex_type", "filename", "body_part", "type_of_activity"
                FROM "videos"
                ORDER BY "ex_type", "video_name"
                """
            )
        )
    ).mappings()
    exercises = [
        ExerciseSchema(
            id=int(row["idvideos"]),
            name=(row["video_name"] or "").strip(),
            type=(row["ex_type"] or "").strip(),
            description=row.get("filename"),
            bodyPart=_normalize_optional_str(row.get("body_part")),
            typeOfActivity=_normalize_optional_str(row.get("type_of_activity")),
        )
        for row in exercises_rows
    ]

    patient_disorders_rows = (
        await db.execute(
            text(
                """
                SELECT "patient_id", "disorder_id"
                FROM "patient_disorders"
                """
            )
        )
    ).mappings()
    patient_disorders_map: dict[int, list[int]] = defaultdict(list)
    for row in patient_disorders_rows:
        patient_id = row.get("patient_id")
        disorder_id = row.get("disorder_id")
        if patient_id is None or disorder_id is None:
            continue
        patient_disorders_map[int(patient_id)].append(int(disorder_id))

    appointments_rows = (
        await db.execute(
            text(
                """
                SELECT "idAppointments", "idPatients", "idvideos", "Starttime", "Endtime",
                       "kolvden", "sdelanovsego", "done_percent",
                       EXTRACT(EPOCH FROM "dlitelnost") AS duration_seconds
                FROM "appointments"
                """
            )
        )
    ).mappings()
    appointments_map: dict[int, list[AppointmentSchema]] = defaultdict(list)
    for row in appointments_rows:
        patient_id = row.get("idPatients")
        if patient_id is None:
            continue
        appointment = AppointmentSchema(
            id=str(row["idAppointments"]),
            exerciseId=int(row["idvideos"]),
            start=_to_iso_date(row["Starttime"]),
            end=_to_iso_date(row["Endtime"]),
            perDay=int(row.get("kolvden") or 0),
            totalCompleted=int(row.get("sdelanovsego") or 0),
            donePercent=int(row.get("done_percent") or 0),
            durationSeconds=int(row.get("duration_seconds") or 0),
        )
        appointments_map[int(patient_id)].append(appointment)

    for patient_id, disorder_ids in patient_disorders_map.items():
        if patient_id in patients_data:
            patients_data[patient_id]["disorders"] = disorder_ids

    for patient_id, patient_appointments in appointments_map.items():
        if patient_id in patients_data:
            patients_data[patient_id]["appointments"] = patient_appointments

    disorder_video_rows = (
        await db.execute(
            text(
                """
                SELECT "disorder_id", "videos_id"
                FROM "disorder_videos"
                """
            )
        )
    ).mappings()
    disorder_exercise_map: dict[int, list[int]] = defaultdict(list)
    for row in disorder_video_rows:
        disorder_id = row.get("disorder_id")
        video_id = row.get("videos_id")
        if disorder_id is None or video_id is None:
            continue
        disorder_exercise_map[int(disorder_id)].append(int(video_id))

    patients = [PatientSchema(**data) for data in patients_data.values()]

    return DoctorDashboardResponse(
        patients=patients,
        disorders=disorders,
        exercises=exercises,
        disorderExerciseMap={key: value for key, value in disorder_exercise_map.items()},
    )


@app.post("/api/patients", response_model=PatientSchema, status_code=201)
async def create_patient(
    payload: PatientCreateRequest, db: AsyncSession = Depends(get_session)
) -> PatientSchema:
    first_name = payload.firstName.strip()
    last_name = payload.lastName.strip()
    middle_name_raw = (payload.middleName or "").strip()

    if not first_name or not last_name:
        raise HTTPException(status_code=400, detail="Укажите фамилию и имя пациента.")

    if payload.generateCredentials:
        username = await _generate_unique_username(db, first_name, last_name)
        password = _generate_random_password()
    else:
        username_input = _filter_alpha_numeric((payload.username or "").strip())
        if not username_input:
            raise HTTPException(status_code=400, detail="Введите логин пациента.")
        if await _username_exists(db, username_input):
            raise HTTPException(
                status_code=400,
                detail="Такой логин уже существует. Укажите другой.",
            )
        username = username_input
        password = (payload.password or "").strip() or _generate_random_password()

    insert_result = await db.execute(
        text(
            """
            INSERT INTO "patients" ("Name", "Surname", "Secname", "Birthdate", "Username", "Password")
            VALUES (:first_name, :last_name, :middle_name, :birth_date, :username, :password)
            RETURNING "idPatients"
            """
        ),
        {
            "first_name": first_name,
            "last_name": last_name,
            "middle_name": middle_name_raw or None,
            "birth_date": payload.birthDate,
            "username": username,
            "password": password,
        },
    )
    row = insert_result.fetchone()
    if row is None:
        await db.rollback()
        raise HTTPException(status_code=500, detail="Не удалось сохранить пациента.")

    await db.commit()

    patient_id = int(row[0])
    return PatientSchema(
        id=patient_id,
        firstName=first_name,
        lastName=last_name,
        middleName=middle_name_raw,
        birthDate=payload.birthDate.isoformat(),
        username=username,
        password=password,
        disorders=[],
        appointments=[],
    )


@app.delete("/api/patients/{patient_id}", status_code=204)
async def delete_patient(patient_id: int, db: AsyncSession = Depends(get_session)) -> None:
    patient_exists = await db.execute(
        text(
            """
            SELECT 1
            FROM "patients"
            WHERE "idPatients" = :patient_id
            """
        ),
        {"patient_id": patient_id},
    )

    if patient_exists.scalar() is None:
        raise HTTPException(status_code=404, detail="Patient not found")

    await db.execute(
        text('DELETE FROM "appointments" WHERE "idPatients" = :patient_id'),
        {"patient_id": patient_id},
    )
    await db.execute(
        text('DELETE FROM "patient_disorders" WHERE "patient_id" = :patient_id'),
        {"patient_id": patient_id},
    )
    await db.execute(
        text('DELETE FROM "patients" WHERE "idPatients" = :patient_id'),
        {"patient_id": patient_id},
    )
    await db.commit()


def _quote_identifier(identifier: str) -> str:
    escaped = identifier.replace('"', '""')
    return f'"{escaped}"'


async def _get_public_tables(db: AsyncSession) -> list[str]:
    rows = await db.execute(
        text(
            """
            SELECT table_name
            FROM information_schema.tables
            WHERE table_schema = 'public'
              AND table_type = 'BASE TABLE'
            ORDER BY table_name
            """
        )
    )
    return [str(row[0]) for row in rows]


async def _get_table_columns(db: AsyncSession, table_name: str) -> list[str]:
    rows = await db.execute(
        text(
            """
            SELECT column_name
            FROM information_schema.columns
            WHERE table_schema = 'public'
              AND table_name = :table_name
            ORDER BY ordinal_position
            """
        ),
        {"table_name": table_name},
    )
    return [str(row[0]) for row in rows]


async def _get_table_pk_columns(db: AsyncSession, table_name: str) -> list[str]:
    rows = await db.execute(
        text(
            """
            SELECT kcu.column_name
            FROM information_schema.table_constraints tc
            JOIN information_schema.key_column_usage kcu
              ON tc.constraint_name = kcu.constraint_name
             AND tc.table_schema = kcu.table_schema
            WHERE tc.constraint_type = 'PRIMARY KEY'
              AND tc.table_schema = 'public'
              AND tc.table_name = :table_name
            ORDER BY kcu.ordinal_position
            """
        ),
        {"table_name": table_name},
    )
    return [str(row[0]) for row in rows]


def _build_filter_clause(filter_model: dict[str, Any], valid_columns: set[str]) -> tuple[str, dict[str, Any]]:
    if not filter_model:
        return "", {}

    params: dict[str, Any] = {}
    clauses: list[str] = []
    counter = 0

    def build_condition(column: str, config: dict[str, Any]) -> tuple[str, dict[str, Any]]:
        nonlocal counter
        if not isinstance(config, dict):
            return "", {}

        operator = config.get("operator")
        condition1 = config.get("condition1")
        condition2 = config.get("condition2")
        if operator and isinstance(condition1, dict) and isinstance(condition2, dict):
            clause1, params1 = build_condition(column, condition1)
            clause2, params2 = build_condition(column, condition2)
            merged = {**params1, **params2}
            if clause1 and clause2:
                op = "AND" if str(operator).lower() == "and" else "OR"
                return f"({clause1} {op} {clause2})", merged
            if clause1:
                return clause1, merged
            if clause2:
                return clause2, merged
            return "", merged

        filter_type = config.get("filterType")
        column_expr = _quote_identifier(column)

        def make_param(value: Any) -> tuple[str, dict[str, Any]]:
            nonlocal counter
            param_name = f"p_{counter}"
            counter += 1
            return param_name, {param_name: value}

        if filter_type == "text":
            filter_condition = config.get("type")
            if filter_condition in {"blank", "notBlank"}:
                if filter_condition == "blank":
                    return f"({column_expr} IS NULL OR CAST({column_expr} AS TEXT) = '')", {}
                return f"({column_expr} IS NOT NULL AND CAST({column_expr} AS TEXT) <> '')", {}

            value = config.get("filter")
            if not isinstance(value, str):
                return "", {}
            param_name, param_value = make_param(value)
            if filter_condition == "contains":
                return (
                    f"CAST({column_expr} AS TEXT) ILIKE '%' || :{param_name} || '%'",
                    param_value,
                )
            if filter_condition == "notContains":
                return (
                    f"CAST({column_expr} AS TEXT) NOT ILIKE '%' || :{param_name} || '%'",
                    param_value,
                )
            if filter_condition == "equals":
                return (f"CAST({column_expr} AS TEXT) ILIKE :{param_name}", param_value)
            if filter_condition == "notEqual":
                return (f"CAST({column_expr} AS TEXT) NOT ILIKE :{param_name}", param_value)
            if filter_condition == "startsWith":
                return (
                    f"CAST({column_expr} AS TEXT) ILIKE :{param_name} || '%'",
                    param_value,
                )
            if filter_condition == "endsWith":
                return (
                    f"CAST({column_expr} AS TEXT) ILIKE '%' || :{param_name}",
                    param_value,
                )
            return "", {}

        if filter_type == "number":
            filter_condition = config.get("type")
            if filter_condition in {"blank", "notBlank"}:
                if filter_condition == "blank":
                    return f"{column_expr} IS NULL", {}
                return f"{column_expr} IS NOT NULL", {}

            value = config.get("filter")
            filter_to = config.get("filterTo")
            try:
                numeric_value = float(value)
            except (TypeError, ValueError):
                return "", {}

            if filter_condition == "inRange":
                try:
                    numeric_value_to = float(filter_to)
                except (TypeError, ValueError):
                    return "", {}
                param_from, param_dict_from = make_param(numeric_value)
                param_to, param_dict_to = make_param(numeric_value_to)
                return (
                    f"{column_expr} BETWEEN :{param_from} AND :{param_to}",
                    {**param_dict_from, **param_dict_to},
                )

            param_name, param_value = make_param(numeric_value)
            if filter_condition == "equals":
                return (f"{column_expr} = :{param_name}", param_value)
            if filter_condition == "notEqual":
                return (f"{column_expr} <> :{param_name}", param_value)
            if filter_condition == "lessThan":
                return (f"{column_expr} < :{param_name}", param_value)
            if filter_condition == "lessThanOrEqual":
                return (f"{column_expr} <= :{param_name}", param_value)
            if filter_condition == "greaterThan":
                return (f"{column_expr} > :{param_name}", param_value)
            if filter_condition == "greaterThanOrEqual":
                return (f"{column_expr} >= :{param_name}", param_value)
            return "", {}

        if filter_type == "set":
            values = config.get("values")
            if not isinstance(values, list) or not values:
                return "", {}
            placeholders: list[str] = []
            collected: dict[str, Any] = {}
            for value in values:
                param_name, param_value = make_param(value)
                placeholders.append(f":{param_name}")
                collected.update(param_value)
            joined = ", ".join(placeholders)
            return (f"{column_expr} IN ({joined})", collected)

        if filter_type == "date":
            filter_condition = config.get("type")
            if filter_condition in {"blank", "notBlank"}:
                if filter_condition == "blank":
                    return f"{column_expr} IS NULL", {}
                return f"{column_expr} IS NOT NULL", {}

            value = config.get("dateFrom") or config.get("filter")
            if not value:
                return "", {}
            identifier = f"CAST({column_expr} AS DATE)"
            if filter_condition == "inRange":
                value_to = config.get("dateTo") or config.get("filterTo")
                if not value_to:
                    return "", {}
                param_from, param_dict_from = make_param(value)
                param_to, param_dict_to = make_param(value_to)
                return (
                    f"{identifier} BETWEEN :{param_from} AND :{param_to}",
                    {**param_dict_from, **param_dict_to},
                )
            param_name, param_value = make_param(value)
            if filter_condition == "equals":
                return (f"{identifier} = :{param_name}", param_value)
            if filter_condition == "notEqual":
                return (f"{identifier} <> :{param_name}", param_value)
            if filter_condition == "lessThan":
                return (f"{identifier} < :{param_name}", param_value)
            if filter_condition == "lessThanOrEqual":
                return (f"{identifier} <= :{param_name}", param_value)
            if filter_condition == "greaterThan":
                return (f"{identifier} > :{param_name}", param_value)
            if filter_condition == "greaterThanOrEqual":
                return (f"{identifier} >= :{param_name}", param_value)
            return "", {}

        return "", {}

    for column, config in filter_model.items():
        if not isinstance(config, dict):
            continue
        if column not in valid_columns:
            continue
        clause, clause_params = build_condition(column, config)
        if clause:
            clauses.append(clause)
            params.update(clause_params)

    if not clauses:
        return "", {}

    return " AND ".join(clauses), params


def _build_sort_clause(sort_model: list[dict[str, Any]], valid_columns: set[str]) -> str:
    if not sort_model:
        return ""
    parts: list[str] = []
    for item in sort_model:
        column = item.get("colId")
        if column not in valid_columns:
            continue
        direction_raw = str(item.get("sort", "asc")).lower()
        direction = "DESC" if direction_raw == "desc" else "ASC"
        parts.append(f"{_quote_identifier(column)} {direction}")
    return ", ".join(parts)


@app.get("/api/database/tables", response_model=DatabaseTablesResponse)
async def list_database_tables(db: AsyncSession = Depends(get_session)) -> DatabaseTablesResponse:
    tables = await _get_public_tables(db)
    return DatabaseTablesResponse(tables=tables)


@app.get("/api/database/tables/{table_name}", response_model=TableDataResponse)
async def get_table_data(
    table_name: str,
    offset: int = 0,
    limit: int = 100,
    filter: str | None = None,
    sort: str | None = None,
    db: AsyncSession = Depends(get_session),
) -> TableDataResponse:
    offset = max(offset, 0)
    limit = min(max(limit, 1), 500)

    tables = await _get_public_tables(db)
    if table_name not in tables:
        raise HTTPException(status_code=404, detail="Table not found")

    quoted_table = _quote_identifier(table_name)

    try:
        filter_model = json.loads(filter) if filter else {}
    except json.JSONDecodeError:
        filter_model = {}
    if not isinstance(filter_model, dict):
        filter_model = {}

    try:
        sort_model = json.loads(sort) if sort else []
    except json.JSONDecodeError:
        sort_model = []
    if not isinstance(sort_model, list):
        sort_model = []

    columns = await _get_table_columns(db, table_name)
    valid_columns = set(columns)
    filter_clause, filter_params = _build_filter_clause(filter_model, valid_columns)
    sort_clause = _build_sort_clause(sort_model, valid_columns)
    pk_columns = await _get_table_pk_columns(db, table_name)

    count_query = f"SELECT COUNT(*) FROM {quoted_table}"
    if filter_clause:
        count_query += f" WHERE {filter_clause}"
    count_result = await db.execute(text(count_query), filter_params)
    total_rows_raw = count_result.scalar()
    total_rows = int(total_rows_raw or 0)

    query = f"SELECT * FROM {quoted_table}"
    if filter_clause:
        query += f" WHERE {filter_clause}"
    if sort_clause:
        query += f" ORDER BY {sort_clause}"
    query += " OFFSET :offset LIMIT :limit"

    params = {**filter_params, "offset": offset, "limit": limit}
    data_result = await db.execute(text(query), params)

    if not columns:
        columns = list(data_result.keys())
    rows = [
        {column: _serialize_value(value) for column, value in row.items()}
        for row in data_result.mappings()
    ]

    return TableDataResponse(
        columns=columns,
        rows=rows,
        totalRows=total_rows,
        pkColumns=pk_columns,
    )


@app.post("/api/database/tables/{table_name}", response_model=TableMutationResponse)
async def mutate_table_data(
    table_name: str,
    payload: TableMutationRequest,
    db: AsyncSession = Depends(get_session),
) -> TableMutationResponse:
    tables = await _get_public_tables(db)
    if table_name not in tables:
        raise HTTPException(status_code=404, detail="Table not found")

    quoted_table = _quote_identifier(table_name)
    columns = await _get_table_columns(db, table_name)
    pk_columns = await _get_table_pk_columns(db, table_name)
    column_set = set(columns)
    pk_set = set(pk_columns)

    inserted_rows: list[InsertedRowSchema] = []

    try:
        # Handle inserts
        for index, row in enumerate(payload.new_rows):
            if not isinstance(row, dict):
                continue
            temp_id = row.get("__tmp_id") or row.get("tempId") or row.get("__rowId")
            values = {key: row[key] for key in row if key in column_set and key not in pk_set}
            params: dict[str, Any] = {}
            if values:
                column_names = ", ".join(_quote_identifier(column) for column in values)
                value_placeholders: list[str] = []
                for position, (column, value) in enumerate(values.items()):
                    param_name = f"ins_{index}_{position}"
                    params[param_name] = value
                    value_placeholders.append(f":{param_name}")
                query = f"INSERT INTO {quoted_table} ({column_names}) VALUES ({', '.join(value_placeholders)})"
            else:
                query = f"INSERT INTO {quoted_table} DEFAULT VALUES"
            if pk_columns:
                returning = ", ".join(_quote_identifier(column) for column in pk_columns)
                query = f"{query} RETURNING {returning}"
            result = await db.execute(text(query), params)
            if pk_columns:
                mapping = result.mappings().first()
                if mapping is not None:
                    primary_key = {
                        column: _serialize_value(mapping[column]) for column in pk_columns
                    }
                else:
                    primary_key = {column: None for column in pk_columns}
            else:
                primary_key = {}
            inserted_rows.append(InsertedRowSchema(tempId=temp_id, primaryKey=primary_key))

        # Handle updates
        for index, row in enumerate(payload.updated_rows):
            if not isinstance(row, dict):
                continue
            if not pk_columns:
                continue
            pk_values = {column: row.get(column) for column in pk_columns}
            if any(value is None for value in pk_values.values()):
                continue
            updates = {
                key: row[key]
                for key in row
                if key in column_set and key not in pk_set and key not in {"__deleted"}
            }
            if not updates:
                continue
            set_clauses: list[str] = []
            params: dict[str, Any] = {}
            for position, (column, value) in enumerate(updates.items()):
                param_name = f"upd_{index}_{position}"
                set_clauses.append(f"{_quote_identifier(column)} = :{param_name}")
                params[param_name] = value
            where_clauses: list[str] = []
            for position, (column, value) in enumerate(pk_values.items()):
                param_name = f"upd_pk_{index}_{position}"
                where_clauses.append(f"{_quote_identifier(column)} = :{param_name}")
                params[param_name] = value
            query = (
                f"UPDATE {quoted_table} SET {', '.join(set_clauses)}"
                f" WHERE {' AND '.join(where_clauses)}"
            )
            await db.execute(text(query), params)

        # Handle deletes
        for index, row in enumerate(payload.deleted_rows):
            if not isinstance(row, dict):
                continue
            if not pk_columns:
                continue
            pk_values = {column: row.get(column) for column in pk_columns}
            if any(value is None for value in pk_values.values()):
                continue
            where_clauses: list[str] = []
            params: dict[str, Any] = {}
            for position, (column, value) in enumerate(pk_values.items()):
                param_name = f"del_{index}_{position}"
                where_clauses.append(f"{_quote_identifier(column)} = :{param_name}")
                params[param_name] = value
            query = f"DELETE FROM {quoted_table} WHERE {' AND '.join(where_clauses)}"
            await db.execute(text(query), params)

        await db.commit()
    except Exception as exc:  # pragma: no cover - defensive branch
        await db.rollback()
        raise HTTPException(status_code=400, detail="Не удалось сохранить изменения") from exc

    return TableMutationResponse(inserted=inserted_rows)
