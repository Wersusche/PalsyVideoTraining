from collections import defaultdict
from datetime import date
import random
import re
from typing import Any

from fastapi import Depends, FastAPI, HTTPException
from sqlalchemy import text
from sqlalchemy.ext.asyncio import AsyncSession

from app.core.config import get_settings
from app.db.session import get_session

from pydantic import BaseModel

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


class DoctorDashboardResponse(BaseModel):
    patients: list[PatientSchema]
    disorders: list[DisorderSchema]
    exercises: list[ExerciseSchema]
    disorderExerciseMap: dict[int, list[int]]


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
                SELECT "idvideos", "video_name", "ex_type", "filename"
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
