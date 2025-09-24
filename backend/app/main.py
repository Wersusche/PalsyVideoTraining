from collections import defaultdict
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
