import { ChangeEvent, FormEvent, useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { AllCommunityModule, ModuleRegistry, themeQuartz } from 'ag-grid-community';
import type { ColDef, GridApi, GridReadyEvent, IDatasource, IGetRowsParams } from 'ag-grid-community';
import { AgGridReact } from 'ag-grid-react';
import 'ag-grid-community/styles/ag-grid.css';
import 'ag-grid-community/styles/ag-theme-quartz.css';

ModuleRegistry.registerModules([AllCommunityModule]);

type Disorder = {
  id: number;
  name: string;
  category: string;
};

type Exercise = {
  id: number;
  name: string;
  type: string;
  description?: string;
};

type Appointment = {
  id: string;
  exerciseId: number;
  start: string;
  end: string;
  perDay: number;
  totalCompleted: number;
  donePercent: number;
  durationSeconds: number;
};

type Patient = {
  id: number;
  firstName: string;
  lastName: string;
  middleName: string;
  birthDate: string;
  username: string;
  password: string;
  disorders: number[];
  appointments: Appointment[];
};

type DoctorDashboardProps = {
  onLogout: () => void;
};

type DoctorDashboardData = {
  patients: Patient[];
  disorders: Disorder[];
  exercises: Exercise[];
  disorderExerciseMap: Record<number, number[]>;
};

type DoctorViewMode = 'dashboard' | 'database';

type DatabaseTablesResponse = {
  tables: string[];
};

type TableDataResponse = {
  columns: string[];
  rows: Record<string, unknown>[];
  totalRows: number;
};

const COMPLETED_INITIAL_COUNT = 6;
const COMPLETED_BATCH_SIZE = 4;

const transliterate = (value: string) => {
  const map: Record<string, string> = {
    а: 'a',
    б: 'b',
    в: 'v',
    г: 'g',
    д: 'd',
    е: 'e',
    ё: 'yo',
    ж: 'zh',
    з: 'z',
    и: 'i',
    й: 'y',
    к: 'k',
    л: 'l',
    м: 'm',
    н: 'n',
    о: 'o',
    п: 'p',
    р: 'r',
    с: 's',
    т: 't',
    у: 'u',
    ф: 'f',
    х: 'h',
    ц: 'ts',
    ч: 'ch',
    ш: 'sh',
    щ: 'sch',
    ъ: '',
    ы: 'y',
    ь: '',
    э: 'e',
    ю: 'yu',
    я: 'ya',
  };

  return value
    .toLowerCase()
    .split('')
    .map((char) => map[char] ?? char)
    .join('');
};

const generateRandomPassword = () => {
  const letters = 'abcdefghijkmnopqrstuvwxyz';
  const digits = '0123456789';
  const digitsAtStart = Math.random() < 0.5;
  let result = '';

  const randomLetter = () => letters[Math.floor(Math.random() * letters.length)];
  const randomDigit = () => digits[Math.floor(Math.random() * digits.length)];

  if (digitsAtStart) {
    result += randomDigit();
    result += randomDigit();
  }

  for (let index = 0; index < 4; index += 1) {
    result += randomLetter();
  }

  if (!digitsAtStart) {
    result += randomDigit();
    result += randomDigit();
  }

  return result;
};

const filterAlphaNumeric = (value: string) => value.replace(/[^a-z0-9]/gi, '');

const getAge = (birthDate: string) => {
  const birth = new Date(birthDate);
  if (Number.isNaN(birth.getTime())) {
    return { years: 0, months: 0 };
  }
  const now = new Date();

  const totalMonths =
    (now.getFullYear() - birth.getFullYear()) * 12 + (now.getMonth() - birth.getMonth());
  const years = Math.floor(totalMonths / 12);
  const months = totalMonths - years * 12;

  return { years, months: Math.max(0, months) };
};

const DoctorDashboard = ({ onLogout }: DoctorDashboardProps) => {
  const [patients, setPatients] = useState<Patient[]>([]);
  const [patientSearch, setPatientSearch] = useState('');
  const [selectedPatientId, setSelectedPatientId] = useState<number | null>(null);
  const [disorders, setDisorders] = useState<Disorder[]>([]);
  const [exercises, setExercises] = useState<Exercise[]>([]);
  const [disorderExerciseMap, setDisorderExerciseMap] = useState<Record<number, number[]>>({});
  const [isLoading, setIsLoading] = useState(true);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [viewMode, setViewMode] = useState<DoctorViewMode>('dashboard');
  const [autoCredentials, setAutoCredentials] = useState(true);
  const [newPatient, setNewPatient] = useState({
    lastName: '',
    firstName: '',
    middleName: '',
    birthDate: '',
    username: '',
    password: '',
  });
  const [formMessage, setFormMessage] = useState<string | null>(null);
  const [showOnlyRecommended, setShowOnlyRecommended] = useState(true);
  const [selectedExercises, setSelectedExercises] = useState<number[]>([]);
  const [exerciseForm, setExerciseForm] = useState({
    start: new Date().toISOString().slice(0, 10),
    end: new Date().toISOString().slice(0, 10),
    perDay: '1',
    minutes: '05',
    seconds: '00',
  });
  const [completedVisibleCount, setCompletedVisibleCount] = useState(COMPLETED_INITIAL_COUNT);
  const completedListRef = useRef<HTMLUListElement | null>(null);
  const previousPatientIdRef = useRef<number | null>(null);
  const [databaseTables, setDatabaseTables] = useState<string[]>([]);
  const [isLoadingTables, setIsLoadingTables] = useState(false);
  const [tablesLoadError, setTablesLoadError] = useState<string | null>(null);
  const [selectedTable, setSelectedTable] = useState('');
  const [databaseColumns, setDatabaseColumns] = useState<ColDef[]>([]);
  const [tableError, setTableError] = useState<string | null>(null);
  const gridApiRef = useRef<GridApi | null>(null);
  const emptyDataSource = useMemo<IDatasource>(
    () => ({
      getRows: (params: IGetRowsParams) => {
        params.successCallback([], 0);
      },
    }),
    [],
  );
  const databaseDefaultColDef = useMemo<ColDef>(
    () => ({
      sortable: true,
      resizable: true,
      filter: true,
      flex: 1,
      minWidth: 140,
    }),
    [],
  );

  const getLatestPatientId = (list: Patient[]) => {
    if (list.length === 0) {
      return null;
    }

    const [latest] = [...list].sort((a, b) => b.id - a.id);
    return latest?.id ?? null;
  };

  useEffect(() => {
    let isMounted = true;

    const loadData = async () => {
      try {
        setIsLoading(true);
        const response = await fetch('/api/doctor-dashboard');
        if (!response.ok) {
          throw new Error('Failed to load doctor dashboard data');
        }
        const data: DoctorDashboardData = await response.json();
        if (!isMounted) return;

        setPatients(data.patients);
        setDisorders(data.disorders);
        setExercises(data.exercises);
        const normalizedDisorderMap = Object.entries(data.disorderExerciseMap ?? {}).reduce(
          (acc, [key, value]) => {
            const disorderId = Number.parseInt(key, 10);
            if (!Number.isNaN(disorderId) && Array.isArray(value)) {
              const exercisesForDisorder = value
                .map((item) => Number(item))
                .filter((item) => !Number.isNaN(item));
              acc[disorderId] = exercisesForDisorder;
            }
            return acc;
          },
          {} as Record<number, number[]>,
        );
        setDisorderExerciseMap(normalizedDisorderMap);
        const latestPatientId = getLatestPatientId(data.patients);
        setSelectedPatientId((prev) => {
          if (prev && data.patients.some((patient) => patient.id === prev)) {
            return prev;
          }
          return latestPatientId;
        });
        setLoadError(null);
      } catch (error) {
        console.error(error);
        if (isMounted) {
          setLoadError('Не удалось загрузить данные. Обновите страницу или попробуйте позже.');
        }
      } finally {
        if (isMounted) {
          setIsLoading(false);
        }
      }
    };

    loadData();

    return () => {
      isMounted = false;
    };
  }, []);

  const loadTables = useCallback(async () => {
    try {
      setIsLoadingTables(true);
      setTablesLoadError(null);
      const response = await fetch('/api/database/tables');
      if (!response.ok) {
        throw new Error('Failed to load table list');
      }
      const data: DatabaseTablesResponse = await response.json();
      setDatabaseTables(Array.isArray(data.tables) ? data.tables : []);
    } catch (error) {
      console.error(error);
      setTablesLoadError('Не удалось получить список таблиц. Попробуйте позже.');
    } finally {
      setIsLoadingTables(false);
    }
  }, []);

  useEffect(() => {
    if (viewMode !== 'database') {
      return;
    }
    if (databaseTables.length > 0 || isLoadingTables) {
      return;
    }
    void loadTables();
  }, [viewMode, databaseTables.length, isLoadingTables, loadTables]);

  useEffect(() => {
    setSelectedExercises([]);
  }, [selectedPatientId]);

  const selectedPatient = patients.find((patient) => patient.id === selectedPatientId) ?? null;

  const displayedPatients = useMemo(() => {
    if (patients.length === 0) {
      return [] as Patient[];
    }

    const normalizedSearch = patientSearch.replace(/\s+/g, ' ').trim().toLowerCase();

    if (!normalizedSearch) {
      return [...patients].sort((a, b) => b.id - a.id).slice(0, 5);
    }

    type RankedPatient = {
      patient: Patient;
      priority: number;
      position: number;
      referenceName: string;
    };

    const ranked: RankedPatient[] = [];

    patients.forEach((patient) => {
      const lastFirst = `${patient.lastName} ${patient.firstName}`.trim().toLowerCase();
      const firstLast = `${patient.firstName} ${patient.lastName}`.trim().toLowerCase();

      const matches = [lastFirst.indexOf(normalizedSearch), firstLast.indexOf(normalizedSearch)].filter(
        (index) => index >= 0,
      );

      if (matches.length === 0) {
        return;
      }

      const bestPosition = Math.min(...matches);
      const priority = matches.some((index) => index === 0) ? 0 : 1;
      ranked.push({
        patient,
        priority,
        position: bestPosition,
        referenceName: lastFirst || firstLast,
      });
    });

    ranked.sort((a, b) => {
      if (a.priority !== b.priority) {
        return a.priority - b.priority;
      }
      if (a.position !== b.position) {
        return a.position - b.position;
      }
      return a.referenceName.localeCompare(b.referenceName);
    });

    return ranked.slice(0, 5).map((item) => item.patient);
  }, [patients, patientSearch]);

  const groupedDisorders = useMemo(() => {
    return disorders.reduce<Record<string, Disorder[]>>((acc, item) => {
      if (!acc[item.category]) {
        acc[item.category] = [];
      }
      acc[item.category].push(item);
      return acc;
    }, {});
  }, [disorders]);

  const recommendedExercises = useMemo(() => {
    if (!selectedPatient) {
      return new Set<number>();
    }

    return new Set(
      selectedPatient.disorders.flatMap((disorderId) => disorderExerciseMap[disorderId] ?? []),
    );
  }, [disorderExerciseMap, selectedPatient]);

  const stats = useMemo(() => {
    const now = new Date();
    const total = patients.length;
    let active = 0;
    let atRisk = 0;

    patients.forEach((patient) => {
      const activeAppointments = patient.appointments.filter((appointment) => {
        const start = new Date(appointment.start);
        const end = new Date(appointment.end);
        return start <= now && now <= end;
      });

      if (activeAppointments.length > 0) {
        active += 1;
        if (activeAppointments.some((item) => item.donePercent < 70)) {
          atRisk += 1;
        }
      }
    });

    return { total, active, atRisk };
  }, [patients]);

  const activePatients = useMemo(() => {
    const now = new Date();
    return patients.filter((patient) =>
      patient.appointments.some((appointment) => {
        const start = new Date(appointment.start);
        const end = new Date(appointment.end);
        return start <= now && now <= end;
      }),
    );
  }, [patients]);

  const categorizedExercises = useMemo(() => {
    return exercises.reduce<Record<string, Exercise[]>>((acc, exercise) => {
      if (showOnlyRecommended && selectedPatient) {
        const recommended = recommendedExercises.has(exercise.id);
        if (!recommended) {
          return acc;
        }
      }

      if (!acc[exercise.type]) {
        acc[exercise.type] = [];
      }
      acc[exercise.type].push(exercise);
      return acc;
    }, {});
  }, [exercises, recommendedExercises, selectedPatient, showOnlyRecommended]);

  const resetForm = () => {
    setNewPatient({
      lastName: '',
      firstName: '',
      middleName: '',
      birthDate: '',
      username: '',
      password: '',
    });
  };

  const handlePatientFieldChange = (
    field: keyof typeof newPatient,
    event: ChangeEvent<HTMLInputElement>,
  ) => {
    const value = event.target.value;

    setNewPatient((prev) => ({
      ...prev,
      [field]:
        field === 'username' || field === 'password' ? filterAlphaNumeric(value) : value,
    }));
  };

  const generateUsername = (firstName: string, lastName: string) => {
    const base = `${transliterate(firstName)}.${transliterate(lastName).charAt(0)}`;
    if (!patients.some((patient) => patient.username === base)) {
      return base || 'user';
    }

    let counter = 2;
    let candidate = `${base}${counter}`;
    while (patients.some((patient) => patient.username === candidate)) {
      counter += 1;
      candidate = `${base}${counter}`;
    }
    return candidate;
  };

  const handleAddPatient = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setFormMessage(null);

    if (!newPatient.lastName || !newPatient.firstName || !newPatient.birthDate) {
      setFormMessage('Укажите фамилию, имя и дату рождения пациента.');
      return;
    }

    const username = autoCredentials
      ? generateUsername(newPatient.firstName, newPatient.lastName)
      : newPatient.username;

    if (!username) {
      setFormMessage('Введите логин пациента.');
      return;
    }

    if (!autoCredentials && patients.some((patient) => patient.username === username)) {
      setFormMessage('Такой логин уже существует. Укажите другой.');
      return;
    }

    const password = autoCredentials
      ? undefined
      : newPatient.password || generateRandomPassword();

    const payload = {
      firstName: newPatient.firstName.trim(),
      lastName: newPatient.lastName.trim(),
      middleName: newPatient.middleName.trim() || null,
      birthDate: newPatient.birthDate,
      generateCredentials: autoCredentials,
      username: autoCredentials ? undefined : username.trim(),
      password,
    };

    try {
      const response = await fetch('/api/patients', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload),
      });

      const data = (await response.json().catch(() => null)) as
        | { detail?: string }
        | Patient
        | null;

      if (!response.ok || !data || 'detail' in data) {
        const message = data && 'detail' in data && data.detail
          ? data.detail
          : 'Не удалось сохранить пациента. Попробуйте ещё раз.';
        throw new Error(message);
      }

      const createdPatient = data as Patient;
      setPatients((prev) => [...prev, createdPatient]);
      setSelectedPatientId(createdPatient.id);
      resetForm();
      setFormMessage('Пациент добавлен. Передайте ему логин и пароль.');
    } catch (error) {
      const message = error instanceof Error
        ? error.message
        : 'Не удалось сохранить пациента. Попробуйте ещё раз.';
      setFormMessage(message);
    }
  };

  const toggleDisorder = (disorderId: number) => {
    if (!selectedPatient) return;

    setPatients((prevPatients) =>
      prevPatients.map((patient) => {
        if (patient.id !== selectedPatient.id) {
          return patient;
        }

        const hasDisorder = patient.disorders.includes(disorderId);
        return {
          ...patient,
          disorders: hasDisorder
            ? patient.disorders.filter((item) => item !== disorderId)
            : [...patient.disorders, disorderId],
        };
      }),
    );
  };

  const handleExerciseSelection = (exerciseId: number, checked: boolean) => {
    setSelectedExercises((prev) => {
      if (checked) {
        return prev.includes(exerciseId) ? prev : [...prev, exerciseId];
      }
      return prev.filter((id) => id !== exerciseId);
    });
  };

  const updateExerciseForm = (
    field: keyof typeof exerciseForm,
    event: ChangeEvent<HTMLInputElement>,
  ) => {
    let value = event.target.value;
    if (field === 'minutes' || field === 'seconds') {
      value = value.replace(/\D/g, '');
      if (field === 'seconds' && Number(value) > 59) {
        value = '59';
      }
      if (field === 'minutes' && Number(value) > 59) {
        value = '59';
      }
      if (value.length === 1) {
        value = `0${value}`;
      }
      if (value === '') {
        value = '00';
      }
    }

    setExerciseForm((prev) => ({
      ...prev,
      [field]: value,
    }));
  };

  const handleAssignExercises = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();

    if (!selectedPatient) {
      setFormMessage('Сначала выберите пациента.');
      return;
    }

    if (selectedExercises.length === 0) {
      setFormMessage('Выберите хотя бы одно упражнение для назначения.');
      return;
    }

    const start = new Date(exerciseForm.start);
    const end = new Date(exerciseForm.end);

    if (end < start) {
      setFormMessage('Дата окончания не может быть раньше даты начала.');
      return;
    }

    const perDay = Math.max(1, Number.parseInt(exerciseForm.perDay, 10) || 1);
    const durationSeconds = Number.parseInt(exerciseForm.minutes, 10) * 60 +
      Number.parseInt(exerciseForm.seconds, 10);

    if (Number.isNaN(durationSeconds) || durationSeconds <= 0) {
      setFormMessage('Укажите длительность упражнения.');
      return;
    }

    let overlapsDetected = false;

    setPatients((prevPatients) =>
      prevPatients.map((patient) => {
        if (patient.id !== selectedPatient.id) {
          return patient;
        }

        const updatedAppointments = [...patient.appointments];

        selectedExercises.forEach((exerciseId) => {
          const overlap = patient.appointments.some((appointment) => {
            if (appointment.exerciseId !== exerciseId) {
              return false;
            }
            const existingStart = new Date(appointment.start);
            const existingEnd = new Date(appointment.end);
            return (
              (start >= existingStart && start <= existingEnd) ||
              (end >= existingStart && end <= existingEnd) ||
              (existingStart >= start && existingStart <= end)
            );
          });

          if (overlap) {
            const confirmOverwrite = window.confirm(
              'Для выбранного периода уже назначено упражнение. Добавить ещё одну запись?',
            );
            if (!confirmOverwrite) {
              overlapsDetected = true;
              return;
            }
          }

          updatedAppointments.push({
            id: `app-${Date.now()}-${Math.random().toString(16).slice(2)}`,
            exerciseId,
            start: exerciseForm.start,
            end: exerciseForm.end,
            perDay,
            totalCompleted: 0,
            donePercent: 0,
            durationSeconds,
          });
        });

        return {
          ...patient,
          appointments: updatedAppointments,
        };
      }),
    );

    if (!overlapsDetected) {
      setSelectedExercises([]);
      setFormMessage('Назначение сохранено.');
    }
  };

  const handleDeleteAppointment = (appointmentId: string) => {
    if (!selectedPatient) return;

    setPatients((prevPatients) =>
      prevPatients.map((patient) => {
        if (patient.id !== selectedPatient.id) {
          return patient;
        }

        return {
          ...patient,
          appointments: patient.appointments.filter((appointment) => appointment.id !== appointmentId),
        };
      }),
    );
  };

  const handleDeletePatient = async (patientId: number) => {
    const patient = patients.find((item) => item.id === patientId);
    if (!patient) return;

    const confirmed = window.confirm(
      `Удалить пациента ${patient.lastName} ${patient.firstName}? Все назначения будут потеряны.`,
    );

    if (!confirmed) {
      return;
    }

    try {
      const response = await fetch(`/api/patients/${patientId}`, {
        method: 'DELETE',
      });

      if (!response.ok) {
        throw new Error('Failed to delete patient');
      }
    } catch (error) {
      console.error(error);
      window.alert('Не удалось удалить пациента. Попробуйте позже.');
      return;
    }

    setPatients((prev) => {
      const updated = prev.filter((item) => item.id !== patientId);
      if (selectedPatientId === patientId) {
        setSelectedPatientId(getLatestPatientId(updated));
      }
      return updated;
    });
  };

  const appointmentsByStatus = useMemo(() => {
    if (!selectedPatient) {
      return { current: [], future: [], past: [] } as Record<
        'current' | 'future' | 'past',
        Appointment[]
      >;
    }
    const now = new Date();
    const current: Appointment[] = [];
    const future: Appointment[] = [];
    const past: Appointment[] = [];

    selectedPatient.appointments.forEach((appointment) => {
      const start = new Date(appointment.start);
      const end = new Date(appointment.end);
      if (start <= now && now <= end) {
        current.push(appointment);
      } else if (now < start) {
        future.push(appointment);
      } else {
        past.push(appointment);
      }
    });

    return { current, future, past } as const;
  }, [selectedPatient]);

  const visibleCompletedAppointments = useMemo(
    () => appointmentsByStatus.past.slice(0, completedVisibleCount),
    [appointmentsByStatus.past, completedVisibleCount],
  );

  useEffect(() => {
    const total = appointmentsByStatus.past.length;
    const initialCount = total === 0 ? 0 : Math.min(COMPLETED_INITIAL_COUNT, total);

    if (previousPatientIdRef.current !== selectedPatientId) {
      previousPatientIdRef.current = selectedPatientId;
      setCompletedVisibleCount(initialCount);
      if (completedListRef.current) {
        completedListRef.current.scrollTop = 0;
      }
      return;
    }

    setCompletedVisibleCount((prev) => {
      if (total === 0) {
        return 0;
      }

      if (prev === 0) {
        return initialCount;
      }

      if (prev > total) {
        return total;
      }

      return prev;
    });
  }, [appointmentsByStatus.past.length, selectedPatientId]);

  const handleCompletedScroll = useCallback(() => {
    const element = completedListRef.current;
    if (!element) {
      return;
    }

    const { scrollTop, scrollHeight, clientHeight } = element;
    if (scrollTop + clientHeight < scrollHeight - 16) {
      return;
    }

    setCompletedVisibleCount((prev) => {
      if (prev >= appointmentsByStatus.past.length) {
        return prev;
      }

      return Math.min(prev + COMPLETED_BATCH_SIZE, appointmentsByStatus.past.length);
    });
  }, [appointmentsByStatus.past.length]);

  const renderAppointment = (appointment: Appointment) => {
    const exercise = exercises.find((item) => item.id === appointment.exerciseId);
    const minutes = Math.floor(appointment.durationSeconds / 60)
      .toString()
      .padStart(2, '0');
    const seconds = (appointment.durationSeconds % 60).toString().padStart(2, '0');

    return (
      <li key={appointment.id} className="appointment-card">
        <header>
          <span className="appointment-title">
            {exercise ? exercise.name : 'Упражнение'} · {appointment.perDay} раз в день
          </span>
          <button
            type="button"
            className="link-button"
            onClick={() => handleDeleteAppointment(appointment.id)}
          >
            Удалить
          </button>
        </header>
        <p className="appointment-period">
          {appointment.start} — {appointment.end}
        </p>
        <p className="appointment-meta">
          Длительность: {minutes}:{seconds} · Выполнено: {appointment.totalCompleted} ·
          Прогресс: {appointment.donePercent}%
        </p>
      </li>
    );
  };

  const createDatasource = useCallback(
    (tableName: string): IDatasource => ({
      getRows: async (params: IGetRowsParams) => {
        try {
          setTableError(null);
          const requested = Math.max(params.endRow - params.startRow, 1);
          const response = await fetch(
            `/api/database/tables/${encodeURIComponent(tableName)}?offset=${params.startRow}&limit=${requested}`,
          );
          if (!response.ok) {
            throw new Error('Failed to load table data');
          }
          const data: TableDataResponse = await response.json();
          const newColumnDefs = (data.columns ?? []).map((column) => ({
            field: column,
            headerName: column,
          }));
          setDatabaseColumns((previous) => {
            const previousKey = previous.map((item) => String(item.field ?? '')).join('|');
            const nextKey = newColumnDefs.map((item) => String(item.field ?? '')).join('|');
            if (previousKey === nextKey) {
              return previous;
            }
            return newColumnDefs;
          });
          const rows = Array.isArray(data.rows) ? data.rows : [];
          const totalRows = Number.isFinite(data.totalRows) ? data.totalRows : rows.length;
          params.successCallback(rows, totalRows);
        } catch (error) {
          console.error(error);
          setTableError('Не удалось загрузить данные таблицы. Попробуйте позже.');
          params.failCallback();
        }
      },
    }),
    [setDatabaseColumns, setTableError],
  );

  useEffect(() => {
    setTableError(null);
    if (!gridApiRef.current) {
      return;
    }
    if (!selectedTable) {
      setDatabaseColumns([]);
      gridApiRef.current.setGridOption('datasource', emptyDataSource);
      return;
    }
    setDatabaseColumns([]);
    gridApiRef.current.setGridOption('datasource', createDatasource(selectedTable));
  }, [selectedTable, emptyDataSource, createDatasource]);

  const handleGridReady = useCallback(
    (event: GridReadyEvent) => {
      gridApiRef.current = event.api;
      event.api.setGridOption(
        'datasource',
        selectedTable ? createDatasource(selectedTable) : emptyDataSource,
      );
    },
    [emptyDataSource, selectedTable, createDatasource],
  );

  const handleOpenDatabase = () => {
    setViewMode('database');
    setTablesLoadError(null);
    setTableError(null);
    if (databaseTables.length === 0 && !isLoadingTables) {
      void loadTables();
    }
  };

  const handleBackToDashboard = () => {
    setViewMode('dashboard');
  };

  const handleTableSelect = (event: ChangeEvent<HTMLSelectElement>) => {
    setSelectedTable(event.target.value);
  };

  const handleRefreshTable = () => {
    if (!selectedTable || !gridApiRef.current) {
      return;
    }
    gridApiRef.current.refreshInfiniteCache();
  };

  if (viewMode === 'database') {
    return (
      <div className="app page doctor-page">
        <header className="page-header doctor-header">
          <div className="doctor-header-top">
            <div className="doctor-heading" role="presentation">
              <div className="doctor-heading-row">
                <h1>База данных</h1>
                <div className="doctor-actions horizontal">
                  <button type="button" className="secondary-button" onClick={handleBackToDashboard}>
                    Вернуться в кабинет
                  </button>
                  <button type="button" className="secondary-button" onClick={onLogout}>
                    Выйти из кабинета
                  </button>
                </div>
              </div>
              <p className="lead">
                Просматривайте содержимое таблиц и анализируйте данные в режиме реального времени.
              </p>
            </div>
          </div>
        </header>

        <main className="doctor-layout database-layout">
          <section className="doctor-column wide">
            <div className="panel database-panel">
              <div className="database-toolbar">
                <label className="form-label database-label">
                  Таблица
                  <select
                    className="database-select"
                    value={selectedTable}
                    onChange={handleTableSelect}
                    disabled={isLoadingTables || databaseTables.length === 0}
                  >
                    <option value="">Выберите таблицу</option>
                    {databaseTables.map((table) => (
                      <option key={table} value={table}>
                        {table}
                      </option>
                    ))}
                  </select>
                </label>
                <button
                  type="button"
                  className="secondary-button"
                  onClick={handleRefreshTable}
                  disabled={!selectedTable}
                >
                  Обновить
                </button>
              </div>
              {isLoadingTables && <p className="muted">Загружаем список таблиц...</p>}
              {tablesLoadError && (
                <p className="form-message" role="alert">
                  {tablesLoadError}
                </p>
              )}
              {tableError && (
                <p className="form-message" role="alert">
                  {tableError}
                </p>
              )}
              {!selectedTable && !isLoadingTables && !tablesLoadError && (
                <p className="muted">Выберите таблицу, чтобы загрузить данные.</p>
              )}
              <div className="database-grid-wrapper">
                <div className="database-grid">
                  <AgGridReact
                    columnDefs={databaseColumns}
                    defaultColDef={databaseDefaultColDef}
                    rowModelType="infinite"
                    cacheBlockSize={100}
                    maxBlocksInCache={4}
                    onGridReady={handleGridReady}
                    theme={themeQuartz}
                    suppressCellFocus
                    animateRows
                  />
                </div>
              </div>
            </div>
          </section>
        </main>
      </div>
    );
  }

  return (
    <div className="app page doctor-page">
      <header className="page-header doctor-header">
        <div className="doctor-header-top">
          <div className="doctor-heading" role="presentation">
            <div className="doctor-heading-row">
              <h1>Кабинет врача</h1>
              <div className="doctor-stat-cards" role="list">
                <article className="stat-card" role="listitem">
                  <span className="stat-label">Пациентов в базе</span>
                  <span className="stat-value">{stats.total}</span>
                </article>
                <article className="stat-card" role="listitem">
                  <span className="stat-label">Активные назначения</span>
                  <span className="stat-value">{stats.active}</span>
                </article>
                <article className="stat-card warning" role="listitem">
                  <span className="stat-label">Низкий прогресс</span>
                  <span className="stat-value">{stats.atRisk}</span>
                </article>
              </div>
              <div className="doctor-actions">
                <button type="button" className="secondary-button doctor-logout" onClick={onLogout}>
                  Выйти из кабинета
                </button>
                <button type="button" className="secondary-button" onClick={handleOpenDatabase}>
                  База данных
                </button>
              </div>
            </div>
            <p className="lead">
              Управляйте пациентами, назначайте упражнения и следите за прогрессом прямо из браузера.
            </p>
          </div>
        </div>
        {isLoading && <p className="muted">Загружаем данные кабинета...</p>}
        {loadError && (
          <p className="form-message" role="alert">
            {loadError}
          </p>
        )}
      </header>

      <main className="doctor-layout">
        <section className="doctor-column" aria-labelledby="new-patient-heading">
          <div className="panel">
            <header className="panel-header">
              <h2 id="new-patient-heading">Добавить пациента</h2>
              <p className="panel-description">
                Создайте карточку пациента и передайте ему учётные данные. Можно сгенерировать их
                автоматически.
              </p>
            </header>
            <form className="form" onSubmit={handleAddPatient}>
              <div className="form-row">
                <label className="form-label">
                  Фамилия*
                  <input
                    type="text"
                    required
                    value={newPatient.lastName}
                    onChange={(event) => handlePatientFieldChange('lastName', event)}
                  />
                </label>
                <label className="form-label">
                  Имя*
                  <input
                    type="text"
                    required
                    value={newPatient.firstName}
                    onChange={(event) => handlePatientFieldChange('firstName', event)}
                  />
                </label>
                <label className="form-label">
                  Отчество
                  <input
                    type="text"
                    value={newPatient.middleName}
                    onChange={(event) => handlePatientFieldChange('middleName', event)}
                  />
                </label>
              </div>

              <label className="form-label">
                Дата рождения*
                <input
                  type="date"
                  required
                  value={newPatient.birthDate}
                  onChange={(event) => handlePatientFieldChange('birthDate', event)}
                />
              </label>

              <label className="form-checkbox">
                <input
                  type="checkbox"
                  checked={autoCredentials}
                  onChange={(event) => setAutoCredentials(event.target.checked)}
                />
                Генерировать логин и пароль автоматически
              </label>

              {!autoCredentials && (
                <div className="form-row">
                  <label className="form-label">
                    Логин*
                    <input
                      type="text"
                      required
                      value={newPatient.username}
                      onChange={(event) => handlePatientFieldChange('username', event)}
                    />
                  </label>
                  <label className="form-label">
                    Пароль*
                    <input
                      type="text"
                      required
                      value={newPatient.password}
                      onChange={(event) => handlePatientFieldChange('password', event)}
                    />
                  </label>
                </div>
              )}

              {formMessage && <p className="form-message">{formMessage}</p>}

              <button type="submit" className="primary-button">
                Сохранить пациента
              </button>
            </form>
          </div>

          <div className="panel" aria-labelledby="active-patients-heading">
            <header className="panel-header">
              <h2 id="active-patients-heading">Активные пациенты</h2>
              <p className="panel-description">
                Сейчас выполняют программу:{' '}
                {isLoading ? 'загрузка…' : activePatients.length || 'нет данных'}
              </p>
            </header>
            <ul className="patient-list">
              {isLoading ? (
                <li className="patient-empty">Загрузка данных…</li>
              ) : (
                <>
                  {activePatients.map((patient) => (
                    <li key={patient.id} className="patient-item">
                      <button
                        type="button"
                        className={`patient-button ${
                          selectedPatientId === patient.id ? 'is-active' : ''
                        }`}
                        onClick={() => setSelectedPatientId(patient.id)}
                      >
                        {patient.lastName} {patient.firstName}
                      </button>
                    </li>
                  ))}
                  {activePatients.length === 0 && (
                    <li className="patient-empty">Нет активных назначений</li>
                  )}
                </>
              )}
            </ul>
          </div>
        </section>

        <section className="doctor-column" aria-labelledby="patient-list-heading">
          <div className="panel">
            <header className="panel-header">
              <h2 id="patient-list-heading">Список пациентов</h2>
              <div className="patient-search">
                <input
                  type="search"
                  value={patientSearch}
                  onChange={(event) => setPatientSearch(event.target.value)}
                  placeholder="Поиск по фамилии и имени"
                  aria-label="Поиск пациента по фамилии и имени"
                  className="patient-search-input"
                />
              </div>
              <p className="panel-description">Выберите пациента для просмотра подробностей.</p>
            </header>
            <ul className="patient-list">
              {isLoading ? (
                <li className="patient-empty">Загрузка данных…</li>
              ) : (
                <>
                  {displayedPatients.map((patient) => (
                    <li key={patient.id} className="patient-item">
                      <button
                        type="button"
                        className={`patient-button ${
                          selectedPatientId === patient.id ? 'is-active' : ''
                        }`}
                        onClick={() => setSelectedPatientId(patient.id)}
                      >
                        <span>{patient.lastName} {patient.firstName}</span>
                        <span className="patient-username">{patient.username}</span>
                      </button>
                      <button
                        type="button"
                        className="link-button"
                        onClick={() => handleDeletePatient(patient.id)}
                      >
                        Удалить
                      </button>
                    </li>
                  ))}
                  {patients.length === 0 ? (
                    <li className="patient-empty">Пациентов пока нет</li>
                  ) : (
                    displayedPatients.length === 0 && (
                      <li className="patient-empty">Ничего не найдено</li>
                    )
                  )}
                </>
              )}
            </ul>
          </div>
        </section>

        <section className="doctor-column wide" aria-live="polite">
          {isLoading ? (
            <div className="panel">
              <header className="panel-header">
                <h2>Загружаем данные…</h2>
                <p className="panel-description">
                  Подождите, пока мы получим информацию о пациентах и назначениях.
                </p>
              </header>
            </div>
          ) : selectedPatient ? (
            <div className="panel patient-details">
              <header className="panel-header">
                <h2>
                  {selectedPatient.lastName} {selectedPatient.firstName}{' '}
                  {selectedPatient.middleName}
                </h2>
                <p className="panel-description">
                  Возраст: {(() => {
                    const age = getAge(selectedPatient.birthDate);
                    return `${age.years} лет ${age.months} мес.`;
                  })()}
                </p>
              </header>

              <div className="credentials">
                <div>
                  <span className="credentials-label">Логин</span>
                  <span className="credentials-value">{selectedPatient.username}</span>
                </div>
                <div>
                  <span className="credentials-label">Пароль</span>
                  <span className="credentials-value">{selectedPatient.password}</span>
                </div>
              </div>

              <div className="patient-section">
                <h3>Патологии</h3>
                <div className="disorder-groups">
                  {Object.entries(groupedDisorders).map(([category, items]) => (
                    <article className="disorder-group" key={category}>
                      <header>
                        <h4>{category}</h4>
                      </header>
                      <ul>
                        {items.map((disorder) => (
                          <li key={disorder.id}>
                            <label className="form-checkbox">
                              <input
                                type="checkbox"
                                checked={selectedPatient.disorders.includes(disorder.id)}
                                onChange={() => toggleDisorder(disorder.id)}
                              />
                              {disorder.name}
                            </label>
                          </li>
                        ))}
                      </ul>
                    </article>
                  ))}
                </div>
              </div>

              <div className="patient-section">
                <h3>Назначить упражнения</h3>
                <form className="form" onSubmit={handleAssignExercises}>
                  <div className="exercise-toolbar">
                    <label className="form-checkbox">
                      <input
                        type="checkbox"
                        checked={showOnlyRecommended}
                        onChange={(event) => setShowOnlyRecommended(event.target.checked)}
                      />
                      Показывать упражнения по патологиям пациента
                    </label>
                    <span className="exercise-counter">
                      Выбрано: {selectedExercises.length}
                    </span>
                  </div>

                  <div className="exercise-groups">
                    {Object.entries(categorizedExercises).map(([group, items]) => (
                      <article key={group} className="exercise-group">
                        <header>
                          <h4>{group}</h4>
                        </header>
                        <ul>
                          {items.map((exercise) => (
                            <li key={exercise.id}>
                              <label className="form-checkbox">
                                <input
                                  type="checkbox"
                                  checked={selectedExercises.includes(exercise.id)}
                                  onChange={(event) =>
                                    handleExerciseSelection(exercise.id, event.target.checked)
                                  }
                                />
                                {exercise.name}
                              </label>
                            </li>
                          ))}
                        </ul>
                      </article>
                    ))}
                    {Object.keys(categorizedExercises).length === 0 && (
                      <p className="muted">Нет подходящих упражнений. Добавьте патологии пациенту.</p>
                    )}
                  </div>

                  <div className="form-row">
                    <label className="form-label">
                      Дата начала
                      <input
                        type="date"
                        value={exerciseForm.start}
                        onChange={(event) => updateExerciseForm('start', event)}
                      />
                    </label>
                    <label className="form-label">
                      Дата окончания
                      <input
                        type="date"
                        value={exerciseForm.end}
                        onChange={(event) => updateExerciseForm('end', event)}
                      />
                    </label>
                    <label className="form-label">
                      Раз в день
                      <input
                        type="number"
                        min="1"
                        value={exerciseForm.perDay}
                        onChange={(event) => updateExerciseForm('perDay', event)}
                      />
                    </label>
                  </div>

                  <div className="form-row">
                    <label className="form-label">
                      Минуты
                      <input
                        type="text"
                        inputMode="numeric"
                        value={exerciseForm.minutes}
                        onChange={(event) => updateExerciseForm('minutes', event)}
                      />
                    </label>
                    <label className="form-label">
                      Секунды
                      <input
                        type="text"
                        inputMode="numeric"
                        value={exerciseForm.seconds}
                        onChange={(event) => updateExerciseForm('seconds', event)}
                      />
                    </label>
                  </div>

                  <button type="submit" className="primary-button">
                    Назначить упражнения
                  </button>
                </form>
              </div>

              <div className="patient-section">
                <h3>Назначения</h3>
                <div className="appointment-columns">
                  <section>
                    <h4>Текущие</h4>
                    <ul className="appointment-list">
                      {appointmentsByStatus.current.map((appointment) =>
                        renderAppointment(appointment),
                      )}
                      {appointmentsByStatus.current.length === 0 && (
                        <li className="muted">Сейчас назначений нет.</li>
                      )}
                    </ul>
                  </section>
                  <section>
                    <h4>Предстоящие</h4>
                    <ul className="appointment-list">
                      {appointmentsByStatus.future.map((appointment) =>
                        renderAppointment(appointment),
                      )}
                      {appointmentsByStatus.future.length === 0 && (
                        <li className="muted">Пока нет запланированных упражнений.</li>
                      )}
                    </ul>
                  </section>
                  <section>
                    <h4>Завершённые</h4>
                    <ul
                      className="appointment-list completed-appointment-list"
                      onScroll={handleCompletedScroll}
                      ref={completedListRef}
                    >
                      {visibleCompletedAppointments.map((appointment) => renderAppointment(appointment))}
                      {appointmentsByStatus.past.length === 0 && (
                        <li className="muted">Нет завершённых назначений.</li>
                      )}
                      {appointmentsByStatus.past.length > visibleCompletedAppointments.length && (
                        <li className="muted" role="status">
                          Прокрутите вниз, чтобы посмотреть остальные назначения…
                        </li>
                      )}
                    </ul>
                  </section>
                </div>
              </div>
            </div>
          ) : (
            <div className="panel">
              <header className="panel-header">
                <h2>Выберите пациента</h2>
                <p className="panel-description">
                  Чтобы увидеть детали, выберите пациента из списка слева или добавьте нового.
                </p>
              </header>
            </div>
          )}
        </section>
      </main>
    </div>
  );
};

export default DoctorDashboard;
