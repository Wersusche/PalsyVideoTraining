import { ChangeEvent, FormEvent, useEffect, useMemo, useState } from 'react';

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

const disorders: Disorder[] = [
  { id: 1, name: 'Спастичность верхних конечностей', category: 'Двигательные нарушения' },
  { id: 2, name: 'Спастичность нижних конечностей', category: 'Двигательные нарушения' },
  { id: 3, name: 'Нарушения осанки', category: 'Двигательные нарушения' },
  { id: 4, name: 'Сложности с речью', category: 'Коммуникативные нарушения' },
  { id: 5, name: 'Нарушения глотания', category: 'Коммуникативные нарушения' },
  { id: 6, name: 'Сенсорные перегрузки', category: 'Сенсорные нарушения' },
  { id: 7, name: 'Проблемы с координацией', category: 'Двигательные нарушения' },
  { id: 8, name: 'Проблемы с вниманием', category: 'Когнитивные нарушения' },
];

const exercises: Exercise[] = [
  { id: 101, name: 'Растяжка кистей', type: 'Разминка' },
  { id: 102, name: 'Растяжка стоп', type: 'Разминка' },
  { id: 103, name: 'Упражнение на осанку «Ласточка»', type: 'Сила и контроль' },
  { id: 104, name: 'Видео с артикуляционной гимнастикой', type: 'Речь' },
  { id: 105, name: 'Упражнение на глотание «Глоток воды»', type: 'Речь' },
  { id: 106, name: 'Десенсибилизация кистей', type: 'Сенсорика' },
  { id: 107, name: 'Баланс на платформе', type: 'Координация' },
  { id: 108, name: 'Игровая тренировка на внимание', type: 'Когнитивные' },
];

const disorderExerciseMap: Record<number, number[]> = {
  1: [101, 107],
  2: [102, 107],
  3: [103],
  4: [104],
  5: [105],
  6: [106],
  7: [103, 107],
  8: [108],
};

const initialPatients: Patient[] = [
  {
    id: 1,
    firstName: 'Алексей',
    lastName: 'Иванов',
    middleName: 'Сергеевич',
    birthDate: '2010-02-14',
    username: 'alexei.i',
    password: 'rehab123',
    disorders: [1, 3, 7],
    appointments: [
      {
        id: 'app-1',
        exerciseId: 101,
        start: new Date().toISOString().slice(0, 10),
        end: new Date(Date.now() + 7 * 24 * 60 * 60 * 1000).toISOString().slice(0, 10),
        perDay: 3,
        totalCompleted: 9,
        donePercent: 45,
        durationSeconds: 8 * 60,
      },
      {
        id: 'app-2',
        exerciseId: 103,
        start: new Date(Date.now() - 10 * 24 * 60 * 60 * 1000).toISOString().slice(0, 10),
        end: new Date(Date.now() - 2 * 24 * 60 * 60 * 1000).toISOString().slice(0, 10),
        perDay: 2,
        totalCompleted: 16,
        donePercent: 80,
        durationSeconds: 10 * 60,
      },
    ],
  },
  {
    id: 2,
    firstName: 'Мария',
    lastName: 'Петрова',
    middleName: 'Ильинична',
    birthDate: '2012-07-03',
    username: 'maria.p',
    password: 'palsy2024',
    disorders: [4, 5, 8],
    appointments: [
      {
        id: 'app-3',
        exerciseId: 104,
        start: new Date(Date.now() + 2 * 24 * 60 * 60 * 1000).toISOString().slice(0, 10),
        end: new Date(Date.now() + 9 * 24 * 60 * 60 * 1000).toISOString().slice(0, 10),
        perDay: 1,
        totalCompleted: 0,
        donePercent: 0,
        durationSeconds: 6 * 60,
      },
    ],
  },
];

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
  const now = new Date();

  const totalMonths =
    (now.getFullYear() - birth.getFullYear()) * 12 + (now.getMonth() - birth.getMonth());
  const years = Math.floor(totalMonths / 12);
  const months = totalMonths - years * 12;

  return { years, months: Math.max(0, months) };
};

const DoctorDashboard = ({ onLogout }: DoctorDashboardProps) => {
  const [patients, setPatients] = useState<Patient[]>(initialPatients);
  const [selectedPatientId, setSelectedPatientId] = useState<number | null>(
    initialPatients[0]?.id ?? null,
  );
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

  useEffect(() => {
    setSelectedExercises([]);
  }, [selectedPatientId]);

  const selectedPatient = patients.find((patient) => patient.id === selectedPatientId) ?? null;

  const groupedDisorders = useMemo(() => {
    return disorders.reduce<Record<string, Disorder[]>>((acc, item) => {
      if (!acc[item.category]) {
        acc[item.category] = [];
      }
      acc[item.category].push(item);
      return acc;
    }, {});
  }, []);

  const recommendedExercises = useMemo(() => {
    if (!selectedPatient) {
      return new Set<number>();
    }

    return new Set(
      selectedPatient.disorders.flatMap((disorderId) => disorderExerciseMap[disorderId] ?? []),
    );
  }, [selectedPatient]);

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
  }, [recommendedExercises, selectedPatient, showOnlyRecommended]);

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

  const handleAddPatient = (event: FormEvent<HTMLFormElement>) => {
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
      ? generateRandomPassword()
      : newPatient.password || generateRandomPassword();

    const createdPatient: Patient = {
      id: Date.now(),
      firstName: newPatient.firstName.trim(),
      lastName: newPatient.lastName.trim(),
      middleName: newPatient.middleName.trim(),
      birthDate: newPatient.birthDate,
      username,
      password,
      disorders: [],
      appointments: [],
    };

    setPatients((prev) => [...prev, createdPatient]);
    setSelectedPatientId(createdPatient.id);
    resetForm();
    setFormMessage('Пациент добавлен. Передайте ему логин и пароль.');
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

  const handleDeletePatient = (patientId: number) => {
    const patient = patients.find((item) => item.id === patientId);
    if (!patient) return;

    const confirmed = window.confirm(
      `Удалить пациента ${patient.lastName} ${patient.firstName}? Все назначения будут потеряны.`,
    );

    if (!confirmed) {
      return;
    }

    setPatients((prev) => {
      const updated = prev.filter((item) => item.id !== patientId);
      if (selectedPatientId === patientId) {
        setSelectedPatientId(updated[0]?.id ?? null);
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

  return (
    <div className="app page doctor-page">
      <header className="page-header">
        <h1>Кабинет врача</h1>
        <p className="lead">
          Управляйте пациентами, назначайте упражнения и следите за прогрессом прямо из браузера.
        </p>
        <div className="doctor-actions">
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
          <button type="button" className="secondary-button" onClick={onLogout}>
            Выйти из кабинета
          </button>
        </div>
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
                Сейчас выполняют программу: {activePatients.length || 'нет данных'}
              </p>
            </header>
            <ul className="patient-list">
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
              {activePatients.length === 0 && <li className="patient-empty">Нет активных назначений</li>}
            </ul>
          </div>
        </section>

        <section className="doctor-column" aria-labelledby="patient-list-heading">
          <div className="panel">
            <header className="panel-header">
              <h2 id="patient-list-heading">Список пациентов</h2>
              <p className="panel-description">Выберите пациента для просмотра подробностей.</p>
            </header>
            <ul className="patient-list">
              {patients.map((patient) => (
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
              {patients.length === 0 && <li className="patient-empty">Пациентов пока нет</li>}
            </ul>
          </div>
        </section>

        <section className="doctor-column wide" aria-live="polite">
          {selectedPatient ? (
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
                    <ul className="appointment-list">
                      {appointmentsByStatus.past.map((appointment) => renderAppointment(appointment))}
                      {appointmentsByStatus.past.length === 0 && (
                        <li className="muted">Нет завершённых назначений.</li>
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
