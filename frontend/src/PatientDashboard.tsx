import { useEffect, useMemo, useRef, useState } from 'react';

export type PatientSessionProfile = {
  id: number;
  firstName: string;
  lastName: string;
  middleName?: string | null;
  username: string;
};

type ExerciseStatus = 'pending' | 'in_progress' | 'completed';

type PatientAppointment = {
  id: string;
  exerciseId: number;
  start: string;
  end: string;
  perDay: number;
  totalCompleted: number;
  donePercent: number;
  durationSeconds: number;
  videoUrl: string | null;
  videoName: string | null;
  mimeType: string | null;
  status: ExerciseStatus;
};

type PatientExercise = {
  id: number;
  name: string | null;
  type: string | null;
  filename: string | null;
  bodyPart: string | null;
  typeOfActivity: string | null;
  filePath: string | null;
  mimeType: string | null;
  videoUrl: string | null;
  donePercent: number;
  status: ExerciseStatus;
};

type PatientDashboardProps = {
  token: string;
  patient: PatientSessionProfile;
  onLogout: () => void;
};

const statusLabels: Record<ExerciseStatus, string> = {
  pending: 'Не начато',
  in_progress: 'В процессе',
  completed: 'Завершено',
};

const parseErrorResponse = async (response: Response): Promise<string> => {
  const contentType = response.headers.get('content-type') ?? '';

  if (contentType.includes('application/json')) {
    try {
      const data = await response.json();
      const detail = typeof data?.detail === 'string' ? data.detail.trim() : '';
      if (detail) {
        return detail;
      }
    } catch (error) {
      console.error('Не удалось разобрать ответ сервера', error);
    }
  }

  try {
    const text = (await response.text()).trim();
    if (text) {
      return text;
    }
  } catch (error) {
    console.error('Не удалось получить текст ответа', error);
  }

  return 'Не удалось загрузить данные. Попробуйте обновить страницу.';
};

const formatDate = (value: string) => {
  if (!value) {
    return '—';
  }
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) {
    return value;
  }
  return new Intl.DateTimeFormat('ru-RU').format(parsed);
};

const formatDuration = (seconds: number) => {
  if (!Number.isFinite(seconds) || seconds <= 0) {
    return '—';
  }
  const minutes = Math.round(seconds / 60);
  if (minutes < 60) {
    return `${minutes} мин.`;
  }
  const hours = Math.floor(minutes / 60);
  const restMinutes = minutes % 60;
  return restMinutes ? `${hours} ч. ${restMinutes} мин.` : `${hours} ч.`;
};

const PatientDashboard = ({ token, patient, onLogout }: PatientDashboardProps) => {
  const [appointments, setAppointments] = useState<PatientAppointment[]>([]);
  const [exercises, setExercises] = useState<PatientExercise[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [reloadCounter, setReloadCounter] = useState(0);
  const [selectedAppointmentIndex, setSelectedAppointmentIndex] = useState(0);
  const videoRef = useRef<HTMLVideoElement | null>(null);

  useEffect(() => {
    let isActive = true;
    const controller = new AbortController();

    const loadData = async () => {
      setLoading(true);
      setError('');

      try {
        const [appointmentsResponse, exercisesResponse] = await Promise.all([
          fetch('/api/patients/me/appointments', {
            headers: {
              Authorization: `Bearer ${token}`,
            },
            signal: controller.signal,
          }),
          fetch('/api/patients/me/exercises', {
            headers: {
              Authorization: `Bearer ${token}`,
            },
            signal: controller.signal,
          }),
        ]);

        if (!appointmentsResponse.ok) {
          throw new Error(await parseErrorResponse(appointmentsResponse));
        }
        if (!exercisesResponse.ok) {
          throw new Error(await parseErrorResponse(exercisesResponse));
        }

        const [appointmentsData, exercisesData] = await Promise.all([
          appointmentsResponse.json() as Promise<PatientAppointment[]>,
          exercisesResponse.json() as Promise<PatientExercise[]>,
        ]);

        if (!isActive) {
          return;
        }

        setAppointments(appointmentsData);
        setExercises(exercisesData);
        setSelectedAppointmentIndex(0);
      } catch (error) {
        if (!isActive) {
          return;
        }
        if ((error as DOMException).name === 'AbortError') {
          return;
        }
        console.error(error);
        setError(
          error instanceof Error
            ? error.message
            : 'Не удалось загрузить данные. Попробуйте позже.',
        );
      } finally {
        if (isActive) {
          setLoading(false);
        }
      }
    };

    void loadData();

    return () => {
      isActive = false;
      controller.abort();
    };
  }, [token, reloadCounter]);

  const completedAppointments = useMemo(
    () => appointments.filter((item) => item.status === 'completed').length,
    [appointments],
  );

  const totalProgress = useMemo(() => {
    if (!appointments.length) {
      return 0;
    }
    const totalPercent = appointments.reduce((acc, item) => acc + item.donePercent, 0);
    return Math.round(totalPercent / appointments.length);
  }, [appointments]);

  const handleRefresh = () => {
    setReloadCounter((value) => value + 1);
  };

  const handleSelectAppointment = (index: number) => {
    setSelectedAppointmentIndex(index);
  };

  const handlePrevAppointment = () => {
    setSelectedAppointmentIndex((current) => Math.max(0, current - 1));
  };

  const handleNextAppointment = () => {
    setSelectedAppointmentIndex((current) =>
      Math.min(appointments.length - 1, current + 1),
    );
  };

  const selectedAppointment = appointments[selectedAppointmentIndex];

  useEffect(() => {
    if (videoRef.current) {
      videoRef.current.load();
    }
  }, [selectedAppointment?.id, selectedAppointment?.videoUrl, selectedAppointment?.mimeType]);

  useEffect(() => {
    if (!appointments.length) {
      if (selectedAppointmentIndex !== 0) {
        setSelectedAppointmentIndex(0);
      }
      return;
    }

    if (selectedAppointmentIndex >= appointments.length) {
      setSelectedAppointmentIndex(appointments.length - 1);
    }
  }, [appointments, selectedAppointmentIndex]);

  const patientFullName = useMemo(() => {
    const parts = [patient.lastName, patient.firstName];
    if (patient.middleName) {
      parts.push(patient.middleName);
    }
    return parts.filter(Boolean).join(' ');
  }, [patient.firstName, patient.lastName, patient.middleName]);

  return (
    <div className="app page patient-dashboard">
      <header className="page-header">
        <div>
          <p className="badge">Личный кабинет</p>
          <h1>{patientFullName}</h1>
          <p className="lead">Логин: {patient.username}</p>
        </div>
        <div className="patient-dashboard__actions">
          <button type="button" className="secondary-button" onClick={handleRefresh} disabled={loading}>
            Обновить
          </button>
          <button type="button" className="secondary-button" onClick={onLogout}>
            Выйти
          </button>
        </div>
      </header>

      <main className="page-body">
        {loading && <p>Загружаем данные...</p>}
        {error && !loading && <p className="error">{error}</p>}

        {!loading && !error && (
          <>
            <section className="section">
              <h2>Ваш прогресс</h2>
              {appointments.length ? (
                <div className="patient-dashboard__summary-grid">
                  <div className="patient-dashboard__summary-card">
                    <span className="patient-dashboard__summary-title">Назначений</span>
                    <span className="patient-dashboard__summary-value">{appointments.length}</span>
                  </div>
                  <div className="patient-dashboard__summary-card">
                    <span className="patient-dashboard__summary-title">Завершено</span>
                    <span className="patient-dashboard__summary-value">{completedAppointments}</span>
                  </div>
                  <div className="patient-dashboard__summary-card">
                    <span className="patient-dashboard__summary-title">Средний прогресс</span>
                    <span className="patient-dashboard__summary-value">{totalProgress}%</span>
                  </div>
                </div>
              ) : (
                <p>У вас пока нет активных назначений.</p>
              )}
            </section>

            <section className="section">
              <h2>Назначения</h2>
              {appointments.length === 0 ? (
                <p>
                  Назначения отсутствуют. Как только врач добавит новые упражнения, они появятся здесь.
                </p>
              ) : (
                <div className="patient-dashboard__player">
                  <div className="patient-dashboard__player-main">
                    {selectedAppointment ? (
                      <>
                        <header className="patient-dashboard__player-header">
                          <div>
                            <h3>
                              {selectedAppointment.videoName ?? `Упражнение №${selectedAppointment.exerciseId}`}
                            </h3>
                            <p className="patient-dashboard__muted">
                              {formatDate(selectedAppointment.start)} — {formatDate(selectedAppointment.end)} •{' '}
                              {selectedAppointment.perDay} раз(а) в день
                            </p>
                          </div>
                          <span
                            className={`patient-status patient-status--${selectedAppointment.status}`}
                          >
                            {statusLabels[selectedAppointment.status]}
                          </span>
                        </header>
                        <div className="patient-dashboard__player-media">
                          {selectedAppointment.videoUrl ? (
                            <video
                              key={selectedAppointment.id}
                              ref={videoRef}
                              controls
                              preload="metadata"
                              width="100%"
                            >
                              <source
                                src={selectedAppointment.videoUrl}
                                type={selectedAppointment.mimeType ?? undefined}
                              />
                              Ваш браузер не поддерживает воспроизведение этого видео.
                            </video>
                          ) : (
                            <div className="patient-dashboard__player-placeholder">
                              <p>Видео недоступно.</p>
                            </div>
                          )}
                        </div>
                        <footer className="patient-dashboard__player-footer">
                          <div>
                            <strong>Прогресс:</strong> {selectedAppointment.donePercent}%
                          </div>
                          <div>
                            <strong>Выполнено:</strong> {selectedAppointment.totalCompleted} раз(а)
                          </div>
                          <div>
                            <strong>Длительность:</strong> {formatDuration(selectedAppointment.durationSeconds)}
                          </div>
                        </footer>
                        <div className="patient-dashboard__player-controls">
                          <button
                            type="button"
                            className="secondary-button"
                            onClick={handlePrevAppointment}
                            disabled={selectedAppointmentIndex === 0}
                          >
                            Предыдущее
                          </button>
                          <span>
                            {selectedAppointmentIndex + 1} из {appointments.length}
                          </span>
                          <button
                            type="button"
                            className="secondary-button"
                            onClick={handleNextAppointment}
                            disabled={selectedAppointmentIndex === appointments.length - 1}
                          >
                            Следующее
                          </button>
                        </div>
                      </>
                    ) : (
                      <p>Выберите назначение из списка справа.</p>
                    )}
                  </div>

                  <aside className="patient-dashboard__playlist">
                    <h3>Плейлист</h3>
                    <ul>
                      {appointments.map((appointment, index) => {
                        const isActive = index === selectedAppointmentIndex;
                        return (
                          <li key={appointment.id}>
                            <button
                              type="button"
                              className={`patient-dashboard__playlist-item${
                                isActive ? ' patient-dashboard__playlist-item--active' : ''
                              }`}
                              onClick={() => handleSelectAppointment(index)}
                            >
                              <span className="patient-dashboard__playlist-title">
                                {appointment.videoName ?? `Упражнение №${appointment.exerciseId}`}
                              </span>
                              <span className="patient-dashboard__playlist-meta">
                                {appointment.donePercent}% • {statusLabels[appointment.status]}
                              </span>
                            </button>
                          </li>
                        );
                      })}
                    </ul>
                  </aside>
                </div>
              )}
            </section>

            <section className="section">
              <h2>Все упражнения</h2>
              {exercises.length === 0 ? (
                <p>Список упражнений пока пуст.</p>
              ) : (
                <ul className="patient-dashboard__card-list patient-dashboard__card-list--compact">
                  {exercises.map((exercise) => (
                    <li key={exercise.id} className="patient-dashboard__card patient-dashboard__card--compact">
                      <header className="patient-dashboard__card-header">
                        <div>
                          <h3>{exercise.name ?? `Упражнение №${exercise.id}`}</h3>
                          <p className="patient-dashboard__muted">{exercise.type ?? 'Тип не указан'}</p>
                        </div>
                        <span className={`patient-status patient-status--${exercise.status}`}>
                          {statusLabels[exercise.status]}
                        </span>
                      </header>
                      <footer className="patient-dashboard__card-footer">
                        <div>
                          <strong>Прогресс:</strong> {exercise.donePercent}%
                        </div>
                        {exercise.bodyPart && (
                          <div>
                            <strong>Область:</strong> {exercise.bodyPart}
                          </div>
                        )}
                      </footer>
                    </li>
                  ))}
                </ul>
              )}
            </section>
          </>
        )}
      </main>
    </div>
  );
};

export default PatientDashboard;
