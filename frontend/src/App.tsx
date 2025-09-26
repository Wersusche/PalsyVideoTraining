import { FormEvent, useMemo, useState } from 'react';
import DoctorDashboard from './DoctorDashboard';
import PatientDashboard, { PatientSessionProfile } from './PatientDashboard';

type Role = 'doctor' | 'patient';

type RoleContent = {
  title: string;
  description: string;
  redirectMessage: string;
  demoCredentials?: {
    login: string;
    password: string;
  };
};

const roleContent: Record<Role, RoleContent> = {
  doctor: {
    title: 'Вход для врачей',
    description:
      'Получите доступ к инструментам назначения упражнений и отслеживания прогресса пациентов.',
    redirectMessage: 'После авторизации вы перейдёте в кабинет врача.',
  },
  patient: {
    title: 'Вход для пациентов',
    description: 'Откройте персональный план тренировок и рекомендации от специалистов.',
    redirectMessage: 'После авторизации вы перейдёте в личный кабинет пациента.',
    demoCredentials: {
      login: 'user',
      password: '123',
    },
  },
};

const App = () => {
  const roadmapItems = useMemo(
    () => [
      { title: 'MVP платформы', description: 'Базовый просмотр контента и личный кабинет.' },
      { title: 'Интерактивные упражнения', description: 'Добавление обратной связи и подсказок в реальном времени.' },
      { title: 'Аналитика', description: 'Расширенные отчёты для врачей и кураторов.' },
    ],
    [],
  );
  const [view, setView] = useState<'landing' | Role>('landing');
  const [activeRole, setActiveRole] = useState<Role | null>(null);
  const [login, setLogin] = useState('');
  const [password, setPassword] = useState('');
  const [error, setError] = useState('');
  const [patientSession, setPatientSession] = useState<{
    token: string;
    profile: PatientSessionProfile;
  } | null>(null);

  const patientDemoCredentials = roleContent.patient.demoCredentials;
  const activeRoleContent = activeRole ? roleContent[activeRole] : null;

  const readErrorMessage = async (response: Response) => {
    const contentType = response.headers.get('content-type') ?? '';

    if (contentType.includes('application/json')) {
      try {
        const data = await response.clone().json();
        const detail = typeof data?.detail === 'string' ? data.detail.trim() : '';
        if (detail) {
          return detail;
        }
      } catch (error_) {
        console.error('Не удалось обработать ответ сервера', error_);
      }
    }

    try {
      const text = (await response.text()).trim();
      if (text) {
        return text;
      }
    } catch (error_) {
      console.error('Не удалось прочитать текст ответа', error_);
    }

    return 'Не удалось выполнить запрос. Попробуйте позже.';
  };

  const openModal = (role: Role) => {
    setActiveRole(role);
    if (role === 'patient' && patientDemoCredentials) {
      setLogin(patientDemoCredentials.login);
      setPassword(patientDemoCredentials.password);
    } else {
      setLogin('');
      setPassword('');
    }
    setError('');
  };

  const closeModal = () => {
    setActiveRole(null);
    setError('');
  };

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();

    if (!activeRole) {
      return;
    }

    setError('');

    const normalizedLogin = login.trim();

    if (activeRole === 'doctor') {
      if (!normalizedLogin || !password) {
        setError('Введите логин и пароль.');
        return;
      }

      try {
        const response = await fetch('/api/doctor-login', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ login: normalizedLogin, password }),
        });

        if (response.ok) {
          setView('doctor');
          closeModal();
          return;
        }

        setError(await readErrorMessage(response));
      } catch (error_) {
        console.error(error_);
        setError('Не удалось связаться с сервером. Проверьте соединение и попробуйте снова.');
      }

      return;
    }

    if (!normalizedLogin || !password) {
      setError('Введите логин и пароль.');
      return;
    }

    try {
      const response = await fetch('/api/patient-login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ username: normalizedLogin, password }),
      });

      if (response.ok) {
        const data = await response.json();
        const profile: PatientSessionProfile = {
          id: data.patientId,
          firstName: data.firstName,
          lastName: data.lastName,
          middleName: data.middleName ?? null,
          username: data.username,
        };
        setPatientSession({ token: data.token, profile });
        setView('patient');
        closeModal();
        return;
      }

      setError(await readErrorMessage(response));
    } catch (error_) {
      console.error(error_);
      setError('Не удалось связаться с сервером. Проверьте соединение и попробуйте снова.');
    }
  };

  const handleLogout = () => {
    setView('landing');
    setActiveRole(null);
    setLogin('');
    setPassword('');
    setError('');
    setPatientSession(null);
  };

  if (view === 'doctor') {
    return <DoctorDashboard onLogout={handleLogout} />;
  }

  if (view === 'patient' && patientSession) {
    return (
      <PatientDashboard
        token={patientSession.token}
        patient={patientSession.profile}
        onLogout={handleLogout}
      />
    );
  }

  if (view === 'patient') {
    return (
      <div className="app page patient-page">
        <main className="page-body">
          <p>Не удалось загрузить данные пациента. Попробуйте войти снова.</p>
          <button type="button" className="secondary-button" onClick={handleLogout}>
            Вернуться на главную
          </button>
        </main>
      </div>
    );
  }

  return (
    <div className="app">
      <header className="hero" aria-hidden={activeRole !== null}>
        <div className="hero-content">
          <p className="badge">Скоро запуск</p>
          <h1>Платформа Palsy Video Training</h1>
          <p className="lead">
            Мы создаём сервис дистанционной реабилитации пациентов с ДЦП. Здесь появятся занятия,
            расписание и прогресс пациента. Следите за обновлениями!
          </p>
          <a className="cta" href="mailto:info@palsy-training.example">
            Связаться с нами
          </a>
        </div>
        <div className="hero-visual" aria-hidden>
          <div className="gradient"></div>
          <div className="card">
            <span className="card-title">Видео-тренировка</span>
            <span className="card-body">Тренировка №12 • 24 минуты</span>
          </div>
          <div className="card secondary">
            <span className="card-title">Следующий сеанс</span>
            <span className="card-body">5 октября, 10:00</span>
          </div>
        </div>
      </header>

      <main aria-hidden={activeRole !== null}>
        <section className="section">
          <h2>Что вас ждёт</h2>
          <ul className="feature-list">
            {['Обучающие видео от специалистов', 'Персональные планы реабилитации', 'Отслеживание прогресса пациента'].map((feature) => (
              <li key={feature}>{feature}</li>
            ))}
          </ul>
        </section>

        <section className="section">
          <h2>Дорожная карта</h2>
          <div className="roadmap">
            {roadmapItems.map((item) => (
              <div key={item.title} className="roadmap-item">
                <h3>{item.title}</h3>
                <p>{item.description}</p>
              </div>
            ))}
          </div>
        </section>
      </main>

      <footer aria-hidden={activeRole !== null}>
        <small>© {new Date().getFullYear()} Palsy Video Training. Все права защищены.</small>
      </footer>

      <div className="auth-overlay" role="presentation">
        <div className="auth-card">
          <h2>Войдите в платформу</h2>
          <p>
            Выберите тип доступа, чтобы продолжить. Сейчас доступна демонстрационная учётная запись
            пациента.
          </p>
          <div className="auth-actions">
            <button type="button" className="auth-button" onClick={() => openModal('doctor')}>
              Вход для врачей
            </button>
            <button
              type="button"
              className="auth-button secondary"
              onClick={() => openModal('patient')}
            >
              Вход для пациентов
            </button>
          </div>
          {patientDemoCredentials && (
            <small className="auth-hint">
              Демо-доступ: {patientDemoCredentials.login}/{patientDemoCredentials.password}
            </small>
          )}
        </div>
      </div>

      {activeRole && (
        <div className="auth-modal-backdrop" role="presentation">
          <div
            className="auth-modal"
            role="dialog"
            aria-modal="true"
            aria-labelledby="auth-modal-title"
          >
            <button type="button" className="auth-close" onClick={closeModal} aria-label="Закрыть окно">
              ×
            </button>
            <h3 id="auth-modal-title">{activeRoleContent?.title}</h3>
            {activeRoleContent?.description && <p>{activeRoleContent.description}</p>}
            {activeRoleContent?.redirectMessage && (
              <p className="auth-redirect">{activeRoleContent.redirectMessage}</p>
            )}
            {activeRoleContent?.demoCredentials && (
              <p className="auth-hint">
                Демо-доступ: {activeRoleContent.demoCredentials.login}/
                {activeRoleContent.demoCredentials.password}
              </p>
            )}

            <form className="auth-form" onSubmit={handleSubmit}>
              <label className="auth-label">
                Логин
                <input
                  name="login"
                  type="text"
                  autoComplete="username"
                  value={login}
                  onChange={(event) => {
                    setLogin(event.target.value);
                    if (error) {
                      setError('');
                    }
                  }}
                  className="auth-input"
                  required
                />
              </label>

              <label className="auth-label">
                Пароль
                <input
                  name="password"
                  type="password"
                  autoComplete="current-password"
                  value={password}
                  onChange={(event) => {
                    setPassword(event.target.value);
                    if (error) {
                      setError('');
                    }
                  }}
                  className="auth-input"
                  required
                />
              </label>

              {error && <p className="auth-error" role="alert">{error}</p>}

              <button type="submit" className="auth-submit">
                Войти
              </button>
            </form>
          </div>
        </div>
      )}
    </div>
  );
};

export default App;
