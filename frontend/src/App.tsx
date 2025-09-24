import { FormEvent, useMemo, useState } from 'react';
import DoctorDashboard from './DoctorDashboard';

type Role = 'doctor' | 'patient';

type Credentials = {
  login: string;
  password: string;
  title: string;
  description: string;
  redirectMessage: string;
};

const credentialsMap: Record<Role, Credentials> = {
  doctor: {
    login: 'doctor',
    password: '123',
    title: 'Вход для врачей',
    description:
      'Получите доступ к инструментам назначения упражнений и отслеживания прогресса пациентов.',
    redirectMessage: 'После авторизации вы перейдёте в кабинет врача.',
  },
  patient: {
    login: 'user',
    password: '123',
    title: 'Вход для пациентов',
    description: 'Откройте персональный план тренировок и рекомендации от специалистов.',
    redirectMessage: 'После авторизации вы перейдёте в личный кабинет пациента.',
  },
};

const patientPageContent = {
  title: 'Личный кабинет пациента',
  lead: 'Ваша домашняя страница с упражнениями и напоминаниями.',
  description:
    'Мы работаем над разделом, где будут отображаться занятия, видеоинструкции и рекомендации врача. Как только сервис будет готов, вы сможете отслеживать прогресс и получать обратную связь.',
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

  const openModal = (role: Role) => {
    setActiveRole(role);
    setLogin('');
    setPassword('');
    setError('');
  };

  const closeModal = () => {
    setActiveRole(null);
    setError('');
  };

  const handleSubmit = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();

    if (!activeRole) {
      return;
    }

    const expected = credentialsMap[activeRole];

    if (login.trim() === expected.login && password === expected.password) {
      setView(activeRole);
      closeModal();
      return;
    }

    setError('Неверный логин или пароль. Попробуйте ещё раз.');
  };

  const handleLogout = () => {
    setView('landing');
    setActiveRole(null);
    setLogin('');
    setPassword('');
    setError('');
  };

  if (view === 'doctor') {
    return <DoctorDashboard onLogout={handleLogout} />;
  }

  if (view === 'patient') {
    return (
      <div className="app page patient-page">
        <header className="page-header">
          <h1>{patientPageContent.title}</h1>
          <p className="lead">{patientPageContent.lead}</p>
          <button type="button" className="secondary-button" onClick={handleLogout}>
            Выйти
          </button>
        </header>
        <main className="page-body">
          <p>{patientPageContent.description}</p>
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
            Выберите тип доступа, чтобы продолжить. Сейчас доступны демонстрационные учётные записи
            для врачей и пациентов.
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
          <small className="auth-hint">Демо-доступ: doctor/123 и user/123</small>
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
            <h3 id="auth-modal-title">{credentialsMap[activeRole].title}</h3>
            <p>{credentialsMap[activeRole].description}</p>
            <p className="auth-redirect">{credentialsMap[activeRole].redirectMessage}</p>

            <form className="auth-form" onSubmit={handleSubmit}>
              <label className="auth-label">
                Логин
                <input
                  name="login"
                  type="text"
                  autoComplete="username"
                  value={login}
                  onChange={(event) => setLogin(event.target.value)}
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
                  onChange={(event) => setPassword(event.target.value)}
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
