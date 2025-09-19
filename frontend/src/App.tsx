import { useMemo } from 'react';

const features = [
  'Обучающие видео от специалистов',
  'Персональные планы реабилитации',
  'Отслеживание прогресса пациента',
];

const App = () => {
  const roadmapItems = useMemo(
    () => [
      { title: 'MVP платформы', description: 'Базовый просмотр контента и личный кабинет.' },
      { title: 'Интерактивные упражнения', description: 'Добавление обратной связи и подсказок в реальном времени.' },
      { title: 'Аналитика', description: 'Расширенные отчёты для врачей и кураторов.' },
    ],
    [],
  );

  return (
    <div className="app">
      <header className="hero">
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

      <main>
        <section className="section">
          <h2>Что вас ждёт</h2>
          <ul className="feature-list">
            {features.map((feature) => (
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

      <footer>
        <small>© {new Date().getFullYear()} Palsy Video Training. Все права защищены.</small>
      </footer>
    </div>
  );
};

export default App;
