from fastapi.testclient import TestClient

from app.database import get_db
from app.main import create_app


def test_healthcheck():
    app = create_app()

    class DummySession:
        def execute(self, *_args, **_kwargs):
            return None

        def close(self):
            pass

    def dummy_get_db():
        yield DummySession()

    app.dependency_overrides[get_db] = dummy_get_db

    try:
        client = TestClient(app)
        response = client.get("/healthz")
    finally:
        app.dependency_overrides.clear()

    assert response.status_code == 200
    assert response.json() == {"status": "ok"}
