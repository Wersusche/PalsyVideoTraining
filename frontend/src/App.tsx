import { Route, Routes } from "react-router-dom";

import DatabasePage from "./pages/DatabasePage";
import HomePage from "./pages/HomePage";

function App() {
  return (
    <Routes>
      <Route path="/" element={<HomePage />} />
      <Route path="/database" element={<DatabasePage />} />
    </Routes>
  );
}

export default App;
