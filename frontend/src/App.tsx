import { useState } from "react";
import { Button } from "./components/ui/button";
import { cn } from "./lib/utils";

function App() {
  const [count, setCount] = useState(0);

  return (
    <main className={cn("min-h-screen bg-gradient-to-br from-slate-900 via-slate-950 to-black text-white")}
    >
      <div className="mx-auto flex max-w-3xl flex-col items-center justify-center gap-6 px-6 py-20 text-center">
        <h1 className="text-4xl font-bold sm:text-5xl">Palsy Video Training</h1>
        <p className="text-lg text-slate-300">
          Стартовая оболочка React + shadcn/ui для будущего интерфейса управления тренировками.
        </p>
        <Button size="lg" onClick={() => setCount((value) => value + 1)}>
          Нажато {count} раз
        </Button>
      </div>
    </main>
  );
}

export default App;
