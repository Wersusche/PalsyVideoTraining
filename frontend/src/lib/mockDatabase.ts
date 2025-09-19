export type MockTableRow = Record<string, string | number | boolean | null>;

const patients: MockTableRow[] = Array.from({ length: 200 }, (_, index) => ({
  id: index + 1,
  name: `Пациент ${index + 1}`,
  age: 18 + ((index * 7) % 48),
  diagnosis: index % 3 === 0 ? "ДЦП" : index % 3 === 1 ? "Черепно-мозговая травма" : "Инсульт",
  active: index % 4 !== 0,
}));

const sessions: MockTableRow[] = Array.from({ length: 320 }, (_, index) => ({
  id: index + 1,
  patient_id: (index % patients.length) + 1,
  duration_min: 20 + (index % 10) * 5,
  difficulty: ["легкая", "средняя", "сложная"][index % 3],
  completed_at: `2024-03-${(index % 30) + 1}`,
}));

const experts: MockTableRow[] = Array.from({ length: 90 }, (_, index) => ({
  id: index + 1,
  name: `Специалист ${index + 1}`,
  specialization: ["Невролог", "ЛФК", "Реабилитолог"][index % 3],
  rating: 3 + (index % 20) / 10,
}));

const mockTables: Record<string, MockTableRow[]> = {
  patients,
  sessions,
  experts,
};

export async function listTables(): Promise<string[]> {
  await new Promise((resolve) => setTimeout(resolve, 300));
  return Object.keys(mockTables);
}

export async function fetchTableSlice(tableName: string, start: number, end: number): Promise<{
  rows: MockTableRow[];
  total: number;
}> {
  await new Promise((resolve) => setTimeout(resolve, 250));
  const table = mockTables[tableName];
  if (!table) {
    throw new Error(`Unknown table ${tableName}`);
  }

  const safeStart = Math.max(0, start);
  const safeEnd = Math.min(end, table.length);
  return {
    rows: table.slice(safeStart, safeEnd),
    total: table.length,
  };
}

export function getTableColumns(tableName: string): string[] {
  const table = mockTables[tableName];
  if (!table || table.length === 0) {
    return [];
  }
  return Object.keys(table[0]);
}
