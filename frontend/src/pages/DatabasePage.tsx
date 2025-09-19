import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Link } from "react-router-dom";
import type {
  ColDef,
  GridApi,
  GridReadyEvent,
  IDatasource,
  IGetRowsParams,
} from "ag-grid-community";
import { AgGridReact } from "ag-grid-react";
import "ag-grid-community/styles/ag-grid.css";
import "ag-grid-community/styles/ag-theme-quartz.css";

import { Button } from "../components/ui/button";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "../components/ui/select";
import { cn } from "../lib/utils";
import { fetchTableSlice, getTableColumns, listTables } from "../lib/mockDatabase";

function DatabasePage() {
  const [tables, setTables] = useState<string[]>([]);
  const [loadingTables, setLoadingTables] = useState(true);
  const [selectedTable, setSelectedTable] = useState<string>("");
  const [error, setError] = useState<string | null>(null);
  const [columnDefs, setColumnDefs] = useState<ColDef[]>([]);
  const gridApiRef = useRef<GridApi | null>(null);

  useEffect(() => {
    let mounted = true;

    listTables()
      .then((items) => {
        if (!mounted) {
          return;
        }
        setTables(items);
        setError(null);
      })
      .catch(() => {
        if (!mounted) {
          return;
        }
        setError("Не удалось загрузить список таблиц");
      })
      .finally(() => {
        if (mounted) {
          setLoadingTables(false);
        }
      });

    return () => {
      mounted = false;
    };
  }, []);

  const defaultColDef = useMemo<ColDef>(
    () => ({
      flex: 1,
      minWidth: 120,
      sortable: true,
      filter: true,
      resizable: true,
    }),
    [],
  );

  const createDatasource = useCallback(
    (tableName: string): IDatasource => ({
      getRows(params: IGetRowsParams) {
        fetchTableSlice(tableName, params.startRow, params.endRow)
          .then(({ rows, total }) => {
            params.successCallback(rows, total);
          })
          .catch((err) => {
            console.error(err);
            params.failCallback();
          });
      },
    }),
    [],
  );

  useEffect(() => {
    if (!selectedTable) {
      setColumnDefs([]);
      if (gridApiRef.current) {
        gridApiRef.current.setDatasource({
          getRows(params) {
            params.successCallback([], 0);
          },
        });
      }
      return;
    }

    const columns = getTableColumns(selectedTable);
    setColumnDefs(
      columns.map((field) => ({
        field,
        headerName: field,
        filter: true,
        sortable: true,
      })),
    );

    if (gridApiRef.current) {
      gridApiRef.current.setDatasource(createDatasource(selectedTable));
      gridApiRef.current.refreshInfiniteCache();
    }
  }, [createDatasource, selectedTable]);

  const handleGridReady = useCallback(
    (event: GridReadyEvent) => {
      gridApiRef.current = event.api;
      if (selectedTable) {
        event.api.setDatasource(createDatasource(selectedTable));
      }
    },
    [createDatasource, selectedTable],
  );

  return (
    <main className={cn("min-h-screen bg-slate-950 pb-16 text-white")}> 
      <div className="mx-auto flex max-w-6xl flex-col gap-8 px-6 pt-12">
        <div className="flex items-center justify-between gap-4">
          <div>
            <h1 className="text-3xl font-semibold">База данных</h1>
            <p className="mt-1 text-sm text-slate-400">
              Выберите таблицу, чтобы просмотреть данные в режиме бесконечной прокрутки.
            </p>
          </div>
          <Button asChild variant="outline">
            <Link to="/">На главную</Link>
          </Button>
        </div>

        <div className="flex flex-col gap-4 rounded-xl border border-slate-800 bg-slate-900/60 p-6">
          <div className="flex flex-col gap-2 sm:flex-row sm:items-end sm:justify-between">
            <div className="w-full sm:max-w-xs">
              <label className="mb-1 block text-sm font-medium text-slate-300" htmlFor="table-select">
                Таблица
              </label>
              <Select
                onValueChange={setSelectedTable}
                value={selectedTable || undefined}
                disabled={loadingTables}
              >
                <SelectTrigger id="table-select">
                  <SelectValue placeholder={loadingTables ? "Загрузка..." : "Выберите таблицу"} />
                </SelectTrigger>
                <SelectContent>
                  {tables.map((table) => (
                    <SelectItem key={table} value={table}>
                      {table}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>
            {error && <span className="text-sm text-red-400">{error}</span>}
          </div>

          <div className="ag-theme-quartz h-[600px] w-full rounded-lg border border-slate-800 bg-slate-950/50">
            <AgGridReact
              columnDefs={columnDefs}
              defaultColDef={defaultColDef}
              animateRows
              rowModelType="infinite"
              cacheBlockSize={50}
              maxBlocksInCache={4}
              onGridReady={handleGridReady}
              suppressCellFocus
              overlayNoRowsTemplate="Выберите таблицу, чтобы увидеть данные"
            />
          </div>
        </div>
      </div>
    </main>
  );
}

export default DatabasePage;
