"""Neutralizaciones por carrera con FastF1 (2019-2026), desde la cache local.
SOLO para analizar la frecuencia de VSC/SC por circuito con mas historia que OpenF1.
No descarga (modo offline). Usa el estado de pista de cada vuelta del lider:
'4' = SC, '6' o '7' = VSC (desplegado o terminando), '5' = bandera roja.
Uso: python ff1_neutralizaciones.py <carpeta salida>
"""
import sys, logging
from pathlib import Path
import pandas as pd
import fastf1

logging.getLogger("fastf1").setLevel(logging.ERROR)
fastf1.Cache.enable_cache(str(Path.home() / "Library/Caches/fastf1"))
fastf1.Cache.offline_mode(True)
salida = Path(sys.argv[1])
filas = []
for anio in range(2019, 2027):
    try:
        cal = fastf1.get_event_schedule(anio, include_testing=False)
    except Exception as e:
        print(anio, "sin calendario en cache:", e); continue
    for _, ev in cal.iterrows():
        try:
            s = fastf1.get_session(anio, ev["RoundNumber"], "R")
            s.load(laps=True, telemetry=False, weather=True, messages=False)
            laps = s.laps
            if laps is None or len(laps) == 0:
                continue
        except Exception:
            continue
        n = int(laps["LapNumber"].max())
        lider = laps[laps["Driver"] == laps.loc[laps["LapNumber"].idxmax(), "Driver"]].sort_values("LapNumber")
        estado = {int(r.LapNumber): str(r.TrackStatus) for r in lider.itertuples() if pd.notna(r.TrackStatus)}
        w = s.weather_data
        lluvia = bool(w["Rainfall"].any()) if w is not None and len(w) else None
        mojado = bool(laps["Compound"].isin(["INTERMEDIATE", "WET"]).any())
        for v in range(1, n + 1):
            e = estado.get(v, "")
            filas.append(dict(anio=anio, ronda=int(ev["RoundNumber"]), evento=ev["EventName"], lugar=ev["Location"],
                              fecha=str(ev["Session5DateUtc"]), vueltas=n, lluvia=lluvia, mojado=mojado, vuelta=v,
                              sc="4" in e, vsc=("6" in e) or ("7" in e), roja="5" in e, estado=e))
        print(anio, ev["RoundNumber"], ev["EventName"], flush=True)
pd.DataFrame(filas).to_csv(salida / "estado_vueltas.csv", index=False)
print("Listo")
