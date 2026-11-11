#!/usr/bin/env python3
"""Verify frozen replication files and independent loss aggregations.

Uses only Python's standard library. It does not fit any forecasting model.
"""
from pathlib import Path
import csv
import hashlib
import json
import math
import statistics

ROOT = Path(__file__).resolve().parents[1]
TABLES = ROOT / "analysis" / "tables_csv"
LOGS = ROOT / "analysis" / "logs"
CHECKS = []


def check(name, condition):
    CHECKS.append({"check": name, "passed": bool(condition)})


def read(path):
    with path.open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def near(left, right, tolerance=1e-12):
    return math.isclose(float(left), float(right), rel_tol=1e-9, abs_tol=tolerance)


def holm(values):
    ordered = sorted(range(len(values)), key=values.__getitem__)
    result = [0.0] * len(values)
    previous = 0.0
    for rank, index in enumerate(ordered):
        previous = min(1.0, max(previous, (len(values) - rank) * values[index]))
        result[index] = previous
    return result


for line in (ROOT / "MANIFEST_SHA256.txt").read_text().splitlines():
    digest, name = line.split("  ", 1)
    path = ROOT / name
    check("SHA256 " + name, path.is_file() and hashlib.sha256(path.read_bytes()).hexdigest() == digest)

source = read(LOGS / "pipeline_source_checksum.csv")[0]
path = ROOT / "analysis" / source["Source"]
check("Original executed R source", hashlib.md5(path.read_bytes()).hexdigest() == source["MD5"])

for manifest, folder in [("raw_data_checksums.csv", ROOT / "analysis/data_raw"),
                         ("output_table_checksums.csv", TABLES)]:
    for row in read(LOGS / manifest):
        path = folder / row["File"]
        check("Original MD5 " + row["File"], path.is_file() and hashlib.md5(path.read_bytes()).hexdigest() == row["MD5"])

for name in ["fixed_panel_adjusted_inference", "r1_garch_paired_inference"]:
    rows = read(TABLES / (name + ".csv"))
    adjusted = holm([float(row["RawTwoSidedPValue"]) for row in rows])
    check("Separate five-asset Holm family " + name,
          len(rows) == 5 and all(near(value, row["HolmAdjustedPValue"]) for value, row in zip(adjusted, rows)))

metrics = read(TABLES / "r1_distributional_benchmark_metrics.csv")
for row in metrics:
    check("WIS identity " + row["Asset"] + " " + row["Method"],
          near(float(row["WIS"]), 2 * float(row["MeanPinball"])))

origins = read(TABLES / "r1_horizon_origin_losses.csv")
quantiles = read(TABLES / "r1_horizon_quantile_loss_differences.csv")
horizons = read(TABLES / "r1_horizon_loss_differences.csv")
splits = {row["Asset"]: row for row in read(TABLES / "split_audit.csv")}
assets = ["PAVE", "KO", "BTC-USD", "PPLT", "GLD"]
for asset in assets:
    records = [row for row in origins if row["Asset"] == asset]
    qrecords = [row for row in quantiles if row["Asset"] == asset]
    hrecords = [row for row in horizons if row["Asset"] == asset]
    expected = int(splits[asset]["TestN"])
    pairs = {(row["OriginDate"], int(row["Horizon"])) for row in records}
    check("Complete origin-horizon grid " + asset, len(records) == len(pairs) == expected * 30)
    check("Complete horizon-quantile grid " + asset,
          len(qrecords) == 210 and len({(row["Horizon"], row["Tau"]) for row in qrecords}) == 210)
    check("Paired loss identity " + asset,
          all(near(float(row["UnweightedLoss"]) - float(row["SelectedLoss"]), row["Gain"])
              for row in records))
    for horizon in range(1, 31):
        group = [row for row in records if int(row["Horizon"]) == horizon]
        qgroup = [row for row in qrecords if int(row["Horizon"]) == horizon]
        published = next(row for row in hrecords if int(row["Horizon"]) == horizon)
        means = [statistics.fmean(float(row[col]) for row in group)
                 for col in ["UnweightedLoss", "SelectedLoss"]]
        qmeans = [statistics.fmean(float(row[col]) for row in qgroup)
                  for col in ["UnweightedLoss", "SelectedLoss"]]
        check("Independent horizon aggregation " + asset + " " + str(horizon),
              len(group) == expected and len(qgroup) == 7
              and all(near(a, b) for a, b in zip(means, qmeans))
              and near(means[0] - means[1], published["MeanGain"]))
    for method, column in [("QR + exact 2I+", "UnweightedLoss"), ("Selected 2I-FQR", "SelectedLoss")]:
        published = next(row for row in metrics if row["Asset"] == asset and row["Method"] == method)
        check("Aggregate score " + asset + " " + method,
              near(statistics.fmean(float(row[column]) for row in records), published["MeanPinball"]))
    dates = splits[asset]
    check("Target windows do not overlap " + asset,
          dates["TrainLastTargetEnd"] < dates["CalibrationFirstOrigin"]
          and dates["CalibrationLastTargetEnd"] < dates["TestFirstOrigin"])

summary = {"checks": len(CHECKS), "passed": sum(item["passed"] for item in CHECKS),
           "failed": [item["check"] for item in CHECKS if not item["passed"]],
           "origin_horizon_records": len(origins), "horizon_quantile_records": len(quantiles),
           "models_refitted": False}
print(json.dumps(summary, indent=2))
if summary["failed"]:
    raise SystemExit(1)
