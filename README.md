# 2I-FQR forward variance forecasting

Reproducible research code and frozen numerical outputs for **Depth-Weighted
Shape-Constrained Quantile Forecasting with Block-Calibrated Bands for
Cumulative Forward Variance Paths**.

The target is the cumulative sum of future squared daily log returns. The
pipeline combines response-path depth weighting, quantile regression,
Dykstra projection onto joint shape constraints, and moving-block calibration
of simultaneous prediction bands. It also includes a distributional
Student-t GARCH(1,1) benchmark and historical forecasts.

## Contents

| Path | Purpose |
| --- | --- |
| `analysis/2IFQR_v2_1_R1_Full_Pipeline.R` | Original integrated R pipeline, preserved byte for byte |
| `analysis/data_raw/` | Five frozen Yahoo Finance adjusted-price series |
| `analysis/tables_csv/` | Archived empirical, sensitivity, and simulation outputs |
| `analysis/logs/` | Original configuration, versions, checksums, and numerical checks |
| `scripts/verify_replication.py` | Independent file-integrity and loss-aggregation checks |
| `scripts/rebuild_figures.py` | Reconstruct the 13 publication figures from CSV files |
| `MANIFEST_SHA256.txt` | SHA-256 digests for this replication distribution |

## Study configuration

- Assets: PAVE, KO, BTC-USD, PPLT, GLD.
- Requested price window: January 2019 through December 2024.
- Forecast horizon: 30 return observations, respecting each asset's native calendar.
- Quantile levels: 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95.
- Chronological training, calibration, and testing, with 30-origin embargoes.
- Master seed: 20260818, with deterministic task-specific streams.
- Full simulation: six designs with 500 replications each.

## Get the code

```bash
git clone https://github.com/caglarsozen/2ifqr-forward-variance.git
cd 2ifqr-forward-variance
```

For citation and replication, record the full commit identifier returned by
`git rev-parse HEAD`. The paper title above identifies the associated study.

## R setup

The archived run used R 4.5.0 on Windows. Full package versions are in
`analysis/logs/sessionInfo.txt`; the main packages include quantreg 6.1,
quantmod 0.4.28, xts 0.14.1, zoo 1.8-15, ggplot2 4.0.2, and scales 1.4.0.
The following command installs available versions; it does not recreate a
fully locked historical R environment.

```r
install.packages(c("quantmod", "xts", "zoo", "quantreg", "ggplot2", "scales"))
```

The conditional Student-t GARCH likelihood, filtering, and return-path simulation
are implemented in the pipeline; no additional `rugarch` installation is required.

## Quick deterministic check

Open R or RStudio **at the repository root** and run:

```r
Sys.setenv(
  TWOIFQR_RUN_MODE = "selftest",
  TWOIFQR_DATA_CACHE = normalizePath("analysis/data_raw", mustWork = TRUE),
  TWOIFQR_OUTPUT = file.path(getwd(), "results", "selftest")
)
source("analysis/2IFQR_v2_1_R1_Full_Pipeline.R")
```

This diagnostic does not produce empirical manuscript evidence. The `smoke`
mode is a small synthetic integration check, not a replacement for a full run.

## Reproduce the full analysis

From a fresh R session at the repository root:

```r
Sys.setenv(
  TWOIFQR_RUN_MODE = "full",
  TWOIFQR_CODE_REPOSITORY_URL = "https://github.com/caglarsozen/2ifqr-forward-variance",
  TWOIFQR_DATA_CACHE = normalizePath("analysis/data_raw", mustWork = TRUE),
  TWOIFQR_OUTPUT = file.path(getwd(), "results", "full")
)
source("analysis/2IFQR_v2_1_R1_Full_Pipeline.R")
```

Full mode includes regression fitting and selection, the 20,000-path GARCH
forecasts, sensitivity analyses, and all six simulation designs. It is a
substantial computation. Compatible checkpoints are reused when the same
output directory is used again. `TWOIFQR_CORES` controls simulation workers.

The explicit frozen cache avoids silently replacing the research sample with
a new provider download. Data source: [Yahoo Finance](https://finance.yahoo.com/).
The source series use columns `Date`, `Asset`, and `Adjusted`. Fresh downloads
and changed numerical environments can produce different results.

Outputs appear under `results/full/`, including `tables_csv`, `tables_latex`,
`figures`, `objects`, and `logs`. The archived fitted R objects are not included;
the full pipeline can regenerate them. Inspect the completion and audit records
before treating a new run as complete.

## Verify the archived files without R

Python 3.9 or newer is sufficient; no third-party Python packages are needed:

```bash
python scripts/verify_replication.py
```

This checks the manifest, the original R and data checksums, separate Holm
families, WIS identities, and independent aggregation of 43,380 origin-horizon
and 1,050 horizon-quantile loss records. It does not refit the models or rerun
the Monte Carlo experiment.

## Reconstruct the figures

```bash
python -m pip install -r requirements-figures.txt
python scripts/rebuild_figures.py
```

The reconstructed figures are written to `figures/`. The plotting script uses
the archived CSV results; it does not fit statistical models.

## Interpretation

The results concern a five-asset illustrative panel and the stated simulation
designs. Weighting gains, calibration errors, and benchmark rankings are
heterogeneous. The moving-block guard does not establish a general finite-sample
coverage guarantee under unrestricted dependence or distribution shift.
The two five-asset Holm families do not correct for all benchmarks, all horizons,
or earlier asset/model screening. BTC-USD's GARCH persistence estimate reaches
its transformed numerical ceiling. These limits are retained in the study.

## Provenance

The original R pipeline, frozen input prices, and numerical CSV files are
preserved from the final research package dated 26 October 2026. Repository
preparation adds documentation and a standalone verification script; it does
not change the statistical estimates. Original run logs and newly performed
file checks describe different validation stages.



## License and data attribution

The code is distributed under the [MIT License](LICENSE) selected for this
repository. Frozen adjusted prices are attributed to Yahoo Finance; the code
license does not grant additional rights in third-party data.
