# ==============================================================================
# 2I-FQR v2.1 R1 - complete, integrated, single-file research pipeline
# Manuscript SMAP-D-26-00135R1
# Target: cumulative future squared daily log returns (a variance proxy), H=30.
# No repository credentials or GitHub connection are required.
#
# DEFAULT: TWOIFQR_RUN_MODE=full. All five assets, all R1 modules and the original
# six-scenario / 500-replication simulation are enabled.
# revision: full empirical/R1 analysis; reuse simulation only if explicitly given.
# smoke: SYNTHETIC reduced end-to-end run; never manuscript evidence.
# selftest: deterministic mathematical and numerical tests; no market download.
#
# Run: Rscript 2IFQR_v2_1_R1_Full_Pipeline.R
# In RStudio: source("2IFQR_v2_1_R1_Full_Pipeline.R")
# Parameters are read from environment variables BEFORE source/run.
# See the configuration block and generated REPRODUCE.txt.
#
# GARCH benchmark: zero-mean sGARCH(1,1), variance-standardized Student-t,
# conditional MLE in base R, fixed training parameters, causal filtered state,
# conditional return-path simulation, and quantiles of cumulative squared returns.
# This is a distributional GARCH model, not a point-variance surrogate.
# ==============================================================================
options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

as_flag <- function(x, default = FALSE) {
  if (length(x) == 0L || is.na(x) || !nzchar(x)) return(default)
  tolower(trimws(x)) %in% c("1", "true", "t", "yes", "y")
}

FIXED_PANEL <- c("PAVE", "KO", "BTC-USD", "PPLT", "GLD")
RUN_MODE <- tolower(trimws(Sys.getenv("TWOIFQR_RUN_MODE", "full")))
if (!RUN_MODE %in% c("full", "revision", "smoke", "selftest")) {
  stop("TWOIFQR_RUN_MODE must be full, revision, smoke, or selftest.")
}
profile <- if (RUN_MODE == "full") "publication" else RUN_MODE
profile_settings <- list(
  assets = FIXED_PANEL,
  run_empirical = TRUE,
  run_depth_sensitivity = TRUE,
  run_grid_sensitivity = TRUE,
  run_block_sensitivity = TRUE,
  run_simulation = TRUE,
  existing_empirical_rds = trimws(Sys.getenv(
    "TWOIFQR_EXISTING_EMPIRICAL_RDS", ""
  )),
  existing_simulation_rds = trimws(Sys.getenv(
    "TWOIFQR_EXISTING_SIMULATION_RDS", ""
  )),
  simulation_reps = 500L,
  block_boot_reps = 2000L,
  simulation_block_boot_reps = 250L,
  oracle_diagnostic_reps = 100L,
  oracle_origins = 30L,
  oracle_draws = 2000L,
  projection_max_iter = 10000L
)
configured_assets <- FIXED_PANEL
if (!identical(configured_assets, FIXED_PANEL) || anyDuplicated(configured_assets)) {
  stop("The fixed five-asset manuscript panel was altered.")
}

detected_cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
if (!is.finite(detected_cores) || detected_cores < 1L) detected_cores <- 1L
default_cores <- max(1L, as.integer(detected_cores) - 1L)

CFG <- list(
  version = "2.1-R1",
  profile = profile,
  seed = 20260818L,
  assets = configured_assets,
  primary_assets = FIXED_PANEL,
  start_date = as.Date("2019-01-01"),
  end_date = as.Date("2024-12-31"),
  H = 30L,
  taus = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
  split_train_end = 0.60,
  split_cal_end = 0.80,
  embargo = 30L,
  internal_validation_fraction = 0.20,
  har_windows = c(1L, 5L, 22L),
  qr_method = "fn",
  gamma_grid = c(0, 0.5, 1, 2),
  wmin_grid = c(0.01, 0.05, 0.10),
  wmax = 2,
  wmax_sensitivity_grid = c(1.5, 2, 4),
  primary_band_alpha = 0.10,
  primary_upper_band_alpha = 0.05,
  reporting_calibration_guard = 0.020,
  reporting_calibration_guard_grid = c(0.015, 0.020, 0.025),
  primary_block_length = 30L,
  block_length_grid = c(1L, 5L, 7L, 10L, 20L, 30L, 45L, 60L),
  projection_tol = 1e-8,
  surface_audit_tolerance = 1e-7,
  projection_retry_multipliers = c(1L, 2L, 4L),
  projection_max_iter = profile_settings$projection_max_iter,
  block_bootstrap_guard_probability = as.numeric(Sys.getenv(
    "TWOIFQR_BOOTSTRAP_GUARD_PROBABILITY", "0.90"
  )),
  refresh_data = as_flag(Sys.getenv("TWOIFQR_REFRESH_DATA", "false")),
  run_empirical = TRUE,
  run_depth_sensitivity = TRUE,
  run_grid_sensitivity = TRUE,
  run_block_sensitivity = TRUE,
  run_simulation = TRUE,
  existing_empirical_rds = profile_settings$existing_empirical_rds,
  existing_simulation_rds = profile_settings$existing_simulation_rds,
  simulation_reps = as.integer(Sys.getenv(
    "TWOIFQR_SIM_REPS", as.character(profile_settings$simulation_reps)
  )),
  block_boot_reps = profile_settings$block_boot_reps,
  simulation_block_boot_reps = profile_settings$simulation_block_boot_reps,
  oracle_diagnostic_reps = as.integer(Sys.getenv(
    "TWOIFQR_ORACLE_REPS", as.character(profile_settings$oracle_diagnostic_reps)
  )),
  oracle_origins = as.integer(Sys.getenv(
    "TWOIFQR_ORACLE_ORIGINS", as.character(profile_settings$oracle_origins)
  )),
  oracle_draws = as.integer(Sys.getenv(
    "TWOIFQR_ORACLE_DRAWS", as.character(profile_settings$oracle_draws)
  )),
  cores = max(1L, as.integer(Sys.getenv(
    "TWOIFQR_CORES", as.character(default_cores)
  ))),
  rolling_history = 252L,
  paired_inference_comparators = c(
    "QR + exact 2I+", "Training climatology", "Curve persistence",
    "Rolling historical quantiles"
  ),
  paired_zero_tolerance = 1e-12,
  code_repository_url = Sys.getenv(
    "TWOIFQR_CODE_REPOSITORY_URL", ""
  ),
  archive_doi = Sys.getenv("TWOIFQR_ARCHIVE_DOI", ""),
  data_repository_url = Sys.getenv(
    "TWOIFQR_DATA_REPOSITORY_URL", "https://finance.yahoo.com"
  ),
  output_root = Sys.getenv(
    "TWOIFQR_OUTPUT", paste0("2ifqr_v2_1_R1_", RUN_MODE, "_outputs")
  )
)

if (CFG$embargo < CFG$H) {
  stop("Leakage safeguard failed: CFG$embargo must be at least H.")
}
if (!is.finite(CFG$simulation_reps) || CFG$simulation_reps < 1L) {
  stop("TWOIFQR_SIM_REPS must be a positive integer.")
}
if (!is.finite(CFG$cores) || CFG$cores < 1L) {
  stop("TWOIFQR_CORES must be a positive integer.")
}
# ---- R1 controls -------------------------------------------------------------
r1_env_int <- function(key, default, minimum = 1L) {
  z <- suppressWarnings(as.numeric(Sys.getenv(key, as.character(default))))
  if (length(z) != 1L || !is.finite(z) || z < minimum || z != floor(z)) {
    stop(key, " must be an integer >= ", minimum)
  }
  as.integer(z)
}
r1_env_grid <- function(key, default) {
  z <- suppressWarnings(as.numeric(strsplit(Sys.getenv(key, paste(default, collapse=",")), ",")[[1L]]))
  if (!length(z) || any(!is.finite(z)) || any(z < 1) || any(z != floor(z))) {
    stop(key, " must contain positive comma-separated integers.")
  }
  sort(unique(as.integer(z)))
}
CFG$run_mode <- RUN_MODE
CFG$data_mode <- if (RUN_MODE == "smoke") "SYNTHETIC_DIAGNOSTIC" else "Yahoo_adjusted"
CFG$data_cache_dir <- trimws(Sys.getenv("TWOIFQR_DATA_CACHE", ""))
CFG$calibration_reps <- r1_env_int("TWOIFQR_CALIBRATION_REPS", 2000L, 2L)
CFG$block_boot_reps <- r1_env_int("TWOIFQR_INFERENCE_REPS", 2000L, 20L)
CFG$calibration_mc_grid <- r1_env_grid("TWOIFQR_MC_REPS_GRID", c(500L,1000L,2000L,5000L))
CFG$calibration_mc_seeds <- r1_env_int("TWOIFQR_MC_SEEDS", 10L, 2L)
CFG$garch_draws <- r1_env_int("TWOIFQR_GARCH_DRAWS", 20000L, 100L)
CFG$garch_mc_draws <- r1_env_grid("TWOIFQR_GARCH_MC_GRID", c(5000L,20000L,50000L))
CFG$garch_mc_origins <- r1_env_int("TWOIFQR_GARCH_MC_ORIGINS", 5L)
CFG$garch_maxit <- r1_env_int("TWOIFQR_GARCH_MAXIT", 2000L, 50L)
CFG$garch_return_scale <- 100
CFG$garch_mean <- "zero"
CFG$garch_backend <- "native_conditional_MLE"
CFG$garch_parameter_policy <- "training_fixed; causal_state_updates"
CFG$r1_version <- "2.1-R1-20261018a"
CFG$resume <- as_flag(Sys.getenv("TWOIFQR_RESUME", "true"))
CFG$run_simulation <- RUN_MODE %in% c("full", "smoke") || nzchar(CFG$existing_simulation_rds)
CFG$cores <- r1_env_int("TWOIFQR_CORES", min(4L, default_cores))
if (RUN_MODE == "smoke") {
  CFG$H <- 10L; CFG$embargo <- 10L; CFG$primary_block_length <- 10L
  CFG$end_date <- CFG$start_date + 499L
  CFG$block_length_grid <- c(1L,5L,10L,20L)
  CFG$block_boot_reps <- 60L; CFG$calibration_reps <- 60L
  CFG$simulation_reps <- 2L; CFG$simulation_block_boot_reps <- 20L
  CFG$oracle_diagnostic_reps <- 1L; CFG$oracle_origins <- 3L; CFG$oracle_draws <- 100L
  CFG$calibration_mc_grid <- c(30L,60L,120L); CFG$calibration_mc_seeds <- 2L
  CFG$garch_draws <- 500L; CFG$garch_mc_draws <- c(250L,500L,1000L)
  CFG$garch_mc_origins <- 2L
  CFG$run_depth_sensitivity <- FALSE; CFG$run_grid_sensitivity <- FALSE
  CFG$cores <- 1L
}
CFG$calibration_mc_grid <- sort(unique(c(CFG$calibration_mc_grid, CFG$calibration_reps)))
CFG$garch_mc_draws <- sort(unique(c(CFG$garch_mc_draws, CFG$garch_draws)))
if (RUN_MODE == "selftest") {
  CFG$run_empirical <- FALSE; CFG$run_simulation <- FALSE
}
if (RUN_MODE == "full" && (CFG$simulation_reps<500L || CFG$block_boot_reps<2000L ||
    CFG$calibration_reps<2000L || CFG$oracle_diagnostic_reps<100L ||
    CFG$oracle_origins<30L || CFG$oracle_draws<2000L || CFG$garch_draws<20000L ||
    CFG$calibration_mc_seeds<10L)) {
  stop("Full mode requires the publication budgets. Remove reduced environment overrides, or use revision/smoke mode.")
}
if (CFG$refresh_data && (nzchar(CFG$existing_empirical_rds) || nzchar(CFG$existing_simulation_rds))) {
  stop("Refreshing data and explicitly reusing old results are incompatible. Choose one.")
}
R1_SOURCE_PATH <- local({
  z <- lapply(sys.frames(), function(fr) fr$ofile)
  z <- unlist(z[vapply(z, function(x) is.character(x) && length(x)==1L, logical(1L))])
  a <- grep("^--file=", commandArgs(FALSE), value=TRUE)
  f <- if (length(z)) tail(z,1L) else if (length(a)) sub("^--file=", "", a[1L]) else ""
  if (nzchar(f) && file.exists(f)) normalizePath(f, winslash="/", mustWork=TRUE) else ""
})

if (!is.finite(CFG$block_bootstrap_guard_probability) ||
    CFG$block_bootstrap_guard_probability < 0.50 ||
    CFG$block_bootstrap_guard_probability >= 1) {
  stop("TWOIFQR_BOOTSTRAP_GUARD_PROBABILITY must be in [0.50, 1).")
}
if (!any(abs(
  CFG$reporting_calibration_guard_grid - CFG$reporting_calibration_guard
) <= 1e-12)) {
  stop("The primary reporting calibration guard must belong to its sensitivity grid.")
}
if (any(!is.finite(c(
  CFG$oracle_diagnostic_reps, CFG$oracle_origins, CFG$oracle_draws
))) || any(c(
  CFG$oracle_diagnostic_reps, CFG$oracle_origins, CFG$oracle_draws
) < 1L)) {
  stop("Oracle diagnostic controls must be positive integers.")
}

set.seed(CFG$seed)

required_packages <- if (RUN_MODE == "selftest") character(0L) else
  c("quantmod", "xts", "zoo", "quantreg", "ggplot2", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Install them before running, e.g. install.packages(c(",
    paste(sprintf("'%s'", missing_packages), collapse = ", "), "))."
  )
}

if (RUN_MODE != "selftest") suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
  library(zoo)
  library(quantreg)
  library(ggplot2)
  library(scales)
})

# ---- Output layout ------------------------------------------------------------
DIR <- list(
  root = CFG$output_root,
  raw = file.path(CFG$output_root, "data_raw"),
  processed = file.path(CFG$output_root, "data_processed"),
  csv = file.path(CFG$output_root, "tables_csv"),
  tex = file.path(CFG$output_root, "tables_latex"),
  figures = file.path(CFG$output_root, "figures"),
  objects = file.path(CFG$output_root, "objects"),
  logs = file.path(CFG$output_root, "logs")
)
invisible(lapply(DIR, dir.create, recursive = TRUE, showWarnings = FALSE))

LOG_FILE <- file.path(DIR$logs, "run_log.txt")
if (file.exists(LOG_FILE)) file.remove(LOG_FILE)

log_msg <- function(..., level = "INFO") {
  line <- sprintf(
    "%s | %-5s | %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    level,
    paste0(..., collapse = "")
  )
  message(line)
  cat(line, "\n", file = LOG_FILE, append = TRUE)
  invisible(line)
}

safe_name <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

write_csv_safe <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

rbind_fill <- function(items) {
  items <- items[!vapply(items, is.null, logical(1L))]
  if (!length(items)) return(data.frame())
  all_names <- unique(unlist(lapply(items, names), use.names = FALSE))
  items <- lapply(items, function(x) {
    missing <- setdiff(all_names, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[, all_names, drop = FALSE]
  })
  do.call(rbind, items)
}

latex_escape <- function(x, replace_underscores = TRUE) {
  x <- as.character(x)
  if (replace_underscores) x <- gsub("_", " ", x, fixed = TRUE)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

academic_column_labels <- function(column_names) {
  label_map <- c(
    Asset = "Asset",
    Method = "Method",
    Scenario = "Scenario",
    Comparator = "Comparator",
    Grid = "Quantile grid",
    Horizon = "Horizon",
    HorizonSubset = "Horizon range",
    HorizonCount = "Number of horizons",
    Tau = "$\\tau$",
    Gamma = "$\\gamma$",
    WMin = "$w_{\\min}$",
    WMax = "$w_{\\max}$",
    WIS = "WIS",
    MeanPinball = "Mean pinball loss",
    CRPSApprox = "CRPS approximation",
    Coverage05 = "Coverage ($\\tau=0.05$)",
    Coverage50 = "Coverage ($\\tau=0.50$)",
    Coverage95 = "Coverage ($\\tau=0.95$)",
    CalibrationMAE = "Calibration MAE",
    TailPinball = "Tail pinball loss",
    TailPinball05Mean = "Tail loss ($\\tau=0.05$)",
    TailPinball95Mean = "Tail loss ($\\tau=0.95$)",
    ObservedCoverage = "Observed coverage",
    CalibrationError = "Calibration error",
    OracleQuantileBias = "Oracle bias",
    OracleQuantileMAE = "Oracle MAE",
    OracleQuantileRMSE = "Oracle RMSE",
    OracleDiagnosticReplications = "Oracle replications",
    SuccessfulReplications = "Successful replications",
    Replications = "Replications",
    NominalCoverage = "Nominal coverage",
    CoordinatewiseCoverage = "Coordinatewise coverage",
    MinimumHorizonCoverage = "Minimum horizon coverage",
    SimultaneousPathCoverage = "Simultaneous path coverage",
    MeanWidth = "Mean band width",
    CalibrationQuantile = "Calibration quantile",
    BlockLength = "Block length",
    CoordinatewiseUpperCoverage = "Mean upper coverage",
    MinimumHorizonUpperCoverage = "Minimum upper coverage",
    SimultaneousUpperPathCoverage = "Simultaneous upper-path coverage",
    MeanUpperBoundary = "Mean upper boundary",
    LegacyMeanUpperCoverage = "Original mean upper coverage",
    CurrentMeanUpperCoverage = "Revised mean upper coverage",
    LegacyMinimumHorizonUpperCoverage = "Original minimum upper coverage",
    CurrentMinimumHorizonUpperCoverage = "Revised minimum upper coverage",
    ChangeInMeanUpperCoverage = "$\\Delta$ mean upper coverage",
    ChangeInMinimumHorizonUpperCoverage = "$\\Delta$ minimum upper coverage",
    ObservedCoverageUnweighted = "Coverage, unweighted",
    ObservedCoverageWeighted = "Coverage, weighted",
    CalibrationErrorUnweighted = "Calibration error, unweighted",
    CalibrationErrorWeighted = "Calibration error, weighted",
    AbsoluteCalibrationErrorUnweighted = "$|CE|$, unweighted",
    AbsoluteCalibrationErrorWeighted = "$|CE|$, weighted",
    MeanPinballUnweighted = "Pinball loss, unweighted",
    MeanPinballWeighted = "Pinball loss, weighted",
    WeightedMinusUnweightedPredictedQuantile = "$\\Delta$ predicted quantile",
    WeightedMinusUnweightedAbsoluteCalibrationError = "$\\Delta |CE|$",
    WeightedMinusUnweightedPinball = "$\\Delta$ pinball loss",
    SelectedGamma = "Selected $\\gamma$",
    SelectedWMin = "Selected $w_{\\min}$",
    SelectedWMax = "Selected $w_{\\max}$",
    AssetClass = "Asset class",
    OverallTrainingRank = "Training rank",
    WithinClassTrainingRank = "Within-class training rank",
    ValidationWISGainPct = "Validation WIS gain (\\%)",
    ValidationCalibrationMAEChange = "$\\Delta$ validation calibration MAE",
    SelectionUsesTestData = "Test data used in selection",
    ProjectedFits = "Projected fits",
    PassedFits = "Passed fits",
    FailedFits = "Failed fits",
    MeanPinballGainPctOfSelected = "Pinball gain (\\%)",
    CRPSGainPctOfSelected = "CRPS gain (\\%)",
    WISGainPctOfSelected = "WIS gain (\\%)",
    MeanPinballGainOfSelected = "Mean pinball gain",
    Lower95 = "95\\% CI lower",
    Upper95 = "95\\% CI upper",
    RawTwoSidedPValue = "Raw two-sided $p$-value",
    HolmAdjustedPValue = "Holm-adjusted $p$-value",
    BootstrapReplications = "Bootstrap replications",
    TestMeanPinballGainPct = "Test mean-pinball gain (\\%)",
    PercentileCILowerPct = "Percentile CI lower (\\%)",
    PercentileCIUpperPct = "Percentile CI upper (\\%)",
    TwoSidedWidthReductionPct = "Two-sided width reduction (\\%)",
    TestCalibrationMAEChange = "$\\Delta$ test calibration MAE",
    Assessment = "Assessment",
    CalibrationGuard = "Calibration guard",
    ObservedCalibrationMAEChange = "Observed $\\Delta$ calibration MAE",
    GuardStatus = "Guard status",
    OverallPredictiveInference = "Overall predictive inference",
    ValidationWIS = "Validation WIS",
    ValidationMeanPinball = "Validation mean pinball loss",
    TwiceValidationMeanPinball = "Twice validation mean pinball loss",
    WISMinusTwiceMeanPinball = "WIS minus twice mean pinball",
    IdentityAuditPassed = "Identity audit passed",
    LowerClipShare = "Lower clipping share",
    UpperClipShare = "Upper clipping share",
    QuantileCount = "Number of quantiles",
    CommonGridPinball = "Common-grid pinball loss",
    MedianPinball = "Median pinball loss",
    PriceN = "Price observations",
    ReturnN = "Return observations",
    StartDate = "Start date",
    EndDate = "End date",
    MeanReturn = "Mean return",
    SDReturn = "Return SD",
    Skewness = "Skewness",
    Kurtosis = "Kurtosis",
    MeanSquaredReturn = "Mean squared return",
    SDSquaredReturn = "Squared-return SD",
    TrainN = "Training observations",
    CalibrationN = "Calibration observations",
    TestN = "Test observations",
    EmbargoTrainCalibration = "Training--calibration embargo",
    EmbargoCalibrationTest = "Calibration--test embargo",
    TrainCalibrationNoOverlap = "Training--calibration separated",
    CalibrationTestNoOverlap = "Calibration--test separated",
    MinimumPrediction = "Minimum prediction",
    MaximumHorizonViolation = "Maximum horizon violation",
    MaximumQuantileViolation = "Maximum quantile violation",
    Passed = "Audit passed",
    MeanPinballMean = "Mean pinball loss",
    MeanPinballMedian = "Median pinball loss",
    MeanPinballIQR = "IQR of pinball loss",
    MeanPinballQ95 = "95th percentile pinball loss",
    Coverage05Mean = "Coverage ($\\tau=0.05$)",
    Coverage95Mean = "Coverage ($\\tau=0.95$)",
    OracleQuantileBias05Mean = "Oracle bias ($\\tau=0.05$)",
    OracleQuantileBias95Mean = "Oracle bias ($\\tau=0.95$)",
    CoordinatewiseCoverageMean = "Mean coordinatewise coverage",
    SimultaneousPathCoverageMean = "Mean simultaneous coverage",
    MeanWidthMean = "Mean band width",
    UpperCoordinatewiseCoverageMean = "Mean upper coverage",
    UpperSimultaneousPathCoverageMean = "Mean upper-path coverage",
    CoverageMean = "Mean coverage",
    CoverageGap = "Coverage minus target",
    MonteCarloSE = "Monte Carlo SE",
    MonteCarloLower95 = "Monte Carlo 95\\% CI lower",
    MonteCarloUpper95 = "Monte Carlo 95\\% CI upper",
    TargetCoverage = "Target coverage",
    CoverageAssessment = "Coverage assessment",
    BandType = "Band type",
    CalibrationRule = "Calibration rule",
    CalibrationInflation = "Calibration inflation",
    MeanCalibrationInflation = "Mean calibration inflation",
    WeightedMinusUnweightedTailPinball05Mean = "$\\Delta$ tail loss ($\\tau=0.05$)",
    WeightedMinusUnweightedTailPinball95Mean = "$\\Delta$ tail loss ($\\tau=0.95$)",
    WeightedMinusUnweightedAbsoluteCalibrationError05Mean = "$\\Delta |CE|$ ($\\tau=0.05$)",
    WeightedMinusUnweightedAbsoluteCalibrationError95Mean = "$\\Delta |CE|$ ($\\tau=0.95$)",
    WeightedMinusUnweightedAbsoluteOracleBias05Mean = "$\\Delta |OB|$ ($\\tau=0.05$)",
    WeightedMinusUnweightedAbsoluteOracleBias95Mean = "$\\Delta |OB|$ ($\\tau=0.95$)"
  )
  labels <- unname(label_map[column_names])
  missing <- is.na(labels)
  if (any(missing)) {
    fallback <- column_names[missing]
    fallback <- gsub("_", " ", fallback, fixed = TRUE)
    fallback <- gsub("([A-Z]+)([A-Z][a-z])", "\\1 \\2", fallback, perl = TRUE)
    fallback <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", fallback, perl = TRUE)
    labels[missing] <- latex_escape(fallback)
  }
  labels
}

write_latex_table <- function(df, path, caption, label, digits = 4L,
                              column_labels = NULL, note = NULL,
                              missing_symbol = "--") {
  if (!nrow(df) || !ncol(df)) stop("Cannot write an empty LaTeX table: ", path)
  out <- df
  alignment <- paste(vapply(df, function(z) {
    if (is.numeric(z) || is.integer(z)) "r" else "l"
  }, character(1L)), collapse = "")
  for (j in seq_along(out)) {
    if (is.logical(out[[j]])) {
      out[[j]] <- ifelse(
        is.na(out[[j]]), missing_symbol,
        ifelse(out[[j]], "Yes", "No")
      )
    } else if (is.integer(out[[j]])) {
      out[[j]] <- ifelse(
        is.na(out[[j]]), missing_symbol, as.character(out[[j]])
      )
    } else if (is.numeric(out[[j]])) {
      out[[j]] <- ifelse(
        is.finite(out[[j]]),
        formatC(out[[j]], digits = digits, format = "fg", flag = "#"),
        missing_symbol
      )
    } else {
      missing <- is.na(out[[j]])
      out[[j]] <- as.character(out[[j]])
      out[[j]][missing] <- missing_symbol
    }
    out[[j]] <- latex_escape(out[[j]], replace_underscores = TRUE)
  }
  headers <- if (is.null(column_labels)) {
    academic_column_labels(names(df))
  } else {
    if (length(column_labels) != ncol(df)) {
      stop("column_labels must have one entry per table column: ", path)
    }
    as.character(column_labels)
  }
  headers <- paste0("\\textbf{", headers, "}")
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  wide <- ncol(out) > 7L
  font_size <- if (ncol(out) <= 6L) "\\small" else if (ncol(out) <= 9L) {
    "\\footnotesize"
  } else "\\scriptsize"
  opening <- c(
    "\\begin{table}[!ht]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    font_size,
    "\\renewcommand{\\arraystretch}{1.08}",
    "\\setlength{\\tabcolsep}{4pt}"
  )
  if (wide) opening <- c(opening, "\\resizebox{\\linewidth}{!}{%")
  opening <- c(opening,
    paste0("\\begin{tabular}{", alignment, "}"),
    "\\toprule",
    paste(headers, collapse = " & "),
    "\\\\",
    "\\midrule"
  )
  writeLines(opening, con)
  apply(out, 1L, function(z) {
    writeLines(c(paste(z, collapse = " & "), "\\\\"), con)
    NULL
  })
  closing <- c("\\bottomrule", "\\end{tabular}")
  if (wide) closing <- c(closing, "}")
  if (!is.null(note) && nzchar(note)) {
    closing <- c(
      closing,
      "\\vspace{2pt}",
      paste0(
        "\\parbox{0.98\\linewidth}{\\footnotesize\\textit{Notes:} ",
        note, "}"
      )
    )
  }
  closing <- c(closing, "\\end{table}")
  writeLines(closing, con)
  invisible(path)
}

quantile_type8 <- function(x, probs) {
  as.numeric(stats::quantile(x, probs = probs, type = 8, na.rm = TRUE, names = FALSE))
}

finite_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

finite_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) NA_real_ else stats::sd(x)
}

finite_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else quantile_type8(x, p)
}

finite_median <- function(x) finite_quantile(x, 0.50)

finite_iqr <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else diff(quantile_type8(x, c(0.25, 0.75)))
}

with_seed <- function(seed, expr) {
  old_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

stable_seed_from_key <- function(..., base_seed = CFG$seed) {
  key <- enc2utf8(paste(..., collapse = "|"))
  code <- utf8ToInt(key)
  if (!length(code)) return(as.integer(base_seed))
  index <- seq_along(code)
  modulus <- 2147483646
  offset <- sum((as.double(index) * as.double(code)) %% 1000000007) %% modulus
  as.integer((as.double(base_seed) + offset) %% modulus + 1)
}

empirical_band_seed <- function(asset, method, band_type, block_length) {
  stable_seed_from_key(
    "empirical-band", asset, method, band_type, as.integer(block_length)
  )
}

log_msg("Starting 2I-FQR v", CFG$version, " with profile=", CFG$profile)
if (!identical(CFG$profile, "publication")) {
  log_msg(
    "This is a diagnostic run, not a publication run. Publication evidence requires ",
    "TWOIFQR_RUN_MODE=full and the full readiness audit.",
    level = "WARN"
  )
}

# ---- Data acquisition and freezing -------------------------------------------
download_adjusted_prices <- function(symbol,start_date,end_date,refresh=FALSE) {
  cache_file <- file.path(DIR$raw,paste0(safe_name(symbol),"_adjusted.csv"))
  validate <- function(z) {
    required <- c("Date","Asset","Adjusted")
    r1_assert(all(required %in% names(z)),paste("Malformed adjusted-price cache",symbol))
    z$Date <- as.Date(z$Date); z$Adjusted <- as.numeric(z$Adjusted)
    r1_assert(!anyNA(z$Date) && !anyDuplicated(z$Date) && all(z$Asset==symbol) &&
                all(is.finite(z$Adjusted) & z$Adjusted>0),paste("Invalid price data",symbol))
    z <- z[z$Date>=start_date & z$Date<=end_date,required,drop=FALSE]
    z <- z[order(z$Date),,drop=FALSE]; rownames(z) <- NULL
    r1_assert(nrow(z)>=300L && min(z$Date)<=start_date+10L && max(z$Date)>=end_date-10L,
              paste("Price cache does not cover the requested dates",symbol))
    z
  }
  freeze <- function(z) {
    write_csv_safe(validate(z),cache_file)
    # Fit from the actual frozen representation on the first run as well as on
    # resume; otherwise CSV rounding changes object fingerprints on the next run.
    validate(utils::read.csv(cache_file,stringsAsFactors=FALSE))
  }
  if (file.exists(cache_file) && !refresh) {
    log_msg("Using frozen adjusted-price data for ",symbol)
    return(validate(utils::read.csv(cache_file,stringsAsFactors=FALSE)))
  }
  if (RUN_MODE=="smoke") {
    i <- match(symbol,FIXED_PANEL)
    sim <- simulate_garch_returns(500L,alpha=.08,beta=.88,df=6,
                                 seed=stable_seed_from_key("synthetic-smoke",symbol))
    z <- data.frame(Date=seq(start_date,by="day",length.out=length(sim$returns)+1L),
                    Asset=symbol,Adjusted=100*exp(c(0,cumsum(sim$returns))))
    # Match the fixed synthetic smoke calendar exactly, including its price origin.
    z <- z[seq_len(500L),,drop=FALSE]
    log_msg("SYNTHETIC smoke prices generated for ",symbol,"; not empirical evidence")
    return(freeze(z))
  }
  if (nzchar(CFG$data_cache_dir) && !refresh) {
    source_file <- file.path(CFG$data_cache_dir,basename(cache_file))
    r1_assert(file.exists(source_file),paste("Frozen data file not found",source_file))
    z <- validate(utils::read.csv(source_file,stringsAsFactors=FALSE))
    return(freeze(z))
  }
  log_msg("Downloading adjusted prices for ",symbol," from Yahoo Finance")
  x <- quantmod::getSymbols(symbol,src="yahoo",from=start_date,to=end_date+1L,
                            auto.assign=FALSE,warnings=FALSE)
  adjusted <- tryCatch(quantmod::Ad(x),error=function(e)NULL)
  r1_assert(!is.null(adjusted) && any(is.finite(as.numeric(adjusted))),
            paste("Adjusted prices unavailable; close-price substitution is disabled for",symbol))
  z <- data.frame(Date=as.Date(zoo::index(adjusted)),Asset=symbol,
                  Adjusted=as.numeric(adjusted))
  z <- z[is.finite(z$Adjusted) & z$Adjusted>0,,drop=FALSE]
  freeze(z)
}

rolling_mean_right <- function(x, k) {
  zoo::rollapplyr(x, width = k, FUN = mean, fill = NA_real_, align = "right")
}

build_curve_dataset_from_returns <- function(dates, returns, H, har_windows = c(1L, 5L, 22L)) {
  stopifnot(length(dates) == length(returns), H >= 1L)
  n <- length(returns)
  r2 <- returns^2
  Xfull <- do.call(cbind, lapply(har_windows, function(k) rolling_mean_right(r2, k)))
  colnames(Xfull) <- paste0("L", har_windows)

  max_origin <- n - H
  origin_pos <- seq_len(max_origin)
  Y <- matrix(NA_real_, nrow = max_origin, ncol = H)
  for (i in origin_pos) {
    Y[i, ] <- cumsum(r2[(i + 1L):(i + H)])
  }
  colnames(Y) <- paste0("H", seq_len(H))

  valid <- stats::complete.cases(Xfull[origin_pos, , drop = FALSE]) &
    rowSums(!is.finite(Y)) == 0L
  origin_pos <- origin_pos[valid]
  X <- Xfull[origin_pos, , drop = FALSE]
  Y <- Y[valid, , drop = FALSE]

  list(
    X = unname(as.matrix(X)),
    Y = unname(as.matrix(Y)),
    origin_date = as.Date(dates[origin_pos]),
    target_start_date = as.Date(dates[origin_pos + 1L]),
    target_end_date = as.Date(dates[origin_pos + H]),
    origin_pos = origin_pos,
    returns = returns,
    squared_returns = r2,
    dates = as.Date(dates),
    H = H,
    har_windows = har_windows
  )
}

build_curve_dataset_from_prices <- function(price_df, H, har_windows) {
  price_df <- price_df[order(price_df$Date), , drop = FALSE]
  r <- c(NA_real_, diff(log(price_df$Adjusted)))
  build_curve_dataset_from_returns(price_df$Date, r, H, har_windows)
}

# ---- Leakage-disciplined splits ----------------------------------------------
make_embargo_split <- function(n, H, train_end_frac = 0.60, cal_end_frac = 0.80,
                               embargo = H, min_each = 30L) {
  if (embargo < H) stop("Embargo must be at least H.")
  train_end <- floor(n * train_end_frac)
  cal_start <- train_end + embargo + 1L
  cal_end <- floor(n * cal_end_frac)
  test_start <- cal_end + embargo + 1L

  idx <- list(
    train = seq_len(train_end),
    embargo_train_cal = if (cal_start > train_end + 1L) {
      (train_end + 1L):(cal_start - 1L)
    } else integer(0L),
    cal = if (cal_start <= cal_end) cal_start:cal_end else integer(0L),
    embargo_cal_test = if (test_start > cal_end + 1L) {
      (cal_end + 1L):(test_start - 1L)
    } else integer(0L),
    test = if (test_start <= n) test_start:n else integer(0L)
  )
  if (any(vapply(idx[c("train", "cal", "test")], length, integer(1L)) < min_each)) {
    stop("Split too small after embargo. Increase sample size or reduce minimum size.")
  }
  idx
}

make_internal_split <- function(train_idx, H, validation_fraction = 0.20) {
  n <- length(train_idx)
  fit_end_local <- floor(n * (1 - validation_fraction))
  val_start_local <- fit_end_local + H + 1L
  if (val_start_local > n) stop("Internal training split too small after H embargo.")
  list(
    fit = train_idx[seq_len(fit_end_local)],
    embargo = train_idx[(fit_end_local + 1L):(val_start_local - 1L)],
    validation = train_idx[val_start_local:n]
  )
}

audit_split_dates <- function(dataset, split, asset = NA_character_) {
  train_end_target <- max(dataset$target_end_date[split$train])
  cal_first_origin <- min(dataset$origin_date[split$cal])
  cal_end_target <- max(dataset$target_end_date[split$cal])
  test_first_origin <- min(dataset$origin_date[split$test])
  data.frame(
    Asset = asset,
    N = nrow(dataset$Y),
    TrainN = length(split$train),
    CalibrationN = length(split$cal),
    TestN = length(split$test),
    EmbargoTrainCalibration = length(split$embargo_train_cal),
    EmbargoCalibrationTest = length(split$embargo_cal_test),
    TrainLastTargetEnd = as.character(train_end_target),
    CalibrationFirstOrigin = as.character(cal_first_origin),
    CalibrationLastTargetEnd = as.character(cal_end_target),
    TestFirstOrigin = as.character(test_first_origin),
    TrainCalibrationNoOverlap = train_end_target < cal_first_origin,
    CalibrationTestNoOverlap = cal_end_target < test_first_origin,
    stringsAsFactors = FALSE
  )
}

# ---- Exact order-cone projection ---------------------------------------------
pava_equal <- function(y) {
  y <- as.numeric(y)
  n <- length(y)
  if (n <= 1L) return(y)
  level <- numeric(n)
  weight <- numeric(n)
  left <- integer(n)
  right <- integer(n)
  m <- 0L
  for (i in seq_len(n)) {
    m <- m + 1L
    level[m] <- y[i]
    weight[m] <- 1
    left[m] <- i
    right[m] <- i
    while (m >= 2L && level[m - 1L] > level[m]) {
      new_weight <- weight[m - 1L] + weight[m]
      level[m - 1L] <- (
        weight[m - 1L] * level[m - 1L] + weight[m] * level[m]
      ) / new_weight
      weight[m - 1L] <- new_weight
      right[m - 1L] <- right[m]
      m <- m - 1L
    }
  }
  out <- numeric(n)
  for (b in seq_len(m)) out[left[b]:right[b]] <- level[b]
  out
}

project_horizon_set <- function(M) {
  M <- as.matrix(M)
  out <- M
  for (j in seq_len(ncol(M))) out[, j] <- pava_equal(M[, j])
  out
}

project_quantile_set <- function(M) {
  M <- as.matrix(M)
  out <- M
  for (i in seq_len(nrow(M))) out[i, ] <- pava_equal(M[i, ])
  out
}

project_nonnegative_set <- function(M) pmax(as.matrix(M), 0)

dykstra_project_surface <- function(M, tol = CFG$projection_tol,
                                    max_iter = CFG$projection_max_iter,
                                    return_diagnostics = FALSE) {
  M <- as.matrix(M)
  if (any(!is.finite(M))) stop("Projection input contains non-finite values.")
  projectors <- list(project_horizon_set, project_quantile_set, project_nonnegative_set)
  correction <- replicate(length(projectors), matrix(0, nrow(M), ncol(M)), simplify = FALSE)
  x <- M
  converged <- FALSE
  rel_change <- Inf

  for (iter in seq_len(max_iter)) {
    x_cycle_start <- x
    for (j in seq_along(projectors)) {
      y <- x + correction[[j]]
      z <- projectors[[j]](y)
      correction[[j]] <- y - z
      x <- z
    }
    rel_change <- sqrt(sum((x - x_cycle_start)^2)) /
      max(1, sqrt(sum(x_cycle_start^2)))
    if (is.finite(rel_change) && rel_change <= tol) {
      converged <- TRUE
      break
    }
  }

  diag <- list(
    iterations = iter,
    converged = converged,
    relative_change = rel_change,
    objective = sum((x - M)^2),
    min_value = min(x),
    max_horizon_violation = if (nrow(x) > 1L) max(0, max(-diff(x))) else 0,
    max_quantile_violation = if (ncol(x) > 1L) {
      max(0, max(-t(apply(x, 1L, diff))))
    } else 0
  )
  if (!converged) {
    warning("Dykstra projection reached max_iter; relative change=", signif(rel_change, 4L))
  }
  if (return_diagnostics) list(surface = x, diagnostics = diag) else x
}

project_array_exact <- function(A, return_diagnostics = FALSE,
                                tol = CFG$projection_tol,
                                max_iter = CFG$projection_max_iter) {
  stopifnot(length(dim(A)) == 3L)
  out <- A
  diag_list <- vector("list", dim(A)[1L])
  for (i in seq_len(dim(A)[1L])) {
    M <- matrix(A[i, , ], nrow = dim(A)[2L], ncol = dim(A)[3L])
    z <- dykstra_project_surface(
      M, tol = tol, max_iter = max_iter,
      return_diagnostics = return_diagnostics
    )
    if (return_diagnostics) {
      out[i, , ] <- z$surface
      diag_list[[i]] <- z$diagnostics
    } else {
      out[i, , ] <- z
    }
  }
  if (return_diagnostics) list(array = out, diagnostics = diag_list) else out
}

project_tau_only_array <- function(A, nonnegative = FALSE) {
  stopifnot(length(dim(A)) == 3L)
  out <- A
  for (i in seq_len(dim(A)[1L])) {
    M <- matrix(out[i, , ], nrow = dim(A)[2L], ncol = dim(A)[3L])
    M <- project_quantile_set(M)
    if (nonnegative) M <- project_nonnegative_set(M)
    out[i, , ] <- M
  }
  out
}

surface_audit <- function(A, tolerance = CFG$surface_audit_tolerance) {
  stopifnot(length(dim(A)) == 3L)
  neg <- min(A, na.rm = TRUE)
  h_v <- 0
  tau_v <- 0
  for (i in seq_len(dim(A)[1L])) {
    M <- matrix(A[i, , ], nrow = dim(A)[2L], ncol = dim(A)[3L])
    if (nrow(M) > 1L) h_v <- max(h_v, max(0, -diff(M)))
    if (ncol(M) > 1L) tau_v <- max(tau_v, max(0, -t(apply(M, 1L, diff))))
  }
  data.frame(
    MinimumPrediction = neg,
    MaximumHorizonViolation = h_v,
    MaximumQuantileViolation = tau_v,
    Passed = neg >= -tolerance && h_v <= tolerance && tau_v <= tolerance
  )
}

strict_project_array <- function(A, context = "shape-constrained surface",
                                 audit_tolerance = CFG$surface_audit_tolerance) {
  candidate <- A
  attempts <- as.integer(CFG$projection_retry_multipliers)
  if (!length(attempts) || any(!is.finite(attempts)) || any(attempts < 1L)) {
    stop("Invalid projection retry multipliers.")
  }
  last_audit <- NULL
  for (attempt in seq_along(attempts)) {
    candidate <- project_array_exact(
      candidate,
      tol = min(CFG$projection_tol, audit_tolerance / 100),
      max_iter = as.integer(CFG$projection_max_iter * attempts[attempt])
    )
    last_audit <- surface_audit(candidate, tolerance = audit_tolerance)
    if (isTRUE(last_audit$Passed[1L])) {
      attr(candidate, "projection_attempts") <- attempt
      return(candidate)
    }
    log_msg(
      "Projection audit retry ", attempt, " for ", context,
      ": min=", signif(last_audit$MinimumPrediction, 5L),
      ", horizon violation=", signif(last_audit$MaximumHorizonViolation, 5L),
      ", quantile violation=", signif(last_audit$MaximumQuantileViolation, 5L),
      level = "WARN"
    )
  }
  stop(
    "Projection audit failed after ", length(attempts), " attempts for ", context,
    "; min=", signif(last_audit$MinimumPrediction, 6L),
    ", horizon violation=", signif(last_audit$MaximumHorizonViolation, 6L),
    ", quantile violation=", signif(last_audit$MaximumQuantileViolation, 6L)
  )
}

# A deterministic counterexample protects against accidental replacement of
# Dykstra by ordinary cyclic projections.
projection_unit_test <- function() {
  M <- matrix(c(2, 4.4, 3.3, 3.6, 3.3, 6.5, 3.2, 8.8, 8.7), 3, 3, byrow = TRUE)
  z <- dykstra_project_surface(M, return_diagnostics = TRUE)
  expected_objective <- 0.8916666666667
  ok <- isTRUE(z$diagnostics$converged) &&
    abs(z$diagnostics$objective - expected_objective) < 1e-6 &&
    z$diagnostics$min_value >= -1e-10 &&
    z$diagnostics$max_horizon_violation <= 1e-8 &&
    z$diagnostics$max_quantile_violation <= 1e-8
  if (!ok) stop("Exact-projection unit test failed.")
  data.frame(
    Test = "Exact Dykstra projection counterexample",
    Objective = z$diagnostics$objective,
    ExpectedObjective = expected_objective,
    Passed = ok
  )
}

# ---- Functional depth and weighted quantile regression -----------------------
fm_depth <- function(Y) {
  Y <- as.matrix(Y)
  n <- nrow(Y)
  if (n < 2L) return(rep(1, n))
  U <- apply(Y, 2L, function(z) (rank(z, ties.method = "average") - 0.5) / n)
  if (is.null(dim(U))) U <- matrix(U, ncol = 1L)
  rowMeans(1 - abs(2 * U - 1))
}

make_depth_weights <- function(Y, gamma, wmin, wmax = CFG$wmax) {
  D <- fm_depth(Y)
  if (gamma == 0) {
    raw <- rep(1, length(D))
  } else {
    raw <- pmax(D, .Machine$double.eps)^gamma
    raw <- raw / mean(raw)
  }
  clipped <- pmin(pmax(raw, wmin), wmax)
  weights <- clipped / mean(clipped)
  list(
    weights = weights,
    depth = D,
    diagnostics = data.frame(
      Gamma = gamma,
      WMin = wmin,
      WMax = wmax,
      DepthMin = min(D),
      DepthMedian = stats::median(D),
      DepthMax = max(D),
      WeightMin = min(weights),
      WeightMedian = stats::median(weights),
      WeightMax = max(weights),
      LowerClipShare = mean(raw < wmin),
      UpperClipShare = mean(raw > wmax)
    )
  )
}

fit_quantile_surface <- function(X, Y, taus, weights = NULL,
                                 method = CFG$qr_method, context = "model") {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  if (nrow(X) != nrow(Y)) stop("X and Y row counts differ in ", context)
  if (is.null(weights)) weights <- rep(1, nrow(X))
  if (length(weights) != nrow(X) || any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Invalid quantile-regression weights in ", context)
  }

  dat <- as.data.frame(X)
  names(dat) <- paste0("X", seq_len(ncol(X)))
  dat$y <- 0
  form <- stats::as.formula(paste("y ~", paste(names(dat)[names(dat) != "y"], collapse = " + ")))
  beta <- array(
    NA_real_,
    dim = c(ncol(X) + 1L, ncol(Y), length(taus)),
    dimnames = list(c("Intercept", colnames(X) %||% paste0("X", seq_len(ncol(X)))),
                    paste0("H", seq_len(ncol(Y))), paste0("tau", taus))
  )
  failures <- character(0L)

  for (h in seq_len(ncol(Y))) {
    dat$y <- Y[, h]
    for (k in seq_along(taus)) {
      fit <- tryCatch(
        suppressWarnings(quantreg::rq(
          form, data = dat, tau = taus[k], weights = weights, method = method
        )),
        error = function(e) e
      )
      b <- if (inherits(fit, "error")) NULL else as.numeric(stats::coef(fit))
      if (is.null(b) || length(b) != ncol(X) + 1L || any(!is.finite(b))) {
        retry <- tryCatch(quantreg::rq(
          form, data = dat, tau = taus[k], weights = weights, method = "br"
        ), error = function(e) NULL)
        b <- if (is.null(retry)) NULL else as.numeric(stats::coef(retry))
        if (is.null(b) || length(b) != ncol(X) + 1L || any(!is.finite(b))) {
          stop("Both QR solvers failed in ", context, ", h=", h, ", tau=", taus[k])
        }
      }
      beta[, h, k] <- b
    }
  }
  list(beta = beta, taus = taus, H = ncol(Y), failures = failures, context = context)
}

predict_quantile_surface <- function(model, Xnew) {
  Xnew <- as.matrix(Xnew)
  X1 <- cbind(1, Xnew)
  n <- nrow(Xnew)
  H <- dim(model$beta)[2L]
  Tn <- dim(model$beta)[3L]
  out <- array(NA_real_, dim = c(n, H, Tn))
  for (h in seq_len(H)) {
    for (k in seq_len(Tn)) out[, h, k] <- as.numeric(X1 %*% model$beta[, h, k])
  }
  out
}

depth_configuration_grid <- function(gammas = CFG$gamma_grid,
                                     wmins = CFG$wmin_grid,
                                     wmaxs = CFG$wmax) {
  z <- expand.grid(
    Gamma = gammas, WMin = wmins, WMax = wmaxs, KEEP.OUT.ATTRS = FALSE
  )
  z <- z[!(z$Gamma == 0 & duplicated(z$Gamma)), , drop = FALSE]
  z$WMin[z$Gamma == 0] <- 1
  z$WMax[z$Gamma == 0] <- 1
  z$ConfigID <- sprintf("g%g_wmin%g_wmax%g", z$Gamma, z$WMin, z$WMax)
  rownames(z) <- NULL
  z
}

# ---- Proper scores and loss diagnostics --------------------------------------
pinball_loss <- function(y, q, tau) (y - q) * (tau - as.numeric(y < q))

mean_pinball_score <- function(Y, Q, taus) {
  stopifnot(length(dim(Q)) == 3L, dim(Q)[3L] == length(taus))
  losses <- vapply(seq_along(taus), function(k) {
    mean(pinball_loss(as.vector(Y), as.vector(Q[, , k]), taus[k]), na.rm = TRUE)
  }, numeric(1L))
  mean(losses)
}

observation_pinball_loss <- function(Y, Q, taus) {
  n <- nrow(Y)
  out <- numeric(n)
  for (i in seq_len(n)) {
    val <- 0
    for (k in seq_along(taus)) {
      val <- val + mean(pinball_loss(Y[i, ], Q[i, , k], taus[k]))
    }
    out[i] <- val / length(taus)
  }
  out
}

crps_quantile_approx <- function(Y, Q, taus) {
  ord <- order(taus)
  taus <- taus[ord]
  Q <- Q[, , ord, drop = FALSE]
  if (length(taus) < 2L) return(NA_real_)

  y <- as.vector(Y)
  losses <- matrix(NA_real_, nrow = length(y), ncol = length(taus) + 2L)
  tau_aug <- c(0, taus, 1)
  losses[, 1L] <- pinball_loss(y, rep(0, length(y)), 0)
  for (k in seq_along(taus)) {
    losses[, k + 1L] <- pinball_loss(y, as.vector(Q[, , k]), taus[k])
  }

  q_last <- as.vector(Q[, , length(taus)])
  q_prev <- as.vector(Q[, , length(taus) - 1L])
  upper_slope <- pmax(0, (q_last - q_prev) / (taus[length(taus)] - taus[length(taus) - 1L]))
  q_one <- q_last + upper_slope * (1 - taus[length(taus)])
  losses[, ncol(losses)] <- pinball_loss(y, q_one, 1)

  integral <- numeric(length(y))
  for (k in seq_len(length(tau_aug) - 1L)) {
    integral <- integral + 0.5 * (losses[, k] + losses[, k + 1L]) *
      (tau_aug[k + 1L] - tau_aug[k])
  }
  2 * mean(integral, na.rm = TRUE)
}

weighted_interval_score <- function(Y, Q, taus) {
  find_tau <- function(x) {
    k <- which(abs(taus - x) < 1e-10)
    if (length(k)) k[1L] else NA_integer_
  }
  km <- find_tau(0.50)
  alphas <- c(0.10, 0.20, 0.50)
  pairs <- lapply(alphas, function(a) c(find_tau(a / 2), find_tau(1 - a / 2)))
  valid <- !is.na(km) && all(vapply(pairs, function(z) all(!is.na(z)), logical(1L)))
  if (!valid) return(NA_real_)

  y <- as.vector(Y)
  med <- as.vector(Q[, , km])
  numerator <- 0.5 * abs(y - med)
  for (j in seq_along(alphas)) {
    a <- alphas[j]
    lo <- as.vector(Q[, , pairs[[j]][1L]])
    hi <- as.vector(Q[, , pairs[[j]][2L]])
    interval_score <- (hi - lo) + (2 / a) * (lo - y) * (y < lo) +
      (2 / a) * (y - hi) * (y > hi)
    numerator <- numerator + (a / 2) * interval_score
  }
  mean(numerator / (length(alphas) + 0.5), na.rm = TRUE)
}

quantile_calibration <- function(Y, Q, taus) {
  data.frame(
    Tau = taus,
    ObservedCoverage = vapply(seq_along(taus), function(k) {
      mean(as.vector(Y) <= as.vector(Q[, , k]), na.rm = TRUE)
    }, numeric(1L))
  )
}

score_surface <- function(Y, Q, taus) {
  cal <- quantile_calibration(Y, Q, taus)
  exact_coverage <- function(target) {
    k <- which(abs(cal$Tau - target) < 1e-10)
    if (length(k)) cal$ObservedCoverage[k[1L]] else NA_real_
  }
  data.frame(
    MeanPinball = mean_pinball_score(Y, Q, taus),
    CRPSApprox = crps_quantile_approx(Y, Q, taus),
    WIS = weighted_interval_score(Y, Q, taus),
    Coverage05 = exact_coverage(0.05),
    Coverage50 = exact_coverage(0.50),
    Coverage95 = exact_coverage(0.95),
    CalibrationMAE = mean(abs(cal$ObservedCoverage - taus))
  )
}

tail_surface_diagnostics <- function(Y, Q, taus, targets = c(0.05, 0.95)) {
  rows <- lapply(targets, function(target) {
    k <- which(abs(taus - target) < 1e-10)
    if (!length(k)) stop("Requested tail quantile is absent from the grid: ", target)
    y <- as.vector(Y)
    q <- as.vector(Q[, , k[1L]])
    observed <- mean(y <= q, na.rm = TRUE)
    data.frame(
      Tau = target,
      ObservedCoverage = observed,
      CalibrationError = observed - target,
      AbsoluteCalibrationError = abs(observed - target),
      MeanPinball = mean(pinball_loss(y, q, target), na.rm = TRUE),
      MeanPredictedQuantile = mean(q, na.rm = TRUE),
      MeanObservedTarget = mean(y, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

compare_weighted_unweighted_tails <- function(Y, Qunweighted, Qweighted, taus,
                                               asset = NA_character_,
                                               selected_row = NULL) {
  u <- tail_surface_diagnostics(Y, Qunweighted, taus)
  w <- tail_surface_diagnostics(Y, Qweighted, taus)
  names(u)[names(u) != "Tau"] <- paste0(names(u)[names(u) != "Tau"], "Unweighted")
  names(w)[names(w) != "Tau"] <- paste0(names(w)[names(w) != "Tau"], "Weighted")
  out <- merge(u, w, by = "Tau", sort = FALSE)
  out$Asset <- asset
  out$WeightedMinusUnweightedCoverage <-
    out$ObservedCoverageWeighted - out$ObservedCoverageUnweighted
  out$WeightedMinusUnweightedCalibrationError <-
    out$CalibrationErrorWeighted - out$CalibrationErrorUnweighted
  out$WeightedMinusUnweightedAbsoluteCalibrationError <-
    out$AbsoluteCalibrationErrorWeighted - out$AbsoluteCalibrationErrorUnweighted
  out$WeightedMinusUnweightedPinball <-
    out$MeanPinballWeighted - out$MeanPinballUnweighted
  out$WeightedMinusUnweightedPredictedQuantile <-
    out$MeanPredictedQuantileWeighted - out$MeanPredictedQuantileUnweighted
  if (!is.null(selected_row) && nrow(selected_row)) {
    out$SelectedGamma <- selected_row$Gamma[1L]
    out$SelectedWMin <- selected_row$WMin[1L]
    out$SelectedWMax <- selected_row$WMax[1L]
  }
  out[, c("Asset", "Tau", setdiff(names(out), c("Asset", "Tau"))), drop = FALSE]
}

moving_block_mean_ci <- function(x, block_length, reps = 1000L,
                                 level = 0.95, seed = CFG$seed) {
  x <- as.numeric(x[is.finite(x)])
  n <- length(x)
  if (n < 2L) {
    return(c(
      Mean = finite_mean(x), Lower = NA, Upper = NA,
      TwoSidedPValue = NA, BootstrapReplications = 0
    ))
  }
  B <- max(1L, min(as.integer(block_length), n))
  starts <- seq_len(n - B + 1L)
  vals <- with_seed(seed, replicate(reps, {
    take <- integer(0L)
    while (length(take) < n) {
      s <- sample(starts, 1L)
      take <- c(take, s:(s + B - 1L))
    }
    mean(x[take[seq_len(n)]])
  }))
  alpha <- 1 - level
  # The p-value is computed from the same non-centred bootstrap distribution
  # that defines the percentile interval. Consequently, a pointwise two-sided
  # interval excludes zero if and only if the corresponding two-sided bootstrap
  # sign test rejects at the same nominal level, up to Monte Carlo discreteness.
  p_left <- (1 + sum(vals <= 0)) / (length(vals) + 1)
  p_right <- (1 + sum(vals >= 0)) / (length(vals) + 1)
  c(
    Mean = mean(x),
    Lower = quantile_type8(vals, alpha / 2),
    Upper = quantile_type8(vals, 1 - alpha / 2),
    TwoSidedPValue = min(1, 2 * min(p_left, p_right)),
    BootstrapReplications = length(vals)
  )
}

# ---- Leakage-safe benchmarks --------------------------------------------------
make_training_climatology <- function(Ytrain, n_forecast, taus) {
  H <- ncol(Ytrain)
  out <- array(NA_real_, dim = c(n_forecast, H, length(taus)))
  for (h in seq_len(H)) {
    qh <- quantile_type8(Ytrain[, h], taus)
    out[, h, ] <- matrix(rep(qh, times = n_forecast), nrow = n_forecast, byrow = TRUE)
  }
  project_array_exact(out)
}

make_curve_persistence <- function(dataset, forecast_idx, taus) {
  H <- dataset$H
  out <- array(NA_real_, dim = c(length(forecast_idx), H, length(taus)))
  for (ii in seq_along(forecast_idx)) {
    pos <- dataset$origin_pos[forecast_idx[ii]]
    if (pos - H + 1L < 1L) stop("Insufficient history for curve persistence.")
    past_curve <- cumsum(rev(dataset$squared_returns[(pos - H + 1L):pos]))
    out[ii, , ] <- matrix(rep(past_curve, times = length(taus)), nrow = H, ncol = length(taus))
  }
  project_array_exact(out)
}

make_rolling_historical_quantiles <- function(dataset, forecast_idx, taus,
                                              history = CFG$rolling_history,
                                              fallback_idx = NULL) {
  H <- dataset$H
  out <- array(NA_real_, dim = c(length(forecast_idx), H, length(taus)))
  for (ii in seq_along(forecast_idx)) {
    f <- forecast_idx[ii]
    eligible <- which(dataset$target_end_date < dataset$origin_date[f])
    eligible <- eligible[eligible < f]
    if (length(eligible) > history) eligible <- tail(eligible, history)
    if (length(eligible) < 30L) eligible <- fallback_idx %||% eligible
    if (!length(eligible)) stop("No leakage-safe history for rolling benchmark.")
    for (h in seq_len(H)) out[ii, h, ] <- quantile_type8(dataset$Y[eligible, h], taus)
  }
  project_array_exact(out)
}

# ---- Simultaneous block-calibrated CQR bands ----------------------------------
finite_sample_upper_quantile <- function(x, alpha) {
  x <- sort(as.numeric(x[is.finite(x)]))
  n <- length(x)
  if (!n) stop("No finite calibration scores.")
  rank <- min(n, ceiling((n + 1) * (1 - alpha)))
  x[rank]
}

# The upper bootstrap guard protects against downward quantile-estimation noise.
# It is a prespecified conservative calibration rule, not an exchangeability-based
# finite-sample guarantee under arbitrary serial dependence.
moving_block_calibration_quantile <- function(scores, alpha, block_length,
                                              reps, seed,
                                              guard_probability =
                                                CFG$block_bootstrap_guard_probability) {
  scores <- as.numeric(scores[is.finite(scores)])
  n <- length(scores)
  if (!n) stop("No calibration scores.")
  guard_probability <- max(as.numeric(guard_probability), 1 - alpha)
  B <- max(1L, min(as.integer(block_length), n))
  direct <- finite_sample_upper_quantile(scores, alpha)
  if (B == 1L || reps <= 1L) {
    return(list(
      qhat = max(0, direct),
      direct_quantile = direct,
      bootstrap_median = direct,
      bootstrap_guard_quantile = direct,
      guard_probability = NA_real_,
      rule = "Nonnegative direct finite-sample quantile"
    ))
  }
  starts <- seq_len(n - B + 1L)
  q_boot <- with_seed(seed, replicate(reps, {
    take <- integer(0L)
    while (length(take) < n) {
      s <- sample(starts, 1L)
      take <- c(take, s:(s + B - 1L))
    }
    finite_sample_upper_quantile(scores[take[seq_len(n)]], alpha)
  }))
  bootstrap_median <- stats::median(q_boot)
  bootstrap_guard <- finite_quantile(q_boot, guard_probability)
  list(
    qhat = max(0, direct, bootstrap_guard),
    direct_quantile = direct,
    bootstrap_median = bootstrap_median,
    bootstrap_guard_quantile = bootstrap_guard,
    guard_probability = guard_probability,
    rule = "Nonnegative target-matched upper-guarded moving-block quantile"
  )
}

training_horizon_scale <- function(Ytrain) {
  s <- apply(Ytrain, 2L, stats::IQR, na.rm = TRUE, type = 8)
  fallback <- stats::median(s[s > 0 & is.finite(s)])
  if (!is.finite(fallback)) fallback <- stats::median(abs(Ytrain - stats::median(Ytrain)))
  if (!is.finite(fallback) || fallback <= 0) fallback <- 1
  pmax(s, fallback * 1e-4, .Machine$double.eps)
}

make_simultaneous_band <- function(Ytrain, Ycal, Qcal, Qtest, taus,
                                   alpha = CFG$primary_band_alpha,
                                   block_length = CFG$primary_block_length,
                                   block_reps = CFG$calibration_reps,
                                   seed = CFG$seed) {
  klo <- which.min(abs(taus - alpha / 2))
  khi <- which.min(abs(taus - (1 - alpha / 2)))
  if (abs(taus[klo] - alpha / 2) > 1e-8 || abs(taus[khi] - (1 - alpha / 2)) > 1e-8) {
    stop("Quantile grid does not contain the requested band endpoints.")
  }
  scale_h <- training_horizon_scale(Ytrain)
  score <- vapply(seq_len(nrow(Ycal)), function(i) {
    max(c(
      (Qcal[i, , klo] - Ycal[i, ]) / scale_h,
      (Ycal[i, ] - Qcal[i, , khi]) / scale_h
    ))
  }, numeric(1L))
  calibration <- moving_block_calibration_quantile(
    score, alpha, block_length, block_reps, seed
  )
  qhat <- calibration$qhat

  lower <- matrix(NA_real_, nrow(Qtest), ncol(Ytrain))
  upper <- matrix(NA_real_, nrow(Qtest), ncol(Ytrain))
  for (i in seq_len(nrow(Qtest))) {
    raw_lower <- Qtest[i, , klo] - qhat * scale_h
    raw_upper <- Qtest[i, , khi] + qhat * scale_h

    # Coverage-preserving shape envelope.  The lower boundary is moved only
    # downward (apart from the logically safe zero bound for nonnegative Y),
    # while the upper boundary is moved only upward.  Unlike a least-squares
    # projection of the two boundaries, this cannot shrink the raw band for a
    # nonnegative monotone response path.
    shaped_lower <- rev(cummin(rev(raw_lower)))
    shaped_lower <- pmax(shaped_lower, 0)
    shaped_upper <- cummax(raw_upper)
    shaped_upper <- pmax(shaped_upper, shaped_lower)
    lower[i, ] <- shaped_lower
    upper[i, ] <- shaped_upper
  }
  list(
    lower = lower,
    upper = upper,
    qhat = qhat,
    direct_qhat = calibration$direct_quantile,
    bootstrap_median_qhat = calibration$bootstrap_median,
    bootstrap_guard_qhat = calibration$bootstrap_guard_quantile,
    calibration_guard_probability = calibration$guard_probability,
    calibration_rule = calibration$rule,
    calibration_scores = score,
    scale_h = scale_h,
    alpha = alpha,
    block_length = block_length,
    block_reps = block_reps
  )
}

make_simultaneous_upper_band <- function(
    Ytrain, Ycal, Qcal, Qtest, taus,
    alpha = CFG$primary_upper_band_alpha,
    block_length = CFG$primary_block_length,
    block_reps = CFG$calibration_reps,
    seed = CFG$seed) {
  kupper <- which.min(abs(taus - (1 - alpha)))
  if (abs(taus[kupper] - (1 - alpha)) > 1e-8) {
    stop("Quantile grid does not contain the requested one-sided upper endpoint.")
  }
  scale_h <- training_horizon_scale(Ytrain)
  score <- vapply(seq_len(nrow(Ycal)), function(i) {
    max((Ycal[i, ] - Qcal[i, , kupper]) / scale_h)
  }, numeric(1L))
  calibration <- moving_block_calibration_quantile(
    score, alpha, block_length, block_reps, seed
  )
  qhat <- calibration$qhat

  upper <- matrix(NA_real_, nrow(Qtest), ncol(Ytrain))
  for (i in seq_len(nrow(Qtest))) {
    raw_upper <- Qtest[i, , kupper] + qhat * scale_h
    # The cumulative FRV response is nonnegative and nondecreasing in horizon.
    # cummax moves an upper boundary only upward, so shaping cannot remove a
    # point covered by the unshaped upper band.
    upper[i, ] <- pmax(cummax(raw_upper), 0)
  }
  list(
    upper = upper,
    qhat = qhat,
    direct_qhat = calibration$direct_quantile,
    bootstrap_median_qhat = calibration$bootstrap_median,
    bootstrap_guard_qhat = calibration$bootstrap_guard_quantile,
    calibration_guard_probability = calibration$guard_probability,
    calibration_rule = calibration$rule,
    calibration_scores = score,
    scale_h = scale_h,
    alpha = alpha,
    endpoint_tau = taus[kupper],
    block_length = block_length,
    block_reps = block_reps
  )
}

band_metrics <- function(Y, band) {
  inside <- Y >= band$lower & Y <= band$upper
  upper_inside <- Y <= band$upper
  lower_inside <- Y >= band$lower
  width <- band$upper - band$lower
  data.frame(
    NominalCoverage = 1 - band$alpha,
    CoordinatewiseCoverage = mean(inside),
    MinimumHorizonCoverage = min(colMeans(inside)),
    SimultaneousPathCoverage = mean(apply(inside, 1L, all)),
    UpperCoordinatewiseCoverage = mean(upper_inside),
    LowerCoordinatewiseCoverage = mean(lower_inside),
    MeanWidth = mean(width),
    MedianWidth = stats::median(width),
    CalibrationQuantile = band$qhat,
    DirectCalibrationQuantile = band$direct_qhat,
    BootstrapMedianCalibrationQuantile = band$bootstrap_median_qhat,
    CalibrationInflation = band$qhat - band$direct_qhat,
    CalibrationGuardProbability = band$calibration_guard_probability,
    CalibrationRule = band$calibration_rule,
    BlockLength = band$block_length,
    CalibrationScoreN = length(band$calibration_scores)
  )
}

upper_band_metrics <- function(Y, band) {
  inside <- Y <= band$upper
  data.frame(
    NominalCoverage = 1 - band$alpha,
    CoordinatewiseUpperCoverage = mean(inside),
    MinimumHorizonUpperCoverage = min(colMeans(inside)),
    SimultaneousUpperPathCoverage = mean(apply(inside, 1L, all)),
    MeanUpperBoundary = mean(band$upper),
    MedianUpperBoundary = stats::median(band$upper),
    CalibrationQuantile = band$qhat,
    DirectCalibrationQuantile = band$direct_qhat,
    BootstrapMedianCalibrationQuantile = band$bootstrap_median_qhat,
    CalibrationInflation = band$qhat - band$direct_qhat,
    CalibrationGuardProbability = band$calibration_guard_probability,
    CalibrationRule = band$calibration_rule,
    EndpointTau = band$endpoint_tau,
    BlockLength = band$block_length,
    CalibrationScoreN = length(band$calibration_scores)
  )
}

# ---- Training-only tuning -----------------------------------------------------
select_depth_configuration <- function(dataset, split, taus, asset) {
  internal <- make_internal_split(
    split$train, dataset$H, CFG$internal_validation_fraction
  )
  grid <- depth_configuration_grid()
  rows <- vector("list", nrow(grid))

  log_msg(asset, ": training-only depth tuning over ", nrow(grid), " configurations")
  for (g in seq_len(nrow(grid))) {
    wd <- make_depth_weights(
      dataset$Y[internal$fit, , drop = FALSE],
      gamma = grid$Gamma[g], wmin = grid$WMin[g], wmax = grid$WMax[g]
    )
    model <- fit_quantile_surface(
      dataset$X[internal$fit, , drop = FALSE],
      dataset$Y[internal$fit, , drop = FALSE],
      taus, weights = wd$weights,
      context = paste(asset, "internal", grid$ConfigID[g])
    )
    q_val_raw <- predict_quantile_surface(
      model, dataset$X[internal$validation, , drop = FALSE]
    )
    q_val <- strict_project_array(
      q_val_raw, context = paste(asset, "internal validation", grid$ConfigID[g])
    )
    score <- score_surface(dataset$Y[internal$validation, , drop = FALSE], q_val, taus)
    rows[[g]] <- cbind(
      data.frame(
        Asset = asset,
        ConfigID = grid$ConfigID[g],
        Gamma = grid$Gamma[g],
        WMin = grid$WMin[g],
        WMax = grid$WMax[g],
        InternalFitN = length(internal$fit),
        InternalValidationN = length(internal$validation),
        InternalEmbargoN = length(internal$embargo),
        QRFailures = length(model$failures),
        stringsAsFactors = FALSE
      ),
      wd$diagnostics[, setdiff(names(wd$diagnostics), c("Gamma", "WMin", "WMax")), drop = FALSE],
      score
    )
  }
  leaderboard <- do.call(rbind, rows)
  criterion <- ifelse(is.finite(leaderboard$WIS), leaderboard$WIS, leaderboard$MeanPinball)
  best <- which.min(criterion)
  leaderboard$Selected <- seq_len(nrow(leaderboard)) == best
  list(
    selected = leaderboard[best, , drop = FALSE],
    leaderboard = leaderboard,
    internal_split = internal
  )
}

fit_empirical_bundle <- function(dataset, split, taus, selected_row, asset) {
  train <- split$train
  cal <- split$cal
  test <- split$test

  unweighted <- rep(1, length(train))
  selected_weights <- make_depth_weights(
    dataset$Y[train, , drop = FALSE],
    gamma = selected_row$Gamma,
    wmin = selected_row$WMin,
    wmax = selected_row$WMax
  )

  model_unweighted <- fit_quantile_surface(
    dataset$X[train, , drop = FALSE], dataset$Y[train, , drop = FALSE],
    taus, unweighted, context = paste(asset, "unweighted final")
  )
  model_selected <- fit_quantile_surface(
    dataset$X[train, , drop = FALSE], dataset$Y[train, , drop = FALSE],
    taus, selected_weights$weights, context = paste(asset, "selected final")
  )

  q_cal_unweighted_raw <- predict_quantile_surface(
    model_unweighted, dataset$X[cal, , drop = FALSE]
  )
  q_test_unweighted_raw <- predict_quantile_surface(
    model_unweighted, dataset$X[test, , drop = FALSE]
  )
  q_cal_selected_raw <- predict_quantile_surface(
    model_selected, dataset$X[cal, , drop = FALSE]
  )
  q_test_selected_raw <- predict_quantile_surface(
    model_selected, dataset$X[test, , drop = FALSE]
  )

  q_cal_unweighted_shape <- strict_project_array(
    q_cal_unweighted_raw, context = paste(asset, "unweighted calibration")
  )
  q_test_unweighted_shape <- strict_project_array(
    q_test_unweighted_raw, context = paste(asset, "unweighted test")
  )
  q_cal_selected_shape <- strict_project_array(
    q_cal_selected_raw, context = paste(asset, "selected calibration")
  )
  q_test_selected_shape <- strict_project_array(
    q_test_selected_raw, context = paste(asset, "selected test")
  )

  test_predictions <- list(
    "QR raw" = q_test_unweighted_raw,
    "QR + quantile order" = project_tau_only_array(q_test_unweighted_raw, nonnegative = TRUE),
    "QR + exact 2I+" = q_test_unweighted_shape,
    "Depth QR raw" = q_test_selected_raw,
    "Selected 2I-FQR" = q_test_selected_shape,
    "Training climatology" = make_training_climatology(
      dataset$Y[train, , drop = FALSE], length(test), taus
    ),
    "Curve persistence" = make_curve_persistence(dataset, test, taus),
    "Rolling historical quantiles" = make_rolling_historical_quantiles(
      dataset, test, taus, history = CFG$rolling_history, fallback_idx = train
    )
  )

  list(
    model_unweighted = model_unweighted,
    model_selected = model_selected,
    selected_weight_info = selected_weights,
    q_cal_unweighted_shape = q_cal_unweighted_shape,
    q_test_unweighted_shape = q_test_unweighted_shape,
    q_cal_selected_shape = q_cal_selected_shape,
    q_test_selected_shape = q_test_selected_shape,
    test_predictions = test_predictions
  )
}

evaluate_method_bundle <- function(dataset, split, bundle, taus, asset) {
  Ytest <- dataset$Y[split$test, , drop = FALSE]
  metric_rows <- list()
  audit_rows <- list()
  loss_by_method <- list()

  for (method in names(bundle$test_predictions)) {
    Q <- bundle$test_predictions[[method]]
    metric_rows[[method]] <- cbind(
      data.frame(Asset = asset, Method = method, stringsAsFactors = FALSE),
      score_surface(Ytest, Q, taus)
    )
    a <- surface_audit(Q)
    audit_rows[[method]] <- cbind(
      data.frame(Asset = asset, Method = method, stringsAsFactors = FALSE), a
    )
    loss_by_method[[method]] <- observation_pinball_loss(Ytest, Q, taus)
  }

  proposed_loss <- loss_by_method[["Selected 2I-FQR"]]
  comparator_methods <- intersect(
    CFG$paired_inference_comparators, names(loss_by_method)
  )
  ci_rows <- lapply(comparator_methods, function(method) {
    gain <- loss_by_method[[method]] - proposed_loss
    if (any(!is.finite(gain))) stop("Nonfinite paired loss differences for ", asset)
    ci <- moving_block_mean_ci(
      gain, block_length = dataset$H, reps = CFG$block_boot_reps,
      seed = CFG$seed + match(method, names(loss_by_method))
    )
    data.frame(
      Asset = asset,
      Comparator = method,
      MeanPinballGainOfSelected = unname(ci["Mean"]),
      Lower95 = unname(ci["Lower"]),
      Upper95 = unname(ci["Upper"]),
      RawTwoSidedPValue = unname(ci["TwoSidedPValue"]),
      BootstrapReplications = as.integer(ci["BootstrapReplications"]),
      stringsAsFactors = FALSE
    )
  })

  list(
    metrics = do.call(rbind, metric_rows),
    audits = do.call(rbind, audit_rows),
    paired_ci = rbind_fill(ci_rows),
    observation_losses = loss_by_method
  )
}

evaluate_horizon_subsets <- function(dataset, split, bundle, taus, asset) {
  Ytest <- dataset$Y[split$test, , drop = FALSE]
  subsets <- unique(pmin(c(5L, 10L, dataset$H), dataset$H))
  rows <- list()
  for (Hs in subsets) {
    method_scores <- lapply(names(bundle$test_predictions), function(method) {
      Q <- bundle$test_predictions[[method]][, seq_len(Hs), , drop = FALSE]
      Y <- Ytest[, seq_len(Hs), drop = FALSE]
      data.frame(
        Asset = asset,
        HorizonSubset = paste0("H1-", Hs),
        HorizonCount = Hs,
        Method = method,
        MeanPinball = mean_pinball_score(Y, Q, taus),
        CRPSApprox = crps_quantile_approx(Y, Q, taus),
        WIS = weighted_interval_score(Y, Q, taus),
        stringsAsFactors = FALSE
      )
    })
    scores <- do.call(rbind, method_scores)
    proposed <- scores[scores$Method == "Selected 2I-FQR", , drop = FALSE]
    scores$MeanPinballGainPctOfSelected <- 100 *
      (scores$MeanPinball - proposed$MeanPinball) / scores$MeanPinball
    scores$CRPSGainPctOfSelected <- 100 *
      (scores$CRPSApprox - proposed$CRPSApprox) / scores$CRPSApprox
    scores$WISGainPctOfSelected <- 100 *
      (scores$WIS - proposed$WIS) / scores$WIS
    rows[[as.character(Hs)]] <- scores
  }
  do.call(rbind, rows)
}

evaluate_bands <- function(dataset, split, bundle, taus, asset) {
  methods <- list(
    "QR + exact 2I+" = list(
      cal = bundle$q_cal_unweighted_shape,
      test = bundle$q_test_unweighted_shape
    ),
    "Selected 2I-FQR" = list(
      cal = bundle$q_cal_selected_shape,
      test = bundle$q_test_selected_shape
    )
  )
  rows <- list()
  bands <- list()
  for (method in names(methods)) {
    band <- make_simultaneous_band(
      Ytrain = dataset$Y[split$train, , drop = FALSE],
      Ycal = dataset$Y[split$cal, , drop = FALSE],
      Qcal = methods[[method]]$cal,
      Qtest = methods[[method]]$test,
      taus = taus,
      alpha = CFG$primary_band_alpha,
      block_length = CFG$primary_block_length,
      block_reps = CFG$calibration_reps,
      seed = empirical_band_seed(
        asset, method, "two-sided", CFG$primary_block_length
      )
    )
    rows[[method]] <- cbind(
      data.frame(Asset = asset, Method = method, stringsAsFactors = FALSE),
      band_metrics(dataset$Y[split$test, , drop = FALSE], band)
    )
    bands[[method]] <- band
  }
  list(metrics = do.call(rbind, rows), bands = bands)
}

evaluate_upper_bands <- function(dataset, split, bundle, taus, asset) {
  methods <- list(
    "QR + exact 2I+" = list(
      cal = bundle$q_cal_unweighted_shape,
      test = bundle$q_test_unweighted_shape
    ),
    "Selected 2I-FQR" = list(
      cal = bundle$q_cal_selected_shape,
      test = bundle$q_test_selected_shape
    )
  )
  rows <- list()
  bands <- list()
  for (method in names(methods)) {
    band <- make_simultaneous_upper_band(
      Ytrain = dataset$Y[split$train, , drop = FALSE],
      Ycal = dataset$Y[split$cal, , drop = FALSE],
      Qcal = methods[[method]]$cal,
      Qtest = methods[[method]]$test,
      taus = taus,
      alpha = CFG$primary_upper_band_alpha,
      block_length = CFG$primary_block_length,
      block_reps = CFG$calibration_reps,
      seed = empirical_band_seed(
        asset, method, "one-sided-upper", CFG$primary_block_length
      )
    )
    rows[[method]] <- cbind(
      data.frame(
        Asset = asset,
        Method = method,
        BandType = "One-sided upper",
        stringsAsFactors = FALSE
      ),
      upper_band_metrics(dataset$Y[split$test, , drop = FALSE], band)
    )
    bands[[method]] <- band
  }
  list(metrics = do.call(rbind, rows), bands = bands)
}

run_depth_sensitivity <- function(dataset, split, taus, asset) {
  grid <- depth_configuration_grid(wmaxs = CFG$wmax_sensitivity_grid)
  rows <- vector("list", nrow(grid))
  for (g in seq_len(nrow(grid))) {
    wd <- make_depth_weights(
      dataset$Y[split$train, , drop = FALSE],
      grid$Gamma[g], grid$WMin[g], grid$WMax[g]
    )
    model <- fit_quantile_surface(
      dataset$X[split$train, , drop = FALSE],
      dataset$Y[split$train, , drop = FALSE],
      taus, wd$weights, context = paste(asset, "depth sensitivity", grid$ConfigID[g])
    )
    Q <- strict_project_array(
      predict_quantile_surface(model, dataset$X[split$test, , drop = FALSE]),
      context = paste(asset, "depth sensitivity", grid$ConfigID[g])
    )
    rows[[g]] <- cbind(
      data.frame(
        Asset = asset,
        ConfigID = grid$ConfigID[g],
        Gamma = grid$Gamma[g],
        WMin = grid$WMin[g],
        WMax = grid$WMax[g],
        QRFailures = length(model$failures),
        stringsAsFactors = FALSE
      ),
      wd$diagnostics[, setdiff(names(wd$diagnostics), c("Gamma", "WMin", "WMax")), drop = FALSE],
      score_surface(dataset$Y[split$test, , drop = FALSE], Q, taus)
    )
  }
  do.call(rbind, rows)
}

quantile_grid_definitions <- function() {
  list(
    MedianOnly = c(0.50),
    SparseSymmetric = c(0.05, 0.25, 0.50, 0.75, 0.95),
    PrimarySymmetric = CFG$taus,
    DenseSymmetric = c(0.025, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50,
                       0.60, 0.70, 0.80, 0.90, 0.95, 0.975),
    UpperTailAsymmetric = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975, 0.99)
  )
}

run_grid_sensitivity <- function(dataset, split, selected_row, asset) {
  wd <- make_depth_weights(
    dataset$Y[split$train, , drop = FALSE],
    selected_row$Gamma, selected_row$WMin, selected_row$WMax
  )
  grids <- quantile_grid_definitions()
  rows <- list()
  for (grid_name in names(grids)) {
    taus <- grids[[grid_name]]
    model <- fit_quantile_surface(
      dataset$X[split$train, , drop = FALSE],
      dataset$Y[split$train, , drop = FALSE],
      taus, wd$weights, context = paste(asset, "grid", grid_name)
    )
    Q <- strict_project_array(
      predict_quantile_surface(model, dataset$X[split$test, , drop = FALSE]),
      context = paste(asset, "quantile-grid sensitivity", grid_name)
    )
    score <- score_surface(dataset$Y[split$test, , drop = FALSE], Q, taus)
    common_target <- c(0.05, 0.50, 0.95)
    common <- intersect(taus, common_target)
    common_score <- if (length(common) == length(common_target)) {
      idx <- match(common, taus)
      mean_pinball_score(
        dataset$Y[split$test, , drop = FALSE], Q[, , idx, drop = FALSE], common
      )
    } else NA_real_
    median_idx <- which(abs(taus - 0.5) < 1e-10)
    median_pinball <- if (length(median_idx)) {
      mean(pinball_loss(
        as.vector(dataset$Y[split$test, , drop = FALSE]),
        as.vector(Q[, , median_idx[1L]]), 0.5
      ))
    } else NA_real_
    rows[[grid_name]] <- cbind(
      data.frame(
        Asset = asset,
        Grid = grid_name,
        QuantileCount = length(taus),
        Quantiles = paste(taus, collapse = ","),
        CommonGridPinball = common_score,
        MedianPinball = median_pinball,
        QRFailures = length(model$failures),
        stringsAsFactors = FALSE
      ),
      score
    )
  }
  do.call(rbind, rows)
}

run_block_sensitivity <- function(dataset, split, bundle, taus, asset) {
  rows <- list()
  for (B in unique(CFG$block_length_grid)) {
    band <- make_simultaneous_band(
      Ytrain = dataset$Y[split$train, , drop = FALSE],
      Ycal = dataset$Y[split$cal, , drop = FALSE],
      Qcal = bundle$q_cal_selected_shape,
      Qtest = bundle$q_test_selected_shape,
      taus = taus,
      alpha = CFG$primary_band_alpha,
      block_length = B,
      block_reps = CFG$calibration_reps,
      seed = empirical_band_seed(asset, "Selected 2I-FQR", "two-sided", B)
    )
    rows[[as.character(B)]] <- cbind(
      data.frame(Asset = asset, Method = "Selected 2I-FQR", stringsAsFactors = FALSE),
      band_metrics(dataset$Y[split$test, , drop = FALSE], band)
    )
  }
  do.call(rbind, rows)
}

run_upper_block_sensitivity <- function(dataset, split, bundle, taus, asset) {
  rows <- list()
  for (B in unique(CFG$block_length_grid)) {
    band <- make_simultaneous_upper_band(
      Ytrain = dataset$Y[split$train, , drop = FALSE],
      Ycal = dataset$Y[split$cal, , drop = FALSE],
      Qcal = bundle$q_cal_selected_shape,
      Qtest = bundle$q_test_selected_shape,
      taus = taus,
      alpha = CFG$primary_upper_band_alpha,
      block_length = B,
      block_reps = CFG$calibration_reps,
      seed = empirical_band_seed(
        asset, "Selected 2I-FQR", "one-sided-upper", B
      )
    )
    rows[[as.character(B)]] <- cbind(
      data.frame(
        Asset = asset,
        Method = "Selected 2I-FQR",
        BandType = "One-sided upper",
        stringsAsFactors = FALSE
      ),
      upper_band_metrics(dataset$Y[split$test, , drop = FALSE], band)
    )
  }
  do.call(rbind, rows)
}

descriptive_statistics <- function(price_df, asset) {
  r <- diff(log(price_df$Adjusted))
  r2 <- r^2
  skewness <- function(x) mean((x - mean(x))^3) / stats::sd(x)^3
  kurtosis <- function(x) mean((x - mean(x))^4) / stats::sd(x)^4
  data.frame(
    Asset = asset,
    PriceN = nrow(price_df),
    ReturnN = length(r),
    StartDate = as.character(min(price_df$Date)),
    EndDate = as.character(max(price_df$Date)),
    MeanReturn = mean(r),
    SDReturn = stats::sd(r),
    Skewness = skewness(r),
    Kurtosis = kurtosis(r),
    MeanSquaredReturn = mean(r2),
    SDSquaredReturn = stats::sd(r2),
    stringsAsFactors = FALSE
  )
}

run_empirical_asset <- function(asset, prices = NULL) {
  log_msg("Empirical analysis started for ", asset)
  if (is.null(prices)) prices <- download_adjusted_prices(
    asset, CFG$start_date, CFG$end_date, CFG$refresh_data
  )
  dataset <- build_curve_dataset_from_prices(prices, CFG$H, CFG$har_windows)
  split <- make_embargo_split(
    nrow(dataset$Y), CFG$H,
    CFG$split_train_end, CFG$split_cal_end, CFG$embargo
  )
  split_audit <- audit_split_dates(dataset, split, asset)
  if (!all(split_audit$TrainCalibrationNoOverlap, split_audit$CalibrationTestNoOverlap)) {
    stop("Target-window overlap detected for ", asset)
  }

  tuning <- select_depth_configuration(dataset, split, CFG$taus, asset)
  selected <- tuning$selected
  log_msg(
    asset, ": selected gamma=", selected$Gamma,
    ", wmin=", selected$WMin, ", wmax=", selected$WMax,
    " using internal validation only"
  )
  bundle <- fit_empirical_bundle(dataset, split, CFG$taus, selected, asset)
  evaluation <- evaluate_method_bundle(dataset, split, bundle, CFG$taus, asset)
  horizon_subsets <- evaluate_horizon_subsets(dataset, split, bundle, CFG$taus, asset)
  bands <- evaluate_bands(dataset, split, bundle, CFG$taus, asset)
  upper_bands <- evaluate_upper_bands(dataset, split, bundle, CFG$taus, asset)
  tail_weighting_comparison <- compare_weighted_unweighted_tails(
    Y = dataset$Y[split$test, , drop = FALSE],
    Qunweighted = bundle$q_test_unweighted_shape,
    Qweighted = bundle$q_test_selected_shape,
    taus = CFG$taus,
    asset = asset,
    selected_row = selected
  )
  calibration <- quantile_calibration(
    dataset$Y[split$test, , drop = FALSE], bundle$q_test_selected_shape, CFG$taus
  )
  calibration$Asset <- asset
  calibration$Method <- "Selected 2I-FQR"

  depth_sensitivity <- if (CFG$run_depth_sensitivity) {
    run_depth_sensitivity(dataset, split, CFG$taus, asset)
  } else NULL
  grid_sensitivity <- if (CFG$run_grid_sensitivity) {
    run_grid_sensitivity(dataset, split, selected, asset)
  } else NULL
  block_sensitivity <- if (CFG$run_block_sensitivity) {
    run_block_sensitivity(dataset, split, bundle, CFG$taus, asset)
  } else NULL
  upper_block_sensitivity <- if (CFG$run_block_sensitivity) {
    run_upper_block_sensitivity(dataset, split, bundle, CFG$taus, asset)
  } else NULL

  # Horizon profiles for the primary simultaneous band.
  primary_band <- bands$bands[["Selected 2I-FQR"]]
  inside <- dataset$Y[split$test, , drop = FALSE] >= primary_band$lower &
    dataset$Y[split$test, , drop = FALSE] <= primary_band$upper
  width_profile <- data.frame(
    Asset = asset,
    Horizon = seq_len(CFG$H),
    Coverage = colMeans(inside),
    MeanWidth = colMeans(primary_band$upper - primary_band$lower),
    stringsAsFactors = FALSE
  )
  primary_upper_band <- upper_bands$bands[["Selected 2I-FQR"]]
  upper_inside <- dataset$Y[split$test, , drop = FALSE] <= primary_upper_band$upper
  upper_profile <- data.frame(
    Asset = asset,
    Horizon = seq_len(CFG$H),
    UpperCoverage = colMeans(upper_inside),
    MeanUpperBoundary = colMeans(primary_upper_band$upper),
    stringsAsFactors = FALSE
  )

  result <- list(
    asset = asset,
    prices = prices,
    dataset = dataset,
    split = split,
    split_audit = split_audit,
    tuning = tuning,
    bundle = bundle,
    evaluation = evaluation,
    horizon_subsets = horizon_subsets,
    bands = bands,
    upper_bands = upper_bands,
    calibration = calibration,
    tail_weighting_comparison = tail_weighting_comparison,
    depth_sensitivity = depth_sensitivity,
    grid_sensitivity = grid_sensitivity,
    block_sensitivity = block_sensitivity,
    upper_block_sensitivity = upper_block_sensitivity,
    width_profile = width_profile,
    upper_profile = upper_profile,
    descriptive = descriptive_statistics(prices, asset)
  )
  saveRDS(result, file.path(DIR$objects, paste0("empirical_", safe_name(asset), ".rds")))
  log_msg("Empirical analysis completed for ", asset)
  result
}

run_empirical_suite <- function(assets) {
  worker <- function(asset) {
    tryCatch(
      list(
        Asset = asset, Status = "ok", Error = "",
        Result = run_empirical_asset(asset)
      ),
      error = function(e) {
        list(
          Asset = asset, Status = "failed",
          Error = conditionMessage(e), Result = NULL
        )
      }
    )
  }
  ans <- lapply(assets, worker)
  success <- vapply(ans, function(z) identical(z$Status, "ok"), logical(1L))
  results <- lapply(ans[success], function(z) z$Result)
  names(results) <- vapply(
    ans[success], function(z) z$Asset, character(1L)
  )
  failures <- if (any(!success)) {
    do.call(rbind, lapply(ans[!success], function(z) {
      data.frame(
        Asset = z$Asset, Status = z$Status, Error = z$Error,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      Asset = character(0L), Status = character(0L),
      Error = character(0L), stringsAsFactors = FALSE
    )
  }
  if (nrow(failures)) {
    stop(
      "Empirical asset failure: ",
      paste(paste(failures$Asset, failures$Error, sep = ": "), collapse = "; ")
    )
  }
  list(results = results, failures = failures)
}

refresh_empirical_paired_inference <- function(empirical) {
  if (!identical(names(empirical), FIXED_PANEL)) {
    stop(
      "The reused empirical object must contain the fixed panel in this order: ",
      paste(FIXED_PANEL, collapse = ", ")
    )
  }
  for (asset in FIXED_PANEL) {
    losses <- empirical[[asset]]$evaluation$observation_losses
    if (is.null(losses) || !"Selected 2I-FQR" %in% names(losses)) {
      stop("Stored observation-level losses are unavailable for ", asset)
    }
    proposed_loss <- losses[["Selected 2I-FQR"]]
    comparators <- intersect(CFG$paired_inference_comparators, names(losses))
    rows <- lapply(comparators, function(method) {
      gain <- losses[[method]] - proposed_loss
      if (any(!is.finite(gain))) stop("Nonfinite paired loss differences for ", asset)
      ci <- moving_block_mean_ci(
        gain, block_length = CFG$H, reps = CFG$block_boot_reps,
        seed = CFG$seed + match(method, names(losses))
      )
      data.frame(
        Asset = asset,
        Comparator = method,
        MeanPinballGainOfSelected = unname(ci["Mean"]),
        Lower95 = unname(ci["Lower"]),
        Upper95 = unname(ci["Upper"]),
        RawTwoSidedPValue = unname(ci["TwoSidedPValue"]),
        BootstrapReplications = as.integer(ci["BootstrapReplications"]),
        stringsAsFactors = FALSE
      )
    })
    empirical[[asset]]$evaluation$paired_ci <- rbind_fill(rows)
  }
  empirical
}

# ---- Simulation study ---------------------------------------------------------
simulation_scenarios <- function() {
  list(
    list(
      name = "Gaussian moderate persistence",
      n = 900L, H = 30L, alpha = 0.08, beta = 0.86, df = Inf,
      contamination_prob = 0, contamination_scale = 1, regime_scale = 1
    ),
    list(
      name = "Student t5 high persistence",
      n = 900L, H = 30L, alpha = 0.08, beta = 0.90, df = 5,
      contamination_prob = 0, contamination_scale = 1, regime_scale = 1
    ),
    list(
      name = "Student t3.5 high persistence",
      n = 900L, H = 30L, alpha = 0.08, beta = 0.90, df = 3.5,
      contamination_prob = 0, contamination_scale = 1, regime_scale = 1
    ),
    list(
      name = "Student t5 with contamination",
      n = 900L, H = 30L, alpha = 0.08, beta = 0.90, df = 5,
      contamination_prob = 0.02, contamination_scale = 5, regime_scale = 1
    ),
    list(
      name = "Student t5 variance regime shift",
      n = 900L, H = 30L, alpha = 0.08, beta = 0.88, df = 5,
      contamination_prob = 0, contamination_scale = 1, regime_scale = 2.5
    ),
    list(
      name = "Student t5 short horizon",
      n = 900L, H = 10L, alpha = 0.08, beta = 0.90, df = 5,
      contamination_prob = 0, contamination_scale = 1, regime_scale = 1
    )
  )
}

simulate_garch_returns <- function(n, alpha, beta, df = Inf,
                                   contamination_prob = 0,
                                   contamination_scale = 1,
                                   regime_scale = 1,
                                   burn = 500L,
                                   unconditional_variance = 1e-4,
                                   seed = CFG$seed) {
  if (alpha + beta >= 0.995) stop("Simulation alpha + beta is too close to one.")
  set.seed(seed)
  N <- n + burn
  r <- numeric(N)
  h <- numeric(N)
  base_omega <- unconditional_variance * (1 - alpha - beta)
  h[1L] <- unconditional_variance
  regime_at <- burn + floor(0.70 * n)

  draw_z <- function() {
    if (is.finite(df)) {
      stats::rt(1L, df = df) / sqrt(df / (df - 2))
    } else {
      stats::rnorm(1L)
    }
  }

  for (t in 2:N) {
    omega_t <- base_omega * ifelse(t >= regime_at, regime_scale, 1)
    h[t] <- omega_t + alpha * r[t - 1L]^2 + beta * h[t - 1L]
    z <- draw_z()
    if (stats::runif(1L) < contamination_prob) z <- z * contamination_scale
    r[t] <- sqrt(max(h[t], .Machine$double.eps)) * z
  }
  list(
    returns = r[(burn + 1L):N],
    variance = h[(burn + 1L):N]
  )
}

draw_standardized_innovations <- function(n, df, contamination_prob = 0,
                                          contamination_scale = 1) {
  z <- if (is.finite(df)) {
    stats::rt(n, df = df) / sqrt(df / (df - 2))
  } else {
    stats::rnorm(n)
  }
  if (contamination_prob > 0) {
    contaminated <- stats::runif(n) < contamination_prob
    z[contaminated] <- z[contaminated] * contamination_scale
  }
  z
}

simulate_conditional_oracle_quantiles <- function(
    scenario, sim, dataset, test_idx, taus,
    draws = CFG$oracle_draws,
    origin_count = CFG$oracle_origins,
    seed = CFG$seed) {
  if (!length(test_idx)) stop("Oracle diagnostic received an empty test set.")
  take_n <- min(length(test_idx), as.integer(origin_count))
  local_rows <- unique(as.integer(round(seq(1, length(test_idx), length.out = take_n))))
  global_rows <- test_idx[local_rows]
  origin_pos <- dataset$origin_pos[global_rows]
  H <- scenario$H
  base_omega <- 1e-4 * (1 - scenario$alpha - scenario$beta)
  regime_at <- floor(0.70 * scenario$n)

  oracle <- with_seed(seed, {
    out <- array(
      NA_real_,
      dim = c(length(local_rows), H, length(taus)),
      dimnames = list(NULL, paste0("H", seq_len(H)), paste0("tau", taus))
    )
    for (ii in seq_along(local_rows)) {
      pos <- origin_pos[ii]
      r_now <- rep(sim$returns[pos], draws)
      h_now <- rep(sim$variance[pos], draws)
      cumulative <- numeric(draws)
      for (horizon in seq_len(H)) {
        future_pos <- pos + horizon
        omega_now <- base_omega * ifelse(
          future_pos >= regime_at, scenario$regime_scale, 1
        )
        h_now <- omega_now + scenario$alpha * r_now^2 + scenario$beta * h_now
        z <- draw_standardized_innovations(
          draws, scenario$df,
          scenario$contamination_prob, scenario$contamination_scale
        )
        r_now <- sqrt(pmax(h_now, .Machine$double.eps)) * z
        cumulative <- cumulative + r_now^2
        out[ii, horizon, ] <- quantile_type8(cumulative, taus)
      }
    }
    out
  })
  list(
    Q = oracle,
    test_rows = local_rows,
    dataset_rows = global_rows,
    origin_pos = origin_pos,
    draws = draws,
    origins = length(local_rows)
  )
}

oracle_quantile_diagnostics <- function(Q, oracle, taus) {
  empty <- data.frame(
    OracleQuantileBias05 = NA_real_,
    OracleQuantileBias95 = NA_real_,
    OracleQuantileMAE05 = NA_real_,
    OracleQuantileMAE95 = NA_real_,
    OracleQuantileRMSE05 = NA_real_,
    OracleQuantileRMSE95 = NA_real_,
    OracleOrigins = NA_integer_,
    OracleDraws = NA_integer_
  )
  if (is.null(oracle)) return(empty)
  q <- Q[oracle$test_rows, , , drop = FALSE]
  one_tail <- function(target) {
    k <- which(abs(taus - target) < 1e-10)
    if (!length(k)) return(c(Bias = NA_real_, MAE = NA_real_, RMSE = NA_real_))
    difference <- as.vector(q[, , k[1L]]) - as.vector(oracle$Q[, , k[1L]])
    c(
      Bias = mean(difference, na.rm = TRUE),
      MAE = mean(abs(difference), na.rm = TRUE),
      RMSE = sqrt(mean(difference^2, na.rm = TRUE))
    )
  }
  lo <- one_tail(0.05)
  hi <- one_tail(0.95)
  data.frame(
    OracleQuantileBias05 = unname(lo["Bias"]),
    OracleQuantileBias95 = unname(hi["Bias"]),
    OracleQuantileMAE05 = unname(lo["MAE"]),
    OracleQuantileMAE95 = unname(hi["MAE"]),
    OracleQuantileRMSE05 = unname(lo["RMSE"]),
    OracleQuantileRMSE95 = unname(hi["RMSE"]),
    OracleOrigins = oracle$origins,
    OracleDraws = oracle$draws
  )
}

simulation_method_result <- function(scenario_name, replication, method, Ytest, Q,
                                     taus, band = NULL, upper_band = NULL,
                                     oracle = NULL,
                                     calibration_audit = NULL) {
  score <- score_surface(Ytest, Q, taus)
  tail_score <- tail_surface_diagnostics(Ytest, Q, taus)
  tail_value <- function(tau, column) {
    z <- tail_score[abs(tail_score$Tau - tau) < 1e-10, column]
    if (length(z)) z[1L] else NA_real_
  }
  audit <- surface_audit(Q)
  projected_method <- grepl("exact 2I\\+", method)
  if (projected_method) {
    if (is.null(calibration_audit) || !isTRUE(calibration_audit$Passed[1L]) ||
        !isTRUE(audit$Passed[1L])) {
      stop(
        "Projected simulation calibration or test surface failed the shape audit for ",
        scenario_name, ", replication ", replication, ", method ", method
      )
    }
  }
  calibration_audit_row <- if (is.null(calibration_audit)) {
    data.frame(
      CalibrationMinimumPrediction = NA_real_,
      CalibrationMaximumHorizonViolation = NA_real_,
      CalibrationMaximumQuantileViolation = NA_real_,
      CalibrationShapeAuditPassed = NA
    )
  } else {
    data.frame(
      CalibrationMinimumPrediction = calibration_audit$MinimumPrediction,
      CalibrationMaximumHorizonViolation = calibration_audit$MaximumHorizonViolation,
      CalibrationMaximumQuantileViolation = calibration_audit$MaximumQuantileViolation,
      CalibrationShapeAuditPassed = calibration_audit$Passed
    )
  }
  oracle_row <- oracle_quantile_diagnostics(Q, oracle, taus)
  band_row <- if (is.null(band)) {
    data.frame(
      NominalCoverage = NA_real_,
      CoordinatewiseCoverage = NA_real_,
      MinimumHorizonCoverage = NA_real_,
      SimultaneousPathCoverage = NA_real_,
      MeanWidth = NA_real_,
      CalibrationQuantile = NA_real_,
      DirectCalibrationQuantile = NA_real_,
      CalibrationInflation = NA_real_,
      CalibrationRule = NA_character_
    )
  } else {
    bm <- band_metrics(Ytest, band)
    bm[, c(
      "NominalCoverage", "CoordinatewiseCoverage", "MinimumHorizonCoverage",
      "SimultaneousPathCoverage", "MeanWidth", "CalibrationQuantile",
      "DirectCalibrationQuantile", "CalibrationInflation", "CalibrationRule"
    ), drop = FALSE]
  }
  upper_band_row <- if (is.null(upper_band)) {
    data.frame(
      UpperNominalCoverage = NA_real_,
      UpperCoordinatewiseCoverage = NA_real_,
      UpperMinimumHorizonCoverage = NA_real_,
      UpperSimultaneousPathCoverage = NA_real_,
      MeanUpperBoundary = NA_real_,
      UpperCalibrationQuantile = NA_real_,
      UpperDirectCalibrationQuantile = NA_real_,
      UpperCalibrationInflation = NA_real_,
      UpperCalibrationRule = NA_character_
    )
  } else {
    bm <- upper_band_metrics(Ytest, upper_band)
    data.frame(
      UpperNominalCoverage = bm$NominalCoverage,
      UpperCoordinatewiseCoverage = bm$CoordinatewiseUpperCoverage,
      UpperMinimumHorizonCoverage = bm$MinimumHorizonUpperCoverage,
      UpperSimultaneousPathCoverage = bm$SimultaneousUpperPathCoverage,
      MeanUpperBoundary = bm$MeanUpperBoundary,
      UpperCalibrationQuantile = bm$CalibrationQuantile,
      UpperDirectCalibrationQuantile = bm$DirectCalibrationQuantile,
      UpperCalibrationInflation = bm$CalibrationInflation,
      UpperCalibrationRule = bm$CalibrationRule
    )
  }
  cbind(
    data.frame(
      Scenario = scenario_name,
      Replication = replication,
      Method = method,
      Status = "ok",
      Error = "",
      stringsAsFactors = FALSE
    ),
    score,
    data.frame(
      TailPinball05 = tail_value(0.05, "MeanPinball"),
      TailPinball95 = tail_value(0.95, "MeanPinball"),
      TailCalibrationError05 = score$Coverage05 - 0.05,
      TailCalibrationError95 = score$Coverage95 - 0.95,
      MinimumPrediction = audit$MinimumPrediction,
      MaximumHorizonViolation = audit$MaximumHorizonViolation,
      MaximumQuantileViolation = audit$MaximumQuantileViolation,
      ShapeAuditPassed = audit$Passed,
      ProjectedSetAuditPassed = if (projected_method) {
        isTRUE(calibration_audit$Passed[1L]) && isTRUE(audit$Passed[1L])
      } else {
        NA
      }
    ),
    calibration_audit_row,
    oracle_row,
    band_row,
    upper_band_row
  )
}

run_simulation_replication <- function(scenario, replication) {
  seed <- CFG$seed + 100000L * match(
    scenario$name, vapply(simulation_scenarios(), `[[`, character(1L), "name")
  ) + replication
  tryCatch({
    sim <- simulate_garch_returns(
      n = scenario$n,
      alpha = scenario$alpha,
      beta = scenario$beta,
      df = scenario$df,
      contamination_prob = scenario$contamination_prob,
      contamination_scale = scenario$contamination_scale,
      regime_scale = scenario$regime_scale,
      seed = seed
    )
    dates <- as.Date("2000-01-01") + seq_len(scenario$n) - 1L
    dataset <- build_curve_dataset_from_returns(
      dates, sim$returns, scenario$H, CFG$har_windows
    )
    split <- make_embargo_split(
      nrow(dataset$Y), scenario$H,
      CFG$split_train_end, CFG$split_cal_end,
      embargo = scenario$H, min_each = 30L
    )
    taus <- c(0.05, 0.50, 0.95)
    train <- split$train
    cal <- split$cal
    test <- split$test

    model_u <- fit_quantile_surface(
      dataset$X[train, , drop = FALSE], dataset$Y[train, , drop = FALSE],
      taus, rep(1, length(train)), context = paste(scenario$name, replication, "unweighted")
    )
    wd <- make_depth_weights(dataset$Y[train, , drop = FALSE], 1, 0.05, 2)
    model_w <- fit_quantile_surface(
      dataset$X[train, , drop = FALSE], dataset$Y[train, , drop = FALSE],
      taus, wd$weights, context = paste(scenario$name, replication, "weighted")
    )

    qcal_u_raw <- predict_quantile_surface(model_u, dataset$X[cal, , drop = FALSE])
    qtest_u_raw <- predict_quantile_surface(model_u, dataset$X[test, , drop = FALSE])
    qcal_w_raw <- predict_quantile_surface(model_w, dataset$X[cal, , drop = FALSE])
    qtest_w_raw <- predict_quantile_surface(model_w, dataset$X[test, , drop = FALSE])
    qcal_u <- strict_project_array(
      qcal_u_raw,
      context = paste(scenario$name, replication, "unweighted calibration")
    )
    qtest_u <- strict_project_array(
      qtest_u_raw,
      context = paste(scenario$name, replication, "unweighted test")
    )
    qcal_w <- strict_project_array(
      qcal_w_raw,
      context = paste(scenario$name, replication, "weighted calibration")
    )
    qtest_w <- strict_project_array(
      qtest_w_raw,
      context = paste(scenario$name, replication, "weighted test")
    )
    qcal_audit_u <- surface_audit(qcal_u)
    qtest_audit_u <- surface_audit(qtest_u)
    qcal_audit_w <- surface_audit(qcal_w)
    qtest_audit_w <- surface_audit(qtest_w)
    if (!all(
      qcal_audit_u$Passed, qtest_audit_u$Passed,
      qcal_audit_w$Passed, qtest_audit_w$Passed
    )) {
      stop(
        "Final projected-surface audit failed before band construction for ",
        scenario$name, ", replication ", replication
      )
    }

    band_u <- make_simultaneous_band(
      dataset$Y[train, , drop = FALSE], dataset$Y[cal, , drop = FALSE],
      qcal_u, qtest_u, taus,
      alpha = 0.10, block_length = scenario$H,
      block_reps = CFG$simulation_block_boot_reps,
      seed = seed + 11L
    )
    band_w <- make_simultaneous_band(
      dataset$Y[train, , drop = FALSE], dataset$Y[cal, , drop = FALSE],
      qcal_w, qtest_w, taus,
      alpha = 0.10, block_length = scenario$H,
      block_reps = CFG$simulation_block_boot_reps,
      seed = seed + 29L
    )
    upper_band_u <- make_simultaneous_upper_band(
      dataset$Y[train, , drop = FALSE], dataset$Y[cal, , drop = FALSE],
      qcal_u, qtest_u, taus,
      alpha = 0.05, block_length = scenario$H,
      block_reps = CFG$simulation_block_boot_reps,
      seed = seed + 41L
    )
    upper_band_w <- make_simultaneous_upper_band(
      dataset$Y[train, , drop = FALSE], dataset$Y[cal, , drop = FALSE],
      qcal_w, qtest_w, taus,
      alpha = 0.05, block_length = scenario$H,
      block_reps = CFG$simulation_block_boot_reps,
      seed = seed + 59L
    )
    Ytest <- dataset$Y[test, , drop = FALSE]
    oracle <- if (replication <= CFG$oracle_diagnostic_reps) {
      simulate_conditional_oracle_quantiles(
        scenario = scenario,
        sim = sim,
        dataset = dataset,
        test_idx = test,
        taus = taus,
        draws = CFG$oracle_draws,
        origin_count = CFG$oracle_origins,
        seed = seed + 71L
      )
    } else NULL
    do.call(rbind, list(
      simulation_method_result(
        scenario$name, replication, "QR raw", Ytest, qtest_u_raw, taus,
        oracle = oracle
      ),
      simulation_method_result(
        scenario$name, replication, "QR + exact 2I+", Ytest, qtest_u, taus,
        band = band_u, upper_band = upper_band_u, oracle = oracle,
        calibration_audit = qcal_audit_u
      ),
      simulation_method_result(
        scenario$name, replication, "Depth QR + exact 2I+", Ytest, qtest_w, taus,
        band = band_w, upper_band = upper_band_w, oracle = oracle,
        calibration_audit = qcal_audit_w
      )
    ))
  }, error = function(e) {
    data.frame(
      Scenario = scenario$name,
      Replication = replication,
      Method = NA_character_,
      Status = "failed",
      Error = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

summarise_simulation <- function(replication_level) {
  ok <- replication_level[replication_level$Status == "ok", , drop = FALSE]
  if (!nrow(ok)) return(data.frame())
  keys <- interaction(ok$Scenario, ok$Method, drop = TRUE, lex.order = TRUE)
  groups <- split(ok, keys)
  metrics <- c(
    "MeanPinball", "CRPSApprox", "Coverage05", "Coverage50", "Coverage95",
    "CalibrationMAE", "TailPinball05", "TailPinball95",
    "TailCalibrationError05", "TailCalibrationError95",
    "OracleQuantileBias05", "OracleQuantileBias95",
    "OracleQuantileMAE05", "OracleQuantileMAE95",
    "OracleQuantileRMSE05", "OracleQuantileRMSE95",
    "MaximumHorizonViolation", "MaximumQuantileViolation",
    "CoordinatewiseCoverage", "MinimumHorizonCoverage",
    "SimultaneousPathCoverage", "MeanWidth", "CalibrationInflation",
    "UpperCoordinatewiseCoverage", "UpperMinimumHorizonCoverage",
    "UpperSimultaneousPathCoverage", "MeanUpperBoundary",
    "UpperCalibrationInflation"
  )
  rows <- lapply(groups, function(g) {
    base <- data.frame(
      Scenario = g$Scenario[1L],
      Method = g$Method[1L],
      SuccessfulReplications = nrow(g),
      OracleDiagnosticReplications = sum(is.finite(g$OracleQuantileBias05)),
      stringsAsFactors = FALSE
    )
    for (m in metrics) {
      x <- g[[m]]
      base[[paste0(m, "Mean")]] <- finite_mean(x)
      base[[paste0(m, "Median")]] <- finite_median(x)
      base[[paste0(m, "IQR")]] <- finite_iqr(x)
      base[[paste0(m, "SD")]] <- finite_sd(x)
      base[[paste0(m, "Q05")]] <- finite_quantile(x, 0.05)
      base[[paste0(m, "Q95")]] <- finite_quantile(x, 0.95)
    }
    base
  })
  do.call(rbind, rows)
}

monte_carlo_coverage_summary <- function(x, target) {
  x <- as.numeric(x[is.finite(x)])
  n <- length(x)
  if (!n) {
    return(data.frame(
      Replications = 0L, TargetCoverage = target, CoverageMean = NA_real_,
      CoverageGap = NA_real_, MonteCarloSE = NA_real_,
      MonteCarloLower95 = NA_real_, MonteCarloUpper95 = NA_real_,
      CoverageAssessment = "Unavailable", stringsAsFactors = FALSE
    ))
  }
  estimate <- mean(x)
  mcse <- if (n > 1L) stats::sd(x) / sqrt(n) else NA_real_
  lower <- if (is.finite(mcse)) max(0, estimate - 1.96 * mcse) else NA_real_
  upper <- if (is.finite(mcse)) min(1, estimate + 1.96 * mcse) else NA_real_
  assessment <- if (!is.finite(lower) || !is.finite(upper)) {
    "Insufficient Monte Carlo replications"
  } else if (upper < target) {
    "Below target"
  } else if (lower > target) {
    "Above target"
  } else {
    "Compatible with target"
  }
  data.frame(
    Replications = n,
    TargetCoverage = target,
    CoverageMean = estimate,
    CoverageGap = estimate - target,
    MonteCarloSE = mcse,
    MonteCarloLower95 = lower,
    MonteCarloUpper95 = upper,
    CoverageAssessment = assessment,
    stringsAsFactors = FALSE
  )
}

simulation_band_claim_diagnostic <- function(replication_level) {
  ok <- replication_level[
    replication_level$Status == "ok" &
      replication_level$Method %in% c(
        "QR + exact 2I+", "Depth QR + exact 2I+"
      ),
    , drop = FALSE
  ]
  if (!nrow(ok)) return(data.frame())
  groups <- split(
    ok, interaction(ok$Scenario, ok$Method, drop = TRUE, lex.order = TRUE)
  )
  rows <- lapply(groups, function(g) {
    two_sided <- cbind(
      data.frame(
        Scenario = g$Scenario[1L], Method = g$Method[1L],
        BandType = "Two-sided simultaneous path", stringsAsFactors = FALSE
      ),
      monte_carlo_coverage_summary(g$SimultaneousPathCoverage, 0.90)
    )
    two_sided$MeanCalibrationInflation <- finite_mean(g$CalibrationInflation)
    upper <- cbind(
      data.frame(
        Scenario = g$Scenario[1L], Method = g$Method[1L],
        BandType = "One-sided simultaneous upper path", stringsAsFactors = FALSE
      ),
      monte_carlo_coverage_summary(g$UpperSimultaneousPathCoverage, 0.95)
    )
    upper$MeanCalibrationInflation <- finite_mean(g$UpperCalibrationInflation)
    rbind(two_sided, upper)
  })
  do.call(rbind, rows)
}

simulation_shape_audit_summary <- function(replication_level) {
  projected <- replication_level[
    replication_level$Status == "ok" &
      replication_level$Method %in% c(
        "QR + exact 2I+", "Depth QR + exact 2I+"
      ),
    , drop = FALSE
  ]
  if (!nrow(projected)) return(data.frame())
  groups <- split(
    projected,
    interaction(projected$Scenario, projected$Method, drop = TRUE, lex.order = TRUE)
  )
  rows <- lapply(groups, function(g) {
    passed <- !is.na(g$ProjectedSetAuditPassed) &
      as.logical(g$ProjectedSetAuditPassed)
    all_minima <- c(g$CalibrationMinimumPrediction, g$MinimumPrediction)
    all_horizon <- c(
      g$CalibrationMaximumHorizonViolation, g$MaximumHorizonViolation
    )
    all_quantile <- c(
      g$CalibrationMaximumQuantileViolation, g$MaximumQuantileViolation
    )
    data.frame(
      Scenario = g$Scenario[1L],
      Method = g$Method[1L],
      ProjectedFits = nrow(g),
      PassedFits = sum(passed),
      FailedFits = sum(!passed),
      MinimumPrediction = min(all_minima, na.rm = TRUE),
      MaximumHorizonViolation = max(all_horizon, na.rm = TRUE),
      MaximumQuantileViolation = max(all_quantile, na.rm = TRUE),
      Passed = all(passed),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

build_simulation_weighting_comparison <- function(replication_level) {
  ok <- replication_level[replication_level$Status == "ok", , drop = FALSE]
  fields <- c(
    "Scenario", "Replication", "MeanPinball", "Coverage05", "Coverage95",
    "TailPinball05", "TailPinball95",
    "TailCalibrationError05", "TailCalibrationError95",
    "OracleQuantileBias05", "OracleQuantileBias95",
    "OracleQuantileMAE05", "OracleQuantileMAE95",
    "UpperSimultaneousPathCoverage"
  )
  u <- ok[ok$Method == "QR + exact 2I+", fields, drop = FALSE]
  w <- ok[ok$Method == "Depth QR + exact 2I+", fields, drop = FALSE]
  out <- merge(
    u, w, by = c("Scenario", "Replication"),
    suffixes = c("Unweighted", "Weighted"), sort = FALSE
  )
  if (!nrow(out)) return(out)
  out$WeightedMinusUnweightedMeanPinball <-
    out$MeanPinballWeighted - out$MeanPinballUnweighted
  out$WeightedMinusUnweightedTailPinball05 <-
    out$TailPinball05Weighted - out$TailPinball05Unweighted
  out$WeightedMinusUnweightedTailPinball95 <-
    out$TailPinball95Weighted - out$TailPinball95Unweighted
  out$WeightedMinusUnweightedCoverage05 <-
    out$Coverage05Weighted - out$Coverage05Unweighted
  out$WeightedMinusUnweightedCoverage95 <-
    out$Coverage95Weighted - out$Coverage95Unweighted
  out$WeightedMinusUnweightedAbsoluteCalibrationError05 <-
    abs(out$TailCalibrationError05Weighted) - abs(out$TailCalibrationError05Unweighted)
  out$WeightedMinusUnweightedAbsoluteCalibrationError95 <-
    abs(out$TailCalibrationError95Weighted) - abs(out$TailCalibrationError95Unweighted)
  out$WeightedMinusUnweightedOracleBias05 <-
    out$OracleQuantileBias05Weighted - out$OracleQuantileBias05Unweighted
  out$WeightedMinusUnweightedOracleBias95 <-
    out$OracleQuantileBias95Weighted - out$OracleQuantileBias95Unweighted
  out$WeightedMinusUnweightedAbsoluteOracleBias05 <-
    abs(out$OracleQuantileBias05Weighted) - abs(out$OracleQuantileBias05Unweighted)
  out$WeightedMinusUnweightedAbsoluteOracleBias95 <-
    abs(out$OracleQuantileBias95Weighted) - abs(out$OracleQuantileBias95Unweighted)
  out$WeightedMinusUnweightedUpperPathCoverage <-
    out$UpperSimultaneousPathCoverageWeighted -
    out$UpperSimultaneousPathCoverageUnweighted
  out
}

summarise_weighting_comparison <- function(comparison) {
  if (!nrow(comparison)) return(data.frame())
  groups <- split(comparison, comparison$Scenario)
  metrics <- grep("^WeightedMinusUnweighted", names(comparison), value = TRUE)
  rows <- lapply(groups, function(g) {
    out <- data.frame(
      Scenario = g$Scenario[1L],
      Replications = nrow(g),
      OracleDiagnosticReplications = sum(
        is.finite(g$WeightedMinusUnweightedOracleBias05) &
          is.finite(g$WeightedMinusUnweightedOracleBias95)
      ),
      stringsAsFactors = FALSE
    )
    for (m in metrics) out[[paste0(m, "Mean")]] <- finite_mean(g[[m]])
    out
  })
  do.call(rbind, rows)
}

run_simulation_suite <- function() {
  scenarios <- simulation_scenarios()
  tasks <- do.call(rbind, lapply(seq_along(scenarios), function(s) {
    data.frame(ScenarioIndex = s, Replication = seq_len(CFG$simulation_reps))
  }))
  log_msg(
    "Simulation started: ", length(scenarios), " scenarios x ",
    CFG$simulation_reps, " replications"
  )
  worker <- function(i) {
    run_simulation_replication(
      scenarios[[tasks$ScenarioIndex[i]]], tasks$Replication[i]
    )
  }
  if (.Platform$OS.type != "windows" && CFG$cores > 1L) {
    ans <- parallel::mclapply(
      seq_len(nrow(tasks)), worker,
      mc.cores = min(CFG$cores, nrow(tasks)), mc.set.seed = FALSE
    )
  } else if (.Platform$OS.type == "windows" && CFG$cores > 1L) {
    worker_count <- min(CFG$cores, nrow(tasks))
    log_msg("Using ", worker_count, " PSOCK workers for the Windows simulation")
    cl <- parallel::makePSOCKcluster(worker_count)
    exports <- intersect(R1_WORKER_EXPORTS, ls(envir = .GlobalEnv, all.names = TRUE))
    parallel::clusterExport(cl, varlist = exports, envir = .GlobalEnv)
    ans <- tryCatch(
      parallel::parLapply(
        cl, seq_len(nrow(tasks)),
        function(i, task_grid, scenario_list) {
          run_simulation_replication(
            scenario_list[[task_grid$ScenarioIndex[i]]],
            task_grid$Replication[i]
          )
        },
        task_grid = tasks,
        scenario_list = scenarios
      ),
      finally = parallel::stopCluster(cl)
    )
  } else {
    ans <- lapply(seq_len(nrow(tasks)), worker)
  }
  replication_level <- rbind_fill(ans)
  failed <- replication_level[replication_level$Status == "failed", , drop = FALSE]
  if (nrow(failed)) {
    log_msg("Simulation failures: ", nrow(failed), level = "WARN")
  }
  summary <- summarise_simulation(replication_level)
  weighting_comparison <- build_simulation_weighting_comparison(replication_level)
  weighting_comparison_summary <- summarise_weighting_comparison(weighting_comparison)
  tail_summary <- simulation_tail_summary(summary)
  band_claim_diagnostic <- simulation_band_claim_diagnostic(replication_level)
  shape_audit <- simulation_shape_audit_summary(replication_level)
  log_msg("Simulation completed")
  list(
    replication_level = replication_level,
    summary = summary,
    tail_summary = tail_summary,
    band_claim_diagnostic = band_claim_diagnostic,
    shape_audit = shape_audit,
    weighting_comparison = weighting_comparison,
    weighting_comparison_summary = weighting_comparison_summary
  )
}

# ---- Aggregation, figures, and reproducibility output -------------------------
compact_simulation_summary <- function(x) {
  if (!nrow(x)) return(x)
  keep <- c(
    "Scenario", "Method", "SuccessfulReplications", "OracleDiagnosticReplications",
    "MeanPinballMean", "MeanPinballMedian", "MeanPinballIQR",
    "MeanPinballQ05", "MeanPinballQ95",
    "TailPinball05Mean", "TailPinball95Mean",
    "Coverage05Mean", "Coverage95Mean",
    "TailCalibrationError05Mean", "TailCalibrationError95Mean",
    "OracleQuantileBias05Mean", "OracleQuantileBias95Mean",
    "OracleQuantileMAE05Mean", "OracleQuantileMAE95Mean",
    "MaximumHorizonViolationMean", "MaximumQuantileViolationMean",
    "CoordinatewiseCoverageMean", "SimultaneousPathCoverageMean", "MeanWidthMean",
    "UpperCoordinatewiseCoverageMean", "UpperSimultaneousPathCoverageMean"
  )
  x[, intersect(keep, names(x)), drop = FALSE]
}

simulation_tail_summary <- function(x) {
  if (!nrow(x)) return(data.frame())
  d <- x[x$Method %in% c("QR + exact 2I+", "Depth QR + exact 2I+"), , drop = FALSE]
  rows <- list()
  counter <- 0L
  for (i in seq_len(nrow(d))) {
    for (tau in c(0.05, 0.95)) {
      suffix <- if (tau == 0.05) "05" else "95"
      counter <- counter + 1L
      rows[[counter]] <- data.frame(
        Scenario = d$Scenario[i],
        Method = d$Method[i],
        Tau = tau,
        SuccessfulReplications = d$SuccessfulReplications[i],
        OracleDiagnosticReplications = d$OracleDiagnosticReplications[i],
        TailPinball = d[[paste0("TailPinball", suffix, "Mean")]][i],
        ObservedCoverage = d[[paste0("Coverage", suffix, "Mean")]][i],
        CalibrationError = d[[paste0("TailCalibrationError", suffix, "Mean")]][i],
        OracleQuantileBias = d[[paste0("OracleQuantileBias", suffix, "Mean")]][i],
        OracleQuantileMAE = d[[paste0("OracleQuantileMAE", suffix, "Mean")]][i],
        OracleQuantileRMSE = d[[paste0("OracleQuantileRMSE", suffix, "Mean")]][i],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

publication_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(colour = "black"),
      axis.text = ggplot2::element_text(colour = "black"),
      plot.caption = ggplot2::element_text(
        hjust = 0, colour = "black", size = base_size * 0.72,
        margin = ggplot2::margin(t = 8)
      )
    )
}

BAND_WIDTH_SCALE_NOTE <- paste0(
  "Vertical scales are asset-specific; widths should be compared across ",
  "horizons within an asset, not across panels."
)

FIXED_PANEL_ASSESSMENT_LEVELS <- c(
  "Holm-significant + calibration trade-off",
  "Holm-significant + wider band",
  "Not Holm-significant",
  "Holm-significant gain without observed cost",
  "Holm-significant loss increase"
)

academicize_display_data <- function(df) {
  for (nm in names(df)) {
    if (is.character(df[[nm]])) {
      df[[nm]] <- gsub("_", " ", df[[nm]], fixed = TRUE)
    } else if (is.factor(df[[nm]])) {
      df[[nm]] <- factor(gsub("_", " ", as.character(df[[nm]]), fixed = TRUE))
    }
  }
  df
}

make_empirical_figures <- function(empirical, display_assets = names(empirical)) {
  metrics <- academicize_display_data(
    rbind_fill(lapply(empirical, function(z) z$evaluation$metrics))
  )
  calibration <- academicize_display_data(
    rbind_fill(lapply(empirical, `[[`, "calibration"))
  )
  width_profile <- academicize_display_data(
    rbind_fill(lapply(empirical, `[[`, "width_profile"))
  )
  band_metrics_all <- academicize_display_data(
    rbind_fill(lapply(empirical, function(z) z$bands$metrics))
  )
  upper_profile <- academicize_display_data(
    rbind_fill(lapply(empirical, `[[`, "upper_profile"))
  )
  upper_metrics_all <- academicize_display_data(
    rbind_fill(lapply(empirical, function(z) z$upper_bands$metrics))
  )
  display_assets <- unique(as.character(display_assets))
  keep_assets <- function(x) {
    if (!nrow(x) || !"Asset" %in% names(x)) return(x)
    x[x$Asset %in% display_assets, , drop = FALSE]
  }
  metrics <- keep_assets(metrics)
  calibration <- keep_assets(calibration)
  width_profile <- keep_assets(width_profile)
  band_metrics_all <- keep_assets(band_metrics_all)
  upper_profile <- keep_assets(upper_profile)
  upper_metrics_all <- keep_assets(upper_metrics_all)

  key_methods <- c(
    "QR + exact 2I+", "Selected 2I-FQR", "Curve persistence",
    "Rolling historical quantiles"
  )
  d <- metrics[metrics$Method %in% key_methods, , drop = FALSE]
  if (nrow(d)) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = Method, y = MeanPinball, fill = Method)) +
      ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
      ggplot2::facet_wrap(~ Asset, scales = "free_y") +
      ggplot2::scale_y_continuous(labels = scales::label_scientific()) +
      ggplot2::labs(x = NULL, y = "Mean pinball loss") +
      publication_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 28, hjust = 1))
    ggplot2::ggsave(
      file.path(DIR$figures, "fig_empirical_mean_pinball.png"),
      p, width = 10, height = 6.2, dpi = 400
    )
  }

  if (nrow(calibration)) {
    p <- ggplot2::ggplot(
      calibration,
      ggplot2::aes(x = Tau, y = ObservedCoverage, colour = Asset, group = Asset)
    ) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey35") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::scale_x_continuous(breaks = CFG$taus) +
      ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
      ggplot2::labs(x = "Nominal quantile level", y = "Observed coordinatewise coverage", colour = "Asset") +
      publication_theme()
    ggplot2::ggsave(
      file.path(DIR$figures, "fig_quantile_reliability.png"),
      p, width = 8, height = 5.2, dpi = 400
    )
  }

  if (nrow(width_profile)) {
    p <- ggplot2::ggplot(
      width_profile,
      ggplot2::aes(x = Horizon, y = MeanWidth, group = Asset)
    ) +
      ggplot2::geom_line(linewidth = 0.85, colour = "#0072B2") +
      ggplot2::facet_wrap(~ Asset, scales = "free_y", ncol = 3) +
      ggplot2::scale_y_continuous(
        labels = scales::label_scientific()
      ) +
      ggplot2::labs(
        x = "Forecast horizon", y = "Mean simultaneous-band width",
        caption = BAND_WIDTH_SCALE_NOTE
      ) +
      publication_theme()
    ggplot2::ggsave(
      file.path(DIR$figures, "fig_simultaneous_band_width.png"),
      p, width = 10, height = 6.2, dpi = 400
    )
    ggplot2::ggsave(
      file.path(DIR$figures, "fig_simultaneous_band_width.pdf"),
      p, width = 10, height = 6.2, device = "pdf"
    )

    p_cov <- ggplot2::ggplot(
      width_profile,
      ggplot2::aes(x = Horizon, y = Coverage, colour = Asset, group = Asset)
    ) +
      ggplot2::geom_hline(
        yintercept = 1 - CFG$primary_band_alpha,
        linetype = "dashed", colour = "grey35"
      ) +
      ggplot2::geom_line(linewidth = 0.85) +
      ggplot2::coord_cartesian(ylim = c(0.90, 1.00)) +
      ggplot2::scale_y_continuous(
        breaks = seq(0.90, 1.00, by = 0.02),
        labels = scales::label_percent(accuracy = 1)
      ) +
      ggplot2::labs(x = "Forecast horizon", y = "Coordinatewise band coverage", colour = "Asset") +
      publication_theme()
    ggplot2::ggsave(
      file.path(DIR$figures, "fig_band_coverage_by_horizon.png"),
      p_cov, width = 8, height = 5.2, dpi = 400
    )
  }

  if (nrow(band_metrics_all)) {
    p <- ggplot2::ggplot(
      band_metrics_all,
      ggplot2::aes(x = Asset, y = SimultaneousPathCoverage, fill = Method)
    ) +
      ggplot2::geom_hline(
        yintercept = 1 - CFG$primary_band_alpha,
        linetype = "dashed", colour = "grey35"
      ) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1), labels = scales::label_percent(accuracy = 1)
      ) +
      ggplot2::labs(x = NULL, y = "Simultaneous path coverage", fill = "Method") +
      publication_theme()
    ggplot2::ggsave(
      file.path(DIR$figures, "fig_simultaneous_path_coverage.png"),
      p, width = 8, height = 5.2, dpi = 400
    )
  }

  if (nrow(upper_profile)) {
    p <- ggplot2::ggplot(
      upper_profile,
      ggplot2::aes(x = Horizon, y = UpperCoverage, colour = Asset, group = Asset)
    ) +
      ggplot2::geom_hline(
        yintercept = 1 - CFG$primary_upper_band_alpha,
        linetype = "dashed", colour = "grey35"
      ) +
      ggplot2::geom_line(linewidth = 0.85) +
      ggplot2::coord_cartesian(ylim = c(0.95, 1.00)) +
      ggplot2::scale_y_continuous(
        breaks = seq(0.95, 1.00, by = 0.01),
        labels = scales::label_percent(accuracy = 1)
      ) +
      ggplot2::labs(
        x = "Forecast horizon", y = "One-sided upper-band coverage", colour = "Asset"
      ) +
      publication_theme()
    ggplot2::ggsave(
      file.path(DIR$figures, "fig_upper_band_coverage_by_horizon.png"),
      p, width = 8, height = 5.2, dpi = 400
    )
  }

  if (nrow(upper_metrics_all)) {
    p <- ggplot2::ggplot(
      upper_metrics_all,
      ggplot2::aes(x = Asset, y = SimultaneousUpperPathCoverage, fill = Method)
    ) +
      ggplot2::geom_hline(
        yintercept = 1 - CFG$primary_upper_band_alpha,
        linetype = "dashed", colour = "grey35"
      ) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1), labels = scales::label_percent(accuracy = 1)
      ) +
      ggplot2::labs(x = NULL, y = "One-sided simultaneous upper-path coverage", fill = "Method") +
      publication_theme()
    ggplot2::ggsave(
      file.path(DIR$figures, "fig_simultaneous_upper_path_coverage.png"),
      p, width = 8, height = 5.2, dpi = 400
    )
  }
}

make_fixed_panel_inference_figure <- function(adjusted_inference) {
  if (!nrow(adjusted_inference)) return(invisible(NULL))
  d <- academicize_display_data(adjusted_inference)
  d$Assessment <- factor(
    d$Assessment,
    levels = FIXED_PANEL_ASSESSMENT_LEVELS
  )
  if (any(is.na(d$Assessment))) {
    stop("Unrecognized fixed-panel assessment class in the gain figure.")
  }
  d$Asset <- factor(
    d$Asset,
    levels = d$Asset[order(d$TestMeanPinballGainPct)]
  )
  colours <- c(
    "Holm-significant + calibration trade-off" = "#00A6B4",
    "Holm-significant + wider band" = "#B565D9",
    "Not Holm-significant" = "#7A8F00",
    "Holm-significant gain without observed cost" = "#24783C",
    "Holm-significant loss increase" = "#C34C3C"
  )
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = TestMeanPinballGainPct, y = Asset, colour = Assessment
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed", linewidth = 0.55,
      colour = "grey35"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = PercentileCILowerPct, xend = PercentileCIUpperPct,
        y = Asset, yend = Asset
      ),
      linewidth = 0.9
    ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_colour_manual(
      values = colours,
      breaks = FIXED_PANEL_ASSESSMENT_LEVELS,
      drop = TRUE
    ) +
    ggplot2::labs(
      x = "Mean pinball-loss gain (%)", y = NULL, colour = "Assessment",
      caption = paste0(
        "Intervals are two-sided moving-block-bootstrap percentile intervals. ",
        "Holm significance is assessed across the five fixed asset-level tests."
      )
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(ncol = 1, byrow = TRUE)
    ) +
    publication_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(size = 9),
      legend.key.width = grid::unit(1.1, "lines"),
      plot.margin = ggplot2::margin(8, 18, 24, 8)
    )
  ggplot2::ggsave(
    file.path(DIR$figures, "fig_fixed_panel_adjusted_pinball_gain.png"),
    p, width = 10.5, height = 6.5, dpi = 400
  )
  ggplot2::ggsave(
    file.path(DIR$figures, "fig_fixed_panel_adjusted_pinball_gain.pdf"),
    p, width = 10.5, height = 6.5, device = "pdf"
  )
  invisible(p)
}

make_pave_guard_sensitivity_figure <- function(pave_sensitivity) {
  if (!nrow(pave_sensitivity)) return(invisible(NULL))
  if (!identical(
    as.numeric(pave_sensitivity$CalibrationGuard),
    as.numeric(CFG$reporting_calibration_guard_grid)
  )) {
    stop("PAVE calibration-guard grid differs from the prespecified grid.")
  }
  d <- pave_sensitivity
  d$GuardStatus <- factor(
    d$GuardStatus,
    levels = c("Guard passed", "Guard failed")
  )
  pave_holm <- unique(d$HolmAdjustedPValue)
  if (length(pave_holm) != 1L || !is.finite(pave_holm)) {
    stop("PAVE Holm-adjusted p-value is unavailable or non-unique.")
  }
  caption <- paste0(
    "Guard passage concerns calibration tolerance only. PAVE is ",
    if (pave_holm < 0.05) "Holm-significant" else "not Holm-significant",
    " (adjusted p = ",
    formatC(pave_holm, format = "f", digits = 4), ")."
  )
  colours <- c(
    "Guard passed" = "#F8766D",
    "Guard failed" = "#00BFC4"
  )
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = CalibrationGuard,
      y = ObservedCalibrationMAEChange,
      colour = GuardStatus,
      group = 1
    )
  ) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0, linetype = "dashed",
      colour = "grey35", linewidth = 0.55
    ) +
    ggplot2::geom_line(colour = "grey55", linewidth = 0.75) +
    ggplot2::geom_point(size = 3.2) +
    ggplot2::scale_colour_manual(values = colours, drop = TRUE) +
    ggplot2::scale_x_continuous(
      breaks = CFG$reporting_calibration_guard_grid,
      labels = scales::label_number(accuracy = 0.001)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.001),
      limits = c(
        min(0, 1.20 * d$ObservedCalibrationMAEChange),
        1.20 * max(c(
          CFG$reporting_calibration_guard_grid,
          d$ObservedCalibrationMAEChange
        ))
      ),
      breaks = unique(c(0, CFG$reporting_calibration_guard_grid))
    ) +
    ggplot2::labs(
      x = "Calibration guard",
      y = "Observed change in calibration MAE",
      colour = "Calibration-guard status",
      caption = caption
    ) +
    publication_theme() +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1))
  ggplot2::ggsave(
    file.path(DIR$figures, "fig_pave_calibration_guard_sensitivity.png"),
    p, width = 8.5, height = 5.7, dpi = 400
  )
  ggplot2::ggsave(
    file.path(DIR$figures, "fig_pave_calibration_guard_sensitivity.pdf"),
    p, width = 8.5, height = 5.7, device = "pdf"
  )
  invisible(p)
}

make_simulation_figures <- function(band_claim_diagnostic) {
  if (!nrow(band_claim_diagnostic)) return(invisible(NULL))
  d <- academicize_display_data(band_claim_diagnostic[
    band_claim_diagnostic$Method %in% c(
      "QR + exact 2I+", "Depth QR + exact 2I+"
    ),
    , drop = FALSE
  ])
  make_coverage_plot <- function(data, target, y_limits, y_label, path) {
    if (!nrow(data)) return(invisible(NULL))
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Method, y = CoverageMean, ymin = MonteCarloLower95,
        ymax = MonteCarloUpper95, colour = Method
      )
    ) +
      ggplot2::geom_hline(
        yintercept = target, linetype = "dashed", colour = "grey35"
      ) +
      ggplot2::geom_pointrange(linewidth = 0.65, fatten = 2.0) +
      ggplot2::facet_wrap(~ Scenario) +
      ggplot2::coord_cartesian(ylim = y_limits) +
      ggplot2::scale_y_continuous(
        breaks = seq(y_limits[1L], y_limits[2L], by = 0.05),
        labels = scales::label_percent(accuracy = 1)
      ) +
      ggplot2::labs(
        x = NULL, y = y_label,
        caption = "Points are Monte Carlo means; bars are 95% Monte Carlo intervals."
      ) +
      publication_theme() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
      )
    ggplot2::ggsave(path, p, width = 11, height = 7, dpi = 400)
  }
  make_coverage_plot(
    d[d$BandType == "Two-sided simultaneous path", , drop = FALSE],
    target = 0.90, y_limits = c(0.70, 1.00),
    y_label = "Mean simultaneous path coverage",
    path = file.path(DIR$figures, "fig_simulation_path_coverage.png")
  )
  make_coverage_plot(
    d[d$BandType == "One-sided simultaneous upper path", , drop = FALSE],
    target = 0.95, y_limits = c(0.80, 1.00),
    y_label = "Mean one-sided simultaneous upper-path coverage",
    path = file.path(DIR$figures, "fig_simulation_upper_path_coverage.png")
  )
  invisible(NULL)
}

flatten_config <- function(cfg) {
  rows <- lapply(names(cfg), function(nm) {
    val <- cfg[[nm]]
    if (inherits(val, "Date")) val <- as.character(val)
    if (length(val) > 1L) val <- paste(val, collapse = ",")
    data.frame(Parameter = nm, Value = as.character(val), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

write_reproducibility_files <- function(empirical=NULL,simulation=NULL) {
  write_csv_safe(flatten_config(CFG),file.path(DIR$logs,"configuration.csv"))
  saveRDS(CFG,file.path(DIR$logs,"configuration.rds"),version=2)
  raw <- list.files(DIR$raw,pattern="\\.csv$",full.names=TRUE)
  checksums <- data.frame(File=basename(raw),MD5=unname(tools::md5sum(raw)),stringsAsFactors=FALSE)
  write_csv_safe(checksums,file.path(DIR$logs,"raw_data_checksums.csv"))
  write_csv_safe(data.frame(Source=CFG$data_mode,Asset=CFG$assets,
    StartDate=as.character(CFG$start_date),EndDate=as.character(CFG$end_date),
    Target="Cumulative squared daily log returns; variance proxy",
    HorizonUnits="Successive available return observations; not a common calendar-day horizon"),
    file.path(DIR$logs,"data_source_manifest.csv"))
  capture.output(sessionInfo(),file=file.path(DIR$logs,"sessionInfo.txt"))
  versions <- data.frame(Package=required_packages,
    Version=vapply(required_packages,function(p)as.character(utils::packageVersion(p)),character(1L)))
  write_csv_safe(versions,file.path(DIR$logs,"package_versions.csv"))
  resolved <- function(x) nzchar(x) && !toupper(x) %in% c("TO_BE_ASSIGNED","TBA","NA","NONE","NULL","PLACEHOLDER")
  metadata <- data.frame(Item=c("Code repository","Archive DOI","Data source"),
    Identifier=c(if (resolved(CFG$code_repository_url)) CFG$code_repository_url else "",
                 if (resolved(CFG$archive_doi)) CFG$archive_doi else "",CFG$data_repository_url),
    Status=c(if (resolved(CFG$code_repository_url)) "Supplied" else "Pending; not a computation requirement",
             if (resolved(CFG$archive_doi)) "Supplied" else "Pending; not a computation requirement",
             if (RUN_MODE=="smoke") "Synthetic diagnostic" else "Yahoo Finance adjusted prices"))
  write_csv_safe(metadata,file.path(DIR$logs,"citation_metadata.csv"))
  requirements <- data.frame(
    Requirement=c("Full mode","Real adjusted-price data","Fixed five-asset panel",
      "Horizon 30","Empirical modules","Empirical calibration replications",
      "Paired inference replications","Simulation replications","Simulation bootstrap replications",
      "GARCH conditional draws","Calibration numerical seeds","R1 audit completed"),
    Actual=c(RUN_MODE,CFG$data_mode,paste(CFG$assets,collapse=","),CFG$H,
      CFG$run_empirical && CFG$run_depth_sensitivity && CFG$run_grid_sensitivity && CFG$run_block_sensitivity,
      CFG$calibration_reps,CFG$block_boot_reps,if(is.null(simulation)) 0 else CFG$simulation_reps,
      CFG$simulation_block_boot_reps,CFG$garch_draws,CFG$calibration_mc_seeds,
      file.exists(file.path(DIR$logs,"r1_final_audit.csv"))),
    Passed=c(RUN_MODE=="full",CFG$data_mode=="Yahoo_adjusted",identical(CFG$assets,FIXED_PANEL),CFG$H==30L,
      CFG$run_empirical && CFG$run_depth_sensitivity && CFG$run_grid_sensitivity && CFG$run_block_sensitivity,
      CFG$calibration_reps>=2000L,CFG$block_boot_reps>=2000L,
      !is.null(simulation) && CFG$simulation_reps>=500L,
      CFG$simulation_block_boot_reps>=250L,CFG$garch_draws>=20000L,CFG$calibration_mc_seeds>=10L,
      file.exists(file.path(DIR$logs,"r1_final_audit.csv"))))
  write_csv_safe(requirements,file.path(DIR$logs,"publication_run_readiness.csv"))
  write_csv_safe(data.frame(Item=c("Raw data checksum records","Session information",
    "Repository and archive metadata"),Status=c(if(nrow(checksums)) "Recorded" else "Absent",
    "Recorded","External identifiers are optional during computation; supply truthful availability at submission")),
    file.path(DIR$logs,"reproducibility_readiness.csv"))
  mapping <- data.frame(
    R1Comment=seq_len(6L),
    Evidence=c("r1_depth_information_summary.csv; r1_depth_training_details.csv; METHOD_NOTES_R1.txt",
      "r1_projection_decomposition.csv; tab_r1_projection_decomposition.tex",
      "r1_horizon_loss_differences.csv; fig_r1_horizon_pinball_difference.pdf",
      "r1_gamma_validation_profile.csv; r1_weight_clipping_by_fit.csv",
      "r1_block_length_sensitivity.csv; r1_bootstrap_numerical_stability.csv",
      "r1_distributional_benchmark_metrics.csv; r1_garch_paired_inference.csv; r1_garch_parameters.csv"),
    ManuscriptAction=c("Explain retained/discarded rank information and response-weighted target",
      "Separate projection coherence from predictive loss effects",
      "Insert the horizon-specific figure and report heterogeneous path patterns",
      "Describe validation separation and runner-up gamma gap without test retuning",
      "Report sensitivity, direct L1 special case, and empirical validity limits",
      "Report fair GARCH comparison and clarify the cumulative squared-return proxy"))
  write_csv_safe(mapping,file.path(DIR$logs,"R1_comment_evidence_map.csv"))
  if (nzchar(R1_SOURCE_PATH) && file.exists(R1_SOURCE_PATH)) {
    target <- file.path(DIR$root,"2IFQR_v2_1_R1_Full_Pipeline.R")
    if (normalizePath(target,mustWork=FALSE)!=normalizePath(R1_SOURCE_PATH,mustWork=TRUE)) {
      file.copy(R1_SOURCE_PATH,target,overwrite=TRUE)
    }
    write_csv_safe(data.frame(Source=basename(R1_SOURCE_PATH),MD5=unname(tools::md5sum(R1_SOURCE_PATH))),
                   file.path(DIR$logs,"pipeline_source_checksum.csv"))
  }
  writeLines(c(
    "2I-FQR v2.1 R1 replication instructions",
    "",
    "Run in a writable project directory with R 4.3 or later.",
    "Install once: install.packages(c('quantmod','xts','zoo','quantreg','ggplot2','scales'))",
    "The GARCH benchmark uses native base R conditional Student-t maximum likelihood.",
    "No rugarch installation, GitHub credentials, repository URL or DOI is needed to compute results.",
    "",
    "Default full analysis:",
    "  Rscript 2IFQR_v2_1_R1_Full_Pipeline.R",
    "RStudio equivalent:",
    "  Sys.setenv(TWOIFQR_RUN_MODE='full')",
    "  source('2IFQR_v2_1_R1_Full_Pipeline.R')",
    "",
    "Revision analysis without rerunning the original simulation:",
    "  Sys.setenv(TWOIFQR_RUN_MODE='revision')",
    "  source('2IFQR_v2_1_R1_Full_Pipeline.R')",
    "To reuse a validated simulation, set TWOIFQR_EXISTING_SIMULATION_RDS to simulation_results.rds.",
    "To reuse v2.0j empirical objects, set TWOIFQR_EXISTING_EMPIRICAL_RDS to empirical_all_assets.rds.",
    "Reused objects are checked against their cached prices, target construction, splits, coefficients and tuning grid.",
    "",
    "Preserve existing frozen data:",
    "  Sys.setenv(TWOIFQR_DATA_CACHE='C:/previous_outputs/data_raw')",
    "Required names: PAVE_adjusted.csv, KO_adjusted.csv, BTC_USD_adjusted.csv, PPLT_adjusted.csv, GLD_adjusted.csv.",
    "Each CSV has Date, Asset, Adjusted columns. A named external cache never silently falls back to new data.",
    "Without a frozen cache the initial full run downloads and freezes Yahoo adjusted prices.",
    "Historical vendor data may differ from a previous download; compare raw_data_checksums.csv before asserting exact replication.",
    "",
    "Automatic resume is on. Completed baseline assets, GARCH origins and calibration diagnostics are checkpointed.",
    "Set TWOIFQR_RESUME=false to recompute in the same output directory.",
    "Set TWOIFQR_OUTPUT to choose a separate output directory. Do not mix synthetic and financial output folders.",
    "",
    "Self-tests: Sys.setenv(TWOIFQR_RUN_MODE='selftest'); source('2IFQR_v2_1_R1_Full_Pipeline.R')",
    "Reduced synthetic integration: Sys.setenv(TWOIFQR_RUN_MODE='smoke'); source('2IFQR_v2_1_R1_Full_Pipeline.R')",
    "Smoke outputs are SYNTHETIC and cannot be used as empirical evidence.",
    "",
    "After completion inspect RUN_COMPLETED.txt, logs/final_audit.csv, logs/r1_final_audit.csv and",
    "logs/publication_run_readiness.csv. A revision-only run intentionally has no fresh full-simulation readiness.",
    "Scores, calibration and coverage may favor either method. Audits do not demand a desired scientific finding.",
    "Send the complete outputs folder when preparing the manuscript and R1 response."),
    file.path(DIR$root,"REPRODUCE.txt"))
  writeLines(c(
    "Method interpretation for SMAP-D-26-00135R1",
    "",
    "1. Target: Y[t,h] = sum_{j=1}^h r[t+j]^2, a cumulative forward variance proxy based on daily squared log returns.",
    "It is neither an intraday realized-variance measure nor its square-root volatility transformation.",
    "Horizons count available observations. Bitcoin includes weekend observations; exchange-traded assets do not.",
    "",
    "2. Depth retains average marginal-rank centrality along the path. It does not explicitly identify the copula,",
    "increment timing, slopes or magnitude of extremes. Distinct paths can share the same depth.",
    "Because horizon contributions belong to the same path, do not claim that all dependence information is discarded.",
    "For fixed population response weights, unrestricted conditional quantile fitting uses the tilted law",
    "F_w(q|x) = E[w(Y) 1(Y_h<=q)|X=x] / E[w(Y)|X=x]. Linear QR restricts this weighted objective.",
    "This illustration does not establish conditional consistency or a particular direction of bias.",
    "They can attenuate genuine low- or high-volatility episodes as well as contaminated paths.",
    "",
    "3. The cap wmax acts on mean-normalized preliminary weights, before a second normalization.",
    "It is not a hard bound on the final normalized weight. Report clipping for both inner and full training fits.",
    "Gamma=0 is an exact unweighted candidate. Selection uses internally embargoed training validation only.",
    "WIS is twice mean pinball on the symmetric seven-level grid and is not independent evidence.",
    "",
    "4. Exact projection enforces nonnegativity and both index orders. Its predictive effect must be measured",
    "separately from depth weighting. Equal rounded losses do not establish equal surfaces.",
    "Horizon intervals are pointwise; do not interpret them as a simultaneous 30-horizon significance statement.",
    "",
    "5. Moving-block calibration is a practical dependence-aware rule, not a general finite-sample coverage theorem.",
    "Length 1 uses the direct finite-sample-rank quantile without the bootstrap guard in this implementation.",
    "More bootstrap draws reduce numerical resampling error; they do not remedy structural shift.",
    "Keep empirical conservatism and simulation undercoverage visible. Do not select the block length on test coverage.",
    "",
    "6. The GARCH benchmark has zero conditional mean, positive stationary sGARCH(1,1) variance and standardized",
    "Student-t innovations. Parameters use a prespecified constrained conditional MLE with four deterministic starts.",
    "Persistence is below 0.9999 and degrees of freedom lie in [2.05,200]. Returns are scaled by 100 for fitting.",
    "Parameters are frozen at the last training target-end date. Later observed returns update only the causal state.",
    "Quantiles come from simulated cumulative squared-return paths, not quantiles of a point variance forecast.",
    "The same test origins, quantile scores and band calibration design are used for all principal comparisons.",
    "The original five primary tests and the five GARCH tests have separate, explicitly declared Holm families.",
    "Conditional simulation treats fitted parameters as fixed; it does not integrate parameter uncertainty.",
    "",
    "The numerical conclusions, manuscript claims and response locations must be updated after the actual run."),
    file.path(DIR$root,"METHOD_NOTES_R1.txt"))
  files <- list.files(DIR$csv,pattern="\\.csv$",full.names=TRUE)
  write_csv_safe(data.frame(File=basename(files),MD5=unname(tools::md5sum(files))),
                 file.path(DIR$logs,"output_table_checksums.csv"))
}

legacy_upper_coverage_reference <- function() {
  data.frame(
    Asset = "BTC-USD",
    LegacyMeanUpperCoverage = 0.9880,
    LegacyMinimumHorizonUpperCoverage = 0.9487,
    LegacySource = "Original manuscript Table 4; reported values, not recomputed",
    stringsAsFactors = FALSE
  )
}

asset_metadata_for <- function(assets) {
  class_map <- c(
    "PAVE" = "Real estate and infrastructure",
    "KO" = "Large-cap equity",
    "BTC-USD" = "Cryptoasset",
    "PPLT" = "Commodity",
    "GLD" = "Commodity"
  )
  data.frame(
    Asset = assets,
    AssetClass = unname(class_map[assets]),
    PrimaryAsset = assets %in% CFG$primary_assets,
    stringsAsFactors = FALSE
  )
}

build_fixed_panel_adjusted_inference <- function(tables) {
  paired <- tables$paired_loss_inference
  paired$HolmAdjustedPValue <- NA_real_
  primary_idx <- which(
    paired$Comparator == "QR + exact 2I+" &
      paired$Asset %in% FIXED_PANEL &
      is.finite(paired$RawTwoSidedPValue)
  )
  if (length(primary_idx)) {
    paired$HolmAdjustedPValue[primary_idx] <- stats::p.adjust(
      paired$RawTwoSidedPValue[primary_idx], method = "holm"
    )
  }
  tables$paired_loss_inference <- paired

  rows <- lapply(FIXED_PANEL, function(asset) {
    metrics <- tables$empirical_main_metrics
    selected <- metrics[
      metrics$Asset == asset & metrics$Method == "Selected 2I-FQR",
      , drop = FALSE
    ]
    unweighted <- metrics[
      metrics$Asset == asset & metrics$Method == "QR + exact 2I+",
      , drop = FALSE
    ]
    bands <- tables$simultaneous_band_metrics
    selected_band <- bands[
      bands$Asset == asset & bands$Method == "Selected 2I-FQR",
      , drop = FALSE
    ]
    unweighted_band <- bands[
      bands$Asset == asset & bands$Method == "QR + exact 2I+",
      , drop = FALSE
    ]
    inference <- paired[
      paired$Asset == asset & paired$Comparator == "QR + exact 2I+",
      , drop = FALSE
    ]
    audit <- tables$prediction_surface_audit[
      tables$prediction_surface_audit$Asset == asset &
        tables$prediction_surface_audit$Method == "Selected 2I-FQR",
      , drop = FALSE
    ]
    if (!nrow(selected) || !nrow(unweighted) || !nrow(selected_band) ||
        !nrow(unweighted_band) || !nrow(inference) || !nrow(audit)) {
      stop("Incomplete fixed-panel inference inputs for ", asset)
    }
    benchmark_loss <- unweighted$MeanPinball[1L]
    gain_pct <- 100 * inference$MeanPinballGainOfSelected[1L] / benchmark_loss
    lower_pct <- 100 * inference$Lower95[1L] / benchmark_loss
    upper_pct <- 100 * inference$Upper95[1L] / benchmark_loss
    width_reduction <- 100 * (
      unweighted_band$MeanWidth[1L] - selected_band$MeanWidth[1L]
    ) / unweighted_band$MeanWidth[1L]
    calibration_change <-
      selected$CalibrationMAE[1L] - unweighted$CalibrationMAE[1L]
    holm_positive <- is.finite(inference$HolmAdjustedPValue[1L]) &&
      inference$HolmAdjustedPValue[1L] < 0.05 && gain_pct > 0
    band_not_wider <- is.finite(width_reduction) && width_reduction >= 0
    shape_passed <- isTRUE(as.logical(audit$Passed[1L]))
    status <- if (is.finite(inference$HolmAdjustedPValue[1L]) &&
                  inference$HolmAdjustedPValue[1L] < 0.05 && gain_pct < 0) {
      "Holm-significant loss increase"
    } else if (!holm_positive) {
      "Not Holm-significant"
    } else if (!band_not_wider) {
      "Holm-significant + wider band"
    } else if (is.finite(calibration_change) && calibration_change > 0) {
      "Holm-significant + calibration trade-off"
    } else {
      "Holm-significant gain without observed cost"
    }
    data.frame(
      Asset = asset,
      TestMeanPinballGainPct = gain_pct,
      PercentileCILowerPct = lower_pct,
      PercentileCIUpperPct = upper_pct,
      RawTwoSidedPValue = inference$RawTwoSidedPValue[1L],
      HolmAdjustedPValue = inference$HolmAdjustedPValue[1L],
      TwoSidedWidthReductionPct = width_reduction,
      TestCalibrationMAEChange = calibration_change,
      HolmPositive = holm_positive,
      BandNotWider = band_not_wider,
      ShapeAuditPassed = shape_passed,
      Assessment = status,
      stringsAsFactors = FALSE
    )
  })
  list(tables = tables, summary = do.call(rbind, rows))
}

build_pave_guard_sensitivity <- function(adjusted_inference) {
  pave <- adjusted_inference[
    adjusted_inference$Asset == "PAVE", , drop = FALSE
  ]
  if (nrow(pave) != 1L) {
    stop("Exactly one PAVE row is required for guard sensitivity.")
  }
  observed_change <- pave$TestCalibrationMAEChange[1L]
  holm_p <- pave$HolmAdjustedPValue[1L]
  if (!is.finite(observed_change) || !is.finite(holm_p)) {
    stop("PAVE calibration change or Holm-adjusted p-value is unavailable.")
  }
  grid <- CFG$reporting_calibration_guard_grid
  data.frame(
    Asset = "PAVE",
    CalibrationGuard = grid,
    ObservedCalibrationMAEChange = observed_change,
    GuardStatus = ifelse(
      observed_change <= grid + 1e-12,
      "Guard passed", "Guard failed"
    ),
    HolmAdjustedPValue = holm_p,
    OverallPredictiveInference = ifelse(
      holm_p < 0.05,
      "Holm-significant",
      "Not Holm-significant"
    ),
    stringsAsFactors = FALSE
  )
}

build_validation_wis_identity_audit <- function(selected_configuration) {
  out <- selected_configuration[, c(
    "Asset", "WIS", "MeanPinball"
  ), drop = FALSE]
  names(out)[names(out) == "MeanPinball"] <- "ValidationMeanPinball"
  names(out)[names(out) == "WIS"] <- "ValidationWIS"
  out$TwiceValidationMeanPinball <- 2 * out$ValidationMeanPinball
  out$WISMinusTwiceMeanPinball <-
    out$ValidationWIS - out$TwiceValidationMeanPinball
  out$IdentityAuditPassed <- is.finite(out$WISMinusTwiceMeanPinball) &
    abs(out$WISMinusTwiceMeanPinball) <= 1e-12
  out
}

export_empirical_results <- function(
    empirical,
    failures = data.frame(
      Asset = character(0L), Status = character(0L),
      Error = character(0L), stringsAsFactors = FALSE
    )) {
  tables <- list(
    empirical_asset_failures = failures,
    descriptive_statistics = rbind_fill(lapply(empirical, `[[`, "descriptive")),
    split_audit = rbind_fill(lapply(empirical, `[[`, "split_audit")),
    depth_tuning_leaderboard = rbind_fill(lapply(empirical, function(z) z$tuning$leaderboard)),
    selected_depth_configuration = rbind_fill(lapply(empirical, function(z) z$tuning$selected)),
    empirical_main_metrics = rbind_fill(lapply(empirical, function(z) z$evaluation$metrics)),
    horizon_subset_efficiency = rbind_fill(lapply(empirical, `[[`, "horizon_subsets")),
    paired_loss_inference = rbind_fill(lapply(empirical, function(z) z$evaluation$paired_ci)),
    prediction_surface_audit = rbind_fill(lapply(empirical, function(z) z$evaluation$audits)),
    simultaneous_band_metrics = rbind_fill(lapply(empirical, function(z) z$bands$metrics)),
    simultaneous_upper_band_metrics = rbind_fill(lapply(empirical, function(z) z$upper_bands$metrics)),
    quantile_calibration = rbind_fill(lapply(empirical, `[[`, "calibration")),
    tail_weighting_comparison = rbind_fill(lapply(empirical, `[[`, "tail_weighting_comparison")),
    horizon_band_profiles = rbind_fill(lapply(empirical, `[[`, "width_profile")),
    horizon_upper_band_profiles = rbind_fill(lapply(empirical, `[[`, "upper_profile")),
    depth_sensitivity = rbind_fill(lapply(empirical, `[[`, "depth_sensitivity")),
    quantile_grid_sensitivity = rbind_fill(lapply(empirical, `[[`, "grid_sensitivity")),
    block_length_sensitivity = rbind_fill(lapply(empirical, `[[`, "block_sensitivity")),
    upper_block_length_sensitivity = rbind_fill(lapply(empirical, `[[`, "upper_block_sensitivity"))
  )
  adjusted <- build_fixed_panel_adjusted_inference(tables)
  tables <- adjusted$tables
  tables$fixed_panel_adjusted_inference <- adjusted$summary
  tables$validation_wis_pinball_identity_audit <-
    build_validation_wis_identity_audit(tables$selected_depth_configuration)
  tables$pave_calibration_guard_sensitivity <-
    build_pave_guard_sensitivity(tables$fixed_panel_adjusted_inference)
  current_upper <- tables$simultaneous_upper_band_metrics[
    tables$simultaneous_upper_band_metrics$Method == "Selected 2I-FQR",
    c(
      "Asset", "CoordinatewiseUpperCoverage", "MinimumHorizonUpperCoverage",
      "SimultaneousUpperPathCoverage", "CalibrationQuantile", "BlockLength"
    ),
    drop = FALSE
  ]
  names(current_upper)[names(current_upper) == "CoordinatewiseUpperCoverage"] <-
    "CurrentMeanUpperCoverage"
  names(current_upper)[names(current_upper) == "MinimumHorizonUpperCoverage"] <-
    "CurrentMinimumHorizonUpperCoverage"
  tables$legacy_upper_coverage_comparison <- merge(
    legacy_upper_coverage_reference(), current_upper,
    by = "Asset", all = FALSE, sort = FALSE
  )
  tables$legacy_upper_coverage_comparison$ChangeInMeanUpperCoverage <-
    tables$legacy_upper_coverage_comparison$CurrentMeanUpperCoverage -
    tables$legacy_upper_coverage_comparison$LegacyMeanUpperCoverage
  tables$legacy_upper_coverage_comparison$ChangeInMinimumHorizonUpperCoverage <-
    tables$legacy_upper_coverage_comparison$CurrentMinimumHorizonUpperCoverage -
    tables$legacy_upper_coverage_comparison$LegacyMinimumHorizonUpperCoverage
  for (nm in names(tables)) {
    if (nrow(tables[[nm]])) {
      write_csv_safe(tables[[nm]], file.path(DIR$csv, paste0(nm, ".csv")))
    }
  }

  write_latex_table(
    tables$descriptive_statistics,
    file.path(DIR$tex, "tab_descriptive_statistics.tex"),
    "Descriptive statistics of daily returns and squared returns.",
    "tab:descriptive-v2",
    note = "The sample is frozen at the dates reported for each asset."
  )
  write_latex_table(
    tables$selected_depth_configuration[, c(
      "Asset", "Gamma", "WMin", "WMax", "WIS", "MeanPinball",
      "LowerClipShare", "UpperClipShare"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_selected_depth_configuration.tex"),
    paste0(
      "Pre-test selection of the depth-weight configuration using the ",
      "internally embargoed training-validation split."
    ),
    "tab:depth-selection-v2",
    note = paste0(
      "Selection uses only the internally embargoed training-validation split. ",
      "$\\gamma=0$ denotes the unweighted specification."
    )
  )
  write_latex_table(
    tables$empirical_main_metrics[, c(
      "Asset", "Method", "MeanPinball", "CRPSApprox", "WIS",
      "Coverage05", "Coverage50", "Coverage95", "CalibrationMAE"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_empirical_main_metrics.tex"),
    "Test-set distributional accuracy and quantile calibration.",
    "tab:empirical-main-v2",
    note = "Lower pinball loss, CRPS approximation, WIS, and calibration MAE indicate better performance."
  )
  write_latex_table(
    tables$horizon_subset_efficiency[, c(
      "Asset", "HorizonSubset", "Method", "MeanPinball", "CRPSApprox", "WIS",
      "MeanPinballGainPctOfSelected", "CRPSGainPctOfSelected", "WISGainPctOfSelected"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_horizon_subset_efficiency.tex"),
    "Horizon-subset accuracy and gains of selected 2I--FQR; positive gains favor the selected method.",
    "tab:horizon-efficiency-v2",
    note = paste0(
      "Horizon ranges are evaluated separately to avoid interpreting full-path ",
      "gains as short-horizon gains."
    )
  )
  write_latex_table(
    tables$simultaneous_band_metrics[, c(
      "Asset", "Method", "NominalCoverage", "CoordinatewiseCoverage",
      "MinimumHorizonCoverage", "SimultaneousPathCoverage", "MeanWidth",
      "CalibrationQuantile", "CalibrationInflation", "BlockLength"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_simultaneous_band_metrics.tex"),
    "Coordinatewise and simultaneous coverage of the block-calibrated bands.",
    "tab:band-coverage-v2",
    note = paste0(
      "These are two-sided 90\\% bands. Coverage is reported empirically; ",
      "the calibration quantile is the larger of the direct finite-sample ",
      "quantile and the prespecified upper moving-block bootstrap guard. ",
      "The moving-block bootstrap calibration is not presented as exact ",
      "finite-sample distribution-free inference under arbitrary serial dependence."
    )
  )
  write_latex_table(
    tables$simultaneous_upper_band_metrics[, c(
      "Asset", "Method", "NominalCoverage", "CoordinatewiseUpperCoverage",
      "MinimumHorizonUpperCoverage", "SimultaneousUpperPathCoverage",
      "MeanUpperBoundary", "CalibrationQuantile", "CalibrationInflation",
      "BlockLength"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_simultaneous_upper_band_metrics.tex"),
    "Coverage of the one-sided 95\\% block-calibrated simultaneous upper bands.",
    "tab:upper-band-coverage-v2e",
    note = paste0(
      "The nominal target applies to simultaneous upper-path coverage. Mean ",
      "coordinatewise coverage can therefore exceed 95\\% and is not described ",
      "as near nominal when materially conservative. The calibration quantile ",
      "uses the same prespecified upper moving-block bootstrap guard as the ",
      "two-sided band."
    )
  )
  write_latex_table(
    tables$legacy_upper_coverage_comparison[, c(
      "Asset", "LegacyMeanUpperCoverage", "CurrentMeanUpperCoverage",
      "LegacyMinimumHorizonUpperCoverage", "CurrentMinimumHorizonUpperCoverage",
      "SimultaneousUpperPathCoverage", "ChangeInMeanUpperCoverage",
      "ChangeInMinimumHorizonUpperCoverage"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_legacy_vs_current_upper_coverage.tex"),
    paste0(
      "Direct comparison of the originally reported upper-side coverage with ",
      "the leakage-disciplined one-sided 95\\% simultaneous upper-band results."
    ),
    "tab:legacy-current-upper-coverage",
    note = paste0(
      "Original values are transcribed from the manuscript's former Table 4 ",
      "and are not recomputed. Revised values are generated by this pipeline. ",
      "BTC-USD is the only asset common to the former and revised manuscript ",
      "panels; hence it is the only directly comparable series."
    )
  )
  write_latex_table(
    tables$tail_weighting_comparison[, c(
      "Asset", "Tau", "ObservedCoverageUnweighted", "ObservedCoverageWeighted",
      "AbsoluteCalibrationErrorUnweighted", "AbsoluteCalibrationErrorWeighted",
      "WeightedMinusUnweightedPredictedQuantile",
      "WeightedMinusUnweightedPinball"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_tail_weighting_comparison.tex"),
    paste0(
      "Tail-specific comparison of selected depth weighting with the unweighted fit. ",
      "Negative changes in absolute calibration error or pinball loss favor weighting."
    ),
    "tab:tail-weighting-v2e",
    note = paste0(
      "$CE$ denotes coverage minus the nominal quantile level. ",
      "$\\Delta$ is weighted minus unweighted; negative loss changes favor weighting."
    )
  )
  write_latex_table(
    tables$paired_loss_inference[, c(
      "Asset", "Comparator", "MeanPinballGainOfSelected", "Lower95", "Upper95",
      "RawTwoSidedPValue", "HolmAdjustedPValue"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_paired_loss_inference.tex"),
    paste0(
      "Two-sided moving-block bootstrap inference for mean pinball-loss gains ",
      "of selected 2I--FQR."
    ),
    "tab:paired-inference-v2j",
    note = paste0(
      "Positive gains favor selected 2I--FQR. The two-sided percentile interval ",
      "and raw two-sided bootstrap p-value use the same non-centred block-bootstrap ",
      "distribution. Holm adjustment is reported across the fixed five-asset ",
      "panel for the selected-versus-unweighted comparison; adjusted values are ",
      "undefined for the remaining descriptive benchmark comparisons. ",
      "Self-comparisons and numerically identical comparator pairs are omitted. ",
      "Inference is restricted to the prespecified substantive benchmarks."
    )
  )
  write_latex_table(
    tables$fixed_panel_adjusted_inference[, c(
      "Asset", "TestMeanPinballGainPct", "PercentileCILowerPct",
      "PercentileCIUpperPct", "RawTwoSidedPValue", "HolmAdjustedPValue",
      "TwoSidedWidthReductionPct", "TestCalibrationMAEChange", "Assessment"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_fixed_panel_adjusted_inference.tex"),
    paste0(
      "Multiplicity-adjusted predictive evidence for the fixed five-asset ",
      "manuscript panel."
    ),
    "tab:fixed-panel-adjusted-inference-v2j",
    note = paste0(
      "Intervals and raw p-values are two-sided and are obtained from the same ",
      "moving-block bootstrap distribution. Holm adjustment is applied across ",
      "the five fixed asset-level selected-versus-unweighted tests. Positive ",
      "width reductions denote narrower two-sided bands. The panel is specified ",
      "in the design configuration and is not formed from these results."
    )
  )
  write_latex_table(
    tables$validation_wis_pinball_identity_audit[, c(
      "Asset", "ValidationWIS", "ValidationMeanPinball",
      "TwiceValidationMeanPinball", "WISMinusTwiceMeanPinball",
      "IdentityAuditPassed"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_validation_wis_pinball_identity_audit.tex"),
    paste0(
      "Validation-sample audit of the WIS and mean-pinball relationship on ",
      "the seven-quantile grid."
    ),
    "tab:validation-wis-pinball-identity-v2j",
    note = paste0(
      "The quantities in this table are computed on the internally embargoed ",
      "training-validation sample used for depth-configuration selection; they ",
      "are not test-set scores. With the symmetric seven-quantile grid and the ",
      "implemented normalization, WIS equals twice the equally averaged pinball ",
      "loss. Their percentage gains are identical by construction and are not ",
      "treated as independent selection criteria."
    )
  )
  write_latex_table(
    tables$pave_calibration_guard_sensitivity[, c(
      "CalibrationGuard", "ObservedCalibrationMAEChange", "GuardStatus",
      "HolmAdjustedPValue", "OverallPredictiveInference"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_pave_calibration_guard_sensitivity.tex"),
    "PAVE calibration-guard sensitivity for the fixed five-asset manuscript panel.",
    "tab:pave-calibration-guard-sensitivity-v2j",
    note = paste0(
      "The guard class changes because the observed calibration-MAE increase ",
      "lies between 0.015 and 0.020. Guard passage is a calibration diagnostic ",
      "and does not override the non-significant Holm-adjusted predictive test."
    )
  )
  if (nrow(tables$depth_sensitivity)) {
    write_latex_table(
      tables$depth_sensitivity[, c(
        "Asset", "Gamma", "WMin", "WMax", "MeanPinball", "WIS",
        "Coverage05", "Coverage95", "CalibrationMAE"
      ), drop = FALSE],
      file.path(DIR$tex, "tab_depth_sensitivity.tex"),
      "Sensitivity to the response-depth weighting parameters.",
      "tab:depth-sensitivity-v2",
      note = "$\\gamma=0$ is the unweighted benchmark; all configurations are evaluated on the same test set."
    )
  }
  if (nrow(tables$quantile_grid_sensitivity)) {
    write_latex_table(
      tables$quantile_grid_sensitivity[, c(
        "Asset", "Grid", "QuantileCount", "CommonGridPinball",
        "MedianPinball", "MeanPinball", "CRPSApprox", "WIS"
      ), drop = FALSE],
      file.path(DIR$tex, "tab_quantile_grid_sensitivity.tex"),
      "Sensitivity to sparse, dense, median-only, and asymmetric quantile grids.",
      "tab:grid-sensitivity-v2",
      note = paste0(
        "Common-grid pinball loss uses the shared quantiles 0.05, 0.50, and 0.95 ",
        "when available. CRPS approximation is undefined for the median-only grid ",
        "because numerical integration requires more than one quantile. WIS is ",
        "undefined for grids that omit any prespecified interval endpoint. ",
        "A double dash denotes an undefined quantity."
      )
    )
  }
  if (nrow(tables$block_length_sensitivity)) {
    write_latex_table(
      tables$block_length_sensitivity[, c(
        "Asset", "BlockLength", "CoordinatewiseCoverage",
        "MinimumHorizonCoverage", "SimultaneousPathCoverage", "MeanWidth"
      ), drop = FALSE],
      file.path(DIR$tex, "tab_block_length_sensitivity.tex"),
      "Sensitivity of simultaneous bands to the moving-block length.",
      "tab:block-sensitivity-v2",
      note = "All specifications use the selected 2I--FQR quantile surface."
    )
  }
  if (nrow(tables$upper_block_length_sensitivity)) {
    write_latex_table(
      tables$upper_block_length_sensitivity[, c(
        "Asset", "BlockLength", "CoordinatewiseUpperCoverage",
        "MinimumHorizonUpperCoverage", "SimultaneousUpperPathCoverage",
        "MeanUpperBoundary"
      ), drop = FALSE],
      file.path(DIR$tex, "tab_upper_block_length_sensitivity.tex"),
      "Sensitivity of the one-sided 95\\% simultaneous upper band to block length.",
      "tab:upper-block-sensitivity-v2e",
      note = "Coverage and the mean upper boundary are reported for each prespecified block length."
    )
  }

  write_latex_table(
    tables$split_audit[, c(
      "Asset", "TrainN", "CalibrationN", "TestN",
      "EmbargoTrainCalibration", "EmbargoCalibrationTest",
      "TrainCalibrationNoOverlap", "CalibrationTestNoOverlap"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_leakage_split_audit.tex"),
    "Leakage and target-window overlap audit for empirical sample splits.",
    "tab:split-audit-v2e",
    note = "A value of Yes in the final two columns confirms non-overlapping target windows."
  )
  write_latex_table(
    tables$prediction_surface_audit[
      tables$prediction_surface_audit$Method == "Selected 2I-FQR",
      c(
        "Asset", "Method", "MinimumPrediction", "MaximumHorizonViolation",
        "MaximumQuantileViolation", "Passed"
      ),
      drop = FALSE
    ],
    file.path(DIR$tex, "tab_prediction_surface_audit.tex"),
    "Audit of non-negativity and monotonicity constraints for selected 2I--FQR forecasts.",
    "tab:surface-audit-v2e",
    note = "Numerical violations are assessed using the prespecified projection tolerance."
  )
  write_latex_table(
    tables$quantile_calibration[, c(
      "Asset", "Method", "Tau", "ObservedCoverage"
    ), drop = FALSE],
    file.path(DIR$tex, "tab_quantile_calibration.tex"),
    "Observed test-set coverage by quantile level.",
    "tab:quantile-calibration-v2e",
    note = "Observed coverage is the proportion of realized responses not exceeding the fitted quantile."
  )
  make_empirical_figures(empirical, display_assets = FIXED_PANEL)
  make_fixed_panel_inference_figure(tables$fixed_panel_adjusted_inference)
  make_pave_guard_sensitivity_figure(
    tables$pave_calibration_guard_sensitivity
  )
  tables
}

expected_latex_tables <- function() {
  expected <- character(0L)
  if (CFG$run_empirical) {
    expected <- c(
      expected,
      "tab_descriptive_statistics.tex",
      "tab_selected_depth_configuration.tex",
      "tab_empirical_main_metrics.tex",
      "tab_horizon_subset_efficiency.tex",
      "tab_simultaneous_band_metrics.tex",
      "tab_simultaneous_upper_band_metrics.tex",
      "tab_legacy_vs_current_upper_coverage.tex",
      "tab_tail_weighting_comparison.tex",
      "tab_paired_loss_inference.tex",
      "tab_fixed_panel_adjusted_inference.tex",
      "tab_validation_wis_pinball_identity_audit.tex",
      "tab_pave_calibration_guard_sensitivity.tex",
      "tab_leakage_split_audit.tex",
      "tab_prediction_surface_audit.tex",
      "tab_quantile_calibration.tex"
    )
    if (CFG$run_depth_sensitivity) {
      expected <- c(expected, "tab_depth_sensitivity.tex")
    }
    if (CFG$run_grid_sensitivity) {
      expected <- c(expected, "tab_quantile_grid_sensitivity.tex")
    }
    if (CFG$run_block_sensitivity) {
      expected <- c(
        expected,
        "tab_block_length_sensitivity.tex",
        "tab_upper_block_length_sensitivity.tex"
      )
    }
  }
  if (CFG$run_simulation) {
    expected <- c(
      expected,
      "tab_simulation_performance.tex",
      "tab_simulation_band_coverage.tex",
      "tab_simulation_shape_audit.tex",
      "tab_simulation_tail_calibration.tex",
      "tab_simulation_oracle_bias.tex",
      "tab_simulation_weighting_comparison.tex"
    )
  }
  unique(expected)
}

audit_latex_tables <- function() {
  expected <- expected_latex_tables()
  if (!length(expected)) return(data.frame())
  count_fixed <- function(x, pattern) {
    sum((nchar(x) - nchar(gsub(pattern, "", x, fixed = TRUE))) / nchar(pattern))
  }
  rows <- lapply(expected, function(filename) {
    path <- file.path(DIR$tex, filename)
    exists <- file.exists(path)
    lines <- if (exists) readLines(path, warn = FALSE, encoding = "UTF-8") else character(0L)
    escaped_underscores <- if (length(lines)) count_fixed(lines, "\\\\_") else NA_real_
    data.frame(
      File = filename,
      Exists = exists,
      NonEmpty = exists && file.info(path)$size > 0,
      EscapedUnderscoreCount = escaped_underscores,
      Passed = exists && file.info(path)$size > 0 &&
        is.finite(escaped_underscores) && escaped_underscores == 0,
      stringsAsFactors = FALSE
    )
  })
  audit <- do.call(rbind, rows)
  write_csv_safe(audit, file.path(DIR$logs, "latex_table_audit.csv"))
  audit
}

run_final_audit <- function(projection_test, empirical_tables = NULL,
                            simulation = NULL) {
  rows <- list(projection_test)
  if (!is.null(empirical_tables)) {
    s <- empirical_tables$split_audit
    rows[[length(rows) + 1L]] <- data.frame(
      Test = paste0("Leakage-safe split: ", s$Asset),
      Objective = NA_real_,
      ExpectedObjective = NA_real_,
      Passed = s$TrainCalibrationNoOverlap & s$CalibrationTestNoOverlap
    )
    a <- empirical_tables$prediction_surface_audit
    a <- a[a$Method == "Selected 2I-FQR", , drop = FALSE]
    rows[[length(rows) + 1L]] <- data.frame(
      Test = paste0("Selected surface constraints: ", a$Asset),
      Objective = NA_real_,
      ExpectedObjective = NA_real_,
      Passed = a$Passed
    )
    ub <- empirical_tables$simultaneous_upper_band_metrics
    rows[[length(rows) + 1L]] <- data.frame(
      Test = paste0("One-sided 95% upper-band output: ", ub$Asset, " / ", ub$Method),
      Objective = ub$NominalCoverage,
      ExpectedObjective = 0.95,
      Passed = abs(ub$NominalCoverage - 0.95) < 1e-12 &
        is.finite(ub$SimultaneousUpperPathCoverage),
      stringsAsFactors = FALSE
    )
    tw <- empirical_tables$tail_weighting_comparison
    rows[[length(rows) + 1L]] <- data.frame(
      Test = paste0("Tail weighting diagnostic: ", tw$Asset, " / tau=", tw$Tau),
      Objective = tw$Tau,
      ExpectedObjective = tw$Tau,
      Passed = tw$Tau %in% c(0.05, 0.95) &
        is.finite(tw$WeightedMinusUnweightedPinball),
      stringsAsFactors = FALSE
    )
    if (CFG$run_block_sensitivity) {
      primary_two <- empirical_tables$simultaneous_band_metrics[
        empirical_tables$simultaneous_band_metrics$Method == "Selected 2I-FQR",
        c(
          "Asset", "CoordinatewiseCoverage", "MinimumHorizonCoverage",
          "SimultaneousPathCoverage", "MeanWidth", "CalibrationQuantile"
        ),
        drop = FALSE
      ]
      sensitivity_two <- empirical_tables$block_length_sensitivity[
        empirical_tables$block_length_sensitivity$Method == "Selected 2I-FQR" &
          empirical_tables$block_length_sensitivity$BlockLength ==
            CFG$primary_block_length,
        c(
          "Asset", "CoordinatewiseCoverage", "MinimumHorizonCoverage",
          "SimultaneousPathCoverage", "MeanWidth", "CalibrationQuantile"
        ),
        drop = FALSE
      ]
      two_compare <- merge(
        primary_two, sensitivity_two, by = "Asset",
        suffixes = c("Primary", "Sensitivity"), all = TRUE
      )
      two_fields <- c(
        "CoordinatewiseCoverage", "MinimumHorizonCoverage",
        "SimultaneousPathCoverage", "MeanWidth", "CalibrationQuantile"
      )
      two_differences <- unlist(lapply(two_fields, function(field) {
        abs(
          two_compare[[paste0(field, "Primary")]] -
            two_compare[[paste0(field, "Sensitivity")]]
        )
      }), use.names = FALSE)
      max_two_difference <- if (length(two_differences)) {
        max(two_differences, na.rm = TRUE)
      } else {
        Inf
      }
      rows[[length(rows) + 1L]] <- data.frame(
        Test = "Primary two-sided band equals its block-sensitivity row",
        Objective = max_two_difference,
        ExpectedObjective = 0,
        Passed = nrow(two_compare) == nrow(primary_two) &&
          all(is.finite(two_differences)) && max_two_difference <= 1e-12,
        stringsAsFactors = FALSE
      )

      primary_upper <- empirical_tables$simultaneous_upper_band_metrics[
        empirical_tables$simultaneous_upper_band_metrics$Method ==
          "Selected 2I-FQR",
        c(
          "Asset", "CoordinatewiseUpperCoverage",
          "MinimumHorizonUpperCoverage", "SimultaneousUpperPathCoverage",
          "MeanUpperBoundary", "CalibrationQuantile"
        ),
        drop = FALSE
      ]
      sensitivity_upper <- empirical_tables$upper_block_length_sensitivity[
        empirical_tables$upper_block_length_sensitivity$Method ==
          "Selected 2I-FQR" &
          empirical_tables$upper_block_length_sensitivity$BlockLength ==
            CFG$primary_block_length,
        c(
          "Asset", "CoordinatewiseUpperCoverage",
          "MinimumHorizonUpperCoverage", "SimultaneousUpperPathCoverage",
          "MeanUpperBoundary", "CalibrationQuantile"
        ),
        drop = FALSE
      ]
      upper_compare <- merge(
        primary_upper, sensitivity_upper, by = "Asset",
        suffixes = c("Primary", "Sensitivity"), all = TRUE
      )
      upper_fields <- c(
        "CoordinatewiseUpperCoverage", "MinimumHorizonUpperCoverage",
        "SimultaneousUpperPathCoverage", "MeanUpperBoundary",
        "CalibrationQuantile"
      )
      upper_differences <- unlist(lapply(upper_fields, function(field) {
        abs(
          upper_compare[[paste0(field, "Primary")]] -
            upper_compare[[paste0(field, "Sensitivity")]]
        )
      }), use.names = FALSE)
      max_upper_difference <- if (length(upper_differences)) {
        max(upper_differences, na.rm = TRUE)
      } else {
        Inf
      }
      rows[[length(rows) + 1L]] <- data.frame(
        Test = "Primary upper band equals its block-sensitivity row",
        Objective = max_upper_difference,
        ExpectedObjective = 0,
        Passed = nrow(upper_compare) == nrow(primary_upper) &&
          all(is.finite(upper_differences)) && max_upper_difference <= 1e-12,
        stringsAsFactors = FALSE
      )
    }
    paired <- empirical_tables$paired_loss_inference
    paired_clean <- nrow(paired) > 0L &&
      !any(paired$Comparator == "Selected 2I-FQR") &&
      all(is.finite(paired$MeanPinballGainOfSelected))
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "Paired-inference comparator cleanup",
      Objective = nrow(paired),
      ExpectedObjective = NA_real_,
      Passed = paired_clean,
      stringsAsFactors = FALSE
    )
    primary_paired <- paired[
      paired$Comparator == "QR + exact 2I+" &
        paired$Asset %in% FIXED_PANEL,
      , drop = FALSE
    ]
    ci_excludes_zero <- with(
      primary_paired,
      (Lower95 > 0 & Upper95 > 0) | (Lower95 < 0 & Upper95 < 0)
    )
    raw_test_rejects <- primary_paired$RawTwoSidedPValue < 0.05
    inference_aligned <-
      nrow(primary_paired) == length(FIXED_PANEL) &&
      all(is.finite(primary_paired$RawTwoSidedPValue)) &&
      all(is.finite(primary_paired$HolmAdjustedPValue)) &&
      all(primary_paired$Lower95 <= primary_paired$Upper95) &&
      all(primary_paired$RawTwoSidedPValue >= 0 & primary_paired$RawTwoSidedPValue <= 1) &&
      all(primary_paired$HolmAdjustedPValue + 1e-12 >= primary_paired$RawTwoSidedPValue)
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "Finite two-sided intervals and multiplicity-adjusted tests",
      Objective = sum(is.finite(primary_paired$RawTwoSidedPValue)),
      ExpectedObjective = length(FIXED_PANEL),
      Passed = inference_aligned,
      stringsAsFactors = FALSE
    )
    identity <- empirical_tables$validation_wis_pinball_identity_audit
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "Validation-sample WIS and pinball identity",
      Objective = sum(identity$IdentityAuditPassed),
      ExpectedObjective = length(FIXED_PANEL),
      Passed = nrow(identity) == length(FIXED_PANEL) &&
        all(identity$IdentityAuditPassed),
      stringsAsFactors = FALSE
    )
    fixed_panel_ok <- identical(
      as.character(empirical_tables$fixed_panel_adjusted_inference$Asset),
      FIXED_PANEL
    )
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "Fixed manuscript panel retained without outcome-based replacement",
      Objective = nrow(empirical_tables$fixed_panel_adjusted_inference),
      ExpectedObjective = length(FIXED_PANEL),
      Passed = fixed_panel_ok,
      stringsAsFactors = FALSE
    )
    observed_classes <- unique(as.character(
      empirical_tables$fixed_panel_adjusted_inference$Assessment
    ))
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "Fixed-panel assessment labels describe observed outcomes",
      Objective = length(observed_classes),
      ExpectedObjective = NA_real_,
      Passed = all(observed_classes %in% FIXED_PANEL_ASSESSMENT_LEVELS),
      stringsAsFactors = FALSE
    )
    pave_sensitivity <- empirical_tables$pave_calibration_guard_sensitivity
    expected_guard_status <- ifelse(
      pave_sensitivity$ObservedCalibrationMAEChange <=
        pave_sensitivity$CalibrationGuard + 1e-12, "Guard passed", "Guard failed"
    )
    pave_guard_ok <- nrow(pave_sensitivity) == 3L &&
      identical(
        as.numeric(pave_sensitivity$CalibrationGuard),
        as.numeric(CFG$reporting_calibration_guard_grid)
      ) &&
      identical(
        as.character(pave_sensitivity$GuardStatus),
        expected_guard_status
      ) &&
      all(
        pave_sensitivity$OverallPredictiveInference == ifelse(
          pave_sensitivity$HolmAdjustedPValue < 0.05,
          "Holm-significant", "Not Holm-significant"
        )
      )
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "PAVE calibration-guard sensitivity and predictive inference",
      Objective = sum(pave_sensitivity$GuardStatus == "Guard passed"),
      ExpectedObjective = sum(expected_guard_status == "Guard passed"),
      Passed = pave_guard_ok,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(simulation) && nrow(simulation$replication_level)) {
    failures <- sum(simulation$replication_level$Status == "failed")
    total_tasks <- length(simulation_scenarios()) * CFG$simulation_reps
    successful <- simulation$replication_level[
      simulation$replication_level$Status == "ok", , drop = FALSE
    ]
    completed_tasks <- length(unique(paste(successful$Scenario, successful$Replication)))
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "Simulation task completion",
      Objective = failures,
      ExpectedObjective = 0,
      Passed = failures == 0 && completed_tasks == total_tasks
    )
    oracle_rows <- sum(is.finite(successful$OracleQuantileBias05))
    expected_oracle_rows <- length(simulation_scenarios()) *
      min(CFG$simulation_reps, CFG$oracle_diagnostic_reps) * 3L
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "Simulation oracle diagnostic completion",
      Objective = oracle_rows,
      ExpectedObjective = expected_oracle_rows,
      Passed = oracle_rows == expected_oracle_rows
    )
    projected <- successful[
      successful$Method %in% c("QR + exact 2I+", "Depth QR + exact 2I+"),
      , drop = FALSE
    ]
    expected_projected <- total_tasks * 2L
    projected_pass <- !is.na(projected$ProjectedSetAuditPassed) &
      as.logical(projected$ProjectedSetAuditPassed) &
      !is.na(projected$CalibrationShapeAuditPassed) &
      as.logical(projected$CalibrationShapeAuditPassed) &
      !is.na(projected$ShapeAuditPassed) &
      as.logical(projected$ShapeAuditPassed) &
      projected$CalibrationMinimumPrediction >= -CFG$surface_audit_tolerance &
      projected$CalibrationMaximumHorizonViolation <= CFG$surface_audit_tolerance &
      projected$CalibrationMaximumQuantileViolation <= CFG$surface_audit_tolerance &
      projected$MinimumPrediction >= -CFG$surface_audit_tolerance &
      projected$MaximumHorizonViolation <= CFG$surface_audit_tolerance &
      projected$MaximumQuantileViolation <= CFG$surface_audit_tolerance
    projected_pass[is.na(projected_pass)] <- FALSE
    rows[[length(rows) + 1L]] <- data.frame(
      Test = paste0(
        "All projected simulation calibration and test surfaces satisfy ",
        "shape constraints"
      ),
      Objective = sum(!projected_pass),
      ExpectedObjective = 0,
      Passed = nrow(projected) == expected_projected && all(projected_pass),
      stringsAsFactors = FALSE
    )
    diagnostic <- simulation$band_claim_diagnostic
    expected_diagnostic_rows <- length(simulation_scenarios()) * 2L * 2L
    diagnostic_complete <- nrow(diagnostic) == expected_diagnostic_rows &&
      all(is.finite(diagnostic$CoverageMean)) &&
      all(is.finite(diagnostic$MonteCarloSE)) &&
      all(nzchar(diagnostic$CoverageAssessment))
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "Simulation band-coverage claim diagnostic completion",
      Objective = nrow(diagnostic),
      ExpectedObjective = expected_diagnostic_rows,
      Passed = diagnostic_complete,
      stringsAsFactors = FALSE
    )
  }
  if (identical(CFG$profile, "publication")) {
    publication_ready <- CFG$run_empirical && CFG$run_depth_sensitivity &&
      CFG$run_grid_sensitivity && CFG$run_block_sensitivity &&
      CFG$run_simulation && CFG$simulation_reps >= 500L &&
      CFG$block_boot_reps >= 2000L &&
      CFG$simulation_block_boot_reps >= 250L &&
      CFG$oracle_diagnostic_reps >= 100L && CFG$oracle_origins >= 30L &&
      CFG$oracle_draws >= 2000L
    rows[[length(rows) + 1L]] <- data.frame(
      Test = "Publication-profile computational requirements",
      Objective = CFG$simulation_reps,
      ExpectedObjective = 500,
      Passed = publication_ready,
      stringsAsFactors = FALSE
    )
  }
  latex_audit <- audit_latex_tables()
  if (nrow(latex_audit)) {
    rows[[length(rows) + 1L]] <- data.frame(
      Test = paste0("Manuscript-ready LaTeX table: ", latex_audit$File),
      Objective = latex_audit$EscapedUnderscoreCount,
      ExpectedObjective = 0,
      Passed = latex_audit$Passed,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(empirical_tables)) {
    required_reporting_figures <- c(
      "fig_fixed_panel_adjusted_pinball_gain.png",
      "fig_fixed_panel_adjusted_pinball_gain.pdf",
      "fig_simultaneous_band_width.png",
      "fig_simultaneous_band_width.pdf",
      "fig_pave_calibration_guard_sensitivity.png",
      "fig_pave_calibration_guard_sensitivity.pdf"
    )
    reporting_paths <- file.path(DIR$figures, required_reporting_figures)
    reporting_exists <- file.exists(reporting_paths)
    reporting_nonempty <- reporting_exists
    reporting_nonempty[reporting_exists] <-
      file.info(reporting_paths[reporting_exists])$size > 0
    reporting_nonempty[is.na(reporting_nonempty)] <- FALSE
    rows[[length(rows) + 1L]] <- data.frame(
      Test = paste0(
        "Integrated manuscript reporting figure: ",
        required_reporting_figures
      ),
      Objective = as.numeric(reporting_nonempty),
      ExpectedObjective = 1,
      Passed = reporting_nonempty,
      stringsAsFactors = FALSE
    )
  }
  audit <- rbind_fill(rows)
  write_csv_safe(audit, file.path(DIR$logs, "final_audit.csv"))
  if (!all(audit$Passed)) {
    failed <- paste(audit$Test[!audit$Passed], collapse = "; ")
    stop("Final audit failed: ", failed)
  }
  audit
}

core_unit_tests <- function() {
  projection <- projection_unit_test()

  split <- make_embargo_split(
    n = 300L, H = 10L, train_end_frac = 0.60,
    cal_end_frac = 0.80, embargo = 10L, min_each = 20L
  )
  split_ok <- max(split$train) + 10L < min(split$cal) &&
    max(split$cal) + 10L < min(split$test)
  split_test <- data.frame(
    Test = "Synthetic H-embargo separation",
    Objective = as.numeric(split_ok),
    ExpectedObjective = 1,
    Passed = split_ok
  )

  Y <- cbind(c(1, 2, 3, 4), c(11, 12, 13, 14))
  taus <- c(0.25, 0.50, 0.75)
  Q <- make_training_climatology(Y, n_forecast = 3L, taus = taus)
  expected <- rbind(
    quantile_type8(Y[, 1L], taus),
    quantile_type8(Y[, 2L], taus)
  )
  benchmark_ok <- all(vapply(seq_len(dim(Q)[1L]), function(i) {
    max(abs(matrix(Q[i, , ], nrow = 2L, ncol = 3L) - expected)) < 1e-10
  }, logical(1L)))
  benchmark_test <- data.frame(
    Test = "Climatology quantile replication uses times",
    Objective = as.numeric(benchmark_ok),
    ExpectedObjective = 1,
    Passed = benchmark_ok
  )

  Ytrain_band <- cbind(seq(0.10, 0.29, length.out = 20L),
                       seq(0.20, 0.58, length.out = 20L),
                       seq(0.30, 0.87, length.out = 20L))
  Ycal_band <- Ytrain_band[1:8, , drop = FALSE]
  taus_band <- c(0.05, 0.50, 0.95)
  Qcal_band <- array(NA_real_, dim = c(8L, 3L, 3L))
  Qtest_band <- array(NA_real_, dim = c(4L, 3L, 3L))
  for (k in seq_along(taus_band)) {
    Qcal_band[, , k] <- Ycal_band * (0.6 + 0.4 * taus_band[k])
    Qtest_band[, , k] <- Ytrain_band[9:12, , drop = FALSE] *
      (0.6 + 0.4 * taus_band[k])
  }
  upper_test_band <- make_simultaneous_upper_band(
    Ytrain_band, Ycal_band, Qcal_band, Qtest_band, taus_band,
    alpha = 0.05, block_length = 1L, block_reps = 1L,
    seed = CFG$seed
  )
  upper_ok <- identical(dim(upper_test_band$upper), c(4L, 3L)) &&
    all(upper_test_band$upper >= 0) &&
    all(apply(upper_test_band$upper, 1L, function(z) all(diff(z) >= -1e-12))) &&
    abs(upper_test_band$endpoint_tau - 0.95) < 1e-12
  upper_band_test <- data.frame(
    Test = "One-sided 95% simultaneous upper-band construction",
    Objective = as.numeric(upper_ok),
    ExpectedObjective = 1,
    Passed = upper_ok
  )

  same_tail <- compare_weighted_unweighted_tails(
    Ycal_band, Qcal_band, Qcal_band, taus_band, asset = "unit-test"
  )
  tail_ok <- all(abs(same_tail$WeightedMinusUnweightedCoverage) < 1e-12) &&
    all(abs(same_tail$WeightedMinusUnweightedPinball) < 1e-12)
  tail_test <- data.frame(
    Test = "Weighted--unweighted tail diagnostic identity",
    Objective = as.numeric(tail_ok),
    ExpectedObjective = 1,
    Passed = tail_ok
  )

  latex_test_file <- tempfile(fileext = ".tex")
  write_latex_table(
    data.frame(
      Model_ID = "depth_weighted_fit",
      MeanPinball = 0.125,
      Gamma = 1,
      UndefinedMetric = NA_real_,
      stringsAsFactors = FALSE
    ),
    latex_test_file,
    "Academic table writer test.",
    "tab:academic-writer-test"
  )
  latex_lines <- readLines(latex_test_file, warn = FALSE, encoding = "UTF-8")
  unlink(latex_test_file)
  latex_ok <- !any(grepl("\\\\_", latex_lines, fixed = TRUE)) &&
    any(grepl("Mean pinball loss", latex_lines, fixed = TRUE)) &&
    any(grepl("depth weighted fit", latex_lines, fixed = TRUE)) &&
    any(grepl("--", latex_lines, fixed = TRUE))
  latex_test <- data.frame(
    Test = "Academic LaTeX labels and underscore removal",
    Objective = as.numeric(latex_ok),
    ExpectedObjective = 1,
    Passed = latex_ok
  )
  rbind(
    projection, split_test, benchmark_test, upper_band_test, tail_test, latex_test
  )
}

# ==============================================================================
# R1 additions: diagnostics, sensitivity, conditional distributional benchmark
# ==============================================================================
r1_assert <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call.=FALSE)
  invisible(TRUE)
}
r1_close <- function(a,b,tol=1e-10) {
  identical(dim(a),dim(b)) && length(a)==length(b) &&
    all(is.finite(a)) && all(is.finite(b)) && max(abs(a-b),0) <= tol
}
r1_md5_object <- function(x) {
  f <- tempfile(fileext=".rds"); on.exit(unlink(f),add=TRUE)
  saveRDS(x,f,version=2,compress=FALSE)
  unname(tools::md5sum(f))
}
r1_save_rds <- function(x,path) {
  tmp <- tempfile(pattern="checkpoint_",tmpdir=dirname(path))
  on.exit(unlink(tmp),add=TRUE)
  saveRDS(x,tmp,version=2,compress="gzip")
  if (.Platform$OS.type == "windows" && file.exists(path)) unlink(path)
  if (!file.rename(tmp,path)) stop("Cannot install checkpoint: ",path)
  invisible(path)
}
r1_core_design <- function() {
  CFG[c("seed","assets","start_date","end_date","H","taus","split_train_end",
        "split_cal_end","embargo","internal_validation_fraction","har_windows",
        "qr_method","gamma_grid","wmin_grid","wmax","wmax_sensitivity_grid","projection_tol",
        "surface_audit_tolerance","projection_max_iter","block_bootstrap_guard_probability","rolling_history",
        "primary_block_length","block_boot_reps","calibration_reps",
        "run_depth_sensitivity","run_grid_sensitivity","run_block_sensitivity",
        "data_mode","r1_version")]
}
r1_block_indices <- function(n,L,B,seed) {
  r1_assert(n>=2L && L>=1L && B>=1L,"Invalid resampling dimensions.")
  L <- min(as.integer(L),as.integer(n))
  with_seed(seed,replicate(B,{
    starts <- sample.int(n-L+1L,ceiling(n/L),replace=TRUE)
    as.integer(as.vector(outer(seq_len(L)-1L,starts,"+"))[seq_len(n)])
  }))
}
r1_validate_one <- function(z,asset) {
  r1_assert(identical(as.character(z$asset),asset),paste("Wrong cached asset",asset))
  p <- z$prices
  r1_assert(is.data.frame(p) && all(c("Date","Asset","Adjusted") %in% names(p)),
            paste("Cached price data absent",asset))
  r1_assert(all(as.character(p$Asset)==asset) && !anyDuplicated(p$Date) &&
              all(is.finite(p$Adjusted) & p$Adjusted>0),paste("Invalid cached prices",asset))
  p$Date <- as.Date(p$Date)
  r1_assert(min(p$Date)>=CFG$start_date && max(p$Date)<=CFG$end_date &&
              min(p$Date)<=CFG$start_date+10L && max(p$Date)>=CFG$end_date-10L,
            paste("Cached sample dates differ from the declared design",asset))
  fresh <- build_curve_dataset_from_prices(p,CFG$H,CFG$har_windows)
  r1_assert(r1_close(fresh$Y,z$dataset$Y,1e-12) &&
              r1_close(fresh$X,z$dataset$X,1e-12) &&
              identical(fresh$origin_date,z$dataset$origin_date),
            paste("Cached targets/predictors do not match cached prices",asset))
  split <- make_embargo_split(nrow(fresh$Y),CFG$H,CFG$split_train_end,
                             CFG$split_cal_end,CFG$embargo)
  r1_assert(identical(split,z$split),paste("Cached sample split differs",asset))
  r1_assert(all(unlist(audit_split_dates(fresh,split,asset)[c(
    "TrainCalibrationNoOverlap","CalibrationTestNoOverlap")])),paste("Cached overlap",asset))
  lb <- z$tuning$leaderboard
  grid <- depth_configuration_grid()
  r1_assert(is.data.frame(lb) && nrow(lb)==nrow(grid) &&
              setequal(lb$ConfigID,grid$ConfigID) && all(lb$Asset==asset) &&
              all(lb$QRFailures==0),paste("Wrong or incomplete tuning leaderboard",asset))
  at <- match(lb$ConfigID,grid$ConfigID)
  r1_assert(all(lb$Gamma==grid$Gamma[at] & lb$WMin==grid$WMin[at] & lb$WMax==grid$WMax[at]),
            paste("Tuning identifiers and parameter values disagree",asset))
  inner <- make_internal_split(split$train,CFG$H,CFG$internal_validation_fraction)
  r1_assert(identical(inner,z$tuning$internal_split),paste("Wrong inner split",asset))
  criterion <- ifelse(is.finite(lb$WIS),lb$WIS,lb$MeanPinball)
  r1_assert(all(is.finite(criterion)) && sum(lb$Selected)==1L &&
              which(lb$Selected)==which.min(criterion),paste("Invalid training-only selection",asset))
  r1_assert(identical(as.character(z$tuning$selected$ConfigID),
                      as.character(lb$ConfigID[which.min(criterion)])),
            paste("Cached selected row differs from the leaderboard",asset))
  r1_assert(r1_close(as.numeric(unlist(z$tuning$selected[1L,c("Gamma","WMin","WMax","WIS")],use.names=FALSE)),
                    as.numeric(unlist(lb[which.min(criterion),c("Gamma","WMin","WMax","WIS")],use.names=FALSE)),1e-12),
            paste("Selected parameter values differ from the winning validation row",asset))
  for (nm in c("model_unweighted","model_selected")) {
    model <- z$bundle[[nm]]
    r1_assert(identical(as.numeric(model$taus),as.numeric(CFG$taus)) &&
                model$H==CFG$H && !length(model$failures),paste("Invalid cached QR model",asset,nm))
  }
  Qs <- z$bundle$q_test_selected_shape; Qu <- z$bundle$q_test_unweighted_shape
  dims <- c(length(split$test),CFG$H,length(CFG$taus))
  r1_assert(identical(dim(Qs),as.integer(dims)) && identical(dim(Qu),as.integer(dims)),
            paste("Wrong cached prediction dimensions",asset))
  for (part in c("cal","test")) {
    rawu <- predict_quantile_surface(z$bundle$model_unweighted,fresh$X[split[[part]],,drop=FALSE])
    raww <- predict_quantile_surface(z$bundle$model_selected,fresh$X[split[[part]],,drop=FALSE])
    u <- strict_project_array(rawu,context=paste(asset,"cache",part,"unweighted"))
    w <- strict_project_array(raww,context=paste(asset,"cache",part,"weighted"))
    r1_assert(r1_close(u,z$bundle[[paste0("q_",part,"_unweighted_shape")]],1e-7) &&
                r1_close(w,z$bundle[[paste0("q_",part,"_selected_shape")]],1e-7),
              paste("Cached predictions do not match cached coefficients",asset,part))
  }
  for (kind in c("bands","upper_bands")) for (method in c("Selected 2I-FQR","QR + exact 2I+")) {
    band <- z[[kind]]$bands[[method]]
    r1_assert(!is.null(band) && band$block_length==CFG$primary_block_length &&
                band$block_reps==CFG$calibration_reps,
              paste("Cached calibration settings differ; recompute the baseline",asset,method))
  }
  z$prices <- p
  z$r1_data_md5 <- r1_md5_object(p)
  z
}
r1_validate_empirical_cache <- function(empirical) {
  r1_assert(identical(names(empirical),FIXED_PANEL),
            "Empirical cache must contain PAVE, KO, BTC-USD, PPLT, GLD in this order.")
  for (asset in FIXED_PANEL) {
    empirical[[asset]] <- r1_validate_one(empirical[[asset]],asset)
    # Materialize the exact cached data into this run's frozen data directory.
    target <- file.path(DIR$raw,paste0(safe_name(asset),"_adjusted.csv"))
    if (file.exists(target)) {
      current <- read.csv(target,stringsAsFactors=FALSE); current$Date <- as.Date(current$Date)
      r1_assert(r1_close(as.numeric(current$Adjusted),empirical[[asset]]$prices$Adjusted,1e-12) &&
                  identical(current$Date,empirical[[asset]]$prices$Date),
                paste("Current raw cache differs from reused object",asset))
    } else write_csv_safe(empirical[[asset]]$prices,target)
  }
  empirical
}
r1_simulation_design <- function() {
  list(scenarios=simulation_scenarios(),seed=CFG$seed,
    repetitions=CFG$simulation_reps,bootstrap=CFG$simulation_block_boot_reps,
    guard=CFG$block_bootstrap_guard_probability,oracle_repetitions=CFG$oracle_diagnostic_reps,
    oracle_origins=CFG$oracle_origins,oracle_draws=CFG$oracle_draws,
    qr_method=CFG$qr_method,projection_tolerance=CFG$projection_tol,
    shape_audit_tolerance=CFG$surface_audit_tolerance)
}
r1_validate_simulation_cache <- function(z) {
  r1_assert(is.list(z) && is.data.frame(z$replication_level),"Malformed simulation cache.")
  d <- z$replication_level
  scenarios <- vapply(simulation_scenarios(),`[[`,character(1L),"name")
  r1_assert(setequal(unique(d$Scenario),scenarios) && all(d$Status=="ok"),
            "Simulation cache has missing scenarios or failed tasks.")
  expected <- expand.grid(Scenario=scenarios,Replication=seq_len(CFG$simulation_reps),
                          Method=c("QR raw","QR + exact 2I+","Depth QR + exact 2I+"),
                          stringsAsFactors=FALSE)
  key <- function(a) paste(a$Scenario,a$Replication,a$Method,sep="|")
  r1_assert(nrow(d)==nrow(expected) && !anyDuplicated(key(d)) &&
              setequal(key(d),key(expected)),"Simulation cache replication grid differs.")
  needed <- c("summary","tail_summary","band_claim_diagnostic","shape_audit",
              "weighting_comparison","weighting_comparison_summary")
  r1_assert(all(needed %in% names(z)),"Simulation cache lacks diagnostic summaries.")
  contract <- attr(z,"r1_simulation_design")
  if (!is.null(contract)) {
    r1_assert(identical(contract,r1_simulation_design()),"Cached simulation design differs from this run.")
  } else {
    configuration <- file.path(dirname(dirname(CFG$existing_simulation_rds)),"logs","configuration.csv")
    r1_assert(file.exists(configuration),paste("Legacy simulation cache requires original logs/configuration.csv:",configuration))
    old <- read.csv(configuration,stringsAsFactors=FALSE)
    fields <- c("seed","simulation_reps","simulation_block_boot_reps",
                "block_bootstrap_guard_probability","oracle_diagnostic_reps","oracle_origins","oracle_draws")
    for (field in fields) {
      value <- old$Value[old$Parameter==field]
      r1_assert(length(value)==1L && is.finite(as.numeric(value)) &&
                  abs(as.numeric(value)-as.numeric(CFG[[field]]))<1e-12,
                paste("Legacy simulation configuration mismatch:",field))
    }
  }
  z
}

# Automatic per-asset checkpoints include data and numerical design fingerprints.
r1_original_run_empirical_asset <- run_empirical_asset
run_empirical_asset <- function(asset) {
  p <- download_adjusted_prices(asset,CFG$start_date,CFG$end_date,CFG$refresh_data)
  signature <- r1_md5_object(list(r1_core_design(),p))
  path <- file.path(DIR$objects,paste0("empirical_",safe_name(asset),".rds"))
  if (CFG$resume && !CFG$refresh_data && file.exists(path)) {
    z <- readRDS(path)
    if (identical(z$r1_baseline_signature,signature)) {
      log_msg("Resuming audited baseline checkpoint for ",asset)
      return(r1_validate_one(z,asset))
    }
  }
  z <- r1_original_run_empirical_asset(asset,prices=p)
  z$r1_baseline_signature <- signature
  r1_save_rds(z,path)
  z
}

# Each simulation replication is saved independently and resumes with the same
# prespecified seed. Failed replications are not replaced or selectively dropped.
r1_original_simulation_replication <- run_simulation_replication
run_simulation_replication <- function(scenario,replication) {
  task_dir <- file.path(DIR$objects,"simulation_tasks")
  dir.create(task_dir,recursive=TRUE,showWarnings=FALSE)
  path <- file.path(task_dir,sprintf("%s_%05d.rds",safe_name(scenario$name),replication))
  signature <- r1_md5_object(list(scenario,replication,r1_simulation_design()))
  if (CFG$resume && file.exists(path)) {
    old <- readRDS(path)
    if (identical(old$signature,signature) && is.data.frame(old$result) &&
        nrow(old$result)==3L && all(old$result$Status=="ok")) return(old$result)
  }
  result <- r1_original_simulation_replication(scenario,replication)
  if (is.data.frame(result) && nrow(result)==3L && all(result$Status=="ok")) {
    r1_save_rds(list(signature=signature,result=result),path)
  }
  result
}

r1_horizon_analysis <- function(z) {
  Y <- z$dataset$Y[z$split$test,,drop=FALSE]
  u <- z$bundle$q_test_unweighted_shape; w <- z$bundle$q_test_selected_shape
  n <- nrow(Y); H <- ncol(Y); K <- length(CFG$taus)
  lu <- lw <- matrix(0,n,H)
  tails <- vector("list",K)
  for (k in seq_len(K)) {
    a <- pinball_loss(Y,u[,,k],CFG$taus[k]); b <- pinball_loss(Y,w[,,k],CFG$taus[k])
    lu <- lu+a/K; lw <- lw+b/K
    tails[[k]] <- data.frame(Asset=z$asset,Horizon=seq_len(H),Tau=CFG$taus[k],
                              UnweightedLoss=colMeans(a),SelectedLoss=colMeans(b),
                              MeanGain=colMeans(a-b))
  }
  gain <- lu-lw
  ids <- r1_block_indices(n,CFG$H,CFG$block_boot_reps,
                         stable_seed_from_key("r1-horizon",z$asset))
  draws <- vapply(seq_len(ncol(ids)),function(b) colMeans(gain[ids[,b],,drop=FALSE]),numeric(H))
  lo <- apply(draws,1L,quantile_type8,probs=.025)
  hi <- apply(draws,1L,quantile_type8,probs=.975)
  summary <- data.frame(Asset=z$asset,Horizon=seq_len(H),TestOrigins=n,
                        UnweightedLoss=colMeans(lu),SelectedLoss=colMeans(lw),
                        MeanGain=colMeans(gain),Lower95=lo,Upper95=hi,
                        GainPct=100*colMeans(gain)/colMeans(lu),
                        IntervalType="Pointwise paired moving-block percentile interval")
  origin <- data.frame(Asset=z$asset,
                       OriginDate=rep(as.character(z$dataset$origin_date[z$split$test]),H),
                       Horizon=rep(seq_len(H),each=n),UnweightedLoss=as.vector(lu),
                       SelectedLoss=as.vector(lw),Gain=as.vector(gain))
  expected <- mean_pinball_score(Y,u,CFG$taus)-mean_pinball_score(Y,w,CFG$taus)
  discrepancy <- abs(mean(summary$MeanGain)-expected)
  r1_assert(discrepancy<1e-12,paste("Horizon aggregate identity failed",z$asset))
  list(summary=summary,origin=origin,by_quantile=rbind_fill(tails),aggregate_error=discrepancy)
}
r1_projection_decomposition <- function(z) {
  rows <- list(); j <- 0L
  for (part in c("cal","test")) for (method in c("Unweighted","Depth weighted")) {
    weighted <- method=="Depth weighted"
    model <- z$bundle[[if (weighted) "model_selected" else "model_unweighted"]]
    raw <- predict_quantile_surface(model,z$dataset$X[z$split[[part]],,drop=FALSE])
    shaped <- z$bundle[[paste0("q_",part,if (weighted) "_selected_shape" else "_unweighted_shape")]]
    Y <- z$dataset$Y[z$split[[part]],,drop=FALSE]
    sr <- score_surface(Y,raw,CFG$taus); sp <- score_surface(Y,shaped,CFG$taus)
    scale <- training_horizon_scale(z$dataset$Y[z$split$train,,drop=FALSE])
    delta <- sweep(shaped-raw,2L,scale,"/")
    tol <- 1e-10*max(1,max(abs(raw)))
    ar <- surface_audit(raw); ap <- surface_audit(shaped)
    j <- j+1L
    rows[[j]] <- data.frame(Asset=z$asset,Partition=part,Weighting=method,
      RawPinball=sr$MeanPinball,ProjectedPinball=sp$MeanPinball,
      ProjectionPinballGain=sr$MeanPinball-sp$MeanPinball,
      ProjectionGainPct=100*(sr$MeanPinball-sp$MeanPinball)/sr$MeanPinball,
      ChangedElementShare=mean(abs(shaped-raw)>tol),
      MaximumAbsoluteCorrection=max(abs(shaped-raw)),
      ScaledRMSECorrection=sqrt(mean(delta^2)),
      RawMinimum=ar$MinimumPrediction,RawHorizonViolation=ar$MaximumHorizonViolation,
      RawQuantileViolation=ar$MaximumQuantileViolation,
      ProjectedMinimum=ap$MinimumPrediction,ProjectedHorizonViolation=ap$MaximumHorizonViolation,
      ProjectedQuantileViolation=ap$MaximumQuantileViolation,Passed=ap$Passed)
  }
  rbind_fill(rows)
}
r1_gamma_profile <- function(z) {
  lb <- z$tuning$leaderboard
  best <- min(lb$WIS)
  r1_assert(is.finite(best) && best>0,"Validation WIS is nonpositive or undefined.")
  rows <- lapply(CFG$gamma_grid,function(g) {
    d <- lb[abs(lb$Gamma-g)<1e-12,,drop=FALSE]; d <- d[order(d$WIS,d$WMin),,drop=FALSE]
    r1_assert(nrow(d)>0,"A gamma candidate is missing.")
    data.frame(Asset=z$asset,Gamma=g,ProfiledWMin=d$WMin[1L],WMax=d$WMax[1L],
      ValidationWIS=d$WIS[1L],ValidationPinball=d$MeanPinball[1L],
      ValidationRegretPct=100*(d$WIS[1L]-best)/best,
      EquivalentWMinCount=sum(abs(d$WIS-d$WIS[1L])<=max(1e-14,best*1e-10)),
      SelectedGamma=abs(g-z$tuning$selected$Gamma[1L])<1e-12)
  })
  profile <- rbind_fill(rows)
  other <- profile$ValidationWIS[!profile$SelectedGamma]
  profile$BestDifferentGammaGapPct <- 100*(min(other)-best)/best
  profile
}
r1_depth_diagnostics <- function(z) {
  tr <- z$split$train; Y <- z$dataset$Y[tr,,drop=FALSE]
  s <- z$tuning$selected
  info <- make_depth_weights(Y,s$Gamma[1L],s$WMin[1L],s$WMax[1L])
  terminal_rank <- (rank(Y[,ncol(Y)],ties.method="average")-.5)/nrow(Y)
  increments <- cbind(Y[,1L],Y[,-1L,drop=FALSE]-Y[,-ncol(Y),drop=FALSE])
  fraction <- increments/pmax(rowSums(increments),.Machine$double.eps)
  details <- data.frame(Asset=z$asset,OriginDate=as.character(z$dataset$origin_date[tr]),
    Depth=info$depth,Weight=info$weights,TerminalRank=terminal_rank,
    TerminalTarget=Y[,ncol(Y)],MaximumIncrementShare=apply(fraction,1L,max),
    IncrementConcentration=rowSums(fraction^2))
  summary <- data.frame(Asset=z$asset,TrainingOrigins=length(tr),
    EffectiveTrainingN=sum(info$weights)^2/sum(info$weights^2),
    LowerTerminalDecileObservationShare=mean(terminal_rank<=.1),
    UpperTerminalDecileObservationShare=mean(terminal_rank>=.9),
    LowerTerminalDecileWeightShare=sum(info$weights[terminal_rank<=.1])/sum(info$weights),
    UpperTerminalDecileWeightShare=sum(info$weights[terminal_rank>=.9])/sum(info$weights),
    DepthTerminalRankSpearman=if (stats::sd(info$depth)>0) suppressWarnings(
      stats::cor(info$depth,terminal_rank,method="spearman")) else NA_real_)
  clipping <- rbind_fill(list(
    cbind(data.frame(Asset=z$asset,FitSample="Internal fitting"),
          z$tuning$selected[,c("Gamma","WMin","WMax","LowerClipShare","UpperClipShare")]),
    cbind(data.frame(Asset=z$asset,FitSample="Full outer training"),info$diagnostics)))
  list(details=details,summary=summary,clipping=clipping)
}

# ---- Native conditional Student-t GARCH(1,1) ----------------------------------
# Returns are multiplied by 100 during likelihood optimization. Forecast targets
# are divided by 100^2. Parameters are fitted once through the last training
# target-end date, shared by all models as the information cutoff.
r1_garch_unpack <- function(theta) {
  persistence <- .9999*stats::plogis(theta[2L])
  alpha <- persistence*stats::plogis(theta[3L])
  c(omega=exp(theta[1L]),alpha=alpha,beta=persistence-alpha,nu=2+exp(theta[4L]))
}
r1_garch_variance <- compiler::cmpfun(function(x,par,initial) {
  n <- length(x); v <- numeric(n); v[1L] <- initial
  if (n>1L) for (i in 2L:n) {
    v[i] <- par["omega"]+par["alpha"]*x[i-1L]^2+par["beta"]*v[i-1L]
  }
  v
})
r1_garch_nll <- function(theta,x,initial) {
  par <- r1_garch_unpack(theta)
  v <- r1_garch_variance(x,par,initial)
  if (any(!is.finite(v)) || any(v<=0)) return(1e100)
  sd <- sqrt(v*(par["nu"]-2)/par["nu"])
  value <- -sum(stats::dt(x/sd,df=par["nu"],log=TRUE)-log(sd))
  if (is.finite(value)) value else 1e100
}
r1_fit_garch <- function(returns) {
  x <- as.numeric(returns)*CFG$garch_return_scale
  r1_assert(length(x)>=80L && all(is.finite(x)) && mean(x^2)>0,
            "GARCH requires at least 80 finite, nonconstant training returns.")
  initial <- mean(x^2)
  starts <- rbind(c(.05,.90,8),c(.10,.85,5),c(.03,.95,12),c(.15,.80,4))
  lower <- c(log(max(initial*1e-10,1e-15)),-12,-12,log(.05))
  upper <- c(log(initial*100+1),12,12,log(198))
  fits <- vector("list",nrow(starts)); rows <- vector("list",nrow(starts))
  for (j in seq_len(nrow(starts))) {
    a <- starts[j,1L]; b <- starts[j,2L]; nu <- starts[j,3L]
    theta <- c(log(initial*(1-a-b)),stats::qlogis((a+b)/.9999),
               stats::qlogis(a/(a+b)),log(nu-2))
    fit <- tryCatch(stats::optim(theta,r1_garch_nll,x=x,initial=initial,
      method="L-BFGS-B",lower=lower,upper=upper,
      control=list(maxit=CFG$garch_maxit,factr=1e7)),error=function(e)NULL)
    ok <- !is.null(fit) && fit$convergence==0L && is.finite(fit$value) && fit$value<1e90
    fits[[j]] <- fit
    rows[[j]] <- data.frame(Start=j,Convergence=if (is.null(fit)) -999L else fit$convergence,
      NLL=if (is.null(fit)) NA_real_ else fit$value,Accepted=ok,
      Message=if (is.null(fit)) "Optimizer error" else paste(fit$message,collapse=" "))
  }
  diagnostic <- rbind_fill(rows)
  valid <- which(diagnostic$Accepted)
  r1_assert(length(valid)>0L,"No Student-t GARCH likelihood optimization converged.")
  winner <- valid[which.min(diagnostic$NLL[valid])]
  best <- fits[[winner]]; par <- r1_garch_unpack(best$par)
  r1_assert(all(is.finite(par)) && par["omega"]>0 && par["alpha"]>=0 && par["beta"]>=0 &&
              par["alpha"]+par["beta"]<1 && par["nu"]>2,
            "Invalid fitted GARCH parameter constraints.")
  diagnostic$Selected <- diagnostic$Start==winner
  list(par=par,theta=best$par,initial_variance=initial,return_scale=CFG$garch_return_scale,
       training_n=length(x),NLL=best$value,convergence=best$convergence,
       fit_diagnostic=diagnostic,mean=0,backend="base R conditional Student-t MLE")
}
r1_filter_garch <- function(fit,returns) {
  x <- as.numeric(returns)*fit$return_scale
  r1_assert(all(is.finite(x)),"Nonfinite input in causal GARCH filter.")
  list(returns=x,variance=r1_garch_variance(x,fit$par,fit$initial_variance))
}
r1_garch_quantiles <- function(fit,last_return,last_variance,H,taus,draw_grid,seed) {
  draw_grid <- sort(unique(as.integer(draw_grid))); M <- max(draw_grid)
  r1_assert(M>=100L && H>=1L && is.finite(last_variance) && last_variance>0,
            "Invalid GARCH prediction request.")
  with_seed(seed,{
    output <- array(NA_real_,c(H,length(taus),length(draw_grid)))
    cumulative <- rep(0,M)
    v <- rep(fit$par["omega"]+fit$par["alpha"]*last_return^2+
               fit$par["beta"]*last_variance,M)
    for (h in seq_len(H)) {
      innovation <- stats::rt(M,df=fit$par["nu"])*sqrt((fit$par["nu"]-2)/fit$par["nu"])
      simulated_return <- sqrt(v)*innovation
      cumulative <- cumulative+(simulated_return/fit$return_scale)^2
      for (j in seq_along(draw_grid)) {
        output[h,,j] <- quantile_type8(cumulative[seq_len(draw_grid[j])],taus)
      }
      v <- fit$par["omega"]+fit$par["alpha"]*simulated_return^2+fit$par["beta"]*v
    }
    r1_assert(all(is.finite(output)),"Nonfinite conditional GARCH path quantiles.")
    output
  })
}
r1_garch_h1_quantile <- function(fit,last_return,last_variance,taus) {
  v <- fit$par["omega"]+fit$par["alpha"]*last_return^2+fit$par["beta"]*last_variance
  v*((fit$par["nu"]-2)/fit$par["nu"])*
    stats::qt((1+taus)/2,df=fit$par["nu"])^2/fit$return_scale^2
}

# ---- Calibration diagnostics using fixed forecast surfaces --------------------
r1_band_context <- function(z,method,band_type,Qcal=NULL,Qtest=NULL) {
  if (is.null(Qcal)) {
    key <- if (method=="Selected 2I-FQR") "selected" else "unweighted"
    Qcal <- z$bundle[[paste0("q_cal_",key,"_shape")]]
    Qtest <- z$bundle[[paste0("q_test_",key,"_shape")]]
  }
  Ycal <- z$dataset$Y[z$split$cal,,drop=FALSE]
  scale <- training_horizon_scale(z$dataset$Y[z$split$train,,drop=FALSE])
  alpha <- if (band_type=="two-sided") CFG$primary_band_alpha else CFG$primary_upper_band_alpha
  low <- which.min(abs(CFG$taus-alpha/2)); upper <- which.min(abs(CFG$taus-(1-alpha/2)))
  if (band_type=="upper") upper <- which.min(abs(CFG$taus-(1-alpha)))
  score <- vapply(seq_len(nrow(Ycal)),function(i) {
    a <- (Ycal[i,]-Qcal[i,,upper])/scale
    if (band_type=="two-sided") a <- c(a,(Qcal[i,,low]-Ycal[i,])/scale)
    max(a)
  },numeric(1L))
  list(asset=z$asset,method=method,type=band_type,alpha=alpha,scale=scale,
       score=score,Qtest=Qtest,low=low,upper=upper,
       Ytest=z$dataset$Y[z$split$test,,drop=FALSE])
}
r1_band_from_calibration <- function(ctx,cal,L,B) {
  n <- dim(ctx$Qtest)[1L]; H <- dim(ctx$Qtest)[2L]
  upper <- matrix(NA_real_,n,H); lower <- if (ctx$type=="two-sided") matrix(NA_real_,n,H) else NULL
  for (i in seq_len(n)) {
    upper[i,] <- pmax(cummax(ctx$Qtest[i,,ctx$upper]+cal$qhat*ctx$scale),0)
    if (ctx$type=="two-sided") {
      raw <- ctx$Qtest[i,,ctx$low]-cal$qhat*ctx$scale
      lower[i,] <- pmax(rev(cummin(rev(raw))),0)
      upper[i,] <- pmax(upper[i,],lower[i,])
    }
  }
  list(lower=lower,upper=upper,qhat=cal$qhat,direct_qhat=cal$direct_quantile,
       bootstrap_median_qhat=cal$bootstrap_median,
       bootstrap_guard_qhat=cal$bootstrap_guard_quantile,
       calibration_guard_probability=cal$guard_probability,calibration_rule=cal$rule,
       calibration_scores=ctx$score,scale_h=ctx$scale,alpha=ctx$alpha,
       endpoint_tau=CFG$taus[ctx$upper],block_length=L,block_reps=B)
}
r1_context_metrics <- function(ctx,cal,L,B) {
  band <- r1_band_from_calibration(ctx,cal,L,B)
  metric <- if (ctx$type=="two-sided") band_metrics(ctx$Ytest,band) else upper_band_metrics(ctx$Ytest,band)
  cbind(data.frame(Asset=ctx$asset,Method=ctx$method,BandType=ctx$type,
                   BootstrapReplications=B,EffectiveBootstrapReplications=if (L==1L) 0L else B,
                   stringsAsFactors=FALSE),metric)
}
r1_bootstrap_score_quantiles <- function(scores,alpha,L,B,seed) {
  r1_assert(L>1L,"Direct L=1 calibration has no bootstrap guard.")
  ids <- r1_block_indices(length(scores),L,B,seed)
  rank <- min(length(scores),ceiling((length(scores)+1)*(1-alpha)))
  vapply(seq_len(ncol(ids)),function(b) {
    sort(scores[ids[,b]],partial=rank)[rank]
  },numeric(1L))
}
r1_calibration_diagnostics <- function(z) {
  block_rows <- mc_rows <- list(); i <- j <- 0L
  for (method in c("Selected 2I-FQR","QR + exact 2I+")) for (type in c("two-sided","upper")) {
    ctx <- r1_band_context(z,method,type)
    for (L in CFG$block_length_grid) {
      seed <- empirical_band_seed(z$asset,method,if (type=="upper") "one-sided-upper" else "two-sided",L)
      cal <- moving_block_calibration_quantile(ctx$score,ctx$alpha,L,CFG$calibration_reps,seed)
      i <- i+1L
      block_rows[[i]] <- r1_context_metrics(ctx,cal,L,CFG$calibration_reps)
    }
    direct <- finite_sample_upper_quantile(ctx$score,ctx$alpha)
    probability <- max(CFG$block_bootstrap_guard_probability,1-ctx$alpha)
    for (s in seq_len(CFG$calibration_mc_seeds)) {
      # Method is deliberately absent from this seed key: identical block draws
      # compare the two methods without adding independent resampling noise.
      qboot <- r1_bootstrap_score_quantiles(ctx$score,ctx$alpha,CFG$primary_block_length,
        max(CFG$calibration_mc_grid),stable_seed_from_key("r1-calibration-mc",z$asset,type,s))
      for (B in CFG$calibration_mc_grid) {
        prefix <- qboot[seq_len(B)]
        guard <- finite_quantile(prefix,probability)
        cal <- list(qhat=max(0,direct,guard),direct_quantile=direct,
          bootstrap_median=stats::median(prefix),bootstrap_guard_quantile=guard,
          guard_probability=probability,rule="Upper-guarded moving-block quantile; nested Monte Carlo prefix")
        j <- j+1L
        mc_rows[[j]] <- cbind(data.frame(SeedIndex=s),
          r1_context_metrics(ctx,cal,CFG$primary_block_length,B))
      }
    }
  }
  block <- rbind_fill(block_rows); mc <- rbind_fill(mc_rows)
  key <- paste(mc$Method,mc$BandType,mc$SeedIndex)
  reference <- mc[mc$BootstrapReplications==max(CFG$calibration_mc_grid),,drop=FALSE]
  ref_key <- paste(reference$Method,reference$BandType,reference$SeedIndex)
  match_ref <- match(key,ref_key)
  r1_assert(!anyNA(match_ref),"Missing Monte Carlo reference rows.")
  mc$ReferenceReplications <- max(CFG$calibration_mc_grid)
  mc$ReferenceCalibrationQuantile <- reference$CalibrationQuantile[match_ref]
  mc$QuantileDifferenceFromReference <- mc$CalibrationQuantile-mc$ReferenceCalibrationQuantile
  mc$RelativeQuantileDifference <- mc$QuantileDifferenceFromReference/
    pmax(abs(mc$ReferenceCalibrationQuantile),1e-12)
  groups <- split(seq_len(nrow(mc)),paste(mc$Method,mc$BandType,mc$BootstrapReplications))
  stability <- rbind_fill(lapply(groups,function(ix) {
    d <- mc[ix,,drop=FALSE]
    data.frame(Asset=z$asset,Method=d$Method[1L],BandType=d$BandType[1L],
      BootstrapReplications=d$BootstrapReplications[1L],Seeds=nrow(d),
      MeanQhat=mean(d$CalibrationQuantile),SDQhat=stats::sd(d$CalibrationQuantile),
      MinQhat=min(d$CalibrationQuantile),MaxQhat=max(d$CalibrationQuantile),
      MeanAbsoluteRelativeDifference=mean(abs(d$RelativeQuantileDifference)),
      MaximumAbsoluteRelativeDifference=max(abs(d$RelativeQuantileDifference)))
  }))
  list(block=block,mc=mc,stability=stability)
}

# ---- Fit and evaluate GARCH at precisely the existing origins -----------------
r1_garch_benchmark <- function(z) {
  signature <- r1_md5_object(list(z$r1_data_md5,r1_core_design(),
    CFG[c("garch_draws","garch_mc_draws","garch_mc_origins","garch_maxit",
          "garch_return_scale","garch_mean")],body(r1_garch_quantiles),body(r1_fit_garch)))
  cache <- file.path(DIR$objects,paste0("garch_t_",safe_name(z$asset),".rds"))
  if (CFG$resume && file.exists(cache)) {
    old <- readRDS(cache)
    if (identical(old$signature,signature)) {
      log_msg("Resuming conditional GARCH benchmark for ",z$asset)
      r1_assert(isTRUE(surface_audit(old$Qcal)$Passed) && isTRUE(surface_audit(old$Qtest)$Passed),
                paste("Invalid stored GARCH forecast surface",z$asset))
      return(old)
    }
  }
  d <- z$dataset
  valid <- which(is.finite(d$returns))
  r1_assert(identical(valid,seq.int(min(valid),max(valid))),"Interior missing returns in GARCH input.")
  fit_end <- max(d$origin_pos[z$split$train])+d$H
  origin_idx <- c(z$split$cal,z$split$test)
  r1_assert(fit_end<min(d$origin_pos[z$split$cal]),"GARCH fit cutoff overlaps calibration origins.")
  training <- d$returns[valid[valid<=fit_end]]
  log_msg("Fitting fixed-parameter Student-t GARCH for ",z$asset,
          " through ",as.character(d$dates[fit_end]))
  fit <- r1_fit_garch(training)
  last_observed <- max(d$origin_pos[origin_idx])
  filter_pos <- valid[valid<=last_observed]
  state <- r1_filter_garch(fit,d$returns[filter_pos])
  map <- match(d$origin_pos[origin_idx],filter_pos)
  r1_assert(!anyNA(map),"A GARCH forecast origin is absent from the causal filter.")
  Q <- array(NA_real_,c(length(origin_idx),d$H,length(CFG$taus)))
  timing <- vector("list",length(origin_idx))
  checkpoint <- file.path(DIR$objects,paste0("garch_progress_",safe_name(z$asset),".rds"))
  completed <- 0L
  if (CFG$resume && file.exists(checkpoint)) {
    old <- readRDS(checkpoint)
    if (identical(old$signature,signature) && identical(dim(old$Q),dim(Q))) {
      Q <- old$Q; completed <- old$completed
      log_msg("Resuming GARCH origins for ",z$asset," after ",completed," forecasts")
    }
  }
  if (completed<length(origin_idx)) for (i in seq.int(completed+1L,length(origin_idx))) {
    prediction <- r1_garch_quantiles(fit,state$returns[map[i]],state$variance[map[i]],
      d$H,CFG$taus,CFG$garch_draws,stable_seed_from_key("garch-main",z$asset,d$origin_pos[origin_idx[i]]))
    Q[i,,] <- prediction[,,1L]
    if (i%%50L==0L || i==length(origin_idx)) {
      r1_save_rds(list(signature=signature,Q=Q,completed=i),checkpoint)
      log_msg(z$asset,": conditional GARCH forecasts ",i,"/",length(origin_idx))
    }
  }
  timing <- data.frame(Asset=z$asset,Partition=c(rep("calibration",length(z$split$cal)),
                                                rep("test",length(z$split$test))),
    OriginDate=as.character(d$origin_date[origin_idx]),
    LastObservedReturnDate=as.character(d$dates[d$origin_pos[origin_idx]]),
    FitEndDate=as.character(d$dates[fit_end]),Draws=CFG$garch_draws,
    TrainingParametersFixed=TRUE)
  audit <- surface_audit(Q)
  r1_assert(isTRUE(audit$Passed),paste("GARCH simulated quantiles violate coherence",z$asset))
  ncal <- length(z$split$cal)
  Qcal <- Q[seq_len(ncal),,,drop=FALSE]
  Qtest <- Q[ncal+seq_along(z$split$test),,,drop=FALSE]
  Ytest <- d$Y[z$split$test,,drop=FALSE]
  metrics <- cbind(data.frame(Asset=z$asset,Method="Student-t GARCH(1,1)"),
                   score_surface(Ytest,Qtest,CFG$taus))
  loss <- observation_pinball_loss(Ytest,Qtest,CFG$taus)
  primary_loss <- observation_pinball_loss(Ytest,z$bundle$q_test_selected_shape,CFG$taus)
  ci <- moving_block_mean_ci(loss-primary_loss,CFG$H,CFG$block_boot_reps,
                            seed=stable_seed_from_key("garch-paired",z$asset))
  inference <- data.frame(Asset=z$asset,Comparator="Student-t GARCH(1,1)",
    MeanPinballGainOfSelected=unname(ci["Mean"]),Lower95=unname(ci["Lower"]),
    Upper95=unname(ci["Upper"]),RawTwoSidedPValue=unname(ci["TwoSidedPValue"]),
    BootstrapReplications=CFG$block_boot_reps,MultiplicityFamily="Five asset-level GARCH comparisons")
  bandrows <- lapply(c("two-sided","upper"),function(type) {
    ctx <- r1_band_context(z,"Student-t GARCH(1,1)",type,Qcal,Qtest)
    cal <- moving_block_calibration_quantile(ctx$score,ctx$alpha,CFG$primary_block_length,
      CFG$calibration_reps,empirical_band_seed(z$asset,"Student-t GARCH(1,1)",
        if (type=="upper") "one-sided-upper" else "two-sided",CFG$primary_block_length))
    r1_context_metrics(ctx,cal,CFG$primary_block_length,CFG$calibration_reps)
  })
  # Draw-count convergence is diagnostic at predetermined, evenly spaced test origins.
  selected <- unique(as.integer(round(seq(1L,length(z$split$test),
                                          length.out=min(CFG$garch_mc_origins,length(z$split$test))))))
  mc <- list(); j <- 0L
  for (position in selected) {
    all_i <- ncal+position; origin <- origin_idx[all_i]
    predictions <- r1_garch_quantiles(fit,state$returns[map[all_i]],state$variance[map[all_i]],
      d$H,CFG$taus,CFG$garch_mc_draws,stable_seed_from_key("garch-mc",z$asset,d$origin_pos[origin]))
    reference <- predictions[,,length(CFG$garch_mc_draws)]
    scale <- training_horizon_scale(d$Y[z$split$train,,drop=FALSE])
    for (b in seq_along(CFG$garch_mc_draws)) {
      q <- predictions[,,b]; delta <- sweep(q-reference,1L,scale,"/")
      first_true <- r1_garch_h1_quantile(fit,state$returns[map[all_i]],state$variance[map[all_i]],CFG$taus)
      j <- j+1L
      mc[[j]] <- data.frame(Asset=z$asset,OriginDate=as.character(d$origin_date[origin]),
        Draws=CFG$garch_mc_draws[b],ReferenceDraws=max(CFG$garch_mc_draws),
        MeanAbsoluteScaledQuantileDifference=mean(abs(delta)),
        MaximumAbsoluteScaledQuantileDifference=max(abs(delta)),
        H1AnalyticMaximumScaledError=max(abs(q[1L,]-first_true))/scale[1L],
        MeanPinball=mean_pinball_score(matrix(d$Y[origin,],1L),
                                      array(q,c(1L,d$H,length(CFG$taus))),CFG$taus))
    }
  }
  # A future-data perturbation must not change states at an earlier forecast origin.
  cut <- map[ncal+1L]
  perturbed <- d$returns[filter_pos]
  if (cut<length(perturbed)) perturbed[(cut+1L):length(perturbed)] <-
    perturbed[(cut+1L):length(perturbed)]+.123
  alt <- r1_filter_garch(fit,perturbed)
  causal_error <- max(abs(state$variance[seq_len(cut)]-alt$variance[seq_len(cut)]))
  r1_assert(causal_error==0,"GARCH state is contaminated by future returns.")
  params <- data.frame(Asset=z$asset,OmegaPercentReturnUnits=unname(fit$par["omega"]),
    Alpha=unname(fit$par["alpha"]),Beta=unname(fit$par["beta"]),
    Persistence=unname(fit$par["alpha"]+fit$par["beta"]),Nu=unname(fit$par["nu"]),
    MeanReturn=0,TrainingReturns=fit$training_n,FitEndDate=as.character(d$dates[fit_end]),
    NLL=fit$NLL,Convergence=fit$convergence,FuturePerturbationStateError=causal_error,
    Backend=fit$backend,ReturnScale=fit$return_scale)
  out <- list(signature=signature,fit=fit,parameters=params,Qcal=Qcal,Qtest=Qtest,
    metrics=metrics,inference=inference,bands=rbind_fill(bandrows),mc=rbind_fill(mc),
    timing=timing,shape_audit=cbind(data.frame(Asset=z$asset,Method="Student-t GARCH(1,1)"),audit))
  r1_save_rds(out,cache); unlink(checkpoint)
  out
}

r1_write_table <- function(d,name,caption,labels=NULL,digits=8L,note=NULL) {
  write_latex_table(d,file.path(DIR$tex,paste0("tab_r1_",name,".tex")),caption,
    paste0("tab:r1-",gsub("_","-",name)),digits=digits,column_labels=labels,note=note)
}
r1_save_plot <- function(plot,name,width=10,height=8) {
  if (!is.null(plot$labels$caption)) plot <- plot+ggplot2::labs(
    caption=paste(strwrap(plot$labels$caption,width=100),collapse="\n"))
  if (CFG$data_mode=="SYNTHETIC_DIAGNOSTIC") plot <- plot+ggplot2::labs(
    subtitle="SYNTHETIC DIAGNOSTIC - NOT EMPIRICAL EVIDENCE")
  ggplot2::ggsave(file.path(DIR$figures,paste0(name,".png")),plot,
                 width=width,height=height,dpi=320,bg="white")
  ggplot2::ggsave(file.path(DIR$figures,paste0(name,".pdf")),plot,
                 width=width,height=height,device="pdf",bg="white")
}
r1_make_figures <- function(tables) {
  d <- tables$r1_horizon_loss_differences
  d$Asset <- factor(d$Asset,levels=FIXED_PANEL)
  plot <- ggplot2::ggplot(d,ggplot2::aes(Horizon,MeanGain))+
    ggplot2::geom_hline(yintercept=0,linetype="dashed",colour="grey40")+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=Lower95,ymax=Upper95),fill="#337EA5",alpha=.18)+
    ggplot2::geom_line(colour="#155A7C",linewidth=.7)+
    ggplot2::facet_wrap(~Asset,scales="free_y",ncol=2)+
    ggplot2::labs(x="Forecast horizon",y="Unweighted minus depth-weighted pinball loss",
      caption=paste("Positive values favor depth weighting under identical shape constraints.",
                    "Bands are pointwise paired block-bootstrap intervals; scales differ by asset."))+
    publication_theme()
  r1_save_plot(plot,"fig_r1_horizon_pinball_difference",height=9)
  d <- tables$r1_gamma_validation_profile
  d$Asset <- factor(d$Asset,levels=FIXED_PANEL)
  plot <- ggplot2::ggplot(d,ggplot2::aes(Gamma,ValidationRegretPct))+
    ggplot2::geom_line(colour="#155A7C")+ggplot2::geom_point(ggplot2::aes(shape=SelectedGamma),size=2.5)+
    ggplot2::facet_wrap(~Asset,scales="free_y",ncol=2)+
    ggplot2::scale_x_continuous(breaks=CFG$gamma_grid)+
    ggplot2::labs(x="Depth exponent gamma",y="Validation WIS above the minimum (%)",
      shape="Selected exponent",caption="The lower weight bound is profiled within each exponent using internal validation only.")+
    publication_theme()
  r1_save_plot(plot,"fig_r1_gamma_validation_profile",height=9)
  d <- tables$r1_distributional_benchmark_metrics
  d <- d[d$Method %in% c("Selected 2I-FQR","QR + exact 2I+","Student-t GARCH(1,1)"),]
  d$Asset <- factor(d$Asset,levels=FIXED_PANEL)
  plot <- ggplot2::ggplot(d,ggplot2::aes(Method,MeanPinball,fill=Method))+
    ggplot2::geom_col(width=.7)+ggplot2::facet_wrap(~Asset,scales="free_y",ncol=2)+
    ggplot2::labs(x=NULL,y="Test mean pinball loss",caption="Identical test origins and quantile levels; asset-specific vertical scales.")+
    publication_theme()+ggplot2::theme(axis.text.x=ggplot2::element_blank(),axis.ticks.x=ggplot2::element_blank())+
    ggplot2::guides(fill=ggplot2::guide_legend(ncol=1))
  r1_save_plot(plot,"fig_r1_distributional_benchmark",height=9)
}

run_r1_suite <- function(empirical) {
  pieces <- vector("list",length(FIXED_PANEL)); names(pieces) <- FIXED_PANEL
  for (asset in FIXED_PANEL) {
    z <- empirical[[asset]]
    log_msg("R1 diagnostics started for ",asset)
    h <- r1_horizon_analysis(z)
    projection <- r1_projection_decomposition(z)
    gamma <- r1_gamma_profile(z)
    depth <- r1_depth_diagnostics(z)
    # GARCH has its own per-origin checkpoint, useful on Windows and after interruption.
    garch <- r1_garch_benchmark(z)
    cal_signature <- r1_md5_object(list(z$r1_data_md5,r1_core_design(),
      z$bundle$q_cal_selected_shape,z$bundle$q_cal_unweighted_shape,
      CFG[c("calibration_mc_grid","calibration_mc_seeds","block_length_grid")],
      body(r1_calibration_diagnostics)))
    path <- file.path(DIR$objects,paste0("r1_calibration_",safe_name(asset),".rds"))
    previous <- if (CFG$resume && file.exists(path)) readRDS(path) else NULL
    if (!is.null(previous) && identical(previous$signature,cal_signature)) {
      calibration <- previous$result
      log_msg("Resuming calibration diagnostics for ",asset)
    } else {
      log_msg("Computing fixed-surface calibration diagnostics for ",asset)
      calibration <- r1_calibration_diagnostics(z)
      r1_save_rds(list(signature=cal_signature,result=calibration),path)
    }
    pieces[[asset]] <- list(horizon=h,projection=projection,gamma=gamma,
                            depth=depth,garch=garch,calibration=calibration)
  }
  collect <- function(f) rbind_fill(lapply(pieces,f))
  tables <- list(
    r1_horizon_loss_differences=collect(function(x)x$horizon$summary),
    r1_horizon_origin_losses=collect(function(x)x$horizon$origin),
    r1_horizon_quantile_loss_differences=collect(function(x)x$horizon$by_quantile),
    r1_projection_decomposition=collect(function(x)x$projection),
    r1_gamma_validation_profile=collect(function(x)x$gamma),
    r1_depth_training_details=collect(function(x)x$depth$details),
    r1_depth_information_summary=collect(function(x)x$depth$summary),
    r1_weight_clipping_by_fit=collect(function(x)x$depth$clipping),
    r1_block_length_sensitivity=collect(function(x)x$calibration$block),
    r1_bootstrap_replication_sensitivity=collect(function(x)x$calibration$mc),
    r1_bootstrap_numerical_stability=collect(function(x)x$calibration$stability),
    r1_garch_parameters=collect(function(x)x$garch$parameters),
    r1_garch_optimization=collect(function(x)cbind(data.frame(Asset=x$garch$parameters$Asset),
                                                  x$garch$fit$fit_diagnostic)),
    r1_garch_forecast_origins=collect(function(x)x$garch$timing),
    r1_garch_mc_stability=collect(function(x)x$garch$mc),
    r1_garch_shape_audit=collect(function(x)x$garch$shape_audit),
    r1_garch_paired_inference=collect(function(x)x$garch$inference),
    r1_distributional_benchmark_metrics=rbind_fill(list(
      rbind_fill(lapply(empirical,function(z)z$evaluation$metrics)),
      collect(function(x)x$garch$metrics))),
    r1_distributional_benchmark_bands=rbind_fill(list(
      collect(function(x)x$calibration$block[
        x$calibration$block$BlockLength==CFG$primary_block_length,,drop=FALSE]),
      collect(function(x)x$garch$bands)))
  )
  tables$r1_garch_paired_inference$HolmAdjustedPValue <- stats::p.adjust(
    tables$r1_garch_paired_inference$RawTwoSidedPValue,method="holm",n=length(FIXED_PANEL))
  for (name in names(tables)) write_csv_safe(tables[[name]],file.path(DIR$csv,paste0(name,".csv")))
  r1_write_table(tables$r1_gamma_validation_profile[,c("Asset","Gamma","ProfiledWMin",
      "ValidationWIS","ValidationRegretPct","BestDifferentGammaGapPct")],
    "gamma_validation_profile","Training-only validation profiles across the depth exponents.",
    c("Asset","$\\gamma$","Profiled lower bound","Validation WIS","Regret (\\%)","Different-$\\gamma$ gap (\\%)"),
    note="The lower bound is profiled within each exponent. These are internal validation scores, not test scores.")
  d <- tables$r1_projection_decomposition
  r1_write_table(d[d$Partition=="test",c("Asset","Weighting","RawPinball","ProjectedPinball",
                                       "ProjectionPinballGain","ChangedElementShare","ScaledRMSECorrection")],
    "projection_decomposition","Predictive and geometric effects of exact projection.",
    c("Asset","Weighting","Raw loss","Projected loss","Loss reduction","Changed share","Scaled correction"),
    digits=10L,note="Small loss changes need not imply identical forecast surfaces. A positive loss reduction favors projection.")
  d <- tables$r1_distributional_benchmark_metrics
  r1_write_table(d[d$Method %in% c("Selected 2I-FQR","QR + exact 2I+","Student-t GARCH(1,1)"),
    c("Asset","Method","MeanPinball","CRPSApprox","WIS","CalibrationMAE")],
    "distributional_benchmarks","Distributional forecasts evaluated at identical test origins.",
    c("Asset","Method","Mean pinball","CRPS approximation","WIS","Calibration MAE"),
    note="All methods use the same quantile grid. Lower scores are better. GARCH forecasts are conditional quantiles of cumulative squared returns.")
  r1_write_table(tables$r1_garch_paired_inference[,c("Asset","MeanPinballGainOfSelected",
      "Lower95","Upper95","RawTwoSidedPValue","HolmAdjustedPValue")],
    "garch_paired_inference","Paired mean pinball-loss comparison with Student-t GARCH.",
    c("Asset","Gain of selected","CI lower","CI upper","Raw two-sided $p$","Holm-adjusted $p$"),
    note="Positive gains favor Selected 2I--FQR. Holm adjustment is applied to this separate five-asset GARCH family.")
  d <- tables$r1_block_length_sensitivity
  d$PathCoverage <- ifelse(d$BandType=="two-sided",d$SimultaneousPathCoverage,d$SimultaneousUpperPathCoverage)
  d$BandSize <- ifelse(d$BandType=="two-sided",d$MeanWidth,d$MeanUpperBoundary)
  compact <- d[d$BlockLength %in% unique(c(1L,10L,CFG$primary_block_length,60L)),
               c("Asset","Method","BandType","BlockLength","CalibrationQuantile","PathCoverage","BandSize")]
  r1_write_table(compact,"block_sensitivity","Sensitivity of path calibration to block length.",
    c("Asset","Method","Band","Block length","Inflation quantile","Path coverage","Band size"),
    note="Length 1 is direct calibration without a bootstrap guard. Band size is two-sided width or the one-sided upper boundary.")
  r1_write_table(tables$r1_bootstrap_numerical_stability[,c("Asset","Method","BandType",
    "BootstrapReplications","MeanQhat","SDQhat","MaximumAbsoluteRelativeDifference")],
    "bootstrap_stability","Numerical stability of the block calibration quantile.",
    c("Asset","Method","Band","Replications","Mean quantile","Quantile SD","Max relative difference"),
    note="Differences use the largest replication count as a numerical reference, not a population truth. Seeds vary while forecasts remain fixed.")
  r1_make_figures(tables)
  checks <- list(); add <- function(name,ok,value=NA_real_,expected=NA_real_) {
    checks[[length(checks)+1L]] <<- data.frame(Test=name,Objective=value,ExpectedObjective=expected,Passed=isTRUE(ok))
  }
  for (asset in FIXED_PANEL) {
    x <- pieces[[asset]]; z <- empirical[[asset]]
    add(paste("R1 horizon identity",asset),x$horizon$aggregate_error<1e-12,x$horizon$aggregate_error,0)
    add(paste("R1 complete horizons",asset),identical(x$horizon$summary$Horizon,seq_len(CFG$H)))
    add(paste("R1 projection audit",asset),all(x$projection$Passed))
    add(paste("R1 gamma candidates",asset),setequal(x$gamma$Gamma,CFG$gamma_grid) && sum(x$gamma$SelectedGamma)==1L)
    add(paste("R1 block grid",asset),nrow(x$calibration$block)==4L*length(CFG$block_length_grid))
    direct <- x$calibration$block[x$calibration$block$BlockLength==1L,,drop=FALSE]
    add(paste("R1 direct-calibration label",asset),nrow(direct)==4L &&
        all(grepl("direct",direct$CalibrationRule,ignore.case=TRUE)))
    add(paste("R1 Monte Carlo grid",asset),nrow(x$calibration$mc)==
        4L*length(CFG$calibration_mc_grid)*CFG$calibration_mc_seeds)
    mc_ref <- x$calibration$mc[x$calibration$mc$BootstrapReplications==max(CFG$calibration_mc_grid),]
    add(paste("R1 numerical reference identity",asset),all(mc_ref$QuantileDifferenceFromReference==0))
    add(paste("R1 GARCH convergence and causality",asset),
        x$garch$parameters$Convergence==0L && x$garch$parameters$FuturePerturbationStateError==0)
    add(paste("R1 GARCH coherent quantiles",asset),all(x$garch$shape_audit$Passed))
    add(paste("R1 GARCH same calibration origins",asset),dim(x$garch$Qcal)[1L]==length(z$split$cal))
    add(paste("R1 GARCH same test origins",asset),dim(x$garch$Qtest)[1L]==length(z$split$test))
  }
  for (name in names(tables)) {
    path <- file.path(DIR$csv,paste0(name,".csv"))
    add(paste("R1 output",name),file.exists(path) && file.info(path)$size>0)
  }
  for (name in c("fig_r1_horizon_pinball_difference","fig_r1_gamma_validation_profile",
                 "fig_r1_distributional_benchmark")) for (ext in c("png","pdf")) {
    path <- file.path(DIR$figures,paste0(name,".",ext))
    add(paste("R1 figure",name,ext),file.exists(path) && file.info(path)$size>0)
  }
  audit <- rbind_fill(checks)
  write_csv_safe(audit,file.path(DIR$logs,"r1_final_audit.csv"))
  r1_assert(all(audit$Passed),paste("R1 audit failed:",paste(audit$Test[!audit$Passed],collapse="; ")))
  output <- list(tables=tables,audit=audit)
  r1_save_rds(output,file.path(DIR$objects,"r1_revision_results.rds"))
  output
}

# Tests include analytic GARCH squared-return quantiles, a future perturbation,
# response-weighting invariances, actual optimizer convergence, and RNG isolation.
r1_unit_tests <- function() {
  rows <- list(); test <- function(name,ok,value=NA_real_,expected=NA_real_) {
    rows[[length(rows)+1L]] <<- data.frame(Test=name,Objective=value,ExpectedObjective=expected,Passed=isTRUE(ok))
  }
  Y <- cbind(c(1,2,3,4,5),c(2,3,5,9,10),c(3,4,7,10,12),c(4,5,9,14,16))
  D <- fm_depth(Y)
  test("R1 marginal-rank depth can assign equal centrality to distinct paths",abs(D[2]-D[4])<1e-12)
  wd <- make_depth_weights(Y,0,.05,2)
  test("R1 gamma zero produces exact unit weights",all(wd$weights==1))
  # These rank-preserving magnifications leave depth unchanged; magnitudes are lost.
  test("R1 depth is unchanged by increasing coordinate transformations",r1_close(D,fm_depth(exp(Y/10))))
  ids <- r1_block_indices(20L,4L,5L,12L)
  test("R1 block samples use valid indices",identical(dim(ids),c(20L,5L)) && all(ids>=1L & ids<=20L))
  direct <- moving_block_calibration_quantile(seq_len(30L),.1,1L,20L,1L)
  test("R1 L1 calibration has no bootstrap guard",is.na(direct$guard_probability) &&
        grepl("direct",direct$rule,ignore.case=TRUE))
  q0 <- r1_bootstrap_score_quantiles(seq_len(30L),.1,5L,40L,18L)
  q1 <- r1_bootstrap_score_quantiles(seq_len(30L),.1,5L,80L,18L)
  test("R1 calibration prefixes are nested",identical(q0,q1[seq_along(q0)]))
  zero <- moving_block_mean_ci(rep(0,40L),5L,reps=40L,seed=2L)
  test("R1 identical primary models retain zero gain with p one",zero["Mean"]==0 &&
       zero["Lower"]==0 && zero["Upper"]==0 && zero["TwoSidedPValue"]==1)
  fixture <- list(par=c(omega=.04,alpha=.08,beta=.88,nu=7),initial_variance=1,
                  return_scale=100,training_n=300L,mean=0)
  taus <- c(.05,.5,.95)
  sim <- r1_garch_quantiles(fixture,.6,.9,5L,taus,60000L,771L)[,,1L]
  truth <- r1_garch_h1_quantile(fixture,.6,.9,taus)
  relative <- max(abs(sim[1L,]-truth)/truth)
  test("R1 GARCH H1 simulated quantiles match analytic squared-t law",relative<.12,relative,.12)
  test("R1 GARCH shared paths imply ordered nonnegative quantiles",
       min(sim)>=0 && all(apply(sim,2L,function(x)all(diff(x)>=0))) &&
       all(apply(sim,1L,function(x)all(diff(x)>=0))))
  x <- with_seed(12L,stats::rnorm(300L,sd=.01))
  first <- r1_filter_garch(fixture,x)
  altered <- x; altered[151:300] <- 20
  second <- r1_filter_garch(fixture,altered)
  test("R1 GARCH filtering excludes later returns",r1_close(first$variance[1:150],second$variance[1:150]))
  set.seed(191L); before <- .Random.seed
  invisible(r1_garch_quantiles(fixture,.6,.9,2L,taus,200L,15L))
  test("R1 GARCH simulation preserves the caller RNG state",identical(before,.Random.seed))
  synthetic <- simulate_garch_returns(450L,.08,.88,df=7,burn=150L,seed=177L)
  fit <- r1_fit_garch(synthetic$returns)
  test("R1 Student-t likelihood optimizer converges on a synthetic sample",
       fit$convergence==0L && fit$par["alpha"]+fit$par["beta"]<1 && fit$par["nu"]>2)
  out <- rbind_fill(rows)
  if (!all(out$Passed)) {
    write_csv_safe(out,file.path(DIR$logs,"r1_failed_unit_tests.csv"))
    stop("R1 unit test failed: ",paste(out$Test[!out$Passed],collapse="; "))
  }
  out
}



# ---- Main ---------------------------------------------------------------------
main <- function() {
  projection_test <- rbind(core_unit_tests(), r1_unit_tests())
  if (!all(projection_test$Passed)) stop("A core/R1 self-test failed.")
  write_csv_safe(projection_test, file.path(DIR$logs, "core_and_r1_unit_tests.csv"))
  if (RUN_MODE == "selftest") return(invisible(list(audit = projection_test)))
  log_msg("Core projection, embargo, and benchmark unit tests passed")

  empirical <- NULL
  empirical_tables <- NULL
  if (CFG$run_empirical) {
    empirical_run <- if (
      nzchar(CFG$existing_empirical_rds) &&
        file.exists(CFG$existing_empirical_rds)
    ) {
      log_msg(
        "Reusing empirical object and refreshing paired inference: ",
        CFG$existing_empirical_rds
      )
      list(
        results = refresh_empirical_paired_inference(
          r1_validate_empirical_cache(readRDS(CFG$existing_empirical_rds))
        ),
        failures = data.frame(
          Asset = character(0L), Status = character(0L),
          Error = character(0L), stringsAsFactors = FALSE
        )
      )
    } else {
      if (nzchar(CFG$existing_empirical_rds)) {
        stop(
          "TWOIFQR_EXISTING_EMPIRICAL_RDS does not exist: ",
          CFG$existing_empirical_rds
        )
      }
      run_empirical_suite(CFG$assets)
    }
    empirical <- empirical_run$results
    empirical <- r1_validate_empirical_cache(empirical)
    saveRDS(empirical, file.path(DIR$objects, "empirical_all_assets.rds"))
    empirical_tables <- export_empirical_results(
      empirical, failures = empirical_run$failures
    )
  }

  revision <- if (!is.null(empirical)) run_r1_suite(empirical) else NULL

  simulation <- NULL
  if (CFG$run_simulation) {
    simulation <- if (
      nzchar(CFG$existing_simulation_rds) &&
        file.exists(CFG$existing_simulation_rds)
    ) {
      log_msg(
        "Reusing audited simulation object: ", CFG$existing_simulation_rds
      )
      r1_validate_simulation_cache(readRDS(CFG$existing_simulation_rds))
    } else {
      if (nzchar(CFG$existing_simulation_rds)) {
        stop(
          "TWOIFQR_EXISTING_SIMULATION_RDS does not exist: ",
          CFG$existing_simulation_rds
        )
      }
      run_simulation_suite()
    }
    write_csv_safe(
      simulation$replication_level,
      file.path(DIR$csv, "simulation_replication_level.csv")
    )
    write_csv_safe(
      simulation$summary,
      file.path(DIR$csv, "simulation_summary.csv")
    )
    write_csv_safe(
      simulation$tail_summary,
      file.path(DIR$csv, "simulation_tail_summary.csv")
    )
    write_csv_safe(
      simulation$band_claim_diagnostic,
      file.path(DIR$csv, "simulation_band_claim_diagnostic.csv")
    )
    write_csv_safe(
      simulation$shape_audit,
      file.path(DIR$csv, "simulation_shape_audit.csv")
    )
    write_csv_safe(
      simulation$weighting_comparison,
      file.path(DIR$csv, "simulation_weighting_comparison_replication_level.csv")
    )
    write_csv_safe(
      simulation$weighting_comparison_summary,
      file.path(DIR$csv, "simulation_weighting_comparison_summary.csv")
    )
    compact <- compact_simulation_summary(simulation$summary)
    if (nrow(compact)) {
      simulation_performance <- compact[, c(
        "Scenario", "Method", "SuccessfulReplications",
        "MeanPinballMean", "MeanPinballMedian", "MeanPinballIQR",
        "MeanPinballQ95",
        "Coverage05Mean", "Coverage95Mean",
        "OracleQuantileBias05Mean", "OracleQuantileBias95Mean"
      ), drop = FALSE]
      write_latex_table(
        simulation_performance,
        file.path(DIR$tex, "tab_simulation_performance.tex"),
        "Simulation performance under persistence, heavy tails, contamination, and regime change.",
        "tab:simulation-performance-v2e",
        note = paste0(
          "The median, IQR, and 95th percentile accompany the mean because the ",
          "contamination design can generate highly skewed replication losses. ",
          "Oracle bias is estimated by conditional Monte Carlo for the prespecified ",
          "diagnostic replications and origins; it is not available for every replication."
        )
      )
      simulation_bands <- simulation$band_claim_diagnostic[, c(
        "Scenario", "Method", "BandType", "Replications", "TargetCoverage",
        "CoverageMean", "CoverageGap", "MeanCalibrationInflation", "MonteCarloSE",
        "MonteCarloLower95", "MonteCarloUpper95", "CoverageAssessment"
      ), drop = FALSE]
      write_latex_table(
        simulation_bands,
        file.path(DIR$tex, "tab_simulation_band_coverage.tex"),
        "Simulation coverage and Monte Carlo uncertainty for two-sided and one-sided block-calibrated bands.",
        "tab:simulation-band-coverage-v2e",
        note = paste0(
          "The two-sided simultaneous target is 90\\%; the one-sided simultaneous ",
          "upper-path target is 95\\%. Assessments compare the target with the ",
          "normal-approximation 95\\% Monte Carlo interval across replications. ",
          "They do not constitute a finite-sample validity theorem."
        )
      )
      make_simulation_figures(simulation$band_claim_diagnostic)
    }
    if (nrow(simulation$shape_audit)) {
      write_latex_table(
        simulation$shape_audit[, c(
          "Scenario", "Method", "ProjectedFits", "PassedFits", "FailedFits",
          "MinimumPrediction", "MaximumHorizonViolation",
          "MaximumQuantileViolation", "Passed"
        ), drop = FALSE],
        file.path(DIR$tex, "tab_simulation_shape_audit.tex"),
        paste0(
          "Final constraint audit for all projected simulation calibration ",
          "and test surfaces."
        ),
        "tab:simulation-shape-audit-v2e",
        note = paste0(
          "Each replication-level audit covers every horizon--quantile surface ",
          "in both calibration and test arrays. A failed audit terminates the task."
        )
      )
    }
    if (nrow(simulation$tail_summary)) {
      tail_calibration <- simulation$tail_summary[, c(
        "Scenario", "Method", "Tau", "SuccessfulReplications",
        "TailPinball", "ObservedCoverage", "CalibrationError"
      ), drop = FALSE]
      write_latex_table(
        tail_calibration,
        file.path(DIR$tex, "tab_simulation_tail_calibration.tex"),
        "Tail-specific simulation calibration for unweighted and depth-weighted shape-constrained fits.",
        "tab:simulation-tail-calibration-v2e",
        note = "Calibration error equals observed coverage minus the nominal quantile level."
      )
      tail_oracle <- simulation$tail_summary[, c(
        "Scenario", "Method", "Tau", "OracleDiagnosticReplications",
        "OracleQuantileBias", "OracleQuantileMAE", "OracleQuantileRMSE"
      ), drop = FALSE]
      write_latex_table(
        tail_oracle,
        file.path(DIR$tex, "tab_simulation_oracle_bias.tex"),
        "Conditional Monte Carlo assessment of tail-quantile bias in the simulation study.",
        "tab:simulation-oracle-bias-v2e",
        note = paste0(
          "Bias is fitted minus oracle quantile. MAE and RMSE summarize absolute ",
          "and squared deviations across the prespecified diagnostic origins."
        )
      )
    }
    if (nrow(simulation$weighting_comparison_summary)) {
      weighting_compact <- simulation$weighting_comparison_summary[, c(
        "Scenario", "Replications", "OracleDiagnosticReplications",
        "WeightedMinusUnweightedTailPinball05Mean",
        "WeightedMinusUnweightedTailPinball95Mean",
        "WeightedMinusUnweightedAbsoluteCalibrationError05Mean",
        "WeightedMinusUnweightedAbsoluteCalibrationError95Mean",
        "WeightedMinusUnweightedAbsoluteOracleBias05Mean",
        "WeightedMinusUnweightedAbsoluteOracleBias95Mean"
      ), drop = FALSE]
      write_latex_table(
        weighting_compact,
        file.path(DIR$tex, "tab_simulation_weighting_comparison.tex"),
        paste0(
          "Simulation comparison of depth-weighted and unweighted fits. ",
          "Differences are weighted minus unweighted; negative absolute-error ",
          "differences favor depth weighting."
        ),
        "tab:simulation-weighting-v2e",
        note = paste0(
          "$CE$ denotes calibration error and $OB$ oracle bias. All changes are ",
          "depth-weighted minus unweighted; negative absolute-error changes favor weighting."
        )
      )
    }
    attr(simulation, "r1_simulation_design") <- r1_simulation_design()
    saveRDS(simulation, file.path(DIR$objects, "simulation_results.rds"))
  }

  audit <- run_final_audit(projection_test, empirical_tables, simulation)
  if (!is.null(revision)) {
    audit <- rbind_fill(list(audit, revision$audit))
    write_csv_safe(audit, file.path(DIR$logs, "final_audit.csv"))
  }
  write_reproducibility_files(empirical, simulation)
  writeLines(c(paste("Run mode:", RUN_MODE),
               paste("Synthetic diagnostic:", RUN_MODE == "smoke"),
               paste("Audit rows:", nrow(audit)),
               paste("All computational checks passed:", all(audit$Passed)),
               "Passing checks are not evidence of predictive superiority or coverage validity."),
             file.path(DIR$root, "RUN_COMPLETED.txt"))
  log_msg("All audits passed. Outputs are available under ", DIR$root)
  invisible(list(empirical = empirical, revision = revision, simulation = simulation, audit = audit))
}

# Export only pipeline definitions, never unrelated RStudio objects.
R1_WORKER_EXPORTS <- c(
  "as_flag", "r1_env_int", "r1_env_grid", "log_msg",
  "safe_name", "write_csv_safe", "rbind_fill", "latex_escape",
  "academic_column_labels", "write_latex_table", "quantile_type8", "finite_mean",
  "finite_sd", "finite_quantile", "finite_median", "finite_iqr",
  "with_seed", "stable_seed_from_key", "empirical_band_seed", "download_adjusted_prices",
  "rolling_mean_right", "build_curve_dataset_from_returns", "build_curve_dataset_from_prices", "make_embargo_split",
  "make_internal_split", "audit_split_dates", "pava_equal", "project_horizon_set",
  "project_quantile_set", "project_nonnegative_set", "dykstra_project_surface", "project_array_exact",
  "project_tau_only_array", "surface_audit", "strict_project_array", "projection_unit_test",
  "fm_depth", "make_depth_weights", "fit_quantile_surface", "predict_quantile_surface",
  "depth_configuration_grid", "pinball_loss", "mean_pinball_score", "observation_pinball_loss",
  "crps_quantile_approx", "weighted_interval_score", "quantile_calibration", "score_surface",
  "tail_surface_diagnostics", "compare_weighted_unweighted_tails", "moving_block_mean_ci", "make_training_climatology",
  "make_curve_persistence", "make_rolling_historical_quantiles", "finite_sample_upper_quantile", "moving_block_calibration_quantile",
  "training_horizon_scale", "make_simultaneous_band", "make_simultaneous_upper_band", "band_metrics",
  "upper_band_metrics", "select_depth_configuration", "fit_empirical_bundle", "evaluate_method_bundle",
  "evaluate_horizon_subsets", "evaluate_bands", "evaluate_upper_bands", "run_depth_sensitivity",
  "quantile_grid_definitions", "run_grid_sensitivity", "run_block_sensitivity", "run_upper_block_sensitivity",
  "descriptive_statistics", "run_empirical_asset", "run_empirical_suite", "refresh_empirical_paired_inference",
  "simulation_scenarios", "simulate_garch_returns", "draw_standardized_innovations", "simulate_conditional_oracle_quantiles",
  "oracle_quantile_diagnostics", "simulation_method_result", "run_simulation_replication", "summarise_simulation",
  "monte_carlo_coverage_summary", "simulation_band_claim_diagnostic", "simulation_shape_audit_summary", "build_simulation_weighting_comparison",
  "summarise_weighting_comparison", "run_simulation_suite", "compact_simulation_summary", "simulation_tail_summary",
  "publication_theme", "academicize_display_data", "make_empirical_figures", "make_fixed_panel_inference_figure",
  "make_pave_guard_sensitivity_figure", "make_simulation_figures", "flatten_config", "write_reproducibility_files",
  "legacy_upper_coverage_reference", "asset_metadata_for", "build_fixed_panel_adjusted_inference", "build_pave_guard_sensitivity",
  "build_validation_wis_identity_audit", "export_empirical_results", "expected_latex_tables", "audit_latex_tables",
  "run_final_audit", "core_unit_tests", "r1_assert", "r1_close",
  "r1_md5_object", "r1_save_rds", "r1_core_design", "r1_block_indices",
  "r1_validate_one", "r1_validate_empirical_cache", "r1_simulation_design", "r1_validate_simulation_cache",
  "r1_horizon_analysis", "r1_projection_decomposition", "r1_gamma_profile", "r1_depth_diagnostics",
  "r1_garch_unpack", "r1_garch_nll", "r1_fit_garch", "r1_filter_garch",
  "r1_garch_quantiles", "r1_garch_h1_quantile", "r1_band_context", "r1_band_from_calibration",
  "r1_context_metrics", "r1_bootstrap_score_quantiles", "r1_calibration_diagnostics", "r1_garch_benchmark",
  "r1_write_table", "r1_save_plot", "r1_make_figures", "run_r1_suite",
  "r1_unit_tests", "main", "CFG", "DIR",
  "LOG_FILE", "RUN_MODE", "FIXED_PANEL", "FIXED_PANEL_ASSESSMENT_LEVELS",
  "BAND_WIDTH_SCALE_NOTE", "%||%", "r1_garch_variance", "r1_original_run_empirical_asset",
  "r1_original_simulation_replication"
)

if (!as_flag(Sys.getenv("TWOIFQR_NO_MAIN", "false"))) RESULT <- main()
