# HStat : Shiny Statistical Analysis Application

[![R-CMD-check](https://github.com/houphouet/Hstat/actions/workflows/R.yml/badge.svg)](https://github.com/houphouet/Hstat/actions/workflows/R.yml)

HStat is an interactive web application built with R Shiny that enables a
complete data analysis pipeline, from data import to advanced multivariate
analyses without writing a single line of code.

---

## Prerequisites

- **R** ≥ 4.4.0
- **RStudio** (recommended) or any other R environment
- Internet connection for automatic package installation (first run only)

---

## Installation

Install directly from GitHub using `remotes`:

```r
# install.packages("remotes")
remotes::install_github("houphouet/Hstat")
```

## Launch the application

```r
library(HStat)
run_hstat()
```

This opens the app in your default web browser. Required packages are
installed automatically if needed on first run.

---

## Large datasets (out-of-memory engine)

HStat handles datasets far beyond available RAM. Files under the
out-of-memory threshold (500 MB by default) are loaded in memory with
`data.table::fread`. Above it, CSV/Parquet/DuckDB files are **never loaded
into RAM**: DuckDB queries them on disk, exact statistics are computed by
SQL on the *full* dataset, and interactive analyses run on a reproducible
random sample (100 000 rows by default, adjustable up to 10 million in the
UI). Row counts above 2^31 (2.1 billion rows) are supported.

Environment variables (all optional):

| Variable | Default | Purpose |
|---|---|---|
| `HSTAT_MAX_UPLOAD_MB` | `102400` (100 GB) | Max upload size |
| `HSTAT_BIGDATA_THRESHOLD_MB` | `500` | Out-of-memory switch threshold |
| `HSTAT_SAMPLE_SIZE` | `100000` | Working sample size |
| `HSTAT_DUCKDB_MEMORY` | *(unset)* | DuckDB RAM cap, e.g. `8GB` (spills to disk beyond) |
| `HSTAT_PLOT_MAX_POINTS` | `100000` | Max points drawn on scatterplots |
| `HSTAT_DIST_MAX_N` | `5000` | Cap for O(n²) distance-matrix analyses |
| `HSTAT_KENDALL_MAX_N` | `20000` | Cap for Kendall correlation |
| `HSTAT_IMPUTE_MAX_N` | `100000` | Cap for kNN/missForest imputation |
| `HSTAT_ML_MAX_N` | `200000` | Training-set cap for ML/DL models |

---

## Predictive modelling (v0.5.x)

Three dedicated modules cover forecasting and prediction end to end. Every
model reports interpreted metrics, customizable plots, an automatic
plain-language interpretation, a prediction simulator (manual input or batch
import of new cases), and full export: tables as CSV/Excel, figures as
PNG/JPG/TIFF/BMP/PDF/SVG at up to 20,000 DPI (with an automatic pixel safety
cap for raster formats).

**Time series** — naïve & seasonal naïve, historical mean, drift, SES, Holt,
damped Holt, Holt-Winters (additive/multiplicative), ETS, auto-ARIMA, manual
SARIMA, TBATS, Theta, STL+ETS, NNAR, Prophet, DLM (dynamic linear model:
local level + trend + seasonality estimated by MLE and filtered by Kalman,
`dlm` package) and DLNM (distributed lag non-linear model, `dlnm` package:
delayed non-linear effect of an exposure variable — e.g. temperature or
pollution — on the outcome, the classic environmental-epidemiology design;
quasi-Poisson family is selected automatically for count outcomes, and the
simulator accepts a file of future exposure values). Models are
compared on a held-out test window (RMSE/MAE/MAPE/MASE/AIC), residuals are
diagnosed (Ljung-Box, ACF), the series is decomposed (STL), and the simulator
forecasts any horizon — optionally after appending newly imported
observations.

**Machine learning** — task auto-detected (regression vs classification):
linear/logistic model, Ridge/Lasso/Elastic-Net (glmnet), decision tree
(rpart), random forest, gradient boosting (xgboost), SVM (e1071), k-nearest
neighbours (kknn), Naïve Bayes, and a single-layer neural network (nnet),
plus unsupervised clustering (k-means, hierarchical, PAM, DBSCAN, Gaussian
mixtures) with silhouette and elbow diagnostics. Includes ROC curves,
confusion matrices, variable importance, and model comparison on a held-out
test set.

**Deep learning** — 100 % R, no Python required: multi-layer perceptron via
`neuralnet` (always available) or via `torch` (optional, with per-epoch
learning curves), and an LSTM sequence forecaster (torch). Predictors are
standardized automatically; the architecture and trained parameter count are
reported.

All modelling packages — including `prophet`, `torch`, `dlm` and `dlnm` —
are installed automatically at first launch. They are loaded via their
namespaces only (never attached), so none of their exports can mask core
functions of the app (e.g. `mclust::em` vs `shiny::em`). torch additionally
downloads its native libraries once (`torch::install_torch()` is run
automatically; if it fails, the neuralnet engine remains fully available).

---

## Project structure

```
├── DESCRIPTION
├── NAMESPACE
├── R/
│   └── run_hstat.R
├── inst/
│   └── app/
│       ├── HStat.R
│       ├── app_server.R
│       ├── UX.R
│       ├── Utils.R
│       ├── mod_clean.R
│       ├── mod_descriptive.R
│       ├── mod_design.R
│       ├── mod_explore.R
│       ├── mod_filter.R
│       ├── mod_qualitative.R
│       ├── mod_tests.R
│       ├── mod_threshold.R
│       ├── mod_viz.R
│       └── www/
│           ├── fonts/
│           ├── hstat-theme.css
│           └── Sortable.min.js
├── tests/
│   ├── testthat.R
│   └── testthat/
│       └── test-hstat.R
└── README.md
```
---

## How to cite / Comment citer

If HStat is useful for your work, please cite it. In R:

```r
citation("HStat")
```

Or use one of the following:

**Text**
> KOUADIO, Houphouet (2026). HStat: Application Shiny interactive pour l'analyse statistique. Version 0.5.1. https://github.com/houphouet/hstat

**BibTeX**
```bibtex
@Manual{hstat,
  title  = {HStat: Application Shiny interactive pour l'analyse statistique},
  author = {Houphouet KOUADIO},
  year   = {2026},
  note   = {Version 0.5.1},
  url    = {https://github.com/houphouet/hstat},
}
```

The application also has a **"Citer HStat"** tab offering the citation in Text,
BibTeX, RIS, APA, Vancouver and Markdown styles, with copy and download buttons.

---

## License

This project is licensed under the GPL-3.0 License.

---

## Author

**Houphouet KOUADIO**
ORCID: [0000-0002-8238-1091](https://orcid.org/0000-0002-8238-1091)

Development started on **17 September 2025** (développement débuté le 17 septembre 2025).

---

*HStat is developed to make statistical analysis accessible without any
programming barrier.*
