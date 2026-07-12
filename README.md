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
> KOUADIO, Houphouet (2026). HStat: Application Shiny interactive pour l'analyse statistique. Version 0.4.0. https://github.com/houphouet/hstat

**BibTeX**
```bibtex
@Manual{hstat,
  title  = {HStat: Application Shiny interactive pour l'analyse statistique},
  author = {Houphouet KOUADIO},
  year   = {2026},
  note   = {Version 0.4.0},
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
