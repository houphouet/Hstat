# HStat : Shiny Statistical Analysis Application

[![R-CMD-check](https://github.com/houphouet/Hstat/actions/workflows/R.yml/badge.svg)](https://github.com/houphouet/Hstat/actions/workflows/R.yml)

HStat is an interactive web application built with R Shiny that enables a
complete data analysis pipeline, from data import to advanced multivariate
analyses  without writing a single line of code.

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

## Project structure

```
├── DESCRIPTION
├── NAMESPACE
├── R/
│   └── run_hstat.R
├── inst/
│   └── app/
│       ├── HStat.R
│       ├── Server.R
│       ├── UX.R
│       ├── Utils.R
│       ├── mod_clean.R
│       ├── mod_crosstab.R
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

## License

This project is licensed under the GPL-3.0 License.

---

## Author

**Houphouet KOUADIO**
ORCID: [0000-0002-8238-1091](https://orcid.org/0000-0002-8238-1091)

---

*HStat is developed to make statistical analysis accessible without any
programming barrier.*
