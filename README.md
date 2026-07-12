# HStat — Shiny Statistical Analysis Application

[![R-CMD-check](https://github.com/houphouet/Hstat/actions/workflows/R.yml/badge.svg)](https://github.com/houphouet/Hstat/actions/workflows/R.yml)

HStat is an interactive web application built with R Shiny that enables a
complete data analysis pipeline — from data import to advanced multivariate
analyses — without writing a single line of code.

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
│       ├── app_server.R
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

## Deployment / Déploiement

**Local (recommended):** install the package and run:
```r
library(HStat)
run_hstat()   # installs missing dependencies, then launches
```

**Shiny hosting (shinyapps.io, Posit Connect, Shiny Server):** these platforms
look for `app.R` at the *root* of the deployed folder. A root `app.R` is
provided for this purpose (it bridges to `inst/app/`). Deploy the repository
root; make sure all packages listed in `DESCRIPTION` are available on the
server.

---

## How to cite / Comment citer

If HStat is useful for your work, please cite it. In R:

```r
citation("HStat")
```

Or use one of the following:

**Text**
> KOUADIO, Houphouet (2026). HStat: Application Shiny interactive pour l'analyse statistique. Version 0.2.4. https://github.com/houphouet/hstat

**BibTeX**
```bibtex
@Manual{hstat,
  title  = {HStat: Application Shiny interactive pour l'analyse statistique},
  author = {Houphouet KOUADIO},
  year   = {2026},
  note   = {Version 0.2.4},
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
