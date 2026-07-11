# HStat — Shiny Statistical Analysis Application

HStat is an interactive web application built with R Shiny that enables a complete data analysis pipeline — from data import to advanced multivariate analyses — without writing a single line of code.

---

## Prerequisites

- **R** ≥ 4.4.0
- **RStudio** (recommended) or any other R environment
- Internet connection for automatic package installation (first run only)

---

## Launch the application

Open RStudio, then run in the console:

```r
shiny::runApp("HStat.R")
```

Required packages are installed automatically if needed.

---

## Project structure

```
├── HStat.R
├── Hstat.Rproj
├── mod_clean.R
├── mod_crosstab.R
├── mod_descriptive.R
├── mod_design.R
├── mod_explore.R
├── mod_filter.R
├── mod_qualitative.R
├── mod_tests.R
├── mod_threshold.R
├── mod_viz.R
├── README.md
├── Server.R
├── tests
│   └── test-hstat.R
├── Utils.R
├── UX.R
└── www
    ├── fonts
    │   ├── archivo-latin-400-normal.woff2
    │   ├── archivo-latin-500-normal.woff2
    │   ├── archivo-latin-600-normal.woff2
    │   ├── archivo-latin-700-normal.woff2
    │   ├── Archivo-LICENSE.txt
    │   ├── ibm-plex-mono-latin-400-normal.woff2
    │   ├── ibm-plex-mono-latin-500-normal.woff2
    │   ├── ibm-plex-mono-latin-600-normal.woff2
    │   ├── ibm-plex-sans-latin-400-normal.woff2
    │   ├── ibm-plex-sans-latin-500-normal.woff2
    │   ├── ibm-plex-sans-latin-600-normal.woff2
    │   ├── ibm-plex-sans-latin-700-normal.woff2
    │   ├── inter-latin-400-normal.woff2
    │   ├── inter-latin-500-normal.woff2
    │   ├── inter-latin-600-normal.woff2
    │   ├── inter-latin-700-normal.woff2
    │   ├── Inter-LICENSE.txt
    │   ├── newsreader-latin-400-italic.woff2
    │   ├── newsreader-latin-400-normal.woff2
    │   ├── newsreader-latin-500-normal.woff2
    │   ├── newsreader-latin-600-normal.woff2
    │   └── Newsreader-LICENSE.txt
    ├── hstat-theme.css
    └── Sortable.min.js
```

## License

This project is open source.

---

*HStat is developed to make statistical analysis accessible without any programming barrier.*
