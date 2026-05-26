# HStat, Shiny Statistical Analysis Application

HStat is an interactive web application built with R Shiny that enables a complete data analysis pipeline, from data import to advanced multivariate analyses, without writing a single line of code.

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
.
├── HStat.R      # Entry point: sources the modules and calls shinyApp()
├── Server.R     # Server logic (server function)
├── Utils.R      # Encoding, package installation, utility functions
├── UX.R         # User interface (ui object)
└── www
    ├── fonts
    │   ├── inter-latin-400-normal.woff2
    │   ├── inter-latin-500-normal.woff2
    │   ├── inter-latin-600-normal.woff2
    │   ├── inter-latin-700-normal.woff2
    │   └── Inter-LICENSE.txt
    └── Sortable.min.js

```

## License

This project is open source.

---

*HStat is developed to make statistical analysis accessible without any programming barrier.*
