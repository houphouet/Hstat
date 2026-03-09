# HStat — Shiny Statistical Analysis Application

HStat is an interactive web application built with R Shiny that enables a complete data analysis pipeline — from data import to advanced multivariate analyses — without writing a single line of code.

---

## Prerequisites

- **R** ≥ 4.2.0
- **RStudio** (recommended) or any other R environment
- Internet connection for automatic package installation (first run only)

---

### 1. Launch the application

Open RStudio, then run in the console:

```r
shiny::runApp("HStat.R")
```

Required packages are installed automatically if needed.

---

## Project structure

```
HStat/
├── HStat.R        # Entry point: sources the modules and calls shinyApp()
├── Utils.R      # Encoding, package installation, utility functions
├── UX.R         # User interface (ui object)
├── Server.R     # Server logic (server function)
└── README.md    # This file
```

## License

This project is open source.

---

*HStat is developed to make statistical analysis accessible without any programming barrier.*
