#' Launch the HStat Shiny Application
#'
#' Starts the HStat interactive statistical analysis application in your
#' default web browser. On first launch, any missing package dependencies
#' are installed automatically (an internet connection is required the first
#' time).
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'   (e.g. \code{launch.browser}, \code{port}).
#'
#' @return No return value. Called for its side effect of launching the app.
#'
#' @examples
#' if (interactive()) {
#'   run_hstat()
#' }
#'
#' @export
run_hstat <- function(...) {
  app_dir <- system.file("app", package = "HStat")

  if (app_dir == "") {
    stop(
      "Could not find the HStat app directory. ",
      "Try re-installing the package with remotes::install_github('houphouet/hstat').",
      call. = FALSE
    )
  }

  app_file <- file.path(app_dir, "app.R")
  if (!file.exists(app_file)) {
    stop("Could not find app.R inside the installed package (expected at: ",
         app_file, ").", call. = FALSE)
  }

  # --- Dependances critiques pour CONSTRUIRE l'interface -------------------
  # Sans elles, HStat.R echoue sur dashboardPage()/pickerInput() et Shiny
  # renvoie l'erreur trompeuse "No UI defined". On s'assure donc qu'elles
  # sont presentes AVANT de lancer l'application, avec un message clair.
  ui_critical <- c("shiny", "shinydashboard", "shinyWidgets", "shinyjs",
                   "DT", "ggplot2", "plotly", "dplyr")
  missing_ui <- ui_critical[
    !vapply(ui_critical, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_ui) > 0) {
    message("HStat : installation des paquets d'interface requis : ",
            paste(missing_ui, collapse = ", "))
    installed <- tryCatch({
      utils::install.packages(missing_ui, quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    still_missing <- ui_critical[
      !vapply(ui_critical, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still_missing) > 0) {
      stop(
        "HStat ne peut pas demarrer : les paquets suivants, indispensables a\n",
        "l'interface, sont manquants et n'ont pas pu etre installes :\n  ",
        paste(still_missing, collapse = ", "),
        "\n\nInstallez-les manuellement puis relancez run_hstat() :\n",
        '  install.packages(c(', paste(sprintf('"%s"', still_missing),
                                        collapse = ", "), "))",
        call. = FALSE)
    }
  }

  # runApp() sur un DOSSIER sert automatiquement le sous-dossier www/
  # (feuille de style + polices) et detecte app.R.
  shiny::runApp(app_dir, ...)
}
