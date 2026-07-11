#' Launch the HStat Shiny Application
#'
#' Starts the HStat interactive statistical analysis application in your
#' default web browser.
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
      "Try re-installing the package with remotes::install_github('houphouet/Hstat').",
      call. = FALSE
    )
  }

  # HStat.R is a "single-file" Shiny app (it ends with shinyApp(ui, server)),
  # exactly as launched in the original README via shiny::runApp("HStat.R").
  # We point runApp() directly at that file rather than at the app_dir,
  # since app_dir does not contain an app.R/ui.R/server.R that runApp()
  # would auto-detect.
  app_file <- file.path(app_dir, "HStat.R")

  if (!file.exists(app_file)) {
    stop(
      "Could not find HStat.R inside the installed package (expected at: ",
      app_file, "). Make sure inst/app/HStat.R exists in the package source.",
      call. = FALSE
    )
  }

  shiny::runApp(app_file, ...)
}
