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

  # runApp() sur un DOSSIER sert automatiquement le sous-dossier www/
  # (feuille de style + polices) et detecte app.R. Le fichier inst/app/app.R
  # se contente de sourcer HStat.R, qui est l'application mono-fichier.
  if (!file.exists(file.path(app_dir, "app.R"))) {
    stop("Could not find app.R inside the installed package (expected at: ",
         file.path(app_dir, "app.R"), ").", call. = FALSE)
  }

  shiny::runApp(app_dir, ...)
}
