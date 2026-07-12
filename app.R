# =============================================================================
# Point d'entree pour les PLATEFORMES D'HEBERGEMENT Shiny
# (shinyapps.io, Posit Connect, Shiny Server, ou `shiny::runApp()` a la racine).
#
# Ces plateformes cherchent un app.R / ui.R / www/index.html a la RACINE du
# projet deploye. La logique de l'application vit dans inst/app/ (structure de
# package R) ; ce fichier fait donc le pont vers inst/app/.
#
# Pour lancer depuis le PACKAGE installe, utilisez plutot : HStat::run_hstat()
# =============================================================================

app_dir <- "inst/app"
if (!file.exists(file.path(app_dir, "HStat.R"))) {
  # Cas d'un package installe : inst/ est aplati, l'app est sous app/
  alt <- system.file("app", package = "HStat")
  if (nzchar(alt) && file.exists(file.path(alt, "HStat.R"))) {
    app_dir <- alt
  } else {
    stop("Impossible de localiser le code de l'application (inst/app/HStat.R).",
         call. = FALSE)
  }
}

# Se placer dans le dossier de l'app : les source(\"Utils.R\", ...) et les
# ressources www/ (chemins relatifs) en dependent.
setwd(app_dir)

# HStat.R se termine par shinyApp(ui, server) ; on retourne cette valeur.
.hstat_app <- source("HStat.R", local = FALSE, encoding = "UTF-8")$value

if (!inherits(.hstat_app, "shiny.appobj")) {
  stop("HStat : l'application n'a pas pu etre construite. Verifiez que tous ",
       "les paquets requis sont installes (shinydashboard, shinyWidgets, ...).",
       call. = FALSE)
}

.hstat_app
