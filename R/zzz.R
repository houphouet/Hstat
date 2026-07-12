## Message affiche au chargement du package (library(HStat) / require(HStat)).
## Propose immediatement a l'utilisateur comment citer HStat.

.onAttach <- function(libname, pkgname) {
  vers <- tryCatch(as.character(utils::packageVersion("HStat")),
                   error = function(e) "")
  year <- tryCatch(sub("-.*", "", as.character(utils::packageDate("HStat"))),
                   error = function(e) NA_character_)
  if (is.null(year) || is.na(year) || !nzchar(year))
    year <- format(Sys.Date(), "%Y")

  msg <- paste0(
    "HStat ", vers, " charge.\n",
    "Pour lancer l'application : run_hstat()\n\n",
    "Si HStat vous est utile, merci de le citer :\n",
    "  KOUADIO, Houphouet (", year, "). HStat : Application Shiny interactive ",
    "pour l'analyse statistique.\n",
    "  Version ", vers, ". https://github.com/houphouet/hstat\n\n",
    "Citation complete et autres styles (BibTeX, RIS, APA...) : citation(\"HStat\")"
  )
  packageStartupMessage(msg)
}
