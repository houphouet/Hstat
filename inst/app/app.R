# Point d entree standard Shiny : runApp(dossier) detecte ce fichier et sert
# automatiquement le sous-dossier www/ (feuille de style + polices).
# HStat.R est l application mono-fichier : il source les modules puis appelle
# shinyApp(ui, server). On recupere sa valeur pour la retourner a Shiny.
app <- source("HStat.R", local = FALSE, encoding = "UTF-8")$value
app
