################################################################################
#
#  HStat - Application Shiny d'analyse statistique
#
################################################################################

# Source des modules
source("Utils.R",  local = FALSE, encoding = "UTF-8")
source("UX.R",    local = FALSE, encoding = "UTF-8")
source("Server.R", local = FALSE, encoding = "UTF-8")

# Lancement de l'application
shinyApp(ui, server)
