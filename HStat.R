#  HStat - Application Shiny d'analyse statistique

local({
  candidates <- if (.Platform$OS.type == "windows") {
    c("French_France.utf8", "C.UTF-8", "en_US.UTF-8")
  } else {
    c("fr_FR.UTF-8", "fr_FR.utf8", "en_US.UTF-8", "C.UTF-8", "C.utf8")
  }
  for (loc in candidates) {
    ok <- tryCatch(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "",
                   error = function(e) FALSE)
    if (isTRUE(ok)) break
  }
})

source("Utils.R",  local = FALSE, encoding = "UTF-8")
source("mod_threshold.R", local = FALSE, encoding = "UTF-8")
source("mod_explore.R", local = FALSE, encoding = "UTF-8")
source("mod_clean.R", local = FALSE, encoding = "UTF-8")
source("mod_filter.R", local = FALSE, encoding = "UTF-8")
source("mod_descriptive.R", local = FALSE, encoding = "UTF-8")
source("mod_viz.R", local = FALSE, encoding = "UTF-8")
source("mod_tests.R", local = FALSE, encoding = "UTF-8")
source("mod_design.R", local = FALSE, encoding = "UTF-8")
source("mod_qualitative.R", local = FALSE, encoding = "UTF-8")
source("UX.R",     local = FALSE, encoding = "UTF-8")
source("Server.R", local = FALSE, encoding = "UTF-8")

shinyApp(ui, server)
