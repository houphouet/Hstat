################################################################################
#
#             Encodage de l'application
#
################################################################################


Sys.setlocale("LC_ALL", "C")
options(encoding = "UTF-8")

# Force UTF-8 pour les entrées/sorties
if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_CTYPE", "French_France.UTF-8")
} else {
  Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")
}


################################################################################
# 
# Fonctions utilitaires
#
################################################################################


# ---- Gestion des packages ----
install_and_load <- function(packages) {
  installed_packages <- installed.packages()[, "Package"]
  to_install <- packages[!packages %in% installed_packages]
  if (length(to_install) > 0) install.packages(to_install, repos = "https://cran.r-project.org")
  for (pkg in packages) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

required_packages <- c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets", "shinyalert","DT", "shinycssloaders",
  "RColorBrewer", "colourpicker", "ggrepel",  "openxlsx", "rmarkdown", "haven",
  "dplyr", "knitr", "stringr", "scales", "ggplot2", "ggdendro", "reshape2", "sortable",
  "tibble", "plotrix", "plotly",  "qqplotr", "tidyr",  "report", "see", "corrplot",
  "car", "agricolae","forcats", "bslib", "factoextra",  "FactoMineR","questionr",  "digest",
  "MASS", "cluster", "GGally", "psych", "nortest", "lmtest", "multcomp","FSA", "treemapify",
  "stats",  "emmeans", "performance","purrr", "PMCMRplus","multcompView", "rcompanion"
)

install_and_load(required_packages)

# Petit utilitaire
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Fonction d'interprétation des p-values
interpret_p_value <- function(p_value) {
  if (is.na(p_value)) {
    return("NA")
  } else if (p_value < 0.001) {
    return("Hautement significatif (p < 0.001)")
  } else if (p_value < 0.01) {
    return("Très significatif (p < 0.01)")
  } else if (p_value < 0.05) {
    return("Significatif (p < 0.05)")
  } else {
    return("Non significatif (p >= 0.05)")
  }
}

# Fonction pour interpréter les résultats statistiques
interpret_test_results <- function(test_type, p_value, test_object = NULL) {
  if (is.na(p_value)) return("Résultat non disponible")
  
  significance <- ifelse(p_value < 0.05, "significative", "non significative")
  
  switch(test_type,
         "t.test" = paste0("Le test t montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "wilcox.test" = paste0("Le test de Wilcoxon montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "anova" = paste0("L'ANOVA montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "kruskal.test" = paste0("Le test de Kruskal-Wallis montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "scheirerRayHare" = paste0("Le test de Scheirer-Ray-Hare montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "chisq.test" = paste0("Le test du chi² montre une association ", significance, " entre les variables (p = ", round(p_value, 8), ")"),
         "cor.test" = paste0("La corrélation est ", significance, " (p = ", round(p_value, 8), ")"),
         paste0("Le test ", test_type, " montre un résultat ", significance, " (p = ", round(p_value, 8), ")")
  )
}

# Fonctions d'interprétation pour la normalité et l'homogénéité
interpret_normality <- function(p_value) {
  if (is.na(p_value)) return("Résultat non disponible")
  if (p_value >= 0.05) {
    return(paste0("La distribution est normale (p = ", round(p_value, 8), " >= 0.05)"))
  } else {
    return(paste0("La distribution n'est pas normale (p = ", round(p_value, 8), " < 0.05)"))
  }
}

interpret_homogeneity <- function(p_value) {
  if (is.na(p_value)) return("Résultat non disponible")
  if (p_value >= 0.05) {
    return(paste0("Les variances sont homogènes (p = ", round(p_value, 8), " >= 0.05)"))
  } else {
    return(paste0("Les variances ne sont pas homogènes (p = ", round(p_value, 8), " < 0.05)"))
  }
}

# Fonction pour interpréter la normalité des résidus
interpret_normality_resid <- function(p_value) {
  if (is.na(p_value)) return("Test non applicable")
  if (p_value > 0.05) {
    return("Les résidus suivent une distribution normale (p > 0.05). Les conditions pour les tests paramétriques sont respectées.")
  } else {
    return("Les résidus ne suivent pas une distribution normale (p < 0.05). Considérez l'utilisation de tests non-paramétriques.")
  }
}

# Fonction pour interpréter l'homogénéité des résidus
interpret_homogeneity_resid <- function(p_value) {
  if (is.na(p_value)) return("Test non applicable")
  if (p_value > 0.05) {
    return("Les variances sont homogènes (p > 0.05). Les conditions pour les tests paramétriques sont respectées.")
  } else {
    return("Les variances ne sont pas homogènes (p < 0.05). Utilisez des tests robustes à l'hétérogénéité des variances.")
  }
}

# Fonction pour filtrage croisé complet (2 facteurs)
filter_complete_cross <- function(df, A, B, reqA = TRUE, reqB = FALSE) {
  df <- df[, , drop = FALSE]
  if (!is.factor(df[[A]])) df[[A]] <- factor(df[[A]])
  if (!is.factor(df[[B]])) df[[B]] <- factor(df[[B]])
  changed <- TRUE
  while (changed) {
    tab <- table(droplevels(df[[A]]), droplevels(df[[B]]))
    keepA <- rownames(tab)[apply(tab > 0, 1, all)]
    keepB <- colnames(tab)[apply(tab > 0, 2, all)]
    old_n <- nrow(df)
    if (reqA) df <- df[df[[A]] %in% keepA, , drop = FALSE]
    if (reqB) df <- df[df[[B]] %in% keepB, , drop = FALSE]
    df <- droplevels(df)
    changed <- nrow(df) < old_n
  }
  return(df)
}

# Fonction pour filtrage croisé complet (N facteurs)
filter_complete_cross_n <- function(df, factors) {
  if (length(factors) < 2) return(df)
  dfx <- df
  for (f in factors) if (!is.factor(dfx[[f]])) dfx[[f]] <- factor(dfx[[f]])
  changed <- TRUE
  while (changed) {
    cnt <- dfx %>% dplyr::count(dplyr::across(dplyr::all_of(factors)), name = "n", .drop = FALSE)
    full <- tidyr::complete(cnt, tidyr::nesting(!!!rlang::syms(factors)))
    miss <- full[is.na(full$n), , drop = FALSE]
    if (nrow(miss) == 0) break
    levels_to_drop <- lapply(factors, function(f) unique(miss[[f]]))
    names(levels_to_drop) <- factors
    old_n <- nrow(dfx)
    for (f in factors) {
      dfx <- dfx[!(dfx[[f]] %in% levels_to_drop[[f]]), , drop = FALSE]
    }
    dfx <- droplevels(dfx)
    changed <- nrow(dfx) < old_n
    if (nrow(dfx) == 0) break
  }
  dfx
}

# Fonction pour calcul du CV
calc_cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100

################################################################################
#
# Interface UI de l'application Shiny
#
#
################################################################################


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(icon("flask"), "HStat"), 
    titleWidth = 300,
    dropdownMenu(
      type = "notifications", 
      badgeStatus = "info",
      notificationItem(
        text = "L'application est prête",
        icon = icon("check-circle"),
        status = "success"
      )
    )
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Chargement", tabName = "load", icon = icon("upload")),
      menuItem("Exploration", tabName = "explore", icon = icon("binoculars")),
      menuItem("Nettoyage", tabName = "clean", icon = icon("broom")),
      menuItem("Filtrage", tabName = "filter", icon = icon("filter")),
      menuItem("Analyses descriptives", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("Tableaux croisés", tabName = "crosstab", icon = icon("table")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Réalisation des tests statistiques", tabName = "tests", icon = icon("calculator")),
      menuItem("Comparaisons multiples PostHoc", tabName = "multiple", icon = icon("sort-amount-down")),
      menuItem("Analyses multivariées", tabName = "multivariate", icon = icon("project-diagram")),
      menuItem("Seuils d'efficacité", tabName = "threshold", icon = icon("chart-bar")),
      menuItem("Rapport de synthèse des analyses", tabName = "report", icon = icon("file-alt")),
      hr(),
      actionButton("helpBtn", "Aide", icon = icon("question-circle"), class = "btn-info"),
      actionButton("resetBtn", "Réinitialiser", icon = icon("redo"), class = "btn-warning")
    )
  ),
  dashboardBody(
    useShinyjs(),
    useShinyalert(force = TRUE),
    tags$head(
      tags$style(HTML("
        .box-title { font-weight: bold; }
        .btn { margin-right: 5px; }
        .progress-bar { background-color: #3c8dbc; }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #3c8dbc; }
        .info-box { min-height: 80px; }
        .info-box-icon { height: 80px; line-height: 80px; }
        .info-box-content { padding: 10px; }
        .small-box { border-radius: 5px; }
        .main-header .logo { font-weight: bold; }
        .interpretation-box { background-color: #f9f9f9; border-left: 4px solid #3c8dbc; padding: 10px; margin-top: 10px; }
      ")),
    ),
    tabItems(
      # ---- Chargement ----
      tabItem(tabName = "load",
              fluidRow(
                box(title = "Charger données", status = "primary", width = 12, solidHeader = TRUE,
                    fileInput("file", "Choisir un fichier",
                              accept = c(".csv", ".xlsx", ".xls", ".txt", ".sav", ".dta", ".rds")),
                    uiOutput("sheetUI"),
                    radioButtons("sep", "Séparateur (CSV/TXT)",
                                 choices = c(Virgule = ",", `Point-virgule` = ";", Tab = "\t"), selected = ","),
                    checkboxInput("header", "Avec en-têtes", TRUE),
                    actionButton("loadData", "Charger", class = "btn-primary", icon = icon("upload")),
                    hr(),
                    h4("Exemple de données"),
                    p("Vous pouvez utiliser vos propres données ou télécharger un exemple pour tester l'application:"),
                    downloadButton("downloadExample", "Télécharger exemple (CSV)", class = "btn-info")
                )
              ),
              fluidRow(
                valueBoxOutput("nrowBox", width = 3),
                valueBoxOutput("ncolBox", width = 3),
                valueBoxOutput("naBox", width = 3),
                valueBoxOutput("memBox", width = 3)
              ),
              fluidRow(
                box(title = "Aperçu des données", status = "info", width = 12, solidHeader = TRUE,
                    DTOutput("preview"))
              )
      ),
      
      # ---- Exploration ----
      
      tabItem(tabName = "explore",
              # En-tête de section
              fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  background = "light-blue",
                  h3(icon("chart-line"), "Exploration des Données", style = "margin: 0; color: white;"),
                  p("Analysez la structure, les corrélations et les distributions de vos données", 
                    style = "margin: 5px 0 0 0; color: white; opacity: 0.9;")
                )
              ),
              
              # Structure et Résumé statistique
              fluidRow(
                box(
                  title = tagList(icon("database"), "Structure des données"), 
                  status = "info", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  verbatimTextOutput("dataStructure"),
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("info-circle"), 
                    " Visualisez les types de variables et leur structure"
                  )
                ),
                box(
                  title = tagList(icon("calculator"), "Résumé statistique"), 
                  status = "info", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  verbatimTextOutput("dataSummary"),
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("info-circle"), 
                    " Statistiques descriptives pour chaque variable"
                  )
                )
              ),
              
              # Matrice de corrélation
              fluidRow(
                box(
                  title = tagList(icon("project-diagram"), "Matrice de Corrélation"), 
                  status = "success", 
                  width = 12, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  # Section de sélection des variables
                  fluidRow(
                    column(12,
                           div(
                             style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                             uiOutput("corrVarSelect")
                           )
                    )
                  ),
                  
                  # Accordéon pour les options
                  tags$div(
                    class = "panel-group",
                    id = "corrOptionsAccordion",
                    
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h4(
                          class = "panel-title",
                          tags$a(
                            `data-toggle` = "collapse",
                            `data-parent` = "#corrOptionsAccordion",
                            href = "#corrOptionsCollapse",
                            style = "text-decoration: none;",
                            icon("cog"), " Options graphiques avancées",
                            tags$span(
                              class = "pull-right",
                              icon("chevron-down")
                            )
                          )
                        )
                      ),
                      tags$div(
                        id = "corrOptionsCollapse",
                        class = "panel-collapse collapse",
                        tags$div(
                          class = "panel-body",
                          style = "background-color: #f8f9fa;",
                          
                          fluidRow(
                            column(3,
                                   h5(icon("sliders-h"), "Méthode", style = "color: #27ae60; font-weight: bold;"),
                                   selectInput(
                                     "corrMethod", 
                                     "Méthode de corrélation:", 
                                     choices = list(
                                       "Pearson (linéaire)" = "pearson", 
                                       "Spearman (monotone)" = "spearman", 
                                       "Kendall (robuste)" = "kendall"
                                     ),
                                     selected = "pearson"
                                   ),
                                   div(
                                     style = "font-size: 11px; color: #7f8c8d; margin-top: -10px;",
                                     icon("info-circle"), " Pearson pour relations linéaires"
                                   )
                            ),
                            
                            column(3,
                                   h5(icon("palette"), "Affichage", style = "color: #27ae60; font-weight: bold;"),
                                   selectInput(
                                     "corrDisplay", 
                                     "Mode d'affichage:", 
                                     choices = list(
                                       "Nombres" = "number", 
                                       "Cercles" = "circle", 
                                       "Carrés" = "square",
                                       "Ellipses" = "ellipse",
                                       "Couleurs" = "color",
                                       "Secteurs" = "pie"
                                     ),
                                     selected = "number"
                                   ),
                                   selectInput(
                                     "corrType", 
                                     "Type d'affichage:", 
                                     choices = list(
                                       "Complet" = "full", 
                                       "Triangulaire supérieur" = "upper", 
                                       "Triangulaire inférieur" = "lower"
                                     ),
                                     selected = "upper"
                                   )
                            ),
                            
                            column(3,
                                   h5(icon("text-height"), "Tailles", style = "color: #27ae60; font-weight: bold;"),
                                   sliderInput(
                                     "corrTextSize", 
                                     "Taille des valeurs:", 
                                     min = 0.3, max = 2, value = 0.8, step = 0.1,
                                     ticks = FALSE
                                   ),
                                   sliderInput(
                                     "corrLabelSize", 
                                     "Taille des labels:", 
                                     min = 0.3, max = 2, value = 0.8, step = 0.1,
                                     ticks = FALSE
                                   )
                            ),
                            
                            column(3,
                                   h5(icon("heading"), "Titre", style = "color: #27ae60; font-weight: bold;"),
                                   textInput(
                                     "corrTitle", 
                                     "Titre personnalisé:", 
                                     placeholder = "Laisser vide pour titre auto"
                                   ),
                                   checkboxInput(
                                     "corrCenterTitle", 
                                     tagList(icon("align-center"), " Centrer le titre"), 
                                     value = TRUE
                                   ),
                                   numericInput(
                                     "corrDPI", 
                                     tagList(icon("image"), " DPI export:"), 
                                     value = 300, 
                                     min = 72, 
                                     max = 600, 
                                     step = 50
                                   )
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  hr(),
                  
                  # Boutons d'action
                  fluidRow(
                    column(12,
                           div(
                             style = "text-align: center; margin: 20px 0;",
                             downloadButton(
                               "downloadCorrPlot", 
                               tagList(icon("download"), " Télécharger PNG"), 
                               class = "btn-info btn-lg"
                             )
                           )
                    )
                  ),
                  
                  # Graphique
                  div(
                    style = "background-color: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    withSpinner(
                      plotOutput("corrPlot", height = "600px"),
                      type = 6,
                      color = "#27ae60"
                    )
                  ),
                  
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("lightbulb"), 
                    " Astuce: Les corrélations proches de 1 ou -1 indiquent des relations fortes"
                  )
                )
              ),
              
              # Distribution et Valeurs manquantes
              fluidRow(
                # Distribution
                box(
                  title = tagList(icon("chart-area"), "Distribution des Variables"), 
                  status = "primary", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  uiOutput("distVarSelect"),
                  
                  # Accordéon pour les options
                  tags$div(
                    class = "panel-group",
                    id = "distOptionsAccordion",
                    style = "margin-top: 15px;",
                    
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h4(
                          class = "panel-title",
                          tags$a(
                            `data-toggle` = "collapse",
                            `data-parent` = "#distOptionsAccordion",
                            href = "#distOptionsCollapse",
                            style = "text-decoration: none;",
                            icon("cog"), " Options graphiques",
                            tags$span(class = "pull-right", icon("chevron-down"))
                          )
                        )
                      ),
                      tags$div(
                        id = "distOptionsCollapse",
                        class = "panel-collapse collapse",
                        tags$div(
                          class = "panel-body",
                          style = "background-color: #f8f9fa;",
                          
                          fluidRow(
                            column(6,
                                   h5(icon("text-height"), "Tailles", style = "color: #3498db; font-weight: bold;"),
                                   sliderInput("distTitleSize", "Taille titre:", 
                                               min = 8, max = 24, value = 14, ticks = FALSE),
                                   sliderInput("distAxisTitleSize", "Taille titres axes:", 
                                               min = 8, max = 20, value = 12, ticks = FALSE),
                                   sliderInput("distAxisTextSize", "Taille texte axes:", 
                                               min = 6, max = 16, value = 10, ticks = FALSE)
                            ),
                            column(6,
                                   h5(icon("heading"), "Personnalisation", style = "color: #3498db; font-weight: bold;"),
                                   textInput("distTitle", "Titre personnalisé:", 
                                             placeholder = "Laisser vide pour titre auto"),
                                   checkboxInput("distCenterTitle", 
                                                 tagList(icon("align-center"), " Centrer le titre"), 
                                                 value = TRUE),
                                   checkboxInput("distShowDensity", 
                                                 tagList(icon("wave-square"), " Afficher courbe densité"), 
                                                 value = TRUE),
                                   numericInput("distDPI", tagList(icon("image"), " DPI export:"), 
                                                value = 300, min = 72, max = 600, step = 50)
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  hr(),
                  
                  div(
                    style = "text-align: center; margin: 15px 0;",
                    downloadButton("downloadDistPlot", 
                                   tagList(icon("download"), " Télécharger PNG"), 
                                   class = "btn-info")
                  ),
                  
                  div(
                    style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    withSpinner(
                      plotOutput("distPlot", height = "500px"),
                      type = 6,
                      color = "#3498db"
                    )
                  ),
                  
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("info-circle"), 
                    " Analysez la normalité et la dispersion de vos variables"
                  )
                ),
                
                # Valeurs manquantes
                box(
                  title = tagList(icon("exclamation-triangle"), "Analyse des Valeurs Manquantes"), 
                  status = "warning", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  # Accordéon pour les options
                  tags$div(
                    class = "panel-group",
                    id = "missingOptionsAccordion",
                    
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h4(
                          class = "panel-title",
                          tags$a(
                            `data-toggle` = "collapse",
                            `data-parent` = "#missingOptionsAccordion",
                            href = "#missingOptionsCollapse",
                            style = "text-decoration: none;",
                            icon("cog"), " Options graphiques",
                            tags$span(class = "pull-right", icon("chevron-down"))
                          )
                        )
                      ),
                      tags$div(
                        id = "missingOptionsCollapse",
                        class = "panel-collapse collapse",
                        tags$div(
                          class = "panel-body",
                          style = "background-color: #fff8e1;",
                          
                          fluidRow(
                            column(4,
                                   h5(icon("text-height"), "Tailles", style = "color: #f39c12; font-weight: bold;"),
                                   sliderInput("missingTitleSize", "Taille titre:", 
                                               min = 8, max = 24, value = 14, ticks = FALSE),
                                   sliderInput("missingAxisTitleSize", "Taille titres axes:", 
                                               min = 8, max = 20, value = 12, ticks = FALSE)
                            ),
                            column(4,
                                   h5(icon("palette"), "Affichage", style = "color: #f39c12; font-weight: bold;"),
                                   sliderInput("missingAxisTextSize", "Taille texte axes:", 
                                               min = 6, max = 16, value = 10, ticks = FALSE),
                                   checkboxInput("missingRotateLabels", 
                                                 tagList(icon("sync-alt"), " Incliner labels X"), 
                                                 value = TRUE)
                            ),
                            column(4,
                                   h5(icon("heading"), "Personnalisation", style = "color: #f39c12; font-weight: bold;"),
                                   textInput("missingTitle", "Titre personnalisé:", 
                                             placeholder = "Laisser vide pour titre auto"),
                                   checkboxInput("missingCenterTitle", 
                                                 tagList(icon("align-center"), " Centrer le titre"), 
                                                 value = TRUE),
                                   numericInput("missingDPI", tagList(icon("image"), " DPI export:"), 
                                                value = 300, min = 72, max = 600, step = 50)
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  hr(),
                  
                  div(
                    style = "text-align: center; margin: 15px 0;",
                    downloadButton("downloadMissingPlot", 
                                   tagList(icon("download"), " Télécharger PNG"), 
                                   class = "btn-info")
                  ),
                  
                  div(
                    style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    withSpinner(
                      plotOutput("missingPlot", height = "400px"),
                      type = 6,
                      color = "#f39c12"
                    )
                  ),
                  
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("info-circle"), 
                    " Identifiez les variables nécessitant un traitement"
                  )
                )
              )
      ),
      # ---- Nettoyage ----
      
      tabItem(tabName = "clean",
              # En-tête de section
              fluidRow(
                box(
                  width = 12,
                  status = "warning",
                  solidHeader = FALSE,
                  background = "yellow",
                  h3(icon("broom"), "Nettoyage et Préparation des Données", style = "margin: 0;"),
                  p("Transformez, nettoyez et préparez vos données pour l'analyse", 
                    style = "margin: 5px 0 0 0; opacity: 0.9;")
                )
              ),
              
              # Étape 1: Types des variables
              fluidRow(
                box(
                  title = tagList(
                    tags$span(
                      class = "badge bg-yellow",
                      style = "font-size: 14px; margin-right: 10px;",
                      "1"
                    ),
                    icon("cogs"), 
                    "Définition des Types de Variables"
                  ), 
                  status = "warning", 
                  width = 12, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  
                  fluidRow(
                    column(8,
                           div(
                             style = "background-color: #fff8e1; padding: 15px; border-radius: 5px; border-left: 4px solid #f39c12;",
                             h4(icon("info-circle"), "Instructions", style = "margin-top: 0; color: #f39c12;"),
                             p(
                               style = "margin-bottom: 0;",
                               "Sélectionnez le type approprié pour chaque variable. ",
                               "Les conversions sont appliquées automatiquement."
                             ),
                             tags$ul(
                               style = "margin-top: 10px; margin-bottom: 0;",
                               tags$li(strong("Numérique:"), " Pour les valeurs continues (ex: âge, prix)"),
                               tags$li(strong("Facteur:"), " Pour les catégories (ex: genre, région)"),
                               tags$li(strong("Texte:"), " Pour du texte libre"),
                               tags$li(strong("Date:"), " Pour les dates (format: YYYY-MM-DD)")
                             )
                           )
                    ),
                    column(4,
                           div(
                             style = "text-align: center; padding: 30px;",
                             actionButton(
                               "applyTypes", 
                               tagList(icon("check-circle"), " Appliquer les Types"), 
                               class = "btn-warning btn-lg",
                               style = "font-size: 18px; padding: 15px 30px;"
                             ),
                             br(), br(),
                             div(
                               style = "font-size: 12px; color: #7f8c8d;",
                               icon("clock"), " Les modifications sont appliquées instantanément"
                             )
                           )
                    )
                  ),
                  
                  hr(),
                  
                  div(
                    style = "max-height: 400px; overflow-y: auto; background-color: #fafafa; padding: 15px; border-radius: 5px;",
                    uiOutput("varTypeUI")
                  )
                )
              ),
              
              # Étape 2: Gestion des variables
              fluidRow(
                # Supprimer/Ajouter variables
                box(
                  title = tagList(
                    tags$span(
                      class = "badge bg-red",
                      style = "font-size: 14px; margin-right: 10px;",
                      "2"
                    ),
                    icon("edit"), 
                    "Gestion des Variables"
                  ), 
                  status = "danger", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  # Supprimer une variable
                  div(
                    style = "background-color: #ffebee; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                    h5(icon("trash-alt"), "Supprimer une Variable", style = "color: #e74c3c; margin-top: 0;"),
                    uiOutput("removeVarUI"),
                    actionButton(
                      "removeVar", 
                      tagList(icon("trash"), " Supprimer"), 
                      class = "btn-danger btn-block",
                      style = "margin-top: 10px;"
                    )
                  ),
                  
                  # Ajouter une constante
                  div(
                    style = "background-color: #e8f5e9; padding: 15px; border-radius: 5px;",
                    h5(icon("plus-circle"), "Ajouter une Variable Constante", style = "color: #27ae60; margin-top: 0;"),
                    p(style = "font-size: 12px; color: #7f8c8d;", 
                      "Créez une nouvelle colonne avec une valeur identique pour toutes les lignes"),
                    textInput(
                      "newVarName", 
                      "Nom de la nouvelle variable:",
                      placeholder = "ex: Categorie"
                    ),
                    numericInput(
                      "newVarValue", 
                      "Valeur par défaut:", 
                      0
                    ),
                    actionButton(
                      "addVar", 
                      tagList(icon("plus"), " Ajouter Constante"), 
                      class = "btn-success btn-block"
                    )
                  )
                ),
                
                # Variables calculées
                box(
                  title = tagList(
                    tags$span(
                      class = "badge bg-blue",
                      style = "font-size: 14px; margin-right: 10px;",
                      "3"
                    ),
                    icon("calculator"), 
                    "Créer une Variable Calculée"
                  ), 
                  status = "primary", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  div(
                    style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px;",
                    
                    h5(icon("info-circle"), "Assistant de Formule", style = "color: #3498db; margin-top: 0;"),
                    p(style = "font-size: 12px; color: #7f8c8d;", 
                      "Créez des variables basées sur des calculs. Exemple: (Var1 + Var2) / 2"),
                    
                    textInput(
                      "calcVarName", 
                      "Nom de la variable calculée:",
                      placeholder = "ex: Moyenne_Score"
                    ),
                    
                    fluidRow(
                      column(6,
                             uiOutput("colPicker")
                      ),
                      column(6,
                             h6("Opérateurs:", style = "margin-bottom: 5px;"),
                             div(
                               style = "display: flex; flex-wrap: wrap; gap: 5px;",
                               actionButton("insertPlus", "+", class = "btn-info btn-sm", style = "flex: 1;"),
                               actionButton("insertMoins", "-", class = "btn-info btn-sm", style = "flex: 1;"),
                               actionButton("insertMult", "×", class = "btn-info btn-sm", style = "flex: 1;"),
                               actionButton("insertDiv", "÷", class = "btn-info btn-sm", style = "flex: 1;")
                             ),
                             br(),
                             h6("Fonctions:", style = "margin-bottom: 5px;"),
                             div(
                               style = "display: flex; flex-wrap: wrap; gap: 5px;",
                               actionButton("insertLog", "log()", class = "btn-info btn-sm", style = "flex: 1;"),
                               actionButton("insertSqrt", "√()", class = "btn-info btn-sm", style = "flex: 1;")
                             )
                      )
                    ),
                    
                    textInput(
                      "calcFormula", 
                      "Formule de calcul:",
                      placeholder = "ex: log(Var1) ou (Var1 + Var2) / 2"
                    ),
                    
                    actionButton(
                      "addCalcVar", 
                      tagList(icon("calculator"), " Créer Variable Calculée"), 
                      class = "btn-primary btn-block btn-lg"
                    )
                  ),
                  
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("lightbulb"), 
                    " Astuce: Cliquez sur une colonne pour l'insérer dans la formule"
                  )
                )
              ),
              
              # Étape 4: Valeurs manquantes
              fluidRow(
                box(
                  title = tagList(
                    tags$span(
                      class = "badge bg-aqua",
                      style = "font-size: 14px; margin-right: 10px;",
                      "4"
                    ),
                    icon("band-aid"), 
                    "Traitement des Valeurs Manquantes"
                  ), 
                  status = "info", 
                  width = 12, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  fluidRow(
                    column(8,
                           div(
                             style = "background-color: #e1f5fe; padding: 15px; border-radius: 5px;",
                             
                             uiOutput("naVarSelect"),
                             
                             h5(icon("tools"), "Méthode de Traitement", style = "color: #3498db; margin-top: 20px;"),
                             
                             radioButtons(
                               "naMethod", 
                               NULL,
                               choices = c(
                                 "Supprimer les lignes contenant des NA" = "remove", 
                                 "Remplacer par la moyenne (variables numériques)" = "mean",
                                 "Remplacer par la médiane (variables numériques)" = "median",
                                 "Remplacer par une valeur spécifique" = "value"
                               ),
                               selected = "remove"
                             ),
                             
                             conditionalPanel(
                               condition = "input.naMethod == 'value'",
                               div(
                                 style = "margin-top: 15px; padding: 10px; background-color: white; border-radius: 5px;",
                                 numericInput(
                                   "naValue", 
                                   "Valeur de remplacement:", 
                                   0
                                 )
                               )
                             )
                           )
                    ),
                    column(4,
                           div(
                             style = "background-color: #fff; padding: 20px; border-radius: 5px; border: 2px solid #3498db;",
                             h5(icon("exclamation-triangle"), "Attention", style = "color: #e74c3c; margin-top: 0;"),
                             tags$ul(
                               style = "font-size: 13px; color: #7f8c8d;",
                               tags$li("La suppression de lignes peut réduire votre jeu de données"),
                               tags$li("La moyenne est sensible aux valeurs extrêmes"),
                               tags$li("La médiane est plus robuste aux outliers"),
                               tags$li("Vérifiez la pertinence de votre choix")
                             ),
                             hr(),
                             actionButton(
                               "applyNA", 
                               tagList(icon("magic"), " Appliquer le Traitement"), 
                               class = "btn-info btn-block btn-lg",
                               style = "margin-top: 20px;"
                             )
                           )
                    )
                  )
                )
              ),
              
              # Aperçu des données nettoyées
              fluidRow(
                box(
                  title = tagList(
                    icon("table"), 
                    "Aperçu des Données Nettoyées"
                  ), 
                  status = "success", 
                  width = 12, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  div(
                    style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                    icon("check-circle"), 
                    strong(" Les modifications sont appliquées en temps réel. "),
                    "Vous pouvez continuer vers l'étape suivante."
                  ),
                  
                  withSpinner(
                    DTOutput("cleanedData"),
                    type = 6,
                    color = "#27ae60"
                  ),
                  
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("info-circle"), 
                    " Vérifiez vos données avant de passer à l'analyse ou à la modélisation"
                  )
                )
              )
      ),
      
      # ---- Filtrage ----
      tabItem(tabName = "filter",
              fluidRow(
                box(title = "Filtre croisement complet (2 facteurs)", status = "primary", width = 6, solidHeader = TRUE,
                    uiOutput("filterFactorA"),
                    uiOutput("filterFactorB"),
                    checkboxInput("requireA", "Garder niveaux de A présents pour tous les niveaux de B", TRUE),
                    checkboxInput("requireB", "Garder niveaux de B présents pour tous les niveaux de A", FALSE),
                    actionButton("applyCrossFilter", "Appliquer (2 facteurs)", class = "btn-primary", icon = icon("filter"))
                ),
                box(title = "Filtre croisement complet (N facteurs)", status = "primary", width = 6, solidHeader = TRUE,
                    uiOutput("filterFactorsN"),
                    helpText("Garde uniquement les niveaux qui forment un croisement complet entre tous les facteurs sélectionnés."),
                    actionButton("applyCrossFilterN", "Appliquer (N facteurs)", class = "btn-primary", icon = icon("filter")),
                    actionButton("resetFilter", "Réinitialiser", class = "btn-warning", icon = icon("redo")),
                    hr(),
                    h5("Résumé :"),
                    verbatimTextOutput("filterSummary")
                )
              ),
              fluidRow(
                valueBoxOutput("originalRows", width = 3),
                valueBoxOutput("filteredRows", width = 3),
                valueBoxOutput("removedRows", width = 3),
                valueBoxOutput("completeCases", width = 3)
              ),
              fluidRow(
                box(title = "Données filtrées", status = "info", width = 12, solidHeader = TRUE, 
                    DTOutput("filteredData"))
              )
      ),
      # ---- Analyse descriptives ----
      tabItem(tabName = "descriptive",
              fluidRow(
                box(title = tags$span(icon("chart-bar"), " Sélection des Variables"), 
                    status = "success", width = 4, solidHeader = TRUE,
                    tags$div(
                      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 15px; border-radius: 8px; margin-bottom: 15px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                      tags$h5(icon("lightbulb"), " Guide rapide", style = "margin: 0 0 10px 0; font-weight: bold;"),
                      tags$p("1. Sélectionnez vos variables numériques", style = "margin: 5px 0; font-size: 13px;"),
                      tags$p("2. Choisissez les statistiques à calculer", style = "margin: 5px 0; font-size: 13px;"),
                      tags$p("3. Optionnel: grouper par facteurs", style = "margin: 5px 0; font-size: 13px;"),
                      tags$p("4. Cliquez sur 'Calculer' et visualisez!", style = "margin: 5px 0; font-size: 13px;")
                    ),
                    uiOutput("numVarSelect"),
                    checkboxInput("selectAllNum", 
                                  HTML("<span style='font-weight: 600;'><i class='fa fa-check-square'></i> Sélectionner toutes les variables numériques</span>"), 
                                  FALSE),
                    hr(style = "border-color: #ddd; margin: 20px 0;"),
                    tags$div(
                      style = "background-color: #f8f9fa; padding: 12px; border-radius: 6px; border-left: 4px solid #28a745;",
                      tags$h6(icon("calculator"), " Statistiques à calculer", 
                              style = "font-weight: bold; color: #155724; margin-bottom: 12px;"),
                      checkboxGroupInput("descStats", NULL,
                                         choices = list(
                                           "Moyenne" = "mean", 
                                           "Médiane" = "median", 
                                           "Écart-type" = "sd", 
                                           "Variance" = "var", 
                                           "CV (%)" = "cv", 
                                           "Minimum" = "min", 
                                           "Maximum" = "max", 
                                           "1er Quartile" = "q1", 
                                           "3ème Quartile" = "q3"
                                         ),
                                         selected = c("mean", "median", "sd", "cv")
                      )
                    ),
                    hr(style = "border-color: #ddd; margin: 20px 0;"),
                    uiOutput("descFactorUI"),
                    br(),
                    actionButton("calcDesc", 
                                 HTML("<i class='fa fa-calculator'></i> Calculer les statistiques"), 
                                 class = "btn-success btn-block btn-lg", 
                                 style = "font-weight: bold; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: all 0.3s;")
                ),
                box(title = tags$span(icon("table"), " Résultats des Analyses Descriptives"), 
                    status = "success", width = 8, solidHeader = TRUE,
                    tags$div(
                      style = "background-color: #e8f5e9; padding: 12px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #4caf50;",
                      tags$p(icon("info-circle"), " Les résultats s'afficheront ici après le calcul. Vous pouvez trier, filtrer et rechercher dans le tableau.",
                             style = "margin: 0; color: #2e7d32; font-size: 14px;")
                    ),
                    DTOutput("descResults"),
                    br(),
                    fluidRow(
                      column(6,
                             downloadButton("downloadDesc", 
                                            HTML("<i class='fa fa-file-csv'></i> Télécharger CSV"), 
                                            class = "btn-info btn-block", 
                                            style = "font-weight: 600; padding: 10px;")
                      ),
                      column(6,
                             downloadButton("downloadDescExcel", 
                                            HTML("<i class='fa fa-file-excel'></i> Télécharger Excel"), 
                                            class = "btn-success btn-block", 
                                            style = "font-weight: 600; padding: 10px;")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = tags$span(icon("chart-line"), " Visualisation des Données"), 
                    status = "info", width = 12, solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(
                      column(4,
                             tags$div(
                               style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); padding: 15px; border-radius: 8px; margin-bottom: 15px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                               tags$h5(icon("chart-pie"), " Configuration du graphique", 
                                       style = "margin: 0; font-weight: bold; text-align: center;")
                             ),
                             uiOutput("descPlotVarSelect"),
                             uiOutput("descPlotFactorSelect"),
                             hr(style = "border-color: #ddd;"),
                             
                             # OPTIONS BOXPLOT (SIMPLIFIÉES - SANS BARRE Q3)
                             conditionalPanel(
                               condition = "input.descPlotFactor != 'Aucun'",
                               tags$div(
                                 style = "background-color: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                                 h6(icon("sliders-h"), " Options du Boxplot", 
                                    style = "color: #856404; font-weight: bold; margin-bottom: 12px;"),
                                 checkboxInput("descPlotShowValues", 
                                               HTML("<span style='font-size: 14px;'><i class='fa fa-list-ol'></i> Afficher les valeurs extrêmes (min/max)</span>"), 
                                               value = FALSE)
                               ),
                               hr(style = "border-color: #ddd;")
                             ),
                             
                             tags$div(
                               style = "background-color: #f3e5f5; padding: 15px; border-radius: 8px; border-left: 4px solid #9c27b0; margin-bottom: 15px;",
                               h6(icon("palette"), " Palette de couleurs", 
                                  style = "color: #6a1b9a; font-weight: bold; margin-bottom: 12px;"),
                               selectInput("descPlotColorPalette", NULL,
                                           choices = list(
                                             "Par défaut (ggplot2)" = "ggplot2",
                                             "--- Palettes RColorBrewer ---" = "",
                                             "Set1 (Multicolore vif)" = "Set1",
                                             "Set2 (Multicolore doux)" = "Set2",
                                             "Pastel1 (Pastel)" = "Pastel1",
                                             "Dark2 (Foncé)" = "Dark2",
                                             "--- Palette Viridis ---" = "",
                                             "Viridis (Accessible)" = "viridis",
                                             "--- Monochromatique ---" = "",
                                             "Bleu" = "mono_blue",
                                             "Vert" = "mono_green",
                                             "Rouge" = "mono_red",
                                             "Violet" = "mono_purple",
                                             "Orange" = "mono_orange",
                                             "Noir" = "mono_black"
                                           ),
                                           selected = "ggplot2",
                                           width = "100%")
                             ),
                             
                             # DIMENSIONS DU GRAPHIQUE
                             tags$div(
                               style = "background-color: #e3f2fd; padding: 15px; border-radius: 8px; border-left: 4px solid #2196f3; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                               h6(icon("expand-arrows-alt"), " Dimensions du Graphique", 
                                  style = "color: #1565c0; font-weight: bold; margin-bottom: 12px;"),
                               sliderInput("descPlotWidth", "Largeur (pixels):", 
                                           min = 400, max = 2000, value = 900, step = 50, width = "100%"),
                               sliderInput("descPlotHeight", "Hauteur (pixels):", 
                                           min = 400, max = 2000, value = 600, step = 50, width = "100%")
                             ),
                             
                             # TÉLÉCHARGEMENT DU GRAPHIQUE
                             tags$div(
                               style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15);",
                               h6(icon("download"), " Télécharger le Graphique", 
                                  style = "color: white; font-weight: bold; margin-bottom: 15px; text-align: center; font-size: 16px;"),
                               sliderInput("descPlotDPI", "Résolution (DPI):", 
                                           min = 300, max = 600, value = 300, step = 50, width = "100%"),
                               tags$div(
                                 style = "background-color: rgba(255,255,255,0.2); padding: 10px; border-radius: 6px; margin-bottom: 10px;",
                                 tags$p(icon("info-circle"), " Formats bitmap (avec pertes)", 
                                        style = "color: white; font-weight: bold; margin: 5px 0; font-size: 13px;")
                               ),
                               downloadButton("downloadDescPlotPNG", 
                                              HTML("<i class='fa fa-image'></i> PNG (recommandé)"), 
                                              class = "btn-light btn-block", 
                                              style = "font-weight: 600; margin-bottom: 8px;"),
                               downloadButton("downloadDescPlotJPEG", 
                                              HTML("<i class='fa fa-image'></i> JPEG"), 
                                              class = "btn-light btn-block", 
                                              style = "font-weight: 600; margin-bottom: 8px;"),
                               downloadButton("downloadDescPlotTIFF", 
                                              HTML("<i class='fa fa-image'></i> TIFF (haute qualité)"), 
                                              class = "btn-light btn-block", 
                                              style = "font-weight: 600; margin-bottom: 15px;"),
                               tags$div(
                                 style = "background-color: rgba(255,255,255,0.2); padding: 10px; border-radius: 6px; margin-bottom: 10px;",
                                 tags$p(icon("vector-square"), " Formats vectoriels (sans pertes)", 
                                        style = "color: white; font-weight: bold; margin: 5px 0; font-size: 13px;")
                               ),
                               downloadButton("downloadDescPlotPDF", 
                                              HTML("<i class='fa fa-file-pdf'></i> PDF"), 
                                              class = "btn-light btn-block", 
                                              style = "font-weight: 600; margin-bottom: 8px;"),
                               downloadButton("downloadDescPlotSVG", 
                                              HTML("<i class='fa fa-file-code'></i> SVG (web)"), 
                                              class = "btn-light btn-block", 
                                              style = "font-weight: 600; margin-bottom: 8px;"),
                               downloadButton("downloadDescPlotEPS", 
                                              HTML("<i class='fa fa-file'></i> EPS (publication)"), 
                                              class = "btn-light btn-block", 
                                              style = "font-weight: 600;")
                             ),
                             br(),
                             tags$div(
                               style = "background-color: #e8f5e9; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50;",
                               h6(icon("lightbulb"), " Conseils", style = "color: #2e7d32; font-weight: bold; margin-bottom: 10px;"),
                               tags$ul(
                                 style = "font-size: 13px; color: #424242; margin: 0; padding-left: 20px;",
                                 tags$li("PNG: Idéal pour le web et les présentations", style = "margin-bottom: 5px;"),
                                 tags$li("PDF/SVG: Parfaits pour l'impression et publications", style = "margin-bottom: 5px;"),
                                 tags$li("TIFF: Pour l'archivage haute qualité", style = "margin-bottom: 5px;"),
                                 tags$li("Augmentez le DPI pour une meilleure qualité d'impression")
                               )
                             )
                      ),
                      column(8,
                             wellPanel(
                               style = "background: linear-gradient(to right, #f8f9fa 0%, #e9ecef 100%); border-left: 5px solid #3498db; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                               h5(icon("paint-brush"), " Personnalisation du Graphique", 
                                  style = "font-weight: bold; color: #2c3e50; margin-bottom: 20px; text-align: center; font-size: 18px;"),
                               
                               # TITRE DU GRAPHIQUE
                               tags$div(
                                 style = "background-color: white; padding: 20px; border-radius: 10px; margin-bottom: 15px; box-shadow: 0 3px 6px rgba(0,0,0,0.08); border-top: 3px solid #3498db;",
                                 h6(icon("heading"), " Titre du graphique", 
                                    style = "font-weight: bold; color: #2980b9; margin-bottom: 15px; font-size: 15px;"),
                                 textInput("descPlotTitle", NULL, 
                                           placeholder = "Ex: Distribution des rendements par traitement...",
                                           width = "100%"),
                                 fluidRow(
                                   column(4,
                                          checkboxInput("descPlotCenterTitle", 
                                                        HTML("<span style='font-size: 14px;'><i class='fa fa-align-center'></i> Centrer</span>"), 
                                                        value = TRUE)
                                   ),
                                   column(4,
                                          checkboxInput("descPlotTitleBold", 
                                                        HTML("<span style='font-size: 14px;'><strong>Gras</strong></span>"), 
                                                        value = TRUE)
                                   ),
                                   column(4,
                                          checkboxInput("descPlotTitleItalic", 
                                                        HTML("<span style='font-size: 14px;'><em>Italique</em></span>"), 
                                                        value = FALSE)
                                   )
                                 )
                               ),
                               
                               # LABELS DES AXES
                               tags$div(
                                 style = "background-color: white; padding: 20px; border-radius: 10px; margin-bottom: 15px; box-shadow: 0 3px 6px rgba(0,0,0,0.08); border-top: 3px solid #27ae60;",
                                 h6(icon("arrows-alt-h"), " Labels des axes", 
                                    style = "font-weight: bold; color: #27ae60; margin-bottom: 15px; font-size: 15px;"),
                                 fluidRow(
                                   column(6,
                                          tags$div(
                                            style = "background: linear-gradient(to bottom, #ebf8ff 0%, #bee3f8 100%); padding: 15px; border-radius: 8px; border-left: 4px solid #3498db;",
                                            h6(icon("arrows-alt-h"), " Axe X", 
                                               style = "color: #2c5aa0; font-weight: bold; margin-bottom: 12px;"),
                                            textInput("descPlotXLabel", NULL, 
                                                      placeholder = "Label horizontal...",
                                                      width = "100%"),
                                            fluidRow(
                                              column(6,
                                                     checkboxInput("descPlotXBold", 
                                                                   HTML("<small><strong>Gras</strong></small>"), 
                                                                   value = FALSE)
                                              ),
                                              column(6,
                                                     checkboxInput("descPlotXItalic", 
                                                                   HTML("<small><em>Italique</em></small>"), 
                                                                   value = FALSE)
                                              )
                                            )
                                          )
                                   ),
                                   column(6,
                                          tags$div(
                                            style = "background: linear-gradient(to bottom, #f0fff4 0%, #c6f6d5 100%); padding: 15px; border-radius: 8px; border-left: 4px solid #27ae60;",
                                            h6(icon("arrows-alt-v"), " Axe Y", 
                                               style = "color: #22863a; font-weight: bold; margin-bottom: 12px;"),
                                            textInput("descPlotYLabel", NULL, 
                                                      placeholder = "Label vertical...",
                                                      width = "100%"),
                                            fluidRow(
                                              column(6,
                                                     checkboxInput("descPlotYBold", 
                                                                   HTML("<small><strong>Gras</strong></small>"), 
                                                                   value = FALSE)
                                              ),
                                              column(6,
                                                     checkboxInput("descPlotYItalic", 
                                                                   HTML("<small><em>Italique</em></small>"), 
                                                                   value = FALSE)
                                              )
                                            )
                                          )
                                   )
                                 )
                               ),
                               
                               # GRADUATIONS DES AXES
                               tags$div(
                                 style = "background-color: white; padding: 20px; border-radius: 10px; box-shadow: 0 3px 6px rgba(0,0,0,0.08); border-top: 3px solid #e74c3c;",
                                 h6(icon("ruler-horizontal"), " Graduations des axes", 
                                    style = "font-weight: bold; color: #e74c3c; margin-bottom: 15px; font-size: 15px;"),
                                 fluidRow(
                                   column(6,
                                          tags$div(
                                            style = "background-color: #f0f8ff; padding: 15px; border-radius: 8px; border: 2px solid #b3d9ff;",
                                            h6(icon("long-arrow-alt-right"), " Axe X", 
                                               style = "color: #3498db; font-weight: bold; margin-bottom: 12px; text-align: center;"),
                                            sliderInput("descPlotXAngle", "Angle d'inclinaison:", 
                                                        min = 0, max = 90, value = 0, step = 15,
                                                        post = "°", width = "100%"),
                                            tags$hr(style = "margin: 15px 0; border-color: #b3d9ff;"),
                                            h6("Style du texte:", style = "font-size: 13px; color: #555; margin-bottom: 10px; font-weight: bold;"),
                                            fluidRow(
                                              column(6,
                                                     checkboxInput("descPlotXTickBold", 
                                                                   HTML("<small><strong>Gras</strong></small>"), 
                                                                   value = FALSE)
                                              ),
                                              column(6,
                                                     checkboxInput("descPlotXTickItalic", 
                                                                   HTML("<small><em>Italique</em></small>"), 
                                                                   value = FALSE)
                                              )
                                            )
                                          )
                                   ),
                                   column(6,
                                          tags$div(
                                            style = "background-color: #f0fff4; padding: 15px; border-radius: 8px; border: 2px solid #b3e6cc;",
                                            h6(icon("long-arrow-alt-up"), " Axe Y", 
                                               style = "color: #27ae60; font-weight: bold; margin-bottom: 12px; text-align: center;"),
                                            tags$div(
                                              style = "height: 78px; display: flex; align-items: center; justify-content: center; background-color: rgba(39,174,96,0.1); border-radius: 6px; color: #27ae60; font-size: 13px; font-style: italic; padding: 10px; text-align: center;", 
                                              "Les graduations Y restent toujours horizontales"
                                            ),
                                            tags$hr(style = "margin: 15px 0; border-color: #b3e6cc;"),
                                            h6("Style du texte:", style = "font-size: 13px; color: #555; margin-bottom: 10px; font-weight: bold;"),
                                            fluidRow(
                                              column(6,
                                                     checkboxInput("descPlotYTickBold", 
                                                                   HTML("<small><strong>Gras</strong></small>"), 
                                                                   value = FALSE)
                                              ),
                                              column(6,
                                                     checkboxInput("descPlotYTickItalic", 
                                                                   HTML("<small><em>Italique</em></small>"), 
                                                                   value = FALSE)
                                              )
                                            )
                                          )
                                   )
                                 )
                               )
                             )
                      )
                    ),
                    hr(style = "border-color: #ccc; margin: 30px 0;"),
                    tags$div(
                      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 15px; border-radius: 10px; margin-bottom: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.15);",
                      h4(icon("chart-area"), " Aperçu du graphique", 
                         style = "color: white; margin: 0; text-align: center; font-weight: bold;")
                    ),
                    uiOutput("descPlotOutput")
                )
              )
      ),
      
      # ---- Tableaux croisés dynamiques  ----
      tabItem(tabName = "crosstab",
              
              # En-tête avec instructions
              fluidRow(
                box(title = tagList(icon("info-circle"), "Guide d'utilisation"), 
                    status = "info", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    div(style = "padding: 15px;",
                        tags$ol(
                          tags$li(tags$strong("Sélectionnez"), " vos variables en lignes et colonnes"),
                          tags$li(tags$strong("Choisissez"), " les analyses statistiques souhaitées"),
                          tags$li(tags$strong("Cliquez"), " sur 'Générer l'analyse complète'"),
                          tags$li(tags$strong("Explorez"), " les résultats dans les différents onglets"),
                          tags$li(tags$strong("Personnalisez"), " vos graphiques si nécessaire"),
                          tags$li(tags$strong("Téléchargez"), " vos tableaux et graphiques")
                        )
                    )
                )
              ),
              
              # Section Configuration
              fluidRow(
                box(title = tagList(icon("cogs"), "Configuration de l'analyse"), 
                    status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,
                    
                    # Sélection des variables
                    div(class = "form-group",
                        tags$label("Sélection des variables", 
                                   style = "font-weight: bold; color: #2c3e50; font-size: 15px;"),
                        uiOutput("crosstabRowVarSelect"),
                        uiOutput("crosstabColVarSelect"),
                        uiOutput("crosstabFilterVarSelect")
                    ),
                    
                    hr(style = "border-top: 2px solid #3498db; margin: 20px 0;"),
                    
                    # Options d'analyse
                    div(class = "well well-sm",
                        tags$h5(
                          icon("chart-bar"), 
                          "Options d'analyse statistique",
                          style = "font-weight: bold; color: #337ab7; margin-top: 5px;"
                        ),
                        checkboxGroupInput("analysisOptions", NULL,
                                           choices = list(
                                             "Proportions en lignes (%)" = "row_prop",
                                             "Proportions en colonnes (%)" = "col_prop", 
                                             "Proportions totales (%)" = "total_prop",
                                             "Test du Chi-deux" = "chi_test",
                                             "Test exact de Fisher" = "fisher_test",
                                             "Résidus standardisés" = "residuals"
                                           ),
                                           selected = c("row_prop", "col_prop", "chi_test")),
                        tags$small(
                          class = "text-muted",
                          icon("lightbulb"), " Note: Les résidus nécessitent le test du Chi-deux"
                        )
                    ),
                    
                    hr(style = "border-top: 2px solid #27ae60; margin: 20px 0;"),
                    
                    # Type de graphique
                    div(class = "well well-sm",
                        tags$h5(
                          icon("paint-brush"), 
                          "Type de représentation graphique",
                          style = "font-weight: bold; color: #27ae60; margin-top: 5px;"
                        ),
                        radioButtons("plotType", NULL,
                                     choices = c("Histogramme groupé" = "bar",
                                                 "Histogramme empilé" = "stacked_bar",
                                                 "Graphique mosaïque (proportions)" = "mosaic"),
                                     selected = "bar"),
                        selectInput("colorPalette", "Palette de couleurs :",
                                    choices = c(
                                      "Par défaut" = "ggplot_default",
                                      "Noir" = "black",
                                      "Viridis" = "viridis",
                                      "Plasma" = "plasma",
                                      "Inferno" = "inferno",
                                      "Magma" = "magma",
                                      "Set1" = "Set1",
                                      "Set2" = "Set2",
                                      "Pastel" = "Pastel1",
                                      "Spectral" = "Spectral",
                                      "Niveaux de gris" = "grey"
                                    ),
                                    selected = "ggplot_default",
                                    width = "100%")
                    ),
                    
                    hr(style = "border-top: 2px solid #e74c3c; margin: 20px 0;"),
                    
                    # Bouton de génération
                    actionButton("generateCrosstab", 
                                 "Générer l'analyse complète", 
                                 class = "btn-primary btn-lg btn-block", 
                                 icon = icon("play-circle"),
                                 style = "font-weight: bold; font-size: 16px; padding: 15px; margin-top: 10px; margin-bottom: 10px; background: linear-gradient(to right, #3498db, #2980b9); box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
                ),
                
                # Section Résultats
                box(title = tagList(icon("table"), "Résultats de l'analyse croisée"), 
                    status = "primary", width = 8, solidHeader = TRUE, collapsible = TRUE,
                    
                    tabBox(
                      title = NULL, id = "crosstabTabs", width = 12,
                      
                      # Onglet Effectifs
                      tabPanel(
                        tagList(icon("table"), "Effectifs"), 
                        value = "tab_effectifs",
                        br(),
                        DTOutput("crosstabTable"),
                        br(),
                        div(class = "text-center",
                            fluidRow(
                              column(6,
                                     downloadButton("downloadCrosstabExcel", 
                                                    "Télécharger Excel", 
                                                    class = "btn-success btn-lg", 
                                                    icon = icon("file-excel"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;")),
                              column(6,
                                     downloadButton("downloadCrosstabCSV", 
                                                    "Télécharger CSV", 
                                                    class = "btn-info btn-lg", 
                                                    icon = icon("file-csv"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;"))
                            )
                        )
                      ),
                      
                      # Onglet Proportions lignes
                      tabPanel(
                        tagList(icon("percentage"), "Proportions lignes"), 
                        value = "tab_prop_lignes",
                        br(),
                        div(class = "alert alert-info",
                            icon("info-circle"), 
                            tags$strong(" Interprétation:"),
                            " Pourcentages calculés par rapport au total de chaque ligne (somme = 100% par ligne)"
                        ),
                        DTOutput("crosstabRowProp"),
                        br(),
                        div(class = "text-center",
                            fluidRow(
                              column(6,
                                     downloadButton("downloadRowPropExcel", 
                                                    "Télécharger Excel", 
                                                    class = "btn-success btn-lg", 
                                                    icon = icon("file-excel"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;")),
                              column(6,
                                     downloadButton("downloadRowPropCSV", 
                                                    "Télécharger CSV", 
                                                    class = "btn-info btn-lg", 
                                                    icon = icon("file-csv"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;"))
                            )
                        )
                      ),
                      
                      # Onglet Proportions colonnes
                      tabPanel(
                        tagList(icon("percentage"), "Proportions colonnes"), 
                        value = "tab_prop_colonnes",
                        br(),
                        div(class = "alert alert-info",
                            icon("info-circle"), 
                            tags$strong(" Interprétation:"),
                            " Pourcentages calculés par rapport au total de chaque colonne (somme = 100% par colonne)"
                        ),
                        DTOutput("crosstabColProp"),
                        br(),
                        div(class = "text-center",
                            fluidRow(
                              column(6,
                                     downloadButton("downloadColPropExcel", 
                                                    "Télécharger Excel", 
                                                    class = "btn-success btn-lg", 
                                                    icon = icon("file-excel"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;")),
                              column(6,
                                     downloadButton("downloadColPropCSV", 
                                                    "Télécharger CSV", 
                                                    class = "btn-info btn-lg", 
                                                    icon = icon("file-csv"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;"))
                            )
                        )
                      ),
                      
                      # Onglet Proportions totales
                      tabPanel(
                        tagList(icon("percentage"), "Proportions totales"), 
                        value = "tab_prop_totales",
                        br(),
                        div(class = "alert alert-info",
                            icon("info-circle"), 
                            tags$strong(" Interprétation:"),
                            " Pourcentages calculés par rapport au total général (somme totale = 100%)"
                        ),
                        DTOutput("crosstabTotalProp"),
                        br(),
                        div(class = "text-center",
                            fluidRow(
                              column(6,
                                     downloadButton("downloadTotalPropExcel", 
                                                    "Télécharger Excel", 
                                                    class = "btn-success btn-lg", 
                                                    icon = icon("file-excel"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;")),
                              column(6,
                                     downloadButton("downloadTotalPropCSV", 
                                                    "Télécharger CSV", 
                                                    class = "btn-info btn-lg", 
                                                    icon = icon("file-csv"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;"))
                            )
                        )
                      ),
                      
                      # Onglet Tests statistiques
                      tabPanel(
                        tagList(icon("calculator"), "Tests statistiques"), 
                        value = "tab_tests",
                        br(),
                        div(class = "alert alert-success",
                            icon("check-circle"), 
                            tags$strong(" Objectif:"),
                            " Tester l'existence d'une association significative entre les deux variables"
                        ),
                        div(class = "well",
                            style = "background-color: #f8f9fa; border-left: 4px solid #3498db; padding: 20px;",
                            verbatimTextOutput("crosstabTests")
                        ),
                        br(),
                        div(class = "text-center",
                            fluidRow(
                              column(6,
                                     downloadButton("downloadTestsExcel", 
                                                    "Télécharger Excel", 
                                                    class = "btn-success btn-lg", 
                                                    icon = icon("file-excel"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;")),
                              column(6,
                                     downloadButton("downloadTestsCSV", 
                                                    "Télécharger CSV", 
                                                    class = "btn-info btn-lg", 
                                                    icon = icon("file-csv"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;"))
                            )
                        )
                      ),
                      
                      # Onglet Résidus
                      tabPanel(
                        tagList(icon("chart-area"), "Résidus standardisés"), 
                        value = "tab_residus",
                        br(),
                        div(class = "alert alert-info",
                            icon("info-circle"),
                            tags$strong(" Interprétation: "),
                            "Les résidus standardisés mesurent l'écart à l'indépendance. ",
                            tags$ul(
                              tags$li(tags$strong("| valeur | > 2:"), " Contribution importante (significative)"),
                              tags$li(tags$strong("Valeur positive:"), " Sur-représentation (plus que prévu)"),
                              tags$li(tags$strong("Valeur négative:"), " Sous-représentation (moins que prévu)")
                            )
                        ),
                        DTOutput("crosstabResiduals"),
                        br(),
                        div(class = "text-center",
                            fluidRow(
                              column(6,
                                     downloadButton("downloadResidualsExcel", 
                                                    "Télécharger Excel", 
                                                    class = "btn-success btn-lg", 
                                                    icon = icon("file-excel"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;")),
                              column(6,
                                     downloadButton("downloadResidualsCSV", 
                                                    "Télécharger CSV", 
                                                    class = "btn-info btn-lg", 
                                                    icon = icon("file-csv"),
                                                    style = "font-weight: bold; padding: 10px 20px; width: 90%;"))
                            )
                        )
                      )
                    )
                )
              ),
              
              # Section Personnalisation
              fluidRow(
                box(title = tagList(icon("sliders-h"), "Personnalisation des graphiques"), 
                    status = "warning", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    
                    fluidRow(
                      # Titres et labels
                      column(3,
                             div(class = "well",
                                 style = "background-color: #fff9e6; border-left: 4px solid #f39c12;",
                                 tags$h5(
                                   icon("heading"), 
                                   "Titres et étiquettes",
                                   style = "font-weight: bold; color: #f39c12; margin-top: 0;"
                                 ),
                                 textInput("crosstabTitle", 
                                           "Titre principal :", 
                                           placeholder = "Automatique si vide"),
                                 textInput("crosstabXLabel", 
                                           "Étiquette axe X :", 
                                           placeholder = "Automatique si vide"),
                                 textInput("crosstabYLabel", 
                                           "Étiquette axe Y :", 
                                           placeholder = "Automatique si vide"),
                                 tags$small(class = "text-muted", 
                                            icon("lightbulb"), " Laissez vide pour les titres automatiques")
                             )
                      ),
                      
                      # Tailles de police
                      column(3,
                             div(class = "well",
                                 style = "background-color: #e8f4f8; border-left: 4px solid #3498db;",
                                 tags$h5(
                                   icon("font"), 
                                   "Tailles des polices",
                                   style = "font-weight: bold; color: #3498db; margin-top: 0;"
                                 ),
                                 sliderInput("titleSize", 
                                             "Titre principal :", 
                                             min = 8, max = 32, value = 16, step = 1, 
                                             post = " pt"),
                                 sliderInput("axisLabelSize", 
                                             "Titres des axes :", 
                                             min = 8, max = 24, value = 12, step = 1, 
                                             post = " pt"),
                                 sliderInput("axisTextSize", 
                                             "Texte des graduations :", 
                                             min = 6, max = 20, value = 10, step = 1, 
                                             post = " pt"),
                                 sliderInput("legendTextSize", 
                                             "Texte de la légende :", 
                                             min = 6, max = 20, value = 10, step = 1, 
                                             post = " pt")
                             )
                      ),
                      
                      # Style des textes
                      column(3,
                             div(class = "well",
                                 style = "background-color: #fdeaea; border-left: 4px solid #e74c3c;",
                                 tags$h5(
                                   icon("bold"), 
                                   "Style du texte",
                                   style = "font-weight: bold; color: #e74c3c; margin-top: 0;"
                                 ),
                                 tags$strong("Titres des axes :"),
                                 checkboxInput("axisTitleBold", "Gras", value = TRUE),
                                 checkboxInput("axisTitleItalic", "Italique", value = FALSE),
                                 hr(style = "margin: 10px 0; border-top: 1px solid #ddd;"),
                                 tags$strong("Graduations :"),
                                 checkboxInput("axisTextBold", "Gras", value = FALSE),
                                 checkboxInput("axisTextItalic", "Italique", value = FALSE)
                             )
                      ),
                      
                      # Options d'affichage
                      column(3,
                             div(class = "well",
                                 style = "background-color: #e8f8f5; border-left: 4px solid #1abc9c;",
                                 tags$h5(
                                   icon("adjust"), 
                                   "Options d'affichage",
                                   style = "font-weight: bold; color: #1abc9c; margin-top: 0;"
                                 ),
                                 sliderInput("xAxisRotation", 
                                             "Rotation axe X :", 
                                             min = 0, max = 90, value = 45, step = 5, 
                                             post = "°"),
                                 checkboxInput("showPercentages", 
                                               "Afficher les valeurs", 
                                               value = TRUE),
                                 checkboxInput("showGridLines", 
                                               "Afficher la grille", 
                                               value = TRUE),
                                 tags$small(class = "help-block text-muted", 
                                            icon("info-circle"), " Rotation à 0° = étiquettes horizontales")
                             )
                      )
                    )
                )
              ),
              
              # Section Graphiques
              fluidRow(
                # Graphique principal
                box(title = tagList(icon("chart-line"), "Graphique principal"), 
                    status = "success", width = 6, solidHeader = TRUE, collapsible = TRUE,
                    
                    conditionalPanel(
                      condition = "!output.crosstabPlot",
                      div(class = "alert alert-warning text-center", 
                          style = "margin: 50px 20px;",
                          icon("exclamation-triangle", class = "fa-3x"),
                          tags$h4("Aucun graphique généré", style = "margin-top: 20px;"),
                          tags$p("Veuillez cliquer sur 'Générer l'analyse complète' pour créer le graphique")
                      )
                    ),
                    
                    plotOutput("crosstabPlot", height = "550px"),
                    
                    hr(style = "border-top: 2px solid #27ae60; margin: 20px 0;"),
                    
                    # Paramètres d'export
                    div(style = "background-color: #eafaf1; padding: 20px; border-radius: 8px; border: 1px solid #27ae60;",
                        tags$h5(
                          icon("download"), 
                          "Paramètres d'exportation",
                          style = "font-weight: bold; color: #27ae60; margin-top: 0; margin-bottom: 15px;"
                        ),
                        
                        fluidRow(
                          column(6,
                                 numericInput("mainPlotWidth", 
                                              "Largeur (pixels) :", 
                                              value = 800, min = 400, max = 4000, step = 50,
                                              width = "100%")),
                          column(6,
                                 numericInput("mainPlotHeight", 
                                              "Hauteur (pixels) :", 
                                              value = 600, min = 400, max = 4000, step = 50,
                                              width = "100%"))
                        ),
                        
                        fluidRow(
                          column(6,
                                 numericInput("mainPlotDPI", 
                                              "Résolution (DPI) :", 
                                              value = 300, min = 72, max = 600, step = 50,
                                              width = "100%")),
                          column(6,
                                 selectInput("mainPlotFormat", 
                                             "Format d'export :", 
                                             choices = c(
                                               "PNG (recommandé)" = "png", 
                                               "JPEG" = "jpeg",
                                               "TIFF (haute qualité)" = "tiff",
                                               "PDF (vectoriel)" = "pdf",
                                               "SVG (vectoriel web)" = "svg",
                                               "EPS (publication)" = "eps",
                                               "BMP" = "bmp"
                                             ),
                                             selected = "png",
                                             width = "100%"))
                        ),
                        
                        div(class = "alert alert-info", style = "margin-top: 15px; margin-bottom: 15px;",
                            icon("info-circle"),
                            tags$strong(" Guide des formats:"),
                            tags$ul(style = "margin-bottom: 0;",
                                    tags$li(tags$strong("PNG/TIFF:"), " Meilleure qualité pour impression"),
                                    tags$li(tags$strong("PDF/SVG/EPS:"), " Qualité vectorielle, redimensionnable"),
                                    tags$li(tags$strong("JPEG:"), " Fichier plus léger, pour web")
                            )
                        ),
                        
                        downloadButton("downloadPlot", 
                                       "Télécharger le graphique", 
                                       class = "btn-success btn-lg btn-block", 
                                       icon = icon("download"),
                                       style = "font-weight: bold; font-size: 16px; padding: 15px; margin-top: 10px; background: linear-gradient(to right, #27ae60, #219653); box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
                    )
                ),
                
                # Graphique en secteurs
                box(title = tagList(icon("chart-pie"), "Graphique en secteurs"), 
                    status = "info", width = 6, solidHeader = TRUE, collapsible = TRUE,
                    
                    selectInput("pieVariable", 
                                "Variable à représenter :",
                                choices = c(
                                  "Variable en lignes" = "row", 
                                  "Variable en colonnes" = "col"
                                ),
                                width = "100%"),
                    
                    selectInput("pieColorPalette", 
                                "Palette de couleurs :",
                                choices = c(
                                  "Par défaut" = "ggplot_default",
                                  "Noir" = "black",
                                  "Viridis" = "viridis",
                                  "Plasma" = "plasma",
                                  "Inferno" = "inferno",
                                  "Set1" = "Set1",
                                  "Set2" = "Set2",
                                  "Pastel" = "Pastel1",
                                  "Spectral" = "Spectral"
                                ),
                                selected = "ggplot_default",
                                width = "100%"),
                    
                    conditionalPanel(
                      condition = "!output.crosstabPiePlot",
                      div(class = "alert alert-warning text-center", 
                          style = "margin: 50px 20px;",
                          icon("exclamation-triangle", class = "fa-3x"),
                          tags$h4("Aucun graphique généré", style = "margin-top: 20px;"),
                          tags$p("Veuillez cliquer sur 'Générer l'analyse complète' pour créer le graphique")
                      )
                    ),
                    
                    plotOutput("crosstabPiePlot", height = "450px"),
                    
                    hr(style = "border-top: 2px solid #3498db; margin: 20px 0;"),
                    
                    # Paramètres d'export
                    div(style = "background-color: #ebf5fb; padding: 20px; border-radius: 8px; border: 1px solid #3498db;",
                        tags$h5(
                          icon("download"), 
                          "Paramètres d'exportation",
                          style = "font-weight: bold; color: #3498db; margin-top: 0; margin-bottom: 15px;"
                        ),
                        
                        fluidRow(
                          column(6,
                                 numericInput("piePlotWidth", 
                                              "Largeur (pixels) :", 
                                              value = 800, min = 400, max = 4000, step = 50,
                                              width = "100%")),
                          column(6,
                                 numericInput("piePlotHeight", 
                                              "Hauteur (pixels) :", 
                                              value = 800, min = 400, max = 4000, step = 50,
                                              width = "100%"))
                        ),
                        
                        fluidRow(
                          column(6,
                                 numericInput("piePlotDPI", 
                                              "Résolution (DPI) :", 
                                              value = 300, min = 72, max = 600, step = 50,
                                              width = "100%")),
                          column(6,
                                 selectInput("piePlotFormat", 
                                             "Format d'export :", 
                                             choices = c(
                                               "PNG (recommandé)" = "png", 
                                               "JPEG" = "jpeg",
                                               "TIFF (haute qualité)" = "tiff",
                                               "PDF (vectoriel)" = "pdf",
                                               "SVG (vectoriel web)" = "svg",
                                               "EPS (publication)" = "eps",
                                               "BMP" = "bmp"
                                             ),
                                             selected = "png",
                                             width = "100%"))
                        ),
                        
                        div(class = "alert alert-warning", style = "margin-top: 15px; margin-bottom: 15px;",
                            icon("lightbulb"),
                            tags$strong(" Conseil:"), 
                            " Pour un graphique circulaire optimal, utilisez la même largeur et hauteur (format carré)"
                        ),
                        
                        downloadButton("downloadPiePlot", 
                                       "Télécharger le graphique", 
                                       class = "btn-info btn-lg btn-block", 
                                       icon = icon("download"),
                                       style = "font-weight: bold; font-size: 16px; padding: 15px; margin-top: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
                    )
                )
              )
      ),
      # ---- Visualisation des données ----
      tabItem(tabName = "visualization",
              # En-tête avec instructions
              fluidRow(
                box(
                  title = tagList(icon("chart-line"), " Visualisation Interactive des Données"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  p("Cette interface se met à jour automatiquement à chaque modification. ", 
                    "Sélectionnez vos variables et ajustez les paramètres pour voir les changements en temps réel.",
                    style = "font-size: 14px;"),
                  tags$ul(
                    tags$li(icon("check-circle", style = "color: #28a745;"), " Sélection automatique des variables"),
                    tags$li(icon("check-circle", style = "color: #28a745;"), " Mise à jour instantanée du graphique"),
                    tags$li(icon("check-circle", style = "color: #28a745;"), " Détection automatique des types de données"),
                    tags$li(icon("check-circle", style = "color: #28a745;"), " Personnalisation en temps réel")
                  )
                )
              ),
              
              # Ligne principale avec panneau de contrôle et graphique
              fluidRow(
                
                # === PANNEAU DE CONTRÔLE (Gauche) ===
                box(
                  title = tagList(icon("sliders-h"), " Paramètres de Visualisation"),
                  status = "primary",
                  width = 4,
                  solidHeader = TRUE,
                  
                  # Section: Variables de base
                  div(
                    class = "well",
                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(icon("database"), " Sélection des Variables", 
                       style = "color: #007bff; margin-top: 0; font-size: 16px;"),
                    
                    # Variable X avec actualisation auto
                    uiOutput("vizXVarSelect"),
                    
                    # Type de variable X avec détection automatique
                    selectInput(
                      "xVarType",
                      tagList(icon("magic"), " Type de la variable X:"),
                      choices = c(
                        "Auto (détection automatique)" = "auto",
                        "Date/Temporelle" = "date",
                        "Catégorielle" = "categorical",
                        "Texte libre" = "text",
                        "Facteur" = "factor",
                        "Numérique continue" = "numeric"
                      ),
                      selected = "auto"
                    ),
                    helpText(
                      icon("info-circle", style = "color: #17a2b8;"),
                      "Le mode 'Auto' détecte automatiquement le type optimal pour votre variable."
                    )
                  ),
                  
                  # Section: Personnalisation des niveaux X (conditionnelle)
                  conditionalPanel(
                    condition = "input.xVarType == 'factor' || input.xVarType == 'categorical' || input.xVarType == 'text' || input.xVarType == 'auto'",
                    div(
                      class = "well",
                      style = "background-color: #fff3e0; border-left: 4px solid #ff9800; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      h4(
                        icon("edit"), " Personnalisation des Étiquettes",
                        style = "color: #ff9800; font-weight: bold; margin-top: 0; font-size: 15px;"
                      ),
                      p("Modifiez les noms des catégories pour améliorer la lisibilité de votre graphique.",
                        style = "font-size: 13px; color: #666; margin-bottom: 10px;"),
                      uiOutput("xLevelsEditor"),
                      helpText(
                        icon("lightbulb", style = "color: #ffc107;"),
                        "Les modifications sont conservées jusqu'à la fermeture de l'application."
                      )
                    )
                  ),
                  
                  # Section: Ordre des catégories X (pour seasonal_evolution)
                  conditionalPanel(
                    condition = "input.vizType == 'seasonal_evolution' && (input.xVarType == 'factor' || input.xVarType == 'categorical' || input.xVarType == 'text' || input.xVarType == 'auto')",
                    div(
                      class = "well",
                      style = "background-color: #e8f5e9; border-left: 4px solid #28a745; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      h4(
                        icon("sort"), " Ordre des Catégories",
                        style = "color: #28a745; font-weight: bold; margin-top: 0; font-size: 15px;"
                      ),
                      p("Définissez l'ordre d'apparition des catégories sur l'axe X.",
                        style = "font-size: 13px; color: #666; margin-bottom: 10px;"),
                      uiOutput("xOrderEditor"),
                      helpText(
                        icon("hand-point-up", style = "color: #28a745;"),
                        "Glissez-déposez pour réorganiser. Le graphique se met à jour automatiquement."
                      )
                    )
                  ),
                  
                  # Section: Options pour les dates
                  conditionalPanel(
                    condition = "input.xVarType == 'date'",
                    div(
                      class = "well",
                      style = "background-color: #e3f2fd; border-left: 4px solid #2196F3; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      h4(
                        icon("calendar-alt"), " Paramètres de Date",
                        style = "color: #2196F3; font-weight: bold; margin-top: 0; font-size: 15px;"
                      ),
                      selectInput(
                        "xDateFormat",
                        "Format de conversion:",
                        choices = c(
                          "AAAA-MM-JJ (ISO 8601)" = "%Y-%m-%d",
                          "JJ/MM/AAAA (France)" = "%d/%m/%Y",
                          "MM/JJ/AAAA (US)" = "%m/%d/%Y",
                          "AAAA/MM/JJ" = "%Y/%m/%d",
                          "JJ-Mois-AAAA" = "%d-%b-%Y",
                          "Mois JJ, AAAA" = "%B %d, %Y"
                        ),
                        selected = "%Y-%m-%d"
                      ),
                      helpText(
                        icon("exclamation-triangle", style = "color: #ffc107;"),
                        "Assurez-vous que le format correspond à vos données pour une conversion correcte."
                      )
                    )
                  ),
                  
                  # Section: Variable(s) Y avec support multi-sélection
                  div(
                    class = "well",
                    style = "background-color: #f1f8e9; border-left: 4px solid #8bc34a; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(
                      icon("chart-bar"), " Variable(s) à Visualiser",
                      style = "color: #689f38; font-weight: bold; margin-top: 0; font-size: 15px;"
                    ),
                    uiOutput("vizYVarSelect"),
                    # Indicateur de mode multi-Y
                    conditionalPanel(
                      condition = "output.multiYIndicator === true",
                      div(
                        style = "margin-top: 10px; padding: 8px; background-color: #c8e6c9; border-radius: 4px; border: 1px solid #81c784;",
                        icon("layer-group", style = "color: #388e3c;"),
                        span(
                          id = "multiYBadge",
                          style = "font-weight: bold; color: #1b5e20; margin-left: 5px;",
                          "Mode multi-variables activé"
                        )
                      )
                    )
                  ),
                  
                  # Section: Variables supplémentaires (Couleur et Facette)
                  div(
                    class = "well",
                    style = "background-color: #fce4ec; border-left: 4px solid #e91e63; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(
                      icon("palette"), " Variables Supplémentaires",
                      style = "color: #c2185b; font-weight: bold; margin-top: 0; font-size: 15px;"
                    ),
                    
                    # Variable de couleur
                    uiOutput("vizColorVarSelect"),
                    
                    # Variable de facetting
                    uiOutput("vizFacetVarSelect"),
                    
                    # Avertissement pour facetting
                    conditionalPanel(
                      condition = "input.vizFacetVar != 'Aucun' && input.vizFacetVar != null",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 4px;",
                        icon("exclamation-triangle", style = "color: #ff9800;"),
                        span(
                          style = "font-size: 12px; color: #856404; margin-left: 5px;",
                          "Utilisez une variable avec 2-10 catégories maximum et sans valeurs manquantes."
                        )
                      )
                    ),
                    
                    # Options de facetting
                    conditionalPanel(
                      condition = "input.vizFacetVar != 'Aucun' && input.vizFacetVar != null",
                      div(
                        style = "margin-top: 10px;",
                        checkboxInput(
                          "facetScalesFree",
                          tagList(icon("expand"), " Échelles libres pour chaque facette"),
                          value = FALSE
                        ),
                        helpText("Permet à chaque sous-graphique d'avoir ses propres limites d'axes.")
                      )
                    )
                  ),
                  
                  # Section: Type de visualisation
                  div(
                    class = "well",
                    style = "background-color: #e1f5fe; border-left: 4px solid #03a9f4; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(
                      icon("chart-pie"), " Type de Graphique",
                      style = "color: #0288d1; font-weight: bold; margin-top: 0; font-size: 15px;"
                    ),
                    selectInput(
                      "vizType",
                      "Sélectionnez le type:",
                      choices = c(
                        "Nuage de points (Scatter)" = "scatter",
                        "Courbe avec lissage (Seasonal Smooth)" = "seasonal_smooth",
                        "Courbe d'évolution (Seasonal Evolution)" = "seasonal_evolution",
                        "Boîte à moustaches (Boxplot)" = "box",
                        "Graphique en violon (Violin)" = "violin",
                        "Diagramme en barres (Bar)" = "bar",
                        "Graphique en lignes (Line)" = "line",
                        "Densité de distribution (Density)" = "density",
                        "Histogramme" = "histogram",
                        "Carte de chaleur (Heatmap)" = "heatmap",
                        "Aires empilées (Area)" = "area",
                        "Diagramme circulaire (Pie)" = "pie",
                        "Graphique en anneau (Donut)" = "donut",
                        "Carte proportionnelle (Treemap)" = "treemap"
                      ),
                      selected = "scatter"
                    ),
                    
                    # Badge de compatibilité multi-Y
                    conditionalPanel(
                      condition = "input.vizType == 'scatter' || input.vizType == 'line' || input.vizType == 'area' || input.vizType == 'bar' || input.vizType == 'seasonal_smooth' || input.vizType == 'seasonal_evolution'",
                      div(
                        style = "margin-top: 8px; padding: 6px 10px; background-color: #d4edda; border-radius: 4px; font-size: 12px; display: inline-block; border: 1px solid #c3e6cb;",
                        icon("layer-group", style = "color: #28a745;"),
                        span(
                          style = "color: #155724; margin-left: 5px; font-weight: 600;",
                          "Compatible multi-variables Y"
                        )
                      )
                    )
                  ),
                  
                  # Section: Options d'agrégation
                  div(
                    class = "well",
                    style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(
                      icon("calculator"), " Agrégation des Données",
                      style = "color: #f57c00; font-weight: bold; margin-top: 0; font-size: 15px;"
                    ),
                    checkboxInput(
                      "useAggregation",
                      tagList(icon("check-square"), " Activer l'agrégation"),
                      value = FALSE
                    ),
                    
                    conditionalPanel(
                      condition = "input.useAggregation == true",
                      selectInput(
                        "aggFunction",
                        "Fonction d'agrégation:",
                        choices = c(
                          "Moyenne" = "mean",
                          "Médiane" = "median",
                          "Somme" = "sum",
                          "Comptage" = "count",
                          "Minimum" = "min",
                          "Maximum" = "max",
                          "Écart-type" = "sd"
                        ),
                        selected = "mean"
                      ),
                      # CORRECTION: Ajout du uiOutput pour les variables de regroupement
                      uiOutput("groupVarsSelect"),
                      div(
                        style = "margin-top: 10px; padding: 8px; background-color: #d1ecf1; border-radius: 4px;",
                        icon("info-circle", style = "color: #0c5460;"),
                        span(
                          style = "font-size: 12px; color: #0c5460; margin-left: 5px;",
                          "L'agrégation résume vos données selon la fonction choisie."
                        )
                      ),
                      verbatimTextOutput("aggregationInfo")
                    )
                  ),
                  
                  # Section: Bouton de mise à jour (optionnel, car auto)
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton(
                      "refreshPlot",
                      tagList(icon("sync-alt"), " Actualiser le Graphique"),
                      class = "btn-success btn-lg",
                      style = "width: 100%; font-weight: bold;"
                    ),
                    helpText(
                      icon("magic", style = "color: #28a745;"),
                      "Le graphique se met à jour automatiquement, mais vous pouvez forcer un rafraîchissement.",
                      style = "margin-top: 10px; color: #666;"
                    )
                  )
                ),
                
                # === PANNEAU D'AFFICHAGE DU GRAPHIQUE (Droite) 
                box(
                  title = tagList(icon("chart-area"), " Graphique Interactif"),
                  status = "success",
                  width = 8,
                  solidHeader = TRUE,
                  
                  # Zone du graphique avec loader
                  div(
                    style = "position: relative; min-height: 500px;",
                    
                    # Loader animé (visible pendant le chargement)
                    div(
                      id = "plotLoader",
                      style = "display: none; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); text-align: center; z-index: 1000;",
                      div(
                        style = "font-size: 48px; color: #007bff;",
                        icon("spinner", class = "fa-spin")
                      ),
                      p("Chargement du graphique...", 
                        style = "margin-top: 10px; font-size: 16px; color: #666;")
                    ),
                    
                    # Graphique plotly interactif
                    plotlyOutput("interactivePlot", height = "600px")
                  ),
                  
                  # Barre d'outils sous le graphique
                  div(
                    style = "margin-top: 15px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
                      # Indicateurs de statut
                      div(
                        style = "display: flex; gap: 15px; align-items: center;",
                        div(
                          style = "padding: 5px 10px; background-color: #d4edda; border-radius: 4px; font-size: 12px; border: 1px solid #c3e6cb;",
                          icon("check-circle", style = "color: #28a745;"),
                          span(
                            style = "margin-left: 5px; color: #155724; font-weight: 600;",
                            "Mise à jour auto"
                          )
                        ),
                        div(
                          id = "lastUpdateTime",
                          style = "font-size: 12px; color: #666;",
                          paste("Dernière mise à jour:", format(Sys.time(), "%H:%M:%S"))
                        )
                      )
                    )
                  )
                ),
              ),
              
              # === SECTION EXPORT DU GRAPHIQUE ===
              fluidRow(
                # NOUVELLE SECTION: EXPORT AVANCÉ
                box(
                  title = tagList(icon("download"), " Exporter le graphique"),
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  
                  # DPI recommandés
                  div(
                    style = "margin: 10px 0; padding: 10px; background-color: #e7f3ff; border-left: 3px solid #007bff; border-radius: 4px;",
                    icon("info-circle", style = "color: #007bff;"),
                    span(style = "margin-left: 5px; font-weight: 600; color: #004085;",
                         "DPI recommandés: 72-150 (écran), 300 (impression standard), 600+ (haute qualité)")
                  ),
                  
                  # Largeur et hauteur
                  fluidRow(
                    column(6,
                           numericInput("exportWidth", 
                                        label = "Largeur (pouces):",
                                        value = 10,
                                        min = 1,
                                        max = 100,
                                        step = 0.5)
                    ),
                    column(6,
                           numericInput("exportHeight", 
                                        label = "Hauteur (pouces):",
                                        value = 6,
                                        min = 1,
                                        max = 100,
                                        step = 0.5)
                    )
                  ),
                  
                  # Format et DPI
                  fluidRow(
                    column(6,
                           selectInput("exportFormat", 
                                       label = "Format d'export:",
                                       choices = c(
                                         "PNG (Recommandé)" = "png",
                                         "JPEG/JPG" = "jpg",
                                         "TIFF" = "tiff",
                                         "BMP" = "bmp",
                                         "PDF (Vectoriel)" = "pdf",
                                         "SVG (Vectoriel)" = "svg",
                                         "EPS (Vectoriel)" = "eps"
                                       ),
                                       selected = "png")
                    ),
                    column(6,
                           numericInput("exportDPI", 
                                        label = "DPI (résolution):",
                                        value = 300,
                                        min = 72,
                                        max = 20000,
                                        step = 50)
                    )
                  ),
                  
                  # Dimensions finales
                  uiOutput("exportDimensionsInfo"),
                  
                  # Presets rapides
                  div(
                    style = "margin: 15px 0; padding: 15px; background-color: #d4edda; border-left: 3px solid #28a745; border-radius: 4px;",
                    h5(icon("magic"), " Presets rapides:", 
                       style = "color: #155724; margin-top: 0; font-size: 14px; font-weight: bold;"),
                    fluidRow(
                      column(4,
                             actionButton("presetScreen", 
                                          "Écran",
                                          class = "btn-sm btn-info",
                                          style = "width: 100%;")
                      ),
                      column(4,
                             actionButton("presetPrint", 
                                          "Impression",
                                          class = "btn-sm btn-success",
                                          style = "width: 100%;")
                      ),
                      column(4,
                             actionButton("presetHighQuality", 
                                          "Haute qualité",
                                          class = "btn-sm btn-warning",
                                          style = "width: 100%;")
                      )
                    )
                  ),
                  
                  # Options JPEG
                  conditionalPanel(
                    condition = "input.exportFormat == 'jpg'",
                    div(
                      style = "margin: 10px 0;",
                      sliderInput("jpegQuality",
                                  "Qualité JPEG:",
                                  min = 50,
                                  max = 100,
                                  value = 95,
                                  step = 5)
                    )
                  ),
                  
                  # Compression TIFF
                  conditionalPanel(
                    condition = "input.exportFormat == 'tiff'",
                    div(
                      style = "margin: 10px 0; padding: 10px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 4px;",
                      h5(icon("compress"), " Compression TIFF:", 
                         style = "color: #856404; margin-top: 0; font-size: 14px; font-weight: bold;"),
                      selectInput("tiffCompression",
                                  label = NULL,
                                  choices = c("Aucune" = "none",
                                              "LZW" = "lzw",
                                              "ZIP" = "zip"),
                                  selected = "lzw")
                    )
                  ),
                  
                  # Bouton de téléchargement - AMÉLIORÉ ET FONCTIONNEL
                  div(
                    style = "margin-top: 20px; text-align: center; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
                    downloadButton(
                      "downloadPlot", 
                      label = tagList(icon("download"), " Télécharger le graphique"),
                      class = "btn-primary btn-lg",
                      style = "width: 90%; padding: 15px 30px; font-size: 18px; font-weight: bold; box-shadow: 0 4px 6px rgba(0,0,0,0.2);"
                    ),
                    br(),
                    helpText(
                      icon("info-circle", style = "color: #007bff;"),
                      "Cliquez pour télécharger le graphique avec les paramètres sélectionnés ci-dessus.",
                      style = "margin-top: 10px; font-size: 13px; color: #666;"
                    )
                  )
                )
              ),
              # === PANNEAU D'OPTIONS AVANCÉES (Extensible) 
              fluidRow(
                box(
                  title = tagList(icon("cog"), " Options Avancées de Personnalisation"),
                  status = "warning",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  # En-tête avec bouton de personnalisation rapide
                  div(
                    style = "margin-bottom: 20px; padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 8px; text-align: center;",
                    actionButton(
                      "customizePlot",
                      tagList(icon("paint-brush"), " PERSONNALISATION RAPIDE"),
                      class = "btn-light btn-lg",
                      style = "font-weight: bold; padding: 12px 30px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
                    ),
                    p("Accès rapide aux paramètres les plus utilisés", 
                      style = "color: white; margin-top: 10px; margin-bottom: 0; font-size: 13px;")
                  ),
                  
                  fluidRow(
                    # ========== COLONNE 1: TEXTES ET LABELS =
                    column(
                      width = 4,
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px;",
                        
                        h4(icon("font"), " Textes et Labels", 
                           style = "color: #9c27b0; font-weight: bold; border-bottom: 3px solid #9c27b0; padding-bottom: 10px; margin-top: 0;"),
                        
                        # Titres
                        div(
                          style = "margin-bottom: 20px;",
                          h5(icon("heading"), " Titres", style = "color: #666; font-size: 14px; font-weight: bold;"),
                          textInput(
                            "plotTitle",
                            "Titre principal:",
                            value = "",
                            placeholder = "Titre du graphique..."
                          ),
                        ),
                        
                        # Labels des axes
                        div(
                          style = "margin-bottom: 20px;",
                          h5(icon("arrows-alt-h"), " Labels des axes", style = "color: #666; font-size: 14px; font-weight: bold;"),
                          textInput(
                            "xAxisLabel",
                            "Label axe X:",
                            value = "",
                            placeholder = "Auto"
                          ),
                          textInput(
                            "yAxisLabel",
                            "Label axe Y:",
                            value = "",
                            placeholder = "Auto"
                          )
                        ),
                        
                        # Légende
                        div(
                          style = "margin-bottom: 20px;",
                          h5(icon("list"), " Légende", style = "color: #666; font-size: 14px; font-weight: bold;"),
                          textInput(
                            "legendTitle",
                            "Titre de la légende:",
                            value = "",
                            placeholder = "Auto"
                          ),
                          conditionalPanel(
                            condition = "input.vizColorVar != 'Aucun' || output.multiYIndicator === true",
                            actionButton(
                              "customizeLegendLabels",
                              tagList(icon("edit"), " Personnaliser les niveaux"),
                              class = "btn-sm btn-info",
                              style = "width: 100%; margin-top: 5px;"
                            )
                          ),
                          selectInput(
                            "legendPosition",
                            "Position:",
                            choices = c(
                              "Droite" = "right",
                              "Gauche" = "left",
                              "Haut" = "top",
                              "Bas" = "bottom",
                              "Aucune" = "none"
                            ),
                            selected = "right"
                          )
                        )
                      )
                    ),
                    
                    # ========== COLONNE 2: FORMATAGE DES TEXTES 
                    column(
                      width = 4,
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px;",
                        
                        h4(icon("text-height"), " Formatage des Textes", 
                           style = "color: #ff5722; font-weight: bold; border-bottom: 3px solid #ff5722; padding-bottom: 10px; margin-top: 0;"),
                        
                        # Tailles
                        div(
                          style = "margin-bottom: 20px;",
                          h5(icon("text-width"), " Tailles", style = "color: #666; font-size: 14px; font-weight: bold;"),
                          sliderInput(
                            "baseFontSize",
                            "Police de base:",
                            min = 8,
                            max = 20,
                            value = 12,
                            step = 1
                          ),
                          sliderInput(
                            "titleSize",
                            "Titre:",
                            min = 10,
                            max = 24,
                            value = 14,
                            step = 1
                          ),
                          sliderInput(
                            "axisLabelSize",
                            "Labels d'axes:",
                            min = 8,
                            max = 18,
                            value = 11,
                            step = 1
                          )
                        ),
                        
                        # Formatage Label X
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #e3f2fd; border-radius: 6px;",
                          h5(icon("long-arrow-alt-right"), " Label axe X", 
                             style = "color: #1976d2; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput(
                              "xAxisBold",
                              tagList(icon("bold"), " Gras"),
                              value = FALSE
                            ),
                            checkboxInput(
                              "xAxisItalic",
                              tagList(icon("italic"), " Italique"),
                              value = FALSE
                            )
                          )
                        ),
                        
                        # Formatage Label Y
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #f3e5f5; border-radius: 6px;",
                          h5(icon("long-arrow-alt-up"), " Label axe Y", 
                             style = "color: #7b1fa2; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput(
                              "yAxisBold",
                              tagList(icon("bold"), " Gras"),
                              value = FALSE
                            ),
                            checkboxInput(
                              "yAxisItalic",
                              tagList(icon("italic"), " Italique"),
                              value = FALSE
                            )
                          )
                        ),
                        
                        # Formatage Niveaux X
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #fff3e0; border-radius: 6px;",
                          h5(icon("tag"), " Niveaux axe X", 
                             style = "color: #f57c00; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput(
                              "xTickBold",
                              tagList(icon("bold"), " Gras"),
                              value = FALSE
                            ),
                            checkboxInput(
                              "xTickItalic",
                              tagList(icon("italic"), " Italique"),
                              value = FALSE
                            )
                          ),
                          sliderInput(
                            "xAxisAngle",
                            "Angle d'inclinaison (°):",
                            min = 0,
                            max = 90,
                            value = 0,
                            step = 15
                          )
                        )
                      )
                    ),
                    
                    # ========== COLONNE 3: APPARENCE ET ÉLÉMENTS 
                    column(
                      width = 4,
                      # Apparence visuelle
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px; margin-bottom: 15px;",
                        
                        h4(icon("palette"), " Apparence Visuelle", 
                           style = "color: #ff9800; font-weight: bold; border-bottom: 3px solid #ff9800; padding-bottom: 10px; margin-top: 0;"),
                        
                        sliderInput(
                          "pointSize",
                          tagList(icon("circle"), " Taille des points:"),
                          min = 1,
                          max = 10,
                          value = 3,
                          step = 0.5
                        ),
                        
                        sliderInput(
                          "pointAlpha",
                          tagList(icon("adjust"), " Transparence des points:"),
                          min = 0,
                          max = 1,
                          value = 0.7,
                          step = 0.1
                        ),
                        
                        sliderInput(
                          "lineWidth",
                          tagList(icon("minus"), " Épaisseur des lignes:"),
                          min = 0.5,
                          max = 5,
                          value = 1,
                          step = 0.5
                        ),
                        
                        # Options spécifiques aux barres
                        conditionalPanel(
                          condition = "input.vizType == 'bar'",
                          div(
                            style = "margin-top: 15px; padding: 10px; background-color: #e8f5e9; border-radius: 6px;",
                            h5(icon("chart-bar"), " Options Barres", 
                               style = "color: #388e3c; font-size: 13px; font-weight: bold; margin-top: 0;"),
                            sliderInput(
                              "barWidth",
                              "Largeur:",
                              min = 0.3,
                              max = 1,
                              value = 0.8,
                              step = 0.1
                            ),
                            selectInput(
                              "barPosition",
                              "Position:",
                              choices = c(
                                "Côte à côte" = "dodge",
                                "Empilées" = "stack",
                                "Remplissage" = "fill"
                              ),
                              selected = "dodge"
                            )
                          )
                        ),
                        
                        # Options spécifiques à l'histogramme
                        conditionalPanel(
                          condition = "input.vizType == 'histogram'",
                          div(
                            style = "margin-top: 15px; padding: 10px; background-color: #e1f5fe; border-radius: 6px;",
                            h5(icon("chart-area"), " Options Histogramme", 
                               style = "color: #0277bd; font-size: 13px; font-weight: bold; margin-top: 0;"),
                            sliderInput(
                              "histBins",
                              "Nombre de bins:",
                              min = 10,
                              max = 100,
                              value = 30,
                              step = 5
                            ),
                            colourInput(
                              "histColor",
                              "Couleur:",
                              value = "steelblue"
                            )
                          )
                        )
                      ),
                      
                      # Éléments du graphique
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px;",
                        
                        h4(icon("layer-group"), " Éléments du Graphique", 
                           style = "color: #2196F3; font-weight: bold; border-bottom: 3px solid #2196F3; padding-bottom: 10px; margin-top: 0;"),
                        
                        checkboxInput(
                          "showPoints",
                          tagList(icon("circle"), " Afficher les points"),
                          value = TRUE
                        ),
                        
                        checkboxInput(
                          "showValues",
                          tagList(icon("hashtag"), " Afficher les valeurs"),
                          value = FALSE
                        ),
                        
                        # Options pour scatter et line
                        conditionalPanel(
                          condition = "input.vizType == 'scatter' || input.vizType == 'line'",
                          checkboxInput(
                            "showTrendLine",
                            tagList(icon("chart-line"), " Ligne de tendance"),
                            value = FALSE
                          ),
                          conditionalPanel(
                            condition = "input.showTrendLine == true",
                            selectInput(
                              "trendMethod",
                              "Méthode:",
                              choices = c(
                                "Linéaire" = "lm",
                                "LOESS" = "loess",
                                "GAM" = "gam"
                              ),
                              selected = "lm"
                            )
                          )
                        ),
                        
                        # Options pour seasonal_smooth
                        conditionalPanel(
                          condition = "input.vizType == 'seasonal_smooth'",
                          checkboxInput(
                            "showSmoothLine",
                            tagList(icon("bezier-curve"), " Ligne de lissage"),
                            value = TRUE
                          ),
                          conditionalPanel(
                            condition = "input.showSmoothLine == true",
                            selectInput(
                              "smoothMethod",
                              "Méthode:",
                              choices = c(
                                "LOESS" = "loess",
                                "Linéaire" = "lm",
                                "GAM" = "gam"
                              ),
                              selected = "loess"
                            ),
                            sliderInput(
                              "smoothSpan",
                              "Degré de lissage:",
                              min = 0.1,
                              max = 2,
                              value = 0.75,
                              step = 0.05
                            )
                          )
                        ),
                        
                        checkboxInput(
                          "showConfidenceInterval",
                          tagList(icon("area-chart"), " Intervalle de confiance"),
                          value = TRUE
                        ),
                        
                        # Options pour boxplot
                        conditionalPanel(
                          condition = "input.vizType == 'box'",
                          checkboxInput(
                            "showOutliers",
                            tagList(icon("circle-notch"), " Afficher les outliers"),
                            value = TRUE
                          )
                        ),
                        
                        # Options pour violin
                        conditionalPanel(
                          condition = "input.vizType == 'violin'",
                          checkboxInput(
                            "showBoxInsideViolin",
                            tagList(icon("box"), " Boîte à l'intérieur"),
                            value = FALSE
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              # === PANNEAU DES STATISTIQUES ET INFORMATIONS
              fluidRow(
                # Statistiques des données
                box(
                  title = tagList(icon("table"), " Statistiques des Données"),
                  status = "info",
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  div(
                    style = "padding: 15px;",
                    verbatimTextOutput("dataStatsSummary")
                  )
                ),
                
                # Statistiques du graphique
                box(
                  title = tagList(icon("chart-line"), " Informations sur le Graphique"),
                  status = "primary",
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  div(
                    style = "padding: 15px;",
                    verbatimTextOutput("plotStatsSummary")
                  )
                ),
                
                # Informations saisonnières (conditionnelles)
                conditionalPanel(
                  condition = "input.vizType == 'seasonal_smooth' || input.vizType == 'seasonal_evolution'",
                  box(
                    title = tagList(icon("calendar-alt"), " Analyse Saisonnière"),
                    status = "success",
                    width = 4,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    div(
                      style = "padding: 15px;",
                      verbatimTextOutput("seasonalAnalysisSummary")
                    )
                  )
                )
              ),
              # CORRECTION: Ajout du script SortableJS pour le drag-and-drop
              tags$head(
                tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"),
                tags$script(HTML("
            $(document).ready(function() {
              // Fonction pour initialiser Sortable
              function initSortable() {
                var el = document.getElementById('xOrderSortable');
                if (el && !el.sortableInstance) {
                  el.sortableInstance = Sortable.create(el, {
                    animation: 150,
                    ghostClass: 'sortable-ghost',
                    handle: '.sortable-item',
                    onEnd: function(evt) {
                      // Récupérer l'ordre des éléments
                      var items = el.querySelectorAll('.sortable-item');
                      var order = [];
                      items.forEach(function(item) {
                        order.push(item.getAttribute('data-value'));
                      });
                      // Envoyer l'ordre à Shiny
                      Shiny.setInputValue('xLevelOrder', order, {priority: 'event'});
                    }
                  });
                }
              }
              
              // Initialiser au chargement
              setTimeout(initSortable, 500);
              
              // Réinitialiser quand le contenu change
              $(document).on('shiny:value', function(event) {
                if (event.target.id === 'xOrderEditor') {
                  setTimeout(initSortable, 100);
                }
              });
            });
          ")),
                tags$style(HTML("
            .sortable-ghost {
              opacity: 0.4;
              background-color: #e3f2fd;
            }
            .sortable-item:hover {
              box-shadow: 0 4px 8px rgba(0,0,0,0.2) !important;
              border-color: #28a745 !important;
            
            /* CORRECTION: Styles pour rendre le bouton download cliquable */
            #downloadPlot {
              cursor: pointer !important;
              pointer-events: auto !important;
              opacity: 1 !important;
              z-index: 1000;
            }
            
            #downloadPlot:hover {
              transform: translateY(-2px);
              box-shadow: 0 6px 12px rgba(0,0,0,0.3) !important;
              background-color: #0056b3 !important;
              transition: all 0.3s ease;
            }
            
            #downloadPlot:active {
              transform: translateY(0);
            }
          "))
              )
              
      ),
      # ---- Tests statistiques ----
      tabItem(tabName = "tests",
              fluidRow(
                box(title = "Paramètres des tests", status = "danger", width = 12, solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             uiOutput("responseVarSelect"),
                             uiOutput("factorVarSelect"),
                             checkboxInput("interaction", "Inclure les interactions (ANOVA/Scheirer-Ray-Hare)", FALSE)
                      ),
                      column(4,
                             h4("Tests sur données brutes", style = "color: #3c8dbc;"),
                             actionButton("testNormalityRaw", "Test de normalité", class = "btn-warning", icon = icon("chart-line")),
                             br(), br(),
                             actionButton("testHomogeneityRaw", "Test d'homogénéité", class = "btn-warning", icon = icon("balance-scale")),
                             br(), br(),
                             h4("Tests paramétriques", style = "color: #00a65a;"),
                             actionButton("testT", "Test t de Student", class = "btn-success", icon = icon("check")),
                             actionButton("testANOVA", "ANOVA", class = "btn-success", icon = icon("check")),
                             actionButton("testLM", "Régression linéaire", class = "btn-success", icon = icon("check")),
                             actionButton("testGLM", "Modèle linéaire généralisé", class = "btn-success", icon = icon("check"))
                      ),
                      column(4,
                             h4("Tests non-paramétriques", style = "color: #f39c12;"),
                             actionButton("testWilcox", "Test de Wilcoxon", class = "btn-warning", icon = icon("check")),
                             actionButton("testKruskal", "Test de Kruskal-Wallis", class = "btn-warning", icon = icon("check")),
                             actionButton("testScheirerRayHare", "Test de Scheirer-Ray-Hare", class = "btn-warning", icon = icon("check"))
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Résultats des tests", status = "danger", width = 12, solidHeader = TRUE,
                    DTOutput("testResultsDF"),
                    br(),
                    downloadButton("downloadTestsExcel", "Télécharger les résultats (Excel)", class = "btn-info"))
              ),
              conditionalPanel(
                condition = "output.showParametricDiagnostics",
                fluidRow(
                  box(title = "Diagnostics des modèles", status = "info", width = 6, solidHeader = TRUE,
                      conditionalPanel(
                        condition = "output.showModelNavigation",
                        wellPanel(
                          h6("Navigation des modèles", style = "margin-top: 0; margin-bottom: 10px;"),
                          div(style = "text-align: center;",
                              uiOutput("modelDiagNavigation")
                          )
                        )
                      ),
                      plotOutput("modelDiagnostics", height = "500px"),
                      br(),
                      downloadButton("downloadModelDiagnostics", "Télécharger (PNG)", class = "btn-success"),
                      htmlOutput("modelDiagnosticsInterpretation")
                  ),
                  box(title = "Résidus et validation", status = "info", width = 6, solidHeader = TRUE,
                      conditionalPanel(
                        condition = "output.showResidNavigation",
                        wellPanel(
                          h6("Navigation des variables", style = "margin-top: 0; margin-bottom: 10px;"),
                          div(style = "text-align: center;",
                              uiOutput("residNavigation")
                          )
                        )
                      ),
                      tabBox(
                        title = "Analyses des résidus",
                        id = "residualTabs", width = 12,
                        tabPanel("QQ-plot", 
                                 plotOutput("qqPlotResiduals", height = "320px"),
                                 br(),
                                 downloadButton("downloadQQPlot", "Télécharger (PNG)", class = "btn-success"),
                                 htmlOutput("qqPlotInterpretation")),
                        tabPanel("Normalité", 
                                 verbatimTextOutput("normalityResult"),
                                 htmlOutput("normalityResidInterpretation")),
                        tabPanel("Homogénéité", 
                                 verbatimTextOutput("leveneResidResult"),
                                 htmlOutput("homogeneityResidInterpretation")),
                        tabPanel("Autocorrélation", 
                                 verbatimTextOutput("autocorrResult"),
                                 htmlOutput("autocorrInterpretation")),
                        tabPanel("Summary", verbatimTextOutput("modelSummary"))
                      )
                  )
                )
              )
      ),
      # ---- Comparaisons multiples PostHoc ----
      tabItem(tabName = "multiple",
              fluidRow(
                # -------- PANEL GAUCHE - Configuration 
                box(title = div(icon("cog"), " Configuration de l'analyse"), 
                    status = "primary", width = 4, solidHeader = TRUE,
                    
                    # Section Variables
                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                        h4(style = "color: #2c3e50; margin-top: 0;", icon("chart-line"), " Sélection des variables"),
                        uiOutput("multiResponseSelect"),
                        uiOutput("multiFactorSelect")
                    ),
                    
                    # Section Type de test 
                    div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                        h4(style = "color: #2c3e50; margin-top: 0;", icon("vial"), " Tests statistiques"),
                        radioButtons("testType", "Type de comparaisons",
                                     choiceNames = list(
                                       HTML("<b>Paramétrique</b> (ANOVA) <small style='color:#7f8c8d;'>- Données normales</small>"), 
                                       HTML("<b>Non paramétrique</b> (Kruskal) <small style='color:#7f8c8d;'>- Sans normalité</small>")
                                     ),
                                     choiceValues = list("param", "nonparam"),
                                     selected = "param"
                        ),
                        conditionalPanel(
                          condition = "input.testType == 'param'",
                          selectInput("multiTest", "Méthode post-hoc paramétrique",
                                      choices = list(
                                        "Tukey HSD (recommandé)" = "tukey", 
                                        "LSD (Fisher)" = "lsd", 
                                        "Duncan" = "duncan", 
                                        "SNK (Student-Newman-Keuls)" = "snk",
                                        "Scheffe (conservateur)" = "scheffe",
                                        "REGW" = "regw",
                                        "Waller-Duncan" = "waller",
                                        "Bonferroni" = "bonferroni",
                                        "Dunnett" = "dunnett", 
                                        "Games-Howell (variances inégales)" = "games"
                                      ),
                                      selected = "tukey"
                          )
                        ),
                        conditionalPanel(
                          condition = "input.testType == 'nonparam'",
                          selectInput("multiTestNonParam", "Méthode post-hoc non paramétrique",
                                      choices = list(
                                        "Kruskal-Wallis (base)" = "kruskal",
                                        "Dunn (recommandé)" = "dunn",
                                        "Conover" = "conover",
                                        "Nemenyi" = "nemenyi"
                                      ),
                                      selected = "dunn"
                          )
                        )
                    ),
                    
                    # Section Interactions - AMÉLIORÉE
                    div(style = "border: 3px solid #e74c3c; border-radius: 8px; padding: 15px; margin-bottom: 15px; background: linear-gradient(135deg, #fff5f5 0%, #ffe8e8 100%);",
                        h4(style = "color: #c0392b; margin-top: 0;", 
                           icon("project-diagram"), " Analyse des interactions"),
                        checkboxInput("posthocInteraction", 
                                      HTML("<strong style='color: #c0392b;'>Activer l'analyse des interactions</strong>"), 
                                      value = FALSE),
                        conditionalPanel(
                          condition = "input.posthocInteraction == true",
                          div(style = "margin-top: 10px; padding: 12px; background-color: white; border-radius: 5px; border-left: 4px solid #e74c3c;",
                              icon("info-circle", style = "color: #e74c3c;"), 
                              HTML("<strong> Décomposition bidirectionnelle automatique</strong><br/>
                             <small style='color: #34495e;'>
                             <b>Prérequis :</b> >= 2 facteurs sélectionnés<br/><br/>
                             <b>Si interaction significative (p < 0.05) :</b><br/>
                             &#8226; <u>Effets simples de Facteur 1</u> :<br/>
                             &nbsp;&nbsp;&#8594; Compare F1 à chaque niveau de F2<br/>
                             &nbsp;&nbsp;&#8594; Ex: Traitement à T0, T1, T2...<br/><br/>
                             &#8226; <u>Effets simples de Facteur 2</u> :<br/>
                             &nbsp;&nbsp;&#8594; Compare F2 à chaque niveau de F1<br/>
                             &nbsp;&nbsp;&#8594; Ex: Temps pour chaque traitement<br/><br/>
                             <b> Résultats :</b> Onglet 'Effets simples'
                             </small>")
                          ),
                          div(style = "margin-top: 10px; padding: 10px; background-color: #fff9e6; border-radius: 5px; border: 1px dashed #f39c12;",
                              icon("lightbulb", style = "color: #f39c12;"),
                              HTML(" <strong>Test approprié :</strong><br/>
                             <small>
                             &#8226; <b>Paramétrique</b> : utilise le test choisi ci-dessus<br/>
                             &#8226; <b>Non paramétrique</b> : Kruskal-Wallis/Dunn/etc.<br/>
                             &#8594; Même méthode pour effets principaux et simples
                             </small>")
                          )
                        )
                    ),
                    
                    hr(),
                    
                    # Bouton d'exécution - TAILLE STANDARDISÉE
                    actionButton("runMultiple", 
                                 HTML("<h5 style='margin: 5px 0;'><i class='fa fa-play'></i> LANCER L'ANALYSE</h5>"), 
                                 class = "btn-success btn-lg", 
                                 style = "width: 100%; height: 70px; font-weight: bold; box-shadow: 0 4px 6px rgba(0,0,0,0.2);"),
                    
                    br(), br(),
                    
                    # Section Options graphiques - Collapsible
                    div(style = "margin-top: 15px;",
                        tags$button(
                          class = "btn btn-link btn-block",
                          `data-toggle` = "collapse",
                          `data-target` = "#graphOptions",
                          style = "text-align: left; font-weight: bold; color: #2c3e50;",
                          icon("chart-bar"), " Options graphiques avancées ", icon("chevron-down")
                        ),
                        div(id = "graphOptions", class = "collapse",
                            div(style = "padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-top: 10px;",
                                
                                selectInput("boxColor", "Palette de couleurs",
                                            choices = c("Default ggplot2" = "default",
                                                        "Bleu professionnel" = "Blues", 
                                                        "Vert nature" = "Greens", 
                                                        "Rouge intense" = "Reds", 
                                                        "Set1 (coloré)" = "Set1", 
                                                        "Pastel (doux)" = "Pastel1", 
                                                        "Paired (contrasté)" = "Paired"),
                                            selected = "Set1"
                                ),
                                
                                radioButtons("plotType", "Type de graphique",
                                             choices = c("Boxplot" = "box", 
                                                         "Violon" = "violin", 
                                                         "Points + barres" = "point",
                                                         "Barres" = "hist"), 
                                             selected = "box", inline = TRUE),
                                
                                radioButtons("errorType", "Barres d'erreur",
                                             choices = c("Erreur-type (SE)" = "se", 
                                                         "Écart-type (SD)" = "sd", 
                                                         "IC 95%" = "ci",
                                                         "Aucune" = "none"), 
                                             selected = "se", inline = TRUE),
                                
                                checkboxInput("colorByGroups", 
                                              HTML("<b>Colorer par groupes statistiques</b> <small>(a, b, c...)</small>"), 
                                              value = FALSE),
                                
                                hr(),
                                h5("Personnalisation"),
                                textInput("customTitle", "Titre", placeholder = "Auto"),
                                fluidRow(
                                  column(6, textInput("customXLabel", "Axe X", placeholder = "Auto")),
                                  column(6, textInput("customYLabel", "Axe Y", placeholder = "Auto"))
                                ),
                                textInput("customLegendTitle", "Titre légende", placeholder = "Groupes statistiques"),
                                checkboxInput("rotateXLabels", "Rotation labels 45°", value = TRUE),
                                
                                sliderInput("titleSize", "Taille titre", min = 8, max = 32, value = 16, step = 1),
                                sliderInput("axisTitleSize", "Taille axes", min = 8, max = 28, value = 14, step = 1),
                                sliderInput("axisTextSize", "Taille texte axes", min = 6, max = 24, value = 12, step = 1),
                                
                                hr(),
                                h5("Export"),
                                fluidRow(
                                  column(4, numericInput("plotWidth", "Largeur", value = 8, min = 3, max = 20)),
                                  column(4, numericInput("plotHeight", "Hauteur", value = 6, min = 3, max = 20)),
                                  column(4, numericInput("plotDPI", "DPI", value = 300, min = 72, max = 600))
                                ),
                                # BOUTON STANDARDISÉ VERT
                                downloadButton("downloadPlot", "Télécharger PNG", 
                                               class = "btn-success", 
                                               style = "width: 100%; height: 50px; font-weight: bold;",
                                               icon = icon("download"))
                            )
                        )
                    )
                ),
                
                # -------- PANEL DROIT - Résultats 
                box(title = div(icon("table"), " Résultats et visualisations"), 
                    status = "primary", width = 8, solidHeader = TRUE,
                    
                    # Tabs pour organiser les résultats
                    tabsetPanel(id = "resultsTabs", type = "tabs",
                                
                                # ---- TAB 1: Effets principaux 
                                tabPanel(
                                  title = div(icon("layer-group"), " Effets principaux"),
                                  value = "mainEffects",
                                  br(),
                                  conditionalPanel(
                                    condition = "output.showPosthocResults",
                                    div(style = "margin-bottom: 15px;",
                                        uiOutput("analysisSummaryMain")
                                    ),
                                    DTOutput("mainEffectsTable"),
                                    br(),
                                    # BOUTON STANDARDISÉ VERT
                                    downloadButton("downloadMainEffects", 
                                                   "Télécharger effets principaux (.xlsx)", 
                                                   class = "btn-success", 
                                                   style = "width: 100%; height: 50px; font-weight: bold;",
                                                   icon = icon("download"))
                                  )
                                ),
                                
                                # ---- TAB 2: Effets simples - AMÉLIORÉ
                                tabPanel(
                                  title = div(icon("project-diagram"), " Effets simples"),
                                  value = "simpleEffects",
                                  br(),
                                  conditionalPanel(
                                    condition = "output.showSimpleEffects",
                                    # Explication améliorée
                                    div(style = "background: linear-gradient(135deg, #fff5f5 0%, #ffe8e8 100%); padding: 15px; border-radius: 8px; border-left: 5px solid #e74c3c; margin-bottom: 15px;",
                                        h4(style = "color: #c0392b; margin-top: 0;", 
                                           icon("info-circle"), " Interprétation des effets simples"),
                                        HTML("<div style='color: #34495e;'>
                                       <p><b>Objectif :</b> Décomposer les interactions significatives en comparaisons plus simples.</p>
                                       
                                       <p><b>Lecture du format :</b><br/>
                                       <code style='background:#fff;padding:2px 6px;border-radius:3px;'>Facteur testé | Facteur fixé = niveau</code></p>
                                       
                                       <p><b>Exemples concrets :</b></p>
                                       <ul style='margin-left: 20px;'>
                                         <li><code style='background:#fff;padding:2px 6px;'>Traitement | Temps=T0</code><br/>
                                             &#8594; Compare les traitements <u>au temps T0 uniquement</u></li>
                                         <li><code style='background:#fff;padding:2px 6px;'>Temps | Traitement=Ctrl</code><br/>
                                             &#8594; Compare les temps <u>pour le contrôle uniquement</u></li>
                                       </ul>
                                       
                                       <p><b>Utilité :</b> Identifier <i>où précisément</i> les facteurs diffèrent lorsqu'ils interagissent.</p>
                                       </div>")
                                    ),
                                    
                                    # Filtres améliorés
                                    fluidRow(
                                      column(6,
                                             div(style = "background:#f8f9fa; padding:10px; border-radius:5px;",
                                                 selectInput("filterSimpleEffectVar", 
                                                             HTML("<b>Filtrer par variable</b>"),
                                                             choices = NULL,
                                                             width = "100%")
                                             )
                                      ),
                                      column(6,
                                             div(style = "background:#f8f9fa; padding:10px; border-radius:5px;",
                                                 selectInput("filterSimpleEffectInteraction", 
                                                             HTML("<b>Filtrer par interaction</b>"),
                                                             choices = NULL,
                                                             width = "100%")
                                             )
                                      )
                                    ),
                                    
                                    br(),
                                    uiOutput("simpleEffectsSummary"),
                                    DTOutput("simpleEffectsTable"),
                                    br(),
                                    # BOUTON STANDARDISÉ VERT
                                    downloadButton("downloadSimpleEffects", 
                                                   "Télécharger effets simples (.xlsx)", 
                                                   class = "btn-success",
                                                   style = "width: 100%; height: 50px; font-weight: bold;",
                                                   icon = icon("download"))
                                  ),
                                  conditionalPanel(
                                    condition = "!output.showSimpleEffects",
                                    div(style = "text-align: center; padding: 50px; color: #95a5a6;",
                                        icon("project-diagram", style = "font-size: 4em; opacity: 0.3;"),
                                        h4("Aucun effet simple détecté"),
                                        p("Les effets simples apparaissent uniquement quand :"),
                                        tags$ul(style = "text-align: left; display: inline-block;",
                                                tags$li("L'option 'Analyse des interactions' est activée"),
                                                tags$li("Au moins 2 facteurs sont sélectionnés"),
                                                tags$li("Une interaction est significative (p < 0.05)")
                                        )
                                    )
                                  )
                                ),
                                
                                # ---- TAB 3: Visualisations 
                                tabPanel(
                                  title = div(icon("chart-bar"), " Graphiques"),
                                  value = "plots",
                                  br(),
                                  
                                  # Navigation des variables - AMÉLIORÉE
                                  conditionalPanel(
                                    condition = "output.showVariableNavigation",
                                    wellPanel(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; color: white;",
                                              div(style = "display: flex; align-items: center; justify-content: center;",
                                                  uiOutput("variableNavigation")
                                              )
                                    )
                                  ),
                                  
                                  # Sélection du type de graphique - AMÉLIORATION UX
                                  fluidRow(
                                    column(6,
                                           div(style = "background:#e8f4fd; padding:15px; border-radius:8px;",
                                               h5(icon("layer-group"), " Type d'effet"),
                                               selectInput("plotDisplayType", 
                                                           NULL,
                                                           choices = list(
                                                             "Effets principaux" = "main",
                                                             "Effets simples (interactions)" = "simple"
                                                           ),
                                                           selected = "main")
                                           )
                                    ),
                                    column(6,
                                           conditionalPanel(
                                             condition = "input.plotDisplayType == 'simple'",
                                             div(style = "background:#fff5f5; padding:15px; border-radius:8px;",
                                                 h5(icon("filter"), " Sélection effet simple"),
                                                 uiOutput("selectSimpleEffectPlot")
                                             )
                                           )
                                    )
                                  ),
                                  
                                  hr(),
                                  
                                  # Graphique principal
                                  div(style = "background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                      h4(uiOutput("plotTitle"), style = "text-align: center; color: #2c3e50;"),
                                      plotlyOutput("multiPlot", height = "600px"),
                                      br(),
                                      # BOUTON STANDARDISÉ VERT
                                      div(style = "text-align: center;",
                                          downloadButton("downloadMultiPlot", 
                                                         "Télécharger ce graphique (PNG)", 
                                                         class = "btn-success", 
                                                         style = "width: 100%; max-width: 400px; height: 50px; font-weight: bold;",
                                                         icon = icon("image"))
                                      )
                                  )
                                ),
                                
                                # ---- TAB 4: Rapport complet 
                                tabPanel(
                                  title = div(icon("file-alt"), " Rapport"),
                                  value = "report",
                                  br(),
                                  
                                  div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                                      h3(style = "color: #2c3e50;", icon("clipboard-check"), " Résumé de l'analyse"),
                                      hr(),
                                      uiOutput("fullAnalysisReport")
                                  ),
                                  
                                  # Boutons de téléchargement - STANDARDISÉS ET VERTS
                                  div(style = "background: linear-gradient(135deg, #27ae60 0%, #229954 100%); padding: 20px; border-radius: 8px;",
                                      h4(style = "color: white; margin-top: 0;", 
                                         icon("download"), " Téléchargements"),
                                      fluidRow(
                                        column(4,
                                               downloadButton("downloadAllResults", 
                                                              div(icon("file-excel", style = "font-size: 2em; display: block; margin-bottom: 10px;"), 
                                                                  "Toutes les données"),
                                                              class = "btn-light btn-lg",
                                                              style = "width: 100%; height: 120px; font-weight: bold;")
                                        ),
                                        column(4,
                                               downloadButton("downloadSummaryStats", 
                                                              div(icon("chart-pie", style = "font-size: 2em; display: block; margin-bottom: 10px;"), 
                                                                  "Statistiques résumées"),
                                                              class = "btn-light btn-lg",
                                                              style = "width: 100%; height: 120px; font-weight: bold;")
                                        ),
                                        column(4,
                                               downloadButton("downloadFullReport", 
                                                              div(icon("file-pdf", style = "font-size: 2em; display: block; margin-bottom: 10px;"), 
                                                                  "Rapport PDF"),
                                                              class = "btn-light btn-lg",
                                                              style = "width: 100%; height: 120px; font-weight: bold;")
                                        )
                                      )
                                  )
                                )
                    )
                )
              )
      ),
      
      # ---- Analyses multivariees ----
      tabItem(tabName = "multivariate",
              # SECTION ACP
              fluidRow(
                box(title = "Analyse en Composantes Principales (ACP)", status = "info", width = 6, solidHeader = TRUE,
                    
                    uiOutput("pcaVarSelect"),
                    checkboxInput("pcaScale", "Standardiser les variables", TRUE),
                    checkboxInput("pcaUseMeans", "Utiliser les moyennes par groupe", FALSE),
                    conditionalPanel(
                      condition = "input.pcaUseMeans == true",
                      uiOutput("pcaMeansGroupSelect")
                    ),
                    uiOutput("pcaQualiSupSelect"),
                    uiOutput("pcaIndSupSelect"),
                    uiOutput("pcaLabelSourceSelect"),
                    hr(),
                    radioButtons("pcaPlotType", "Type de visualisation:",
                                 choices = c("Variables" = "var", "Individus" = "ind", "Biplot" = "biplot"),
                                 selected = "var", inline = TRUE),
                    numericInput("pcaComponents", "Nombre de composantes:", value = 5, min = 2, max = 10),
                    hr(),
                    h5("Personnalisation graphique:", style = "font-weight: bold; color: #337ab7;"),
                    textInput("pcaPlotTitle", "Titre du graphique:", 
                              value = "ACP - Analyse en Composantes Principales"),
                    textInput("pcaXLabel", "Label axe X:", value = ""),
                    textInput("pcaYLabel", "Label axe Y:", value = ""),
                    checkboxInput("pcaCenterAxes", "Centrer sur (0,0)", TRUE),
                    hr(),
                    h5("Options de telechargement graphique:", style = "font-weight: bold; color: #337ab7;"),
                    fluidRow(
                      column(4,
                             selectInput("pcaPlot_format", "Format:",
                                         choices = c("png", "jpg", "jpeg", "tiff", "bmp", "svg", "pdf", "eps"),
                                         selected = "png")
                      ),
                      column(4,
                             numericInput("pcaPlot_width", "Largeur (cm):", value = 25, min = 5, max = 100)
                      ),
                      column(4,
                             numericInput("pcaPlot_height", "Hauteur (cm):", value = 20, min = 5, max = 100)
                      )
                    ),
                    numericInput("pcaPlot_dpi", "Resolution (DPI):", value = 300, min = 72, max = 1200),
                    hr(),
                    div(style = "text-align: center;",
                        downloadButton("downloadPcaPlot", "Telecharger graphique", class = "btn-info", style = "margin: 5px;"),
                        br(), br(),
                        downloadButton("downloadPcaDataXlsx", "Telecharger donnees (Excel)", class = "btn-success", style = "margin: 5px;"),
                        downloadButton("downloadPcaDataCsv", "Telecharger donnees (CSV)", class = "btn-success", style = "margin: 5px;")
                    )
                ),
                box(title = "Visualisation ACP", status = "info", width = 6, solidHeader = TRUE,
                    plotlyOutput("pcaPlot", height = "550px"),
                    hr(),
                    div(style = "max-height: 300px; overflow-y: auto; font-size: 12px;",
                        verbatimTextOutput("pcaSummary"))
                )
              ),
              
              # SECTION HCPC
              fluidRow(
                box(title = "Classification Hierarchique sur Composantes Principales (HCPC)", 
                    status = "success", width = 12, solidHeader = TRUE,
                    p("Cette analyse combine l'ACP avec une classification hierarchique automatique."),
                    
                    fluidRow(
                      column(4,
                             numericInput("hcpcClusters", "Nombre de clusters:", value = 3, min = 2, max = 10)
                      ),
                      column(8,
                             div(style = "text-align: center; margin-top: 25px;",
                                 downloadButton("downloadHcpcDataXlsx", "Telecharger donnees (Excel)", 
                                                class = "btn-success", style = "margin: 5px;"),
                                 downloadButton("downloadHcpcDataCsv", "Telecharger donnees (CSV)", 
                                                class = "btn-success", style = "margin: 5px;")
                             )
                      )
                    ),
                    hr(),
                    h5("Personnalisation graphique:", style = "font-weight: bold; color: #5cb85c;"),
                    fluidRow(
                      column(6,
                             textInput("hcpcClusterTitle", "Titre carte des clusters:", 
                                       value = "Carte des clusters HCPC"),
                             textInput("hcpcClusterXLabel", "Label axe X:", value = ""),
                             textInput("hcpcClusterYLabel", "Label axe Y:", value = ""),
                             checkboxInput("hcpcCenterAxes", "Centrer sur (0,0)", TRUE),
                             hr(),
                             h5("Options telechargement carte clusters:"),
                             fluidRow(
                               column(6,
                                      selectInput("hcpcCluster_format", "Format:",
                                                  choices = c("png", "jpg", "jpeg", "tiff", "bmp", "svg", "pdf", "eps"),
                                                  selected = "png")
                               ),
                               column(6,
                                      numericInput("hcpcCluster_dpi", "DPI:", value = 300, min = 72, max = 2000)
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("hcpcCluster_width", "Largeur (cm):", value = 25, min = 5, max = 100)
                               ),
                               column(6,
                                      numericInput("hcpcCluster_height", "Hauteur (cm):", value = 20, min = 5, max = 100)
                               )
                             )
                      ),
                      column(6,
                             textInput("hcpcDendTitle", "Titre dendrogramme:", 
                                       value = "Dendrogramme HCPC"),
                             p(style = "font-style: italic; color: #666;", 
                               "Le dendrogramme n'est pas centre sur (0,0)"),
                             hr(),
                             h5("Options telechargement dendrogramme:"),
                             fluidRow(
                               column(6,
                                      selectInput("hcpcDend_format", "Format:",
                                                  choices = c("png", "jpg", "jpeg", "tiff", "bmp", "svg", "pdf", "eps"),
                                                  selected = "png")
                               ),
                               column(6,
                                      numericInput("hcpcDend_dpi", "DPI:", value = 300, min = 72, max = 20000)
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("hcpcDend_width", "Largeur (cm):", value = 30, min = 5, max = 100)
                               ),
                               column(6,
                                      numericInput("hcpcDend_height", "Hauteur (cm):", value = 20, min = 5, max = 100)
                               )
                             )
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(6,
                             div(class = "box box-solid box-success",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Carte des clusters")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("hcpcClusterPlot", height = "500px"),
                                     downloadButton("downloadHcpcClusterPlot", "Telecharger carte")
                                 )
                             )
                      ),
                      column(6,
                             div(class = "box box-solid box-success",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Dendrogramme")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("hcpcDendPlot", height = "500px"),
                                     downloadButton("downloadHcpcDendPlot", "Telecharger dendrogramme")
                                 )
                             )
                      )
                    ),
                    br(),
                    div(class = "box box-solid",
                        div(class = "box-header with-border", style = "background-color: #5cb85c; color: white;",
                            h4(class = "box-title", "Resultats detailles HCPC", style = "color: white; font-weight: bold;")
                        ),
                        div(class = "box-body", style = "background-color: #f9f9f9;",
                            div(style = "max-height: 500px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                verbatimTextOutput("hcpcSummary"))
                        )
                    )
                )
              ),
              
              # SECTION AFD
              fluidRow(
                box(title = "Analyse Factorielle Discriminante (AFD)", 
                    status = "primary", width = 12, solidHeader = TRUE,
                    
                    fluidRow(
                      column(4,
                             uiOutput("afdFactorSelect")),
                      column(8,
                             uiOutput("afdVarSelect"))
                    ),
                    checkboxInput("afdUseMeans", "Utiliser les moyennes par groupe", FALSE),
                    conditionalPanel(
                      condition = "input.afdUseMeans == true",
                      uiOutput("afdMeansGroupSelect")
                    ),
                    div(style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin: 15px 0;",
                        checkboxInput("afdCrossValidation", 
                                      HTML("<strong>Activer la validation croisee (Leave-One-Out)</strong>"), 
                                      FALSE),
                        p(style = "margin: 5px 0 0 25px; font-size: 12px; color: #856404;",
                          icon("exclamation-triangle"), 
                          " ATTENTION : La validation croisee peut etre tres longue sur de grands jeux de donnees.")
                    ),
                    hr(),
                    h5("Personnalisation graphique:", style = "font-weight: bold; color: #337ab7;"),
                    fluidRow(
                      column(6,
                             textInput("afdIndTitle", "Titre projection individus:", 
                                       value = "AFD - Projection des individus"),
                             textInput("afdIndXLabel", "Label axe X:", value = ""),
                             textInput("afdIndYLabel", "Label axe Y:", value = ""),
                             checkboxInput("afdIndCenterAxes", "Centrer sur (0,0)", TRUE),
                             hr(),
                             h5("Options telechargement projection individus:"),
                             fluidRow(
                               column(6,
                                      selectInput("afdInd_format", "Format:",
                                                  choices = c("png", "jpg", "jpeg", "tiff", "bmp", "svg", "pdf", "eps"),
                                                  selected = "png")
                               ),
                               column(6,
                                      numericInput("afdInd_dpi", "DPI:", value = 300, min = 72, max = 20000)
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("afdInd_width", "Largeur (cm):", value = 25, min = 5, max = 100)
                               ),
                               column(6,
                                      numericInput("afdInd_height", "Hauteur (cm):", value = 20, min = 5, max = 100)
                               )
                             )
                      ),
                      column(6,
                             textInput("afdVarTitle", "Titre contribution variables:", 
                                       value = "AFD - Contribution des variables"),
                             textInput("afdVarXLabel", "Label axe X:", value = ""),
                             textInput("afdVarYLabel", "Label axe Y:", value = ""),
                             checkboxInput("afdVarCenterAxes", "Centrer sur (0,0)", TRUE),
                             hr(),
                             h5("Options telechargement contribution variables:"),
                             fluidRow(
                               column(6,
                                      selectInput("afdVar_format", "Format:",
                                                  choices = c("png", "jpg", "jpeg", "tiff", "bmp", "svg", "pdf", "eps"),
                                                  selected = "png")
                               ),
                               column(6,
                                      numericInput("afdVar_dpi", "DPI:", value = 300, min = 72, max = 20000)
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("afdVar_width", "Largeur (cm):", value = 25, min = 5, max = 100)
                               ),
                               column(6,
                                      numericInput("afdVar_height", "Hauteur (cm):", value = 20, min = 5, max = 100)
                               )
                             )
                      )
                    ),
                    hr(),
                    div(style = "text-align: center;",
                        downloadButton("downloadAfdDataXlsx", "Telecharger donnees (Excel)", 
                                       class = "btn-success", style = "margin: 5px;"),
                        downloadButton("downloadAfdDataCsv", "Telecharger donnees (CSV)", 
                                       class = "btn-success", style = "margin: 5px;")
                    ),
                    hr(),
                    fluidRow(
                      column(6,
                             div(class = "box box-solid box-primary",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Projection des individus", style = "color: #fff;")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("afdIndPlot", height = "500px"),
                                     downloadButton("downloadAfdIndPlot", "Telecharger projection")
                                 )
                             )
                      ),
                      column(6,
                             div(class = "box box-solid box-primary",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Contribution des variables", style = "color: #fff;")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("afdVarPlot", height = "500px"),
                                     downloadButton("downloadAfdVarPlot", "Telecharger contribution")
                                 )
                             )
                      )
                    ),
                    br(),
                    div(class = "box box-solid",
                        div(class = "box-header with-border", style = "background-color: #d9534f; color: white;",
                            h4(class = "box-title", "Resultats detailles de l'AFD", style = "color: white; font-weight: bold;")
                        ),
                        div(class = "box-body", style = "background-color: #f9f9f9;",
                            div(style = "max-height: 700px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                verbatimTextOutput("afdSummary"))
                        )
                    )
                )
              )
      ),
      
      # ---- Seuils d'efficacité ----
      tabItem(tabName = "threshold",
              # Message d'information sur la mise à jour automatique
              fluidRow(
                box(width = 12, status = "info", solidHeader = FALSE,
                    icon("info-circle"), 
                    strong(" Mode mise à jour automatique activé : "),
                    "Les modifications sont appliquées instantanément au graphique. Aucun besoin de cliquer sur un bouton !",
                    style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px; border-radius: 8px; margin-bottom: 15px;")
              ),
              
              fluidRow(
                box(title = tagList(icon("sliders"), " Configuration de l'analyse"), 
                    status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,
                    
                    # Section Variables
                    div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 12px; border-radius: 8px; margin-bottom: 15px;",
                        h5(icon("database"), " Sélection des variables", 
                           style = "color: white; font-weight: bold; margin: 0;")
                    ),
                    
                    uiOutput("thresholdXVarSelect"),
                    
                    h6(icon("chart-line"), " Variables Y (Efficacité)", 
                       style = "font-weight: bold; color: #3c8dbc; margin-top: 15px;"),
                    checkboxInput("thresholdMultipleY", 
                                  tagList(icon("layer-group"), " Activer la sélection multiple de Y"), 
                                  value = FALSE),
                    uiOutput("thresholdYVarSelect"),
                    
                    # Info sur les couleurs pour Y multiple
                    conditionalPanel(
                      condition = "input.thresholdMultipleY && input.thresholdYVar && input.thresholdYVar.length > 1",
                      div(style = "background-color: #e3f2fd; padding: 12px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #2196F3;",
                          icon("palette", style = "color: #2196F3;"),
                          strong(" Info : "), 
                          "Les couleurs des variables Y multiples utilisent automatiquement la palette ggplot2 par défaut pour une meilleure distinction visuelle."
                      )
                    ),
                    
                    hr(style = "border-top: 2px solid #3c8dbc; margin: 20px 0;"),
                    
                    # Section Seuil
                    div(style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); padding: 12px; border-radius: 8px; margin-bottom: 15px;",
                        h5(icon("bullseye"), " Paramètres du seuil", 
                           style = "color: white; font-weight: bold; margin: 0;")
                    ),
                    
                    numericInput("thresholdValue", 
                                 tagList(icon("percent"), " Valeur du seuil (%)"), 
                                 value = 80, min = 0, max = 100, step = 1),
                    
                    fluidRow(
                      column(6,
                             colourInput("thresholdColor", "Couleur de la ligne:", 
                                         value = "#e74c3c", showColour = "background")
                      ),
                      column(6,
                             numericInput("thresholdLineWidth", "Épaisseur:", 
                                          value = 1.5, min = 0.5, max = 5, step = 0.5)
                      )
                    ),
                    
                    selectInput("thresholdLineType", "Type de ligne:",
                                choices = c("Solide" = "solid",
                                            "Pointillé" = "dotted",
                                            "Tirets" = "dashed",
                                            "Tirets-points" = "dotdash",
                                            "Tirets longs" = "longdash",
                                            "Deux tirets" = "twodash"),
                                selected = "solid"),
                    
                    hr(style = "border-top: 2px solid #f39c12; margin: 20px 0;"),
                    
                    # Section Filtrage
                    div(style = "background: linear-gradient(135deg, #ffecd2 0%, #fcb69f 100%); padding: 12px; border-radius: 8px; margin-bottom: 15px;",
                        h5(icon("filter"), " Filtrage des données", 
                           style = "color: #d35400; font-weight: bold; margin: 0;")
                    ),
                    
                    uiOutput("thresholdFilterSelect"),
                    
                    hr(style = "border-top: 2px solid #27ae60; margin: 20px 0;"),
                    
                    # Section Éditeur de labels X
                    div(style = "background: linear-gradient(135deg, #a8edea 0%, #fed6e3 100%); padding: 12px; border-radius: 8px; margin-bottom: 15px;",
                        h5(icon("tag"), " Personnalisation des labels X", 
                           style = "color: #16a085; font-weight: bold; margin: 0;")
                    ),
                    
                    div(style = "background-color: #fff9e6; padding: 10px; border-radius: 6px; margin-bottom: 10px; border-left: 4px solid #f39c12;",
                        icon("lightbulb", style = "color: #f39c12;"),
                        em(" Astuce : Modifiez les étiquettes des traitements et appliquez des styles (gras/italique) pour une meilleure présentation.")
                    ),
                    
                    uiOutput("thresholdLevelsEditor"),
                    
                    # Section Éditeur de labels de légende (Y multiples)
                    conditionalPanel(
                      condition = "input.thresholdMultipleY && input.thresholdYVar && input.thresholdYVar.length > 1",
                      hr(style = "border-top: 2px solid #9b59b6; margin: 20px 0;"),
                      
                      div(style = "background: linear-gradient(135deg, #da22ff 0%, #9733ee 100%); padding: 12px; border-radius: 8px; margin-bottom: 15px;",
                          h5(icon("list-ul"), " Personnalisation des labels de légende", 
                             style = "color: white; font-weight: bold; margin: 0;")
                      ),
                      
                      div(style = "background-color: #f3e5f5; padding: 10px; border-radius: 6px; margin-bottom: 10px; border-left: 4px solid #9b59b6;",
                          icon("info-circle", style = "color: #9b59b6;"),
                          em(" Info : Personnalisez les étiquettes affichées dans la légende pour les variables Y sélectionnées.")
                      ),
                      
                      uiOutput("thresholdLegendEditor")
                    ),
                    
                    hr(style = "border-top: 2px solid #8e44ad; margin: 20px 0;"),
                    
                    # Section Options graphiques
                    div(style = "background: linear-gradient(135deg, #a18cd1 0%, #fbc2eb 100%); padding: 12px; border-radius: 8px; margin-bottom: 15px;",
                        h5(icon("palette"), " Options graphiques avancées", 
                           style = "color: white; font-weight: bold; margin: 0;")
                    ),
                    
                    # Titres et labels
                    div(style = "background-color: #f9f9f9; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #e0e0e0;",
                        h6(icon("heading"), " Titres et étiquettes", 
                           style = "font-weight: bold; color: #34495e; margin-bottom: 10px;"),
                        textInput("thresholdPlotTitle", "Titre du graphique:", 
                                  value = "Analyse des seuils d'efficacité"),
                        textInput("thresholdXLabel", "Label axe X:", 
                                  value = "", placeholder = "Par défaut: Traitements"),
                        textInput("thresholdYLabel", "Label axe Y:", 
                                  value = "", placeholder = "Par défaut: Seuil d'efficacité (%)")
                    ),
                    
                    # Style des labels d'axes
                    div(style = "background-color: #fff8e1; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #ffd54f;",
                        h6(icon("font"), " Style des labels d'axes", 
                           style = "font-weight: bold; color: #f57f17; margin-bottom: 10px;"),
                        
                        div(style = "margin-bottom: 10px;",
                            strong("Label axe X:"),
                            div(style = "margin-left: 15px; margin-top: 5px; display: flex; gap: 15px;",
                                checkboxInput("thresholdXLabelBold", "Gras", value = FALSE),
                                checkboxInput("thresholdXLabelItalic", "Italique", value = FALSE)
                            )
                        ),
                        
                        div(
                          strong("Label axe Y:"),
                          div(style = "margin-left: 15px; margin-top: 5px; display: flex; gap: 15px;",
                              checkboxInput("thresholdYLabelBold", "Gras", value = FALSE),
                              checkboxInput("thresholdYLabelItalic", "Italique", value = FALSE)
                          )
                        )
                    ),
                    
                    # Couleurs des barres (une seule variable Y)
                    conditionalPanel(
                      condition = "!input.thresholdMultipleY || (input.thresholdYVar && input.thresholdYVar.length == 1)",
                      div(style = "background-color: #e3f2fd; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #90caf9;",
                          h6(icon("paint-brush"), " Couleurs des barres", 
                             style = "font-weight: bold; color: #1565c0; margin-bottom: 10px;"),
                          
                          checkboxInput("thresholdUseColor", 
                                        tagList(icon("palette"), " Personnaliser les couleurs"), 
                                        value = TRUE),
                          
                          conditionalPanel(
                            condition = "input.thresholdUseColor",
                            radioButtons("thresholdBarColor", "Type de coloration:",
                                         choices = c("ggplot2 (défaut)" = "ggplot",
                                                     "Palette prédéfinie" = "palette",
                                                     "Personnalisé par traitement" = "custom",
                                                     "Couleur unique" = "single",
                                                     "Noir (monochrome)" = "black"),
                                         selected = "ggplot"),
                            
                            conditionalPanel(
                              condition = "input.thresholdBarColor == 'palette'",
                              selectInput("thresholdPalette", "Choisir une palette:",
                                          choices = list(
                                            "Palettes qualitatives" = c("Set1" = "Set1", "Set2" = "Set2", "Set3" = "Set3",
                                                                        "Pastel1" = "Pastel1", "Pastel2" = "Pastel2",
                                                                        "Paired" = "Paired", "Dark2" = "Dark2", "Accent" = "Accent"),
                                            "Palettes divergentes" = c("Spectral" = "Spectral", "RdYlBu" = "RdYlBu", "RdBu" = "RdBu"),
                                            "Palettes séquentielles" = c("Blues" = "Blues", "Greens" = "Greens", 
                                                                         "Oranges" = "Oranges", "Purples" = "Purples")
                                          ),
                                          selected = "Set1")
                            ),
                            
                            conditionalPanel(
                              condition = "input.thresholdBarColor == 'custom'",
                              div(style = "max-height: 300px; overflow-y: auto; padding: 5px;",
                                  uiOutput("thresholdColorPickers")
                              )
                            ),
                            
                            conditionalPanel(
                              condition = "input.thresholdBarColor == 'single'",
                              colourInput("thresholdSingleBarColor", "Couleur des barres:", 
                                          value = "#3498db", showColour = "background")
                            )
                          )
                      )
                    ),
                    
                    # Dimensions des barres - AMÉLIORÉ
                    div(style = "background-color: #f0f8ff; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #b3d9ff;",
                        h6(icon("arrows-alt-h"), " Dimensions et espacement des barres", 
                           style = "font-weight: bold; color: #1e3a8a; margin-bottom: 10px;"),
                        
                        sliderInput("thresholdBarWidth", "Largeur des barres:", 
                                    min = 0.1, max = 1, value = 0.8, step = 0.05),
                        
                        conditionalPanel(
                          condition = "input.thresholdMultipleY && input.thresholdYVar && input.thresholdYVar.length > 1",
                          
                          sliderInput("thresholdBarSpacing", 
                                      tagList(icon("arrows-alt-h"), " Espacement entre barres:"), 
                                      min = 0, max = 0.5, value = 0.1, step = 0.05),
                          
                          div(style = "background-color: #e8f5e9; padding: 8px; border-radius: 4px; margin-top: 10px; border-left: 3px solid #4caf50;",
                              icon("info-circle", style = "color: #388e3c;"),
                              tags$small(" Plus l'espacement est élevé, plus les groupes de barres sont espacés.")
                          ),
                          
                          radioButtons("thresholdBarPosition", "Position des barres:",
                                       choices = c("Côte à côte" = "dodge",
                                                   "Empilées" = "stack"),
                                       selected = "dodge", inline = TRUE)
                        )
                    ),
                    
                    # Légende - AMÉLIORÉ
                    div(style = "background-color: #fff3e0; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #ffb74d;",
                        h6(icon("list"), " Configuration de la légende", 
                           style = "font-weight: bold; color: #e65100; margin-bottom: 10px;"),
                        
                        checkboxInput("thresholdShowLegend", 
                                      tagList(icon("eye"), " Afficher la légende"), 
                                      value = TRUE),
                        
                        conditionalPanel(
                          condition = "input.thresholdShowLegend",
                          textInput("thresholdLegendTitle", "Titre de la légende:", 
                                    value = "", placeholder = "Laisser vide pour défaut"),
                          
                          selectInput("thresholdLegendPosition", "Position:",
                                      choices = c("En bas" = "bottom",
                                                  "En haut" = "top",
                                                  "À gauche" = "left",
                                                  "À droite" = "right",
                                                  "Coin supérieur droit" = "top_right",
                                                  "Coin supérieur gauche" = "top_left",
                                                  "Coin inférieur droit" = "bottom_right",
                                                  "Coin inférieur gauche" = "bottom_left"),
                                      selected = "bottom"),
                          
                          div(style = "display: flex; gap: 15px; margin-top: 5px;",
                              checkboxInput("thresholdLegendBold", "Titre en gras", value = TRUE),
                              checkboxInput("thresholdLegendItalic", "Titre en italique", value = FALSE)
                          )
                        )
                    ),
                    
                    # Apparence des axes
                    div(style = "background-color: #f5f5f5; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #cccccc;",
                        h6(icon("ruler"), " Apparence des axes", 
                           style = "font-weight: bold; color: #424242; margin-bottom: 10px;"),
                        
                        checkboxInput("thresholdBlackAxes", 
                                      tagList(icon("paint-roller"), " Axes en noir (sinon gris)"), 
                                      value = TRUE),
                        checkboxInput("thresholdShowAxisLines", 
                                      tagList(icon("minus"), " Afficher les lignes d'axes"), 
                                      value = TRUE),
                        checkboxInput("thresholdShowTicks", 
                                      tagList(icon("grip-lines"), " Afficher les graduations"), 
                                      value = TRUE),
                        checkboxInput("thresholdShowGrid", 
                                      tagList(icon("th"), " Afficher la grille"), 
                                      value = TRUE),
                        checkboxInput("thresholdRotateLabels", 
                                      tagList(icon("undo"), " Incliner labels X à 45°"), 
                                      value = TRUE)
                    ),
                    
                    # Tailles de texte
                    div(style = "background-color: #fce4ec; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #f48fb1;",
                        h6(icon("text-height"), " Tailles de texte", 
                           style = "font-weight: bold; color: #c2185b; margin-bottom: 10px;"),
                        
                        sliderInput("thresholdTitleSize", "Titre:", 
                                    min = 8, max = 28, value = 16, step = 1),
                        sliderInput("thresholdAxisTitleSize", "Titres des axes:", 
                                    min = 8, max = 24, value = 14, step = 1),
                        sliderInput("thresholdAxisTextSize", "Texte des axes:", 
                                    min = 6, max = 20, value = 12, step = 1),
                        sliderInput("thresholdLegendSize", "Légende:", 
                                    min = 6, max = 20, value = 10, step = 1)
                    ),
                    
                    # Limites de l'axe Y
                    div(style = "background-color: #e8f5e9; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #81c784;",
                        h6(icon("arrows-alt-v"), " Limites de l'axe Y", 
                           style = "font-weight: bold; color: #2e7d32; margin-bottom: 10px;"),
                        
                        fluidRow(
                          column(6,
                                 numericInput("thresholdYMin", "Minimum:", 
                                              value = 0, min = 0, max = 100)
                          ),
                          column(6,
                                 numericInput("thresholdYMax", "Maximum:", 
                                              value = 100, min = 0, max = 200)
                          )
                        )
                    )
                ),
                
                # Panel principal du graphique
                box(title = tagList(icon("chart-bar"), " Graphique des seuils d'efficacité"), 
                    status = "primary", width = 8, solidHeader = TRUE, collapsible = TRUE,
                    
                    plotlyOutput("thresholdPlot", height = "600px"),
                    
                    br(),
                    hr(style = "border-top: 2px solid #3c8dbc;"),
                    
                    # Section Export améliorée
                    div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                        h4(icon("download"), " Options d'exportation haute qualité", 
                           style = "color: white; font-weight: bold; margin: 0;")
                    ),
                    
                    # Avertissement DPI
                    div(style = "background-color: #fff3cd; padding: 12px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #ff9800;",
                        icon("exclamation-triangle", style = "color: #f57c00;"),
                        strong(" Important : "),
                        "Le DPI peut aller jusqu'à 20000 pour les formats raster. Au-delà de 1200 DPI, les fichiers deviennent très volumineux. Pour une qualité optimale sans limite, utilisez les formats vectoriels (SVG, PDF, EPS)."
                    ),
                    
                    # Préréglages rapides
                    div(style = "background-color: #e8eaf6; padding: 15px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #5c6bc0;",
                        h6(icon("magic"), " Préréglages rapides", 
                           style = "font-weight: bold; color: #3f51b5; margin-bottom: 10px;"),
                        
                        fluidRow(
                          column(3,
                                 actionButton("presetWeb", "Web/Écran",
                                              class = "btn-info btn-block",
                                              icon = icon("desktop"),
                                              style = "margin-bottom: 5px;",
                                              onclick = "Shiny.setInputValue('thresholdExportWidth', 1920); Shiny.setInputValue('thresholdExportHeight', 1080); Shiny.setInputValue('thresholdExportDPI', 150); Shiny.setInputValue('thresholdExportFormat', 'png');"),
                                 tags$small("1920×1080 px, 150 DPI", style = "color: #666;")
                          ),
                          column(3,
                                 actionButton("presetPrint", "Impression",
                                              class = "btn-success btn-block",
                                              icon = icon("print"),
                                              style = "margin-bottom: 5px;",
                                              onclick = "Shiny.setInputValue('thresholdExportWidth', 3000); Shiny.setInputValue('thresholdExportHeight', 2000); Shiny.setInputValue('thresholdExportDPI', 300); Shiny.setInputValue('thresholdExportFormat', 'png');"),
                                 tags$small("3000×2000 px, 300 DPI", style = "color: #666;")
                          ),
                          column(3,
                                 actionButton("presetPublication", "Publication",
                                              class = "btn-warning btn-block",
                                              icon = icon("book"),
                                              style = "margin-bottom: 5px;",
                                              onclick = "Shiny.setInputValue('thresholdExportWidth', 5000); Shiny.setInputValue('thresholdExportHeight', 3500); Shiny.setInputValue('thresholdExportDPI', 600); Shiny.setInputValue('thresholdExportFormat', 'tiff');"),
                                 tags$small("5000×3500 px, 600 DPI", style = "color: #666;")
                          ),
                          column(3,
                                 actionButton("presetVectoriel", "Vectoriel",
                                              class = "btn-danger btn-block",
                                              icon = icon("bezier-curve"),
                                              style = "margin-bottom: 5px;",
                                              onclick = "Shiny.setInputValue('thresholdExportWidth', 2000); Shiny.setInputValue('thresholdExportHeight', 1500); Shiny.setInputValue('thresholdExportDPI', 300); Shiny.setInputValue('thresholdExportFormat', 'svg');"),
                                 tags$small("SVG - Infinie", style = "color: #666;")
                          )
                        )
                    ),
                    
                    # Paramètres d'export détaillés
                    div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
                        h6(icon("cogs"), " Paramètres personnalisés", 
                           style = "font-weight: bold; color: #424242; margin-bottom: 10px;"),
                        
                        fluidRow(
                          column(4,
                                 numericInput("thresholdExportWidth", 
                                              tagList(icon("arrows-alt-h"), " Largeur (pixels)"), 
                                              value = 1200, min = 400, max = 20000, step = 100)
                          ),
                          column(4,
                                 numericInput("thresholdExportHeight", 
                                              tagList(icon("arrows-alt-v"), " Hauteur (pixels)"), 
                                              value = 800, min = 400, max = 20000, step = 100)
                          ),
                          column(4,
                                 numericInput("thresholdExportDPI", 
                                              tagList(icon("crosshairs"), " Résolution (DPI)"), 
                                              value = 300, min = 72, max = 20000, step = 50)
                          )
                        ),
                        
                        # Calcul automatique de la taille
                        div(style = "background-color: #e1f5fe; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #0288d1;",
                            icon("info-circle", style = "color: #01579b;"),
                            strong(" Aperçu : "),
                            textOutput("exportSizeEstimate", inline = TRUE)
                        )
                    ),
                    
                    # Format d'export
                    fluidRow(
                      column(12,
                             selectInput("thresholdExportFormat", 
                                         tagList(icon("file-image"), " Format d'export"),
                                         choices = list(
                                           "Formats raster (pixels)" = c("PNG (recommandé)" = "png",
                                                                         "JPEG (compressé)" = "jpeg",
                                                                         "TIFF (haute qualité)" = "tiff",
                                                                         "BMP (non compressé)" = "bmp"),
                                           "Formats vectoriels (résolution infinie)" = c("SVG (web, idéal)" = "svg",
                                                                                         "PDF (publication)" = "pdf",
                                                                                         "EPS (impression pro)" = "eps")
                                         ),
                                         selected = "png")
                      )
                    ),
                    
                    # Guide des formats - AMÉLIORÉ
                    div(style = "background-color: #e3f2fd; padding: 12px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #2196f3;",
                        icon("question-circle", style = "color: #1976d2;"),
                        strong(" Guide des formats : "), br(),
                        tags$ul(style = "margin-bottom: 0; padding-left: 20px; line-height: 1.8;",
                                tags$li(tags$strong("PNG :"), " Idéal pour PowerPoint, web, réseaux sociaux. Sans perte, fond transparent possible."),
                                tags$li(tags$strong("JPEG :"), " Fichier plus petit mais avec compression. Pour emails, partage rapide."),
                                tags$li(tags$strong("TIFF :"), " Maximum de qualité. Pour impression professionnelle (magazines, posters)."),
                                tags$li(tags$strong("SVG :"), " ⭐ Vectoriel, redimensionnable à l'infini ! Parfait pour web et édition ultérieure."),
                                tags$li(tags$strong("PDF/EPS :"), " Standards scientifiques. Requis pour publications académiques.")
                        )
                    ),
                    
                    # Conseils d'utilisation
                    div(style = "background-color: #f1f8e9; padding: 12px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #8bc34a;",
                        icon("lightbulb", style = "color: #689f38;"),
                        strong(" Conseils : "),
                        tags$ul(style = "margin-bottom: 0; padding-left: 20px; margin-top: 5px;",
                                tags$li("Pour présentation : PNG 1920×1080 à 150 DPI"),
                                tags$li("Pour impression A4 : PNG 3000×2000 à 300 DPI"),
                                tags$li("Pour poster : TIFF 5000+ pixels à 600 DPI"),
                                tags$li("Pour publication : SVG ou PDF (résolution infinie)"),
                                tags$li("Limite recommandée : 5000×5000 px à 600 DPI pour formats raster")
                        )
                    ),
                    
                    # Bouton de téléchargement stylisé
                    downloadButton("downloadThresholdPlot", 
                                   tagList(icon("download"), " Télécharger le graphique"), 
                                   class = "btn-success btn-lg btn-block", 
                                   style = "font-size: 18px; font-weight: bold; padding: 15px; background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); border: none; box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
                )
              ),
              
              # Tableau des données
              fluidRow(
                box(title = tagList(icon("table"), " Tableau des données utilisées"), 
                    status = "info", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    
                    div(style = "background-color: #fff9e6; padding: 12px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #ffa726;",
                        icon("info-circle", style = "color: #f57c00;"),
                        strong(" Information : "),
                        "Ce tableau affiche les données filtrées et transformées utilisées pour générer le graphique. ",
                        "Vous pouvez copier, exporter en CSV ou Excel directement depuis le tableau."
                    ),
                    
                    DTOutput("thresholdDataTable"),
                    
                    br(),
                    
                    downloadButton("downloadThresholdData", 
                                   tagList(icon("file-excel"), " Télécharger données complètes (Excel)"), 
                                   class = "btn-info btn-lg",
                                   style = "font-size: 16px; font-weight: bold; padding: 12px 24px;")
                )
              )
      ),
      
      # ---- Rapport ----
      tabItem(tabName = "report",
              fluidRow(
                box(title = "Génération de rapport", status = "success", width = 12, solidHeader = TRUE,
                    textInput("reportTitle", "Titre du rapport:", value = "Analyse Statistique"),
                    textInput("reportAuthor", "Auteur:", value = ""),
                    selectInput("reportFormat", "Format du rapport:",
                                choices = c("HTML" = "html_document", "PDF" = "pdf_document", "Word" = "word_document")),
                    actionButton("generateReport", "Générer le rapport", class = "btn-success", icon = icon("file-pdf")),
                    downloadButton("downloadReport", "Télécharger le rapport", class = "btn-info")
                )
              ),
              fluidRow(
                box(title = "Aperçu du rapport", status = "info", width = 12, solidHeader = TRUE,
                    uiOutput("reportPreview")
                )
              )
      )
    )
  )
)

################################################################################
#
# Server
# Ce Bloc définit la logique serveur de l'application Shiny
#
################################################################################

server <- function(input, output, session) {
  values <- reactiveValues(
    data = NULL, cleanData = NULL, filteredData = NULL, descStats = NULL,
    normResults = NULL, leveneResults = NULL, testResults = NULL,
    anovaModel = NULL, lastKruskal = NULL, multiResults = NULL, multiGroups = NULL,
    currentPlot = NULL, residualsNorm = NULL, leveneResid = NULL,
    multiNormResults = NULL, multiLeveneResults = NULL,
    pcaResult = NULL, clusterResult = NULL, currentModel = NULL,
    testInterpretation = NULL, cahResult = NULL, currentInteractivePlot = NULL,
    cahClusters = NULL,
    testResultsDF = NULL,
    multiResultsMain = NULL, multiResultsInteraction = NULL,
    normalityResults = NULL, homogeneityResults = NULL,
    currentVarIndex = 1, currentValidationVar = 1,
    allTestResults = list(),  # To accumulate multiple test results for Excel
    allPostHocResults = list(),  # To accumulate multiple post-hoc results for Excel
    modelsList = list(),  # Models per variable for diagnostics/resid
    normalityResultsPerVar = list(), homogeneityResultsPerVar = list(),  # Per variable for validation
    currentDiagVar = 1, currentResidVar = 1  # Navigation for diagnostics and resid
  )
  
  # ---- Aide et réinitialisation ----
  observeEvent(input$helpBtn, {
    shinyalert(
      title = "Aide",
      text = "Cette application permet d'analyser statistiquement des données expérimentales. 
              Naviguez à travers les onglets de gauche pour charger, explorer, nettoyer, filtrer et analyser vos données.
              Utilisez le bouton 'Exemple' pour télécharger un jeu de données d'exemple.",
      type = "info"
    )
  })
  
  observeEvent(input$resetBtn, {
    shinyalert(
      title = "Réinitialiser",
      text = "Êtes-vous sûr de vouloir réinitialiser l'application ? Toutes les données seront perdues.",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Oui",
      cancelButtonText = "Non",
      callbackR = function(value) {
        if (value) {
          values$data <- NULL
          values$cleanData <- NULL
          values$filteredData <- NULL
          values$descStats <- NULL
          values$normResults <- NULL
          values$leveneResults <- NULL
          values$testResults <- NULL
          values$anovaModel <- NULL
          values$lastKruskal <- NULL
          values$multiResults <- NULL
          values$multiGroups <- NULL
          values$currentPlot <- NULL
          values$residualsNorm <- NULL
          values$leveneResid <- NULL
          values$multiNormResults <- NULL
          values$multiLeveneResults <- NULL
          values$pcaResult <- NULL
          values$clusterResult <- NULL
          values$currentModel <- NULL
          values$testInterpretation <- NULL
          values$cahResult <- NULL
          values$currentInteractivePlot <- NULL
          values$allTestResults <- list()
          values$allPostHocResults <- list()
          values$modelsList <- list()
          values$normalityResultsPerVar <- list()
          values$homogeneityResultsPerVar <- list()
          
          reset("file")
          updateTabItems(session, "tabs", "load")
          
          showNotification("Application réinitialisée", type = "message")
        }
      }
    )
  })
  
  # ---- Exemple de données ----
  output$downloadExample <- downloadHandler(
    filename = function() {
      "exemple_donnees.csv"
    },
    content = function(file) {
      set.seed(123)
      n <- 100
      exemple <- data.frame(
        Traitement = rep(c("A", "B", "C", "D"), each = n/4),
        Bloc = rep(1:4, each = n/4),
        Genre = sample(c("M", "F"), n, replace = TRUE),
        Age = sample(18:65, n, replace = TRUE),
        Variable1 = c(rnorm(n/4, 10, 2), rnorm(n/4, 12, 2), rnorm(n/4, 15, 2), rnorm(n/4, 11, 2)),
        Variable2 = c(rnorm(n/4, 20, 3), rnorm(n/4, 22, 3), rnorm(n/4, 25, 3), rnorm(n/4, 21, 3)),
        Variable3 = c(rnorm(n/4, 5, 1), rnorm(n/4, 6, 1), rnorm(n/4, 7, 1), rnorm(n/4, 5.5, 1)),
        Variable4 = c(rnorm(n/4, 100, 10), rnorm(n/4, 110, 10), rnorm(n/4, 120, 10), rnorm(n/4, 105, 10))
      )
      for (i in 5:8) {
        exemple[sample(1:n, 5), i] <- NA
      }
      write.csv(exemple, file, row.names = FALSE)
    }
  )
  
  # ---- Chargement ----
  output$sheetUI <- renderUI({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (ext %in% c("xlsx", "xls")) {
      sheets <- readxl::excel_sheets(input$file$datapath)
      selectInput("sheet", "Feuille Excel :", choices = sheets, selected = sheets[1])
    }
  })
  
  observeEvent(input$loadData, {
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    tryCatch({
      withProgress(message = 'Chargement des données', value = 0, {
        incProgress(0.3)
        if (ext %in% c("csv", "txt")) {
          values$data <- read.csv(input$file$datapath, header = input$header, sep = input$sep, check.names = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          values$data <- readxl::read_excel(path = input$file$datapath, sheet = input$sheet %||% 1)
        } else if (ext == "sav") {
          values$data <- haven::read_sav(input$file$datapath)
        } else if (ext == "dta") {
          values$data <- haven::read_dta(input$file$datapath)
        } else if (ext == "rds") {
          values$data <- readRDS(input$file$datapath)
        } else {
          stop("Format non supporté.")
        }
        incProgress(0.7)
        values$cleanData <- as.data.frame(values$data)
        values$filteredData <- values$cleanData
        incProgress(1)
      })
      showNotification("Données chargées avec succès", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur de chargement :", e$message), type = "error")
    })
  })
  
  output$nrowBox <- renderValueBox({
    req(values$data)
    valueBox(
      nrow(values$data), "Lignes", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$ncolBox <- renderValueBox({
    req(values$data)
    valueBox(
      ncol(values$data), "Colonnes", icon = icon("columns"),
      color = "purple"
    )
  })
  
  output$naBox <- renderValueBox({
    req(values$data)
    na_count <- sum(is.na(values$data))
    valueBox(
      na_count, "Valeurs manquantes", icon = icon("question"),
      color = ifelse(na_count > 0, "red", "green")
    )
  })
  
  output$memBox <- renderValueBox({
    req(values$data)
    mem_size <- format(object.size(values$data), units = "auto")
    valueBox(
      mem_size, "Taille mémoire", icon = icon("memory"),
      color = "blue"
    )
  })
  
  output$preview <- renderDT({
    req(values$data)
    datatable(head(values$data, 50), options = list(scrollX = TRUE))
  })
  
  # ---- Exploration ----
  
  #  Outputs de base 
  output$dataStructure <- renderPrint({
    req(values$data)
    cat("STRUCTURE DES DONNÉES\n")
    cat("=====================\n\n")
    str(values$data)
  })
  
  output$dataSummary <- renderPrint({
    req(values$data)
    cat("RÉSUMÉ STATISTIQUE\n")
    cat("==================\n\n")
    summary(values$data)
  })
  
  #  Matrice de corrélation 
  output$corrVarSelect <- renderUI({
    req(values$data)
    num_cols <- names(values$data)[sapply(values$data, is.numeric)]
    
    if (length(num_cols) == 0) {
      return(div(class = "alert alert-warning", 
                 icon("exclamation-triangle"), 
                 " Aucune variable numérique disponible"))
    }
    
    tagList(
      pickerInput(
        inputId = "corrVars",
        label = "Sélectionnez les variables numériques:", 
        choices = num_cols,
        multiple = TRUE,
        selected = num_cols[1:min(5, length(num_cols))],
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      actionButton("selectAllCorrVars", "Tout sélectionner", 
                   class = "btn-success btn-sm", icon = icon("check-square")),
      actionButton("deselectAllCorrVars", "Tout désélectionner", 
                   class = "btn-danger btn-sm", icon = icon("square"))
    )
  })
  
  observeEvent(input$selectAllCorrVars, {
    num_cols <- names(values$data)[sapply(values$data, is.numeric)]
    updatePickerInput(session, "corrVars", selected = num_cols)
  })
  
  observeEvent(input$deselectAllCorrVars, {
    updatePickerInput(session, "corrVars", selected = character(0))
  })
  
  # Fonction réutilisable pour générer le plot de corrélation
  generate_corr_plot <- function(data, vars, method = "pearson", display = "number", 
                                 type = "upper", label_size = 0.8, text_size = 0.8,
                                 title = NULL, center_title = TRUE) {
    
    if (is.null(vars) || length(vars) < 2) {
      return(NULL)
    }
    
    cor_data <- data[, vars, drop = FALSE]
    cor_matrix <- cor(cor_data, use = "complete.obs", method = method)
    
    method_label <- switch(method,
                           "pearson" = "Pearson",
                           "spearman" = "Spearman", 
                           "kendall" = "Kendall")
    
    plot_title <- if (!is.null(title) && title != "") {
      title
    } else {
      paste("Matrice de corrélation -", method_label)
    }
    
    corrplot(cor_matrix, 
             method = display,
             type = type,
             order = "hclust", 
             tl.cex = label_size, 
             tl.col = "black",
             number.cex = text_size,
             title = plot_title,
             mar = c(0, 0, if (center_title) 2 else 1, 0),
             addCoef.col = if (display %in% c("circle", "square", "ellipse", "color", "pie")) "black" else NULL,
             tl.srt = 45)
  }
  
  # Reactive pour les paramètres de corrélation
  corrParams <- reactive({
    list(
      method = input$corrMethod %||% "pearson",
      display = input$corrDisplay %||% "number",
      type = input$corrType %||% "upper",
      label_size = input$corrLabelSize %||% 0.8,
      text_size = input$corrTextSize %||% 0.8,
      title = input$corrTitle,
      center_title = if (is.null(input$corrCenterTitle)) TRUE else input$corrCenterTitle
    )
  }) %>% debounce(500)
  
  output$corrPlot <- renderPlot({
    req(values$data, input$corrVars)
    
    params <- corrParams()
    
    tryCatch({
      generate_corr_plot(
        data = values$data,
        vars = input$corrVars,
        method = params$method,
        display = params$display,
        type = params$type,
        label_size = params$label_size,
        text_size = params$text_size,
        title = params$title,
        center_title = params$center_title
      )
    }, error = function(e) {
      showNotification(paste("Erreur lors de la création de la matrice:", e$message), 
                       type = "error", duration = 5)
      return(NULL)
    })
  })
  
  # Téléchargement matrice de corrélation
  output$downloadCorrPlot <- downloadHandler(
    filename = function() {
      paste0("matrice_correlation_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$data, input$corrVars)
      
      params <- corrParams()
      dpi <- input$corrDPI %||% 300
      
      png(file, width = 2400, height = 2400, res = dpi, type = "cairo")
      
      tryCatch({
        generate_corr_plot(
          data = values$data,
          vars = input$corrVars,
          method = params$method,
          display = params$display,
          type = params$type,
          label_size = params$label_size,
          text_size = params$text_size,
          title = params$title,
          center_title = params$center_title
        )
      }, error = function(e) {
        showNotification(paste("Erreur téléchargement:", e$message), type = "error")
      }, finally = {
        dev.off()
      })
      
      showNotification("Graphique téléchargé avec succès!", type = "message", duration = 3)
    }
  )
  
  #  Distribution des variables 
  output$distVarSelect <- renderUI({
    req(values$data)
    num_cols <- names(values$data)[sapply(values$data, is.numeric)]
    
    if (length(num_cols) == 0) {
      return(div(class = "alert alert-warning", 
                 icon("exclamation-triangle"), 
                 " Aucune variable numérique disponible"))
    }
    
    selectInput("distVar", "Sélectionnez une variable:", 
                choices = num_cols, selected = num_cols[1])
  })
  
  # Fonction réutilisable pour générer le plot de distribution
  generate_dist_plot <- function(data, var, show_density = TRUE, title = NULL, 
                                 center_title = TRUE, title_size = 14, 
                                 axis_title_size = 12, axis_text_size = 10,
                                 legend_text_size = 10) {
    
    plot_title <- if (!is.null(title) && title != "") {
      title
    } else {
      paste("Distribution de", var)
    }
    
    p <- ggplot(data, aes_string(x = var)) +
      geom_histogram(aes(y = ..density..), fill = "lightblue", 
                     color = "black", alpha = 0.7, bins = 30)
    
    if (show_density) {
      p <- p + geom_density(color = "red", size = 1.2)
    }
    
    p <- p + theme_minimal() +
      labs(title = plot_title, x = var, y = "Densité") +
      theme(
        plot.title = element_text(size = title_size, hjust = if (center_title) 0.5 else 0),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        legend.text = element_text(size = legend_text_size),
        legend.title = element_text(size = legend_text_size)
      )
    
    return(p)
  }
  
  # Reactive pour les paramètres de distribution
  distParams <- reactive({
    list(
      show_density = if (is.null(input$distShowDensity)) TRUE else input$distShowDensity,
      title = input$distTitle,
      center_title = if (is.null(input$distCenterTitle)) TRUE else input$distCenterTitle,
      title_size = input$distTitleSize %||% 14,
      axis_title_size = input$distAxisTitleSize %||% 12,
      axis_text_size = input$distAxisTextSize %||% 10,
      legend_text_size = input$distLegendTextSize %||% 10
    )
  }) %>% debounce(500)
  
  output$distPlot <- renderPlot({
    req(values$data, input$distVar)
    
    params <- distParams()
    
    tryCatch({
      generate_dist_plot(
        data = values$data,
        var = input$distVar,
        show_density = params$show_density,
        title = params$title,
        center_title = params$center_title,
        title_size = params$title_size,
        axis_title_size = params$axis_title_size,
        axis_text_size = params$axis_text_size,
        legend_text_size = params$legend_text_size
      )
    }, error = function(e) {
      showNotification(paste("Erreur lors de la création du graphique:", e$message), 
                       type = "error", duration = 5)
      return(NULL)
    })
  })
  
  output$downloadDistPlot <- downloadHandler(
    filename = function() {
      paste0("distribution_", input$distVar, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$data, input$distVar)
      
      params <- distParams()
      dpi <- input$distDPI %||% 300
      
      p <- tryCatch({
        generate_dist_plot(
          data = values$data,
          var = input$distVar,
          show_density = params$show_density,
          title = params$title,
          center_title = params$center_title,
          title_size = params$title_size,
          axis_title_size = params$axis_title_size,
          axis_text_size = params$axis_text_size,
          legend_text_size = params$legend_text_size
        )
      }, error = function(e) {
        showNotification(paste("Erreur téléchargement:", e$message), type = "error")
        return(NULL)
      })
      
      if (!is.null(p)) {
        ggsave(file, plot = p, width = 10, height = 8, dpi = dpi, type = "cairo-png")
        showNotification("Graphique téléchargé avec succès!", type = "message", duration = 3)
      }
    }
  )
  
  #  Analyse des valeurs manquantes 
  generate_missing_data <- function(data) {
    missing_counts <- sapply(data, function(x) sum(is.na(x)))
    
    missing_data <- data.frame(
      Variable = names(missing_counts),
      Missing = as.numeric(missing_counts),
      stringsAsFactors = FALSE
    )
    
    missing_data$PctMissing <- (missing_data$Missing / nrow(data)) * 100
    
    return(missing_data)
  }
  
  # Fonction réutilisable pour générer le plot des valeurs manquantes
  generate_missing_plot <- function(data, title = NULL, center_title = TRUE,
                                    title_size = 14, axis_title_size = 12,
                                    axis_text_size = 10, rotate_labels = TRUE) {
    
    missing_data <- generate_missing_data(data)
    
    plot_title <- if (!is.null(title) && title != "") {
      title
    } else {
      "Analyse des valeurs manquantes"
    }
    
    p <- ggplot(missing_data, aes(x = reorder(Variable, -Missing), y = Missing)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = paste0(round(PctMissing, 1), "%")), 
                vjust = -0.5, size = 3.5) +
      theme_minimal() +
      labs(title = plot_title, x = "Variable", y = "Nombre de valeurs manquantes") +
      theme(
        plot.title = element_text(size = title_size, hjust = if (center_title) 0.5 else 0),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        axis.text.x = element_text(
          angle = if (rotate_labels) 45 else 0, 
          hjust = if (rotate_labels) 1 else 0.5
        )
      )
    
    return(p)
  }
  
  # Reactive pour les paramètres des valeurs manquantes
  missingParams <- reactive({
    list(
      title = input$missingTitle,
      center_title = if (is.null(input$missingCenterTitle)) TRUE else input$missingCenterTitle,
      title_size = input$missingTitleSize %||% 14,
      axis_title_size = input$missingAxisTitleSize %||% 12,
      axis_text_size = input$missingAxisTextSize %||% 10,
      rotate_labels = if (is.null(input$missingRotateLabels)) TRUE else input$missingRotateLabels
    )
  }) %>% debounce(500)
  
  output$missingPlot <- renderPlot({
    req(values$data)
    
    params <- missingParams()
    
    tryCatch({
      generate_missing_plot(
        data = values$data,
        title = params$title,
        center_title = params$center_title,
        title_size = params$title_size,
        axis_title_size = params$axis_title_size,
        axis_text_size = params$axis_text_size,
        rotate_labels = params$rotate_labels
      )
    }, error = function(e) {
      showNotification(paste("Erreur lors de la création du graphique:", e$message), 
                       type = "error", duration = 5)
      return(NULL)
    })
  })
  
  output$downloadMissingPlot <- downloadHandler(
    filename = function() {
      paste0("valeurs_manquantes_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$data)
      
      params <- missingParams()
      dpi <- input$missingDPI %||% 300
      
      p <- tryCatch({
        generate_missing_plot(
          data = values$data,
          title = params$title,
          center_title = params$center_title,
          title_size = params$title_size,
          axis_title_size = params$axis_title_size,
          axis_text_size = params$axis_text_size,
          rotate_labels = params$rotate_labels
        )
      }, error = function(e) {
        showNotification(paste("Erreur téléchargement:", e$message), type = "error")
        return(NULL)
      })
      
      if (!is.null(p)) {
        ggsave(file, plot = p, width = 12, height = 8, dpi = dpi, type = "cairo-png")
        showNotification("Graphique téléchargé avec succès!", type = "message", duration = 3)
      }
    }
  )
  
  # ---- Nettoyage ----
  
  #  Gestion des types de variables 
  output$varTypeUI <- renderUI({
    req(values$data)
    
    tagList(
      div(class = "alert alert-info", 
          icon("info-circle"), 
          " Sélectionnez le type souhaité pour chaque variable"),
      lapply(names(values$data), function(col) {
        current_type <- if (is.numeric(values$data[[col]])) "numeric"
        else if (is.factor(values$data[[col]])) "factor"
        else if (inherits(values$data[[col]], "Date")) "date"
        else "character"
        
        fluidRow(
          column(6, strong(col)),
          column(6, 
                 selectInput(
                   paste0("type_", col), 
                   NULL,
                   choices = c("Numérique" = "numeric", 
                               "Facteur" = "factor", 
                               "Texte" = "character", 
                               "Date" = "date"),
                   selected = current_type,
                   width = "100%"
                 )
          )
        )
      })
    )
  })
  
  observeEvent(input$applyTypes, {
    req(values$cleanData)
    
    withProgress(message = 'Application des types...', value = 0, {
      data_temp <- values$cleanData
      n <- length(names(data_temp))
      
      for (i in seq_along(names(data_temp))) {
        col <- names(data_temp)[i]
        type_input <- input[[paste0("type_", col)]]
        
        if (!is.null(type_input)) {
          tryCatch({
            if (type_input == "numeric") {
              data_temp[[col]] <- suppressWarnings(as.numeric(as.character(data_temp[[col]])))
            } else if (type_input == "factor") {
              data_temp[[col]] <- as.factor(data_temp[[col]])
            } else if (type_input == "character") {
              data_temp[[col]] <- as.character(data_temp[[col]])
            } else if (type_input == "date") {
              data_temp[[col]] <- as.Date(data_temp[[col]])
            }
          }, error = function(e) {
            showNotification(paste("Erreur pour", col, ":", e$message), 
                             type = "warning", duration = 5)
          })
        }
        
        incProgress(1/n, detail = paste("Variable", i, "sur", n))
      }
      
      values$cleanData <- data_temp
      values$filteredData <- values$cleanData
    })
    
    showNotification(
      ui = tagList(icon("check"), " Types de variables appliqués avec succès!"),
      type = "message", 
      duration = 3
    )
  })
  
  #  Gestion des variables 
  output$removeVarUI <- renderUI({
    req(values$cleanData)
    selectInput("removeVarName", "Supprimer variable :", 
                choices = names(values$cleanData))
  })
  
  observeEvent(input$removeVar, {
    req(input$removeVarName)
    
    var_name <- input$removeVarName
    values$cleanData <- values$cleanData[, !(names(values$cleanData) %in% var_name), drop = FALSE]
    values$filteredData <- values$cleanData
    
    showNotification(
      ui = tagList(icon("trash"), paste(" Variable '", var_name, "' supprimée avec succès")),
      type = "message", 
      duration = 3
    )
  })
  
  observeEvent(input$addVar, {
    req(input$newVarName)
    
    if (input$newVarName %in% names(values$cleanData)) {
      showNotification(
        ui = tagList(icon("exclamation-triangle"), " Cette variable existe déjà!"),
        type = "warning", 
        duration = 3
      )
    } else if (input$newVarName == "") {
      showNotification("Le nom de la variable ne peut pas être vide", 
                       type = "error", duration = 3)
    } else {
      values$cleanData[[input$newVarName]] <- rep(input$newVarValue, nrow(values$cleanData))
      values$filteredData <- values$cleanData
      
      showNotification(
        ui = tagList(icon("plus"), paste(" Variable '", input$newVarName, "' ajoutée avec succès")),
        type = "message", 
        duration = 3
      )
    }
  })
  
  #  Variables calculées 
  output$colPicker <- renderUI({
    req(values$cleanData)
    selectInput("colInsert", "Insérer colonne :", 
                choices = c("", names(values$cleanData)))
  })
  
  observeEvent(input$colInsert, {
    req(input$colInsert != "")
    current_formula <- input$calcFormula %||% ""
    new_formula <- paste0(current_formula, 
                          ifelse(nchar(current_formula) > 0, " ", ""), 
                          input$colInsert)
    updateTextInput(session, "calcFormula", value = new_formula)
  })
  
  # Opérateurs mathématiques
  observeEvent(input$insertPlus, { 
    updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " + ")) 
  })
  
  observeEvent(input$insertMoins, { 
    updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " - ")) 
  })
  
  observeEvent(input$insertMult, { 
    updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " * ")) 
  })
  
  observeEvent(input$insertDiv, { 
    updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " / ")) 
  })
  
  observeEvent(input$insertLog, { 
    updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " log()")) 
  })
  
  observeEvent(input$insertSqrt, { 
    updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " sqrt()")) 
  })
  
  observeEvent(input$addCalcVar, {
    req(input$calcVarName, input$calcFormula)
    
    if (input$calcVarName == "") {
      showNotification("Le nom de la variable ne peut pas être vide", 
                       type = "error", duration = 3)
      return()
    }
    
    if (input$calcFormula == "") {
      showNotification("La formule ne peut pas être vide", 
                       type = "error", duration = 3)
      return()
    }
    
    tryCatch({
      new_col <- with(values$cleanData, eval(parse(text = input$calcFormula)))
      
      if (length(new_col) != nrow(values$cleanData)) {
        showNotification("Erreur : la formule ne renvoie pas un vecteur de la bonne longueur", 
                         type = "error", duration = 5)
        return()
      }
      
      values$cleanData[[input$calcVarName]] <- new_col
      values$filteredData <- values$cleanData
      
      showNotification(
        ui = tagList(icon("calculator"), paste(" Variable '", input$calcVarName, "' créée avec succès!")),
        type = "message", 
        duration = 3
      )
      
      # Réinitialiser les champs
      updateTextInput(session, "calcVarName", value = "")
      updateTextInput(session, "calcFormula", value = "")
      
    }, error = function(e) {
      showNotification(paste("Erreur dans la formule:", e$message), 
                       type = "error", duration = 5)
    })
  })
  
  #  Gestion des valeurs manquantes 
  output$naVarSelect <- renderUI({
    req(values$cleanData)
    
    # Identifier les variables avec des NA
    vars_with_na <- names(values$cleanData)[sapply(values$cleanData, function(x) any(is.na(x)))]
    
    if (length(vars_with_na) == 0) {
      return(div(class = "alert alert-success", 
                 icon("check-circle"), 
                 " Aucune valeur manquante détectée dans les données"))
    }
    
    pickerInput(
      inputId = "naVars",
      label = paste0("Sélectionnez les variables à traiter (", 
                     length(vars_with_na), " variables avec NA) :"), 
      choices = names(values$cleanData),
      selected = vars_with_na,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  observeEvent(input$applyNA, {
    req(values$cleanData, input$naVars)
    
    if (length(input$naVars) == 0) {
      showNotification("Veuillez sélectionner au moins une variable", 
                       type = "warning", duration = 3)
      return()
    }
    
    withProgress(message = 'Traitement des valeurs manquantes...', value = 0, {
      data_temp <- values$cleanData
      n <- length(input$naVars)
      
      for (i in seq_along(input$naVars)) {
        col <- input$naVars[i]
        
        tryCatch({
          if (input$naMethod == "remove") {
            data_temp <- data_temp[!is.na(data_temp[[col]]), ]
          } else if (input$naMethod == "mean") {
            if (is.numeric(data_temp[[col]])) {
              mean_val <- mean(data_temp[[col]], na.rm = TRUE)
              data_temp[[col]][is.na(data_temp[[col]])] <- mean_val
            } else {
              showNotification(paste("La variable", col, "n'est pas numérique. Moyenne impossible."), 
                               type = "warning", duration = 3)
            }
          } else if (input$naMethod == "median") {
            if (is.numeric(data_temp[[col]])) {
              median_val <- median(data_temp[[col]], na.rm = TRUE)
              data_temp[[col]][is.na(data_temp[[col]])] <- median_val
            } else {
              showNotification(paste("La variable", col, "n'est pas numérique. Médiane impossible."), 
                               type = "warning", duration = 3)
            }
          } else if (input$naMethod == "value") {
            data_temp[[col]][is.na(data_temp[[col]])] <- input$naValue
          }
        }, error = function(e) {
          showNotification(paste("Erreur pour", col, ":", e$message), 
                           type = "error", duration = 5)
        })
        
        incProgress(1/n, detail = paste("Variable", i, "sur", n))
      }
      
      values$cleanData <- data_temp
      values$filteredData <- values$cleanData
    })
    
    showNotification(
      ui = tagList(icon("check"), " Traitement des valeurs manquantes terminé avec succès!"),
      type = "message", 
      duration = 3
    )
  })
  
  #  Affichage des données nettoyées 
  output$cleanedData <- renderDT({
    req(values$cleanData)
    datatable(
      values$cleanData, 
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = TRUE,
      class = 'cell-border stripe'
    )
  })
  # ---- Filtrage ----
  output$filterFactorA <- renderUI({
    req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    if (length(fac_cols) == 0) {
      helpText("Aucun facteur. Convertissez d'abord des variables en facteurs dans l'onglet Nettoyage.")
    } else {
      selectInput("factorA", "Facteur A :", choices = fac_cols)
    }
  })
  
  output$filterFactorB <- renderUI({
    req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    if (length(fac_cols) == 0) {
      NULL
    } else {
      selectInput("factorB", "Facteur B :", choices = fac_cols, selected = fac_cols[min(2, length(fac_cols))])
    }
  })
  
  observeEvent(input$applyCrossFilter, {
    req(values$cleanData, input$factorA, input$factorB)
    validate(need(input$factorA != input$factorB, "Choisissez deux facteurs distincts."))
    tryCatch({
      df <- values$cleanData
      filtered <- filter_complete_cross(df, input$factorA, input$factorB,
                                        reqA = isTRUE(input$requireA),
                                        reqB = isTRUE(input$requireB))
      values$filteredData <- filtered
      showNotification(paste("Filtrage (2 facteurs) appliqué. Lignes:", nrow(filtered)), type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur filtrage:", e$message), type = "error")
    })
  })
  
  output$filterFactorsN <- renderUI({
    req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    selectInput("factorsN", "Facteurs (>=2) :", choices = fac_cols, multiple = TRUE)
  })
  
  observeEvent(input$applyCrossFilterN, {
    req(values$cleanData, input$factorsN)
    validate(need(length(input$factorsN) >= 2, "Sélectionnez au moins deux facteurs."))
    tryCatch({
      filtered <- filter_complete_cross_n(values$cleanData, input$factorsN)
      values$filteredData <- filtered
      showNotification(paste("Filtrage (N facteurs) appliqué. Lignes:", nrow(filtered)), type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur filtrage N facteurs:", e$message), type = "error")
    })
  })
  
  observeEvent(input$resetFilter, {
    values$filteredData <- values$cleanData
    showNotification("Filtre réinitialisé", type = "message")
  })
  
  output$originalRows <- renderValueBox({
    req(values$cleanData)
    valueBox(
      nrow(values$cleanData), "Lignes originales", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$filteredRows <- renderValueBox({
    req(values$filteredData)
    valueBox(
      nrow(values$filteredData), "Lignes filtrées", icon = icon("filter"),
      color = "green"
    )
  })
  
  output$removedRows <- renderValueBox({
    req(values$cleanData, values$filteredData)
    removed <- nrow(values$cleanData) - nrow(values$filteredData)
    valueBox(
      removed, "Lignes supprimées", icon = icon("trash"),
      color = ifelse(removed > 0, "red", "green")
    )
  })
  
  output$completeCases <- renderValueBox({
    req(values$filteredData)
    complete <- sum(complete.cases(values$filteredData))
    valueBox(
      complete, "Cas complets", icon = icon("check"),
      color = "blue"
    )
  })
  
  output$filteredData <- renderDT({
    req(values$filteredData)
    datatable(values$filteredData, options = list(scrollX = TRUE))
  })
  
  output$filterSummary <- renderPrint({
    req(values$cleanData, values$filteredData)
    cat("Nombre de lignes originales :", nrow(values$cleanData), "\n")
    cat("Nombre de lignes filtrées :", nrow(values$filteredData), "\n")
    cat("Nombre de lignes supprimées :", nrow(values$cleanData) - nrow(values$filteredData), "\n")
  })
  
  # ---- Analyse descriptives ----
  
  # UI pour sélection des variables numériques
  output$numVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    tagList(
      pickerInput(
        inputId = "numVars",
        label = "Sélectionnez les variables numériques:", 
        choices = num_cols,
        multiple = TRUE,
        selected = num_cols[1:min(5, length(num_cols))],
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      div(style = "margin-top: 10px;",
          actionButton("selectAllNumVars", "Tout sélectionner", 
                       class = "btn-success btn-sm", 
                       icon = icon("check-double")),
          actionButton("deselectAllNumVars", "Tout désélectionner", 
                       class = "btn-warning btn-sm", 
                       icon = icon("times"),
                       style = "margin-left: 5px;")
      )
    )
  })
  
  # Boutons pour tout sélectionner/désélectionner
  observeEvent(input$selectAllNumVars, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "numVars", selected = num_cols)
    showNotification("Toutes les variables sélectionnées", type = "message", duration = 2)
  })
  
  observeEvent(input$deselectAllNumVars, {
    updatePickerInput(session, "numVars", selected = character(0))
    showNotification("Toutes les variables désélectionnées", type = "message", duration = 2)
  })
   
  # UI pour sélection des facteurs
  output$descFactorUI <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    tagList(
      pickerInput("descFactors", "Calcul par facteurs (optionnel)", 
                  choices = fac_cols, multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      helpText(icon("info-circle"), " Laissez vide pour des descriptives globales. Sélectionnez un ou plusieurs facteurs pour des descriptives par traitement/facteur.")
    )
  })
  
  # Observer pour le checkbox "Sélectionner toutes les variables"
  observeEvent(input$selectAllNum, {
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (input$selectAllNum) {
      updatePickerInput(session, "numVars", selected = num_cols)
    } else {
      updatePickerInput(session, "numVars", selected = character(0))
    }
  })
  
  # Calcul des statistiques descriptives
  observeEvent(input$calcDesc, {
    req(input$numVars)
    
    # Animation de chargement
    showNotification("Calcul en cours...", type = "message", duration = NULL, id = "calcProgress")
    
    tryCatch({
      stats_sel <- input$descStats
      
      # Fonction pour calculer les statistiques globales
      make_summ_global <- function(df_in, num_vars, stats_sel) {
        summ_list <- lapply(num_vars, function(var) {
          x <- df_in[[var]]
          stats <- list()
          if ("mean" %in% stats_sel) stats$mean <- mean(x, na.rm = TRUE)
          if ("median" %in% stats_sel) stats$median <- median(x, na.rm = TRUE)
          if ("sd" %in% stats_sel) stats$sd <- sd(x, na.rm = TRUE)
          if ("var" %in% stats_sel) stats$var <- var(x, na.rm = TRUE)
          if ("cv" %in% stats_sel) stats$cv <- calc_cv(x)
          if ("min" %in% stats_sel) stats$min <- min(x, na.rm = TRUE)
          if ("max" %in% stats_sel) stats$max <- max(x, na.rm = TRUE)
          if ("q1" %in% stats_sel) stats$q1 <- quantile(x, 0.25, na.rm = TRUE)
          if ("q3" %in% stats_sel) stats$q3 <- quantile(x, 0.75, na.rm = TRUE)
          
          data.frame(Facteurs = "Global", Variable = var, as.data.frame(stats), check.names = FALSE)
        })
        do.call(rbind, summ_list)
      }
      
      # Fonction pour calculer les statistiques groupées
      make_summ_grouped <- function(df_in, group_vars, num_vars, stats_sel) {
        results_list <- lapply(num_vars, function(var_name) {
          var_results <- df_in %>%
            group_by(!!!syms(group_vars)) %>%
            summarise(
              mean = if("mean" %in% stats_sel) mean(.data[[var_name]], na.rm = TRUE) else NA_real_,
              median = if("median" %in% stats_sel) median(.data[[var_name]], na.rm = TRUE) else NA_real_,
              sd = if("sd" %in% stats_sel) sd(.data[[var_name]], na.rm = TRUE) else NA_real_,
              var = if("var" %in% stats_sel) var(.data[[var_name]], na.rm = TRUE) else NA_real_,
              cv = if("cv" %in% stats_sel) calc_cv(.data[[var_name]]) else NA_real_,
              min = if("min" %in% stats_sel) min(.data[[var_name]], na.rm = TRUE) else NA_real_,
              max = if("max" %in% stats_sel) max(.data[[var_name]], na.rm = TRUE) else NA_real_,
              q1 = if("q1" %in% stats_sel) quantile(.data[[var_name]], 0.25, na.rm = TRUE) else NA_real_,
              q3 = if("q3" %in% stats_sel) quantile(.data[[var_name]], 0.75, na.rm = TRUE) else NA_real_,
              .groups = "drop"
            ) %>%
            mutate(Variable = var_name)
          
          return(var_results)
        })
        
        df_combined <- bind_rows(results_list)
        
        # Réorganiser les colonnes
        selected_cols <- c(group_vars, "Variable", stats_sel)
        final_cols <- selected_cols[selected_cols %in% names(df_combined)]
        df_combined <- df_combined[, final_cols, drop = FALSE]
        
        return(df_combined)
      }
      
      # Calculer selon le mode choisi
      if (!is.null(input$descFactors) && length(input$descFactors) > 0) {
        values$descStats <- make_summ_grouped(values$filteredData, input$descFactors, input$numVars, stats_sel)
      } else {
        values$descStats <- make_summ_global(values$filteredData, input$numVars, stats_sel)
      }
      
      removeNotification("calcProgress")
      showNotification("Statistiques calculées avec succès!", type = "message", duration = 3)
      
    }, error = function(e) {
      removeNotification("calcProgress")
      showNotification(paste("Erreur:", e$message), type = "error", duration = 5)
    })
  })
  
  # Affichage des résultats
  output$descResults <- renderDT({
    req(values$descStats)
    datatable(
      values$descStats, 
      options = list(
        scrollX = TRUE, 
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        language = list(
          search = "Rechercher:",
          lengthMenu = "Afficher _MENU_ lignes",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ lignes",
          paginate = list(previous = "Précédent", `next` = "Suivant")
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover',
      filter = 'top'
    ) %>%
      formatRound(columns = which(sapply(values$descStats, is.numeric)), digits = 2)
  })
  
  # Téléchargement CSV
  output$downloadDesc <- downloadHandler(
    filename = function() {
      paste0("descriptives_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$descStats, file, row.names = FALSE)
      showNotification("Fichier CSV téléchargé!", type = "message", duration = 3)
    }
  )
  
  # Téléchargement EXCEL (XLSX)
  output$downloadDescExcel <- downloadHandler(
    filename = function() {
      paste0("descriptives_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      require(writexl)
      write_xlsx(values$descStats, file)
      showNotification("Fichier Excel téléchargé!", type = "message", duration = 3)
    }
  )
  
  # UI pour sélection de la variable à visualiser
  output$descPlotVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    selectInput("descPlotVar", "Variable à visualiser:", choices = num_cols, width = "100%")
  })
  
  # UI pour sélection du facteur de groupement
  output$descPlotFactorSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    selectInput("descPlotFactor", "Grouper par:", choices = c("Aucun", fac_cols), width = "100%")
  })
  
  # Initialiser les valeurs par défaut pour les labels
  observe({
    req(input$descPlotVar)
    
    # Titre par défaut
    if (is.null(input$descPlotTitle) || input$descPlotTitle == "") {
      updateTextInput(session, "descPlotTitle", value = paste("Distribution de", input$descPlotVar))
    }
    
    # Label X par défaut
    if (is.null(input$descPlotXLabel) || input$descPlotXLabel == "") {
      updateTextInput(session, "descPlotXLabel", value = input$descPlotVar)
    }
    
    # Label Y par défaut
    if (is.null(input$descPlotYLabel) || input$descPlotYLabel == "") {
      if (input$descPlotFactor == "Aucun") {
        updateTextInput(session, "descPlotYLabel", value = "Densité")
      } else {
        updateTextInput(session, "descPlotYLabel", value = input$descPlotVar)
      }
    }
  })
  
  # Fonction pour générer le graphique
  generate_desc_plot <- function() {
    req(values$filteredData, input$descPlotVar)
    
    # RÉCUPÉRATION DES PARAMÈTRES
    plot_title <- if(!is.null(input$descPlotTitle) && input$descPlotTitle != "") {
      input$descPlotTitle
    } else {
      paste("Distribution de", input$descPlotVar)
    }
    
    x_label <- if(!is.null(input$descPlotXLabel) && input$descPlotXLabel != "") {
      input$descPlotXLabel
    } else {
      input$descPlotVar
    }
    
    y_label <- if(!is.null(input$descPlotYLabel) && input$descPlotYLabel != "") {
      input$descPlotYLabel
    } else {
      "Valeur"
    }
    
    # STYLE DE POLICE POUR LE TITRE
    title_font_face <- "plain"
    if (isTRUE(input$descPlotTitleBold) && isTRUE(input$descPlotTitleItalic)) {
      title_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotTitleBold)) {
      title_font_face <- "bold"
    } else if (isTRUE(input$descPlotTitleItalic)) {
      title_font_face <- "italic"
    }
    
    # STYLE DE POLICE POUR LE LABEL AXE X
    x_label_font_face <- "plain"
    if (isTRUE(input$descPlotXBold) && isTRUE(input$descPlotXItalic)) {
      x_label_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotXBold)) {
      x_label_font_face <- "bold"
    } else if (isTRUE(input$descPlotXItalic)) {
      x_label_font_face <- "italic"
    }
    
    # STYLE DE POLICE POUR LE LABEL AXE Y
    y_label_font_face <- "plain"
    if (isTRUE(input$descPlotYBold) && isTRUE(input$descPlotYItalic)) {
      y_label_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotYBold)) {
      y_label_font_face <- "bold"
    } else if (isTRUE(input$descPlotYItalic)) {
      y_label_font_face <- "italic"
    }
    
    # STYLE DE POLICE POUR LES GRADUATIONS AXE X
    x_tick_font_face <- "plain"
    if (isTRUE(input$descPlotXTickBold) && isTRUE(input$descPlotXTickItalic)) {
      x_tick_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotXTickBold)) {
      x_tick_font_face <- "bold"
    } else if (isTRUE(input$descPlotXTickItalic)) {
      x_tick_font_face <- "italic"
    }
    
    # STYLE DE POLICE POUR LES GRADUATIONS AXE Y
    y_tick_font_face <- "plain"
    if (isTRUE(input$descPlotYTickBold) && isTRUE(input$descPlotYTickItalic)) {
      y_tick_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotYTickBold)) {
      y_tick_font_face <- "bold"
    } else if (isTRUE(input$descPlotYTickItalic)) {
      y_tick_font_face <- "italic"
    }
    
    # ANGLE D'INCLINAISON POUR L'AXE X
    x_angle <- if(!is.null(input$descPlotXAngle)) input$descPlotXAngle else 0
    x_hjust <- if(x_angle > 0) 1 else 0.5
    x_vjust <- if(x_angle > 0) 1 else 0.5
    
    # PALETTE DE COULEURS
    color_palette <- input$descPlotColorPalette
    if (is.null(color_palette)) color_palette <- "ggplot2"
    
    # CRÉATION DU GRAPHIQUE
    p <- if (input$descPlotFactor == "Aucun") {
      # HISTOGRAMME AVEC DENSITÉ
      hist_color <- switch(color_palette,
                           "ggplot2" = "#3498db",
                           "viridis" = "#440154",
                           "Set1" = "#E41A1C",
                           "Set2" = "#66C2A5",
                           "Pastel1" = "#FBB4AE",
                           "Dark2" = "#1B9E77",
                           "mono_blue" = "#2E86AB",
                           "mono_green" = "#06A77D",
                           "mono_red" = "#D62828",
                           "mono_purple" = "#6A4C93",
                           "mono_orange" = "#F77F00",
                           "mono_black" = "#2C3E50",
                           "#3498db")
      
      density_color <- switch(color_palette,
                              "ggplot2" = "#e74c3c",
                              "viridis" = "#FDE724",
                              "mono_blue" = "#1A5490",
                              "mono_green" = "#048060",
                              "mono_red" = "#9B1B1B",
                              "mono_purple" = "#4A2C6A",
                              "mono_orange" = "#C66300",
                              "mono_black" = "#000000",
                              "#e74c3c")
      
      ggplot(values$filteredData, aes(x = .data[[input$descPlotVar]])) +
        geom_histogram(aes(y = after_stat(density)), alpha = 0.7, fill = hist_color, bins = 30) +
        geom_density(linewidth = 1.2, color = density_color) +
        theme_minimal() +
        labs(title = plot_title, x = x_label, y = y_label) +
        theme(
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5),
          axis.title.x = element_text(face = x_label_font_face, size = 13, margin = margin(t = 10)),
          axis.title.y = element_text(face = y_label_font_face, size = 13, margin = margin(r = 10)),
          axis.text.x = element_text(
            face = x_tick_font_face, 
            angle = x_angle, 
            hjust = x_hjust,
            vjust = x_vjust,
            size = 11
          ),
          axis.text.y = element_text(face = y_tick_font_face, size = 11),
          plot.title = element_text(
            hjust = if(isTRUE(input$descPlotCenterTitle)) 0.5 else 0, 
            face = title_font_face, 
            size = 16,
            margin = margin(b = 15)
          ),
          panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
          panel.grid.minor = element_line(color = "grey95", linewidth = 0.2)
        )
    } else {
      # BOXPLOT GROUPÉ SANS BARRES DE PERCENTILES
      
      # Créer le boxplot de base
      base_plot <- ggplot(values$filteredData, aes(
        x = .data[[input$descPlotFactor]], 
        y = .data[[input$descPlotVar]], 
        fill = .data[[input$descPlotFactor]]
      )) +
        geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2)
      
      # Ajouter les valeurs extrêmes si demandé
      if (isTRUE(input$descPlotShowValues)) {
        extreme_data <- values$filteredData %>%
          group_by(.data[[input$descPlotFactor]]) %>%
          summarise(
            min_val = min(.data[[input$descPlotVar]], na.rm = TRUE),
            max_val = max(.data[[input$descPlotVar]], na.rm = TRUE),
            .groups = "drop"
          )
        
        base_plot <- base_plot +
          geom_text(
            data = extreme_data,
            aes(x = .data[[input$descPlotFactor]], y = min_val, label = round(min_val, 2)),
            vjust = 1.5,
            size = 3.5,
            fontface = "bold",
            color = "#2c3e50",
            inherit.aes = FALSE
          ) +
          geom_text(
            data = extreme_data,
            aes(x = .data[[input$descPlotFactor]], y = max_val, label = round(max_val, 2)),
            vjust = -0.5,
            size = 3.5,
            fontface = "bold",
            color = "#2c3e50",
            inherit.aes = FALSE
          )
      }
      
      # Appliquer le thème et les labels
      base_plot <- base_plot +
        theme_minimal() +
        theme(
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5),
          axis.title.x = element_text(face = x_label_font_face, size = 13, margin = margin(t = 10)),
          axis.title.y = element_text(face = y_label_font_face, size = 13, margin = margin(r = 10)),
          axis.text.x = element_text(
            face = x_tick_font_face, 
            angle = x_angle, 
            hjust = x_hjust,
            vjust = x_vjust,
            size = 11
          ),
          axis.text.y = element_text(face = y_tick_font_face, size = 11),
          plot.title = element_text(
            hjust = if(isTRUE(input$descPlotCenterTitle)) 0.5 else 0,
            face = title_font_face, 
            size = 16,
            margin = margin(b = 15)
          ),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
          panel.grid.minor = element_blank()
        ) +
        labs(title = plot_title, x = x_label, y = y_label)
      
      # Appliquer la palette de couleurs
      if (color_palette == "ggplot2") {
        base_plot
      } else if (startsWith(color_palette, "mono_")) {
        mono_color <- switch(color_palette,
                             "mono_blue" = "#2E86AB",
                             "mono_green" = "#06A77D",
                             "mono_red" = "#D62828",
                             "mono_purple" = "#6A4C93",
                             "mono_orange" = "#F77F00",
                             "mono_black" = "#2C3E50",
                             "#2E86AB")
        n_groups <- length(unique(values$filteredData[[input$descPlotFactor]]))
        base_plot + scale_fill_manual(
          values = colorRampPalette(c("#FFFFFF", mono_color))(n_groups + 2)[2:(n_groups + 1)]
        )
      } else if (color_palette == "viridis") {
        base_plot + scale_fill_viridis_d(option = "D")
      } else {
        base_plot + scale_fill_brewer(palette = color_palette)
      }
    }
    
    return(p)
  }
  
  # Affichage du graphique avec dimensions personnalisables
  output$descPlotOutput <- renderUI({
    req(input$descPlotWidth, input$descPlotHeight)
    plotOutput("descPlot", width = paste0(input$descPlotWidth, "px"), height = paste0(input$descPlotHeight, "px"))
  })
  
  # Génération du graphique pour l'affichage
  output$descPlot <- renderPlot({
    p <- generate_desc_plot()
    print(p)
  }, res = 96)
  
  # Téléchargement PNG
  output$downloadDescPlotPNG <- downloadHandler(
    filename = function() {
      paste0("graphique_descriptif_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(input$descPlotWidth, input$descPlotHeight, input$descPlotDPI)
      width_inches <- input$descPlotWidth / input$descPlotDPI
      height_inches <- input$descPlotHeight / input$descPlotDPI
      p <- generate_desc_plot()
      ggsave(filename = file, plot = p, width = width_inches, height = height_inches, 
             dpi = input$descPlotDPI, units = "in", bg = "white", device = "png")
      showNotification("Graphique PNG téléchargé!", type = "message", duration = 3)
    }
  )
  
  # Téléchargement JPEG
  output$downloadDescPlotJPEG <- downloadHandler(
    filename = function() {
      paste0("graphique_descriptif_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      req(input$descPlotWidth, input$descPlotHeight, input$descPlotDPI)
      width_inches <- input$descPlotWidth / input$descPlotDPI
      height_inches <- input$descPlotHeight / input$descPlotDPI
      p <- generate_desc_plot()
      ggsave(filename = file, plot = p, width = width_inches, height = height_inches, 
             dpi = input$descPlotDPI, units = "in", bg = "white", device = "jpeg", quality = 100)
      showNotification("Graphique JPEG téléchargé!", type = "message", duration = 3)
    }
  )
  
  # Téléchargement TIFF
  output$downloadDescPlotTIFF <- downloadHandler(
    filename = function() {
      paste0("graphique_descriptif_", Sys.Date(), ".tiff")
    },
    content = function(file) {
      req(input$descPlotWidth, input$descPlotHeight, input$descPlotDPI)
      width_inches <- input$descPlotWidth / input$descPlotDPI
      height_inches <- input$descPlotHeight / input$descPlotDPI
      p <- generate_desc_plot()
      ggsave(filename = file, plot = p, width = width_inches, height = height_inches, 
             dpi = input$descPlotDPI, units = "in", bg = "white", device = "tiff", compression = "lzw")
      showNotification("Graphique TIFF téléchargé!", type = "message", duration = 3)
    }
  )
  
  # Téléchargement PDF
  output$downloadDescPlotPDF <- downloadHandler(
    filename = function() {
      paste0("graphique_descriptif_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(input$descPlotWidth, input$descPlotHeight)
      width_inches <- input$descPlotWidth / 96
      height_inches <- input$descPlotHeight / 96
      p <- generate_desc_plot()
      ggsave(filename = file, plot = p, width = width_inches, height = height_inches, 
             units = "in", device = "pdf")
      showNotification("Graphique PDF téléchargé!", type = "message", duration = 3)
    }
  )
  
  # Téléchargement SVG
  output$downloadDescPlotSVG <- downloadHandler(
    filename = function() {
      paste0("graphique_descriptif_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(input$descPlotWidth, input$descPlotHeight)
      width_inches <- input$descPlotWidth / 96
      height_inches <- input$descPlotHeight / 96
      p <- generate_desc_plot()
      ggsave(filename = file, plot = p, width = width_inches, height = height_inches, 
             units = "in", device = "svg")
      showNotification("Graphique SVG téléchargé!", type = "message", duration = 3)
    }
  )
  
  # Téléchargement EPS
  output$downloadDescPlotEPS <- downloadHandler(
    filename = function() {
      paste0("graphique_descriptif_", Sys.Date(), ".eps")
    },
    content = function(file) {
      req(input$descPlotWidth, input$descPlotHeight)
      width_inches <- input$descPlotWidth / 96
      height_inches <- input$descPlotHeight / 96
      p <- generate_desc_plot()
      ggsave(filename = file, plot = p, width = width_inches, height = height_inches, 
             units = "in", device = cairo_ps)
      showNotification("Graphique EPS téléchargé!", type = "message", duration = 3)
    }
  )
  # ---- Tableaux croisés dynamiques ----
  
  # Variables reactives
  crosstab_values <- reactiveValues(
    contingency_table = NULL,
    row_proportions = NULL,
    col_proportions = NULL,
    total_proportions = NULL,
    chi_test = NULL,
    fisher_test = NULL,
    residuals = NULL,
    current_plot = NULL,
    current_pie_plot = NULL
  )
  
  # Interface de selection des variables
  output$crosstabRowVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("crosstabRowVar", "Variable en lignes :", 
                choices = all_cols,
                selected = all_cols[1],
                width = "100%")
  })
  
  output$crosstabColVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("crosstabColVar", "Variable en colonnes :", 
                choices = all_cols,
                selected = if(length(all_cols) > 1) all_cols[2] else all_cols[1],
                width = "100%")
  })
  
  output$crosstabFilterVarSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    selectInput("crosstabFilterVar", "Filtrer par (optionnel) :", 
                choices = c("Aucun" = "", fac_cols),
                width = "100%")
  })
  
  # Generation des analyses statistiques
  observeEvent(input$generateCrosstab, {
    req(input$crosstabRowVar, input$crosstabColVar)
    
    showNotification("Generation des analyses en cours...", 
                     type = "message", 
                     duration = 2, 
                     id = "gen_notif")
    
    tryCatch({
      df <- values$filteredData
      
      if (!is.null(input$crosstabFilterVar) && input$crosstabFilterVar != "") {
        df <- df[!is.na(df[[input$crosstabFilterVar]]), ]
      }
      
      df <- df[!is.na(df[[input$crosstabRowVar]]) & !is.na(df[[input$crosstabColVar]]), ]
      
      if (nrow(df) == 0) {
        showNotification("Aucune donnee disponible apres filtrage!", 
                         type = "error", 
                         duration = 5)
        return()
      }
      
      if (!is.factor(df[[input$crosstabRowVar]])) {
        df[[input$crosstabRowVar]] <- as.factor(df[[input$crosstabRowVar]])
      }
      if (!is.factor(df[[input$crosstabColVar]])) {
        df[[input$crosstabColVar]] <- as.factor(df[[input$crosstabColVar]])
      }
      
      contingency_table <- table(df[[input$crosstabRowVar]], df[[input$crosstabColVar]])
      crosstab_values$contingency_table <- addmargins(contingency_table)
      
      # CORRECTION : Proportions en lignes
      if ("row_prop" %in% input$analysisOptions) {
        row_prop <- prop.table(contingency_table, margin = 1) * 100
        crosstab_values$row_proportions <- addmargins(row_prop, margin = 2)
      } else {
        crosstab_values$row_proportions <- NULL
      }
      
      # CORRECTION : Proportions en colonnes
      if ("col_prop" %in% input$analysisOptions) {
        col_prop <- prop.table(contingency_table, margin = 2) * 100  
        crosstab_values$col_proportions <- addmargins(col_prop, margin = 1)
      } else {
        crosstab_values$col_proportions <- NULL
      }
      
      # CORRECTION MAJEURE : Proportions totales
      if ("total_prop" %in% input$analysisOptions) {
        total_prop <- prop.table(contingency_table) * 100
        # Ajout manuel des marges pour les proportions totales
        total_prop_with_margins <- total_prop
        
        # Calcul des totaux de lignes
        row_totals <- apply(total_prop, 1, sum)
        total_prop_with_margins <- cbind(total_prop, Sum = row_totals)
        
        # Calcul des totaux de colonnes
        col_totals <- apply(total_prop_with_margins, 2, sum)
        total_prop_with_margins <- rbind(total_prop_with_margins, Sum = col_totals)
        
        crosstab_values$total_proportions <- total_prop_with_margins
      } else {
        crosstab_values$total_proportions <- NULL
      }
      
      # Test du Chi2
      if ("chi_test" %in% input$analysisOptions) {
        if (all(contingency_table >= 5)) {
          crosstab_values$chi_test <- chisq.test(contingency_table)
        } else {
          crosstab_values$chi_test <- "Conditions non remplies (effectifs < 5)"
        }
      } else {
        crosstab_values$chi_test <- NULL
      }
      
      # Test exact de Fisher
      if ("fisher_test" %in% input$analysisOptions) {
        if (min(dim(contingency_table)) == 2 && max(dim(contingency_table)) == 2) {
          crosstab_values$fisher_test <- fisher.test(contingency_table)
        } else {
          crosstab_values$fisher_test <- "Test de Fisher disponible uniquement pour tableaux 2x2"
        }
      } else {
        crosstab_values$fisher_test <- NULL
      }
      
      # CORRECTION MAJEURE : Calcul des residus standardises
      if ("residuals" %in% input$analysisOptions) {
        # Vérifier que le test du Chi2 a été calculé ET qu'il est valide
        if (!is.null(crosstab_values$chi_test) && is.list(crosstab_values$chi_test)) {
          residuals_std <- crosstab_values$chi_test$stdres
          crosstab_values$residuals <- residuals_std
        } else {
          crosstab_values$residuals <- NULL
          showNotification("Les residus necessitent un test du Chi2 valide. Veuillez cocher 'Test du Chi2'.", 
                           type = "warning", 
                           duration = 5)
        }
      } else {
        crosstab_values$residuals <- NULL
      }
      
      showNotification("Analyses generees avec succes!", 
                       type = "message", 
                       duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'analyse :", e$message), 
                       type = "error", 
                       duration = 10)
    })
  })
  
  # Affichage des tableaux de resultats
  output$crosstabTable <- renderDT({
    req(crosstab_values$contingency_table)
    datatable(as.data.frame.matrix(crosstab_values$contingency_table), 
              options = list(
                scrollX = TRUE, 
                pageLength = 25, 
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              class = 'cell-border stripe hover',
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center; color: #333; font-size: 16px; font-weight: bold;',
                'Tableau de contingence (effectifs)'
              ))
  })
  
  output$crosstabRowProp <- renderDT({
    req(crosstab_values$row_proportions)
    datatable(round(as.data.frame.matrix(crosstab_values$row_proportions), 2), 
              options = list(
                scrollX = TRUE, 
                pageLength = 25, 
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              class = 'cell-border stripe hover',
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center; color: #333; font-size: 16px; font-weight: bold;',
                'Proportions en lignes (%)'
              ))
  })
  
  output$crosstabColProp <- renderDT({
    req(crosstab_values$col_proportions)
    datatable(round(as.data.frame.matrix(crosstab_values$col_proportions), 2), 
              options = list(
                scrollX = TRUE, 
                pageLength = 25, 
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              class = 'cell-border stripe hover',
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center; color: #333; font-size: 16px; font-weight: bold;',
                'Proportions en colonnes (%)'
              ))
  })
  
  output$crosstabTotalProp <- renderDT({
    req(crosstab_values$total_proportions)
    datatable(round(as.data.frame.matrix(crosstab_values$total_proportions), 2), 
              options = list(
                scrollX = TRUE, 
                pageLength = 25, 
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              class = 'cell-border stripe hover',
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center; color: #333; font-size: 16px; font-weight: bold;',
                'Proportions totales (%)'
              ))
  })
  
  output$crosstabTests <- renderPrint({
    if (!is.null(crosstab_values$chi_test)) {
      if (is.list(crosstab_values$chi_test)) {
        cat("══════════════════════════════════════\n")
        cat("       TEST DU CHI-DEUX (χ²)          \n")
        cat("══════════════════════════════════════\n\n")
        cat("Statistique X2 :", round(crosstab_values$chi_test$statistic, 4), "\n")
        cat("Degres de liberte :", crosstab_values$chi_test$parameter, "\n") 
        cat("p-value :", format.pval(crosstab_values$chi_test$p.value, digits = 4), "\n")
        cat("\nInterpretation : ", 
            ifelse(crosstab_values$chi_test$p.value < 0.05, 
                   "[SIGNIFICATIF] Association SIGNIFICATIVE (p < 0.05)", 
                   "[NON SIGNIFICATIF] Pas d'association significative (p >= 0.05)"), "\n\n")
      } else {
        cat("══════════════════════════════════════\n")
        cat("       TEST DU CHI-DEUX (χ²)          \n")
        cat("══════════════════════════════════════\n\n")
        cat(crosstab_values$chi_test, "\n\n")
      }
    }
    
    if (!is.null(crosstab_values$fisher_test)) {
      if (is.list(crosstab_values$fisher_test)) {
        cat("══════════════════════════════════════\n")
        cat("     TEST EXACT DE FISHER             \n")
        cat("══════════════════════════════════════\n\n")
        cat("p-value :", format.pval(crosstab_values$fisher_test$p.value, digits = 4), "\n")
        cat("\nInterpretation : ", 
            ifelse(crosstab_values$fisher_test$p.value < 0.05, 
                   "[SIGNIFICATIF] Association SIGNIFICATIVE (p < 0.05)", 
                   "[NON SIGNIFICATIF] Pas d'association significative (p >= 0.05)"), "\n\n")
      } else {
        cat("══════════════════════════════════════\n")
        cat("     TEST EXACT DE FISHER             \n")
        cat("══════════════════════════════════════\n\n")
        cat(crosstab_values$fisher_test, "\n\n")
      }
    }
    
    if (is.null(crosstab_values$chi_test) && is.null(crosstab_values$fisher_test)) {
      cat("[INFO] Aucun test statistique selectionne.\n")
      cat("Veuillez cocher 'Test du Chi2' ou 'Test exact de Fisher' dans les options d'analyse.\n")
    }
  })
  
  output$crosstabResiduals <- renderDT({
    req(crosstab_values$residuals)
    datatable(round(as.data.frame.matrix(crosstab_values$residuals), 2), 
              options = list(
                scrollX = TRUE, 
                pageLength = 25, 
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              class = 'cell-border stripe hover',
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center; color: #333; font-size: 16px; font-weight: bold;',
                'Residus standardises (|valeur| > 2 = contribution importante)'
              )) %>%
      formatStyle(names(as.data.frame.matrix(crosstab_values$residuals)),
                  backgroundColor = styleInterval(c(-2, 2), 
                                                  c("#d4edff", "white", "#ffd4d4")))
  })
  
  # Fonctions auxiliaires
  create_text_element <- function(size, bold = FALSE, italic = FALSE) {
    face <- if (bold && italic) "bold.italic" 
    else if (bold) "bold" 
    else if (italic) "italic" 
    else "plain"
    element_text(size = size, face = face)
  }
  
  apply_color_palette <- function(plot, palette_choice) {
    if (is.null(palette_choice) || palette_choice == "ggplot_default") {
      return(plot)
    } else if (palette_choice == "grey") {
      return(plot + scale_fill_grey(start = 0.3, end = 0.9))
    } else if (palette_choice == "black") {
      n_colors <- length(unique(plot$data$Col_Var))
      return(plot + scale_fill_manual(values = rep("black", n_colors)))
    } else if (palette_choice %in% c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", 
                                     "Dark2", "Accent", "Paired", "Spectral", "RdYlBu")) {
      return(plot + scale_fill_brewer(palette = palette_choice))
    } else if (palette_choice == "viridis") {
      return(plot + scale_fill_viridis_d())
    } else if (palette_choice == "plasma") {
      return(plot + scale_fill_viridis_d(option = "plasma"))
    } else if (palette_choice == "inferno") {
      return(plot + scale_fill_viridis_d(option = "inferno"))
    } else if (palette_choice == "magma") {
      return(plot + scale_fill_viridis_d(option = "magma"))
    } else if (palette_choice == "cividis") {
      return(plot + scale_fill_viridis_d(option = "cividis"))
    }
    return(plot)
  }
  
  # Generation du graphique principal
  output$crosstabPlot <- renderPlot({
    req(crosstab_values$contingency_table, input$crosstabRowVar, input$crosstabColVar)
    
    # Créer un data frame à partir du tableau de contingence
    df_plot <- as.data.frame(crosstab_values$contingency_table)
    df_plot <- df_plot[df_plot$Var1 != "Sum" & df_plot$Var2 != "Sum", ]
    names(df_plot) <- c("Row_Var", "Col_Var", "Freq")
    
    # Vérifier que les données sont valides
    if (nrow(df_plot) == 0 || any(is.na(df_plot$Freq))) {
      showNotification("Données invalides pour le graphique!", type = "error", duration = 5)
      return(NULL)
    }
    
    # Convertir explicitement en data frame pour éviter les problèmes avec toJSON
    df_plot <- data.frame(
      Row_Var = as.character(df_plot$Row_Var),
      Col_Var = as.character(df_plot$Col_Var),
      Freq = as.numeric(df_plot$Freq)
    )
    
    title <- if (!is.null(input$crosstabTitle) && input$crosstabTitle != "") {
      input$crosstabTitle
    } else {
      paste("Tableau croise :", input$crosstabRowVar, "vs", input$crosstabColVar)
    }
    
    x_label <- if (!is.null(input$crosstabXLabel) && input$crosstabXLabel != "") {
      input$crosstabXLabel  
    } else {
      input$crosstabRowVar
    }
    
    y_label <- if (!is.null(input$crosstabYLabel) && input$crosstabYLabel != "") {
      input$crosstabYLabel
    } else {
      "Effectifs"
    }
    
    x_rotation <- if (!is.null(input$xAxisRotation)) input$xAxisRotation else 45
    x_hjust <- if (x_rotation > 0) 1 else 0.5
    
    base_theme <- theme_minimal() +
      theme(
        plot.title = element_text(size = input$titleSize, hjust = 0.5, face = "bold"),
        axis.title.x = create_text_element(
          if(!is.null(input$axisLabelSize)) input$axisLabelSize else 12,
          if(!is.null(input$axisTitleBold)) input$axisTitleBold else TRUE,
          if(!is.null(input$axisTitleItalic)) input$axisTitleItalic else FALSE
        ),
        axis.title.y = create_text_element(
          if(!is.null(input$axisLabelSize)) input$axisLabelSize else 12,
          if(!is.null(input$axisTitleBold)) input$axisTitleBold else TRUE,
          if(!is.null(input$axisTitleItalic)) input$axisTitleItalic else FALSE
        ),
        axis.text.x = element_text(
          angle = x_rotation, 
          hjust = x_hjust, 
          size = input$axisTextSize,
          face = if(!is.null(input$axisTextBold) && !is.null(input$axisTextItalic)) {
            if(input$axisTextBold && input$axisTextItalic) "bold.italic" 
            else if(input$axisTextBold) "bold" 
            else if(input$axisTextItalic) "italic" 
            else "plain"
          } else {
            "plain"
          }
        ),
        axis.text.y = create_text_element(
          input$axisTextSize,
          if(!is.null(input$axisTextBold)) input$axisTextBold else FALSE,
          if(!is.null(input$axisTextItalic)) input$axisTextItalic else FALSE
        ),
        legend.text = element_text(size = input$legendTextSize),
        legend.title = element_text(size = input$legendTextSize, face = "bold"),
        panel.grid = if(!is.null(input$showGridLines) && input$showGridLines) {
          element_line(color = "gray90")
        } else {
          element_blank()
        }
      )
    
    if (input$plotType == "bar") {
      p <- ggplot(df_plot, aes(x = Row_Var, y = Freq, fill = Col_Var)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.85, 
                 color = "white", linewidth = 0.3) +
        labs(title = title, x = x_label, y = y_label, fill = input$crosstabColVar) +
        base_theme
      
      if (!is.null(input$showPercentages) && input$showPercentages) {
        p <- p + geom_text(aes(label = Freq), position = position_dodge(0.9), 
                           vjust = -0.5, size = 3.5, fontface = "bold")
      }
      
    } else if (input$plotType == "stacked_bar") {
      p <- ggplot(df_plot, aes(x = Row_Var, y = Freq, fill = Col_Var)) +
        geom_bar(stat = "identity", position = "stack", alpha = 0.85, 
                 color = "white", linewidth = 0.3) +
        labs(title = title, x = x_label, y = y_label, fill = input$crosstabColVar) +
        base_theme
      
      if (!is.null(input$showPercentages) && input$showPercentages) {
        p <- p + geom_text(aes(label = Freq), position = position_stack(vjust = 0.5),
                           size = 3.5, fontface = "bold", color = "white")
      }
      
    } else if (input$plotType == "mosaic") {
      # Calculer les proportions explicitement pour éviter les problèmes de vecteurs nommés
      df_plot$prop <- ave(df_plot$Freq, df_plot$Row_Var, FUN = function(x) x / sum(x))
      
      p <- ggplot(df_plot, aes(x = Row_Var, y = prop, fill = Col_Var)) +
        geom_bar(stat = "identity", position = "fill", alpha = 0.85, 
                 color = "white", linewidth = 0.3) +
        labs(title = title, x = x_label, y = "Proportions", fill = input$crosstabColVar) +
        base_theme +
        scale_y_continuous(labels = scales::percent)
      
      if (!is.null(input$showPercentages) && input$showPercentages) {
        p <- p + geom_text(aes(label = paste0(round(prop * 100, 1), "%")), 
                           position = position_fill(vjust = 0.5),
                           size = 3, fontface = "bold", color = "black")
      }
    }
    
    if (!is.null(input$colorPalette)) {
      p <- apply_color_palette(p, input$colorPalette)
    }
    
    crosstab_values$current_plot <- p
    print(p)
  })
  
  # Generation du graphique en secteurs
  output$crosstabPiePlot <- renderPlot({
    req(crosstab_values$contingency_table, input$pieVariable)
    
    if (input$pieVariable == "row") {
      pie_data <- rowSums(crosstab_values$contingency_table)
      pie_data <- pie_data[names(pie_data) != "Sum"]
      var_name <- input$crosstabRowVar
    } else {
      pie_data <- colSums(crosstab_values$contingency_table)
      pie_data <- pie_data[names(pie_data) != "Sum"]
      var_name <- input$crosstabColVar
    }
    
    df_pie <- data.frame(
      Category = names(pie_data),
      Count = as.numeric(pie_data)
    )
    df_pie$Percentage <- round(df_pie$Count / sum(df_pie$Count) * 100, 1)
    
    title <- if (!is.null(input$crosstabTitle) && input$crosstabTitle != "") {
      paste(input$crosstabTitle, "-", var_name)
    } else {
      paste("Repartition de", var_name)
    }
    
    p <- ggplot(df_pie, aes(x = "", y = Count, fill = Category)) +
      geom_bar(stat = "identity", width = 1, alpha = 0.9, 
               color = "white", linewidth = 1) +
      coord_polar(theta = "y") +
      labs(title = title, fill = var_name) +
      theme_void() +
      theme(
        plot.title = element_text(size = input$titleSize, hjust = 0.5, 
                                  face = "bold", margin = margin(b = 20)),
        legend.text = element_text(size = input$legendTextSize),
        legend.title = element_text(size = input$legendTextSize, face = "bold"),
        legend.position = "right"
      )
    
    if (!is.null(input$showPercentages) && input$showPercentages) {
      p <- p + geom_text(aes(label = paste0(Percentage, "%")), 
                         position = position_stack(vjust = 0.5),
                         size = 4, fontface = "bold", color = "black")
    }
    
    if (!is.null(input$pieColorPalette)) {
      p <- apply_color_palette(p, input$pieColorPalette)
    }
    
    crosstab_values$current_pie_plot <- p
    print(p)
  })
  
  # Telechargement des tableaux - Effectifs
  output$downloadCrosstabExcel <- downloadHandler(
    filename = function() paste0("tableau_croise_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(crosstab_values$contingency_table)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Effectifs")
      openxlsx::writeData(wb, "Effectifs", 
                          as.data.frame.matrix(crosstab_values$contingency_table), 
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Tableau téléchargé avec succès! (Excel)", 
                       type = "message", duration = 3)
    }
  )
  
  output$downloadCrosstabCSV <- downloadHandler(
    filename = function() paste0("tableau_croise_", Sys.Date(), ".csv"),
    content = function(file) {
      req(crosstab_values$contingency_table)
      write.csv(as.data.frame.matrix(crosstab_values$contingency_table), 
                file, row.names = TRUE)
      showNotification("Tableau téléchargé avec succès! (CSV)", 
                       type = "message", duration = 3)
    }
  )
  
  # Telechargement des tableaux - Proportions lignes
  output$downloadRowPropExcel <- downloadHandler(
    filename = function() paste0("proportions_lignes_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(crosstab_values$row_proportions)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Prop_Lignes")
      openxlsx::writeData(wb, "Prop_Lignes", 
                          round(as.data.frame.matrix(crosstab_values$row_proportions), 2), 
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Proportions lignes téléchargées! (Excel)", 
                       type = "message", duration = 3)
    }
  )
  
  output$downloadRowPropCSV <- downloadHandler(
    filename = function() paste0("proportions_lignes_", Sys.Date(), ".csv"),
    content = function(file) {
      req(crosstab_values$row_proportions)
      write.csv(round(as.data.frame.matrix(crosstab_values$row_proportions), 2), 
                file, row.names = TRUE)
      showNotification("Proportions lignes téléchargées! (CSV)", 
                       type = "message", duration = 3)
    }
  )
  
  # Telechargement des tableaux - Proportions colonnes
  output$downloadColPropExcel <- downloadHandler(
    filename = function() paste0("proportions_colonnes_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(crosstab_values$col_proportions)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Prop_Colonnes")
      openxlsx::writeData(wb, "Prop_Colonnes", 
                          round(as.data.frame.matrix(crosstab_values$col_proportions), 2), 
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Proportions colonnes téléchargées! (Excel)", 
                       type = "message", duration = 3)
    }
  )
  
  output$downloadColPropCSV <- downloadHandler(
    filename = function() paste0("proportions_colonnes_", Sys.Date(), ".csv"),
    content = function(file) {
      req(crosstab_values$col_proportions)
      write.csv(round(as.data.frame.matrix(crosstab_values$col_proportions), 2), 
                file, row.names = TRUE)
      showNotification("Proportions colonnes téléchargées! (CSV)", 
                       type = "message", duration = 3)
    }
  )
  
  # Telechargement des tableaux - Proportions totales
  output$downloadTotalPropExcel <- downloadHandler(
    filename = function() paste0("proportions_totales_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(crosstab_values$total_proportions)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Prop_Totales")
      openxlsx::writeData(wb, "Prop_Totales", 
                          round(as.data.frame.matrix(crosstab_values$total_proportions), 2), 
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Proportions totales téléchargées! (Excel)", 
                       type = "message", duration = 3)
    }
  )
  
  output$downloadTotalPropCSV <- downloadHandler(
    filename = function() paste0("proportions_totales_", Sys.Date(), ".csv"),
    content = function(file) {
      req(crosstab_values$total_proportions)
      write.csv(round(as.data.frame.matrix(crosstab_values$total_proportions), 2), 
                file, row.names = TRUE)
      showNotification("Proportions totales téléchargées! (CSV)", 
                       type = "message", duration = 3)
    }
  )
  
  # Telechargement des tableaux - Tests statistiques
  output$downloadTestsExcel <- downloadHandler(
    filename = function() paste0("tests_statistiques_", Sys.Date(), ".xlsx"),
    content = function(file) {
      tests_df <- data.frame(
        Test = character(),
        Statistique = numeric(),
        p_value = numeric(),
        Interpretation = character(),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(crosstab_values$chi_test) && is.list(crosstab_values$chi_test)) {
        tests_df <- rbind(tests_df, data.frame(
          Test = "Chi-deux",
          Statistique = crosstab_values$chi_test$statistic,
          p_value = crosstab_values$chi_test$p.value,
          Interpretation = ifelse(crosstab_values$chi_test$p.value < 0.05, 
                                  "Significatif", "Non significatif")
        ))
      }
      
      if (!is.null(crosstab_values$fisher_test) && is.list(crosstab_values$fisher_test)) {
        tests_df <- rbind(tests_df, data.frame(
          Test = "Fisher exact",
          Statistique = NA,
          p_value = crosstab_values$fisher_test$p.value,
          Interpretation = ifelse(crosstab_values$fisher_test$p.value < 0.05, 
                                  "Significatif", "Non significatif")
        ))
      }
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Tests")
      openxlsx::writeData(wb, "Tests", tests_df)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Tests statistiques téléchargés! (Excel)", 
                       type = "message", duration = 3)
    }
  )
  
  output$downloadTestsCSV <- downloadHandler(
    filename = function() paste0("tests_statistiques_", Sys.Date(), ".csv"),
    content = function(file) {
      tests_df <- data.frame(
        Test = character(),
        Statistique = numeric(),
        p_value = numeric(),
        Interpretation = character(),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(crosstab_values$chi_test) && is.list(crosstab_values$chi_test)) {
        tests_df <- rbind(tests_df, data.frame(
          Test = "Chi-deux",
          Statistique = crosstab_values$chi_test$statistic,
          p_value = crosstab_values$chi_test$p.value,
          Interpretation = ifelse(crosstab_values$chi_test$p.value < 0.05, 
                                  "Significatif", "Non significatif")
        ))
      }
      
      if (!is.null(crosstab_values$fisher_test) && is.list(crosstab_values$fisher_test)) {
        tests_df <- rbind(tests_df, data.frame(
          Test = "Fisher exact",
          Statistique = NA,
          p_value = crosstab_values$fisher_test$p.value,
          Interpretation = ifelse(crosstab_values$fisher_test$p.value < 0.05, 
                                  "Significatif", "Non significatif")
        ))
      }
      
      write.csv(tests_df, file, row.names = FALSE)
      showNotification("Tests statistiques téléchargés! (CSV)", 
                       type = "message", duration = 3)
    }
  )
  
  # Telechargement des tableaux - Résidus
  output$downloadResidualsExcel <- downloadHandler(
    filename = function() paste0("residus_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(crosstab_values$residuals)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Residus")
      openxlsx::writeData(wb, "Residus", 
                          round(as.data.frame.matrix(crosstab_values$residuals), 2), 
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Résidus téléchargés! (Excel)", 
                       type = "message", duration = 3)
    }
  )
  
  output$downloadResidualsCSV <- downloadHandler(
    filename = function() paste0("residus_", Sys.Date(), ".csv"),
    content = function(file) {
      req(crosstab_values$residuals)
      write.csv(round(as.data.frame.matrix(crosstab_values$residuals), 2), 
                file, row.names = TRUE)
      showNotification("Résidus téléchargés! (CSV)", 
                       type = "message", duration = 3)
    }
  )
  
  # Telechargement du graphique principal
  output$downloadPlot <- downloadHandler(
    filename = function() {
      ext <- if(!is.null(input$mainPlotFormat)) input$mainPlotFormat else "png"
      dpi <- if(!is.null(input$mainPlotDPI)) input$mainPlotDPI else 300
      paste0("graphique_croise_", Sys.Date(), "_", dpi, "dpi.", ext)
    },
    content = function(file) {
      req(crosstab_values$current_plot)
      
      width_px <- if(!is.null(input$mainPlotWidth)) input$mainPlotWidth else 800
      height_px <- if(!is.null(input$mainPlotHeight)) input$mainPlotHeight else 600
      dpi <- if(!is.null(input$mainPlotDPI)) input$mainPlotDPI else 300
      format <- if(!is.null(input$mainPlotFormat)) input$mainPlotFormat else "png"
      
      width_in <- width_px / dpi
      height_in <- height_px / dpi
      
      tryCatch({
        if (format %in% c("png", "tiff", "bmp")) {
          ggsave(file, 
                 plot = crosstab_values$current_plot, 
                 width = width_in, 
                 height = height_in, 
                 dpi = dpi,
                 device = format,
                 bg = "white")
        } else if (format == "jpeg") {
          ggsave(file, 
                 plot = crosstab_values$current_plot, 
                 width = width_in, 
                 height = height_in, 
                 dpi = dpi,
                 device = "jpeg",
                 bg = "white",
                 quality = 95)
        } else if (format %in% c("pdf", "svg", "eps")) {
          ggsave(file, 
                 plot = crosstab_values$current_plot, 
                 width = width_in, 
                 height = height_in,
                 device = format)
        }
        
        showNotification(
          paste0("Graphique téléchargé avec succès! (", width_px, "x", height_px, 
                 "px, ", dpi, " DPI, ", toupper(format), ")"), 
          type = "message", 
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("Erreur lors du téléchargement du graphique :", e$message), 
          type = "error", 
          duration = 10
        )
      })
    }
  )
  
  # Telechargement du graphique en secteurs
  output$downloadPiePlot <- downloadHandler(
    filename = function() {
      ext <- if(!is.null(input$piePlotFormat)) input$piePlotFormat else "png"
      dpi <- if(!is.null(input$piePlotDPI)) input$piePlotDPI else 300
      paste0("graphique_secteurs_", Sys.Date(), "_", dpi, "dpi.", ext)
    },
    content = function(file) {
      req(crosstab_values$current_pie_plot)
      
      width_px <- if(!is.null(input$piePlotWidth)) input$piePlotWidth else 800
      height_px <- if(!is.null(input$piePlotHeight)) input$piePlotHeight else 800
      dpi <- if(!is.null(input$piePlotDPI)) input$piePlotDPI else 300
      format <- if(!is.null(input$piePlotFormat)) input$piePlotFormat else "png"
      
      width_in <- width_px / dpi
      height_in <- height_px / dpi
      
      tryCatch({
        if (format %in% c("png", "tiff", "bmp")) {
          ggsave(file, 
                 plot = crosstab_values$current_pie_plot, 
                 width = width_in, 
                 height = height_in, 
                 dpi = dpi,
                 device = format,
                 bg = "white")
        } else if (format == "jpeg") {
          ggsave(file, 
                 plot = crosstab_values$current_pie_plot, 
                 width = width_in, 
                 height = height_in, 
                 dpi = dpi,
                 device = "jpeg",
                 bg = "white",
                 quality = 95)
        } else if (format %in% c("pdf", "svg", "eps")) {
          ggsave(file, 
                 plot = crosstab_values$current_pie_plot, 
                 width = width_in, 
                 height = height_in,
                 device = format)
        }
        
        showNotification(
          paste0("Graphique secteurs téléchargé avec succès! (", width_px, "x", height_px, 
                 "px, ", dpi, " DPI, ", toupper(format), ")"), 
          type = "message", 
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("Erreur lors du téléchargement :", e$message),
          type = "error", 
          duration = 10
        )
      })
    }
  )
  # ---- Visualisation des données ----
  
  
  # SECTION 1: SÉLECTION DES VARIABLES
  
  
  # Sélection des variables X 
  output$vizXVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    all_cols <- iconv(all_cols, to = "UTF-8", sub = "")
    selectInput("vizXVar", "Variable X:", 
                choices = all_cols,
                selected = if(length(all_cols) > 0) all_cols[1] else NULL)
  })
  
  # Sélection des variables Y (MULTIPLE) 
  output$vizYVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    all_cols <- iconv(all_cols, to = "UTF-8", sub = "")
    
    div(
      selectizeInput("vizYVar", "Variable(s) Y:", 
                     choices = all_cols,
                     selected = if(length(all_cols) > 1) all_cols[2] else NULL,
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Sélectionnez une ou plusieurs variables...',
                       maxItems = 10,
                       plugins = list('remove_button')
                     )),
      helpText(icon("info-circle"), "Sélectionnez plusieurs variables Y pour les superposer sur le même graphique.")
    )
  })
  
  # Sélection de la variable de couleur 
  output$vizColorVarSelect <- renderUI({
    req(values$filteredData)
    
    # Exclure les variables Y multiples si actives
    all_cols <- names(values$filteredData)
    if(!is.null(values$multipleY) && values$multipleY) {
      all_cols <- setdiff(all_cols, input$vizYVar)
    }
    
    selectInput("vizColorVar", "Variable couleur:",
                choices = c("Aucun" = "Aucun", all_cols),
                selected = "Aucun")
  })
  
  # Sélection de la variable de facetting 
  output$vizFacetVarSelect <- renderUI({
    req(values$filteredData)
    
    all_cols <- names(values$filteredData)
    # Filtrer pour ne garder que les variables catégorielles
    cat_cols <- names(values$filteredData)[sapply(values$filteredData, function(x) {
      is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 20)
    })]
    
    selectInput("vizFacetVar", "Variable facetting:",
                choices = c("Aucun" = "Aucun", cat_cols),
                selected = "Aucun")
  })
  
  # Sélection des variables de regroupement pour l'agrégation
  output$groupVarsSelect <- renderUI({
    req(values$filteredData)
    req(input$useAggregation == TRUE)
    
    all_cols <- names(values$filteredData)
    
    # Exclure la variable X et les variables Y
    excluded_vars <- c(input$vizXVar, input$vizYVar)
    available_cols <- setdiff(all_cols, excluded_vars)
    
    # Priorité aux variables catégorielles mais inclure aussi les numériques
    cat_cols <- available_cols[sapply(values$filteredData[available_cols], function(x) {
      is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 20)
    })]
    
    # Si pas de variables catégorielles, utiliser toutes les variables disponibles
    if(length(cat_cols) == 0) {
      cat_cols <- available_cols
    }
    
    div(
      selectizeInput(
        "groupVars",
        "Variables de regroupement:",
        choices = c("Variable X (par défaut)" = input$vizXVar, cat_cols),
        selected = input$vizXVar,
        multiple = TRUE,
        options = list(
          placeholder = 'Sélectionnez les variables...',
          maxItems = 5,
          plugins = list('remove_button')
        )
      ),
      helpText(
        icon("info-circle", style = "color: #17a2b8;"),
        "Sélectionnez les variables qui définissent les groupes pour l'agrégation. Par défaut, X est utilisée."
      )
    )
  })
  
  
  
  # SECTION 2: DÉTECTION AUTOMATIQUE
  
  
  # Détection automatique du type de variable X 
  observe({
    req(values$filteredData, input$vizXVar)
    
    if(input$xVarType == "auto") {
      data <- values$filteredData
      x_var_data <- data[[input$vizXVar]]
      
      detected_type <- if(inherits(x_var_data, "Date") || inherits(x_var_data, "POSIXt")) {
        "date"
      } else if(is.factor(x_var_data)) {
        "factor"
      } else if(is.numeric(x_var_data)) {
        "numeric"
      } else if(is.character(x_var_data)) {
        if(length(unique(x_var_data)) < length(x_var_data)/2) {
          "categorical"
        } else {
          "text"
        }
      } else {
        "text"
      }
      
      values$detectedXType <- detected_type
    }
  })
  
  # Détection du mode multi-Y 
  observe({
    req(input$vizYVar)
    
    if(length(input$vizYVar) > 1) {
      values$multipleY <- TRUE
      values$yVarNames <- input$vizYVar
      showNotification(
        paste("Mode multi-Y activé:", length(input$vizYVar), "variables sélectionnées"),
        type = "message",
        duration = 3
      )
    } else {
      values$multipleY <- FALSE
      values$yVarNames <- NULL
    }
  })
  
  
  # SECTION 3: GESTION DES NIVEAUX X - AMÉLIORATION POUR PERSISTANCE
  
  # NOUVELLE FONCTIONNALITÉ: Initialiser le stockage des labels personnalisés
  observe({
    if(is.null(values$storedLevelLabels)) {
      values$storedLevelLabels <- list()
    }
    if(is.null(values$legendLabels)) {
      values$legendLabels <- list()
    }
  }, priority = 1000)
  
  
  # Stocker les niveaux actuels pour l'éditeur d'ordre
  observe({
    req(values$filteredData, input$vizXVar)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    
    if(x_type %in% c("factor", "categorical", "text")) {
      unique_vals <- if(is.factor(data[[x_var]])) {
        levels(data[[x_var]])
      } else {
        unique(as.character(data[[x_var]]))
      }
      values$currentXLevels <- unique_vals
      
      # NOUVELLE: Stocker les labels personnalisés de manière persistante
      if(!is.null(input$vizXVar) && is.null(values$storedLevelLabels[[input$vizXVar]])) {
        values$storedLevelLabels[[input$vizXVar]] <- unique_vals
      }
    }
  })
  
  # Éditeur de niveaux amélioré pour la variable X 
  output$xLevelsEditor <- renderUI({
    req(values$filteredData, input$vizXVar)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    
    if(!x_type %in% c("factor", "categorical", "text", "date")) return(NULL)
    
    if(x_type == "date") {
      if(!inherits(data[[x_var]], "Date") && !inherits(data[[x_var]], "POSIXt")) {
        return(div(
          p("Format de date détecté. Utilisez le sélecteur de format ci-dessus pour la conversion.", 
            style = "color: #666; font-style: italic;")
        ))
      }
      unique_vals <- as.character(sort(unique(data[[x_var]])))
    } else {
      unique_vals <- if(is.factor(data[[x_var]])) {
        levels(droplevels(data[[x_var]]))
      } else {
        sort(unique(as.character(data[[x_var]])))
      }
    }
    
    if(length(unique_vals) == 0) {
      return(div(p("Aucune valeur trouvée pour la variable sélectionnée.", 
                   style = "color: #999;")))
    }
    
    if(length(unique_vals) > 50) {
      return(div(
        p(paste("Trop de valeurs uniques (", length(unique_vals), "). ", 
                "Considérez l'agrégation ou le regroupement.", sep = ""), 
          style = "color: #ff9800; font-weight: bold;"),
        actionButton("showAllLevels", "Afficher quand même", 
                     class = "btn-warning btn-sm", icon = icon("eye"))
      ))
    }
    
    div(
      div(style = "margin-bottom: 10px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              span(paste(length(unique_vals), "niveaux/valeurs"), 
                   style = "color: #666; font-size: 12px;"),
              div(style = "display: flex; gap: 5px;",
                  actionButton("applyLabels", "Appliquer", 
                               class = "btn-success btn-xs", icon = icon("check")),
                  actionButton("resetLevels", "Réinitialiser", 
                               class = "btn-default btn-xs", icon = icon("undo"))
              )
          )
      ),
      
      div(style = if(length(unique_vals) > 10) "max-height: 400px; overflow-y: auto; padding-right: 10px;" else "",
          lapply(seq_along(unique_vals), function(i) {
            lvl <- unique_vals[i]
            # NOUVELLE: Récupérer le label stocké s'il existe
            stored_label <- values$storedLevelLabels[[x_var]]
            default_val <- if(!is.null(stored_label) && !is.null(names(stored_label)) && lvl %in% names(stored_label)) {
              stored_label[[lvl]]
            } else {
              lvl
            }
            
            div(style = "margin-bottom: 8px; padding: 8px; background-color: #fafafa; border-radius: 4px; border: 1px solid #e0e0e0;",
                div(style = "display: flex; align-items: center; gap: 10px;",
                    span(paste0(i, "."), style = "color: #999; font-weight: bold; min-width: 25px;"),
                    div(style = "flex: 1;",
                        div(style = "font-size: 11px; color: #666; margin-bottom: 2px;",
                            paste("Original:", lvl)),
                        textInput(
                          inputId = paste0("xLevel_", make.names(lvl)),
                          label = NULL,
                          value = default_val,
                          placeholder = "Nouvelle étiquette...",
                          width = "100%"
                        )
                    )
                )
            )
          })
      ),
      
      if(length(unique_vals) > 3) {
        div(style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #e0e0e0;",
            h6("Actions rapides", style = "color: #666; font-weight: bold;"),
            div(style = "display: flex; gap: 5px; flex-wrap: wrap;",
                actionButton("addPrefixBtn", "Ajouter préfixe", 
                             class = "btn-info btn-xs", icon = icon("plus-circle")),
                actionButton("addSuffixBtn", "Ajouter suffixe", 
                             class = "btn-info btn-xs", icon = icon("plus-circle")),
                actionButton("numberLevelsBtn", "Numéroter", 
                             class = "btn-info btn-xs", icon = icon("sort-numeric-up")),
                actionButton("cleanSpacesBtn", "Nettoyer espaces", 
                             class = "btn-info btn-xs", icon = icon("broom"))
            )
        )
      }
    )
  })
  
  # NOUVELLE: Observateur pour appliquer et stocker les labels
  observeEvent(input$applyLabels, {
    req(values$currentXLevels, input$vizXVar)
    
    # Créer le mapping des labels
    level_mapping <- sapply(values$currentXLevels, function(lvl) {
      new_label <- input[[paste0("xLevel_", make.names(lvl))]]
      if(is.null(new_label) || new_label == "") lvl else new_label
    })
    
    # Stocker de manière persistante
    names(level_mapping) <- values$currentXLevels
    values$storedLevelLabels[[input$vizXVar]] <- level_mapping
    
    showNotification("Labels appliqués et enregistrés", type = "message", duration = 2)
  })
  
  # Observateur pour réinitialiser les niveaux avec notification
  observeEvent(input$resetLevels, {
    req(values$currentXLevels, input$vizXVar)
    
    # Réinitialiser tous les inputs de niveau
    lapply(values$currentXLevels, function(lvl) {
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), value = lvl)
    })
    
    # Supprimer le stockage persistant
    values$storedLevelLabels[[input$vizXVar]] <- NULL
    
    showNotification("Niveaux réinitialisés", type = "message", duration = 2)
  })
  
  # Actions rapides pour les niveaux
  observeEvent(input$addPrefixBtn, {
    req(values$currentXLevels)
    
    showModal(modalDialog(
      title = "Ajouter un préfixe",
      textInput("prefixText", "Préfixe à ajouter:", value = ""),
      footer = tagList(
        actionButton("applyPrefix", "Appliquer", class = "btn-primary"),
        modalButton("Annuler")
      )
    ))
  })
  
  observeEvent(input$applyPrefix, {
    req(input$prefixText, values$currentXLevels)
    
    lapply(values$currentXLevels, function(lvl) {
      current_val <- input[[paste0("xLevel_", make.names(lvl))]]
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), 
                      value = paste0(input$prefixText, current_val))
    })
    
    removeModal()
    showNotification("Préfixe ajouté à tous les niveaux", type = "message", duration = 2)
  })
  
  observeEvent(input$addSuffixBtn, {
    req(values$currentXLevels)
    
    showModal(modalDialog(
      title = "Ajouter un suffixe",
      textInput("suffixText", "Suffixe à ajouter:", value = ""),
      footer = tagList(
        actionButton("applySuffix", "Appliquer", class = "btn-primary"),
        modalButton("Annuler")
      )
    ))
  })
  
  observeEvent(input$applySuffix, {
    req(input$suffixText, values$currentXLevels)
    
    lapply(values$currentXLevels, function(lvl) {
      current_val <- input[[paste0("xLevel_", make.names(lvl))]]
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), 
                      value = paste0(current_val, input$suffixText))
    })
    
    removeModal()
    showNotification("Suffixe ajouté à tous les niveaux", type = "message", duration = 2)
  })
  
  observeEvent(input$numberLevelsBtn, {
    req(values$currentXLevels)
    
    lapply(seq_along(values$currentXLevels), function(i) {
      lvl <- values$currentXLevels[i]
      current_val <- input[[paste0("xLevel_", make.names(lvl))]]
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), 
                      value = paste0(i, ". ", current_val))
    })
    
    showNotification("Niveaux numérotés", type = "message", duration = 2)
  })
  
  observeEvent(input$cleanSpacesBtn, {
    req(values$currentXLevels)
    
    lapply(values$currentXLevels, function(lvl) {
      current_val <- input[[paste0("xLevel_", make.names(lvl))]]
      cleaned_val <- trimws(gsub("\\s+", " ", current_val))
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), 
                      value = cleaned_val)
    })
    
    showNotification("Espaces nettoyés", type = "message", duration = 2)
  })
  
  
  # SECTION 4: GESTION DE L'ORDRE X
  
  
  # Éditeur d'ordre pour X catégoriel (seasonal_evolution)
  output$xOrderEditor <- renderUI({
    req(values$filteredData, input$vizXVar)
    req(input$vizType == "seasonal_evolution")
    
    x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    
    if(!x_type %in% c("factor", "categorical", "text")) return(NULL)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    
    unique_vals <- if(is.factor(data[[x_var]])) {
      levels(data[[x_var]])
    } else {
      sort(unique(as.character(data[[x_var]])))
    }
    
    if(length(unique_vals) == 0) return(NULL)
    if(length(unique_vals) > 100) {
      return(div(
        p(paste("Trop de catégories (", length(unique_vals), "). Réduisez vos données."), 
          style = "color: #ff9800; font-weight: bold;")
      ))
    }
    
    div(
      div(style = "margin-bottom: 10px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              span(paste(length(unique_vals), "catégories détectées"), 
                   style = "color: #666; font-size: 12px;"),
              div(style = "display: flex; gap: 5px;",
                  actionButton("autoSortX", "Tri auto", 
                               class = "btn-default btn-xs", icon = icon("sort-alpha-down")),
                  actionButton("reverseOrderX", "Inverser", 
                               class = "btn-default btn-xs", icon = icon("exchange-alt")),
                  actionButton("resetOrderX", "Réinitialiser", 
                               class = "btn-default btn-xs", icon = icon("undo"))
              )
          )
      ),
      
      div(id = "xOrderSortable",
          style = if(length(unique_vals) > 15) "max-height: 500px; overflow-y: auto; padding: 10px; background-color: #f9f9f9; border-radius: 5px;" else "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          lapply(seq_along(unique_vals), function(i) {
            val <- unique_vals[i]
            div(
              id = paste0("xorder_", i),
              `data-value` = val,
              class = "sortable-item",
              style = "cursor: move; padding: 12px; margin-bottom: 8px; background-color: white; border: 2px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); transition: all 0.2s;",
              div(style = "display: flex; align-items: center; gap: 10px;",
                  icon("grip-vertical", style = "color: #999;"),
                  span(style = "font-weight: bold; color: #28a745; min-width: 30px;", paste0(i, ".")),
                  span(style = "flex: 1; font-size: 14px;", val)
              )
            )
          })
      ),
      
      helpText(icon("hand-point-up"), 
               "Glissez-déposez les éléments pour définir l'ordre d'apparition sur le graphique.",
               style = "margin-top: 10px; color: #666;")
    )
  })
  
  # Tri automatique de l'ordre X
  observeEvent(input$autoSortX, {
    req(values$currentXLevels)
    sorted_levels <- sort(values$currentXLevels)
    values$customXOrder <- sorted_levels
    showNotification("Ordre trié alphabétiquement", type = "message", duration = 2)
  })
  
  # Inverser l'ordre X
  observeEvent(input$reverseOrderX, {
    req(values$currentXLevels)
    values$customXOrder <- rev(values$currentXLevels)
    showNotification("Ordre inversé", type = "message", duration = 2)
  })
  
  # Réinitialiser l'ordre X
  observeEvent(input$resetOrderX, {
    values$customXOrder <- NULL
    showNotification("Ordre réinitialisé", type = "message", duration = 2)
  })
  
  # Capturer l'ordre personnalisé depuis le sortable (drag-and-drop)
  observeEvent(input$xLevelOrder, {
    if(!is.null(input$xLevelOrder) && length(input$xLevelOrder) > 0) {
      values$customXOrder <- input$xLevelOrder
      showNotification(
        paste("Ordre modifié:", length(input$xLevelOrder), "catégories"), 
        type = "message", 
        duration = 2
      )
    }
  })
  
  
  # SECTION 4.5: AGRÉGATION DES DONNÉES
  
  
  # Expression réactive pour agréger les données si nécessaire
  aggregatedData <- reactive({
    req(values$filteredData, input$vizXVar, input$vizYVar)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    y_vars <- input$vizYVar
    
    # Vérifier si l'agrégation est activée
    if(isTRUE(input$useAggregation) && !is.null(input$aggFunction)) {
      
      # Déterminer les variables de regroupement
      group_vars <- if(!is.null(input$groupVars) && length(input$groupVars) > 0) {
        input$groupVars
      } else {
        x_var
      }
      
      # Ajouter la variable de couleur au groupement si présente et pas en mode multi-Y
      if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun" && 
         (is.null(values$multipleY) || !values$multipleY)) {
        group_vars <- unique(c(group_vars, input$vizColorVar))
      }
      
      # Fonction d'agrégation
      agg_func <- switch(input$aggFunction,
                         "mean" = function(x) mean(x, na.rm = TRUE),
                         "median" = function(x) median(x, na.rm = TRUE),
                         "sum" = function(x) sum(x, na.rm = TRUE),
                         "count" = function(x) length(x),
                         "min" = function(x) min(x, na.rm = TRUE),
                         "max" = function(x) max(x, na.rm = TRUE),
                         "sd" = function(x) sd(x, na.rm = TRUE),
                         function(x) mean(x, na.rm = TRUE))
      
      # Agréger pour chaque variable Y
      if(length(y_vars) == 1) {
        # Mode Y simple
        data <- data %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(!!y_vars[1] := agg_func(.data[[y_vars[1]]]), .groups = "drop")
      } else {
        # Mode multi-Y - agréger chaque variable Y séparément
        agg_list <- list()
        for(y_var in y_vars) {
          temp_data <- data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(value = agg_func(.data[[y_var]]), .groups = "drop") %>%
            mutate(Variable = y_var)
          agg_list[[y_var]] <- temp_data
        }
        data <- bind_rows(agg_list)
      }
      
      showNotification(
        paste("Données agrégées:", nrow(data), "observations"),
        type = "message",
        duration = 2
      )
    }
    
    return(data)
  })
  
  
  # SECTION 5: PRÉPARATION DES DONNÉES
  
  
  # Expression réactive pour préparer les données du graphique
  plotData <- reactive({
    req(values$filteredData, input$vizXVar, input$vizYVar)
    
    # Utiliser les données agrégées si disponibles
    data <- if(isTRUE(input$useAggregation)) {
      aggregatedData()
    } else {
      values$filteredData
    }
    
    x_var <- input$vizXVar
    y_vars <- input$vizYVar
    
    # Appliquer les renommages de niveaux X si disponibles
    x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    
    if(x_type %in% c("factor", "categorical", "text")) {
      # NOUVELLE: Utiliser les labels stockés de manière persistante
      if(!is.null(values$storedLevelLabels[[x_var]])) {
        level_mapping <- values$storedLevelLabels[[x_var]]
        
        # Obtenir les niveaux actuellement présents dans les données
        current_levels <- unique(as.character(data[[x_var]]))
        
        # Ne garder que les niveaux qui existent dans le mapping ET dans les données
        valid_levels <- names(level_mapping)[names(level_mapping) %in% current_levels]
        
        if(length(valid_levels) > 0) {
          data[[x_var]] <- factor(
            data[[x_var]],
            levels = valid_levels,
            labels = as.character(level_mapping[valid_levels])
          )
        } else {
          # Si aucun niveau valide, utiliser les données telles quelles
          data[[x_var]] <- factor(data[[x_var]])
        }
      } else if(!is.null(values$currentXLevels)) {
        # Obtenir les niveaux actuellement présents dans les données
        current_levels <- unique(as.character(data[[x_var]]))
        
        # Ne garder que les niveaux qui existent à la fois dans currentXLevels et dans les données
        valid_levels <- values$currentXLevels[values$currentXLevels %in% current_levels]
        
        if(length(valid_levels) > 0) {
          level_mapping <- sapply(valid_levels, function(lvl) {
            new_label <- input[[paste0("xLevel_", make.names(lvl))]]
            if(is.null(new_label) || new_label == "") lvl else new_label
          })
          
          data[[x_var]] <- factor(
            data[[x_var]],
            levels = valid_levels,
            labels = as.character(level_mapping)
          )
        } else {
          # Si aucun niveau valide, utiliser les données telles quelles
          data[[x_var]] <- factor(data[[x_var]])
        }
      }
      
      # Appliquer l'ordre personnalisé si défini
      if(!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
        # Ne garder que les niveaux de customXOrder qui existent dans les données
        existing_order <- values$customXOrder[values$customXOrder %in% levels(data[[x_var]])]
        if(length(existing_order) > 0) {
          data[[x_var]] <- factor(data[[x_var]], levels = existing_order)
        }
      } else if(is.null(values$storedLevelLabels[[x_var]]) && is.null(values$currentXLevels)) {
        # Assurer un ordre cohérent par défaut seulement si aucun label n'a été appliqué
        data[[x_var]] <- factor(data[[x_var]])
      }
    }
    
    # Conversion de date si nécessaire
    if(x_type == "date" && !inherits(data[[x_var]], "Date")) {
      date_format <- input$xDateFormat %||% "%Y-%m-%d"
      data[[x_var]] <- tryCatch({
        as.Date(data[[x_var]], format = date_format)
      }, error = function(e) {
        showNotification("Erreur de conversion de date. Vérifiez le format.", type = "error")
        data[[x_var]]
      })
    }
    
    # Gestion du mode multi-Y - CORRECTION ERREUR SELECT
    if(!is.null(values$multipleY) && values$multipleY) {
      if(isTRUE(input$useAggregation) && "Variable" %in% names(data)) {
        # Les données sont déjà en format long après agrégation
        data_long <- data %>%
          rename(Value = value)
      } else {
        # Transformer en format long pour multi-Y
        # Vérifier quelles variables Y existent réellement dans les données
        valid_y_vars <- y_vars[y_vars %in% names(data)]
        
        if(length(valid_y_vars) == 0) {
          showNotification("Erreur: Aucune variable Y disponible dans les données", type = "error")
          return(data)
        }
        
        # Sélectionner X et toutes les variables Y valides
        cols_to_keep <- c(x_var, valid_y_vars)
        
        # Vérifier que X existe aussi
        if(!x_var %in% names(data)) {
          showNotification("Erreur: Variable X non disponible", type = "error")
          return(data)
        }
        
        # Transformer en format long
        
        # CORRECTION: Utiliser dplyr::select et dplyr::any_of explicitement
        data_long <- data %>%
          dplyr::select(dplyr::any_of(cols_to_keep)) %>%
          tidyr::pivot_longer(
            cols = dplyr::any_of(valid_y_vars),
            names_to = "Variable",
            values_to = "Value"
          )
      }
      return(data_long)
    }
    
    # Mode Y simple - garder toutes les colonnes nécessaires
    cols_needed <- unique(c(x_var, y_vars[1], input$vizColorVar, input$vizFacetVar))
    cols_needed <- cols_needed[cols_needed != "Aucun"]
    cols_needed <- cols_needed[cols_needed %in% names(data)]
    
    return(data[, cols_needed, drop = FALSE])
  })
  
  # Stocker les données préparées dans values pour accès global
  observe({
    values$plotData <- plotData()
  })
  
  
  # SECTION 5.5: GESTION DES LABELS DE LÉGENDE
  
  
  # NOUVELLE: Créer une interface pour personnaliser les labels de légende
  observeEvent(input$customizeLegendLabels, {
    req(values$plotData)
    
    # Déterminer quels sont les niveaux de la légende
    legend_levels <- NULL
    legend_var_name <- NULL
    
    if(!is.null(values$multipleY) && values$multipleY) {
      # En mode multi-Y, les niveaux sont les noms des variables Y
      legend_levels <- values$yVarNames
      legend_var_name <- "Variables Y"
    } else if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      # Sinon, utiliser les niveaux de la variable de couleur
      color_data <- values$plotData[[input$vizColorVar]]
      if(is.factor(color_data)) {
        legend_levels <- levels(color_data)
      } else {
        legend_levels <- unique(as.character(color_data))
      }
      legend_var_name <- input$vizColorVar
    }
    
    if(is.null(legend_levels) || length(legend_levels) == 0) {
      showNotification("Aucune légende à personnaliser", type = "warning", duration = 3)
      return()
    }
    
    # Créer l'interface modale pour éditer les labels de légende
    showModal(modalDialog(
      title = tagList(icon("tags"), " Personnaliser les labels de la légende"),
      size = "m",
      
      div(
        p(paste("Variable:", legend_var_name), style = "font-weight: bold; color: #666; margin-bottom: 15px;"),
        
        div(style = if(length(legend_levels) > 10) "max-height: 400px; overflow-y: auto;" else "",
            lapply(seq_along(legend_levels), function(i) {
              lvl <- legend_levels[i]
              div(style = "margin-bottom: 10px; padding: 10px; background-color: #f9f9f9; border-radius: 4px;",
                  div(style = "font-size: 12px; color: #666; margin-bottom: 3px;",
                      paste("Original:", lvl)),
                  textInput(
                    inputId = paste0("legendLevel_", make.names(lvl)),
                    label = NULL,
                    value = lvl,
                    placeholder = "Nouveau label...",
                    width = "100%"
                  )
              )
            })
        )
      ),
      
      footer = tagList(
        actionButton("applyLegendLabels", "Appliquer", class = "btn-primary"),
        actionButton("resetLegendLabels", "Réinitialiser", class = "btn-default"),
        modalButton("Fermer")
      )
    ))
  })
  
  # NOUVELLE: Appliquer les labels de légende personnalisés
  observeEvent(input$applyLegendLabels, {
    # Déterminer la clé de stockage
    storage_key <- if(!is.null(values$multipleY) && values$multipleY) {
      "multiY_legend"
    } else if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      input$vizColorVar
    } else {
      return()
    }
    
    # Récupérer les niveaux actuels
    legend_levels <- if(storage_key == "multiY_legend") {
      values$yVarNames
    } else {
      color_data <- values$plotData[[input$vizColorVar]]
      if(is.factor(color_data)) levels(color_data) else unique(as.character(color_data))
    }
    
    # Créer le mapping
    legend_mapping <- sapply(legend_levels, function(lvl) {
      new_label <- input[[paste0("legendLevel_", make.names(lvl))]]
      if(is.null(new_label) || new_label == "") lvl else new_label
    })
    names(legend_mapping) <- legend_levels
    
    # Stocker
    values$legendLabels[[storage_key]] <- legend_mapping
    
    removeModal()
    showNotification("Labels de légende appliqués", type = "message", duration = 2)
  })
  
  # NOUVELLE: Réinitialiser les labels de légende
  observeEvent(input$resetLegendLabels, {
    storage_key <- if(!is.null(values$multipleY) && values$multipleY) {
      "multiY_legend"
    } else if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      input$vizColorVar
    } else {
      return()
    }
    
    legend_levels <- if(storage_key == "multiY_legend") {
      values$yVarNames
    } else {
      color_data <- values$plotData[[input$vizColorVar]]
      if(is.factor(color_data)) levels(color_data) else unique(as.character(color_data))
    }
    
    # Réinitialiser les inputs
    lapply(legend_levels, function(lvl) {
      updateTextInput(session, paste0("legendLevel_", make.names(lvl)), value = lvl)
    })
    
    # Supprimer le stockage
    values$legendLabels[[storage_key]] <- NULL
    
    showNotification("Labels de légende réinitialisés", type = "message", duration = 2)
  })
  
  
  # SECTION 6: CRÉATION DU GRAPHIQUE
  
  
  # Expression réactive pour créer le graphique avec mise à jour automatique
  createPlot <- reactive({
    req(values$plotData, input$vizXVar, input$vizYVar, input$vizType)
    
    data <- values$plotData
    x_var <- input$vizXVar
    viz_type <- input$vizType
    
    # Déterminer la variable Y selon le mode
    if(!is.null(values$multipleY) && values$multipleY) {
      y_var <- "Value"
      color_var <- "Variable"
    } else {
      y_var <- input$vizYVar[1]
      color_var <- if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
        input$vizColorVar
      } else {
        NULL
      }
    }
    
    # Créer le graphique de base selon le type
    p <- tryCatch({
      switch(viz_type,
             "scatter" = create_scatter_plot(data, x_var, y_var, color_var),
             "line" = create_line_plot(data, x_var, y_var, color_var),
             "bar" = create_bar_plot(data, x_var, y_var, color_var),
             "box" = create_box_plot(data, x_var, y_var, color_var),
             "violin" = create_violin_plot(data, x_var, y_var, color_var),
             "seasonal_smooth" = create_seasonal_smooth_plot(data, x_var, y_var, color_var),
             "seasonal_evolution" = create_seasonal_evolution_plot(data, x_var, y_var, color_var),
             "histogram" = create_histogram_plot(data, x_var),
             "density" = create_density_plot(data, x_var, color_var),
             "heatmap" = create_heatmap_plot(data, x_var, y_var),
             "area" = create_area_plot(data, x_var, y_var, color_var),
             "pie" = create_pie_plot(data, x_var, y_var),
             "donut" = create_donut_plot(data, x_var, y_var),
             "treemap" = create_treemap_plot(data, x_var, y_var),
             {
               showNotification("Type de visualisation non reconnu", type = "error")
               return(NULL)
             }
      )
    }, error = function(e) {
      showNotification(paste("Erreur lors de la création du graphique:", e$message), 
                       type = "error", duration = 5)
      return(NULL)
    })
    
    if(is.null(p)) return(NULL)
    
    # Ajouter le facetting si demandé
    if(!is.null(input$vizFacetVar) && input$vizFacetVar != "Aucun") {
      p <- p + facet_wrap(as.formula(paste("~", input$vizFacetVar)), 
                          scales = if(isTRUE(input$facetScalesFree)) "free" else "fixed")
    }
    
    # NOUVELLE: Appliquer les labels de légende personnalisés
    if(!is.null(color_var)) {
      storage_key <- if(color_var == "Variable") {
        "multiY_legend"
      } else {
        color_var
      }
      
      if(!is.null(values$legendLabels[[storage_key]])) {
        legend_mapping <- values$legendLabels[[storage_key]]
        p <- p + scale_color_discrete(labels = legend_mapping) +
          scale_fill_discrete(labels = legend_mapping)
      }
    }
    
    # Personnalisation du thème (sauf pour seasonal_evolution qui a son propre style)
    if(viz_type != "seasonal_evolution") {
      # NOUVELLE: Créer les paramètres de formatage des axes
      x_axis_face <- if(isTRUE(input$xAxisBold) && isTRUE(input$xAxisItalic)) {
        "bold.italic"
      } else if(isTRUE(input$xAxisBold)) {
        "bold"
      } else if(isTRUE(input$xAxisItalic)) {
        "italic"
      } else {
        "plain"
      }
      
      y_axis_face <- if(isTRUE(input$yAxisBold) && isTRUE(input$yAxisItalic)) {
        "bold.italic"
      } else if(isTRUE(input$yAxisBold)) {
        "bold"
      } else if(isTRUE(input$yAxisItalic)) {
        "italic"
      } else {
        "plain"
      }
      
      x_tick_face <- if(isTRUE(input$xTickBold) && isTRUE(input$xTickItalic)) {
        "bold.italic"
      } else if(isTRUE(input$xTickBold)) {
        "bold"
      } else if(isTRUE(input$xTickItalic)) {
        "italic"
      } else {
        "plain"
      }
      
      # NOUVELLE: Obtenir l'angle des labels X
      x_angle <- input$xAxisAngle %||% 0
      x_hjust <- if(x_angle > 0) 1 else 0.5
      x_vjust <- if(x_angle > 0) 1 else 0.5
      
      p <- p + 
        theme_minimal(base_size = input$baseFontSize %||% 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = input$titleSize %||% 14),
          axis.title.x = element_text(face = x_axis_face, size = input$axisLabelSize %||% 11),
          axis.title.y = element_text(face = y_axis_face, size = input$axisLabelSize %||% 11),
          axis.text.x = element_text(face = x_tick_face, angle = x_angle, hjust = x_hjust, vjust = x_vjust),
          legend.position = input$legendPosition %||% "right"
        )
    } else {
      # Pour seasonal_evolution, appliquer TOUS les options de formatage
      x_axis_face <- if(isTRUE(input$xAxisBold) && isTRUE(input$xAxisItalic)) {
        "bold.italic"
      } else if(isTRUE(input$xAxisBold)) {
        "bold"
      } else if(isTRUE(input$xAxisItalic)) {
        "italic"
      } else {
        "plain"
      }
      
      y_axis_face <- if(isTRUE(input$yAxisBold) && isTRUE(input$yAxisItalic)) {
        "bold.italic"
      } else if(isTRUE(input$yAxisBold)) {
        "bold"
      } else if(isTRUE(input$yAxisItalic)) {
        "italic"
      } else {
        "plain"
      }
      
      x_tick_face <- if(isTRUE(input$xTickBold) && isTRUE(input$xTickItalic)) {
        "bold.italic"
      } else if(isTRUE(input$xTickBold)) {
        "bold"
      } else if(isTRUE(input$xTickItalic)) {
        "italic"
      } else {
        "plain"
      }
      
      x_angle <- input$xAxisAngle %||% 45
      x_hjust <- if(x_angle > 0) 1 else 0.5
      
      # Appliquer tous les formatages pour seasonal_evolution
      p <- p + theme(
        axis.text.x = element_text(angle = x_angle, hjust = x_hjust, face = x_tick_face, size = 8),
        axis.title.x = element_text(face = x_axis_face, size = input$axisLabelSize %||% 11),
        axis.title.y = element_text(face = y_axis_face, size = input$axisLabelSize %||% 11),
        plot.title = element_text(size = input$titleSize %||% 14, face = "bold", hjust = 0.5)
      )
    }
    
    # Ajouter les titres personnalisés
    if(!is.null(input$plotTitle) && input$plotTitle != "") {
      p <- p + ggtitle(input$plotTitle)
    }
    
    
    if(!is.null(input$xAxisLabel) && input$xAxisLabel != "") {
      p <- p + xlab(input$xAxisLabel)
    } else {
      p <- p + xlab(x_var)
    }
    
    if(!is.null(input$yAxisLabel) && input$yAxisLabel != "") {
      p <- p + ylab(input$yAxisLabel)
    } else {
      p <- p + ylab(y_var)
    }
    
    # NOUVELLE: Ajouter le titre de légende personnalisé
    if(!is.null(input$legendTitle) && input$legendTitle != "" && !is.null(color_var)) {
      p <- p + labs(color = input$legendTitle, fill = input$legendTitle)
    }
    
    return(p)
  })
  
  # Rendu du graphique interactif 
  output$interactivePlot <- renderPlotly({
    req(createPlot())
    
    p <- createPlot()
    
    # Convertir en plotly avec interactivité
    plotly_obj <- tryCatch({
      ggplotly(p, tooltip = "all") %>%
        layout(
          hovermode = "closest",
          dragmode = "zoom"
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d"),
          displaylogo = FALSE
        )
    }, error = function(e) {
      showNotification("Erreur lors de la conversion en graphique interactif", 
                       type = "error", duration = 3)
      return(NULL)
    })
    
    # Stocker le graphique actuel
    values$currentInteractivePlot <- plotly_obj
    
    return(plotly_obj)
  })
  
  # NOUVELLE: Rendre fonctionnel le bouton Personnaliser
  observeEvent(input$customizePlot, {
    showModal(modalDialog(
      title = tagList(icon("paint-brush"), " Personnalisation Rapide"),
      size = "l",
      
      fluidRow(
        column(6,
               h5("Titres", style = "color: #007bff; font-weight: bold;"),
               textInput("quickPlotTitle", "Titre:", value = input$plotTitle %||% "", placeholder = "Titre du graphique"),
               textInput("quickXLabel", "Label X:", value = input$xAxisLabel %||% "", placeholder = "Auto"),
               textInput("quickYLabel", "Label Y:", value = input$yAxisLabel %||% "", placeholder = "Auto")
        ),
        column(6,
               h5("Apparence", style = "color: #007bff; font-weight: bold;"),
               sliderInput("quickPointSize", "Taille des points:", min = 1, max = 10, value = input$pointSize %||% 3, step = 0.5),
               sliderInput("quickLineWidth", "Épaisseur des lignes:", min = 0.5, max = 5, value = input$lineWidth %||% 1, step = 0.5),
               selectInput("quickLegendPos", "Position légende:", 
                           choices = c("Droite" = "right", "Gauche" = "left", "Haut" = "top", "Bas" = "bottom", "Aucune" = "none"),
                           selected = input$legendPosition %||% "right")
        )
      ),
      
      footer = tagList(
        actionButton("applyQuickCustom", "Appliquer", class = "btn-primary"),
        modalButton("Fermer")
      )
    ))
  })
  
  # NOUVELLE: Appliquer la personnalisation rapide
  observeEvent(input$applyQuickCustom, {
    updateTextInput(session, "plotTitle", value = input$quickPlotTitle)
    updateTextInput(session, "xAxisLabel", value = input$quickXLabel)
    updateTextInput(session, "yAxisLabel", value = input$quickYLabel)
    updateSliderInput(session, "pointSize", value = input$quickPointSize)
    updateSliderInput(session, "lineWidth", value = input$quickLineWidth)
    updateSelectInput(session, "legendPosition", selected = input$quickLegendPos)
    
    removeModal()
    showNotification("Personnalisation appliquée", type = "message", duration = 2)
  })
  
  
  # SECTION 7: FONCTIONS DE CRÉATION
  
  
  # Fonction pour créer un scatter plot
  create_scatter_plot <- function(data, x_var, y_var, color_var = NULL) {
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_point(aes(color = .data[[color_var]]), 
                          size = input$pointSize %||% 3, 
                          alpha = input$pointAlpha %||% 0.7)
    } else {
      p <- p + geom_point(size = input$pointSize %||% 3, 
                          alpha = input$pointAlpha %||% 0.7)
    }
    
    # Ajouter ligne de tendance si demandé
    if(isTRUE(input$showTrendLine)) {
      p <- p + geom_smooth(method = input$trendMethod %||% "lm", 
                           se = isTRUE(input$showConfidenceInterval))
    }
    
    # NOUVELLE: Ajouter les valeurs si demandé
    if(isTRUE(input$showValues)) {
      p <- p + geom_text(aes(label = round(.data[[y_var]], 2)), 
                         vjust = -0.5, size = 3, check_overlap = TRUE)
    }
    
    return(p)
  }
  
  # Fonction pour créer un line plot
  create_line_plot <- function(data, x_var, y_var, color_var = NULL) {
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_line(aes(color = .data[[color_var]], group = .data[[color_var]]), 
                         size = input$lineWidth %||% 1)
      if(isTRUE(input$showPoints)) {
        p <- p + geom_point(aes(color = .data[[color_var]]), 
                            size = input$pointSize %||% 2)
      }
    } else {
      p <- p + geom_line(size = input$lineWidth %||% 1)
      if(isTRUE(input$showPoints)) {
        p <- p + geom_point(size = input$pointSize %||% 2)
      }
    }
    
    # NOUVELLE: Ajouter les valeurs si demandé
    if(isTRUE(input$showValues)) {
      p <- p + geom_text(aes(label = round(.data[[y_var]], 2)), 
                         vjust = -0.5, size = 3, check_overlap = TRUE)
    }
    
    return(p)
  }
  
  # Fonction pour créer un bar plot
  create_bar_plot <- function(data, x_var, y_var, color_var = NULL) {
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_col(aes(fill = .data[[color_var]]), 
                        position = input$barPosition %||% "dodge",
                        width = input$barWidth %||% 0.8)
    } else {
      p <- p + geom_col(width = input$barWidth %||% 0.8)
    }
    
    # NOUVELLE: Ajouter les valeurs si demandé
    if(isTRUE(input$showValues)) {
      p <- p + geom_text(aes(label = round(.data[[y_var]], 2)), 
                         vjust = -0.5, size = 3, 
                         position = position_dodge(width = input$barWidth %||% 0.8))
    }
    
    return(p)
  }
  
  # Fonction pour créer un box plot
  create_box_plot <- function(data, x_var, y_var, color_var = NULL) {
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_boxplot(aes(fill = .data[[color_var]]), alpha = 0.7)
    } else {
      p <- p + geom_boxplot(alpha = 0.7)
    }
    
    if(isTRUE(input$showOutliers)) {
      p <- p + geom_jitter(width = 0.2, alpha = 0.3, size = 1)
    }
    
    return(p)
  }
  
  # Fonction pour créer un violin plot
  create_violin_plot <- function(data, x_var, y_var, color_var = NULL) {
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_violin(aes(fill = .data[[color_var]]), alpha = 0.7)
    } else {
      p <- p + geom_violin(alpha = 0.7)
    }
    
    if(isTRUE(input$showBoxInsideViolin)) {
      p <- p + geom_boxplot(width = 0.1)
    }
    
    return(p)
  }
  
  # Fonction pour créer un seasonal smooth plot
  create_seasonal_smooth_plot <- function(data, x_var, y_var, color_var = NULL) {
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_line(aes(color = .data[[color_var]], group = .data[[color_var]]), alpha = 0.5)
      p <- p + geom_smooth(aes(color = .data[[color_var]], group = .data[[color_var]]),
                           method = input$smoothMethod %||% "loess",
                           span = input$smoothSpan %||% 0.75,
                           se = isTRUE(input$showConfidenceInterval))
    } else {
      p <- p + geom_line(alpha = 0.5)
      p <- p + geom_smooth(method = input$smoothMethod %||% "loess",
                           span = input$smoothSpan %||% 0.75,
                           se = isTRUE(input$showConfidenceInterval))
    }
    
    # NOUVELLE: Ajouter les valeurs si demandé (pour les points)
    if(isTRUE(input$showValues) && isTRUE(input$showPoints)) {
      p <- p + geom_text(aes(label = round(.data[[y_var]], 2)), 
                         vjust = -0.5, size = 2.5, check_overlap = TRUE)
    }
    
    return(p)
  }
  
  # Fonction pour créer un seasonal evolution plot (style graphique de référence)
  create_seasonal_evolution_plot <- function(data, x_var, y_var, color_var = NULL) {
    
    # Créer le graphique de base
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + 
        geom_line(aes(color = .data[[color_var]], group = .data[[color_var]]), 
                  linewidth = input$lineWidth %||% 1.2,
                  na.rm = TRUE) +
        geom_point(aes(color = .data[[color_var]]), 
                   size = input$pointSize %||% 3,
                   na.rm = TRUE)
    } else {
      p <- p + 
        geom_line(linewidth = input$lineWidth %||% 1.2,
                  na.rm = TRUE) +
        geom_point(size = input$pointSize %||% 3,
                   na.rm = TRUE)
    }
    
    # NOUVELLE: Ajouter les valeurs si demandé
    if(isTRUE(input$showValues)) {
      p <- p + geom_text(aes(label = round(.data[[y_var]], 2)), 
                         vjust = -0.5, size = 3, check_overlap = TRUE)
    }
    
    # Appliquer le thème clean (comme la figure de référence)
    p <- p +
      scale_x_discrete(drop = FALSE) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        plot.title = element_text(size = 14, color = "black", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 20),
        axis.text.x = element_text(angle = input$xAxisAngle %||% 45, hjust = 1, size = 8)
      )
    
    # Ajuster l'échelle Y pour commencer à 0 - CORRECTION pour éviter l'avis min()
    y_vals <- data[[y_var]][!is.na(data[[y_var]]) & is.finite(data[[y_var]])]
    if(length(y_vals) > 0) {
      y_max <- max(y_vals, na.rm = TRUE)
      if(!is.infinite(y_max) && !is.na(y_max)) {
        p <- p + scale_y_continuous(
          limits = c(0, y_max + y_max * 0.1),  # Ajouter 10% de marge
          breaks = pretty(c(0, y_max))
        )
      }
    }
    
    # Guide pour la légende
    if(!is.null(color_var)) {
      p <- p + guides(color = guide_legend(override.aes = list(size = 2), ncol = 2))
    }
    
    return(p)
  }
  
  # Fonction pour créer un histogram
  create_histogram_plot <- function(data, x_var) {
    p <- ggplot(data, aes(x = .data[[x_var]])) +
      geom_histogram(bins = input$histBins %||% 30, 
                     fill = input$histColor %||% "steelblue",
                     alpha = 0.7,
                     color = "white")
    return(p)
  }
  
  # Fonction pour créer un density plot
  create_density_plot <- function(data, x_var, color_var = NULL) {
    p <- ggplot(data, aes(x = .data[[x_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_density(aes(fill = .data[[color_var]], color = .data[[color_var]]), alpha = 0.5)
    } else {
      p <- p + geom_density(fill = "steelblue", alpha = 0.5)
    }
    
    return(p)
  }
  
  # Fonction pour créer un heatmap
  create_heatmap_plot <- function(data, x_var, y_var) {
    # Agrégation pour heatmap
    agg_data <- data %>%
      group_by(across(all_of(c(x_var, y_var)))) %>%
      summarise(count = n(), .groups = "drop")
    
    p <- ggplot(agg_data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # NOUVELLE: Ajouter les valeurs si demandé
    if(isTRUE(input$showValues)) {
      p <- p + geom_text(aes(label = count), color = "black", size = 3)
    }
    
    return(p)
  }
  
  # Fonction pour créer un area plot
  create_area_plot <- function(data, x_var, y_var, color_var = NULL) {
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_area(aes(fill = .data[[color_var]]), 
                         position = input$areaPosition %||% "stack",
                         alpha = 0.7)
    } else {
      p <- p + geom_area(fill = "steelblue", alpha = 0.7)
    }
    
    return(p)
  }
  
  # Fonction pour créer un pie chart
  create_pie_plot <- function(data, x_var, y_var) {
    # Agrégation
    pie_data <- data %>%
      group_by(across(all_of(x_var))) %>%
      summarise(total = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop") %>%
      mutate(percentage = total / sum(total) * 100)
    
    p <- ggplot(pie_data, aes(x = "", y = total, fill = .data[[x_var]])) +
      geom_col() +
      coord_polar(theta = "y") +
      theme_void()
    
    # NOUVELLE: Toujours afficher les valeurs/pourcentages pour les pie charts
    p <- p + geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                       position = position_stack(vjust = 0.5))
    
    return(p)
  }
  
  # Fonction pour créer un donut chart
  create_donut_plot <- function(data, x_var, y_var) {
    # Similaire au pie mais avec un trou au centre
    donut_data <- data %>%
      group_by(across(all_of(x_var))) %>%
      summarise(total = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop") %>%
      mutate(percentage = total / sum(total) * 100)
    
    p <- ggplot(donut_data, aes(x = 2, y = total, fill = .data[[x_var]])) +
      geom_col() +
      coord_polar(theta = "y") +
      xlim(c(0.5, 2.5)) +
      theme_void()
    
    # NOUVELLE: Toujours afficher les valeurs/pourcentages pour les donut charts
    p <- p + geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                       position = position_stack(vjust = 0.5))
    
    return(p)
  }
  
  # Fonction pour créer un treemap
  create_treemap_plot <- function(data, x_var, y_var) {
    # Note: Nécessite le package treemapify
    treemap_data <- data %>%
      group_by(across(all_of(x_var))) %>%
      summarise(total = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(treemap_data, aes(area = total, fill = .data[[x_var]], label = .data[[x_var]])) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(colour = "white", place = "centre")
    
    return(p)
  }
  
  
  # SECTION 8: INFORMATIONS ET STATS
  
  
  # Information sur l'agrégation 
  output$aggregationInfo <- renderText({
    req(input$useAggregation, input$aggFunction)
    
    if(!isTRUE(input$useAggregation)) return("")
    
    tryCatch({
      agg_names <- c(
        "mean" = "Moyenne", "median" = "Médiane", "sum" = "Somme",
        "count" = "Comptage", "min" = "Minimum", "max" = "Maximum", "sd" = "Écart-type"
      )
      
      info_text <- paste0("Fonction: ", agg_names[[input$aggFunction]] %||% "Inconnue", "\n")
      
      if(!is.null(input$groupVars) && length(input$groupVars) > 0) {
        info_text <- paste0(info_text, "Groupement par: ", 
                            paste(input$groupVars, collapse = ", "), "\n")
      }
      
      if(values$multipleY) {
        info_text <- paste0(info_text, "\n\nMode multi-Y: Agrégation par variable")
      }
      
      return(info_text)
    }, error = function(e) {
      "Erreur dans le calcul"
    })
  })
  
  # Information saisonnière améliorée 
  output$seasonalInfo <- renderText({
    req(input$vizType %in% c("seasonal_smooth", "seasonal_evolution"))
    
    tryCatch({
      x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
      
      info_text <- if(input$vizType == "seasonal_evolution") {
        "Type: Courbe d'évolution temporelle\n"
      } else {
        "Type: Courbe avec lissage\n"
      }
      
      info_text <- paste0(info_text, "Variable X: ", x_type, "\n")
      
      if(x_type %in% c("factor", "categorical", "text")) {
        info_text <- paste0(info_text, 
                            "Mode catégoriel activé\n",
                            "Ordre personnalisé: ",
                            if(!is.null(input$xLevelOrder)) "Oui" else "Par défaut")
      }
      
      if(values$multipleY) {
        info_text <- paste0(info_text, "\n\nComparaison de ", 
                            length(values$yVarNames), " séries:\n",
                            paste(values$yVarNames, collapse = "\n"))
      }
      
      if(isTRUE(input$useAggregation)) {
        info_text <- paste0(info_text, "\n\nAgrégation: Activée")
      }
      
      return(info_text)
    }, error = function(e) {
      "Erreur"
    })
  })
  
  # Statistiques des données 
  output$dataStatsSummary <- renderText({
    req(values$plotData)
    tryCatch({
      data <- values$plotData
      num_vars <- sum(sapply(data, is.numeric))
      cat_vars <- sum(sapply(data, function(x) is.factor(x) || is.character(x)))
      date_vars <- sum(sapply(data, function(x) inherits(x, "Date") || inherits(x, "POSIXt")))
      logical_vars <- sum(sapply(data, is.logical))
      missing_values <- sum(is.na(data))
      complete_rows <- sum(complete.cases(data))
      
      base_stats <- paste0("Nombre d'observations: ", nrow(data), "\n",
                           "Variables totales: ", ncol(data), "\n",
                           "Variables numériques: ", num_vars, "\n",
                           "Variables catégorielles: ", cat_vars, "\n",
                           "Variables temporelles: ", date_vars, "\n",
                           "Variables logiques: ", logical_vars, "\n",
                           "Valeurs manquantes: ", missing_values, "\n",
                           "Lignes complètes: ", complete_rows, " (", round(complete_rows/nrow(data)*100, 1), "%)")
      
      if(!is.null(values$multipleY) && values$multipleY) {
        base_stats <- paste0(base_stats, "\n\n",
                             "Mode: Variables Y multiples\n",
                             "Variables Y actives: ", length(values$yVarNames), "\n",
                             "Variables: ", paste(values$yVarNames, collapse = ", "))
      }
      
      return(base_stats)
    }, error = function(e) {
      showNotification("Erreur dans le calcul des statistiques", type = "error", duration = 5)
      "Erreur dans le calcul des statistiques"
    })
  })
  
  # Statistiques du graphique 
  output$plotStatsSummary <- renderText({
    req(values$currentInteractivePlot, input$vizXVar, input$vizYVar)
    tryCatch({
      viz_type_names <- c(
        "scatter" = "Nuage de points",
        "seasonal_smooth" = "Courbe saisonnière avec lissage",
        "seasonal_evolution" = "Courbe évolution saison",
        "box" = "Boxplot",
        "violin" = "Violon",
        "bar" = "Barres",
        "line" = "Lignes",
        "density" = "Densité",
        "histogram" = "Histogramme",
        "heatmap" = "Heatmap",
        "area" = "Aires empilées",
        "pie" = "Camembert",
        "donut" = "Donut",
        "treemap" = "Treemap"
      )
      
      viz_type_name <- viz_type_names[[input$vizType]] %||% "Type inconnu"
      
      y_display <- if(!is.null(values$multipleY) && values$multipleY) {
        paste0("Variables Y (", length(values$yVarNames), "): ", 
               paste(values$yVarNames, collapse = ", "))
      } else {
        paste0("Variable Y: ", input$vizYVar[1])
      }
      
      plot_info <- paste0("Type: ", viz_type_name, "\n",
                          "Variable X: ", input$vizXVar, "\n",
                          y_display)
      
      if(is.null(values$multipleY) || !values$multipleY) {
        if (!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
          plot_info <- paste0(plot_info, "\nVariable couleur: ", input$vizColorVar)
        }
      } else {
        plot_info <- paste0(plot_info, "\nCouleurs: Distinguent les variables Y")
      }
      
      if (!is.null(input$vizFacetVar) && input$vizFacetVar != "Aucun") {
        plot_info <- paste0(plot_info, "\nFacetting: ", input$vizFacetVar)
      }
      
      if (isTRUE(input$useAggregation) && !is.null(input$aggFunction)) {
        agg_names <- c(
          "mean" = "Moyenne", "median" = "Médiane", "sum" = "Somme",
          "count" = "Comptage", "min" = "Minimum", "max" = "Maximum", "sd" = "Écart-type"
        )
        agg_name <- agg_names[[input$aggFunction]] %||% "Inconnue"
        plot_info <- paste0(plot_info, "\nAgrégation: ", agg_name)
        if (!is.null(input$groupVars) && length(input$groupVars) > 0) {
          plot_info <- paste0(plot_info, " par ", paste(input$groupVars, collapse = ", "))
        }
      }
      
      plot_info <- paste0(plot_info, "\nObservations utilisées: ", nrow(values$plotData))
      return(plot_info)
    }, error = function(e) {
      showNotification("Erreur dans le calcul des statistiques du graphique", type = "error", duration = 5)
      "Erreur dans le calcul des statistiques du graphique"
    })
  })
  
  # Analyse saisonnière 
  output$seasonalAnalysisSummary <- renderText({
    req(input$vizType %in% c("seasonal_smooth", "seasonal_evolution"))
    tryCatch({
      summary_text <- "Analyse saisonnière activée\n"
      
      if (input$vizType == "seasonal_evolution") {
        summary_text <- paste0(summary_text, "Évolution temporelle directe des observations\n")
      } else {
        summary_text <- paste0(summary_text, "Analyse avec lissage des tendances\n")
      }
      
      if (isTRUE(input$showSmoothLine) && !is.null(input$smoothMethod)) {
        smooth_info <- switch(input$smoothMethod,
                              "loess" = paste0("Lissage LOESS (span: ", input$smoothSpan %||% 0.75, ")"),
                              "lm" = "Régression linéaire",
                              "gam" = "Modèle additif généralisé",
                              "Lissage activé")
        summary_text <- paste0(summary_text, smooth_info, "\n")
      }
      
      if (isTRUE(input$showConfidenceInterval) && isTRUE(input$showSmoothLine)) {
        summary_text <- paste0(summary_text, "Intervalle de confiance affiché\n")
      }
      
      if(!is.null(values$multipleY) && values$multipleY) {
        summary_text <- paste0(summary_text, "\nMode Y multiple: Comparaison de ", 
                               length(values$yVarNames), " séries temporelles")
      }
      
      return(summary_text)
    }, error = function(e) {
      showNotification("Erreur dans l'analyse saisonnière", type = "error", duration = 5)
      "Erreur dans l'analyse saisonnière"
    })
  })
  
  
  # SECTION 9: INDICATEURS RÉACTIFS
  
  
  # Indicateur multi-Y pour l'UI 
  output$multiYIndicator <- reactive({
    !is.null(values$multipleY) && values$multipleY
  })
  outputOptions(output, "multiYIndicator", suspendWhenHidden = FALSE)
  
  # Badge nombre de Y 
  observe({
    if(!is.null(values$multipleY) && values$multipleY) {
      runjs(paste0("
      $('#multiYBadge').text('", length(values$yVarNames), " variables');
    "))
    }
  })
  
  
  # SECTION AJOUTÉE: EXPORT AVANCÉ DES GRAPHIQUES
  
  
  # Stocker le graphique ggplot actuel pour l'export
  # Version améliorée pour garantir la disponibilité
  currentPlotReactive <- reactive({
    req(createPlot())
    createPlot()
  })
  
  # Observer pour mettre à jour values$currentPlot
  observe({
    plot <- currentPlotReactive()
    if(!is.null(plot)) {
      values$currentPlot <- plot
    }
  })
  
  # Interface pour l'export avancé
  # NOTE: La section output$exportControls a été déplacée directement dans UI.R
  # pour éviter le clignotement de l'interface lors des interactions.
  # Les contrôles d'export sont maintenant statiques dans le fichier UI.R.
  
  
  # Affichage des dimensions en pixels
  output$exportDimensionsInfo <- renderUI({
    req(input$exportDPI, input$exportWidth, input$exportHeight)
    
    width_px <- round(input$exportWidth * input$exportDPI)
    height_px <- round(input$exportHeight * input$exportDPI)
    
    div(
      style = "background-color: #e7f3ff; padding: 10px; border-radius: 4px; margin: 10px 0;",
      icon("info-circle", style = "color: #0066cc;"),
      span(style = "margin-left: 5px; font-weight: bold; color: #0066cc;",
           paste0("Dimensions finales: ", width_px, " × ", height_px, " pixels"))
    )
  })
  
  # Presets rapides
  observeEvent(input$presetScreen, {
    updateNumericInput(session, "exportDPI", value = 96)
    updateNumericInput(session, "exportWidth", value = 10)
    updateNumericInput(session, "exportHeight", value = 6)
    updateSelectInput(session, "exportFormat", selected = "png")
    showNotification("Preset 'Écran' appliqué (96 DPI)", type = "message", duration = 2)
  })
  
  observeEvent(input$presetPrint, {
    updateNumericInput(session, "exportDPI", value = 300)
    updateNumericInput(session, "exportWidth", value = 10)
    updateNumericInput(session, "exportHeight", value = 6)
    updateSelectInput(session, "exportFormat", selected = "png")
    showNotification("Preset 'Impression' appliqué (300 DPI)", type = "message", duration = 2)
  })
  
  observeEvent(input$presetHighQuality, {
    updateNumericInput(session, "exportDPI", value = 600)
    updateNumericInput(session, "exportWidth", value = 12)
    updateNumericInput(session, "exportHeight", value = 8)
    updateSelectInput(session, "exportFormat", selected = "tiff")
    showNotification("Preset 'Haute qualité' appliqué (600 DPI, TIFF)", type = "message", duration = 2)
  })
  
  # Fonction de téléchargement - VERSION CORRIGÉE ET FONCTIONNELLE
  output$downloadPlot <- downloadHandler(
    filename = function() {
      format_ext <- input$exportFormat
      if(is.null(format_ext)) format_ext <- "png"
      if(format_ext == "jpg") format_ext <- "jpeg"
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("graphique_", timestamp, ".", format_ext)
    },
    
    content = function(file) {
      # Récupérer le graphique directement
      plot_to_export <- NULL
      
      # Essayer plusieurs sources pour obtenir le graphique
      tryCatch({
        if(!is.null(values$currentPlot)) {
          plot_to_export <- values$currentPlot
        } else {
          plot_to_export <- createPlot()
        }
      }, error = function(e) {
        plot_to_export <- createPlot()
      })
      
      # Vérifier qu'on a bien un graphique
      if(is.null(plot_to_export)) {
        showNotification(
          "Erreur: Aucun graphique disponible pour l'export. Veuillez d'abord créer un graphique.",
          type = "error",
          duration = 5
        )
        return(NULL)
      }
      
      # Validation du DPI
      dpi <- input$exportDPI
      if(is.null(dpi)) dpi <- 300
      dpi <- min(max(dpi, 72), 20000)
      
      # Dimensions
      width <- input$exportWidth
      height <- input$exportHeight
      if(is.null(width)) width <- 10
      if(is.null(height)) height <- 6
      
      # Format
      format_type <- input$exportFormat
      if(is.null(format_type)) format_type <- "png"
      
      # Message de progression
      showNotification(
        "Génération du graphique en cours...",
        id = "export_progress",
        duration = NULL,
        type = "message"
      )
      
      tryCatch({
        # Export selon le format
        if(format_type %in% c("png", "jpg", "tiff", "bmp")) {
          # Formats raster
          if(format_type == "jpg") {
            quality <- input$jpegQuality
            if(is.null(quality)) quality <- 95
            jpeg(file, 
                 width = width, 
                 height = height, 
                 units = "in", 
                 res = dpi,
                 quality = quality)
          } else if(format_type == "tiff") {
            compression <- input$tiffCompression
            if(is.null(compression)) compression <- "lzw"
            tiff(file, 
                 width = width, 
                 height = height, 
                 units = "in", 
                 res = dpi,
                 compression = compression)
          } else if(format_type == "bmp") {
            bmp(file, 
                width = width, 
                height = height, 
                units = "in", 
                res = dpi)
          } else {
            # PNG par défaut
            png(file, 
                width = width, 
                height = height, 
                units = "in", 
                res = dpi,
                type = "cairo")
          }
          print(plot_to_export)
          dev.off()
          
        } else if(format_type == "pdf") {
          # Format PDF vectoriel
          pdf(file, 
              width = width, 
              height = height)
          print(plot_to_export)
          dev.off()
          
        } else if(format_type == "svg") {
          # Format SVG vectoriel
          svg(file, 
              width = width, 
              height = height)
          print(plot_to_export)
          dev.off()
          
        } else if(format_type == "eps") {
          # Format EPS vectoriel
          setEPS()
          postscript(file, 
                     width = width, 
                     height = height,
                     horizontal = FALSE,
                     onefile = FALSE,
                     paper = "special")
          print(plot_to_export)
          dev.off()
        }
        
        removeNotification("export_progress")
        
        # Vérifier que le fichier a bien été créé
        if(file.exists(file) && file.info(file)$size > 0) {
          showNotification(
            paste0("✓ Graphique exporté avec succès (", 
                   round(file.info(file)$size / 1024 / 1024, 2), " MB)"),
            type = "message",
            duration = 5
          )
        } else {
          showNotification(
            "Attention: Le fichier semble vide ou n'a pas été créé correctement",
            type = "warning",
            duration = 5
          )
        }
        
      }, error = function(e) {
        removeNotification("export_progress")
        showNotification(
          paste("❌ Erreur lors de l'export:", e$message),
          type = "error",
          duration = 10
        )
      })
    }
  )
  # ---- Tests statistiques ----
  output$responseVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    tagList(
      pickerInput("responseVar", "Variable(s) réponse:", 
                  choices = num_cols, 
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      actionButton("selectAllResponse", "Tout sélectionner", class = "btn-success btn-sm"),
      actionButton("deselectAllResponse", "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllResponse, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "responseVar", selected = num_cols)
  })
  
  observeEvent(input$deselectAllResponse, {
    updatePickerInput(session, "responseVar", selected = character(0))
  })
  
  output$factorVarSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    tagList(
      pickerInput("factorVar", "Facteur(s):", 
                  choices = fac_cols, 
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      actionButton("selectAllFactors", "Tout sélectionner", class = "btn-success btn-sm"),
      actionButton("deselectAllFactors", "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllFactors, {
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    updatePickerInput(session, "factorVar", selected = fac_cols)
  })
  
  observeEvent(input$deselectAllFactors, {
    updatePickerInput(session, "factorVar", selected = character(0))
  })
  
  # Tests de normalité et homogénéité sur données brutes
  observeEvent(input$testNormalityRaw, {
    req(input$responseVar)
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        data_values <- values$filteredData[[`var`]]
        data_values <- data_values[!is.na(data_values)]
        
        if (length(data_values) >= 3 && length(data_values) <= 5000) {
          norm_test <- shapiro.test(data_values)
          results_list[[`var`]] <- data.frame(
            Test = "Normalité (données brutes)",
            Variable = `var`,
            Facteur = "Global",
            Statistique = round(norm_test$statistic, 4),
            ddl = NA,
            p_value = norm_test$p.value,
            Interpretation = interpret_test_results("shapiro", norm_test$p.value),
            stringsAsFactors = FALSE
          )
        } else {
          results_list[[`var`]] <- data.frame(
            Test = "Normalité (données brutes)",
            Variable = `var`,
            Facteur = "Global",
            Statistique = NA,
            ddl = NA,
            p_value = NA,
            Interpretation = "Échantillon trop petit/grand pour Shapiro-Wilk",
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        results_list[[`var`]] <- data.frame(
          Test = "Normalité (données brutes)",
          Variable = `var`,
          Facteur = "Global",
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  observeEvent(input$testHomogeneityRaw, {
    req(input$responseVar, input$factorVar)
    
    if (length(input$factorVar) != 1) {
      showNotification("Le test d'homogénéité nécessite exactement un facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        formula_str <- as.formula(paste(`var`, "~", `fvar`))
        levene_test <- car::leveneTest(formula_str, data = values$filteredData)
        
        results_list[[`var`]] <- data.frame(
          Test = "Homogénéité (données brutes)",
          Variable = `var`,
          Facteur = `fvar`,
          Statistique = round(levene_test$`F value`[1], 4),
          ddl = paste(levene_test$Df[1], ",", levene_test$Df[2]),
          p_value = levene_test$`Pr(>F)`[1],
          Interpretation = interpret_test_results("levene", levene_test$`Pr(>F)`[1]),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[`var`]] <- data.frame(
          Test = "Homogénéité (données brutes)",
          Variable = `var`,
          Facteur = `fvar`,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  # Test t-student
  observeEvent(input$testT, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Le test t nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    normality_results <- list()
    homogeneity_results <- list()
    model_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        factor_levels <- levels(as.factor(values$filteredData[[`fvar`]]))
        
        if (length(factor_levels) != 2) {
          next
        }
        
        # Tests de validation
        group1_data <- values$filteredData[values$filteredData[[`fvar`]] == factor_levels[1], `var`]
        group2_data <- values$filteredData[values$filteredData[[`fvar`]] == factor_levels[2], `var`]
        
        group1_data <- group1_data[!is.na(group1_data)]
        group2_data <- group2_data[!is.na(group2_data)]
        
        # Test de normalité
        normality_group1 <- if(length(group1_data) >= 3 && length(group1_data) <= 5000) {
          shapiro.test(group1_data)
        } else {
          list(p.value = NA)
        }
        
        normality_group2 <- if(length(group2_data) >= 3 && length(group2_data) <= 5000) {
          shapiro.test(group2_data)
        } else {
          list(p.value = NA)
        }
        
        # Test d'homogénéité
        test_data <- data.frame(
          values = c(group1_data, group2_data),
          group = factor(c(rep(factor_levels[1], length(group1_data)), 
                           rep(factor_levels[2], length(group2_data))))
        )
        
        homogeneity_test <- car::leveneTest(values ~ group, data = test_data)
        
        # Stocker les résultats de validation
        normality_results[[`var`]] <- list(
          group1 = normality_group1,
          group2 = normality_group2,
          group1_name = factor_levels[1],
          group2_name = factor_levels[2]
        )
        
        homogeneity_results[[`var`]] <- homogeneity_test
        
        # Exécuter le t-test et créer un modèle factice pour les diagnostics
        formula_str <- as.formula(paste(`var`, "~", `fvar`))
        test_result <- t.test(formula_str, data = values$filteredData)
        
        # Créer un modèle lm pour les diagnostics
        lm_model <- lm(formula_str, data = values$filteredData)
        model_list[[`var`]] <- lm_model
        
        # Créer le dataframe de résultats
        results_list[[`var`]] <- data.frame(
          Test = "t-test",
          Variable = `var`,
          Facteur = `fvar`,
          Statistique = round(test_result$statistic, 4),
          ddl = round(test_result$parameter, 2),
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("t.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
        
      }, error = function(e) {
        results_list[[`var`]] <- data.frame(
          Test = "t-test",
          Variable = `var`,
          Facteur = `fvar`,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- normality_results
    values$homogeneityResults <- homogeneity_results
    values$currentValidationVar <- 1
    values$modelList <- model_list
    values$currentModelVar <- 1
    values$currentTestType <- "parametric"
  })
  
  # Test de Wilcoxon
  observeEvent(input$testWilcox, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Le test de Wilcoxon nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        formula_str <- as.formula(paste(`var`, "~", `fvar`))
        test_result <- wilcox.test(formula_str, data = values$filteredData, exact = FALSE)
        
        results_list[[`var`]] <- data.frame(
          Test = "Wilcoxon",
          Variable = `var`,
          Facteur = `fvar`,
          Statistique = round(test_result$statistic, 4),
          ddl = NA,
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("wilcox.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[`var`]] <- data.frame(
          Test = "Wilcoxon",
          Variable = `var`,
          Facteur = `fvar`,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  # Test de Kruskal-Wallis
  observeEvent(input$testKruskal, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Kruskal-Wallis nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        formula_str <- as.formula(paste(`var`, "~", `fvar`))
        test_result <- kruskal.test(formula_str, data = values$filteredData)
        
        results_list[[`var`]] <- data.frame(
          Test = "Kruskal-Wallis",
          Variable = `var`,
          Facteur = `fvar`,
          Statistique = round(test_result$statistic, 4),
          ddl = test_result$parameter,
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("kruskal.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[`var`]] <- data.frame(
          Test = "Kruskal-Wallis",
          Variable = `var`,
          Facteur = `fvar`,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  # Test Scheirer-Ray-Hare
  observeEvent(input$testScheirerRayHare, {
    req(input$responseVar, input$factorVar)
    
    if (length(input$factorVar) < 2) {
      showNotification("Scheirer-Ray-Hare nécessite au moins 2 facteurs", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        # Préparer la formule
        if (input$interaction && length(input$factorVar) == 2) {
          formula_str <- as.formula(paste(`var`, "~", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "*")))
        } else {
          formula_str <- as.formula(paste(`var`, "~", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+")))
        }
        
        # Exécuter le test Scheirer-Ray-Hare
        test_result <- rcompanion::scheirerRayHare(formula_str, data = values$filteredData)
        
        # Extraire les résultats pour chaque effet
        for (i in 1:nrow(test_result)) {
          effect_name <- rownames(test_result)[i]
          if (effect_name != "Residuals") {
            results_list[[paste(`var`, effect_name, sep = "_")]] <- data.frame(
              Test = "Scheirer-Ray-Hare",
              Variable = `var`,
              Facteur = effect_name,
              Statistique = round(test_result$H[i], 4),
              ddl = test_result$Df[i],
              p_value = test_result$`p.value`[i],
              Interpretation = interpret_test_results("scheirerRayHare", test_result$`p.value`[i]),
              stringsAsFactors = FALSE
            )
          }
        }
        
        # Stocker le résultat complet pour les post-hoc
        values$scheirerResults <- test_result
        
      }, error = function(e) {
        results_list[[`var`]] <- data.frame(
          Test = "Scheirer-Ray-Hare",
          Variable = `var`,
          Facteur = paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+"),
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  # Test ANOVA
  observeEvent(input$testANOVA, {
    req(input$responseVar, input$factorVar)
    
    results_list <- list()
    normality_results <- list()
    homogeneity_results <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (f in input$factorVar) {
        if (!is.factor(df[[`f`]])) df[[`f`]] <- factor(df[[`f`]])
      }
      
      for (var in input$responseVar) {
        formula_str <- paste(`var`, "~", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = ifelse(input$interaction, "*", "+")))
        model <- aov(as.formula(formula_str), data = df)
        anova_table <- summary(model)[[1]]
        
        # Stocker le modèle
        model_list[[`var`]] <- model
        
        # Créer le dataframe de résultats pour chaque effet
        for (i in 1:(nrow(anova_table) - 1)) {
          effect_name <- rownames(anova_table)[i]
          results_list[[paste(`var`, effect_name, sep = "_")]] <- data.frame(
            Test = "ANOVA",
            Variable = `var`,
            Facteur = effect_name,
            Statistique = round(anova_table$`F value`[i], 4),
            ddl = paste(anova_table$Df[i], ",", anova_table$Df[nrow(anova_table)]),
            p_value = anova_table$`Pr(>F)`[i],
            Interpretation = interpret_test_results("anova", anova_table$`Pr(>F)`[i]),
            stringsAsFactors = FALSE
          )
        }
        
        # Tests de validation des résidus
        residuals_data <- residuals(model)
        if (length(residuals_data) > 3) {
          normality_results[[`var`]] <- shapiro.test(residuals_data)
        }
        
        fitted_data <- fitted(model)
        fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
        test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
        homogeneity_results[[`var`]] <- car::leveneTest(residuals ~ fitted_group, data = test_data)
      }
      
      values$testResultsDF <- do.call(rbind, results_list)
      values$anovaModel <- model
      values$currentModel <- model
      values$modelList <- model_list
      values$currentModelVar <- 1
      values$normalityResults <- normality_results
      values$homogeneityResults <- homogeneity_results
      values$currentValidationVar <- 1
      values$currentTestType <- "parametric"
      
    }, error = function(e) {
      showNotification(paste("Erreur ANOVA :", e$message), type = "error")
    })
  })
  
  # Test de régression linéaire
  observeEvent(input$testLM, {
    req(input$responseVar, input$factorVar)
    
    results_list <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (var in input$responseVar) {
        formula_str <- paste(`var`, "~", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+"))
        model <- lm(as.formula(formula_str), data = df)
        summary_model <- summary(model)
        
        # Stocker le modèle
        model_list[[`var`]] <- model
        
        # Résultat global du modèle
        results_list[[paste(`var`, "global", sep = "_")]] <- data.frame(
          Test = "Régression linéaire",
          Variable = `var`,
          Facteur = "Modèle global",
          Statistique = round(summary_model$fstatistic[1], 4),
          ddl = paste(summary_model$fstatistic[2], ",", summary_model$fstatistic[3]),
          p_value = pf(summary_model$fstatistic[1], summary_model$fstatistic[2], 
                       summary_model$fstatistic[3], lower.tail = FALSE),
          Interpretation = paste("R² =", round(summary_model$r.squared, 4)),
          stringsAsFactors = FALSE
        )
        
        # Coefficients
        coef_table <- summary_model$coefficients
        for (i in 2:nrow(coef_table)) {
          results_list[[paste(`var`, rownames(coef_table)[i], sep = "_")]] <- data.frame(
            Test = "Régression linéaire",
            Variable = `var`,
            Facteur = rownames(coef_table)[i],
            Statistique = round(coef_table[i, "t value"], 4),
            ddl = summary_model$df[2],
            p_value = coef_table[i, "Pr(>|t|)"],
            Interpretation = interpret_test_results("lm", coef_table[i, "Pr(>|t|)"]),
            stringsAsFactors = FALSE
          )
        }
      }
      
      values$testResultsDF <- do.call(rbind, results_list)
      values$currentModel <- model
      values$modelList <- model_list
      values$currentModelVar <- 1
      values$currentTestType <- "parametric"
      
    }, error = function(e) {
      showNotification(paste("Erreur régression :", e$message), type = "error")
    })
  })
  
  # Test GLM
  observeEvent(input$testGLM, {
    req(input$responseVar, input$factorVar)
    
    results_list <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (var in input$responseVar) {
        formula_str <- paste(`var`, "~", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+"))
        model <- glm(as.formula(formula_str), data = df, family = gaussian())
        summary_model <- summary(model)
        
        # Stocker le modèle
        model_list[[`var`]] <- model
        
        # Coefficients
        coef_table <- summary_model$coefficients
        for (i in 2:nrow(coef_table)) {
          results_list[[paste(`var`, rownames(coef_table)[i], sep = "_")]] <- data.frame(
            Test = "GLM",
            Variable = `var`,
            Facteur = rownames(coef_table)[i],
            Statistique = round(coef_table[i, "z value"], 4),
            ddl = NA,
            p_value = coef_table[i, "Pr(>|z|)"],
            Interpretation = interpret_test_results("glm", coef_table[i, "Pr(>|z|)"]),
            stringsAsFactors = FALSE
          )
        }
      }
      
      values$testResultsDF <- do.call(rbind, results_list)
      values$currentModel <- model
      values$modelList <- model_list
      values$currentModelVar <- 1
      values$currentTestType <- "parametric"
      
    }, error = function(e) {
      showNotification(paste("Erreur GLM :", e$message), type = "error")
    })
  })
  
  # Affichage du dataframe des résultats
  output$testResultsDF <- renderDT({
    req(values$testResultsDF)
    datatable(values$testResultsDF, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Contrôle de l'affichage de la validation
  output$showValidation <- reactive({
    !is.null(values$normalityResults) || !is.null(values$homogeneityResults)
  })
  outputOptions(output, "showValidation", suspendWhenHidden = FALSE)
  
  # Contrôle de l'affichage des diagnostics (uniquement pour les tests paramétriques)
  output$showParametricDiagnostics <- reactive({
    !is.null(values$currentTestType) && values$currentTestType == "parametric" && !is.null(values$modelList)
  })
  outputOptions(output, "showParametricDiagnostics", suspendWhenHidden = FALSE)
  
  # Navigation pour la validation
  output$showValidationNavigation <- reactive({
    length(input$responseVar) > 1 && !is.null(values$normalityResults)
  })
  outputOptions(output, "showValidationNavigation", suspendWhenHidden = FALSE)
  
  output$validationNavigation <- renderUI({
    req(input$responseVar, length(input$responseVar) > 1)
    
    current_idx <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total_vars <- length(input$responseVar)
    
    div(style = "display: inline-block;",
        actionButton("prevValidationVar", "", icon = icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        span(paste("Variable", current_idx, "sur", total_vars, ":", input$responseVar[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        actionButton("nextValidationVar", "", icon = icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  # Navigation pour les diagnostics de modèles
  output$showModelNavigation <- reactive({
    !is.null(values$modelList) && length(values$modelList) > 1
  })
  outputOptions(output, "showModelNavigation", suspendWhenHidden = FALSE)
  
  output$modelDiagNavigation <- renderUI({
    req(values$modelList, length(values$modelList) > 1)
    
    current_idx <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total_vars <- length(values$modelList)
    var_names <- names(values$modelList)
    
    div(style = "display: inline-block; margin-bottom: 15px;",
        actionButton("prevModelVar", "", icon = icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        span(paste("Modèle", current_idx, "sur", total_vars, ":", var_names[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        actionButton("nextModelVar", "", icon = icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  # Navigation pour les résidus
  output$showResidNavigation <- reactive({
    !is.null(values$modelList) && length(values$modelList) > 1
  })
  outputOptions(output, "showResidNavigation", suspendWhenHidden = FALSE)
  
  output$residNavigation <- renderUI({
    req(values$modelList, length(values$modelList) > 1)
    
    current_idx <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total_vars <- length(values$modelList)
    var_names <- names(values$modelList)
    
    div(style = "display: inline-block; margin-bottom: 15px;",
        actionButton("prevResidVar", "", icon = icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        span(paste("Variable", current_idx, "sur", total_vars, ":", var_names[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        actionButton("nextResidVar", "", icon = icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  # Gestion des événements de navigation
  observeEvent(input$prevValidationVar, {
    current <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total <- length(input$responseVar)
    values$currentValidationVar <- if (current > 1) current - 1 else total
  })
  
  observeEvent(input$nextValidationVar, {
    current <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total <- length(input$responseVar)
    values$currentValidationVar <- if (current < total) current + 1 else 1
  })
  
  observeEvent(input$prevModelVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current > 1) current - 1 else total
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  observeEvent(input$nextModelVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current < total) current + 1 else 1
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  observeEvent(input$prevResidVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current > 1) current - 1 else total
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  observeEvent(input$nextResidVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current < total) current + 1 else 1
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  # Affichage des résultats de normalité
  output$normalityResults <- renderPrint({
    req(values$normalityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    norm <- values$normalityResults[[`current_var`]]
    
    if (is.null(norm)) {
      cat("Aucun résultat de normalité disponible pour cette variable.\n")
    } else if ("group1" %in% names(norm)) {
      cat("Groupe 1 (", norm$group1_name, "): p = ", norm$group1$p.value, "\n")
      cat("Groupe 2 (", norm$group2_name, "): p = ", norm$group2$p.value, "\n")
    } else {
      cat("Résidus : p = ", norm$p.value, "\n")
    }
  })
  
  output$normalityInterpretation <- renderUI({
    req(values$normalityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    norm <- values$normalityResults[[`current_var`]]
    
    if (is.null(norm)) {
      interp_text <- "Aucun résultat de normalité disponible pour cette variable."
    } else if ("group1" %in% names(norm)) {
      interp1 <- interpret_normality(norm$group1$p.value)
      interp2 <- interpret_normality(norm$group2$p.value)
      interp_text <- paste0("Groupe 1: ", interp1, "<br>Groupe 2: ", interp2)
    } else {
      interp_text <- interpret_normality(norm$p.value)
    }
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Affichage des résultats d'homogénéité
  output$homogeneityResults <- renderPrint({
    req(values$homogeneityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    hom <- values$homogeneityResults[[`current_var`]]
    
    if (is.null(hom)) {
      cat("Aucun résultat d'homogénéité disponible pour cette variable.\n")
    } else {
      cat("p = ", hom$`Pr(>F)`[1], "\n")
    }
  })
  
  output$homogeneityInterpretation <- renderUI({
    req(values$homogeneityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    hom <- values$homogeneityResults[[`current_var`]]
    
    if (is.null(hom)) {
      interp_text <- "Aucun résultat d'homogénéité disponible pour cette variable."
    } else {
      interp_text <- interpret_homogeneity(hom$`Pr(>F)`[1])
    }
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Diagnostics des modèles
  output$modelDiagnostics <- renderPlot({
    req(values$currentModel)
    par(mfrow = c(2, 2))
    plot(values$currentModel)
  })
  
  output$modelDiagnosticsInterpretation <- renderUI({
    req(values$currentModel)
    interp_text <- "Vérifiez les graphiques pour les violations des hypothèses (normalité, homoscédasticité, etc.)."
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Téléchargement des diagnostics de modèles
  output$downloadModelDiagnostics <- downloadHandler(
    filename = function() {
      paste0("diagnostics_modele_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 3200, height = 2400, res = 300, type = "cairo")
      par(mfrow = c(2, 2))
      plot(values$currentModel)
      dev.off()
    }
  )
  
  # Téléchargement du QQ-plot
  output$downloadQQPlot <- downloadHandler(
    filename = function() {
      paste0("qqplot_residus_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$currentModel)
      residuals_data <- residuals(values$currentModel)
      df <- data.frame(sample = residuals_data)
      
      p <- ggplot(df, aes(sample = sample)) +
        qqplotr::stat_qq_band(distribution = "norm", bandType = "pointwise", alpha = 0.2) +
        qqplotr::stat_qq_line(distribution = "norm") +
        qqplotr::stat_qq_point(distribution = "norm") +
        theme_minimal() +
        labs(title = "QQ-plot des résidus", 
             x = "Quantiles théoriques", 
             y = "Quantiles observés")
      
      ggsave(file, plot = p, width = 10, height = 8, dpi = 2000, type = "cairo-png")
    }
  )
  
  # QQ-plot des résidus
  output$qqPlotResiduals <- renderPlot({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    df <- data.frame(sample = residuals_data)
    
    ggplot(df, aes(sample = sample)) +
      qqplotr::stat_qq_band(distribution = "norm", bandType = "pointwise", alpha = 0.2) +
      qqplotr::stat_qq_line(distribution = "norm") +
      qqplotr::stat_qq_point(distribution = "norm") +
      theme_minimal() +
      labs(title = "QQ-plot des résidus", 
           x = "Quantiles théoriques", 
           y = "Quantiles observés")
  })
  
  output$qqPlotInterpretation <- renderUI({
    interp_text <- "Les points devraient suivre la ligne droite pour une normalité des résidus."
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Normalité des résidus
  output$normalityResult <- renderPrint({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    if (length(residuals_data) >= 3 && length(residuals_data) <= 5000) {
      shapiro.test(residuals_data)
    } else {
      cat("Nombre d'observations insuffisant pour le test de Shapiro-Wilk.\n")
    }
  })
  
  output$normalityResidInterpretation <- renderUI({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    if (length(residuals_data) >= 3 && length(residuals_data) <= 5000) {
      norm_test <- shapiro.test(residuals_data)
      interp_text <- interpret_normality_resid(norm_test$p.value)
    } else {
      interp_text <- "Nombre d'observations insuffisant pour le test de Shapiro-Wilk."
    }
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Homogénéité des résidus
  output$leveneResidResult <- renderPrint({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    fitted_data <- fitted(values$currentModel)
    fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
    test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
    car::leveneTest(residuals ~ fitted_group, data = test_data)
  })
  
  output$homogeneityResidInterpretation <- renderUI({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    fitted_data <- fitted(values$currentModel)
    fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
    test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
    hom_test <- car::leveneTest(residuals ~ fitted_group, data = test_data)
    interp_text <- interpret_homogeneity_resid(hom_test$`Pr(>F)`[1])
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Autocorrélation
  output$autocorrResult <- renderPrint({
    req(values$currentModel)
    lmtest::dwtest(values$currentModel)
  })
  
  output$autocorrInterpretation <- renderUI({
    req(values$currentModel)
    dw_test <- lmtest::dwtest(values$currentModel)
    interp_text <- if (dw_test$p.value > 0.05) {
      "Pas d'autocorrélation significative des résidus (p > 0.05)."
    } else {
      "Autocorrélation significative des résidus (p < 0.05). Vérifiez l'indépendance des observations."
    }
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Summary du modèle
  output$modelSummary <- renderPrint({
    req(values$currentModel)
    summary(values$currentModel)
  })
  
  # Télécharger les résultats des tests en Excel
  output$downloadTestsExcel <- downloadHandler(
    filename = function() {
      paste0("resultats_tests_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      
      # Feuille des résultats
      openxlsx::addWorksheet(wb, "Resultats")
      openxlsx::writeData(wb, "Resultats", values$testResultsDF)
      
      # Feuille de validation si disponible
      if (!is.null(values$normalityResults)) {
        validation_df <- data.frame(Variable = names(values$normalityResults))
        validation_df$Normality_p <- sapply(values$normalityResults, function(x) x$p.value %||% NA)
        validation_df$Homogeneity_p <- sapply(values$homogeneityResults, function(x) x$`Pr(>F)`[1] %||% NA)
        
        openxlsx::addWorksheet(wb, "Validation")
        openxlsx::writeData(wb, "Validation", validation_df)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  # ---- Comparaisons multiples PostHoc  ----
  
  # Fonction pour calculer le coefficient de variation
  calc_cv <- function(x) {
    if (length(x) <= 1 || sd(x, na.rm = TRUE) == 0) return(0)
    return((sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100)
  }
  
  # ---- Fonction AMÉLIORÉE pour post-hoc des effets simples 
  perform_simple_effect_posthoc <- function(df, var, factor1, factor2, level, test_type, test_method) {
    # Filtrer les données pour ce niveau spécifique
    df_subset <- df[df[[factor2]] == level, ]
    
    if (nrow(df_subset) < 3) return(NULL)
    
    tryCatch({
      groups <- NULL
      
      if (test_type == "param") {
        model <- aov(as.formula(paste(var, "~", factor1)), data = df_subset)
        
        if (test_method %in% c("lsd", "tukey", "duncan", "snk", "scheffe", "regw", "waller")) {
          mc_func <- switch(test_method,
                            "lsd" = agricolae::LSD.test,
                            "tukey" = agricolae::HSD.test,
                            "duncan" = agricolae::duncan.test,
                            "snk" = agricolae::SNK.test,
                            "scheffe" = agricolae::scheffe.test,
                            "regw" = agricolae::REGW.test,
                            "waller" = agricolae::waller.test)
          mc <- mc_func(model, factor1, group = TRUE)
          groups <- mc$groups
          colnames(groups)[1:2] <- c("means", "groups")
          groups[[factor1]] <- rownames(groups)
        } else if (test_method == "bonferroni") {
          emm <- emmeans::emmeans(model, as.formula(paste("~", factor1)))
          mc <- pairs(emm, adjust = "bonferroni")
          pmat <- as.matrix(summary(mc)$p.value)
          if (is.null(dim(pmat))) {
            groups <- data.frame(groups = rep("a", length(unique(df_subset[[factor1]]))))
            groups[[factor1]] <- unique(df_subset[[factor1]])
          } else {
            pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
            diag(pmat) <- 1
            groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
            groups <- data.frame(groups = groups_letters)
            groups[[factor1]] <- names(groups_letters)
          }
        } else if (test_method == "dunnett") {
          emm <- emmeans::emmeans(model, as.formula(paste("~", factor1)))
          groups_cld <- multcomp::cld(emm, Letters = letters)
          groups <- as.data.frame(groups_cld)
          groups <- groups[, c(factor1, ".group")]
          colnames(groups) <- c(factor1, "groups")
          groups$groups <- trimws(groups$groups)
        } else if (test_method == "games") {
          mc <- PMCMRplus::gamesHowellTest(as.formula(paste(var, "~", factor1)), data = df_subset)
          pmat <- as.matrix(mc$p.value)
          pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
          diag(pmat) <- 1
          groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
          groups <- data.frame(groups = groups_letters)
          groups[[factor1]] <- names(groups_letters)
        }
      } else {  # NON-PARAMÉTRIQUE - LOGIQUE AMÉLIORÉE
        if (test_method == "kruskal") {
          mc <- agricolae::kruskal(df_subset[[var]], df_subset[[factor1]], group = TRUE)
          groups <- mc$groups
          colnames(groups)[1:2] <- c("means", "groups")
          groups[[factor1]] <- rownames(groups)
        } else if (test_method == "dunn") {
          mc <- PMCMRplus::dunnTest(df_subset[[var]], df_subset[[factor1]])
          pmat <- as.matrix(mc$p.value)
          pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
          diag(pmat) <- 1
          groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
          groups <- data.frame(groups = groups_letters)
          groups[[factor1]] <- names(groups_letters)
        } else if (test_method == "conover") {
          mc <- PMCMRplus::kwAllPairsConoverTest(df_subset[[var]], df_subset[[factor1]])
          pmat <- as.matrix(mc$p.value)
          pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
          diag(pmat) <- 1
          groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
          groups <- data.frame(groups = groups_letters)
          groups[[factor1]] <- names(groups_letters)
        } else if (test_method == "nemenyi") {
          mc <- PMCMRplus::kwAllPairsNemenyiTest(df_subset[[var]], df_subset[[factor1]])
          pmat <- as.matrix(mc$p.value)
          pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
          diag(pmat) <- 1
          groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
          groups <- data.frame(groups = groups_letters)
          groups[[factor1]] <- names(groups_letters)
        }
      }
      
      if (is.null(groups)) return(NULL)
      
      # Statistiques descriptives
      desc <- df_subset %>%
        group_by(across(all_of(factor1))) %>%
        summarise(
          Moyenne = mean(.data[[var]], na.rm = TRUE),
          Ecart_type = sd(.data[[var]], na.rm = TRUE),
          N = n(),
          Erreur_type = Ecart_type / sqrt(N),
          CV = calc_cv(.data[[var]]),
          .groups = "drop"
        )
      
      res <- merge(desc, groups, by = factor1, all.x = TRUE)
      res[[factor2]] <- level
      res$groups[is.na(res$groups)] <- "a"
      
      return(res)
      
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # ---- Fonction principale d'analyse 
  observeEvent(input$runMultiple, {
    req(input$multiResponse, input$multiFactor)
    
    showNotification("Analyse en cours...", type = "message", duration = NULL, id = "loading")
    
    multi_results_list <- list()
    simple_effects_list <- list()
    df <- values$filteredData
    
    for (var in input$multiResponse) {
      # -------- EFFETS PRINCIPAUX 
      for (fvar in input$multiFactor) {
        tryCatch({
          if (input$testType == "param") {
            model <- aov(as.formula(paste(var, "~", fvar)), data = df)
            
            if (input$multiTest %in% c("lsd", "tukey", "duncan", "snk", "scheffe", "regw", "waller")) {
              mc_func <- switch(input$multiTest,
                                "lsd" = agricolae::LSD.test,
                                "tukey" = agricolae::HSD.test,
                                "duncan" = agricolae::duncan.test,
                                "snk" = agricolae::SNK.test,
                                "scheffe" = agricolae::scheffe.test,
                                "regw" = agricolae::REGW.test,
                                "waller" = agricolae::waller.test)
              mc <- mc_func(model, fvar, group = TRUE)
              groups <- mc$groups
              colnames(groups) <- c("means", "groups")
              groups[[fvar]] <- rownames(groups)
            } else if (input$multiTest == "bonferroni") {
              emm <- emmeans::emmeans(model, as.formula(paste("~", fvar)))
              mc <- pairs(emm, adjust = "bonferroni")
              pmat <- as.matrix(summary(mc)$p.value)
              if (is.null(dim(pmat))) {
                groups <- data.frame(groups = rep("a", length(levels(df[[fvar]]))))
                groups[[fvar]] <- levels(df[[fvar]])
              } else {
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }
            } else if (input$multiTest == "dunnett") {
              emm <- emmeans::emmeans(model, as.formula(paste("~", fvar)))
              groups_cld <- multcomp::cld(emm, Letters = letters)
              groups <- as.data.frame(groups_cld)
              groups <- groups[, c(fvar, ".group")]
              colnames(groups) <- c(fvar, "groups")
              groups$groups <- trimws(groups$groups)
            } else if (input$multiTest == "games") {
              mc <- PMCMRplus::gamesHowellTest(as.formula(paste(var, "~", fvar)), data = df)
              pmat <- as.matrix(mc$p.value)
              pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
              diag(pmat) <- 1
              groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
              groups <- data.frame(groups = groups_letters)
              groups[[fvar]] <- names(groups_letters)
            }
          } else {  # NON-PARAMÉTRIQUE
            if (input$multiTestNonParam == "kruskal") {
              mc <- agricolae::kruskal(df[[var]], df[[fvar]], group = TRUE)
              groups <- mc$groups
              colnames(groups) <- c("means", "groups")
              groups[[fvar]] <- rownames(groups)
            } else if (input$multiTestNonParam == "dunn") {
              tryCatch({
                mc <- PMCMRplus::dunnTest(df[[var]], df[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }, error = function(e) {
                mc <- agricolae::kruskal(df[[var]], df[[fvar]], group = TRUE)
                groups <- mc$groups
                colnames(groups) <- c("means", "groups")
                groups[[fvar]] <- rownames(groups)
              })
            } else if (input$multiTestNonParam == "conover") {
              tryCatch({
                mc <- PMCMRplus::kwAllPairsConoverTest(df[[var]], df[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }, error = function(e) {
                mc <- agricolae::kruskal(df[[var]], df[[fvar]], group = TRUE)
                groups <- mc$groups
                colnames(groups) <- c("means", "groups")
                groups[[fvar]] <- rownames(groups)
              })
            } else if (input$multiTestNonParam == "nemenyi") {
              tryCatch({
                mc <- PMCMRplus::kwAllPairsNemenyiTest(df[[var]], df[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }, error = function(e) {
                mc <- agricolae::kruskal(df[[var]], df[[fvar]], group = TRUE)
                groups <- mc$groups
                colnames(groups) <- c("means", "groups")
                groups[[fvar]] <- rownames(groups)
              })
            }
          }
          
          # Statistiques descriptives
          desc <- df %>%
            group_by(across(all_of(fvar))) %>%
            summarise(
              Moyenne = mean(.data[[var]], na.rm = TRUE),
              Ecart_type = sd(.data[[var]], na.rm = TRUE),
              N = n(),
              Erreur_type = Ecart_type / sqrt(N),
              CV = calc_cv(.data[[var]]),
              .groups = "drop"
            )
          
          if (fvar %in% colnames(groups) && fvar %in% colnames(desc)) {
            res <- merge(desc, groups, by = fvar, all.x = TRUE)
            res <- res %>%
              mutate(
                Moyenne = round(Moyenne, 2),
                Ecart_type = round(Ecart_type, 2),
                Erreur_type = round(Erreur_type, 2),
                CV = round(CV, 2),
                `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                Variable = var,
                Facteur = fvar,
                Type = "main"
              )
            
            multi_results_list[[paste(var, fvar, "main", sep = "_")]] <- res
          }
        }, error = function(e) {
          showNotification(paste("Erreur effet principal:", var, fvar, "-", e$message), type = "error")
        })
      }
      
      # -------- ANALYSE DES INTERACTIONS ET EFFETS SIMPLES BIDIRECTIONNELS 
      if (input$posthocInteraction && length(input$multiFactor) > 1) {
        factor_combinations <- combn(input$multiFactor, 2, simplify = FALSE)
        
        for (fcomb in factor_combinations) {
          fvar1 <- fcomb[1]
          fvar2 <- fcomb[2]
          interaction_term <- paste(fvar1, fvar2, sep = ":")
          formula_str <- paste(var, "~", fvar1, "*", fvar2)
          
          tryCatch({
            df_temp <- df
            interaction_pvalue <- NA
            
            # ---- TEST D'INTERACTION 
            if (input$testType == "param") {
              # Test paramétrique classique
              model <- aov(as.formula(formula_str), data = df_temp)
              anova_res <- summary(model)[[1]]
              interaction_row <- paste(fvar1, fvar2, sep = ":")
              
              if (interaction_row %in% rownames(anova_res)) {
                interaction_pvalue <- anova_res[interaction_row, "Pr(>F)"]
              }
            } else {  
              # ---- TEST NON-PARAMÉTRIQUE AMÉLIORÉ 
              # Création de la variable combinée pour l'interaction
              df_temp$interaction_combined <- interaction(df_temp[[fvar1]], df_temp[[fvar2]], 
                                                          drop = TRUE, sep = ":")
              
              # Test de Kruskal-Wallis sur l'interaction combinée
              kw_interaction <- kruskal.test(df_temp[[var]] ~ df_temp$interaction_combined)
              interaction_pvalue <- kw_interaction$p.value
              
              showNotification(
                paste0("Test non-paramétrique (Kruskal-Wallis) pour ", fvar1, " × ", fvar2, 
                       ": p = ", round(interaction_pvalue, 4)),
                type = "message", duration = 3
              )
            }
            
            # ---- SI INTERACTION SIGNIFICATIVE : DÉCOMPOSITION BIDIRECTIONNELLE 
            if (!is.na(interaction_pvalue) && interaction_pvalue < 0.05) {
              showNotification(
                paste0(" Interaction significative détectée: ", fvar1, " × ", fvar2, 
                       " (p = ", round(interaction_pvalue, 4), ")\n",
                       " Décomposition bidirectionnelle en cours..."),
                type = "warning", duration = 5
              )
              
              # ---- DIRECTION 1: Comparer fvar1 à chaque niveau de fvar2 
              for (level2 in unique(df[[fvar2]])) {
                res <- perform_simple_effect_posthoc(
                  df, var, fvar1, fvar2, level2,
                  input$testType, 
                  ifelse(input$testType == "param", input$multiTest, input$multiTestNonParam)
                )
                
                if (!is.null(res)) {
                  res <- res %>%
                    mutate(
                      Moyenne = round(Moyenne, 2),
                      Ecart_type = round(Ecart_type, 2),
                      Erreur_type = round(Erreur_type, 2),
                      CV = round(CV, 2),
                      `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                      `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                      Variable = var,
                      Facteur = paste0(fvar1, " | ", fvar2, "=", level2),
                      Type = "simple_effect",
                      Interaction_base = interaction_term,
                      P_interaction = round(interaction_pvalue, 4),
                      Direction = paste0(fvar1, " vers ", fvar2)
                    )
                  
                  simple_effects_list[[paste(var, fvar1, fvar2, level2, sep = "_")]] <- res
                }
              }
              
              # ---- DIRECTION 2: Comparer fvar2 à chaque niveau de fvar1 
              for (level1 in unique(df[[fvar1]])) {
                res <- perform_simple_effect_posthoc(
                  df, var, fvar2, fvar1, level1,
                  input$testType,
                  ifelse(input$testType == "param", input$multiTest, input$multiTestNonParam)
                )
                
                if (!is.null(res)) {
                  res <- res %>%
                    mutate(
                      Moyenne = round(Moyenne, 2),
                      Ecart_type = round(Ecart_type, 2),
                      Erreur_type = round(Erreur_type, 2),
                      CV = round(CV, 2),
                      `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                      `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                      Variable = var,
                      Facteur = paste0(fvar2, " | ", fvar1, "=", level1),
                      Type = "simple_effect",
                      Interaction_base = interaction_term,
                      P_interaction = round(interaction_pvalue, 4),
                      Direction = paste0(fvar2, " vers ", fvar1)
                    )
                  
                  simple_effects_list[[paste(var, fvar2, fvar1, level1, sep = "_")]] <- res
                }
              }
              
              showNotification(
                paste0(" Décomposition complétée pour ", fvar1, " × ", fvar2),
                type = "message", duration = 3
              )
            } else if (!is.na(interaction_pvalue)) {
              showNotification(
                paste0(" Interaction non significative: ", fvar1, " × ", fvar2, 
                       " (p = ", round(interaction_pvalue, 4), ")"),
                type = "default", duration = 3
              )
            }
          }, error = function(e) {
            showNotification(paste("Erreur interaction:", interaction_term, "-", e$message), type = "error")
          })
        }
      }
    }
    
    removeNotification("loading")
    
    # ---- ASSEMBLAGE FINAL DES RÉSULTATS 
    all_results <- c(multi_results_list, simple_effects_list)
    
    if (length(all_results) > 0) {
      all_cols <- unique(unlist(lapply(all_results, colnames)))
      all_results <- lapply(all_results, function(df) {
        missing_cols <- setdiff(all_cols, colnames(df))
        for (col in missing_cols) {
          df[[col]] <- NA
        }
        return(df[, all_cols])
      })
      
      combined_results <- do.call(rbind, all_results)
      values$allPostHocResults[[length(values$allPostHocResults) + 1]] <- combined_results
      values$multiResultsMain <- combined_results
      values$currentVarIndex <- 1
      
      # Message récapitulatif amélioré
      n_main <- sum(combined_results$Type == "main", na.rm = TRUE)
      n_simple <- sum(combined_results$Type == "simple_effect", na.rm = TRUE)
      n_interactions <- length(unique(combined_results$Interaction_base[!is.na(combined_results$Interaction_base)]))
      
      showNotification(
        HTML(paste0(
          "<b> ANALYSE TERMINÉE</b><br/>",
          "• ", n_main, " effet(s) principal(aux)<br/>",
          "• ", n_simple, " effet(s) simple(s)<br/>",
          "• ", n_interactions, " interaction(s) décomposée(s)"
        )),
        type = "message", duration = 8
      )
    } else {
      showNotification("Aucun résultat généré", type = "warning")
    }
  })
  
  # ---- UI Outputs pour Multi-Selection 
  output$multiResponseSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    tagList(
      pickerInput("multiResponse", "Variable(s) réponse:", choices = num_cols, multiple = TRUE, 
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      div(style = "display: flex; gap: 10px;",
          actionButton("selectAllMultiResponse", "Tout sélectionner", 
                       class = "btn-success btn-sm", 
                       style = "flex: 1; height: 40px;"),
          actionButton("deselectAllMultiResponse", "Tout désélectionner", 
                       class = "btn-danger btn-sm",
                       style = "flex: 1; height: 40px;")
      )
    )
  })
  
  observeEvent(input$selectAllMultiResponse, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "multiResponse", selected = num_cols)
  })
  
  observeEvent(input$deselectAllMultiResponse, {
    updatePickerInput(session, "multiResponse", selected = character(0))
  })
  
  output$multiFactorSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    tagList(
      pickerInput("multiFactor", "Facteur(s):", choices = fac_cols, multiple = TRUE, 
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      div(style = "display: flex; gap: 10px;",
          actionButton("selectAllMultiFactors", "Tout sélectionner", 
                       class = "btn-success btn-sm",
                       style = "flex: 1; height: 40px;"),
          actionButton("deselectAllMultiFactors", "Tout désélectionner", 
                       class = "btn-danger btn-sm",
                       style = "flex: 1; height: 40px;")
      )
    )
  })
  
  observeEvent(input$selectAllMultiFactors, {
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    updatePickerInput(session, "multiFactor", selected = fac_cols)
  })
  
  observeEvent(input$deselectAllMultiFactors, {
    updatePickerInput(session, "multiFactor", selected = character(0))
  })
  
  # ---- Tables de résultats 
  output$mainEffectsTable <- renderDT({
    req(values$multiResultsMain)
    
    main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
    
    if (nrow(main_data) == 0) return(NULL)
    
    cols_to_show <- c("Variable", "Facteur", "Moyenne", "Ecart_type", "Erreur_type", "CV", "groups", "N")
    
    for (fvar in input$multiFactor) {
      if (fvar %in% colnames(main_data)) {
        cols_to_show <- c(cols_to_show, fvar)
      }
    }
    
    cols_to_show <- unique(cols_to_show)
    cols_to_show <- cols_to_show[cols_to_show %in% colnames(main_data)]
    
    datatable(
      main_data[, cols_to_show, drop = FALSE],
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      extensions = 'Buttons',
      class = 'cell-border stripe'
    ) %>%
      formatRound(columns = c("Moyenne", "Ecart_type", "Erreur_type", "CV"), digits = 2) %>%
      formatStyle(
        'groups',
        backgroundColor = styleEqual(
          unique(main_data$groups),
          rainbow(length(unique(main_data$groups)), alpha = 0.3)
        ),
        fontWeight = 'bold'
      )
  })
  
  output$simpleEffectsTable <- renderDT({
    req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    if (!is.null(input$filterSimpleEffectVar) && input$filterSimpleEffectVar != "Toutes") {
      simple_data <- simple_data[simple_data$Variable == input$filterSimpleEffectVar, ]
    }
    
    if (!is.null(input$filterSimpleEffectInteraction) && input$filterSimpleEffectInteraction != "Toutes") {
      simple_data <- simple_data[simple_data$Interaction_base == input$filterSimpleEffectInteraction, ]
    }
    
    cols_to_show <- c("Variable", "Facteur", "Direction", "Interaction_base", "P_interaction", 
                      "Moyenne", "Ecart_type", "Erreur_type", "CV", "groups", "N")
    
    for (fvar in input$multiFactor) {
      if (fvar %in% colnames(simple_data)) {
        cols_to_show <- c(cols_to_show, fvar)
      }
    }
    
    cols_to_show <- unique(cols_to_show)
    cols_to_show <- cols_to_show[cols_to_show %in% colnames(simple_data)]
    
    datatable(
      simple_data[, cols_to_show, drop = FALSE],
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      extensions = 'Buttons',
      class = 'cell-border stripe'
    ) %>%
      formatRound(columns = c("Moyenne", "Ecart_type", "Erreur_type", "CV", "P_interaction"), digits = 4) %>%
      formatStyle(
        'groups',
        backgroundColor = styleEqual(
          unique(simple_data$groups),
          rainbow(length(unique(simple_data$groups)), alpha = 0.3)
        ),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'P_interaction',
        backgroundColor = styleInterval(c(0.01, 0.05), c('#e74c3c', '#f39c12', '#95a5a6')),
        color = 'white',
        fontWeight = 'bold'
      )
  })
  
  # ---- Indicateurs 
  output$showPosthocResults <- reactive({
    !is.null(values$multiResultsMain) && nrow(values$multiResultsMain) > 0
  })
  outputOptions(output, "showPosthocResults", suspendWhenHidden = FALSE)
  
  output$showSimpleEffects <- reactive({
    !is.null(values$multiResultsMain) && 
      any(values$multiResultsMain$Type == "simple_effect", na.rm = TRUE)
  })
  outputOptions(output, "showSimpleEffects", suspendWhenHidden = FALSE)
  
  # ---- Résumés 
  output$analysisSummaryMain <- renderUI({
    req(values$multiResultsMain)
    
    main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
    
    n_vars <- length(unique(main_data$Variable))
    n_factors <- length(unique(main_data$Facteur))
    n_comparisons <- nrow(main_data)
    
    div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px; border-radius: 8px;",
        h5(icon("layer-group"), " Effets principaux", style = "margin-top: 0;"),
        tags$ul(
          tags$li(strong(n_vars), " variable(s) analysée(s)"),
          tags$li(strong(n_factors), " facteur(s) testé(s)"),
          tags$li(strong(n_comparisons), " comparaison(s)")
        )
    )
  })
  
  output$simpleEffectsSummary <- renderUI({
    req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    n_interactions <- length(unique(simple_data$Interaction_base))
    n_tests <- nrow(simple_data)
    n_directions <- length(unique(simple_data$Direction[!is.na(simple_data$Direction)]))
    
    div(style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
        h5(icon("project-diagram"), " Décomposition des interactions", style = "margin-top: 0;"),
        tags$ul(
          tags$li(strong(n_interactions), " interaction(s) significative(s)"),
          tags$li(strong(n_tests), " test(s) d'effets simples"),
          tags$li(strong(n_directions), " direction(s) d'analyse")
        )
    )
  })
  
  # ---- Filtres dynamiques 
  observe({
    req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) > 0) {
      vars <- c("Toutes", unique(simple_data$Variable))
      interactions <- c("Toutes", unique(simple_data$Interaction_base))
      
      updateSelectInput(session, "filterSimpleEffectVar", choices = vars, selected = "Toutes")
      updateSelectInput(session, "filterSimpleEffectInteraction", choices = interactions, selected = "Toutes")
    }
  })
  
  # ---- Sélection pour graphiques effets simples 
  output$selectSimpleEffectPlot <- renderUI({
    req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    current_var_idx <- values$currentVarIndex %||% 1
    resp_var <- input$multiResponse[current_var_idx]
    
    simple_var_data <- simple_data[simple_data$Variable == resp_var, ]
    
    if (nrow(simple_var_data) == 0) {
      return(div(style = "color: #e74c3c; font-style: italic;", 
                 "Aucun effet simple pour cette variable"))
    }
    
    factors <- unique(simple_var_data$Facteur)
    
    selectInput("selectedSimpleEffect", 
                "Sélectionner l'effet simple:",
                choices = factors,
                width = "100%")
  })
  
  # ---- Rapport complet 
  output$fullAnalysisReport <- renderUI({
    req(values$multiResultsMain)
    
    main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    tagList(
      h4("Vue d'ensemble"),
      fluidRow(
        column(6,
               div(style = "background-color: white; padding: 15px; border-radius: 5px; border-left: 4px solid #667eea;",
                   h5(icon("layer-group"), " Effets principaux"),
                   p(strong(nrow(main_data)), " comparaisons"),
                   p(strong(length(unique(main_data$Variable))), " variables"),
                   p(strong(length(unique(main_data$Facteur))), " facteurs")
               )
        ),
        column(6,
               div(style = "background-color: white; padding: 15px; border-radius: 5px; border-left: 4px solid #e74c3c;",
                   h5(icon("project-diagram"), " Effets simples"),
                   p(strong(nrow(simple_data)), " tests"),
                   p(strong(length(unique(simple_data$Interaction_base))), " interactions décomposées"),
                   p(strong(length(unique(simple_data$Direction[!is.na(simple_data$Direction)]))), " directions d'analyse")
               )
        )
      ),
      br(),
      h4("Détails par variable"),
      uiOutput("variableDetailedReport")
    )
  })
  
  output$variableDetailedReport <- renderUI({
    req(values$multiResultsMain)
    
    vars <- unique(values$multiResultsMain$Variable)
    
    reports <- lapply(vars, function(v) {
      var_data <- values$multiResultsMain[values$multiResultsMain$Variable == v, ]
      main_count <- sum(var_data$Type == "main")
      simple_count <- sum(var_data$Type == "simple_effect")
      
      div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          strong(v),
          br(),
          sprintf("• %d effet(s) principal(aux)", main_count),
          br(),
          sprintf("• %d effet(s) simple(s)", simple_count)
      )
    })
    
    do.call(tagList, reports)
  })
  
  # ---- Navigation entre variables 
  output$showVariableNavigation <- reactive({
    length(input$multiResponse) > 1
  })
  outputOptions(output, "showVariableNavigation", suspendWhenHidden = FALSE)
  
  output$variableNavigation <- renderUI({
    req(length(input$multiResponse) > 1)
    current_idx <- values$currentVarIndex %||% 1
    total_vars <- length(input$multiResponse)
    
    div(style = "display: flex; align-items: center; gap: 15px;",
        actionButton("prevMultiVar", "", 
                     icon = icon("chevron-left"), 
                     class = "btn-light btn-lg",
                     style = "font-size: 1.5em; padding: 10px 20px; height: 60px; width: 60px;"),
        div(style = "color: white; font-size: 1.2em; font-weight: bold; text-align: center;",
            span(style = "display: block; font-size: 0.8em; opacity: 0.8;", 
                 paste("Variable", current_idx, "/", total_vars)),
            span(input$multiResponse[current_idx])
        ),
        actionButton("nextMultiVar", "", 
                     icon = icon("chevron-right"), 
                     class = "btn-light btn-lg",
                     style = "font-size: 1.5em; padding: 10px 20px; height: 60px; width: 60px;")
    )
  })
  
  observeEvent(input$prevMultiVar, {
    values$currentVarIndex <- if (values$currentVarIndex > 1) 
      values$currentVarIndex - 1 else length(input$multiResponse)
  })
  
  observeEvent(input$nextMultiVar, {
    values$currentVarIndex <- if (values$currentVarIndex < length(input$multiResponse)) 
      values$currentVarIndex + 1 else 1
  })
  
  # ---- Graphiques 
  output$multiPlot <- renderPlotly({
    req(input$multiResponse, input$multiFactor, values$multiResultsMain)
    
    current_var_idx <- values$currentVarIndex %||% 1
    resp_var <- input$multiResponse[current_var_idx]
    
    if (input$plotDisplayType == "main") {
      fvar <- input$multiFactor[1]
      plot_data <- values$filteredData
      agg <- values$multiResultsMain[values$multiResultsMain$Variable == resp_var & 
                                       values$multiResultsMain$Facteur == fvar &
                                       values$multiResultsMain$Type == "main", ]
    } else {
      req(input$selectedSimpleEffect)
      
      parts <- strsplit(input$selectedSimpleEffect, " \\| ")[[1]]
      main_factor <- parts[1]
      condition <- parts[2]
      
      cond_parts <- strsplit(condition, "=")[[1]]
      cond_factor <- cond_parts[1]
      cond_level <- cond_parts[2]
      
      plot_data <- values$filteredData[values$filteredData[[cond_factor]] == cond_level, ]
      
      agg <- values$multiResultsMain[values$multiResultsMain$Variable == resp_var & 
                                       values$multiResultsMain$Facteur == input$selectedSimpleEffect &
                                       values$multiResultsMain$Type == "simple_effect", ]
      
      fvar <- main_factor
    }
    
    if (nrow(agg) == 0) {
      showNotification("Aucune donnée disponible pour le graphique", type = "warning")
      return(NULL)
    }
    
    base_theme <- theme_minimal() +
      theme(
        plot.title = element_text(size = input$titleSize, face = "bold", hjust = 0.5),
        axis.title = element_text(size = input$axisTitleSize),
        axis.text = element_text(size = input$axisTextSize),
        axis.text.x = if (input$rotateXLabels) element_text(angle = 45, hjust = 1) else element_text(),
        legend.position = if (input$colorByGroups) "right" else "none",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    plot_title <- if (!is.null(input$customTitle) && input$customTitle != "") {
      input$customTitle
    } else {
      if (input$plotDisplayType == "main") {
        paste("Effet principal:", resp_var, "par", fvar)
      } else {
        parts <- strsplit(input$selectedSimpleEffect, " \\| ")[[1]]
        paste("Effet simple:", resp_var, "-", parts[1], "à", parts[2])
      }
    }
    
    base_labels <- labs(
      title = plot_title,
      x = input$customXLabel %||% fvar,
      y = input$customYLabel %||% resp_var
    )
    
    legend_title <- ifelse(is.null(input$customLegendTitle) || input$customLegendTitle == "", 
                           "Groupes statistiques", input$customLegendTitle)
    
    if (input$plotType == "box") {
      agg$y_position_groups <- max(plot_data[[resp_var]], na.rm = TRUE) + 
        (max(plot_data[[resp_var]], na.rm = TRUE) - min(plot_data[[resp_var]], na.rm = TRUE)) * 0.05
      
      if (input$colorByGroups) {
        plot_data_with_groups <- merge(plot_data, agg[, c(fvar, "groups")], by = fvar, all.x = TRUE)
        p <- ggplot(plot_data_with_groups, aes_string(x = fvar, y = resp_var, fill = "groups")) +
          geom_boxplot(alpha = 0.7) +
          scale_fill_discrete(name = legend_title)
      } else {
        p <- ggplot(plot_data, aes_string(x = fvar, y = resp_var, fill = fvar)) +
          geom_boxplot(alpha = 0.7) +
          theme(legend.position = "none")
      }
      
      p <- p + base_theme + base_labels
      
      if (!input$colorByGroups) {
        p <- p + geom_text(data = agg, 
                           aes_string(x = fvar, y = "y_position_groups", label = "groups"),
                           size = 5, fontface = "bold", color = "red", inherit.aes = FALSE)
      }
      
    } else if (input$plotType == "violin") {
      agg$y_position_groups <- max(plot_data[[resp_var]], na.rm = TRUE) + 
        (max(plot_data[[resp_var]], na.rm = TRUE) - min(plot_data[[resp_var]], na.rm = TRUE)) * 0.05
      
      if (input$colorByGroups) {
        plot_data_with_groups <- merge(plot_data, agg[, c(fvar, "groups")], by = fvar, all.x = TRUE)
        p <- ggplot(plot_data_with_groups, aes_string(x = fvar, y = resp_var, fill = "groups")) +
          geom_violin(alpha = 0.7) +
          geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
          scale_fill_discrete(name = legend_title)
      } else {
        p <- ggplot(plot_data, aes_string(x = fvar, y = resp_var, fill = fvar)) +
          geom_violin(alpha = 0.7) +
          geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
          theme(legend.position = "none")
      }
      
      p <- p + base_theme + base_labels
      
      if (!input$colorByGroups) {
        p <- p + geom_text(data = agg, 
                           aes_string(x = fvar, y = "y_position_groups", label = "groups"),
                           size = 5, fontface = "bold", color = "red", inherit.aes = FALSE)
      }
      
    } else if (input$plotType == "point") {
      if (input$colorByGroups) {
        p <- ggplot(agg, aes_string(x = fvar, y = "Moyenne", fill = "groups", color = "groups")) +
          geom_point(size = 4, shape = 21, stroke = 2) +
          scale_fill_discrete(name = legend_title) +
          scale_color_discrete(name = legend_title)
      } else {
        p <- ggplot(agg, aes_string(x = fvar, y = "Moyenne", fill = fvar, color = fvar)) +
          geom_point(size = 4, shape = 21, stroke = 2) +
          theme(legend.position = "none")
      }
      
      p <- p + base_theme + base_labels
      
      if (input$errorType == "se") {
        p <- p + geom_errorbar(aes(ymin = Moyenne - Erreur_type, ymax = Moyenne + Erreur_type), 
                               width = 0.2, color = "black")
      } else if (input$errorType == "sd") {
        p <- p + geom_errorbar(aes(ymin = Moyenne - Ecart_type, ymax = Moyenne + Ecart_type), 
                               width = 0.2, color = "black")
      } else if (input$errorType == "ci") {
        ci_margin <- 1.96 * agg$Erreur_type
        p <- p + geom_errorbar(aes(ymin = Moyenne - ci_margin, ymax = Moyenne + ci_margin), 
                               width = 0.2, color = "black")
      }
      
      if (!input$colorByGroups) {
        y_text_position <- max(agg$Moyenne + 
                                 if(input$errorType == "se") agg$Erreur_type 
                               else if(input$errorType == "sd") agg$Ecart_type 
                               else if(input$errorType == "ci") 1.96 * agg$Erreur_type
                               else 0, na.rm = TRUE) * 1.05
        
        agg$y_text_position <- y_text_position
        p <- p + geom_text(data = agg, aes_string(x = fvar, y = "y_text_position", label = "groups"),
                           size = 5, fontface = "bold", color = "red", inherit.aes = FALSE)
      }
      
    } else if (input$plotType == "hist") {
      if (input$colorByGroups) {
        p <- ggplot(agg, aes_string(x = fvar, y = "Moyenne", fill = "groups")) +
          geom_col(alpha = 0.7, color = "black") +
          scale_fill_discrete(name = legend_title)
      } else {
        p <- ggplot(agg, aes_string(x = fvar, y = "Moyenne", fill = fvar)) +
          geom_col(alpha = 0.7, color = "black") +
          theme(legend.position = "none")
      }
      
      p <- p + base_theme + base_labels
      
      if (input$errorType != "none") {
        if (input$errorType == "se") {
          p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + Erreur_type), 
                                 width = 0.2, color = "black")
        } else if (input$errorType == "sd") {
          p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + Ecart_type), 
                                 width = 0.2, color = "black")
        } else if (input$errorType == "ci") {
          ci_margin <- 1.96 * agg$Erreur_type
          p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + ci_margin), 
                                 width = 0.2, color = "black")
        }
      }
      
      p <- p + geom_text(data = agg, 
                         aes_string(x = fvar, y = "Moyenne/2", label = "round(Moyenne, 2)"),
                         size = 4, fontface = "bold", color = "white", inherit.aes = FALSE)
      
      if (!input$colorByGroups) {
        agg$y_text_pos_groups <- agg$Moyenne * 0.8
        p <- p + geom_text(data = agg, 
                           aes_string(x = fvar, y = "y_text_pos_groups", label = "groups"),
                           size = 5, fontface = "bold", color = "red", inherit.aes = FALSE)
      }
    }
    
    if (input$boxColor != "default" && !input$colorByGroups) {
      p <- p + scale_fill_brewer(palette = input$boxColor) +
        scale_color_brewer(palette = input$boxColor)
    }
    
    values$currentPlot <- p
    ggplotly(p) %>%
      layout(showlegend = if (input$colorByGroups) TRUE else FALSE)
  })
  
  output$plotTitle <- renderUI({
    req(input$multiResponse, values$currentVarIndex)
    
    current_var_idx <- values$currentVarIndex %||% 1
    current_var <- input$multiResponse[current_var_idx]
    
    type_text <- if (input$plotDisplayType == "main") "Effets principaux" else "Effets simples"
    
    paste(type_text, "-", current_var)
  })
  
  # ---- Téléchargements 
  output$downloadMainEffects <- downloadHandler(
    filename = function() { paste0("effets_principaux_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(values$multiResultsMain)
      main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Effets_principaux")
      openxlsx::writeData(wb, "Effets_principaux", main_data)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadSimpleEffects <- downloadHandler(
    filename = function() { paste0("effets_simples_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(values$multiResultsMain)
      simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Effets_simples")
      openxlsx::writeData(wb, "Effets_simples", simple_data)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadAllResults <- downloadHandler(
    filename = function() { paste0("analyse_complete_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(values$multiResultsMain)
      wb <- openxlsx::createWorkbook()
      
      main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
      simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
      
      if (nrow(main_data) > 0) {
        openxlsx::addWorksheet(wb, "Effets_principaux")
        openxlsx::writeData(wb, "Effets_principaux", main_data)
      }
      
      if (nrow(simple_data) > 0) {
        openxlsx::addWorksheet(wb, "Effets_simples")
        openxlsx::writeData(wb, "Effets_simples", simple_data)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadMultiPlot <- downloadHandler(
    filename = function() { paste0("graphique_posthoc_", Sys.Date(), ".png") },
    content = function(file) {
      if (!is.null(values$currentPlot)) {
        ggsave(file, plot = values$currentPlot, width = input$plotWidth, 
               height = input$plotHeight, dpi = input$plotDPI)
      }
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0("graphique_posthoc_", Sys.Date(), ".png") },
    content = function(file) {
      if (!is.null(values$currentPlot)) {
        ggsave(file, plot = values$currentPlot, width = input$plotWidth, 
               height = input$plotHeight, dpi = input$plotDPI)
      }
    }
  )
  
  output$downloadSummaryStats <- downloadHandler(
    filename = function() { paste0("statistiques_resumees_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(values$multiResultsMain)
      
      summary_stats <- values$multiResultsMain %>%
        group_by(Variable, Facteur, Type) %>%
        summarise(
          Nb_groupes = n(),
          Moyenne_generale = round(mean(Moyenne, na.rm = TRUE), 2),
          CV_moyen = round(mean(CV, na.rm = TRUE), 2),
          Ecart_type_moyen = round(mean(Ecart_type, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      var_summary <- values$multiResultsMain %>%
        group_by(Variable) %>%
        summarise(
          Nb_facteurs_testes = n_distinct(Facteur),
          Nb_total_groupes = n(),
          Moyenne_min = round(min(Moyenne, na.rm = TRUE), 2),
          Moyenne_max = round(max(Moyenne, na.rm = TRUE), 2),
          CV_min = round(min(CV, na.rm = TRUE), 2),
          CV_max = round(max(CV, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Resume_par_facteur")
      openxlsx::writeData(wb, "Resume_par_facteur", summary_stats)
      openxlsx::addWorksheet(wb, "Resume_par_variable")
      openxlsx::writeData(wb, "Resume_par_variable", var_summary)
      openxlsx::addWorksheet(wb, "Donnees_completes")
      openxlsx::writeData(wb, "Donnees_completes", values$multiResultsMain)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadFullReport <- downloadHandler(
    filename = function() { paste0("rapport_complet_", Sys.Date(), ".pdf") },
    content = function(file) {
      showNotification("Génération du PDF - Fonctionnalité nécessite rmarkdown", 
                       type = "warning", duration = 5)
    }
  )
  # ---- Analyses multivariees ----
  
  # Fonction helper pour telecharger les graphiques avec options avancees
  createPlotDownloadHandler <- function(plot_func, default_name) {
    downloadHandler(
      filename = function() {
        ext <- input[[paste0(default_name, "_format")]]
        paste0(default_name, "_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        width <- input[[paste0(default_name, "_width")]]
        height <- input[[paste0(default_name, "_height")]]
        dpi <- input[[paste0(default_name, "_dpi")]]
        
        p <- plot_func()
        
        ggsave(file, plot = p, device = input[[paste0(default_name, "_format")]], 
               width = width, height = height, dpi = dpi, units = "cm")
      }
    )
  }
  
  # SECTION 1: ACP (Analyse en Composantes Principales)
  
  # Selecteurs d'interface pour l'ACP
  output$pcaMeansGroupSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    if (length(fac_cols) == 0) return(NULL)
    
    selectInput("pcaMeansGroup", "Variable de groupement pour les moyennes:", 
                choices = fac_cols)
  })
  
  output$pcaVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "pcaVars",
      label = "Selectionnez les variables pour l'ACP:",
      choices = num_cols,
      multiple = TRUE,
      selected = num_cols,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$pcaQualiSupSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    if (length(fac_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "pcaQualiSup",
      label = "Variables qualitatives supplementaires:",
      choices = fac_cols,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$pcaIndSupSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "pcaIndSup",
      label = "Individus supplementaires (optionnel):",
      choices = rownames(values$filteredData),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$pcaLabelSourceSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("pcaLabelSource", "Source des labels pour individus (optionnel):",
                choices = c("Rownames" = "rownames", all_cols), selected = "rownames")
  })
  
  # Fonction pour calculer les moyennes par groupe
  calculate_group_means <- function(data, vars, group_var) {
    if (is.null(group_var) || !group_var %in% names(data)) {
      return(data)
    }
    
    means_data <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(across(all_of(vars), mean, na.rm = TRUE), .groups = 'drop') %>%
      column_to_rownames(group_var)
    
    return(means_data)
  }
  
  # ACP MIS A JOUR AUTOMATIQUEMENT
  pcaResultReactive <- reactive({
    req(values$filteredData, input$pcaVars)
    
    input$pcaScale
    input$pcaUseMeans
    input$pcaMeansGroup
    input$pcaQualiSup
    input$pcaIndSup
    input$pcaComponents
    input$pcaLabelSource
    
    tryCatch({
      if (!is.null(input$pcaUseMeans) && input$pcaUseMeans && !is.null(input$pcaMeansGroup)) {
        pca_data <- calculate_group_means(values$filteredData, input$pcaVars, input$pcaMeansGroup)
      } else {
        pca_data <- values$filteredData[, input$pcaVars, drop = FALSE]
      }
      
      quali_sup_indices <- NULL
      if (!is.null(input$pcaQualiSup) && (!input$pcaUseMeans || is.null(input$pcaUseMeans))) {
        quali_sup_indices <- which(names(values$filteredData) %in% input$pcaQualiSup)
      }
      
      ind_sup_indices <- NULL
      if (!is.null(input$pcaIndSup) && (!input$pcaUseMeans || is.null(input$pcaUseMeans))) {
        ind_sup_indices <- which(rownames(values$filteredData) %in% input$pcaIndSup)
      }
      
      if (!input$pcaUseMeans || is.null(input$pcaUseMeans)) {
        all_data <- cbind(pca_data, values$filteredData[, input$pcaQualiSup, drop = FALSE])
        
        if (!is.null(input$pcaLabelSource) && input$pcaLabelSource != "rownames") {
          custom_labels <- as.character(values$filteredData[[input$pcaLabelSource]])
          rownames(all_data) <- make.unique(custom_labels)
        }
      } else {
        all_data <- pca_data
      }
      
      res.pca <- PCA(all_data,
                     scale.unit = ifelse(is.null(input$pcaScale), TRUE, input$pcaScale),
                     quali.sup = quali_sup_indices,
                     ind.sup = ind_sup_indices,
                     ncp = ifelse(is.null(input$pcaComponents), 5, input$pcaComponents),
                     graph = FALSE)
      
      return(res.pca)
      
    }, error = function(e) {
      showNotification(paste("Erreur ACP :", e$message), type = "error")
      return(NULL)
    })
  })
  
  observe({
    res <- pcaResultReactive()
    if (!is.null(res)) {
      values$pcaResult <- res
    }
  })
  
  # DATAFRAMES DES RESULTATS ACP
  pcaDataframes <- reactive({
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    
    tryCatch({
      eigenvalues_df <- as.data.frame(get_eigenvalue(res.pca))
      eigenvalues_df$Dimension <- rownames(eigenvalues_df)
      eigenvalues_df <- eigenvalues_df[, c("Dimension", "eigenvalue", "variance.percent", "cumulative.variance.percent")]
      
      ind_coords_df <- as.data.frame(res.pca$ind$coord)
      ind_coords_df$Individual <- rownames(ind_coords_df)
      ind_coords_df <- ind_coords_df[, c("Individual", names(res.pca$ind$coord))]
      
      ind_contrib_df <- as.data.frame(res.pca$ind$contrib)
      ind_contrib_df$Individual <- rownames(ind_contrib_df)
      ind_contrib_df <- ind_contrib_df[, c("Individual", names(res.pca$ind$contrib))]
      
      ind_cos2_df <- as.data.frame(res.pca$ind$cos2)
      ind_cos2_df$Individual <- rownames(ind_cos2_df)
      ind_cos2_df <- ind_cos2_df[, c("Individual", names(res.pca$ind$cos2))]
      
      var_coords_df <- as.data.frame(res.pca$var$coord)
      var_coords_df$Variable <- rownames(var_coords_df)
      var_coords_df <- var_coords_df[, c("Variable", names(res.pca$var$coord))]
      
      var_contrib_df <- as.data.frame(res.pca$var$contrib)
      var_contrib_df$Variable <- rownames(var_contrib_df)
      var_contrib_df <- var_contrib_df[, c("Variable", names(res.pca$var$contrib))]
      
      var_cos2_df <- as.data.frame(res.pca$var$cos2)
      var_cos2_df$Variable <- rownames(var_cos2_df)
      var_cos2_df <- var_cos2_df[, c("Variable", names(res.pca$var$cos2))]
      
      var_cor_df <- as.data.frame(res.pca$var$cor)
      var_cor_df$Variable <- rownames(var_cor_df)
      var_cor_df <- var_cor_df[, c("Variable", names(res.pca$var$cor))]
      
      return(list(
        eigenvalues = eigenvalues_df,
        ind_coords = ind_coords_df,
        ind_contrib = ind_contrib_df,
        ind_cos2 = ind_cos2_df,
        var_coords = var_coords_df,
        var_contrib = var_contrib_df,
        var_cos2 = var_cos2_df,
        var_cor = var_cor_df
      ))
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$pcaSummary <- renderPrint({
    req(pcaResultReactive())
    summary(pcaResultReactive())
  })
  
  # Fonction pour creer le graphique ACP
  createPcaPlot <- function() {
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    
    eigenvals <- get_eigenvalue(res.pca)
    pc1_var <- round(eigenvals[1, "variance.percent"], 1)
    pc2_var <- round(eigenvals[2, "variance.percent"], 1)
    
    x_label <- if (!is.null(input$pcaXLabel) && input$pcaXLabel != "") {
      input$pcaXLabel
    } else {
      paste0("PC1 (", pc1_var, "%)")
    }
    
    y_label <- if (!is.null(input$pcaYLabel) && input$pcaYLabel != "") {
      input$pcaYLabel
    } else {
      paste0("PC2 (", pc2_var, "%)")
    }
    
    plot_title <- if (!is.null(input$pcaPlotTitle) && input$pcaPlotTitle != "") {
      input$pcaPlotTitle
    } else {
      "ACP - Analyse en Composantes Principales"
    }
    
    plot_type <- ifelse(is.null(input$pcaPlotType), "var", input$pcaPlotType)
    
    if (plot_type == "var") {
      p <- fviz_pca_var(res.pca, 
                        col.var = "cos2",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE,
                        title = plot_title) +
        labs(x = x_label, y = y_label)
      
    } else if (plot_type == "ind") {
      has_ind_sup <- !is.null(res.pca$ind.sup)
      
      if (has_ind_sup) {
        n_active <- nrow(res.pca$ind$coord)
        col_vector <- rep("Active", n_active)
        
        p <- fviz_pca_ind(res.pca,
                          geom.ind = c("point", "text"),
                          col.ind = col_vector,
                          palette = c("Active" = "#00AFBB"),
                          addEllipses = FALSE,
                          repel = TRUE,
                          title = plot_title,
                          legend.title = "Type") +
          labs(x = x_label, y = y_label)
      } else {
        p <- fviz_pca_ind(res.pca, 
                          col.ind = "contrib",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE,
                          title = plot_title) +
          labs(x = x_label, y = y_label)
      }
      
    } else {
      has_ind_sup <- !is.null(res.pca$ind.sup)
      
      if (has_ind_sup) {
        n_active <- nrow(res.pca$ind$coord)
        col_vector <- rep("Active", n_active)
        
        p <- fviz_pca_biplot(res.pca,
                             geom.ind = "point",
                             col.ind = col_vector,
                             palette = c("Active" = "#00AFBB"),
                             col.var = "#FC4E07",
                             repel = TRUE,
                             addEllipses = FALSE,
                             title = plot_title,
                             legend.title = "Type") +
          labs(x = x_label, y = y_label)
      } else {
        p <- fviz_pca_biplot(res.pca, 
                             repel = TRUE,
                             col.var = "#FC4E07", 
                             col.ind = "#00AFBB",
                             label = "all",
                             title = plot_title) +
          labs(x = x_label, y = y_label)
      }
    }
    
    if (!is.null(input$pcaCenterAxes) && input$pcaCenterAxes) {
      if (plot_type == "var") {
        coords <- res.pca$var$coord[, 1:2]
      } else if (plot_type == "ind") {
        coords <- res.pca$ind$coord[, 1:2]
      } else {
        coords_ind <- res.pca$ind$coord[, 1:2]
        coords_var <- res.pca$var$coord[, 1:2]
        coords <- rbind(coords_ind, coords_var)
      }
      
      max_range <- max(abs(range(coords, na.rm = TRUE)))
      p <- p + xlim(-max_range, max_range) + ylim(-max_range, max_range)
    }
    
    return(p)
  }
  
  output$pcaPlot <- renderPlotly({
    p <- createPcaPlot()
    suppressWarnings({
      plotly_obj <- ggplotly(p) %>% 
        layout(showlegend = TRUE) %>%
        config(displayModeBar = TRUE)
    })
    plotly_obj
  })
  
  # Telechargement graphique ACP avec options avancees
  output$downloadPcaPlot <- downloadHandler(
    filename = function() {
      ext <- input$pcaPlot_format
      paste0("pca_plot_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      width <- input$pcaPlot_width
      height <- input$pcaPlot_height
      dpi <- input$pcaPlot_dpi
      
      p <- createPcaPlot()
      ggsave(file, plot = p, device = input$pcaPlot_format, 
             width = width, height = height, dpi = dpi, units = "cm")
    }
  )
  
  # Telechargement donnees ACP - Excel
  output$downloadPcaDataXlsx <- downloadHandler(
    filename = function() {
      paste0("pca_resultats_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(pcaDataframes())
      dfs <- pcaDataframes()
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Valeurs_propres")
      writeData(wb, "Valeurs_propres", dfs$eigenvalues)
      
      addWorksheet(wb, "Ind_coordonnees")
      writeData(wb, "Ind_coordonnees", dfs$ind_coords)
      
      addWorksheet(wb, "Ind_contributions")
      writeData(wb, "Ind_contributions", dfs$ind_contrib)
      
      addWorksheet(wb, "Ind_cos2")
      writeData(wb, "Ind_cos2", dfs$ind_cos2)
      
      addWorksheet(wb, "Var_coordonnees")
      writeData(wb, "Var_coordonnees", dfs$var_coords)
      
      addWorksheet(wb, "Var_contributions")
      writeData(wb, "Var_contributions", dfs$var_contrib)
      
      addWorksheet(wb, "Var_cos2")
      writeData(wb, "Var_cos2", dfs$var_cos2)
      
      addWorksheet(wb, "Var_correlations")
      writeData(wb, "Var_correlations", dfs$var_cor)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Telechargement donnees ACP - CSV
  output$downloadPcaDataCsv <- downloadHandler(
    filename = function() {
      paste0("pca_resultats_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(pcaDataframes())
      dfs <- pcaDataframes()
      
      temp_dir <- tempdir()
      csv_files <- c()
      
      write.csv(dfs$eigenvalues, file.path(temp_dir, "valeurs_propres.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "valeurs_propres.csv")
      
      write.csv(dfs$ind_coords, file.path(temp_dir, "ind_coordonnees.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "ind_coordonnees.csv")
      
      write.csv(dfs$ind_contrib, file.path(temp_dir, "ind_contributions.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "ind_contributions.csv")
      
      write.csv(dfs$ind_cos2, file.path(temp_dir, "ind_cos2.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "ind_cos2.csv")
      
      write.csv(dfs$var_coords, file.path(temp_dir, "var_coordonnees.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "var_coordonnees.csv")
      
      write.csv(dfs$var_contrib, file.path(temp_dir, "var_contributions.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "var_contributions.csv")
      
      write.csv(dfs$var_cos2, file.path(temp_dir, "var_cos2.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "var_cos2.csv")
      
      write.csv(dfs$var_cor, file.path(temp_dir, "var_correlations.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "var_correlations.csv")
      
      zip(file, file.path(temp_dir, csv_files), flags = "-j")
    }
  )
  
  
  # SECTION 2: HCPC (Classification Hierarchique)
  
  hcpcResultReactive <- reactive({
    req(pcaResultReactive())
    input$hcpcClusters
    
    tryCatch({
      res.hcpc <- HCPC(pcaResultReactive(),
                       nb.clust = ifelse(is.null(input$hcpcClusters), 3, input$hcpcClusters),
                       graph = FALSE)
      return(res.hcpc)
    }, error = function(e) {
      showNotification(paste("Erreur HCPC :", e$message), type = "error")
      return(NULL)
    })
  })
  
  observe({
    res <- hcpcResultReactive()
    if (!is.null(res)) {
      values$hcpcResult <- res
    }
  })
  
  hcpcDataframes <- reactive({
    req(hcpcResultReactive())
    res.hcpc <- hcpcResultReactive()
    
    tryCatch({
      cluster_assign_df <- data.frame(
        Individual = rownames(res.hcpc$data.clust),
        Cluster = res.hcpc$data.clust$clust,
        stringsAsFactors = FALSE
      )
      
      desc_var_list <- list()
      if (!is.null(res.hcpc$desc.var$quanti)) {
        for (i in 1:length(res.hcpc$desc.var$quanti)) {
          if (!is.null(res.hcpc$desc.var$quanti[[i]])) {
            df <- as.data.frame(res.hcpc$desc.var$quanti[[i]])
            df$Variable <- rownames(df)
            df$Cluster <- i
            df <- df[, c("Cluster", "Variable", setdiff(names(df), c("Cluster", "Variable")))]
            desc_var_list[[i]] <- df
          }
        }
      }
      desc_var_df <- if (length(desc_var_list) > 0) do.call(rbind, desc_var_list) else NULL
      
      desc_axes_list <- list()
      if (!is.null(res.hcpc$desc.axes$quanti)) {
        for (i in 1:length(res.hcpc$desc.axes$quanti)) {
          if (!is.null(res.hcpc$desc.axes$quanti[[i]])) {
            df <- as.data.frame(res.hcpc$desc.axes$quanti[[i]])
            df$Axe <- rownames(df)
            df$Cluster <- i
            df <- df[, c("Cluster", "Axe", setdiff(names(df), c("Cluster", "Axe")))]
            desc_axes_list[[i]] <- df
          }
        }
      }
      desc_axes_df <- if (length(desc_axes_list) > 0) do.call(rbind, desc_axes_list) else NULL
      
      parangons_list <- list()
      if (!is.null(res.hcpc$desc.ind$para)) {
        for (i in 1:length(res.hcpc$desc.ind$para)) {
          if (!is.null(res.hcpc$desc.ind$para[[i]])) {
            df <- as.data.frame(res.hcpc$desc.ind$para[[i]])
            df$Individual <- rownames(df)
            df$Cluster <- i
            df <- df[, c("Cluster", "Individual", setdiff(names(df), c("Cluster", "Individual")))]
            parangons_list[[i]] <- df
          }
        }
      }
      parangons_df <- if (length(parangons_list) > 0) do.call(rbind, parangons_list) else NULL
      
      dist_list <- list()
      if (!is.null(res.hcpc$desc.ind$dist)) {
        for (i in 1:length(res.hcpc$desc.ind$dist)) {
          if (!is.null(res.hcpc$desc.ind$dist[[i]])) {
            df <- as.data.frame(res.hcpc$desc.ind$dist[[i]])
            df$Individual <- rownames(df)
            df$Cluster <- i
            df <- df[, c("Cluster", "Individual", setdiff(names(df), c("Cluster", "Individual")))]
            dist_list[[i]] <- df
          }
        }
      }
      dist_df <- if (length(dist_list) > 0) do.call(rbind, dist_list) else NULL
      
      return(list(
        cluster_assignment = cluster_assign_df,
        desc_variables = desc_var_df,
        desc_axes = desc_axes_df,
        parangons = parangons_df,
        distant_individuals = dist_df
      ))
    }, error = function(e) {
      return(NULL)
    })
  })
  
  createHcpcDendPlot <- function() {
    req(hcpcResultReactive())
    res.hcpc <- hcpcResultReactive()
    
    dend_title <- if (!is.null(input$hcpcDendTitle) && input$hcpcDendTitle != "") {
      input$hcpcDendTitle
    } else {
      "Dendrogramme HCPC"
    }
    
    p_dend <- fviz_dend(res.hcpc,
                        cex = 0.7,
                        palette = "jco",
                        rect = TRUE,
                        rect_fill = TRUE,
                        rect_border = "jco",
                        main = dend_title,
                        sub = paste("Nombre de clusters:", length(unique(res.hcpc$data.clust$clust))))
    return(p_dend)
  }
  
  createHcpcClusterPlot <- function() {
    req(hcpcResultReactive(), pcaResultReactive())
    res.hcpc <- hcpcResultReactive()
    res.pca <- pcaResultReactive()
    
    cluster_title <- if (!is.null(input$hcpcClusterTitle) && input$hcpcClusterTitle != "") {
      input$hcpcClusterTitle
    } else {
      "Carte des clusters HCPC"
    }
    
    eigenvals <- get_eigenvalue(res.pca)
    pc1_var <- round(eigenvals[1, "variance.percent"], 1)
    pc2_var <- round(eigenvals[2, "variance.percent"], 1)
    
    x_label <- if (!is.null(input$hcpcClusterXLabel) && input$hcpcClusterXLabel != "") {
      input$hcpcClusterXLabel
    } else {
      paste0("PC1 (", pc1_var, "%)")
    }
    
    y_label <- if (!is.null(input$hcpcClusterYLabel) && input$hcpcClusterYLabel != "") {
      input$hcpcClusterYLabel
    } else {
      paste0("PC2 (", pc2_var, "%)")
    }
    
    p_cluster <- fviz_cluster(res.hcpc,
                              repel = TRUE,
                              show.clust.cent = TRUE,
                              palette = "jco",
                              ggtheme = theme_minimal(),
                              main = cluster_title) +
      labs(x = x_label, y = y_label)
    
    if (!is.null(input$hcpcCenterAxes) && input$hcpcCenterAxes) {
      coords <- res.pca$ind$coord[, 1:2]
      max_range <- max(abs(range(coords, na.rm = TRUE)))
      p_cluster <- p_cluster + xlim(-max_range, max_range) + ylim(-max_range, max_range)
    }
    
    return(p_cluster)
  }
  
  output$hcpcDendPlot <- renderPlotly({
    p_dend <- createHcpcDendPlot()
    ggplotly(p_dend) %>% layout(margin = list(b = 100))
  })
  
  output$hcpcClusterPlot <- renderPlotly({
    p_cluster <- createHcpcClusterPlot()
    ggplotly(p_cluster) %>% layout(showlegend = TRUE)
  })
  
  output$hcpcSummary <- renderPrint({
    req(hcpcResultReactive())
    res.hcpc <- hcpcResultReactive()
    
    cat("=== CLASSIFICATION HIERARCHIQUE SUR COMPOSANTES PRINCIPALES (HCPC) ===\n\n")
    cat("Nombre de clusters:", length(unique(res.hcpc$data.clust$clust)), "\n\n")
    
    cluster_results <- data.frame(
      Individual = rownames(res.hcpc$data.clust),
      Cluster = res.hcpc$data.clust$clust
    )
    cat("=== AFFECTATION DES INDIVIDUS AUX CLUSTERS ===\n")
    print(cluster_results)
    cat("\n")
    
    cat("=== VARIABLES LES PLUS DISCRIMINANTES PAR CLUSTER ===\n")
    if (!is.null(res.hcpc$desc.var$quanti)) {
      for (i in 1:length(res.hcpc$desc.var$quanti)) {
        if (!is.null(res.hcpc$desc.var$quanti[[i]])) {
          cat("\n--- CLUSTER", i, "---\n")
          print(round(res.hcpc$desc.var$quanti[[i]], 4))
        }
      }
    }
    
    if (!is.null(res.hcpc$desc.axes$quanti)) {
      cat("\n=== DESCRIPTION DES AXES PAR CLUSTER ===\n")
      for (i in 1:length(res.hcpc$desc.axes$quanti)) {
        if (!is.null(res.hcpc$desc.axes$quanti[[i]])) {
          cat("\n--- CLUSTER", i, " - AXES ---\n")
          print(round(res.hcpc$desc.axes$quanti[[i]], 4))
        }
      }
    }
    
    if (!is.null(res.hcpc$desc.ind$para)) {
      cat("\n=== INDIVIDUS LES PLUS REPRESENTATIFS (PARANGONS) ===\n")
      for (i in 1:length(res.hcpc$desc.ind$para)) {
        if (!is.null(res.hcpc$desc.ind$para[[i]])) {
          cat("\n--- CLUSTER", i, " - PARANGONS ---\n")
          print(res.hcpc$desc.ind$para[[i]])
        }
      }
    }
    
    if (!is.null(res.hcpc$desc.ind$dist)) {
      cat("\n=== INDIVIDUS LES PLUS ELOIGNES DU CENTRE ===\n")
      for (i in 1:length(res.hcpc$desc.ind$dist)) {
        if (!is.null(res.hcpc$desc.ind$dist[[i]])) {
          cat("\n--- CLUSTER", i, " - INDIVIDUS ELOIGNES ---\n")
          print(res.hcpc$desc.ind$dist[[i]])
        }
      }
    }
  })
  
  # Telechargement graphique HCPC Dendrogramme
  output$downloadHcpcDendPlot <- downloadHandler(
    filename = function() {
      ext <- input$hcpcDend_format
      paste0("hcpc_dendrogramme_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      width <- input$hcpcDend_width
      height <- input$hcpcDend_height
      dpi <- input$hcpcDend_dpi
      
      p_dend <- createHcpcDendPlot()
      ggsave(file, plot = p_dend, device = input$hcpcDend_format, 
             width = width, height = height, dpi = dpi, units = "cm")
    }
  )
  
  # Telechargement graphique HCPC Cluster
  output$downloadHcpcClusterPlot <- downloadHandler(
    filename = function() {
      ext <- input$hcpcCluster_format
      paste0("hcpc_clusters_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      width <- input$hcpcCluster_width
      height <- input$hcpcCluster_height
      dpi <- input$hcpcCluster_dpi
      
      p_cluster <- createHcpcClusterPlot()
      ggsave(file, plot = p_cluster, device = input$hcpcCluster_format, 
             width = width, height = height, dpi = dpi, units = "cm")
    }
  )
  
  # Telechargement donnees HCPC - Excel
  output$downloadHcpcDataXlsx <- downloadHandler(
    filename = function() {
      paste0("hcpc_resultats_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(hcpcDataframes())
      dfs <- hcpcDataframes()
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Affectation_clusters")
      writeData(wb, "Affectation_clusters", dfs$cluster_assignment)
      
      if (!is.null(dfs$desc_variables)) {
        addWorksheet(wb, "Desc_variables")
        writeData(wb, "Desc_variables", dfs$desc_variables)
      }
      
      if (!is.null(dfs$desc_axes)) {
        addWorksheet(wb, "Desc_axes")
        writeData(wb, "Desc_axes", dfs$desc_axes)
      }
      
      if (!is.null(dfs$parangons)) {
        addWorksheet(wb, "Parangons")
        writeData(wb, "Parangons", dfs$parangons)
      }
      
      if (!is.null(dfs$distant_individuals)) {
        addWorksheet(wb, "Individus_eloignes")
        writeData(wb, "Individus_eloignes", dfs$distant_individuals)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Telechargement donnees HCPC - CSV
  output$downloadHcpcDataCsv <- downloadHandler(
    filename = function() {
      paste0("hcpc_resultats_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(hcpcDataframes())
      dfs <- hcpcDataframes()
      
      temp_dir <- tempdir()
      csv_files <- c()
      
      write.csv(dfs$cluster_assignment, file.path(temp_dir, "affectation_clusters.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "affectation_clusters.csv")
      
      if (!is.null(dfs$desc_variables)) {
        write.csv(dfs$desc_variables, file.path(temp_dir, "desc_variables.csv"), row.names = FALSE)
        csv_files <- c(csv_files, "desc_variables.csv")
      }
      
      if (!is.null(dfs$desc_axes)) {
        write.csv(dfs$desc_axes, file.path(temp_dir, "desc_axes.csv"), row.names = FALSE)
        csv_files <- c(csv_files, "desc_axes.csv")
      }
      
      if (!is.null(dfs$parangons)) {
        write.csv(dfs$parangons, file.path(temp_dir, "parangons.csv"), row.names = FALSE)
        csv_files <- c(csv_files, "parangons.csv")
      }
      
      if (!is.null(dfs$distant_individuals)) {
        write.csv(dfs$distant_individuals, file.path(temp_dir, "individus_eloignes.csv"), row.names = FALSE)
        csv_files <- c(csv_files, "individus_eloignes.csv")
      }
      
      zip(file, file.path(temp_dir, csv_files), flags = "-j")
    }
  )
  
  
  # SECTION 3: AFD (Analyse Factorielle Discriminante) - VERSION CORRIGEE
  
  output$afdFactorSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    if (length(fac_cols) == 0) return(NULL)
    
    selectInput("afdFactor", "Variable categorielle (groupes):",
                choices = fac_cols)
  })
  
  output$afdVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "afdVars",
      label = "Variables quantitatives:",
      choices = num_cols,
      multiple = TRUE,
      selected = num_cols,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$afdMeansGroupSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    if (length(fac_cols) == 0) return(NULL)
    
    selectInput("afdMeansGroup", "Variable de groupement pour les moyennes:", 
                choices = fac_cols)
  })
  
  # CORRECTION MAJEURE : Gestion correcte des moyennes par groupe
  afdResultReactive <- reactive({
    req(values$filteredData, input$afdVars, input$afdFactor)
    
    input$afdUseMeans
    input$afdMeansGroup
    input$afdCrossValidation
    
    tryCatch({
      if (!is.null(input$afdUseMeans) && input$afdUseMeans && !is.null(input$afdMeansGroup)) {
        # Calculer les moyennes uniquement sur les variables quantitatives
        afd_data_numeric <- calculate_group_means(values$filteredData, 
                                                  input$afdVars, 
                                                  input$afdMeansGroup)
        
        # Ajouter la variable de groupement comme facteur avec les noms des groupes
        afd_data <- afd_data_numeric
        afd_data[[input$afdFactor]] <- factor(rownames(afd_data_numeric))
      } else {
        afd_data <- values$filteredData[, c(input$afdVars, input$afdFactor), drop = FALSE]
      }
      
      afd_formula <- as.formula(paste(input$afdFactor, "~", paste(input$afdVars, collapse = " + ")))
      
      afd_result <- lda(afd_formula, data = afd_data)
      afd_predict <- predict(afd_result, afd_data[, input$afdVars, drop = FALSE])
      
      # Validation croisée uniquement si pas de moyennes
      cv_results <- NULL
      if (!is.null(input$afdCrossValidation) && input$afdCrossValidation && 
          (!input$afdUseMeans || is.null(input$afdUseMeans))) {
        cv_predictions <- numeric(nrow(afd_data))
        for (i in 1:nrow(afd_data)) {
          train_data <- afd_data[-i, ]
          test_data <- afd_data[i, , drop = FALSE]
          cv_model <- lda(afd_formula, data = train_data)
          cv_pred <- predict(cv_model, test_data)
          cv_predictions[i] <- as.character(cv_pred$class)
        }
        cv_predictions <- factor(cv_predictions, levels = levels(afd_data[[input$afdFactor]]))
        cv_results <- list(predictions = cv_predictions)
      }
      
      return(list(
        model = afd_result,
        predictions = afd_predict,
        data = afd_data,
        cv_results = cv_results
      ))
      
    }, error = function(e) {
      showNotification(paste("Erreur AFD :", e$message), type = "error")
      return(NULL)
    })
  })
  
  observe({
    res <- afdResultReactive()
    if (!is.null(res)) {
      values$afdResult <- res$model
    }
  })
  
  # CORRECTION : Renommage de 'loadings' en 'coefficients' pour inclure les coefficients discriminants
  afdDataframes <- reactive({
    req(afdResultReactive())
    afd_res <- afdResultReactive()
    afd_result <- afd_res$model
    afd_predict <- afd_res$predictions
    afd_data <- afd_res$data
    
    tryCatch({
      ind_coords_df <- as.data.frame(afd_predict$x)
      ind_coords_df$Individual <- rownames(ind_coords_df)
      ind_coords_df$Groupe_reel <- afd_data[[input$afdFactor]]
      ind_coords_df$Groupe_predit <- afd_predict$class
      ind_coords_df <- ind_coords_df[, c("Individual", "Groupe_reel", "Groupe_predit", 
                                         setdiff(names(ind_coords_df), c("Individual", "Groupe_reel", "Groupe_predit")))]
      
      # Coefficients des fonctions discriminantes (loadings)
      loadings_df <- as.data.frame(afd_result$scaling)
      loadings_df$Variable <- rownames(loadings_df)
      loadings_df <- loadings_df[, c("Variable", setdiff(names(loadings_df), "Variable"))]
      
      X_std <- scale(afd_data[, input$afdVars])
      scores <- as.matrix(X_std) %*% afd_result$scaling
      structure_matrix <- cor(X_std, scores)
      structure_df <- as.data.frame(structure_matrix)
      structure_df$Variable <- rownames(structure_df)
      structure_df <- structure_df[, c("Variable", setdiff(names(structure_df), "Variable"))]
      
      f_tests_df <- data.frame(Variable = input$afdVars)
      for (var in input$afdVars) {
        aov_result <- aov(as.formula(paste(var, "~", input$afdFactor)), data = afd_data)
        f_stat <- summary(aov_result)[[1]][1, "F value"]
        p_val <- summary(aov_result)[[1]][1, "Pr(>F)"]
        f_tests_df[f_tests_df$Variable == var, "F_statistic"] <- round(f_stat, 4)
        f_tests_df[f_tests_df$Variable == var, "p_value"] <- round(p_val, 4)
      }
      
      confusion_matrix <- table(Reel = afd_data[[input$afdFactor]], 
                                Predit = afd_predict$class)
      confusion_df <- as.data.frame.matrix(confusion_matrix)
      confusion_df$Groupe_reel <- rownames(confusion_df)
      confusion_df <- confusion_df[, c("Groupe_reel", setdiff(names(confusion_df), "Groupe_reel"))]
      
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      group_acc_df <- data.frame(
        Groupe = rownames(confusion_matrix),
        Taux_classification = numeric(nrow(confusion_matrix))
      )
      for (i in 1:nrow(confusion_matrix)) {
        group_acc_df$Taux_classification[i] <- round(confusion_matrix[i,i] / sum(confusion_matrix[i,]) * 100, 2)
      }
      
      centroids_df <- as.data.frame(afd_result$means)
      centroids_df$Groupe <- rownames(centroids_df)
      centroids_df <- centroids_df[, c("Groupe", setdiff(names(centroids_df), "Groupe"))]
      
      eigenvals <- afd_result$svd^2
      prop_var <- eigenvals / sum(eigenvals) * 100
      can_cor <- sqrt(eigenvals / (1 + eigenvals))
      
      variance_df <- data.frame(
        Fonction = paste0("LD", 1:length(eigenvals)),
        Valeur_propre = eigenvals,
        Variance_expliquee = round(prop_var, 2),
        Variance_cumulee = round(cumsum(prop_var), 2),
        Correlation_canonique = round(can_cor, 4)
      )
      
      cv_confusion_df <- NULL
      cv_accuracy_df <- NULL
      if (!is.null(afd_res$cv_results)) {
        cv_confusion <- table(Reel = afd_data[[input$afdFactor]], 
                              Predit = afd_res$cv_results$predictions)
        cv_confusion_df <- as.data.frame.matrix(cv_confusion)
        cv_confusion_df$Groupe_reel <- rownames(cv_confusion_df)
        cv_confusion_df <- cv_confusion_df[, c("Groupe_reel", setdiff(names(cv_confusion_df), "Groupe_reel"))]
        
        cv_accuracy <- sum(diag(cv_confusion)) / sum(cv_confusion)
        cv_accuracy_df <- data.frame(
          Metrique = c("Taux_classification_global"),
          Valeur = round(cv_accuracy * 100, 2)
        )
      }
      
      return(list(
        ind_coords = ind_coords_df,
        coefficients = loadings_df,
        structure_matrix = structure_df,
        f_tests = f_tests_df,
        confusion_matrix = confusion_df,
        classification_rates = group_acc_df,
        centroids = centroids_df,
        variance_explained = variance_df,
        cv_confusion = cv_confusion_df,
        cv_accuracy = cv_accuracy_df
      ))
    }, error = function(e) {
      return(NULL)
    })
  })
  
  createAfdIndPlot <- function() {
    req(afdResultReactive())
    afd_res <- afdResultReactive()
    afd_result <- afd_res$model
    afd_predict <- afd_res$predictions
    afd_data <- afd_res$data
    
    eigenvals <- afd_result$svd^2
    prop_var <- eigenvals / sum(eigenvals) * 100
    
    afd_plot_data <- data.frame(
      LD1 = afd_predict$x[,1], 
      LD2 = if(ncol(afd_predict$x) > 1) afd_predict$x[,2] else 0,
      Group = afd_data[[input$afdFactor]], 
      Individual = rownames(afd_data)
    )
    
    ind_title <- if (!is.null(input$afdIndTitle) && input$afdIndTitle != "") {
      input$afdIndTitle
    } else {
      "AFD - Projection des individus"
    }
    
    x_label <- if (!is.null(input$afdIndXLabel) && input$afdIndXLabel != "") {
      input$afdIndXLabel
    } else {
      if (length(prop_var) >= 1) paste0("LD1 (", round(prop_var[1], 2), "%)") else "LD1"
    }
    
    y_label <- if (!is.null(input$afdIndYLabel) && input$afdIndYLabel != "") {
      input$afdIndYLabel
    } else {
      if (length(prop_var) >= 2) paste0("LD2 (", round(prop_var[2], 2), "%)") else "LD2"
    }
    
    p_ind <- ggplot(afd_plot_data, aes(x = LD1, y = LD2, color = Group)) +
      geom_point() +
      stat_ellipse() +
      geom_text_repel(aes(label = Individual)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      labs(title = ind_title, x = x_label, y = y_label) +
      theme_minimal()
    
    if (!is.null(input$afdIndCenterAxes) && input$afdIndCenterAxes) {
      coords <- afd_plot_data[, c("LD1", "LD2")]
      max_range <- max(abs(range(coords, na.rm = TRUE)))
      p_ind <- p_ind + xlim(-max_range, max_range) + ylim(-max_range, max_range)
    }
    
    return(p_ind)
  }
  
  createAfdVarPlot <- function() {
    req(afdResultReactive())
    afd_res <- afdResultReactive()
    afd_result <- afd_res$model
    
    loadings <- afd_result$scaling
    var_contrib <- data.frame(
      Variable = rownames(loadings), 
      LD1 = loadings[,1], 
      LD2 = if(ncol(loadings) > 1) loadings[,2] else 0
    )
    
    eigenvals <- afd_result$svd^2
    prop_var <- eigenvals / sum(eigenvals) * 100
    
    var_title <- if (!is.null(input$afdVarTitle) && input$afdVarTitle != "") {
      input$afdVarTitle
    } else {
      "AFD - Contribution des variables"
    }
    
    x_label <- if (!is.null(input$afdVarXLabel) && input$afdVarXLabel != "") {
      input$afdVarXLabel
    } else {
      if (length(prop_var) >= 1) paste0("LD1 (", round(prop_var[1], 2), "%)") else "LD1"
    }
    
    y_label <- if (!is.null(input$afdVarYLabel) && input$afdVarYLabel != "") {
      input$afdVarYLabel
    } else {
      if (length(prop_var) >= 2) paste0("LD2 (", round(prop_var[2], 2), "%)") else "LD2"
    }
    
    p_var <- ggplot(var_contrib, aes(x = LD1, y = LD2)) +
      geom_segment(aes(x = 0, y = 0, xend = LD1, yend = LD2), arrow = arrow()) +
      geom_text_repel(aes(label = Variable)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      labs(title = var_title, x = x_label, y = y_label) +
      theme_minimal()
    
    if (!is.null(input$afdVarCenterAxes) && input$afdVarCenterAxes) {
      coords <- var_contrib[, c("LD1", "LD2")]
      max_range <- max(abs(range(coords, na.rm = TRUE)))
      p_var <- p_var + xlim(-max_range, max_range) + ylim(-max_range, max_range)
    }
    
    return(p_var)
  }
  
  output$afdIndPlot <- renderPlotly({
    p_ind <- createAfdIndPlot()
    ggplotly(p_ind) %>% layout(showlegend = TRUE)
  })
  
  output$afdVarPlot <- renderPlotly({
    p_var <- createAfdVarPlot()
    ggplotly(p_var) %>% layout(showlegend = TRUE)
  })
  
  output$afdSummary <- renderPrint({
    req(afdResultReactive())
    afd_res <- afdResultReactive()
    afd_result <- afd_res$model
    afd_predict <- afd_res$predictions
    afd_data <- afd_res$data
    
    cat("=== ANALYSE FACTORIELLE DISCRIMINANTE (AFD) ===\n\n")
    
    cat("1. VARIANCE EXPLIQUEE\n")
    cat("----------------------\n")
    eigenvals <- afd_result$svd^2
    prop_var <- eigenvals / sum(eigenvals) * 100
    can_cor <- sqrt(eigenvals / (1 + eigenvals))
    
    cat("\nCorrelations canoniques:\n")
    print(round(can_cor, 4))
    
    cat("\n2. POIDS DES VARIABLES\n")
    cat("----------------------\n")
    cat("Coefficients des fonctions discriminantes:\n")
    print(round(afd_result$scaling, 4))
    
    X_std <- scale(afd_data[, input$afdVars])
    scores <- as.matrix(X_std) %*% afd_result$scaling
    structure_matrix <- cor(X_std, scores)
    cat("\nMatrice de structure (correlations):\n")
    print(round(structure_matrix, 4))
    
    cat("\nTests F univaries:\n")
    f_tests <- data.frame(Variable = input$afdVars)
    for (var in input$afdVars) {
      aov_result <- aov(as.formula(paste(var, "~", input$afdFactor)), data = afd_data)
      f_stat <- summary(aov_result)[[1]][1, "F value"]
      p_val <- summary(aov_result)[[1]][1, "Pr(>F)"]
      f_tests[f_tests$Variable == var, "F_statistic"] <- round(f_stat, 4)
      f_tests[f_tests$Variable == var, "p_value"] <- round(p_val, 4)
    }
    print(f_tests)
    
    cat("\n3. QUALITE DE CLASSIFICATION\n")
    cat("-----------------------------\n")
    confusion_matrix <- table(Reel = afd_data[[input$afdFactor]], Predit = afd_predict$class)
    cat("Matrice de confusion:\n")
    print(confusion_matrix)
    
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    cat("\nTaux de classification correcte:", round(accuracy * 100, 2), "%\n")
    
    cat("\nTaux de classification par groupe:\n")
    for (i in 1:nrow(confusion_matrix)) {
      group_acc <- confusion_matrix[i,i] / sum(confusion_matrix[i,])
      cat(rownames(confusion_matrix)[i], ":", round(group_acc * 100, 2), "%\n")
    }
    
    if (!is.null(afd_res$cv_results)) {
      cat("\n4. VALIDATION CROISEE (Leave-One-Out)\n")
      cat("-------------------------------------\n")
      
      cv_confusion <- table(Reel = afd_data[[input$afdFactor]], 
                            Predit = afd_res$cv_results$predictions)
      
      cat("Matrice de confusion (validation croisee):\n")
      print(cv_confusion)
      
      cv_accuracy <- sum(diag(cv_confusion)) / sum(cv_confusion)
      cat("\nTaux de classification en validation croisee:", round(cv_accuracy * 100, 2), "%\n")
    } else {
      cat("\n4. VALIDATION CROISEE\n")
      cat("---------------------\n")
      cat("Validation croisee desactivee.\n")
    }
    
    cat("\n5. CENTROIDES DES GROUPES\n")
    cat("-------------------------\n")
    print(round(afd_result$means, 4))
    
    cat("\n6. PROBABILITES A PRIORI\n")
    cat("------------------------\n")
    print(round(afd_result$prior, 4))
  })
  
  # Telechargement graphique AFD Individus
  output$downloadAfdIndPlot <- downloadHandler(
    filename = function() {
      ext <- input$afdInd_format
      paste0("afd_individus_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      width <- input$afdInd_width
      height <- input$afdInd_height
      dpi <- input$afdInd_dpi
      
      p_ind <- createAfdIndPlot()
      ggsave(file, plot = p_ind, device = input$afdInd_format, 
             width = width, height = height, dpi = dpi, units = "cm")
    }
  )
  
  # Telechargement graphique AFD Variables
  output$downloadAfdVarPlot <- downloadHandler(
    filename = function() {
      ext <- input$afdVar_format
      paste0("afd_variables_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      width <- input$afdVar_width
      height <- input$afdVar_height
      dpi <- input$afdVar_dpi
      
      p_var <- createAfdVarPlot()
      ggsave(file, plot = p_var, device = input$afdVar_format, 
             width = width, height = height, dpi = dpi, units = "cm")
    }
  )
  
  # CORRECTION : Telechargement donnees AFD - Excel avec nom correct
  output$downloadAfdDataXlsx <- downloadHandler(
    filename = function() {
      paste0("afd_resultats_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(afdDataframes())
      dfs <- afdDataframes()
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Coordonnees_individus")
      writeData(wb, "Coordonnees_individus", dfs$ind_coords)
      
      addWorksheet(wb, "Coefficients_discriminants")
      writeData(wb, "Coefficients_discriminants", dfs$coefficients)
      
      addWorksheet(wb, "Matrice_structure")
      writeData(wb, "Matrice_structure", dfs$structure_matrix)
      
      addWorksheet(wb, "Tests_F")
      writeData(wb, "Tests_F", dfs$f_tests)
      
      addWorksheet(wb, "Matrice_confusion")
      writeData(wb, "Matrice_confusion", dfs$confusion_matrix)
      
      addWorksheet(wb, "Taux_classification")
      writeData(wb, "Taux_classification", dfs$classification_rates)
      
      addWorksheet(wb, "Centroides")
      writeData(wb, "Centroides", dfs$centroids)
      
      addWorksheet(wb, "Variance_expliquee")
      writeData(wb, "Variance_expliquee", dfs$variance_explained)
      
      if (!is.null(dfs$cv_confusion)) {
        addWorksheet(wb, "CV_confusion")
        writeData(wb, "CV_confusion", dfs$cv_confusion)
        
        addWorksheet(wb, "CV_taux")
        writeData(wb, "CV_taux", dfs$cv_accuracy)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # CORRECTION : Telechargement donnees AFD - CSV avec nom correct
  output$downloadAfdDataCsv <- downloadHandler(
    filename = function() {
      paste0("afd_resultats_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(afdDataframes())
      dfs <- afdDataframes()
      
      temp_dir <- tempdir()
      csv_files <- c()
      
      write.csv(dfs$ind_coords, file.path(temp_dir, "coordonnees_individus.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "coordonnees_individus.csv")
      
      write.csv(dfs$coefficients, file.path(temp_dir, "coefficients_discriminants.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "coefficients_discriminants.csv")
      
      write.csv(dfs$structure_matrix, file.path(temp_dir, "matrice_structure.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "matrice_structure.csv")
      
      write.csv(dfs$f_tests, file.path(temp_dir, "tests_F.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "tests_F.csv")
      
      write.csv(dfs$confusion_matrix, file.path(temp_dir, "matrice_confusion.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "matrice_confusion.csv")
      
      write.csv(dfs$classification_rates, file.path(temp_dir, "taux_classification.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "taux_classification.csv")
      
      write.csv(dfs$centroids, file.path(temp_dir, "centroides.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "centroides.csv")
      
      write.csv(dfs$variance_explained, file.path(temp_dir, "variance_expliquee.csv"), row.names = FALSE)
      csv_files <- c(csv_files, "variance_expliquee.csv")
      
      if (!is.null(dfs$cv_confusion)) {
        write.csv(dfs$cv_confusion, file.path(temp_dir, "cv_confusion.csv"), row.names = FALSE)
        csv_files <- c(csv_files, "cv_confusion.csv")
        
        write.csv(dfs$cv_accuracy, file.path(temp_dir, "cv_taux.csv"), row.names = FALSE)
        csv_files <- c(csv_files, "cv_taux.csv")
      }
      
      zip(file, file.path(temp_dir, csv_files), flags = "-j")
    }
  )
  # ---- Seuils d'efficacité ----
  
  # Valeurs réactives pour l'analyse des seuils
  threshold_values <- reactiveValues(
    plot_data = NULL,
    current_plot = NULL,
    label_mapping = NULL,
    label_styles = NULL,
    data_prepared = FALSE,
    selected_y_vars = NULL,
    y_colors = NULL,
    auto_update = TRUE,
    legend_label_mapping = NULL,
    legend_label_styles = NULL
  )
  
  # Sélection de la variable X
  output$thresholdXVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("thresholdXVar", "Variable X (Traitements):", 
                choices = all_cols,
                selected = if(length(all_cols) > 0) all_cols[1] else NULL)
  })
  
  # Sélection de la/des variable(s) Y
  output$thresholdYVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    
    if(input$thresholdMultipleY) {
      pickerInput("thresholdYVar", "Variables Y (Efficacité) - Sélection multiple:", 
                  choices = num_cols,
                  selected = if(length(num_cols) > 0) num_cols[1] else NULL,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `selected-text-format` = "count > 2",
                                 `count-selected-text` = "{0} variables sélectionnées"))
    } else {
      selectInput("thresholdYVar", "Variable Y (Efficacité):", 
                  choices = num_cols,
                  selected = if(length(num_cols) > 0) num_cols[1] else NULL)
    }
  })
  
  # Filtrage optionnel
  output$thresholdFilterSelect <- renderUI({
    req(values$filteredData, input$thresholdXVar)
    
    if(is.null(input$thresholdXVar)) return(NULL)
    
    x_data <- values$filteredData[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(x_data)
    } else {
      unique(as.character(x_data))
    }
    
    pickerInput("thresholdFilter", 
                "Exclure des traitements (optionnel):",
                choices = unique_vals,
                multiple = TRUE,
                options = list(`actions-box` = TRUE))
  })
  
  # Éditeur de labels pour la variable X avec options de style
  output$thresholdLevelsEditor <- renderUI({
    req(values$filteredData, input$thresholdXVar)
    
    x_data <- values$filteredData[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(droplevels(x_data))
    } else {
      sort(unique(as.character(x_data)))
    }
    
    if(length(unique_vals) == 0) {
      return(p("Aucune valeur trouvée", style = "color: #999;"))
    }
    
    div(
      actionButton("resetThresholdLabels", "Réinitialiser", 
                   class = "btn-default btn-sm", icon = icon("undo"),
                   style = "margin-bottom: 10px;"),
      
      div(style = if(length(unique_vals) > 10) "max-height: 400px; overflow-y: auto;" else "",
          lapply(seq_along(unique_vals), function(i) {
            lvl <- unique_vals[i]
            input_id <- paste0("thresholdLevel_", make.names(lvl))
            bold_id <- paste0("thresholdLevelBold_", make.names(lvl))
            italic_id <- paste0("thresholdLevelItalic_", make.names(lvl))
            
            div(style = "margin-bottom: 10px; padding: 10px; background-color: #f5f5f5; border-radius: 4px; border-left: 4px solid #3498db;",
                div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
                    span(paste0(i, "."), style = "color: #3498db; font-weight: bold; min-width: 25px; font-size: 14px;"),
                    div(style = "flex: 1;",
                        div(style = "font-size: 11px; color: #666; margin-bottom: 3px; font-style: italic;",
                            paste("Original:", lvl)),
                        textInput(
                          inputId = input_id,
                          label = NULL,
                          value = lvl,
                          placeholder = "Nouvelle étiquette...",
                          width = "100%"
                        )
                    )
                ),
                div(style = "display: flex; gap: 15px; padding-left: 35px; align-items: center;",
                    div(style = "display: flex; align-items: center; gap: 5px;",
                        checkboxInput(bold_id, NULL, value = FALSE, width = "20px"),
                        tags$label(`for` = bold_id, style = "margin: 0; font-weight: bold; cursor: pointer;", "Gras")
                    ),
                    div(style = "display: flex; align-items: center; gap: 5px;",
                        checkboxInput(italic_id, NULL, value = FALSE, width = "20px"),
                        tags$label(`for` = italic_id, style = "margin: 0; font-style: italic; cursor: pointer;", "Italique")
                    )
                )
            )
          })
      )
    )
  })
  
  # Éditeur de labels pour la légende (Variables Y multiples)
  output$thresholdLegendEditor <- renderUI({
    req(input$thresholdMultipleY, input$thresholdYVar)
    req(length(input$thresholdYVar) > 1)
    
    y_vars <- input$thresholdYVar
    
    div(
      actionButton("resetThresholdLegendLabels", "Réinitialiser", 
                   class = "btn-default btn-sm", icon = icon("undo"),
                   style = "margin-bottom: 10px;"),
      
      div(style = if(length(y_vars) > 10) "max-height: 400px; overflow-y: auto;" else "",
          lapply(seq_along(y_vars), function(i) {
            var_name <- y_vars[i]
            input_id <- paste0("thresholdLegendLevel_", make.names(var_name))
            bold_id <- paste0("thresholdLegendLevelBold_", make.names(var_name))
            italic_id <- paste0("thresholdLegendLevelItalic_", make.names(var_name))
            
            div(style = "margin-bottom: 10px; padding: 10px; background-color: #f5f5f5; border-radius: 4px; border-left: 4px solid #9b59b6;",
                div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
                    span(paste0(i, "."), style = "color: #9b59b6; font-weight: bold; min-width: 25px; font-size: 14px;"),
                    div(style = "flex: 1;",
                        div(style = "font-size: 11px; color: #666; margin-bottom: 3px; font-style: italic;",
                            paste("Original:", var_name)),
                        textInput(
                          inputId = input_id,
                          label = NULL,
                          value = var_name,
                          placeholder = "Nouvelle étiquette...",
                          width = "100%"
                        )
                    )
                ),
                div(style = "display: flex; gap: 15px; padding-left: 35px; align-items: center;",
                    div(style = "display: flex; align-items: center; gap: 5px;",
                        checkboxInput(bold_id, NULL, value = FALSE, width = "20px"),
                        tags$label(`for` = bold_id, style = "margin: 0; font-weight: bold; cursor: pointer;", "Gras")
                    ),
                    div(style = "display: flex; align-items: center; gap: 5px;",
                        checkboxInput(italic_id, NULL, value = FALSE, width = "20px"),
                        tags$label(`for` = italic_id, style = "margin: 0; font-style: italic; cursor: pointer;", "Italique")
                    )
                )
            )
          })
      )
    )
  })
  
  # Réinitialiser les labels X
  observeEvent(input$resetThresholdLabels, {
    req(values$filteredData, input$thresholdXVar)
    
    x_data <- values$filteredData[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(droplevels(x_data))
    } else {
      sort(unique(as.character(x_data)))
    }
    
    for(lvl in unique_vals) {
      updateTextInput(session, paste0("thresholdLevel_", make.names(lvl)), value = lvl)
      updateCheckboxInput(session, paste0("thresholdLevelBold_", make.names(lvl)), value = FALSE)
      updateCheckboxInput(session, paste0("thresholdLevelItalic_", make.names(lvl)), value = FALSE)
    }
    
    showNotification("Étiquettes X réinitialisées", type = "message", duration = 2)
  })
  
  # Réinitialiser les labels de légende
  observeEvent(input$resetThresholdLegendLabels, {
    req(input$thresholdMultipleY, input$thresholdYVar)
    req(length(input$thresholdYVar) > 1)
    
    y_vars <- input$thresholdYVar
    
    for(var_name in y_vars) {
      updateTextInput(session, paste0("thresholdLegendLevel_", make.names(var_name)), value = var_name)
      updateCheckboxInput(session, paste0("thresholdLegendLevelBold_", make.names(var_name)), value = FALSE)
      updateCheckboxInput(session, paste0("thresholdLegendLevelItalic_", make.names(var_name)), value = FALSE)
    }
    
    showNotification("Étiquettes de légende réinitialisées", type = "message", duration = 2)
  })
  
  # Color pickers personnalisés pour les traitements (une seule variable Y)
  output$thresholdColorPickers <- renderUI({
    req(values$filteredData, input$thresholdXVar)
    req(!input$thresholdMultipleY || length(input$thresholdYVar) == 1)
    
    x_data <- values$filteredData[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(droplevels(x_data))
    } else {
      sort(unique(as.character(x_data)))
    }
    
    # Appliquer le filtre
    if(!is.null(input$thresholdFilter) && length(input$thresholdFilter) > 0) {
      unique_vals <- unique_vals[!unique_vals %in% input$thresholdFilter]
    }
    
    default_colors <- scales::hue_pal()(length(unique_vals))
    
    div(
      lapply(seq_along(unique_vals), function(i) {
        colourInput(paste0("thresholdCustomColor_", i), 
                    paste("Couleur", unique_vals[i], ":"),
                    value = default_colors[i],
                    showColour = "background")
      })
    )
  })
  
  # Préparation automatique des données
  observe({
    req(values$filteredData, input$thresholdXVar, input$thresholdYVar)
    
    tryCatch({
      if(length(input$thresholdYVar) == 1) {
        plot_data <- values$filteredData[, c(input$thresholdXVar, input$thresholdYVar)]
        colnames(plot_data) <- c("Treatment", "Efficacy")
        plot_data <- na.omit(plot_data)
        
        if(!is.null(input$thresholdFilter) && length(input$thresholdFilter) > 0) {
          plot_data <- plot_data[!plot_data$Treatment %in% input$thresholdFilter, ]
        }
        
      } else {
        plot_data <- values$filteredData[, c(input$thresholdXVar, input$thresholdYVar)]
        colnames(plot_data)[1] <- "Treatment"
        
        plot_data <- tidyr::pivot_longer(plot_data, 
                                         cols = -Treatment,
                                         names_to = "Variable",
                                         values_to = "Efficacy")
        plot_data <- na.omit(plot_data)
        
        if(!is.null(input$thresholdFilter) && length(input$thresholdFilter) > 0) {
          plot_data <- plot_data[!plot_data$Treatment %in% input$thresholdFilter, ]
        }
        
        threshold_values$y_colors <- NULL
      }
      
      if(nrow(plot_data) == 0) {
        threshold_values$data_prepared <- FALSE
        return()
      }
      
      plot_data$Treatment <- as.character(plot_data$Treatment)
      
      # Appliquer les labels X personnalisés
      unique_treatments <- sort(unique(plot_data$Treatment))
      label_mapping <- sapply(unique_treatments, function(lvl) {
        new_label <- input[[paste0("thresholdLevel_", make.names(lvl))]]
        if(is.null(new_label) || new_label == "") lvl else new_label
      })
      
      label_styles <- sapply(unique_treatments, function(lvl) {
        is_bold <- input[[paste0("thresholdLevelBold_", make.names(lvl))]]
        is_italic <- input[[paste0("thresholdLevelItalic_", make.names(lvl))]]
        
        if(is.null(is_bold)) is_bold <- FALSE
        if(is.null(is_italic)) is_italic <- FALSE
        
        if(is_bold && is_italic) "bolditalic"
        else if(is_bold) "bold"
        else if(is_italic) "italic"
        else "plain"
      })
      
      if(any(duplicated(label_mapping))) {
        threshold_values$data_prepared <- FALSE
        return()
      }
      
      plot_data$Treatment <- factor(plot_data$Treatment, 
                                    levels = unique_treatments,
                                    labels = label_mapping)
      
      # Appliquer les labels de légende personnalisés pour Y multiples
      if(length(input$thresholdYVar) > 1) {
        unique_vars <- unique(plot_data$Variable)
        
        legend_label_mapping <- sapply(unique_vars, function(var_name) {
          new_label <- input[[paste0("thresholdLegendLevel_", make.names(var_name))]]
          if(is.null(new_label) || new_label == "") var_name else new_label
        })
        
        legend_label_styles <- sapply(unique_vars, function(var_name) {
          is_bold <- input[[paste0("thresholdLegendLevelBold_", make.names(var_name))]]
          is_italic <- input[[paste0("thresholdLegendLevelItalic_", make.names(var_name))]]
          
          if(is.null(is_bold)) is_bold <- FALSE
          if(is.null(is_italic)) is_italic <- FALSE
          
          if(is_bold && is_italic) "bolditalic"
          else if(is_bold) "bold"
          else if(is_italic) "italic"
          else "plain"
        })
        
        plot_data$Variable <- factor(plot_data$Variable,
                                     levels = unique_vars,
                                     labels = legend_label_mapping)
        
        threshold_values$legend_label_mapping <- legend_label_mapping
        threshold_values$legend_label_styles <- legend_label_styles
      }
      
      threshold_values$plot_data <- plot_data
      threshold_values$label_mapping <- label_mapping
      threshold_values$label_styles <- label_styles
      threshold_values$selected_y_vars <- input$thresholdYVar
      threshold_values$data_prepared <- TRUE
      
    }, error = function(e) {
      threshold_values$data_prepared <- FALSE
    })
  })
  
  # Génération réactive du graphique
  threshold_plot_reactive <- reactive({
    req(threshold_values$data_prepared)
    req(threshold_values$plot_data)
    
    # Dépendances pour mise à jour automatique
    input$thresholdValue
    input$thresholdColor
    input$thresholdLineWidth
    input$thresholdLineType
    input$thresholdPlotTitle
    input$thresholdXLabel
    input$thresholdYLabel
    input$thresholdUseColor
    input$thresholdBarColor
    input$thresholdPalette
    input$thresholdSingleBarColor
    input$thresholdXLabelBold
    input$thresholdXLabelItalic
    input$thresholdYLabelBold
    input$thresholdYLabelItalic
    input$thresholdBlackAxes
    input$thresholdShowAxisLines
    input$thresholdShowTicks
    input$thresholdShowGrid
    input$thresholdRotateLabels
    input$thresholdTitleSize
    input$thresholdAxisTitleSize
    input$thresholdAxisTextSize
    input$thresholdLegendSize
    input$thresholdYMin
    input$thresholdYMax
    input$thresholdShowLegend
    input$thresholdLegendPosition
    input$thresholdLegendTitle
    input$thresholdLegendBold
    input$thresholdLegendItalic
    input$thresholdBarWidth
    input$thresholdBarSpacing
    input$thresholdBarPosition
    
    lapply(names(threshold_values$label_mapping), function(lvl) {
      input[[paste0("thresholdLevel_", make.names(lvl))]]
      input[[paste0("thresholdLevelBold_", make.names(lvl))]]
      input[[paste0("thresholdLevelItalic_", make.names(lvl))]]
    })
    
    if(length(threshold_values$selected_y_vars) > 1) {
      lapply(threshold_values$selected_y_vars, function(var_name) {
        input[[paste0("thresholdLegendLevel_", make.names(var_name))]]
        input[[paste0("thresholdLegendLevelBold_", make.names(var_name))]]
        input[[paste0("thresholdLegendLevelItalic_", make.names(var_name))]]
      })
    }
    
    if(!is.null(input$thresholdBarColor) && input$thresholdBarColor == "custom") {
      lapply(seq_along(levels(threshold_values$plot_data$Treatment)), function(i) {
        input[[paste0("thresholdCustomColor_", i)]]
      })
    }
    
    plot_data <- threshold_values$plot_data
    label_styles <- threshold_values$label_styles
    is_multiple_y <- length(threshold_values$selected_y_vars) > 1
    
    tryCatch({
      if(is_multiple_y) {
        p <- ggplot(plot_data, aes(x = Treatment, y = Efficacy, fill = Variable))
        
        # Calcul de la largeur avec espacement
        bar_width <- (input$thresholdBarWidth %||% 0.8)
        dodge_width <- bar_width + (input$thresholdBarSpacing %||% 0.1)
        
        position <- if(!is.null(input$thresholdBarPosition) && input$thresholdBarPosition == "stack") {
          "stack"
        } else {
          position_dodge(width = dodge_width)
        }
        
        p <- p + geom_col(position = position, 
                          width = bar_width,
                          alpha = 0.8)
        
        p <- p + labs(fill = input$thresholdLegendTitle %||% "Variables")
        
      } else {
        p <- ggplot(plot_data, aes(x = Treatment, y = Efficacy))
        
        bar_width <- input$thresholdBarWidth %||% 0.8
        
        if(input$thresholdUseColor) {
          if(input$thresholdBarColor == "ggplot") {
            p <- p + geom_col(aes(fill = Treatment), width = bar_width, alpha = 0.8)
          } else if(input$thresholdBarColor == "palette") {
            p <- p + geom_col(aes(fill = Treatment), width = bar_width, alpha = 0.8) +
              scale_fill_brewer(palette = input$thresholdPalette %||% "Set1",
                                name = input$thresholdLegendTitle %||% "Traitements")
          } else if(input$thresholdBarColor == "custom") {
            custom_colors <- sapply(seq_along(levels(plot_data$Treatment)), function(i) {
              color_input <- input[[paste0("thresholdCustomColor_", i)]]
              if(is.null(color_input)) scales::hue_pal()(length(levels(plot_data$Treatment)))[i] else color_input
            })
            p <- p + geom_col(aes(fill = Treatment), width = bar_width, alpha = 0.8) +
              scale_fill_manual(values = custom_colors,
                                name = input$thresholdLegendTitle %||% "Traitements")
          } else if(input$thresholdBarColor == "black") {
            p <- p + geom_col(fill = "#000000", width = bar_width, alpha = 0.8)
          } else if(input$thresholdBarColor == "single") {
            p <- p + geom_col(fill = input$thresholdSingleBarColor %||% "#3498db", 
                              width = bar_width, alpha = 0.8)
          }
        } else {
          p <- p + geom_col(fill = "#3498db", width = bar_width, alpha = 0.8)
        }
      }
      
      p <- p + geom_hline(yintercept = input$thresholdValue %||% 80, 
                          color = input$thresholdColor %||% "#e74c3c",
                          linewidth = input$thresholdLineWidth %||% 1.5,
                          linetype = input$thresholdLineType %||% "solid")
      
      p <- p + annotate("text", 
                        x = length(levels(plot_data$Treatment)) * 0.9,
                        y = (input$thresholdValue %||% 80) + 5,
                        label = paste("Seuil:", input$thresholdValue %||% 80, "%"),
                        color = input$thresholdColor %||% "#e74c3c",
                        fontface = "bold",
                        size = 4)
      
      plot_title <- if(!is.null(input$thresholdPlotTitle) && input$thresholdPlotTitle != "") {
        input$thresholdPlotTitle
      } else {
        "Analyse des seuils d'efficacité"
      }
      
      x_label <- if(!is.null(input$thresholdXLabel) && input$thresholdXLabel != "") {
        input$thresholdXLabel
      } else {
        "Traitements"
      }
      
      y_label <- if(!is.null(input$thresholdYLabel) && input$thresholdYLabel != "") {
        input$thresholdYLabel
      } else {
        "Seuil d'efficacité (%)"
      }
      
      x_label_face <- if(input$thresholdXLabelBold && input$thresholdXLabelItalic) {
        "bold.italic"
      } else if(input$thresholdXLabelBold) {
        "bold"
      } else if(input$thresholdXLabelItalic) {
        "italic"
      } else {
        "plain"
      }
      
      y_label_face <- if(input$thresholdYLabelBold && input$thresholdYLabelItalic) {
        "bold.italic"
      } else if(input$thresholdYLabelBold) {
        "bold"
      } else if(input$thresholdYLabelItalic) {
        "italic"
      } else {
        "plain"
      }
      
      legend_title_face <- if(input$thresholdLegendBold && input$thresholdLegendItalic) {
        "bold.italic"
      } else if(input$thresholdLegendBold) {
        "bold"
      } else if(input$thresholdLegendItalic) {
        "italic"
      } else {
        "plain"
      }
      
      axis_color <- if(!is.null(input$thresholdBlackAxes) && input$thresholdBlackAxes) {
        "black"
      } else {
        "grey50"
      }
      
      legend_position <- if(!is.null(input$thresholdShowLegend) && input$thresholdShowLegend) {
        pos <- input$thresholdLegendPosition %||% "bottom"
        if(pos == "top_right") c(0.95, 0.95)
        else if(pos == "top_left") c(0.05, 0.95)
        else if(pos == "bottom_right") c(0.95, 0.05)
        else if(pos == "bottom_left") c(0.05, 0.05)
        else pos
      } else {
        "none"
      }
      
      legend_justification <- if(!is.null(input$thresholdLegendPosition)) {
        pos <- input$thresholdLegendPosition
        if(pos == "top_right") c(1, 1)
        else if(pos == "top_left") c(0, 1)
        else if(pos == "bottom_right") c(1, 0)
        else if(pos == "bottom_left") c(0, 0)
        else "center"
      } else {
        "center"
      }
      
      show_legend <- (is_multiple_y || (input$thresholdUseColor && 
                                          input$thresholdBarColor %in% c("ggplot", "custom", "palette"))) &&
        (!is.character(legend_position) || legend_position != "none")
      
      p <- p + labs(title = plot_title, x = x_label, y = y_label) +
        scale_y_continuous(limits = c(input$thresholdYMin %||% 0, input$thresholdYMax %||% 100)) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = input$thresholdTitleSize %||% 16, 
                                    hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size = input$thresholdAxisTitleSize %||% 14, 
                                      face = x_label_face,
                                      color = axis_color),
          axis.title.y = element_text(size = input$thresholdAxisTitleSize %||% 14, 
                                      face = y_label_face,
                                      color = axis_color),
          axis.text.y = element_text(size = input$thresholdAxisTextSize %||% 12,
                                     color = axis_color),
          axis.text.x = if(input$thresholdRotateLabels) {
            element_text(angle = 45, hjust = 1, color = axis_color, 
                         size = input$thresholdAxisTextSize %||% 12)
          } else {
            element_text(color = axis_color, size = input$thresholdAxisTextSize %||% 12)
          },
          axis.line = if(input$thresholdShowAxisLines) {
            element_line(color = axis_color, linewidth = 0.5)
          } else {
            element_blank()
          },
          axis.ticks = if(input$thresholdShowTicks) {
            element_line(color = axis_color, linewidth = 0.5)
          } else {
            element_blank()
          },
          legend.position = if(show_legend) legend_position else "none",
          legend.justification = if(show_legend && is.numeric(legend_position)) legend_justification else NULL,
          legend.background = if(show_legend && is.numeric(legend_position)) {
            element_rect(fill = "white", color = "grey80", linewidth = 0.5)
          } else {
            element_blank()
          },
          legend.title = element_text(size = input$thresholdLegendSize %||% 10, face = legend_title_face),
          legend.text = element_text(size = input$thresholdLegendSize %||% 10),
          panel.grid.major = if(input$thresholdShowGrid) {
            element_line(color = "grey90")
          } else {
            element_blank()
          },
          panel.grid.minor = element_blank(),
          panel.border = if(input$thresholdShowAxisLines) {
            element_rect(color = axis_color, fill = NA, linewidth = 0.5)
          } else {
            element_blank()
          }
        )
      
      if(!is.null(label_styles) && length(label_styles) > 0) {
        treatment_levels <- levels(plot_data$Treatment)
        
        styled_labels <- sapply(seq_along(treatment_levels), function(i) {
          current_label <- as.character(treatment_levels[i])
          
          original_treatments <- names(threshold_values$label_mapping)
          idx <- which(threshold_values$label_mapping == current_label)
          
          if(length(idx) > 0) {
            style <- label_styles[idx[1]]
            
            if(style == "bold") {
              return(bquote(bold(.(current_label))))
            } else if(style == "italic") {
              return(bquote(italic(.(current_label))))
            } else if(style == "bolditalic") {
              return(bquote(bolditalic(.(current_label))))
            }
          }
          return(current_label)
        })
        
        p <- p + scale_x_discrete(labels = styled_labels)
      }
      
      threshold_values$current_plot <- p
      
      return(p)
      
    }, error = function(e) {
      showNotification(paste("Erreur lors de la mise à jour:", e$message), type = "error", duration = 5)
      return(NULL)
    })
  })
  
  # Affichage du graphique avec mise à jour automatique
  output$thresholdPlot <- renderPlotly({
    p <- threshold_plot_reactive()
    req(p)
    
    is_multiple_y <- length(threshold_values$selected_y_vars) > 1
    show_legend <- if(!is.null(input$thresholdShowLegend) && input$thresholdShowLegend) {
      is_multiple_y || (input$thresholdUseColor && input$thresholdBarColor %in% c("ggplot", "custom", "palette"))
    } else {
      FALSE
    }
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = show_legend)
  })
  
  # Tableau des données
  output$thresholdDataTable <- renderDT({
    req(threshold_values$plot_data)
    
    datatable(threshold_values$plot_data,
              options = list(
                pageLength = 10, 
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              class = 'cell-border stripe hover',
              caption = tags$caption(
                style = 'caption-side: top; text-align: center; color: #3c8dbc; font-size: 16px; font-weight: bold;',
                'Données utilisées pour le graphique'
              ))
  })
  
  # Téléchargement du graphique avec correction DPI
  output$downloadThresholdPlot <- downloadHandler(
    filename = function() {
      format <- input$thresholdExportFormat %||% "png"
      paste0("seuils_efficacite_", Sys.Date(), ".", format)
    },
    content = function(file) {
      req(threshold_values$current_plot)
      
      # Paramètres d'export
      width_px <- input$thresholdExportWidth %||% 1200
      height_px <- input$thresholdExportHeight %||% 800
      dpi <- input$thresholdExportDPI %||% 300
      
      # CORRECTION: Limiter le DPI max à 1200 pour éviter les problèmes
      if(dpi > 20000) {
        showNotification(
          paste0("⚠ DPI réduit de ", dpi, " à 20000 pour assurer la compatibilité.\n",
                 "Pour des résolutions supérieures, utilisez les formats vectoriels (SVG, PDF, EPS)."), 
          type = "warning", 
          duration = 6
        )
        dpi <- 20000
      }
      
      # Calculer les dimensions en pouces de manière sûre
      width_in <- max(width_px / dpi, 1)
      height_in <- max(height_px / dpi, 1)
      
      # Limiter les dimensions maximales en pouces pour éviter les erreurs
      max_inches <- 200
      if(width_in > max_inches) {
        width_in <- max_inches
        showNotification("⚠ Largeur limitée à 200 pouces", type = "warning", duration = 3)
      }
      if(height_in > max_inches) {
        height_in <- max_inches
        showNotification("⚠ Hauteur limitée à 200 pouces", type = "warning", duration = 3)
      }
      
      format <- input$thresholdExportFormat %||% "png"
      
      # Gérer les différents formats
      tryCatch({
        if(format == "svg") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 device = "svg")
        } else if(format == "pdf") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 device = "pdf")
        } else if(format == "eps") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 device = "eps")
        } else if(format == "tiff") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 dpi = dpi,
                 device = "tiff",
                 compression = "lzw")
        } else if(format == "bmp") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 dpi = dpi,
                 device = "bmp")
        } else if(format == "jpeg") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 dpi = dpi,
                 device = "jpeg",
                 quality = 95)
        } else {
          # PNG par défaut
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 dpi = dpi,
                 device = "png",
                 type = "cairo")
        }
        
        showNotification(
          paste0("Graphique exporté avec succès\n",
                 "Format: ", toupper(format), "\n",
                 "Dimensions: ", round(width_in, 2), "×", round(height_in, 2), " pouces\n",
                 "Résolution: ", dpi, " DPI"), 
          type = "message", 
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste0("✗ Erreur lors de l'export: ", e$message,
                 "\n\nConseils:",
                 "\n• Réduisez les dimensions ou le DPI",
                 "\n• Utilisez un format vectoriel (SVG, PDF) pour haute résolution",
                 "\n• Maximum recommandé: 5000×5000 px à 600 DPI"), 
          type = "error", 
          duration = 10
        )
      })
    }
  )
  
  # Calcul de l'estimation de la taille du fichier
  output$exportSizeEstimate <- renderText({
    width <- input$thresholdExportWidth %||% 1200
    height <- input$thresholdExportHeight %||% 800
    dpi <- input$thresholdExportDPI %||% 300
    format <- input$thresholdExportFormat %||% "png"
    
    # Limiter DPI affiché
    dpi_display <- min(dpi, 20000)
    warning_text <- if(dpi > 20000) " (DPI sera limité à 1200)" else ""
    
    width_in <- round(width / dpi_display, 2)
    height_in <- round(height / dpi_display, 2)
    
    pixels <- width * height
    size_mb <- if(format %in% c("png", "tiff", "bmp")) {
      (pixels * 3) / (1024 * 1024)
    } else if(format == "jpeg") {
      (pixels * 0.3) / (1024 * 1024)
    } else {
      0.5
    }
    
    paste0(width, "×", height, " pixels = ", width_in, "×", height_in, " pouces à ", dpi_display, " DPI",
           warning_text, " | Taille estimée : ", round(size_mb, 2), " MB")
  })
  
  # Téléchargement des données
  output$downloadThresholdData <- downloadHandler(
    filename = function() {
      paste0("donnees_seuils_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(threshold_values$plot_data)
      
      wb <- openxlsx::createWorkbook()
      
      # Ajouter les données
      openxlsx::addWorksheet(wb, "Données")
      openxlsx::writeData(wb, "Données", threshold_values$plot_data)
      
      # Style pour l'en-tête
      headerStyle <- openxlsx::createStyle(
        fontSize = 12,
        fontColour = "#FFFFFF",
        halign = "center",
        fgFill = "#3c8dbc",
        border = "TopBottomLeftRight",
        borderColour = "#000000",
        textDecoration = "bold"
      )
      
      openxlsx::addStyle(wb, "Données", headerStyle, rows = 1, cols = 1:ncol(threshold_values$plot_data), gridExpand = TRUE)
      
      # Ajouter les paramètres
      y_vars_text <- if(length(threshold_values$selected_y_vars) > 1) {
        paste(threshold_values$selected_y_vars, collapse = ", ")
      } else {
        threshold_values$selected_y_vars
      }
      
      params <- data.frame(
        Paramètre = c("Seuil d'efficacité (%)", 
                      "Variable X", 
                      "Variable(s) Y",
                      "Date d'export",
                      "Nombre de traitements",
                      "Limites Y",
                      "Format d'export",
                      "Dimensions (pixels)",
                      "Résolution (DPI)",
                      "Largeur barres",
                      "Espacement barres"),
        Valeur = c(input$thresholdValue %||% 80, 
                   input$thresholdXVar, 
                   y_vars_text,
                   as.character(Sys.Date()),
                   length(unique(threshold_values$plot_data$Treatment)),
                   paste(input$thresholdYMin %||% 0, "-", input$thresholdYMax %||% 100),
                   input$thresholdExportFormat %||% "png",
                   paste(input$thresholdExportWidth %||% 1200, "×", input$thresholdExportHeight %||% 800),
                   input$thresholdExportDPI %||% 300,
                   input$thresholdBarWidth %||% 0.8,
                   input$thresholdBarSpacing %||% 0.1)
      )
      
      openxlsx::addWorksheet(wb, "Paramètres")
      openxlsx::writeData(wb, "Paramètres", params)
      openxlsx::addStyle(wb, "Paramètres", headerStyle, rows = 1, cols = 1:2, gridExpand = TRUE)
      
      # Ajouter le mapping des labels X
      if(!is.null(threshold_values$label_mapping)) {
        label_info <- data.frame(
          Label_Original = names(threshold_values$label_mapping),
          Label_Personnalisé = as.character(threshold_values$label_mapping),
          Style = if(!is.null(threshold_values$label_styles)) {
            threshold_values$label_styles
          } else {
            rep("plain", length(threshold_values$label_mapping))
          }
        )
        
        openxlsx::addWorksheet(wb, "Labels X")
        openxlsx::writeData(wb, "Labels X", label_info)
        openxlsx::addStyle(wb, "Labels X", headerStyle, rows = 1, cols = 1:3, gridExpand = TRUE)
      }
      
      # Ajouter le mapping des labels de légende (Y multiples)
      if(!is.null(threshold_values$legend_label_mapping)) {
        legend_info <- data.frame(
          Variable_Originale = names(threshold_values$legend_label_mapping),
          Label_Légende = as.character(threshold_values$legend_label_mapping),
          Style = if(!is.null(threshold_values$legend_label_styles)) {
            threshold_values$legend_label_styles
          } else {
            rep("plain", length(threshold_values$legend_label_mapping))
          }
        )
        
        openxlsx::addWorksheet(wb, "Labels Légende")
        openxlsx::writeData(wb, "Labels Légende", legend_info)
        openxlsx::addStyle(wb, "Labels Légende", headerStyle, rows = 1, cols = 1:3, gridExpand = TRUE)
      }
      
      # Ajuster les largeurs de colonnes
      openxlsx::setColWidths(wb, "Données", cols = 1:ncol(threshold_values$plot_data), widths = "auto")
      openxlsx::setColWidths(wb, "Paramètres", cols = 1:2, widths = c(25, 30))
      if(!is.null(threshold_values$label_mapping)) {
        openxlsx::setColWidths(wb, "Labels X", cols = 1:3, widths = c(20, 25, 15))
      }
      if(!is.null(threshold_values$legend_label_mapping)) {
        openxlsx::setColWidths(wb, "Labels Légende", cols = 1:3, widths = c(20, 25, 15))
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      
      showNotification("Données exportées avec succès!", type = "message", duration = 3)
    }
  )
  # ---- Rapport ----
  observeEvent(input$generateReport, {
    showNotification("Génération du rapport en cours...", type = "message")
    
    report_path <- tempfile(fileext = ".Rmd")
    
    writeLines(c(
      "---",
      paste0("title: \"", input$reportTitle, "\""),
      paste0("author: \"", input$reportAuthor, "\""),
      paste0("date: \"", Sys.Date(), "\""),
      paste0("output: ", input$reportFormat),
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
      "```",
      "",
      "# Analyse Statistique des Données",
      "",
      "## Résumé des données",
      "```{r data-summary}",
      "summary(values$filteredData)",
      "```",
      "",
      "## Analyses descriptives",
      "```{r descriptive}",
      "if (!is.null(values$descStats)) {",
      "  knitr::kable(values$descStats, caption = \"Statistiques descriptives\")",
      "}",
      "```",
      "",
      "## Tests statistiques",
      "```{r tests}",
      "if (!is.null(values$testResultsDF)) {",
      "  knitr::kable(values$testResultsDF, caption = \"Résultats des tests\")",
      "}",
      "```",
      "",
      "## Visualisations",
      "```{r plots, fig.cap=\"Visualisations des résultats\"}",
      "if (!is.null(values$currentPlot)) {",
      "  print(values$currentPlot)",
      "}",
      "```"
    ), report_path)
    
    output_file <- rmarkdown::render(report_path, switch(
      input$reportFormat,
      "html_document" = rmarkdown::html_document(),
      "pdf_document" = rmarkdown::pdf_document(),
      "word_document" = rmarkdown::word_document()
    ))
    
    output$reportPreview <- renderUI({
      if (input$reportFormat == "html_document") {
        includeHTML(output_file)
      } else {
        tags$iframe(src = output_file, width = "100%", height = "600px")
      }
    })
    
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste0("rapport_", Sys.Date(), switch(
          input$reportFormat,
          "html_document" = ".html",
          "pdf_document" = ".pdf",
          "word_document" = ".docx"
        ))
      },
      content = function(file) {
        file.copy(output_file, file)
      }
    )
    
    showNotification("Rapport généré avec succès!", type = "message")
  })
}


shinyApp(ui, server)