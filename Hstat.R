################################################################################
#
#             Application d'analyse de données
#
################################################################################

#Sys.setlocale("LC_ALL", "French")
# Configuration de l'encodage UTF-8
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
# 
# Ce Bloc contient les fonctions utilitaires, les interprétations 
# et les helpers pour l'application.
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
  "car", "agricolae","forcats", "bslib", "factoextra",  "FactoMineR","questionr",
  "MASS", "cluster", "GGally", "psych", "nortest", "lmtest", "multcomp","FSA",
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
      "))
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
              fluidRow(
                box(title = "Sélection des variables", status = "primary", width = 4, solidHeader = TRUE,
                    # Variables de base
                    uiOutput("vizXVarSelect"),
                    
                    # Type de variable X
                    selectInput("xVarType", "Type de la variable X:",
                                choices = c("Auto (détection)" = "auto",
                                            "Date" = "date",
                                            "Catégorielle" = "categorical",
                                            "Texte" = "text",
                                            "Facteur" = "factor",
                                            "Numérique" = "numeric"),
                                selected = "auto"),
                    helpText(icon("info-circle"), "Choisissez le type de la variable X. 'Auto' détecte le type automatiquement."),
                    
                    # Éditeur de niveaux pour X
                    conditionalPanel(
                      condition = "input.xVarType == 'factor' || input.xVarType == 'categorical' || input.xVarType == 'text' || (input.xVarType == 'auto' && output.detectedXType)",
                      div(class = "well", style = "background-color: #f5f5f5; border-left: 4px solid #ff9800; padding: 15px; border-radius: 5px; margin-top: 10px;",
                          h5("Personnalisation des niveaux de la variable X", style = "color: #ff9800; font-weight: bold; margin-top: 0;"),
                          uiOutput("xLevelsEditor"),
                          helpText(icon("lightbulb"), "Modifiez les étiquettes pour améliorer la lisibilité. Chaque étiquette doit être unique et non vide.")
                      )
                    ),
                    
                    # Éditeur d'ordre pour X catégoriel (seasonal_evolution)
                    conditionalPanel(
                      condition = "input.vizType == 'seasonal_evolution' && (input.xVarType == 'factor' || input.xVarType == 'categorical' || input.xVarType == 'text' || (input.xVarType == 'auto'))",
                      div(class = "well", 
                          style = "background-color: #f0fff0; border-left: 4px solid #28a745; padding: 15px; border-radius: 5px; margin-top: 10px;",
                          h5(icon("sort"), " Ordre de la variable X", 
                             style = "color: #28a745; font-weight: bold; margin-top: 0;"),
                          uiOutput("xOrderEditor"),
                          helpText(icon("lightbulb"), 
                                   "Définissez l'ordre d'apparition des catégories sur l'axe X pour refléter votre logique d'analyse (ex: T1+3, T1+7, T1+13, T2+3...)")
                      )
                    ),
                    
                    # Options pour les dates
                    conditionalPanel(
                      condition = "input.xVarType == 'date'",
                      div(style = "margin-top: 10px;",
                          selectInput("xDateFormat", "Format de conversion des dates:",
                                      choices = c("AAAA-MM-JJ" = "%Y-%m-%d",
                                                  "JJ/MM/AAAA" = "%d/%m/%Y",
                                                  "MM/JJ/AAAA" = "%m/%d/%Y",
                                                  "AAAA/MM/JJ" = "%Y/%m/%d",
                                                  "JJ-Mois-AAAA" = "%d-%b-%Y"),
                                      selected = "%Y-%m-%d"),
                          helpText(icon("calendar"), "Spécifiez le format si la variable X est une chaîne à convertir en date.")
                      )
                    ),
                    
                    # Sélection Y améliorée
                    div(style = "margin-top: 15px; margin-bottom: 15px;",
                        uiOutput("vizYVarSelect")
                    ),
                    
                    # Variables Couleur et Facette
                    uiOutput("vizColorVarSelect"),
                    uiOutput("vizFacetVarSelect"),
                    
                    # Avertissement pour facetting
                    conditionalPanel(
                      condition = "input.vizFacetVar != 'Aucun' && input.vizFacetVar != null",
                      div(style = "margin: 10px 0; padding: 8px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 4px; font-size: 12px;",
                          icon("exclamation-triangle", style = "color: #ffc107;"),
                          " Assurez-vous que la variable de facetting a peu de catégories (2-10) et pas de valeurs manquantes."
                      )
                    ),
                    
                    # Type de visualisation avec badges informatifs
                    div(style = "margin-top: 15px;",
                        selectInput("vizType", "Type de visualisation:",
                                    choices = c("Nuage de points" = "scatter",
                                                "Courbe saisonnière avec lissage" = "seasonal_smooth",
                                                "Courbe évolution saison" = "seasonal_evolution",
                                                "Boxplot" = "box",
                                                "Violon" = "violin",
                                                "Barres" = "bar",
                                                "Lignes" = "line",
                                                "Densité" = "density",
                                                "Histogramme" = "histogram",
                                                "Heatmap" = "heatmap",
                                                "Aires empilées" = "area"),
                                    selected = "scatter"),
                        
                        # Badge de compatibilité multi-Y
                        conditionalPanel(
                          condition = "input.vizType == 'scatter' || input.vizType == 'line' || input.vizType == 'area' || input.vizType == 'bar' || input.vizType == 'seasonal_smooth' || input.vizType == 'seasonal_evolution'",
                          div(style = "margin-top: 5px; padding: 4px 8px; background-color: #d4edda; border-radius: 4px; font-size: 11px; display: inline-block;",
                              icon("layer-group", style = "color: #28a745;"),
                              span(style = "color: #155724; font-weight: bold;", " Multi-séries compatible")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.vizType == 'box' || input.vizType == 'violin' || input.vizType == 'density' || input.vizType == 'histogram' || input.vizType == 'heatmap'",
                          div(style = "margin-top: 5px; padding: 4px 8px; background-color: #fff3cd; border-radius: 4px; font-size: 11px; display: inline-block;",
                              icon("exclamation-circle", style = "color: #856404;"),
                              span(style = "color: #856404; font-weight: bold;", " Une seule variable Y")
                          )
                        )
                    ),
                    
                    # Mode d'agrégation
                    div(style = "margin-top: 20px;",
                        checkboxInput("useAggregation", "Activer l'agrégation", value = FALSE),
                        conditionalPanel(
                          condition = "input.useAggregation",
                          div(style = "margin-left: 15px; padding: 12px; border-left: 3px solid #007bff; background-color: #f8f9fa; border-radius: 4px;",
                              selectInput("aggFunction", "Fonction d'agrégation:",
                                          choices = c("Moyenne" = "mean",
                                                      "Médiane" = "median",
                                                      "Somme" = "sum",
                                                      "Comptage" = "count",
                                                      "Minimum" = "min",
                                                      "Maximum" = "max",
                                                      "Écart-type" = "sd"),
                                          selected = "mean"),
                              uiOutput("groupVarsSelect"),
                              checkboxInput("showAggInfo", "Afficher les détails de l'agrégation", value = TRUE),
                              helpText(icon("calculator"), "Résumez vos données par groupes. Le comptage est idéal pour les analyses catégoriques.")
                          )
                        )
                    ),
                    
                    # Options pour seasonal_evolution
                    conditionalPanel(
                      condition = "input.vizType == 'seasonal_evolution'",
                      div(class = "well", 
                          style = "background-color: #f0fff0; border-left: 4px solid #28a745; padding: 15px; border-radius: 5px; margin-top: 15px;",
                          h5("Options d'évolution temporelle", 
                             style = "color: #28a745; font-weight: bold; margin-top: 0;"),
                          
                          # Infos sur le type de variable X
                          conditionalPanel(
                            condition = "input.xVarType == 'factor' || input.xVarType == 'categorical' || input.xVarType == 'text'",
                            div(style = "margin-bottom: 15px; padding: 10px; background-color: #d4edda; border-radius: 4px;",
                                icon("check-circle", style = "color: #28a745;"),
                                strong(" Mode catégoriel détecté"),
                                p(style = "margin: 5px 0 0 0; font-size: 12px;",
                                  "L'axe X utilisera vos catégories dans l'ordre défini ci-dessus.")
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.xVarType == 'date'",
                            div(style = "margin-bottom: 15px; padding: 10px; background-color: #d1ecf1; border-radius: 4px;",
                                icon("calendar-alt", style = "color: #0c5460;"),
                                strong(" Mode temporel"),
                                p(style = "margin: 5px 0 0 0; font-size: 12px;",
                                  "L'axe X utilisera les dates dans l'ordre chronologique.")
                            )
                          ),
                          
                          # Style des éléments
                          h6(icon("paint-brush"), " Style", 
                             style = "font-weight: bold; margin-top: 15px; margin-bottom: 10px;"),
                          
                          div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
                              numericInput("evolutionLineWidth", "Épaisseur lignes:", 
                                           value = 1.2, min = 0.25, max = 5, step = 0.25),
                              numericInput("evolutionPointSize", "Taille points:", 
                                           value = 2, min = 0.5, max = 6, step = 0.5)
                          ),
                          
                          selectInput("evolutionLineType", "Type de ligne:",
                                      choices = c("Solide" = "solid", 
                                                  "Pointillé" = "dashed", 
                                                  "Tirets" = "dotted", 
                                                  "Mixte" = "dotdash"),
                                      selected = "solid"),
                          
                          # Format et affichage
                          h6(icon("eye"), " Affichage", 
                             style = "font-weight: bold; margin-top: 15px; margin-bottom: 10px;"),
                          
                          conditionalPanel(
                            condition = "input.xVarType == 'date'",
                            selectInput("evolutionDateFormat", "Format des dates:",
                                        choices = c("Jour-Mois (01-Jan)" = "%d-%b",
                                                    "Mois-Année (Jan-2024)" = "%b-%Y",
                                                    "Date complète (01/01/2024)" = "%d/%m/%Y",
                                                    "Mois abrégé (Jan)" = "%b",
                                                    "Date courte (01/01)" = "%d/%m"),
                                        selected = "%d-%b")
                          ),
                          
                          div(style = "display: flex; gap: 15px; margin: 10px 0;",
                              checkboxInput("evolutionShowGrid", "Grille", value = FALSE),
                              checkboxInput("evolutionShowDataLabels", "Valeurs", value = FALSE)
                          ),
                          
                          conditionalPanel(
                            condition = "input.evolutionShowDataLabels",
                            div(style = "margin-left: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
                                h6("Paramètres des étiquettes:", style = "font-size: 12px; font-weight: bold;"),
                                div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
                                    numericInput("evolutionLabelSize", "Taille:", 
                                                 value = 3, min = 1, max = 6, step = 0.5),
                                    numericInput("evolutionLabelVjust", "Décalage:", 
                                                 value = -0.5, min = -2, max = 2, step = 0.1)
                                )
                            )
                          ),
                          
                          # Expansion des axes
                          h6(icon("expand-arrows-alt"), " Marges des axes", 
                             style = "font-weight: bold; margin-top: 15px; margin-bottom: 10px;"),
                          
                          div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
                              sliderInput("evolutionXExpansion", "Axe X (%):", 
                                          min = 0, max = 20, value = 5, step = 1),
                              sliderInput("evolutionYExpansion", "Axe Y (%):", 
                                          min = 0, max = 20, value = 10, step = 1)
                          ),
                          
                          # Conseil d'agrégation
                          conditionalPanel(
                            condition = "!input.useAggregation",
                            div(style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 4px;",
                                icon("info-circle", style = "color: #856404;"),
                                strong(" Conseil:", style = "color: #856404;"),
                                p(style = "margin: 5px 0 0 0; font-size: 12px; color: #856404;",
                                  "Activez l'agrégation pour calculer les moyennes par groupe (ex: par traitement et période).")
                            )
                          ),
                          
                          helpText(icon("chart-line"), 
                                   "Visualisez l'évolution de vos données dans l'ordre que vous définissez. Parfait pour les comparaisons temporelles ou séquentielles.")
                      )
                    ),
                    
                    # Options pour seasonal_smooth
                    conditionalPanel(
                      condition = "input.vizType == 'seasonal_smooth'",
                      div(class = "well", style = "background-color: #f0f8ff; border-left: 4px solid #1f77b4; padding: 15px; border-radius: 5px; margin-top: 15px;",
                          h5("Options de courbe saisonnière lissée", style = "color: #1f77b4; font-weight: bold; margin-top: 0;"),
                          div(style = "display: flex; gap: 15px; margin-bottom: 10px;",
                              checkboxInput("showPoints", "Points", value = TRUE),
                              checkboxInput("showLines", "Lignes", value = TRUE)
                          ),
                          conditionalPanel(
                            condition = "input.showLines",
                            numericInput("seasonalLineWidth", "Épaisseur des lignes:", value = 1.2, min = 0.25, max = 5, step = 0.25),
                            selectInput("lineType", "Type de ligne:",
                                        choices = c("Solide" = "solid", "Pointillé" = "dashed", "Tirets" = "dotted", "Mixte" = "dotdash"),
                                        selected = "solid")
                          ),
                          conditionalPanel(
                            condition = "input.showPoints",
                            numericInput("seasonalPointSize", "Taille des points:", value = 2, min = 0.5, max = 6, step = 0.5)
                          ),
                          selectInput("dateFormat", "Format des dates (axe X):",
                                      choices = c("Jour-Mois (01-Jan)" = "%d-%b",
                                                  "Mois-Année (Jan-2024)" = "%b-%Y",
                                                  "Date complète (01/01/2024)" = "%d/%m/%Y",
                                                  "Mois abrégé (Jan)" = "%b",
                                                  "Date courte (01/01)" = "%d/%m"),
                                      selected = "%d-%b"),
                          checkboxInput("showGrid", "Afficher la grille", value = TRUE),
                          checkboxInput("showSmoothLine", "Ajouter une ligne de tendance", value = FALSE),
                          conditionalPanel(
                            condition = "input.showSmoothLine",
                            selectInput("smoothMethod", "Méthode de lissage:",
                                        choices = c("LOESS (local)" = "loess", "Linéaire" = "lm", "GAM (spline)" = "gam"),
                                        selected = "loess"),
                            conditionalPanel(
                              condition = "input.smoothMethod == 'loess'",
                              sliderInput("smoothSpan", "Degré de lissage:", min = 0.1, max = 2, value = 0.75, step = 0.05)
                            ),
                            checkboxInput("showConfidenceInterval", "Intervalle de confiance", value = TRUE)
                          ),
                          checkboxInput("showDataLabels", "Afficher les valeurs sur les points", value = FALSE),
                          helpText(icon("chart-area"), "Analysez les tendances cycliques avec lissage.")
                      )
                    ),
                    
                    # Options pour scatter
                    conditionalPanel(
                      condition = "input.vizType == 'scatter'",
                      div(class = "well", style = "background-color: #f8fff8; border-left: 4px solid #28a745; padding: 10px; border-radius: 5px; margin-top: 15px;",
                          checkboxInput("jitterPoints", "Décalage aléatoire (jitter)", value = FALSE),
                          helpText(icon("circle"), "Activez pour mieux visualiser les points superposés.")
                      )
                    ),
                    
                    # Options pour histogrammes
                    conditionalPanel(
                      condition = "input.vizType == 'histogram'",
                      div(class = "well", style = "background-color: #fff8f0; border-left: 4px solid #ff9800; padding: 15px; border-radius: 5px; margin-top: 15px;",
                          h5(icon("chart-bar"), " Paramètres de l'histogramme", style = "color: #ff9800; font-weight: bold; margin-top: 0;"),
                          
                          # Information sur le mode adaptatif
                          div(style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 4px;",
                              icon("info-circle", style = "color: #1976d2;"),
                              strong(" Modes disponibles:", style = "color: #1976d2;"),
                              tags$ul(style = "margin: 5px 0 0 0; font-size: 12px; color: #0d47a1;",
                                      tags$li("X numérique : Distribution avec bins"),
                                      tags$li("X catégoriel + Y numérique : Moyennes ou comptage"),
                                      tags$li("X catégoriel sans Y numérique : Comptage")
                              )
                          ),
                          
                          # Options pour X numérique
                          conditionalPanel(
                            condition = "input.xVarType == 'numeric' || (input.xVarType == 'auto' && typeof(output.detectedXType) == 'undefined')",
                            sliderInput("histBins", "Nombre de barres (bins):", 
                                        min = 5, max = 100, value = 30, step = 5),
                            helpText(icon("info-circle"), "Ajustez le nombre de barres pour la distribution.")
                          ),
                          
                          # Options pour X catégoriel
                          conditionalPanel(
                            condition = "input.xVarType == 'factor' || input.xVarType == 'categorical' || input.xVarType == 'text' || input.xVarType == 'date'",
                            
                            # Nouvelle option : afficher moyennes
                            div(style = "margin-bottom: 15px; padding: 10px; background-color: #e8f5e9; border-radius: 4px;",
                                checkboxInput("histShowMean", "Afficher les moyennes de Y par catégorie", value = FALSE),
                                conditionalPanel(
                                  condition = "input.histShowMean",
                                  div(style = "margin-top: 8px; padding: 8px; background-color: #c8e6c9; border-radius: 3px; font-size: 11px;",
                                      icon("check-circle", style = "color: #2e7d32;"),
                                      span(style = "color: #1b5e20; margin-left: 5px;",
                                           "Mode activé : moyennes de Y par catégorie X")
                                  )
                                ),
                                conditionalPanel(
                                  condition = "!input.histShowMean",
                                  div(style = "margin-top: 8px; padding: 8px; background-color: #fff3e0; border-radius: 3px; font-size: 11px;",
                                      icon("hashtag", style = "color: #e65100;"),
                                      span(style = "color: #e65100; margin-left: 5px;",
                                           "Mode actuel : comptage des occurrences")
                                  )
                                )
                            ),
                            
                            sliderInput("barWidth", "Largeur des barres:", 
                                        min = 0.3, max = 1.5, value = 0.9, step = 0.1),
                            
                            # Position des barres (seulement si variable de couleur définie)
                            conditionalPanel(
                              condition = "input.vizColorVar != 'Aucun' && input.vizColorVar != null",
                              selectInput("barPosition", "Position des barres:",
                                          choices = c("Côte à côte" = "dodge",
                                                      "Empilées" = "stack",
                                                      "Empilées 100%" = "fill"),
                                          selected = "dodge")
                            )
                          ),
                          
                          helpText(icon("lightbulb"), 
                                   "L'histogramme s'adapte automatiquement au type de variable X et à la présence de Y numérique.")
                      )
                    ),
                    
                    # Options pour barres (section séparée)
                    conditionalPanel(
                      condition = "input.vizType == 'bar'",
                      div(class = "well", style = "background-color: #fff8f0; border-left: 4px solid #ff9800; padding: 15px; border-radius: 5px; margin-top: 15px;",
                          h5(icon("chart-bar"), " Paramètres des barres", style = "color: #ff9800; font-weight: bold; margin-top: 0;"),
                          
                          div(style = "margin-bottom: 15px; padding: 10px; background-color: #e8f5e9; border-radius: 4px;",
                              icon("check-circle", style = "color: #388e3c;"),
                              strong(" Types supportés:", style = "color: #388e3c;"),
                              p(style = "margin: 5px 0 0 0; font-size: 12px; color: #1b5e20;",
                                "X : facteurs, dates, textes, catégories", br(),
                                "Y : numériques (continus ou discrets)")
                          ),
                          
                          selectInput("barPosition", "Position des barres:",
                                      choices = c("Côte à côte" = "dodge",
                                                  "Empilées" = "stack",
                                                  "Empilées 100%" = "fill"),
                                      selected = "dodge"),
                          
                          sliderInput("barWidth", "Largeur des barres:", 
                                      min = 0.3, max = 1.5, value = 0.9, step = 0.1),
                          
                          helpText(icon("lightbulb"), "Ajustez la largeur pour une meilleure lisibilité selon le nombre de catégories.")
                      )
                    ),
                    
                    # Bouton de génération
                    div(style = "margin-top: 25px;",
                        actionButton("generateViz", "Générer la visualisation",
                                     class = "btn-primary btn-lg btn-block",
                                     icon = icon("chart-line"),
                                     style = "height: 55px; font-size: 17px; font-weight: bold; box-shadow: 0 4px 6px rgba(0,123,255,0.3);")
                    )
                ),
                
                box(title = "Visualisation", status = "primary", width = 8, solidHeader = TRUE,
                    # Badge indicateur du mode multi-Y
                    conditionalPanel(
                      condition = "output.multiYIndicator",
                      div(style = "margin-bottom: 15px; padding: 12px; background-color: #d4edda; border-left: 4px solid #28a745; border-radius: 4px;",
                          div(style = "display: flex; align-items: center; justify-content: space-between;",
                              div(
                                icon("layer-group", style = "color: #28a745; font-size: 18px; margin-right: 8px;"),
                                strong("Mode Multi-Séries Actif", style = "color: #155724; font-size: 14px;")
                              ),
                              span(id = "multiYBadge", 
                                   style = "padding: 4px 12px; background-color: #28a745; color: white; border-radius: 12px; font-size: 12px; font-weight: bold;")
                          ),
                          div(style = "margin-top: 8px; font-size: 12px; color: #155724;",
                              icon("info-circle"),
                              " Les couleurs distinguent automatiquement chaque variable Y sélectionnée."
                          )
                      )
                    ),
                    
                    # Informations d'agrégation
                    conditionalPanel(
                      condition = "input.useAggregation && input.showAggInfo",
                      div(style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-left: 4px solid #2196f3; border-radius: 4px;",
                          h5(icon("calculator"), " Détails de l'agrégation", style = "margin-top: 0; color: #1976d2;"),
                          verbatimTextOutput("aggregationInfo", placeholder = TRUE)
                      )
                    ),
                    
                    # Informations saisonnières
                    conditionalPanel(
                      condition = "input.vizType == 'seasonal_smooth' || input.vizType == 'seasonal_evolution'",
                      div(style = "margin-bottom: 15px; padding: 10px; background-color: #f0f8ff; border-left: 4px solid #1f77b4; border-radius: 4px;",
                          h5(icon("calendar-alt"), " Analyse saisonnière", style = "margin-top: 0; color: #1f77b4;"),
                          verbatimTextOutput("seasonalInfo", placeholder = TRUE)
                      )
                    ),
                    
                    # Zone de visualisation
                    div(id = "plotContainer", style = "position: relative; min-height: 650px;",
                        conditionalPanel(
                          condition = "$('html').hasClass('shiny-busy')",
                          div(style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000; background: rgba(255,255,255,0.95); padding: 30px; border-radius: 10px; text-align: center; box-shadow: 0 6px 12px rgba(0,0,0,0.15);",
                              icon("spinner", class = "fa-spin", style = "font-size: 36px; color: #007bff;"),
                              h4("Génération en cours...", style = "margin-top: 15px; color: #007bff; font-weight: 500;")
                          )
                        ),
                        plotlyOutput("advancedPlot", height = "650px")
                    ),
                    
                    # Boutons d'action - SIMPLIFIÉS avec modal universel
                    div(style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
                        div(style = "display: flex; justify-content: center; gap: 10px; flex-wrap: wrap;",
                            actionButton("openExportModal", "Exporter le graphique",
                                         class = "btn-success btn-lg", 
                                         icon = icon("download"), 
                                         style = "min-width: 200px; height: 50px; font-size: 16px; font-weight: bold;"),
                            actionButton("resetZoom", "Réinitialiser zoom",
                                         class = "btn-warning", icon = icon("search-minus"), style = "min-width: 120px;")
                        )
                    )
                )
              ),
              
              fluidRow(
                box(title = "Personnalisation avancée", status = "info", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    tabsetPanel(id = "customizationTabs",
                                
                                # Onglet Apparence
                                tabPanel("Apparence", icon = icon("palette"),
                                         br(),
                                         fluidRow(
                                           column(3,
                                                  div(class = "well",
                                                      h5(icon("heading"), " Titres et labels", style = "color: #495057; font-weight: bold; margin-top: 0;"),
                                                      textInput("plotTitle", "Titre du graphique:", value = "", placeholder = "Titre principal"),
                                                      textInput("plotSubtitle", "Sous-titre:", value = "", placeholder = "Sous-titre optionnel"),
                                                      textInput("plotXLab", "Label axe X:", value = "", placeholder = "Auto si vide"),
                                                      textInput("plotYLab", "Label axe Y:", value = "", placeholder = "Auto si vide"),
                                                      textInput("plotCaption", "Légende:", value = "", placeholder = "Source, notes...")
                                                  )
                                           ),
                                           column(3,
                                                  div(class = "well",
                                                      h5(icon("sliders-h"), " Style des éléments", style = "color: #495057; font-weight: bold; margin-top: 0;"),
                                                      sliderInput("plotAlpha", "Transparence:", min = 0.1, max = 1, value = 0.7, step = 0.05),
                                                      sliderInput("plotSize", "Taille des points:", min = 0.5, max = 8, value = 2, step = 0.5),
                                                      sliderInput("lineWidth", "Épaisseur des lignes:", min = 0.25, max = 5, value = 1, step = 0.25),
                                                      helpText(icon("info-circle"), "Ajustez pour optimiser la lisibilité.")
                                                  )
                                           ),
                                           column(3,
                                                  div(class = "well",
                                                      h5(icon("paint-brush"), " Thème et couleurs", style = "color: #495057; font-weight: bold; margin-top: 0;"),
                                                      selectInput("plotTheme", "Thème graphique:",
                                                                  choices = c("Minimal" = "minimal",
                                                                              "Classique" = "classic",
                                                                              "Gris" = "gray",
                                                                              "Sombre" = "dark",
                                                                              "Ligne" = "linedraw",
                                                                              "Vide" = "void"),
                                                                  selected = "minimal"),
                                                      
                                                      # Option de palette (désactivée par défaut pour utiliser ggplot2)
                                                      checkboxInput("useCustomPalette", "Personnaliser la palette de couleurs", value = FALSE),
                                                      
                                                      conditionalPanel(
                                                        condition = "input.useCustomPalette",
                                                        selectInput("plotPalette", "Palette de couleurs:",
                                                                    choices = c("Défaut" = "default",
                                                                                "Set1" = "Set1", "Set2" = "Set2", "Set3" = "Set3",
                                                                                "Pastel1" = "Pastel1", "Pastel2" = "Pastel2",
                                                                                "Paired" = "Paired", "Dark2" = "Dark2",
                                                                                "Viridis" = "viridis", "Plasma" = "plasma", "Inferno" = "inferno"),
                                                                    selected = "default"),
                                                        checkboxInput("customColors", "Couleurs personnalisées", value = FALSE),
                                                        conditionalPanel(
                                                          condition = "input.customColors",
                                                          div(style = "display: flex; gap: 10px; margin-top: 10px;",
                                                              colourInput("color1", "Couleur 1:", value = "#3498db", showColour = "background", width = "100px"),
                                                              colourInput("color2", "Couleur 2:", value = "#e74c3c", showColour = "background", width = "100px")
                                                          )
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(
                                                        condition = "!input.useCustomPalette",
                                                        div(style = "margin-top: 10px; padding: 8px; background-color: #e8f5e9; border-radius: 4px;",
                                                            icon("palette", style = "color: #28a745;"),
                                                            span(style = "color: #155724; font-size: 12px; margin-left: 5px;",
                                                                 "Palette ggplot2 par défaut appliquée automatiquement")
                                                        )
                                                      )
                                                  )
                                           ),
                                           column(3,
                                                  div(class = "well",
                                                      h5(icon("expand-arrows-alt"), " Dimensions et axes", style = "color: #495057; font-weight: bold; margin-top: 0;"),
                                                      numericInput("plotWidthInteractive", "Largeur (px):", value = 1000, min = 300, max = 3000, step = 50),
                                                      numericInput("plotHeightInteractive", "Hauteur (px):", value = 650, min = 300, max = 2000, step = 50),
                                                      h6("Transformations:", style = "font-weight: bold; margin-top: 15px; margin-bottom: 10px;"),
                                                      div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 5px;",
                                                          checkboxInput("fixedAspectRatio", "Ratio fixe", value = FALSE),
                                                          checkboxInput("logScaleX", "Log X", value = FALSE),
                                                          checkboxInput("logScaleY", "Log Y", value = FALSE),
                                                          checkboxInput("reverseY", "Inverser Y", value = FALSE)
                                                      ),
                                                      h6("Labels axe X:", style = "font-weight: bold; margin-top: 15px;"),
                                                      sliderInput("xLabelAngle", "Angle (°):", min = 0, max = 90, value = 0, step = 5),
                                                      helpText(icon("info-circle"), "Inclinez pour éviter les chevauchements.")
                                                  )
                                           )
                                         ),
                                         
                                         # NOUVELLE SECTION : Personnalisation de la légende
                                         fluidRow(
                                           column(12,
                                                  div(class = "well", style = "background-color: #fff8e1; border-left: 4px solid #ff9800; margin-top: 20px;",
                                                      h5(icon("list"), " Personnalisation de la légende", style = "color: #ff9800; font-weight: bold; margin-top: 0;"),
                                                      
                                                      checkboxInput("customizeLegend", "Personnaliser les labels de la légende", value = FALSE),
                                                      
                                                      conditionalPanel(
                                                        condition = "input.customizeLegend",
                                                        div(style = "margin-top: 15px;",
                                                            helpText(icon("info-circle"), "Modifiez les étiquettes de la légende. Les niveaux disponibles dépendent de votre variable de couleur ou des variables Y multiples."),
                                                            
                                                            uiOutput("legendLabelsEditor"),
                                                            
                                                            div(style = "margin-top: 15px;",
                                                                actionButton("resetLegendLabels", "Réinitialiser les labels", 
                                                                             class = "btn-warning btn-sm", icon = icon("undo")),
                                                                actionButton("applyLegendLabels", "Appliquer les modifications", 
                                                                             class = "btn-primary btn-sm", icon = icon("check"))
                                                            )
                                                        )
                                                      ),
                                                      
                                                      h6("Position et style de la légende:", style = "font-weight: bold; margin-top: 20px;"),
                                                      div(style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px;",
                                                          selectInput("legendPosition", "Position:",
                                                                      choices = c("Bas" = "bottom", "Haut" = "top", 
                                                                                  "Droite" = "right", "Gauche" = "left",
                                                                                  "Aucune" = "none"),
                                                                      selected = "bottom"),
                                                          selectInput("legendDirection", "Direction:",
                                                                      choices = c("Horizontale" = "horizontal", 
                                                                                  "Verticale" = "vertical"),
                                                                      selected = "horizontal"),
                                                          numericInput("legendTextSize", "Taille texte:", 
                                                                       value = 10, min = 6, max = 20, step = 1)
                                                      ),
                                                      helpText(icon("lightbulb"), "Personnalisez l'apparence et la position de la légende pour une meilleure présentation.")
                                                  )
                                           )
                                         )
                                ),
                                
                                # Onglet Statistiques
                                tabPanel("Statistiques", icon = icon("chart-bar"),
                                         br(),
                                         fluidRow(
                                           column(6,
                                                  div(class = "well",
                                                      h5(icon("database"), " Résumé des données", style = "color: #495057; font-weight: bold; margin-top: 0;"),
                                                      verbatimTextOutput("dataStatsSummary", placeholder = TRUE),
                                                      conditionalPanel(
                                                        condition = "input.useAggregation",
                                                        hr(),
                                                        div(class = "alert alert-info", style = "margin-bottom: 0; font-size: 13px;",
                                                            icon("info-circle"),
                                                            strong(" Impact de l'agrégation:"), br(),
                                                            "Les statistiques reflètent les données agrégées."
                                                        )
                                                      )
                                                  )
                                           ),
                                           column(6,
                                                  div(class = "well",
                                                      h5(icon("chart-line"), " Configuration du graphique", style = "color: #495057; font-weight: bold; margin-top: 0;"),
                                                      verbatimTextOutput("plotStatsSummary", placeholder = TRUE),
                                                      conditionalPanel(
                                                        condition = "input.vizType == 'seasonal_smooth' || input.vizType == 'seasonal_evolution'",
                                                        hr(),
                                                        h6(icon("calendar-check"), " Analyse saisonnière", style = "color: #17a2b8; font-weight: bold;"),
                                                        verbatimTextOutput("seasonalAnalysisSummary", placeholder = TRUE)
                                                      )
                                                  )
                                           )
                                         )
                                ),
                                
                                # Onglet Aide
                                tabPanel("Aide", icon = icon("question-circle"),
                                         br(),
                                         div(class = "well", style = "max-width: 1200px; margin: 0 auto;",
                                             h4(icon("book"), " Guide de visualisation", style = "color: #007bff; margin-top: 0;"),
                                             
                                             div(class = "alert alert-primary", style = "border-left: 4px solid #007bff;",
                                                 h5(icon("chart-line"), " Types de visualisations"),
                                                 tags$ul(style = "margin-bottom: 0;",
                                                         tags$li(strong("Nuage de points:"), "Relations entre variables numériques"),
                                                         tags$li(strong("Courbe saisonnière avec lissage:"), "Tendances avec lissage LOESS/GAM"),
                                                         tags$li(strong("Courbe évolution saison:"), "Évolution temporelle brute (dates ou catégories)"),
                                                         tags$li(strong("Boxplot/Violon:"), "Distribution et quartiles"),
                                                         tags$li(strong("Barres:"), "Comparaisons catégoriques"),
                                                         tags$li(strong("Histogramme:"), "Distribution de fréquences"),
                                                         tags$li(strong("Heatmap:"), "Matrice de fréquences/corrélations")
                                                 )
                                             ),
                                             
                                             div(class = "alert alert-success", style = "border-left: 4px solid #28a745;",
                                                 h5(icon("star"), " Nouvelles fonctionnalités"),
                                                 tags$ul(
                                                   tags$li(strong("Export universel:"), 
                                                           "Téléchargez vos graphiques dans tous les formats (PNG, JPEG, TIFF, SVG, PDF, EPS) avec un seul bouton"),
                                                   tags$li(strong("Histogrammes adaptatifs:"), 
                                                           "geom_histogram pour X numérique (avec bins), geom_bar pour X catégoriel/date (comptage)"),
                                                   tags$li(strong("Graphiques en barres intelligents:"), 
                                                           "geom_col pour données Y numériques, geom_bar pour comptage. Support de X: facteurs, dates, textes, catégories"),
                                                   tags$li(strong("Palette ggplot2 par défaut:"), 
                                                           "Coloration automatique cohérente sans configuration. Option de personnalisation disponible"),
                                                   tags$li(strong("Application automatique:"), 
                                                           "Les modifications de niveaux X et labels de légende sont appliquées automatiquement dès leur saisie"),
                                                   tags$li(strong("Personnalisation de la légende:"), 
                                                           "Renommez les labels de la légende pour plus de clarté dans vos présentations"),
                                                   tags$li(strong("Contrôle des barres et histogrammes:"), 
                                                           "Ajustez la largeur, l'espacement et le nombre de bins pour une visualisation optimale"),
                                                   tags$li(strong("Contrôles de légende avancés:"), 
                                                           "Position, direction et taille de texte configurables")
                                                 )
                                             ),
                                             
                                             div(class = "alert alert-warning", style = "border-left: 4px solid #ff9800;",
                                                 h5(icon("palette"), " Guide des couleurs et légendes"),
                                                 p(strong("Palette automatique ggplot2:"), "Activez cette option pour appliquer la palette par défaut de ggplot2. Idéal pour une cohérence visuelle sans configuration manuelle."),
                                                 p(strong("Personnalisation des labels:"), "Renommez les entrées de légende pour :"),
                                                 tags$ul(
                                                   tags$li("Clarifier les abréviations (ex: 'Trt A' → 'Traitement A')"),
                                                   tags$li("Traduire les variables (ex: 'Temperature' → 'Température')"),
                                                   tags$li("Améliorer la présentation professionnelle")
                                                 ),
                                                 helpText(icon("lightbulb"), 
                                                          "Les modifications de légende sont appliquées uniquement au graphique, pas aux données sources.")
                                             ),
                                             
                                             div(class = "alert alert-info", style = "border-left: 4px solid #17a2b8;",
                                                 h5(icon("download"), " Guide d'export universel"),
                                                 p("Le nouveau système d'export centralisé vous permet de télécharger vos visualisations dans tous les formats :"),
                                                 
                                                 h6(strong(icon("image"), " Formats Bitmap:"), style = "margin-top: 15px;"),
                                                 tags$ul(
                                                   tags$li(strong("PNG:"), "Format universel, idéal pour le web et les présentations PowerPoint"),
                                                   tags$li(strong("JPEG:"), "Format compressé avec réglage de qualité (10-100%), fichiers plus légers"),
                                                   tags$li(strong("TIFF:"), "Format d'archivage sans perte, idéal pour les publications")
                                                 ),
                                                 
                                                 h6(strong(icon("vector-square"), " Formats Vectoriels:"), style = "margin-top: 15px;"),
                                                 tags$ul(
                                                   tags$li(strong("SVG:"), "Redimensionnable sans perte, parfait pour le web et les éditions graphiques"),
                                                   tags$li(strong("PDF:"), "Standard pour l'impression professionnelle et les rapports"),
                                                   tags$li(strong("EPS:"), "Format standard pour les publications scientifiques et l'édition")
                                                 ),
                                                 
                                                 h6(strong(icon("cog"), " Recommandations DPI:"), style = "margin-top: 15px;"),
                                                 tags$ul(
                                                   tags$li("72-150 DPI: Affichage écran standard"),
                                                   tags$li("300 DPI: Impression bureautique et documents"),
                                                   tags$li("600 DPI: Impression haute qualité professionnelle"),
                                                   tags$li("1200+ DPI: Publications scientifiques et édition"),
                                                   tags$li("20000 DPI: Maximum supporté pour besoins ultra-spécialisés")
                                                 ),
                                                 
                                                 div(style = "background-color: white; padding: 12px; border-radius: 4px; margin-top: 15px; border: 1px solid #17a2b8;",
                                                     icon("lightbulb", style = "color: #17a2b8;"),
                                                     strong(" Astuce:", style = "color: #17a2b8;"),
                                                     p(style = "margin: 5px 0 0 0; font-size: 13px;",
                                                       "Utilisez les presets A4, Letter ou Carré pour les formats vectoriels. L'aperçu en temps réel vous aide à choisir les bonnes dimensions.")
                                                 )
                                             ),
                                             
                                             div(class = "alert alert-success", style = "border-left: 4px solid #28a745;",
                                                 h5(icon("sort-amount-down"), " Courbe d'évolution avec variables catégorielles"),
                                                 p("La courbe d'évolution supporte maintenant les variables catégorielles, facteurs et texte :"),
                                                 tags$ul(
                                                   tags$li(strong("Ordre personnalisé:"), 
                                                           "Glissez-déposez les catégories pour définir leur ordre d'apparition sur l'axe X"),
                                                   tags$li(strong("Agrégation recommandée:"), 
                                                           "Utilisez l'agrégation par moyenne pour calculer des statistiques par groupe (ex: traitement + période)"),
                                                   tags$li(strong("Variable de couleur:"), 
                                                           "Ajoutez une variable de couleur pour comparer plusieurs séries (ex: traitements)"),
                                                   tags$li(strong("Cas d'usage typiques:"), 
                                                           "Périodes d'observation (T1+3, T1+7...), phases expérimentales, catégories ordonnées")
                                                 ),
                                                 div(style = "background-color: white; padding: 10px; border-radius: 4px; margin-top: 10px;",
                                                     strong(icon("example"), " Exemple:"), br(),
                                                     tags$code(style = "font-size: 11px;",
                                                               "Variable X: 'Observation period treatment' (catégorielle)\n",
                                                               "Variable Y: 'Bemisia'\n",
                                                               "Variable couleur: 'Treatment'\n",
                                                               "Agrégation: Moyenne par [Locality, Treatment, Observation period treatment]"
                                                     )
                                                 ),
                                                 helpText(icon("lightbulb"), 
                                                          "Cette approche est idéale pour visualiser l'évolution de mesures répétées dans le temps ou selon une séquence logique.")
                                             ),
                                             
                                             div(class = "alert alert-warning", style = "border-left: 4px solid #ffc107;",
                                                 h5(icon("chart-bar"), " Histogrammes intelligents"),
                                                 p("Les histogrammes s'adaptent automatiquement selon vos données :"),
                                                 
                                                 h6(strong("X numérique:"), style = "margin-top: 10px;"),
                                                 tags$ul(
                                                   tags$li(strong("Distribution classique:"), "Affiche la fréquence des valeurs avec bins réglables (5-100)"),
                                                   tags$li(strong("Utilisation:"), "Visualiser la distribution d'une variable continue")
                                                 ),
                                                 
                                                 h6(strong("X catégoriel + Y numérique:"), style = "margin-top: 10px;"),
                                                 tags$ul(
                                                   tags$li(strong("Mode moyennes:"), "Affiche la moyenne de Y pour chaque catégorie de X"),
                                                   tags$li(strong("Mode comptage:"), "Compte le nombre d'occurrences par catégorie (désactivez 'Afficher moyennes')"),
                                                   tags$li(strong("Cas d'usage:"), "Comparer des mesures moyennes entre groupes")
                                                 ),
                                                 
                                                 h6(strong("X catégoriel sans Y numérique:"), style = "margin-top: 10px;"),
                                                 tags$ul(
                                                   tags$li(strong("Comptage automatique:"), "Affiche le nombre d'observations par catégorie"),
                                                   tags$li(strong("Options:"), "Largeur des barres, position (côte à côte/empilées)")
                                                 ),
                                                 
                                                 div(style = "background-color: white; padding: 10px; border-radius: 4px; margin-top: 10px;",
                                                     strong(icon("example"), " Exemple d'usage:"), br(),
                                                     tags$code(style = "font-size: 11px;",
                                                               "Variable X: 'Treatment' (catégorielle)\n",
                                                               "Variable Y: 'Yield' (numérique)\n",
                                                               "Option: Cocher 'Afficher les moyennes de Y'\n",
                                                               "→ Affiche le rendement moyen par traitement"
                                                     )
                                                 ),
                                                 
                                                 helpText(icon("info-circle"), 
                                                          "L'option 'Afficher moyennes' n'apparaît que si X est catégoriel et Y est numérique.")
                                             ),
                                             
                                             div(class = "alert alert-info", style = "border-left: 4px solid #17a2b8;",
                                                 h5(icon("chart-bar"), " Contrôle des graphiques en barres"),
                                                 p("Options pour optimiser vos visualisations :"),
                                                 
                                                 h6(strong("Graphiques en barres (type 'bar'):"), style = "margin-top: 10px;"),
                                                 tags$ul(
                                                   tags$li(strong("Largeur:"), "0.3 à 1.5 - adaptez selon le nombre de catégories"),
                                                   tags$li(strong("Position:"), "Côte à côte, empilées ou empilées 100%"),
                                                   tags$li(strong("Support:"), "X catégoriel avec Y numérique")
                                                 ),
                                                 
                                                 helpText(icon("lightbulb"), 
                                                          "Un espacement plus grand améliore la lisibilité avec de nombreuses catégories.")
                                             ),
                                             
                                             div(class = "alert alert-info", style = "border-left: 4px solid #17a2b8;",
                                                 h5(icon("calendar-alt"), " Analyses saisonnières et temporelles"),
                                                 p("Deux approches pour l'analyse temporelle :"),
                                                 tags$ul(style = "margin-bottom: 0;",
                                                         tags$li(strong("Avec lissage:"), "Utilise LOESS/GAM avec intervalles de confiance pour détecter les tendances générales et les cycles"),
                                                         tags$li(strong("Évolution saison:"), "Affiche les données brutes avec lignes et points pour voir les variations exactes")
                                                 ),
                                                 p(style = "margin-top: 10px; margin-bottom: 0;", icon("exclamation-circle"), 
                                                   " Important: Pour les dates, utilisez toujours une variable de type Date pour l'axe X.")
                                             ),
                                             
                                             div(class = "alert alert-success", style = "border-left: 4px solid #28a745;",
                                                 h5(icon("layer-group"), " Agrégation des données"),
                                                 p("Résumez vos données par groupes pour simplifier l'analyse :"),
                                                 tags$ul(style = "margin-bottom: 0;",
                                                         tags$li(strong("Comptage:"), "Nombre d'observations par groupe"),
                                                         tags$li(strong("Moyenne/Médiane:"), "Tendances centrales"),
                                                         tags$li(strong("Min/Max:"), "Valeurs extrêmes"),
                                                         tags$li(strong("Écart-type:"), "Mesure de dispersion")
                                                 ),
                                                 helpText(icon("lightbulb"), "Le comptage est idéal pour les analyses de fréquence et les variables catégoriques.")
                                             ),
                                             
                                             div(class = "alert alert-warning", style = "border-left: 4px solid #ffc107;",
                                                 h5(icon("th"), " Facetting (sous-graphiques)"),
                                                 p("Divisez votre graphique en plusieurs panneaux selon une variable catégorique :"),
                                                 tags$ul(
                                                   tags$li(strong("Variable adaptée:"), "Choisissez une colonne avec 2-10 catégories maximum"),
                                                   tags$li(strong("Attention aux NA:"), "Les valeurs manquantes peuvent empêcher la séparation correcte"),
                                                   tags$li(strong("Lecture:"), "Facilite la comparaison entre groupes")
                                                 ),
                                                 p(style = "margin-bottom: 0;", icon("check-circle"), 
                                                   " Exemple: Séparer les données par région, année, ou catégorie de produit.")
                                             ),
                                             
                                             div(class = "alert alert-secondary", style = "border-left: 4px solid #6c757d;",
                                                 h5(icon("exchange-alt"), " Types de variables X"),
                                                 p("Convertissez la variable X selon vos besoins :"),
                                                 tags$ul(
                                                   tags$li(strong("Auto:"), "Détection automatique du type"),
                                                   tags$li(strong("Date:"), "Pour analyses temporelles (format: AAAA-MM-JJ, JJ/MM/AAAA, etc.)"),
                                                   tags$li(strong("Catégorielle/Facteur:"), "Pour regroupements et facetting"),
                                                   tags$li(strong("Numérique:"), "Pour échelles continues et calculs"),
                                                   tags$li(strong("Texte:"), "Pour labels et annotations")
                                                 ),
                                                 helpText(icon("info-circle"), "Vérifiez toujours que le format correspond au type choisi.")
                                             ),
                                             
                                             div(class = "alert alert-light", style = "border-left: 4px solid #ff9800; background-color: #fff8e1;",
                                                 h5(icon("edit"), " Personnalisation des niveaux"),
                                                 p("Renommez les catégories de la variable X pour plus de clarté :"),
                                                 tags$ul(
                                                   tags$li(strong("Quand l'utiliser:"), "Variables catégoriques avec noms peu clairs (ex: 'A', 'B' vers 'Groupe A', 'Groupe B')"),
                                                   tags$li(strong("Règles:"), "Étiquettes uniques, non vides, et descriptives"),
                                                   tags$li(strong("Actions rapides:"), "Ajout de préfixes/suffixes, numérotation, nettoyage d'espaces")
                                                 ),
                                                 div(style = "background-color: white; padding: 8px; border-radius: 4px; margin-top: 10px;",
                                                     icon("exclamation-triangle", style = "color: #ff9800;"),
                                                     " Les modifications sont appliquées uniquement au graphique, pas aux données sources."
                                                 )
                                             ),
                                             
                                             div(class = "alert alert-primary", style = "border-left: 4px solid #007bff;",
                                                 h5(icon("lightbulb"), " Conseils pratiques"),
                                                 tags$ol(
                                                   tags$li(strong("Préparation:"), "Nettoyez vos données (pas de NA dans variables clés)"),
                                                   tags$li(strong("Type de variable:"), "Vérifiez que le type X correspond au graphique souhaité"),
                                                   tags$li(strong("Export:"), "Utilisez le nouveau modal d'export pour tous vos formats en un seul endroit"),
                                                   tags$li(strong("Palettes:"), "Activez les palettes automatiques pour X pour une cohérence visuelle"),
                                                   tags$li(strong("Légendes:"), "Personnalisez les labels pour des présentations professionnelles"),
                                                   tags$li(strong("Barres:"), "Ajustez largeur et espacement selon la densité de vos données"),
                                                   tags$li(strong("DPI:"), "300 pour impression standard, 600+ pour haute qualité"),
                                                   tags$li(strong("Formats:"), "Bitmap pour écran/web, Vectoriel pour impression/édition")
                                                 )
                                             ),
                                             
                                             div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6;",
                                                 h5(icon("keyboard"), " Raccourcis et astuces"),
                                                 tags$ul(style = "margin-bottom: 0; font-size: 13px;",
                                                         tags$li("Utilisez le zoom interactif (cliquer-glisser) pour explorer les détails"),
                                                         tags$li("Double-cliquez pour réinitialiser le zoom rapidement"),
                                                         tags$li("Survolez les points pour voir les valeurs exactes"),
                                                         tags$li("Le modal d'export offre un aperçu en temps réel avant téléchargement"),
                                                         tags$li("Testez différents types de lissage pour trouver le meilleur ajustement"),
                                                         tags$li("Glissez-déposez les catégories pour définir un ordre personnalisé"),
                                                         tags$li("Exportez en SVG/PDF pour éditer dans Illustrator/Inkscape si nécessaire"),
                                                         tags$li("Utilisez les presets A4/Letter pour des dimensions standard")
                                                 )
                                             )
                                         )
                                )
                    )
                )
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
      # ---- Analyses multivariées ----
      tabItem(tabName = "multivariate",
              fluidRow(
                box(title = "Analyse en Composantes Principales (ACP)", status = "info", width = 6, solidHeader = TRUE,
                    uiOutput("pcaVarSelect"),
                    checkboxInput("pcaScale", "Standardiser les variables", TRUE),
                    # Option pour analyses avec moyennes
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
                    numericInput("pcaComponents", "Nombre de composantes:", value = 2, min = 2, max = 10),
                    # Options de personnalisation graphique
                    conditionalPanel(
                      condition = "input.runPCA > 0",
                      hr(),
                      h5("Personnalisation graphique:", style = "font-weight: bold; color: #337ab7;"),
                      textInput("pcaPlotTitle", "Titre du graphique:", 
                                value = "ACP - Analyse en Composantes Principales"),
                      textInput("pcaXLabel", "Label axe X:", value = ""),
                      textInput("pcaYLabel", "Label axe Y:", value = ""),
                      checkboxInput("pcaCenterAxes", "Centrer sur (0,0)", TRUE)
                    ),
                    hr(),
                    actionButton("runPCA", "Exécuter ACP", class = "btn-info", icon = icon("project-diagram"), width = "48%"),
                    downloadButton("downloadPCA", "Télécharger", class = "btn-outline-info", width = "48%")
                ),
                box(title = "Visualisation ACP", status = "info", width = 6, solidHeader = TRUE,
                    plotlyOutput("pcaPlot", height = "550px"),
                    downloadButton("downloadPcaPlot", "Télécharger graphique ACP"),
                    hr(),
                    div(style = "max-height: 300px; overflow-y: auto; font-size: 12px;",
                        verbatimTextOutput("pcaSummary"))
                )
              ),
              fluidRow(
                box(title = "Classification Hiérarchique sur Composantes Principales (HCPC)", status = "success", width = 12, solidHeader = TRUE,
                    p("Cette analyse combine l'ACP avec une classification hiérarchique automatique."),
                    numericInput("hcpcClusters", "Nombre de clusters:", value = 3, min = 2, max = 10),
                    # Options de personnalisation pour HCPC
                    conditionalPanel(
                      condition = "input.runHCPC > 0",
                      hr(),
                      h5("Personnalisation graphique:", style = "font-weight: bold; color: #5cb85c;"),
                      fluidRow(
                        column(6,
                               textInput("hcpcClusterTitle", "Titre carte des clusters:", 
                                         value = "Carte des clusters HCPC"),
                               textInput("hcpcClusterXLabel", "Label axe X:", value = ""),
                               textInput("hcpcClusterYLabel", "Label axe Y:", value = ""),
                               checkboxInput("hcpcCenterAxes", "Centrer sur (0,0)", TRUE)
                        ),
                        column(6,
                               textInput("hcpcDendTitle", "Titre dendrogramme:", 
                                         value = "Dendrogramme HCPC"),
                               # Note: pas de centrage pour le dendrogramme
                               p(style = "font-style: italic; color: #666;", 
                                 "Le dendrogramme n'est pas centré sur (0,0)")
                        )
                      )
                    ),
                    actionButton("runHCPC", "Exécuter HCPC", class = "btn-success", icon = icon("sitemap"), width = "100%"),
                    hr(),
                    fluidRow(
                      column(6,
                             div(class = "box box-solid box-success",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Carte des clusters")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("hcpcClusterPlot", height = "500px"),
                                     downloadButton("downloadHcpcClusterPlot", "Télécharger carte des clusters")
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
                                     downloadButton("downloadHcpcDendPlot", "Télécharger dendrogramme")
                                 )
                             )
                      )
                    ),
                    br(),
                    div(class = "box box-solid",
                        div(class = "box-header with-border", style = "background-color: #5cb85c; color: white;",
                            h4(class = "box-title", "Résultats détaillés HCPC", style = "color: white; font-weight: bold;")
                        ),
                        div(class = "box-body", style = "background-color: #f9f9f9;",
                            div(style = "max-height: 500px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                verbatimTextOutput("hcpcSummary"))
                        )
                    )
                )
              ),
              fluidRow(
                box(title = "Analyse Factorielle Discriminante (AFD)", status = "primary", width = 12, solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             uiOutput("afdFactorSelect")),
                      column(8,
                             uiOutput("afdVarSelect"))
                    ),
                    # Option pour analyses avec moyennes
                    checkboxInput("afdUseMeans", "Utiliser les moyennes par groupe", FALSE),
                    conditionalPanel(
                      condition = "input.afdUseMeans == true",
                      uiOutput("afdMeansGroupSelect")
                    ),
                    # Options de personnalisation pour AFD
                    conditionalPanel(
                      condition = "input.runAFD > 0",
                      hr(),
                      h5("Personnalisation graphique:", style = "font-weight: bold; color: #337ab7;"),
                      fluidRow(
                        column(6,
                               textInput("afdIndTitle", "Titre projection individus:", 
                                         value = "AFD - Projection des individus"),
                               textInput("afdIndXLabel", "Label axe X:", value = ""),
                               textInput("afdIndYLabel", "Label axe Y:", value = ""),
                               checkboxInput("afdIndCenterAxes", "Centrer sur (0,0)", TRUE)
                        ),
                        column(6,
                               textInput("afdVarTitle", "Titre contribution variables:", 
                                         value = "AFD - Contribution des variables"),
                               textInput("afdVarXLabel", "Label axe X:", value = ""),
                               textInput("afdVarYLabel", "Label axe Y:", value = ""),
                               checkboxInput("afdVarCenterAxes", "Centrer sur (0,0)", TRUE)
                        )
                      )
                    ),
                    hr(),
                    actionButton("runAFD", "Exécuter AFD", class = "btn-primary", icon = icon("project-diagram"), width = "100%"),
                    hr(),
                    fluidRow(
                      column(6,
                             div(class = "box box-solid box-primary",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Projection des individus", style = "color: #fff;")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("afdIndPlot", height = "500px"),
                                     downloadButton("downloadAfdIndPlot", "Télécharger projection individus")
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
                                     downloadButton("downloadAfdVarPlot", "Télécharger contribution variables")
                                 )
                             )
                      )
                    ),
                    br(),
                    div(class = "box box-solid",
                        div(class = "box-header with-border", style = "background-color: #d9534f; color: white;",
                            h4(class = "box-title", "Résultats détaillés de l'AFD", style = "color: white; font-weight: bold;")
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
        results_list <- lapply(num_vars, function(var) {
          var_results <- df_in %>%
            group_by(!!!syms(group_vars)) %>%
            summarise(
              mean = if("mean" %in% stats_sel) mean(.data[[var]], na.rm = TRUE) else NA_real_,
              median = if("median" %in% stats_sel) median(.data[[var]], na.rm = TRUE) else NA_real_,
              sd = if("sd" %in% stats_sel) sd(.data[[var]], na.rm = TRUE) else NA_real_,
              var = if("var" %in% stats_sel) var(.data[[var]], na.rm = TRUE) else NA_real_,
              cv = if("cv" %in% stats_sel) calc_cv(.data[[var]]) else NA_real_,
              min = if("min" %in% stats_sel) min(.data[[var]], na.rm = TRUE) else NA_real_,
              max = if("max" %in% stats_sel) max(.data[[var]], na.rm = TRUE) else NA_real_,
              q1 = if("q1" %in% stats_sel) quantile(.data[[var]], 0.25, na.rm = TRUE) else NA_real_,
              q3 = if("q3" %in% stats_sel) quantile(.data[[var]], 0.75, na.rm = TRUE) else NA_real_,
              .groups = "drop"
            ) %>%
            mutate(Variable = var)
          
          return(var_results)
        })
        
        df_combined <- bind_rows(results_list)
        
        # Réorganiser les colonnes
        selected_cols <- c(group_vars, "Variable", stats_sel)
        final_cols <- intersect(selected_cols, names(df_combined))
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
      showNotification("✓ Statistiques calculées avec succès!", type = "message", duration = 3)
      
    }, error = function(e) {
      removeNotification("calcProgress")
      showNotification(paste("❌ Erreur:", e$message), type = "error", duration = 5)
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
      showNotification("✓ Fichier CSV téléchargé!", type = "message", duration = 3)
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
      showNotification("✓ Fichier Excel téléchargé!", type = "message", duration = 3)
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
      showNotification("✓ Graphique PNG téléchargé!", type = "message", duration = 3)
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
      showNotification("✓ Graphique JPEG téléchargé!", type = "message", duration = 3)
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
      showNotification("✓ Graphique TIFF téléchargé!", type = "message", duration = 3)
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
      showNotification("✓ Graphique PDF téléchargé!", type = "message", duration = 3)
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
      showNotification("✓ Graphique SVG téléchargé!", type = "message", duration = 3)
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
      showNotification("✓ Graphique EPS téléchargé!", type = "message", duration = 3)
    }
  )
  # ---- Tableaux croisés dynamiques  ----
  
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
  
  # Détection automatique du type de variable
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
              actionButton("resetLevels", "Réinitialiser", 
                           class = "btn-default btn-xs", icon = icon("undo"))
          )
      ),
      
      div(style = if(length(unique_vals) > 10) "max-height: 400px; overflow-y: auto; padding-right: 10px;" else "",
          lapply(seq_along(unique_vals), function(i) {
            lvl <- unique_vals[i]
            div(style = "margin-bottom: 8px; padding: 8px; background-color: #fafafa; border-radius: 4px; border: 1px solid #e0e0e0;",
                div(style = "display: flex; align-items: center; gap: 10px;",
                    span(paste0(i, "."), style = "color: #999; font-weight: bold; min-width: 25px;"),
                    div(style = "flex: 1;",
                        div(style = "font-size: 11px; color: #666; margin-bottom: 2px;",
                            paste("Original:", lvl)),
                        textInput(
                          inputId = paste0("xLevel_", make.names(lvl)),
                          label = NULL,
                          value = lvl,
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
                  actionButton("resetOrderX", "Réinitialiser", 
                               class = "btn-default btn-xs", icon = icon("undo"))
              )
          )
      ),
      
      div(style = "max-height: 400px; overflow-y: auto; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
          sortable::rank_list(
            text = "Glissez pour réordonner les catégories",
            labels = unique_vals,
            input_id = "xLevelOrder",
            options = sortable::sortable_options(
              multiDrag = FALSE,
              animation = 150
            )
          )
      ),
      
      helpText(icon("lightbulb"), 
               "Définissez l'ordre d'apparition des catégories sur l'axe X (de gauche à droite)")
    )
  })
  
  # NOUVEAU : Éditeur des labels de légende
  output$legendLabelsEditor <- renderUI({
    req(values$filteredData)
    
    # Déterminer la source des labels de légende
    legend_source <- NULL
    legend_levels <- NULL
    
    if(!is.null(values$multipleY) && values$multipleY) {
      # Mode multi-Y : les labels sont les noms des variables Y
      legend_source <- "Multi-Y"
      legend_levels <- values$yVarNames
    } else if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun" && 
              input$vizColorVar != "Automatique (Variables Y)") {
      # Variable de couleur définie
      legend_source <- input$vizColorVar
      color_data <- values$filteredData[[input$vizColorVar]]
      
      if(is.factor(color_data)) {
        legend_levels <- levels(droplevels(color_data))
      } else if(is.character(color_data) || is.numeric(color_data)) {
        legend_levels <- sort(unique(as.character(color_data)))
      }
    }
    
    if(is.null(legend_levels) || length(legend_levels) == 0) {
      return(div(
        p("Aucune légende détectée. Sélectionnez une variable de couleur ou plusieurs variables Y.", 
          style = "color: #999; font-style: italic;")
      ))
    }
    
    if(length(legend_levels) > 20) {
      return(div(
        p(paste("Trop de niveaux de légende (", length(legend_levels), "). Limitez à 20 maximum."), 
          style = "color: #ff9800; font-weight: bold;")
      ))
    }
    
    div(
      div(style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 4px;",
          icon("info-circle", style = "color: #1976d2;"),
          strong(paste(" Source:", legend_source), style = "color: #1976d2;"),
          span(paste(" -", length(legend_levels), "niveaux"), style = "color: #666; font-size: 12px; margin-left: 10px;")
      ),
      
      div(style = if(length(legend_levels) > 6) "max-height: 350px; overflow-y: auto; padding-right: 10px;" else "",
          lapply(seq_along(legend_levels), function(i) {
            lvl <- legend_levels[i]
            div(style = "margin-bottom: 8px; padding: 10px; background-color: #ffffff; border-radius: 4px; border: 1px solid #dee2e6;",
                div(style = "display: flex; align-items: center; gap: 10px;",
                    div(style = "width: 30px; height: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 3px; flex-shrink: 0;"),
                    div(style = "flex: 1;",
                        div(style = "font-size: 11px; color: #666; margin-bottom: 2px;",
                            paste("Original:", lvl)),
                        textInput(
                          inputId = paste0("legendLabel_", make.names(lvl)),
                          label = NULL,
                          value = lvl,
                          placeholder = "Nouveau label...",
                          width = "100%"
                        )
                    )
                )
            )
          })
      ),
      
      helpText(icon("lightbulb"), 
               "Personnalisez les labels pour plus de clarté dans vos présentations. Les modifications sont appliquées au graphique uniquement.")
    )
  })
  
  # Actions pour l'éditeur de niveaux
  observeEvent(input$resetLevels, {
    req(values$filteredData, input$vizXVar)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    
    if(x_type %in% c("factor", "categorical", "text")) {
      unique_vals <- if(is.factor(data[[x_var]])) {
        levels(droplevels(data[[x_var]]))
      } else {
        sort(unique(as.character(data[[x_var]])))
      }
      
      for(lvl in unique_vals) {
        updateTextInput(session, paste0("xLevel_", make.names(lvl)), value = lvl)
      }
      
      showNotification("Étiquettes réinitialisées", type = "message", duration = 2)
    }
  })
  
  observeEvent(input$addPrefixBtn, {
    showModal(modalDialog(
      title = "Ajouter un préfixe",
      textInput("prefixText", "Préfixe à ajouter:", placeholder = "Ex: Groupe "),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("applyPrefix", "Appliquer", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$applyPrefix, {
    req(input$prefixText, values$filteredData, input$vizXVar)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    unique_vals <- if(is.factor(data[[x_var]])) {
      levels(droplevels(data[[x_var]]))
    } else {
      sort(unique(as.character(data[[x_var]])))
    }
    
    for(lvl in unique_vals) {
      current_val <- input[[paste0("xLevel_", make.names(lvl))]]
      if(!is.null(current_val)) {
        updateTextInput(session, paste0("xLevel_", make.names(lvl)), 
                        value = paste0(input$prefixText, current_val))
      }
    }
    
    removeModal()
    showNotification("Préfixe ajouté", type = "message", duration = 2)
  })
  
  # Actions pour l'ordre
  observeEvent(input$autoSortX, {
    req(values$currentXLevels)
    
    sorted_levels <- sort(values$currentXLevels)
    sortable::update_rank_list(session, "xLevelOrder", labels = sorted_levels)
    
    showNotification("Tri alphabétique appliqué", type = "message", duration = 2)
  })
  
  observeEvent(input$resetOrderX, {
    req(values$filteredData, input$vizXVar)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    
    original_vals <- if(is.factor(data[[x_var]])) {
      levels(data[[x_var]])
    } else {
      unique(as.character(data[[x_var]]))
    }
    
    sortable::update_rank_list(session, "xLevelOrder", labels = original_vals)
    showNotification("Ordre réinitialisé", type = "message", duration = 2)
  })
  
  # NOUVEAU : Actions pour les labels de légende
  observeEvent(input$resetLegendLabels, {
    # Obtenir les niveaux originaux
    legend_levels <- NULL
    
    if(!is.null(values$multipleY) && values$multipleY) {
      legend_levels <- values$yVarNames
    } else if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      color_data <- values$filteredData[[input$vizColorVar]]
      if(is.factor(color_data)) {
        legend_levels <- levels(droplevels(color_data))
      } else {
        legend_levels <- sort(unique(as.character(color_data)))
      }
    }
    
    if(!is.null(legend_levels)) {
      for(lvl in legend_levels) {
        updateTextInput(session, paste0("legendLabel_", make.names(lvl)), value = lvl)
      }
      showNotification("Labels de légende réinitialisés", type = "message", duration = 2)
    }
  })
  
  observeEvent(input$applyLegendLabels, {
    showNotification("✓ Modifications appliquées. Régénérez le graphique pour voir les changements.", 
                     type = "message", duration = 4)
  })
  
  # Sélection des variables de couleur
  output$vizColorVarSelect <- renderUI({
    req(values$filteredData)
    
    multiple_y <- !is.null(input$vizYVar) && length(input$vizYVar) > 1
    
    if(multiple_y) {
      div(
        selectInput("vizColorVar", "Variable de couleur:", 
                    choices = "Automatique (Variables Y)",
                    selected = "Automatique (Variables Y)"),
        helpText(icon("info-circle"), 
                 style = "color: #ff9800;",
                 "Les couleurs distinguent automatiquement les variables Y sélectionnées.")
      )
    } else {
      all_cols <- names(values$filteredData)
      all_cols <- iconv(all_cols, to = "UTF-8", sub = "")
      selectInput("vizColorVar", "Variable de couleur (optionnel):", 
                  choices = c("Aucun", all_cols),
                  selected = "Aucun")
    }
  })
  
  # Sélection des variables de facetting
  output$vizFacetVarSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, function(x) is.factor(x) || is.character(x))]
    fac_cols <- iconv(fac_cols, to = "UTF-8", sub = "")
    selectInput("vizFacetVar", "Variable de facetting (optionnel):", 
                choices = c("Aucun", fac_cols),
                selected = "Aucun")
  })
  
  # Sélection des variables de groupement pour l'agrégation
  output$groupVarsSelect <- renderUI({
    req(values$filteredData, input$useAggregation)
    all_cols <- names(values$filteredData)
    all_cols <- iconv(all_cols, to = "UTF-8", sub = "")
    selectizeInput("groupVars", "Variables de groupement:", 
                   choices = all_cols,
                   multiple = TRUE,
                   options = list(placeholder = 'Sélectionnez une ou plusieurs variables...'))
  })
  
  # Fonction d'agrégation sécurisée
  aggregateData <- function(data, group_vars, agg_function, value_var) {
    if (is.null(group_vars) || length(group_vars) == 0) {
      return(data)
    }
    
    tryCatch({
      valid_functions <- c("mean", "median", "sum", "min", "max", "sd", "count")
      if (!agg_function %in% valid_functions) {
        stop("Fonction d'agrégation non valide")
      }
      
      if (agg_function == "count") {
        result <- data %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(!!sym(value_var) := n(), .groups = "drop")
      } else {
        agg_func <- switch(agg_function,
                           "mean" = function(x) mean(x, na.rm = TRUE),
                           "median" = function(x) median(x, na.rm = TRUE),
                           "sum" = function(x) sum(x, na.rm = TRUE),
                           "min" = function(x) min(x, na.rm = TRUE),
                           "max" = function(x) max(x, na.rm = TRUE),
                           "sd" = function(x) sd(x, na.rm = TRUE))
        
        if (!is.numeric(data[[value_var]]) && agg_function != "count") {
          result <- data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(!!sym(value_var) := n(), .groups = "drop")
        } else {
          result <- data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(!!sym(value_var) := agg_func(!!sym(value_var)), .groups = "drop")
        }
      }
      
      return(result)
    }, error = function(e) {
      showNotification(paste("Erreur dans l'agrégation:", e$message), type = "error", duration = 5)
      return(data)
    })
  }
  
  # Génération de la visualisation avec support Y multiple et catégories
  observeEvent(input$generateViz, {
    req(values$filteredData, input$vizXVar, input$vizYVar, input$vizType)
    
    if(is.null(input$vizYVar) || length(input$vizYVar) == 0) {
      showNotification("Veuillez sélectionner au moins une variable Y", type = "warning", duration = 5)
      return()
    }
    
    single_y_types <- c("box", "violin", "density", "histogram")
    if(input$vizType %in% single_y_types && length(input$vizYVar) > 1) {
      showNotification(
        paste("Le type", input$vizType, "ne supporte qu'une seule variable Y. Utilisation de:", input$vizYVar[1]), 
        type = "warning", duration = 5
      )
    }
    
    tryCatch({
      plot_data <- values$filteredData
      
      # Appliquer les étiquettes personnalisées pour X (automatiquement)
      x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
      
      if (x_type %in% c("factor", "categorical", "text") && !is.null(input$vizXVar)) {
        x_var <- input$vizXVar
        
        unique_vals <- if(is.factor(plot_data[[x_var]])) {
          levels(droplevels(plot_data[[x_var]]))
        } else {
          sort(unique(as.character(plot_data[[x_var]])))
        }
        
        # Vérifier si des modifications existent
        has_modifications <- FALSE
        new_labels <- sapply(unique_vals, function(lvl) {
          custom_label <- input[[paste0("xLevel_", make.names(lvl))]]
          if(!is.null(custom_label) && custom_label != lvl) {
            has_modifications <<- TRUE
          }
          custom_label %||% lvl
        })
        
        # Appliquer uniquement s'il y a des modifications
        if(has_modifications) {
          if (any(duplicated(new_labels)) || any(new_labels == "")) {
            showNotification("Les étiquettes doivent être uniques et non vides.", type = "error", duration = 5)
            return()
          }
          
          if(!is.factor(plot_data[[x_var]])) {
            plot_data[[x_var]] <- factor(plot_data[[x_var]], levels = unique_vals, labels = new_labels)
          } else {
            levels(plot_data[[x_var]]) <- new_labels
          }
        }
      }
      
      # Conversion en date si nécessaire
      if (x_type == "date" && !inherits(plot_data[[input$vizXVar]], "Date")) {
        tryCatch({
          plot_data[[input$vizXVar]] <- as.Date(plot_data[[input$vizXVar]], 
                                                format = input$xDateFormat %||% "%Y-%m-%d")
        }, error = function(e) {
          showNotification("Erreur de conversion de date", type = "warning", duration = 5)
        })
      }
      
      # Suppression des NA
      required_vars <- c(input$vizXVar, input$vizYVar)
      color_var <- if (!is.null(input$vizColorVar) && input$vizColorVar != "Aucun" && 
                       input$vizColorVar != "Automatique (Variables Y)" &&
                       input$vizColorVar %in% names(plot_data)) input$vizColorVar else NULL
      
      if (!is.null(color_var)) {
        required_vars <- c(required_vars, color_var)
      }
      
      facet_var <- if (!is.null(input$vizFacetVar) && input$vizFacetVar != "Aucun" && 
                       input$vizFacetVar %in% names(plot_data)) input$vizFacetVar else NULL
      
      if (!is.null(facet_var)) {
        required_vars <- c(required_vars, facet_var)
      }
      
      plot_data <- plot_data %>% 
        filter(if_all(all_of(required_vars), ~ !is.na(.)))
      
      if (nrow(plot_data) == 0) {
        showNotification("Aucune donnée valide après suppression des valeurs manquantes", type = "error", duration = 5)
        return()
      }
      
      x_var <- input$vizXVar
      y_vars <- input$vizYVar
      
      # Traitement multiple Y
      if(length(y_vars) > 1 && !(input$vizType %in% single_y_types)) {
        plot_data_long <- plot_data %>%
          pivot_longer(
            cols = all_of(y_vars),
            names_to = "Variable_Y",
            values_to = "Valeur_Y"
          )
        
        if(!is.null(color_var)) {
          showNotification(
            "Variable de couleur ignorée : les couleurs distinguent les variables Y.", 
            type = "info", duration = 4
          )
        }
        
        actual_color_var <- "Variable_Y"
        plot_data_to_use <- plot_data_long
        actual_y_var <- "Valeur_Y"
        
        values$multipleY <- TRUE
        values$yVarNames <- y_vars
        
      } else {
        if(length(y_vars) > 1 && input$vizType %in% single_y_types) {
          y_vars <- y_vars[1]
        }
        
        plot_data_to_use <- plot_data
        actual_y_var <- y_vars[1]
        actual_color_var <- color_var
        
        values$multipleY <- FALSE
        values$yVarNames <- y_vars
      }
      
      # TRAITEMENT SPÉCIAL SEASONAL_EVOLUTION AVEC CATÉGORIES
      if (input$vizType == "seasonal_evolution") {
        
        # Appliquer l'ordre personnalisé si catégoriel
        if(x_type %in% c("factor", "categorical", "text")) {
          if(!is.null(input$xLevelOrder) && length(input$xLevelOrder) > 0) {
            custom_order <- input$xLevelOrder
            plot_data_to_use[[x_var]] <- factor(
              plot_data_to_use[[x_var]], 
              levels = custom_order, 
              ordered = TRUE
            )
          } else {
            if(!is.factor(plot_data_to_use[[x_var]])) {
              plot_data_to_use[[x_var]] <- factor(plot_data_to_use[[x_var]])
            }
          }
          
          plot_data_to_use <- plot_data_to_use %>%
            arrange(!!sym(x_var))
        }
        
        # Agrégation
        if(isTRUE(input$useAggregation) && 
           !is.null(input$groupVars) && 
           length(input$groupVars) > 0) {
          
          if(values$multipleY) {
            group_vars_agg <- c(x_var, input$groupVars, "Variable_Y")
            group_vars_agg <- unique(group_vars_agg)
            
            plot_data_to_use <- plot_data_to_use %>%
              group_by(across(all_of(group_vars_agg))) %>%
              summarise(!!sym(actual_y_var) := mean(!!sym(actual_y_var), na.rm = TRUE), 
                        .groups = "drop") %>%
              arrange(!!sym(x_var))
            
          } else {
            group_vars_agg <- c(x_var, input$groupVars)
            group_vars_agg <- unique(group_vars_agg)
            
            plot_data_to_use <- plot_data_to_use %>%
              group_by(across(all_of(group_vars_agg))) %>%
              summarise(!!sym(actual_y_var) := mean(!!sym(actual_y_var), na.rm = TRUE), 
                        .groups = "drop") %>%
              arrange(!!sym(x_var))
          }
          
          values$aggregatedData <- plot_data_to_use
        }
        
        # Construction esthétique
        base_aes <- if (!is.null(actual_color_var)) {
          aes(x = !!sym(x_var), y = !!sym(actual_y_var), 
              color = !!sym(actual_color_var), 
              group = !!sym(actual_color_var))
        } else {
          aes(x = !!sym(x_var), y = !!sym(actual_y_var), group = 1)
        }
        
        p <- ggplot(plot_data_to_use, base_aes)
        
        evolution_line_width <- input$evolutionLineWidth %||% 1.2
        evolution_point_size <- input$evolutionPointSize %||% 2
        evolution_line_type <- input$evolutionLineType %||% "solid"
        
        p <- p + 
          geom_line(linewidth = evolution_line_width, 
                    linetype = evolution_line_type,
                    na.rm = TRUE) +
          geom_point(size = evolution_point_size, na.rm = TRUE)
        
        # Étiquettes de données
        if(isTRUE(input$evolutionShowDataLabels)) {
          label_size <- input$evolutionLabelSize %||% 3
          label_vjust <- input$evolutionLabelVjust %||% -0.5
          
          p <- p + geom_text(
            aes(label = round(!!sym(actual_y_var), 1)),
            size = label_size,
            vjust = label_vjust,
            show.legend = FALSE
          )
        }
        
        # Échelles
        if(x_type %in% c("factor", "categorical", "text")) {
          p <- p + scale_x_discrete(drop = FALSE)
        } else if(x_type == "date") {
          p <- p + scale_x_date(
            date_labels = input$evolutionDateFormat %||% "%d-%b",
            expand = expansion(mult = c(0.01, 0.05))
          )
        }
        
        y_expansion <- (input$evolutionYExpansion %||% 10) / 100
        
        p <- p + scale_y_continuous(
          expand = expansion(mult = c(0, y_expansion))
        )
        
        # Grille
        if(isTRUE(input$evolutionShowGrid)) {
          p <- p + theme(
            panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
            panel.grid.minor = element_line(color = "grey95", linewidth = 0.2)
          )
        } else {
          p <- p + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          )
        }
        
        # Thème
        p <- p + theme_minimal(base_size = 14) +
          theme(
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5),
            legend.position = "bottom",
            legend.margin = margin(t = 20),
            axis.text.x = element_text(angle = input$xLabelAngle %||% 0, 
                                       hjust = if((input$xLabelAngle %||% 0) > 0) 1 else 0.5)
          )
        
      } else if (input$vizType == "seasonal_smooth") {
        # Code pour seasonal_smooth (identique à l'original)
        if (inherits(plot_data_to_use[[x_var]], "Date") || inherits(plot_data_to_use[[x_var]], "POSIXt")) {
          plot_data_to_use[[x_var]] <- as.Date(plot_data_to_use[[x_var]])
          
          group_vars <- c(x_var)
          if (!is.null(actual_color_var)) group_vars <- c(group_vars, actual_color_var)
          if (!is.null(facet_var)) group_vars <- c(group_vars, facet_var)
          
          plot_data_to_use <- plot_data_to_use %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(!!sym(actual_y_var) := mean(!!sym(actual_y_var), na.rm = TRUE), .groups = "drop")
          
          plot_data_to_use <- plot_data_to_use[order(plot_data_to_use[[x_var]]), ]
        }
        
        base_aes <- aes(x = !!sym(x_var), y = !!sym(actual_y_var))
        
        p <- ggplot(plot_data_to_use, base_aes)
        
        if (isTRUE(input$showLines)) {
          if (!is.null(actual_color_var)) {
            p <- p + geom_line(aes(color = !!sym(actual_color_var)),
                               size = input$seasonalLineWidth %||% 1.2,
                               linetype = input$lineType %||% "solid")
          } else {
            p <- p + geom_line(size = input$seasonalLineWidth %||% 1.2,
                               linetype = input$lineType %||% "solid",
                               color = "steelblue")
          }
        }
        
        if (isTRUE(input$showPoints)) {
          if (!is.null(actual_color_var)) {
            p <- p + geom_point(aes(color = !!sym(actual_color_var)),
                                size = input$seasonalPointSize %||% 2)
          } else {
            p <- p + geom_point(size = input$seasonalPointSize %||% 2,
                                color = "steelblue")
          }
        }
        
        if (isTRUE(input$showSmoothLine)) {
          smooth_params <- list(
            se = isTRUE(input$showConfidenceInterval),
            alpha = 0.3
          )
          
          if (!is.null(actual_color_var)) {
            smooth_params$mapping <- aes(color = !!sym(actual_color_var))
          }
          
          if (input$smoothMethod == "loess") {
            p <- p + do.call(geom_smooth, c(list(method = "loess", span = input$smoothSpan %||% 0.75), smooth_params))
          } else if (input$smoothMethod == "lm") {
            p <- p + do.call(geom_smooth, c(list(method = "lm", formula = y ~ x), smooth_params))
          } else if (input$smoothMethod == "gam") {
            p <- p + do.call(geom_smooth, c(list(method = "gam", formula = y ~ s(x)), smooth_params))
          }
        }
        
        if (inherits(plot_data_to_use[[x_var]], "Date")) {
          p <- p + scale_x_date(
            date_labels = input$dateFormat %||% "%d-%b",
            expand = expansion(mult = c(0.01, 0.1))
          )
        }
        
      } else {
        # Autres types de visualisation
        base_aes <- if (!is.null(actual_color_var)) {
          aes(x = !!sym(x_var), y = !!sym(actual_y_var), color = !!sym(actual_color_var))
        } else {
          aes(x = !!sym(x_var), y = !!sym(actual_y_var))
        }
        
        p <- ggplot(plot_data_to_use, base_aes)
        
        plot_alpha <- input$plotAlpha %||% 0.7
        plot_size <- input$plotSize %||% 2
        line_width <- input$lineWidth %||% 1
        
        if (input$vizType == "scatter") {
          if (isTRUE(input$jitterPoints)) {
            p <- p + geom_jitter(alpha = plot_alpha, size = plot_size, width = 0.2, height = 0)
          } else {
            p <- p + geom_point(alpha = plot_alpha, size = plot_size)
          }
        } else if (input$vizType == "box") {
          p <- p + geom_boxplot(alpha = plot_alpha)
        } else if (input$vizType == "violin") {
          p <- p + geom_violin(alpha = plot_alpha)
        } else if (input$vizType == "bar") {
          # Paramètres des barres
          bar_width <- input$barWidth %||% 0.9
          bar_position <- input$barPosition %||% "dodge"
          
          # Détection automatique: geom_col si Y existe, geom_bar sinon
          if(is.numeric(plot_data_to_use[[actual_y_var]]) && !all(is.na(plot_data_to_use[[actual_y_var]]))) {
            # geom_col pour données avec Y numérique
            if(!is.null(actual_color_var)) {
              p <- p + geom_col(alpha = plot_alpha, 
                                position = bar_position,
                                width = bar_width)
            } else {
              p <- p + geom_col(alpha = plot_alpha, width = bar_width, fill = "#619CFF")
            }
          } else {
            # geom_bar pour comptage
            if(!is.null(actual_color_var)) {
              p <- p + geom_bar(alpha = plot_alpha, 
                                position = bar_position,
                                width = bar_width)
            } else {
              p <- p + geom_bar(alpha = plot_alpha, width = bar_width, fill = "#619CFF")
            }
          }
        } else if (input$vizType == "line") {
          p <- p + geom_line(alpha = plot_alpha, linewidth = line_width)
        } else if (input$vizType == "density") {
          p <- ggplot(plot_data_to_use, aes(x = !!sym(x_var)))
          if(!is.null(actual_color_var)) {
            p <- p + geom_density(aes(color = !!sym(actual_color_var), fill = !!sym(actual_color_var)), alpha = plot_alpha)
          } else {
            p <- p + geom_density(alpha = plot_alpha)
          }
        } else if (input$vizType == "histogram") {
          # Paramètres de l'histogramme
          hist_bins <- input$histBins %||% 30
          
          # Conversion en numérique si nécessaire pour l'histogramme
          x_data <- plot_data_to_use[[x_var]]
          
          # Vérifier si X est numérique (continu ou discret)
          if(is.numeric(x_data)) {
            # X numérique: histogramme standard de distribution
            p <- ggplot(plot_data_to_use, aes(x = !!sym(x_var)))
            if(!is.null(actual_color_var)) {
              p <- p + geom_histogram(aes(color = !!sym(actual_color_var), fill = !!sym(actual_color_var)), 
                                      alpha = plot_alpha, bins = hist_bins)
            } else {
              p <- p + geom_histogram(alpha = plot_alpha, bins = hist_bins, fill = "#619CFF", color = "white")
            }
          } else if(is.factor(x_data) || is.character(x_data) || inherits(x_data, "Date")) {
            # X catégoriel/date: deux options selon Y
            
            # Vérifier si Y est numérique et non vide
            y_data <- plot_data_to_use[[actual_y_var]]
            y_is_numeric <- is.numeric(y_data) && !all(is.na(y_data))
            
            if(y_is_numeric && isTRUE(input$histShowMean)) {
              # Option 1: Afficher les moyennes de Y par catégorie de X
              p <- ggplot(plot_data_to_use, aes(x = !!sym(x_var), y = !!sym(actual_y_var)))
              
              if(!is.null(actual_color_var)) {
                p <- p + geom_col(aes(fill = !!sym(actual_color_var)), 
                                  alpha = plot_alpha, 
                                  position = input$barPosition %||% "dodge",
                                  width = input$barWidth %||% 0.9)
              } else {
                p <- p + geom_col(alpha = plot_alpha, 
                                  width = input$barWidth %||% 0.9, 
                                  fill = "#619CFF", color = "white")
              }
              
            } else {
              # Option 2: Comptage des occurrences (histogramme classique)
              p <- ggplot(plot_data_to_use, aes(x = !!sym(x_var)))
              if(!is.null(actual_color_var)) {
                p <- p + geom_bar(aes(color = !!sym(actual_color_var), fill = !!sym(actual_color_var)), 
                                  alpha = plot_alpha, width = input$barWidth %||% 0.9)
              } else {
                p <- p + geom_bar(alpha = plot_alpha, width = input$barWidth %||% 0.9, 
                                  fill = "#619CFF", color = "white")
              }
            }
          } else {
            showNotification("Type de variable X non supporté pour l'histogramme", 
                             type = "warning", duration = 5)
            return()
          }
        } else if (input$vizType == "heatmap") {
          heatmap_data <- plot_data_to_use %>%
            group_by(across(c(!!sym(x_var), !!sym(actual_y_var)))) %>%
            summarise(Count = n(), .groups = "drop")
          p <- ggplot(heatmap_data, aes(x = !!sym(x_var), y = !!sym(actual_y_var), fill = Count)) +
            geom_tile(alpha = plot_alpha) +
            scale_fill_gradient(low = "white", high = "red")
        } else if (input$vizType == "area") {
          p <- p + geom_area(alpha = plot_alpha)
        }
      }
      
      # Facetting
      if (!is.null(facet_var)) {
        p <- p + facet_wrap(vars(!!sym(facet_var)), scales = "free")
      }
      
      # Application des palettes de couleurs
      # Par défaut, utiliser la palette ggplot2 pour tous les graphiques
      if(is.null(actual_color_var) && (is.factor(plot_data_to_use[[x_var]]) || 
                                       is.character(plot_data_to_use[[x_var]]))) {
        # Coloration automatique par X (palette ggplot2 par défaut)
        if(input$vizType %in% c("bar", "histogram")) {
          # Pour les barres/histogrammes, appliquer fill sur X
          base_aes_with_fill <- if(input$vizType == "histogram" && !is.numeric(plot_data_to_use[[x_var]])) {
            aes(x = !!sym(x_var), fill = !!sym(x_var))
          } else if(input$vizType == "bar") {
            aes(x = !!sym(x_var), y = !!sym(actual_y_var), fill = !!sym(x_var))
          }
          
          if(!is.null(base_aes_with_fill)) {
            p <- ggplot(plot_data_to_use, base_aes_with_fill)
            
            # Re-générer le geom approprié
            if(input$vizType == "bar") {
              bar_width <- input$barWidth %||% 0.9
              bar_position <- input$barPosition %||% "dodge"
              
              if(is.numeric(plot_data_to_use[[actual_y_var]]) && !all(is.na(plot_data_to_use[[actual_y_var]]))) {
                p <- p + geom_col(alpha = plot_alpha, position = bar_position, width = bar_width)
              } else {
                p <- p + geom_bar(alpha = plot_alpha, position = bar_position, width = bar_width)
              }
            } else if(input$vizType == "histogram") {
              p <- p + geom_bar(alpha = plot_alpha, width = input$barWidth %||% 0.9)
            }
            
            # Ne pas ajouter de guide si on ne veut pas de légende
            p <- p + guides(fill = "none")
          }
        }
      } else if(!is.null(actual_color_var)) {
        # Palette personnalisée uniquement si activée
        if(isTRUE(input$useCustomPalette)) {
          if(!is.null(input$plotPalette) && input$plotPalette != "default") {
            if(input$plotPalette %in% c("viridis", "plasma", "inferno")) {
              p <- p + scale_color_viridis_d(option = substring(input$plotPalette, 1, 1)) +
                scale_fill_viridis_d(option = substring(input$plotPalette, 1, 1))
            } else {
              p <- p + scale_color_brewer(palette = input$plotPalette) +
                scale_fill_brewer(palette = input$plotPalette)
            }
          } else if(isTRUE(input$customColors)) {
            custom_cols <- c(input$color1 %||% "#3498db", input$color2 %||% "#e74c3c")
            p <- p + scale_color_manual(values = custom_cols) +
              scale_fill_manual(values = custom_cols)
          }
        }
        
      }
      
      # Application automatique des labels de légende personnalisés
      if(!is.null(actual_color_var) || (values$multipleY && !is.null(values$yVarNames))) {
        legend_levels <- NULL
        
        if(!is.null(values$multipleY) && values$multipleY) {
          legend_levels <- values$yVarNames
        } else if(!is.null(actual_color_var)) {
          color_data <- plot_data_to_use[[actual_color_var]]
          if(is.factor(color_data)) {
            legend_levels <- levels(droplevels(color_data))
          } else {
            legend_levels <- sort(unique(as.character(color_data)))
          }
        }
        
        if(!is.null(legend_levels) && length(legend_levels) > 0) {
          # Vérifier si des modifications existent
          has_legend_modifications <- FALSE
          custom_legend_labels <- sapply(legend_levels, function(lvl) {
            custom_label <- input[[paste0("legendLabel_", make.names(lvl))]]
            if(!is.null(custom_label) && custom_label != lvl) {
              has_legend_modifications <<- TRUE
            }
            custom_label %||% lvl
          })
          
          # Appliquer uniquement s'il y a des modifications
          if(has_legend_modifications) {
            p <- p + scale_color_discrete(labels = custom_legend_labels) +
              scale_fill_discrete(labels = custom_legend_labels)
          }
        }
      }
      
      # Thème et personnalisation
      x_label_angle <- input$xLabelAngle %||% 0
      x_label_hjust <- if (x_label_angle > 0) 1 else 0.5
      
      legend_pos <- input$legendPosition %||% "bottom"
      legend_dir <- input$legendDirection %||% "horizontal"
      legend_text_size <- input$legendTextSize %||% 10
      
      p <- p + theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = x_label_angle, hjust = x_label_hjust),
          axis.title = element_text(face = "bold"),
          legend.position = legend_pos,
          legend.direction = legend_dir,
          legend.text = element_text(size = legend_text_size),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5)
        )
      
      # Titres et labels
      y_label <- if(values$multipleY) {
        input$plotYLab %||% "Valeurs"
      } else {
        input$plotYLab %||% y_vars[1]
      }
      
      legend_title <- if(values$multipleY) {
        "Variables"
      } else if(!is.null(actual_color_var)) {
        actual_color_var
      } else {
        NULL
      }
      
      p <- p + labs(
        title = input$plotTitle %||% NULL,
        subtitle = input$plotSubtitle %||% NULL,
        x = input$plotXLab %||% input$vizXVar,
        y = y_label,
        caption = input$plotCaption %||% NULL,
        color = legend_title,
        fill = legend_title
      )
      
      # Sauvegarde
      values$currentInteractivePlot <- p
      values$plotData <- plot_data_to_use
      
      success_msg <- if(values$multipleY) {
        paste0("✓ Visualisation générée avec ", length(y_vars), " variables Y!")
      } else {
        "✓ Visualisation générée avec succès!"
      }
      
      showNotification(success_msg, type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Erreur lors de la génération du graphique:", e$message), type = "error", duration = 5)
      print(paste("Erreur détaillée:", e$message))
    })
  })
  
  # Rendu du graphique interactif
  output$advancedPlot <- renderPlotly({
    req(values$currentInteractivePlot)
    tryCatch({
      width_val <- max(300, min(3000, input$plotWidthInteractive %||% 1000))
      height_val <- max(300, min(2000, input$plotHeightInteractive %||% 650))
      
      p_interactive <- ggplotly(values$currentInteractivePlot, 
                                width = width_val, 
                                height = height_val) %>%
        layout(
          dragmode = "zoom",
          showlegend = TRUE,
          legend = list(
            orientation = if((input$legendDirection %||% "horizontal") == "horizontal") "h" else "v",
            xanchor = "center",
            x = 0.5,
            y = -0.2
          ),
          margin = list(l = 50, r = 50, t = 50, b = 100)
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d"),
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = paste0("plot_", Sys.Date()),
            height = height_val,
            width = width_val,
            scale = 2
          )
        )
      
      return(p_interactive)
    }, error = function(e) {
      showNotification(paste("Erreur lors de la création du graphique interactif:", e$message), 
                       type = "error", duration = 5)
      return(NULL)
    })
  })
  
  # Réinitialiser le zoom
  observeEvent(input$resetZoom, {
    tryCatch({
      runjs("
      var plot = document.getElementById('advancedPlot');
      if (plot && plot.layout) {
        Plotly.relayout(plot, {
          'xaxis.autorange': true,
          'yaxis.autorange': true
        });
      }
    ")
      showNotification("Zoom réinitialisé", type = "message", duration = 2)
    }, error = function(e) {
      showNotification("Impossible de réinitialiser le zoom", type = "warning", duration = 5)
    })
  })
  
  # NOUVEAU : Modal d'export universel
  observeEvent(input$openExportModal, {
    req(values$currentInteractivePlot)
    
    showModal(modalDialog(
      title = div(icon("download"), " Export du graphique"),
      size = "l",
      
      fluidRow(
        column(6,
               div(class = "well",
                   h5(icon("cog"), " Format d'export", style = "color: #495057; font-weight: bold; margin-top: 0;"),
                   
                   selectInput("exportFormatModal", "Choisir le format:",
                               choices = c("PNG (recommandé)" = "png",
                                           "JPEG (compressé)" = "jpeg",
                                           "TIFF (archivage)" = "tiff",
                                           "SVG (vectoriel web)" = "svg",
                                           "PDF (vectoriel impression)" = "pdf",
                                           "EPS (vectoriel publication)" = "eps"),
                               selected = "png"),
                   
                   conditionalPanel(
                     condition = "input.exportFormatModal == 'png' || input.exportFormatModal == 'jpeg' || input.exportFormatModal == 'tiff'",
                     h6("Paramètres Bitmap:", style = "font-weight: bold; margin-top: 15px;"),
                     numericInput("exportWidthModal", "Largeur (px):", value = 1920, min = 300, max = 8000, step = 100),
                     numericInput("exportHeightModal", "Hauteur (px):", value = 1080, min = 300, max = 8000, step = 100),
                     numericInput("exportDPIModal", "Résolution (DPI):", value = 300, min = 72, max = 20000, step = 50),
                     
                     div(style = "background-color: #e7f3ff; padding: 10px; border-radius: 4px; margin: 10px 0; font-size: 11px; border-left: 3px solid #2196f3;",
                         strong(icon("lightbulb"), " Guide DPI:"), br(),
                         "• 72-150: Affichage écran", br(),
                         "• 300: Impression standard", br(),
                         "• 600+: Impression haute qualité", br(),
                         "• 1200+: Publication professionnelle"
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.exportFormatModal == 'jpeg'",
                     sliderInput("jpegQualityModal", "Qualité JPEG:", 
                                 min = 10, max = 100, value = 95, post = "%")
                   ),
                   
                   conditionalPanel(
                     condition = "input.exportFormatModal == 'svg' || input.exportFormatModal == 'pdf' || input.exportFormatModal == 'eps'",
                     h6("Paramètres Vectoriels:", style = "font-weight: bold; margin-top: 15px;"),
                     numericInput("exportWidthCmModal", "Largeur (cm):", value = 20, min = 5, max = 50, step = 1),
                     numericInput("exportHeightCmModal", "Hauteur (cm):", value = 15, min = 5, max = 50, step = 1),
                     
                     div(style = "margin-top: 10px;",
                         h6(icon("magic"), " Presets rapides:", style = "font-weight: bold; margin-bottom: 5px;"),
                         div(style = "display: flex; gap: 5px; flex-wrap: wrap;",
                             actionButton("presetA4Modal", "A4 (21×29.7)", class = "btn-sm btn-outline-secondary"),
                             actionButton("presetLetterModal", "Letter (21.6×27.9)", class = "btn-sm btn-outline-secondary"),
                             actionButton("presetSquareModal", "Carré (20×20)", class = "btn-sm btn-outline-secondary")
                         )
                     ),
                     
                     checkboxInput("embedFontsModal", "Incorporer les polices", value = TRUE)
                   )
               )
        ),
        
        column(6,
               div(class = "well",
                   h5(icon("info-circle"), " Aperçu", style = "color: #495057; font-weight: bold; margin-top: 0;"),
                   
                   div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; min-height: 200px; border: 1px solid #dee2e6;",
                       verbatimTextOutput("exportPreviewModal", placeholder = TRUE)
                   ),
                   
                   div(style = "margin-top: 15px;",
                       h6("Informations du graphique:", style = "font-weight: bold;"),
                       uiOutput("exportGraphInfoModal")
                   )
               )
        )
      ),
      
      footer = tagList(
        modalButton("Annuler"),
        downloadButton("downloadFromModal", "Télécharger", 
                       class = "btn-success", 
                       icon = icon("download"))
      ),
      easyClose = FALSE
    ))
  })
  
  # Presets pour le modal
  observeEvent(input$presetA4Modal, {
    updateNumericInput(session, "exportWidthCmModal", value = 21)
    updateNumericInput(session, "exportHeightCmModal", value = 29.7)
    showNotification("Preset A4 appliqué", type = "message", duration = 2)
  })
  
  observeEvent(input$presetLetterModal, {
    updateNumericInput(session, "exportWidthCmModal", value = 21.6)
    updateNumericInput(session, "exportHeightCmModal", value = 27.9)
    showNotification("Preset Letter appliqué", type = "message", duration = 2)
  })
  
  observeEvent(input$presetSquareModal, {
    updateNumericInput(session, "exportWidthCmModal", value = 20)
    updateNumericInput(session, "exportHeightCmModal", value = 20)
    showNotification("Preset Carré appliqué", type = "message", duration = 2)
  })
  
  # Aperçu dans le modal
  output$exportPreviewModal <- renderText({
    format_type <- input$exportFormatModal %||% "png"
    
    if(format_type %in% c("png", "jpeg", "tiff")) {
      width_px <- max(300, min(8000, input$exportWidthModal %||% 1920))
      height_px <- max(300, min(8000, input$exportHeightModal %||% 1080))
      dpi_val <- max(72, min(20000, input$exportDPIModal %||% 300))
      
      width_cm <- round(width_px / dpi_val * 2.54, 1)
      height_cm <- round(height_px / dpi_val * 2.54, 1)
      estimated_size_mb <- round((width_px * height_px * 3) / (1024^2), 2)
      
      quality_level <- if (dpi_val >= 600) {
        "Excellente (impression professionnelle)"
      } else if (dpi_val >= 300) {
        "Très bonne (impression standard)"
      } else if (dpi_val >= 150) {
        "Bonne (affichage HD)"
      } else {
        "Standard (affichage normal)"
      }
      
      quality_info <- if(format_type == "jpeg") {
        paste0("\nQualité JPEG: ", input$jpegQualityModal %||% 95, "%")
      } else {
        ""
      }
      
      paste0(
        "FORMAT BITMAP (", toupper(format_type), ")\n\n",
        "Dimensions: ", width_px, " × ", height_px, " px\n",
        "Équivalent: ", width_cm, " × ", height_cm, " cm\n",
        "Résolution: ", dpi_val, " DPI\n",
        "Qualité: ", quality_level, "\n",
        "Taille estimée: ", estimated_size_mb, " MB",
        quality_info
      )
    } else {
      width_cm <- max(5, min(50, input$exportWidthCmModal %||% 20))
      height_cm <- max(5, min(50, input$exportHeightCmModal %||% 15))
      
      width_inch <- width_cm / 2.54
      height_inch <- height_cm / 2.54
      
      embed_fonts <- if(isTRUE(input$embedFontsModal)) "Oui" else "Non"
      
      format_desc <- switch(format_type,
                            "svg" = "Scalable Vector Graphics - idéal pour le web",
                            "pdf" = "Portable Document Format - idéal pour l'impression",
                            "eps" = "Encapsulated PostScript - standard publication")
      
      paste0(
        "FORMAT VECTORIEL (", toupper(format_type), ")\n\n",
        format_desc, "\n\n",
        "Dimensions: ", width_cm, " × ", height_cm, " cm\n",
        "Équivalent: ", round(width_inch, 2), " × ", round(height_inch, 2), " pouces\n",
        "Polices incorporées: ", embed_fonts, "\n",
        "Taille: Variable (généralement < 1 MB)\n\n",
        "Avantages:\n",
        "• Redimensionnable sans perte de qualité\n",
        "• Idéal pour impression professionnelle\n",
        "• Éditable dans logiciels graphiques"
      )
    }
  })
  
  # Informations du graphique dans le modal
  output$exportGraphInfoModal <- renderUI({
    req(values$currentInteractivePlot)
    
    viz_type_names <- c(
      "scatter" = "Nuage de points",
      "seasonal_smooth" = "Courbe saisonnière",
      "seasonal_evolution" = "Courbe évolution",
      "box" = "Boxplot",
      "violin" = "Violon",
      "bar" = "Barres",
      "line" = "Lignes",
      "density" = "Densité",
      "histogram" = "Histogramme",
      "heatmap" = "Heatmap",
      "area" = "Aires"
    )
    
    viz_name <- viz_type_names[[input$vizType]] %||% "Inconnu"
    
    info_items <- list(
      tags$li(strong("Type:"), viz_name),
      tags$li(strong("Variable X:"), input$vizXVar),
      tags$li(strong("Variable(s) Y:"), 
              if(values$multipleY) paste(values$yVarNames, collapse = ", ") else input$vizYVar[1])
    )
    
    if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      info_items <- c(info_items, list(tags$li(strong("Couleur:"), input$vizColorVar)))
    }
    
    if(!is.null(input$vizFacetVar) && input$vizFacetVar != "Aucun") {
      info_items <- c(info_items, list(tags$li(strong("Facettes:"), input$vizFacetVar)))
    }
    
    info_items <- c(info_items, list(tags$li(strong("Observations:"), nrow(values$plotData))))
    
    div(
      tags$ul(style = "font-size: 12px; padding-left: 20px;", info_items)
    )
  })
  
  # Téléchargement depuis le modal
  output$downloadFromModal <- downloadHandler(
    filename = function() {
      format_type <- input$exportFormatModal %||% "png"
      base_name <- if(!is.null(values$multipleY) && values$multipleY) {
        paste0("viz_multi_", length(values$yVarNames), "vars")
      } else {
        "visualisation"
      }
      paste0(base_name, "_", Sys.Date(), "_", format(Sys.time(), "%H%M%S"), ".", format_type)
    },
    content = function(file) {
      req(values$currentInteractivePlot)
      
      format_type <- input$exportFormatModal %||% "png"
      
      tryCatch({
        if(format_type %in% c("svg", "pdf", "eps")) {
          # Export vectoriel
          device_mapping <- c("svg" = "svg", "pdf" = "pdf", "eps" = "ps")
          device_func <- device_mapping[[format_type]]
          
          width_cm <- max(5, min(50, input$exportWidthCmModal %||% 20))
          height_cm <- max(5, min(50, input$exportHeightCmModal %||% 15))
          
          showNotification(paste0("Export vectoriel ", toupper(format_type), " en cours..."), 
                           type = "message", duration = 3)
          
          ggsave(file, plot = values$currentInteractivePlot,
                 width = width_cm, height = height_cm,
                 units = "cm", device = device_func)
          
          file_size_kb <- round(file.size(file) / 1024, 1)
          showNotification(paste0("✓ Export ", toupper(format_type), " réussi: ", 
                                  width_cm, "×", height_cm, " cm, ", file_size_kb, " KB"), 
                           type = "message", duration = 5)
        } else {
          # Export bitmap
          width_px <- max(300, min(8000, input$exportWidthModal %||% 1920))
          height_px <- max(300, min(8000, input$exportHeightModal %||% 1080))
          dpi_val <- max(72, min(20000, input$exportDPIModal %||% 300))
          
          if (dpi_val > 1200) {
            showNotification(paste0("Export ", toupper(format_type), " haute résolution (", dpi_val, " DPI)..."), 
                             type = "message", duration = 5)
          }
          
          if (format_type == "jpeg") {
            jpeg_quality <- max(10, min(100, input$jpegQualityModal %||% 95))
            ggsave(file, plot = values$currentInteractivePlot,
                   width = width_px/dpi_val, 
                   height = height_px/dpi_val,
                   dpi = dpi_val, units = "in", device = format_type,
                   quality = jpeg_quality)
          } else {
            ggsave(file, plot = values$currentInteractivePlot,
                   width = width_px/dpi_val, 
                   height = height_px/dpi_val,
                   dpi = dpi_val, units = "in", device = format_type)
          }
          
          file_size_mb <- round(file.size(file) / (1024^2), 2)
          showNotification(paste0("✓ Export ", toupper(format_type), " réussi: ", 
                                  width_px, "×", height_px, " px, ", dpi_val, " DPI, ", file_size_mb, " MB"), 
                           type = "message", duration = 5)
        }
        
        # Fermer le modal après succès
        removeModal()
        
      }, error = function(e) {
        showNotification(paste("Erreur lors de l'export:", e$message), 
                         type = "error", duration = 5)
      })
    }
  )
  
  # Informations d'agrégation améliorées
  output$aggregationInfo <- renderText({
    req(input$useAggregation, values$aggregatedData)
    
    tryCatch({
      agg_func_names <- c(
        "mean" = "Moyenne", "median" = "Médiane", "sum" = "Somme",
        "count" = "Comptage", "min" = "Minimum", "max" = "Maximum", "sd" = "Écart-type"
      )
      
      agg_name <- agg_func_names[[input$aggFunction]] %||% "Inconnue"
      
      info_text <- paste0("Fonction: ", agg_name, "\n")
      
      if(!is.null(input$groupVars) && length(input$groupVars) > 0) {
        info_text <- paste0(info_text, 
                            "Groupement par: ", paste(input$groupVars, collapse = ", "), "\n")
      }
      
      if(!is.null(values$aggregatedData)) {
        info_text <- paste0(info_text,
                            "Observations agrégées: ", nrow(values$aggregatedData), "\n",
                            "Observations d'origine: ", nrow(values$filteredData))
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
        "area" = "Aires empilées"
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
  # ---- Analyses multivariées ----
  # Sélection du groupe pour les moyennes (ACP)
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
      label = "Sélectionnez les variables pour l'ACP:",
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
      label = "Variables qualitatives supplémentaires:",
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
      label = "Individus supplémentaires (optionnel):",
      choices = rownames(values$filteredData),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  # Select source for individual labels
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
    
    # Calculer les moyennes par groupe
    means_data <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(across(all_of(vars), mean, na.rm = TRUE), .groups = 'drop') %>%
      column_to_rownames(group_var)
    
    return(means_data)
  }
  
  # ACP avec les nouvelles fonctions
  observeEvent(input$runPCA, {
    req(values$filteredData, input$pcaVars)
    tryCatch({
      # Préparation des données
      if (input$pcaUseMeans && !is.null(input$pcaMeansGroup)) {
        # Utiliser les moyennes par groupe
        pca_data <- calculate_group_means(values$filteredData, input$pcaVars, input$pcaMeansGroup)
      } else {
        # Utiliser les données originales
        pca_data <- values$filteredData[, input$pcaVars, drop = FALSE]
      }
      
      # Variables qualitatives supplémentaires
      quali_sup_indices <- NULL
      if (!is.null(input$pcaQualiSup) && !input$pcaUseMeans) {
        quali_sup_indices <- which(names(values$filteredData) %in% input$pcaQualiSup)
      }
      
      # Individus supplémentaires
      ind_sup_indices <- NULL
      if (!is.null(input$pcaIndSup) && !input$pcaUseMeans) {
        ind_sup_indices <- which(rownames(values$filteredData) %in% input$pcaIndSup)
      }
      
      # Combinaison des données
      if (!input$pcaUseMeans) {
        all_data <- cbind(pca_data, values$filteredData[, input$pcaQualiSup, drop = FALSE])
        
        # Set custom labels for individuals, handling duplicates
        if (input$pcaLabelSource != "rownames") {
          custom_labels <- as.character(values$filteredData[[input$pcaLabelSource]])
          rownames(all_data) <- make.unique(custom_labels)
        }
      } else {
        all_data <- pca_data
      }
      
      # Exécution de l'ACP avec les paramètres demandés
      res.pca <- PCA(all_data,
                     scale.unit = input$pcaScale,
                     quali.sup = quali_sup_indices,
                     ind.sup = ind_sup_indices,
                     ncp = input$pcaComponents,
                     graph = FALSE)
      
      values$pcaResult <- res.pca
      
      # Résumé
      output$pcaSummary <- renderPrint({
        summary(res.pca)
      })
      
      # Visualisation avec les fonctions fviz demandées
      output$pcaPlot <- renderPlotly({
        req(values$pcaResult)
        
        tryCatch({
          # Calcul des pourcentages de variance
          eigenvals <- get_eigenvalue(values$pcaResult)
          pc1_var <- round(eigenvals[1, "variance.percent"], 1)
          pc2_var <- round(eigenvals[2, "variance.percent"], 1)
          
          # Labels par défaut ou personnalisés
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
          
          # Titre personnalisé
          plot_title <- if (!is.null(input$pcaPlotTitle) && input$pcaPlotTitle != "") {
            input$pcaPlotTitle
          } else {
            "ACP - Analyse en Composantes Principales"
          }
          
          if (input$pcaPlotType == "var") {
            # Graphique des variables avec fviz_pca_var
            p <- fviz_pca_var(values$pcaResult, 
                              col.var = "cos2",
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE,
                              title = plot_title) +
              labs(x = x_label, y = y_label)
            
          } else if (input$pcaPlotType == "ind") {
            # Graphique des individus avec fviz_pca_ind
            has_ind_sup <- !is.null(values$pcaResult$ind.sup)
            
            if (has_ind_sup) {
              n_active <- nrow(values$pcaResult$ind$coord)
              col_vector <- rep("Active", n_active)
              
              p <- fviz_pca_ind(values$pcaResult,
                                geom.ind = c("point", "text"),
                                col.ind = col_vector,
                                palette = c("Active" = "#00AFBB"),
                                addEllipses = FALSE,
                                repel = TRUE,
                                title = plot_title,
                                legend.title = "Type") +
                labs(x = x_label, y = y_label)
            } else {
              p <- fviz_pca_ind(values$pcaResult, 
                                col.ind = "contrib",
                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                repel = TRUE,
                                title = plot_title) +
                labs(x = x_label, y = y_label)
            }
            
          } else if (input$pcaPlotType == "biplot") {
            # Biplot avec fviz_pca_biplot
            has_ind_sup <- !is.null(values$pcaResult$ind.sup)
            
            if (has_ind_sup) {
              n_active <- nrow(values$pcaResult$ind$coord)
              col_vector <- rep("Active", n_active)
              
              p <- fviz_pca_biplot(values$pcaResult,
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
              p <- fviz_pca_biplot(values$pcaResult, 
                                   repel = TRUE,
                                   col.var = "#FC4E07", 
                                   col.ind = "#00AFBB",
                                   label = "all",
                                   title = plot_title) +
                labs(x = x_label, y = y_label)
            }
          }
          
          # Centrage sur (0,0) si demandé
          if (!is.null(input$pcaCenterAxes) && input$pcaCenterAxes) {
            # Obtenir les limites actuelles
            if (input$pcaPlotType == "var") {
              coords <- values$pcaResult$var$coord[, 1:2]
            } else if (input$pcaPlotType == "ind") {
              coords <- values$pcaResult$ind$coord[, 1:2]
            } else { # biplot
              coords_ind <- values$pcaResult$ind$coord[, 1:2]
              coords_var <- values$pcaResult$var$coord[, 1:2]
              coords <- rbind(coords_ind, coords_var)
            }
            
            # Calculer les limites symétriques
            max_range <- max(abs(range(coords, na.rm = TRUE)))
            p <- p + xlim(-max_range, max_range) + ylim(-max_range, max_range)
          }
          
          # Convertir en plotly
          suppressWarnings({
            plotly_obj <- ggplotly(p) %>% 
              layout(showlegend = TRUE) %>%
              config(displayModeBar = TRUE)
          })
          
          plotly_obj
          
        }, error = function(e) {
          # Version de base en cas d'erreur
          if (input$pcaPlotType == "var") {
            p <- fviz_pca_var(values$pcaResult, 
                              col.var = "cos2",
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = FALSE,
                              title = "ACP - Cercle de corrélation des variables")
          } else if (input$pcaPlotType == "ind") {
            p <- fviz_pca_ind(values$pcaResult,
                              geom.ind = "point",
                              col.ind = "#00AFBB",
                              addEllipses = FALSE,
                              title = "ACP - Projection des individus")
          } else {
            p <- fviz_pca_biplot(values$pcaResult,
                                 geom.ind = "point",
                                 geom.var = c("arrow", "text"),
                                 col.var = "#FC4E07",
                                 col.ind = "#00AFBB",
                                 repel = FALSE,
                                 addEllipses = FALSE,
                                 title = "ACP - Biplot")
          }
          
          suppressWarnings({
            ggplotly(p) %>% 
              layout(showlegend = TRUE) %>%
              config(displayModeBar = TRUE)
          })
        })
      })
    }, error = function(e) {
      showNotification(paste("Erreur ACP :", e$message), type = "error")
    })
  })
  
  # Download for PCA plot
  output$downloadPcaPlot <- downloadHandler(
    filename = function() { paste("pca_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      eigenvals <- get_eigenvalue(values$pcaResult)
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
      
      p <- switch(input$pcaPlotType,
                  "var" = fviz_pca_var(values$pcaResult, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, title = plot_title) + labs(x = x_label, y = y_label),
                  "ind" = fviz_pca_ind(values$pcaResult, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, addEllipses = !is.null(input$pcaQualiSup), title = plot_title) + labs(x = x_label, y = y_label),
                  "biplot" = fviz_pca_biplot(values$pcaResult, repel = TRUE, col.var = "#FC4E07", col.ind = "#00AFBB", label = "all", addEllipses = !is.null(input$pcaQualiSup), title = plot_title) + labs(x = x_label, y = y_label))
      
      # Appliquer le centrage si demandé
      if (!is.null(input$pcaCenterAxes) && input$pcaCenterAxes) {
        if (input$pcaPlotType == "var") {
          coords <- values$pcaResult$var$coord[, 1:2]
        } else if (input$pcaPlotType == "ind") {
          coords <- values$pcaResult$ind$coord[, 1:2]
        } else {
          coords_ind <- values$pcaResult$ind$coord[, 1:2]
          coords_var <- values$pcaResult$var$coord[, 1:2]
          coords <- rbind(coords_ind, coords_var)
        }
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p <- p + xlim(-max_range, max_range) + ylim(-max_range, max_range)
      }
      
      ggsave(file, plot = p, device = "png", width = 10, height = 8)
    }
  )
  
  output$downloadPCA <- downloadHandler(
    filename = function() {
      paste0("pca_results_", Sys.Date(), ".txt")
    },
    content = function(file) {
      sink(file)
      print(values$pcaResult)
      sink()
    }
  )
  
  # HCPC - Classification Hiérarchique sur Composantes Principales
  observeEvent(input$runHCPC, {
    req(values$pcaResult)
    tryCatch({
      # HCPC avec les paramètres demandés
      res.hcpc <- HCPC(values$pcaResult,
                       nb.clust = input$hcpcClusters,
                       graph = FALSE)
      
      values$hcpcResult <- res.hcpc
      
      # Dendrogramme avec fviz_dend (pas de centrage)
      output$hcpcDendPlot <- renderPlotly({
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
                            sub = paste("Nombre de clusters:", input$hcpcClusters))
        
        ggplotly(p_dend) %>%
          layout(margin = list(b = 100))
      })
      
      # Carte des clusters avec fviz_cluster
      output$hcpcClusterPlot <- renderPlotly({
        cluster_title <- if (!is.null(input$hcpcClusterTitle) && input$hcpcClusterTitle != "") {
          input$hcpcClusterTitle
        } else {
          "Carte des clusters HCPC"
        }
        
        # Labels des axes
        eigenvals <- get_eigenvalue(values$pcaResult)
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
        
        # Centrage sur (0,0) si demandé
        if (!is.null(input$hcpcCenterAxes) && input$hcpcCenterAxes) {
          coords <- values$pcaResult$ind$coord[, 1:2]
          max_range <- max(abs(range(coords, na.rm = TRUE)))
          p_cluster <- p_cluster + xlim(-max_range, max_range) + ylim(-max_range, max_range)
        }
        
        ggplotly(p_cluster) %>%
          layout(showlegend = TRUE)
      })
      
      # Résultats détaillés
      output$hcpcSummary <- renderPrint({
        cat("=== CLASSIFICATION HIÉRARCHIQUE SUR COMPOSANTES PRINCIPALES (HCPC) ===\n\n")
        
        cat("Nombre de clusters:", input$hcpcClusters, "\n\n")
        
        # Affectation des individus aux clusters
        cluster_results <- data.frame(
          Individual = rownames(res.hcpc$data.clust),
          Cluster = res.hcpc$data.clust$clust
        )
        cat("=== AFFECTATION DES INDIVIDUS AUX CLUSTERS ===\n")
        print(cluster_results)
        cat("\n")
        
        # Variables discriminantes par cluster
        cat("=== VARIABLES LES PLUS DISCRIMINANTES PAR CLUSTER ===\n")
        if (!is.null(res.hcpc$desc.var$quanti)) {
          for (i in 1:length(res.hcpc$desc.var$quanti)) {
            if (!is.null(res.hcpc$desc.var$quanti[[i]])) {
              cat("\n--- CLUSTER", i, "---\n")
              cluster_vars <- res.hcpc$desc.var$quanti[[i]]
              print(round(cluster_vars, 4))
            }
          }
        }
        
        # Description des axes par cluster
        if (!is.null(res.hcpc$desc.axes$quanti)) {
          cat("\n=== DESCRIPTION DES AXES PAR CLUSTER ===\n")
          for (i in 1:length(res.hcpc$desc.axes$quanti)) {
            if (!is.null(res.hcpc$desc.axes$quanti[[i]])) {
              cat("\n--- CLUSTER", i, " - AXES ---\n")
              axes_desc <- res.hcpc$desc.axes$quanti[[i]]
              print(round(axes_desc, 4))
            }
          }
        }
        
        # Parangons (individus les plus représentatifs)
        if (!is.null(res.hcpc$desc.ind$para)) {
          cat("\n=== INDIVIDUS LES PLUS REPRÉSENTATIFS (PARANGONS) ===\n")
          for (i in 1:length(res.hcpc$desc.ind$para)) {
            if (!is.null(res.hcpc$desc.ind$para[[i]])) {
              cat("\n--- CLUSTER", i, " - PARANGONS ---\n")
              print(res.hcpc$desc.ind$para[[i]])
            }
          }
        }
        
        # Individus les plus éloignés du centre du cluster
        if (!is.null(res.hcpc$desc.ind$dist)) {
          cat("\n=== INDIVIDUS LES PLUS ÉLOIGNÉS DU CENTRE ===\n")
          for (i in 1:length(res.hcpc$desc.ind$dist)) {
            if (!is.null(res.hcpc$desc.ind$dist[[i]])) {
              cat("\n--- CLUSTER", i, " - INDIVIDUS ÉLOIGNÉS ---\n")
              print(res.hcpc$desc.ind$dist[[i]])
            }
          }
        }
      })
    }, error = function(e) {
      showNotification(paste("Erreur HCPC :", e$message), type = "error")
    })
  })
  
  # Downloads for HCPC plots
  output$downloadHcpcClusterPlot <- downloadHandler(
    filename = function() { paste("hcpc_cluster_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      cluster_title <- if (!is.null(input$hcpcClusterTitle) && input$hcpcClusterTitle != "") {
        input$hcpcClusterTitle
      } else {
        "Carte des clusters HCPC"
      }
      
      eigenvals <- get_eigenvalue(values$pcaResult)
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
      
      p <- fviz_cluster(values$hcpcResult, repel = TRUE, show.clust.cent = TRUE, palette = "jco", ggtheme = theme_minimal(), main = cluster_title) +
        labs(x = x_label, y = y_label)
      
      if (!is.null(input$hcpcCenterAxes) && input$hcpcCenterAxes) {
        coords <- values$pcaResult$ind$coord[, 1:2]
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p <- p + xlim(-max_range, max_range) + ylim(-max_range, max_range)
      }
      
      ggsave(file, plot = p, device = "png", width = 10, height = 8)
    }
  )
  
  output$downloadHcpcDendPlot <- downloadHandler(
    filename = function() { paste("hcpc_dend_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      dend_title <- if (!is.null(input$hcpcDendTitle) && input$hcpcDendTitle != "") {
        input$hcpcDendTitle
      } else {
        "Dendrogramme HCPC"
      }
      
      p <- fviz_dend(values$hcpcResult, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", main = dend_title)
      ggsave(file, plot = p, device = "png", width = 10, height = 8)
    }
  )
  
  # AFD - Sélection du groupe pour les moyennes
  output$afdMeansGroupSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    # Exclure le facteur discriminant des choix
    if (!is.null(input$afdFactor)) {
      fac_cols <- fac_cols[fac_cols != input$afdFactor]
    }
    if (length(fac_cols) == 0) return(NULL)
    
    selectInput("afdMeansGroup", "Variable de groupement pour les moyennes:", 
                choices = fac_cols)
  })
  
  output$afdFactorSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    selectInput("afdFactor", "Facteur discriminant:", choices = fac_cols)
  })
  
  output$afdVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    pickerInput(
      inputId = "afdVars",
      label = "Variables pour l'AFD:",
      choices = num_cols,
      multiple = TRUE,
      selected = num_cols,
      options = list(`actions-box` = TRUE)
    )
  })
  
  observeEvent(input$runAFD, {
    req(values$filteredData, input$afdFactor, input$afdVars)
    tryCatch({
      if (input$afdUseMeans && !is.null(input$afdMeansGroup)) {
        # Utiliser les moyennes par groupe
        temp_data <- values$filteredData[, c(input$afdFactor, input$afdVars, input$afdMeansGroup)]
        temp_data <- na.omit(temp_data)
        
        # Calculer les moyennes par groupe de moyennes, mais garder le facteur discriminant
        afd_data <- temp_data %>%
          group_by(!!sym(input$afdMeansGroup), !!sym(input$afdFactor)) %>%
          summarise(across(all_of(input$afdVars), mean, na.rm = TRUE), .groups = 'drop') %>%
          select(-!!sym(input$afdMeansGroup))
        
      } else {
        # Utiliser les données originales
        afd_data <- values$filteredData[, c(input$afdFactor, input$afdVars)]
        afd_data <- na.omit(afd_data)
      }
      
      # Vérification du nombre de groupes
      if (nlevels(afd_data[[input$afdFactor]]) < 2) {
        showNotification("L'AFD nécessite au moins 2 groupes", type = "error")
        return()
      }
      
      # AFD
      afd_formula <- as.formula(paste(input$afdFactor, "~ ."))
      afd_result <- lda(afd_formula, data = afd_data)
      values$afdResult <- afd_result
      
      # Prédictions
      afd_predict <- predict(afd_result, afd_data)
      
      # Calcul des pourcentages de variance
      eigenvals <- afd_result$svd^2
      prop_var <- eigenvals / sum(eigenvals) * 100
      
      # Préparation des données pour la visualisation
      afd_plot_data <- data.frame(
        LD1 = afd_predict$x[, 1],
        Group = afd_data[[input$afdFactor]],
        Individual = rownames(afd_data)
      )
      
      if (ncol(afd_predict$x) > 1) {
        afd_plot_data$LD2 = afd_predict$x[, 2]
      } else {
        afd_plot_data$LD2 = 0
      }
      
      # Graphique des individus
      output$afdIndPlot <- renderPlotly({
        # Titre et labels personnalisés
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
          geom_point(size = 4, alpha = 0.7) +
          stat_ellipse(type = "norm", level = 0.68, size = 1) +
          geom_text_repel(aes(label = Individual),
                          size = 3.5, box.padding = 0.1, point.padding = 0.03,
                          segment.color = "grey50", max.overlaps = Inf, force = 3,
                          fontface = "bold") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
          labs(title = ind_title, x = x_label, y = y_label, color = input$afdFactor) +
          scale_color_brewer(type = "qual", palette = "Set1") +
          theme_minimal() +
          theme(legend.position = "bottom", panel.grid = element_blank(),
                plot.title = element_text(hjust = 0.5))
        
        # Centrage sur (0,0) si demandé
        if (!is.null(input$afdIndCenterAxes) && input$afdIndCenterAxes) {
          coords <- afd_plot_data[, c("LD1", "LD2")]
          max_range <- max(abs(range(coords, na.rm = TRUE)))
          p_ind <- p_ind + xlim(-max_range, max_range) + ylim(-max_range, max_range)
        }
        
        ggplotly(p_ind, tooltip = "text") %>%
          layout(showlegend = TRUE)
      })
      
      # Graphique des variables (loadings)
      if (ncol(afd_predict$x) >= 1) {
        loadings <- afd_result$scaling
        if (ncol(loadings) == 1) {
          var_contrib <- data.frame(
            Variable = rownames(loadings),
            LD1 = loadings[, 1],
            LD2 = 0
          )
        } else {
          var_contrib <- data.frame(
            Variable = rownames(loadings),
            LD1 = loadings[, 1],
            LD2 = loadings[, 2]
          )
        }
        
        output$afdVarPlot <- renderPlotly({
          # Titre et labels personnalisés
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
            geom_segment(aes(x = 0, y = 0, xend = LD1, yend = LD2),
                         arrow = arrow(length = unit(0.3, "cm")),
                         color = "blue", size = 1) +
            geom_text_repel(aes(label = Variable), size = 3.5, max.overlaps = Inf,
                            color = "blue", fontface = "bold",
                            box.padding = 0.3, segment.color = "blue", force = 2) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
            geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
            labs(title = var_title, x = x_label, y = y_label) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5), panel.grid = element_blank())
          
          # Centrage sur (0,0) si demandé
          if (!is.null(input$afdVarCenterAxes) && input$afdVarCenterAxes) {
            coords <- var_contrib[, c("LD1", "LD2")]
            max_range <- max(abs(range(coords, na.rm = TRUE)))
            p_var <- p_var + xlim(-max_range, max_range) + ylim(-max_range, max_range)
          }
          
          ggplotly(p_var, tooltip = "text")
        })
      }
      
      # Métriques complètes de l'AFD
      output$afdSummary <- renderPrint({
        cat("ANALYSE FACTORIELLE DISCRIMINANTE (AFD)\n")
        cat("----------------------------==\n\n")
        
        # Informations générales
        cat("Facteur discriminant:", input$afdFactor, "\n")
        cat("Variables utilisées:", paste(input$afdVars, collapse = ", "), "\n")
        cat("Nombre d'observations:", nrow(afd_data), "\n")
        cat("Nombre de groupes:", nlevels(afd_data[[input$afdFactor]]), "\n")
        if (input$afdUseMeans && !is.null(input$afdMeansGroup)) {
          cat("Analyse basée sur les moyennes par groupe:", input$afdMeansGroup, "\n")
        }
        cat("\n")
        
        # 1. Tests globaux
        cat("1. TESTS GLOBAUX\n")
        cat("------------=\n")
        
        # Test de Wilks
        eigenvals <- afd_result$svd^2
        wilks_lambda <- prod(1 / (1 + eigenvals))
        cat("Lambda de Wilks:", round(wilks_lambda, 4), "\n")
        
        # Valeurs propres et variance expliquée
        cat("\nValeurs propres:\n")
        print(round(eigenvals, 4))
        
        prop_var <- eigenvals / sum(eigenvals) * 100
        cat("\nVariance expliquée (%):\n")
        for (i in seq_along(prop_var)) {
          cat("LD", i, ": ", round(prop_var[i], 2), "%\n", sep = "")
        }
        cat("Variance cumulée:", round(cumsum(prop_var), 2), "%\n")
        
        # Corrélation canonique
        can_cor <- sqrt(eigenvals / (1 + eigenvals))
        cat("\nCorrélations canoniques:\n")
        print(round(can_cor, 4))
        
        # Coefficients standardisés (loadings)
        cat("\n2. POIDS DES VARIABLES\n")
        cat("----------------==\n")
        
        cat("Coefficients des fonctions discriminantes:\n")
        print(round(afd_result$scaling, 4))
        
        # Structure matrix (corrélations variables-fonctions)
        X_std <- scale(afd_data[, input$afdVars])
        scores <- as.matrix(X_std) %*% afd_result$scaling
        structure_matrix <- cor(X_std, scores)
        cat("\nMatrice de structure (corrélations):\n")
        print(round(structure_matrix, 4))
        
        # F-tests pour chaque variable
        cat("\nTests F univariés:\n")
        f_tests <- data.frame(Variable = input$afdVars)
        for (var in input$afdVars) {
          aov_result <- aov(as.formula(paste(var, "~", input$afdFactor)), data = afd_data)
          f_stat <- summary(aov_result)[[1]][1, "F value"]
          p_val <- summary(aov_result)[[1]][1, "Pr(>F)"]
          f_tests[f_tests$Variable == var, "F_statistic"] <- round(f_stat, 4)
          f_tests[f_tests$Variable == var, "p_value"] <- round(p_val, 4)
        }
        print(f_tests)
        
        # Matrice de confusion
        cat("\n3. QUALITÉ DE CLASSIFICATION\n")
        cat("--------------------====\n")
        
        confusion_matrix <- table(Réel = afd_data[[input$afdFactor]],
                                  Prédit = afd_predict$class)
        cat("Matrice de confusion:\n")
        print(confusion_matrix)
        
        # Taux de bonne attribution
        accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
        cat("\nTaux de classification correcte:", round(accuracy * 100, 2), "%\n")
        
        # Taux par groupe
        cat("\nTaux de classification par groupe:\n")
        for (i in 1:nrow(confusion_matrix)) {
          group_acc <- confusion_matrix[i,i] / sum(confusion_matrix[i,])
          cat(rownames(confusion_matrix)[i], ":", round(group_acc * 100, 2), "%\n")
        }
        
        # Validation croisée (Leave-One-Out)
        cat("\n4. VALIDATION CROISÉE (Leave-One-Out)\n")
        cat("----------------------------==\n")
        
        cv_predictions <- numeric(nrow(afd_data))
        for (i in 1:nrow(afd_data)) {
          # Données d'entraînement (sans l'observation i)
          train_data <- afd_data[-i, ]
          test_data <- afd_data[i, , drop = FALSE]
          
          # Modèle sur données d'entraînement
          cv_model <- lda(afd_formula, data = train_data)
          cv_pred <- predict(cv_model, test_data)
          cv_predictions[i] <- as.character(cv_pred$class)
        }
        
        cv_predictions <- factor(cv_predictions, levels = levels(afd_data[[input$afdFactor]]))
        cv_confusion <- table(Réel = afd_data[[input$afdFactor]],
                              Prédit = cv_predictions)
        
        cat("Matrice de confusion (validation croisée):\n")
        print(cv_confusion)
        
        cv_accuracy <- sum(diag(cv_confusion)) / sum(cv_confusion)
        cat("\nTaux de classification en validation croisée:",
            round(cv_accuracy * 100, 2), "%\n")
        
        cat("\n5. CENTROÏDES DES GROUPES\n")
        cat("--------------------\n")
        print(round(afd_result$means, 4))
        
        cat("\n6. PROBABILITÉS A PRIORI\n")
        cat("----------------====\n")
        print(round(afd_result$prior, 4))
      })
    }, error = function(e) {
      showNotification(paste("Erreur AFD :", e$message), type = "error")
    })
  })
  
  # Downloads for AFD plots
  output$downloadAfdIndPlot <- downloadHandler(
    filename = function() { paste("afd_ind_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      afd_predict <- predict(values$afdResult, values$filteredData[, input$afdVars])
      eigenvals <- values$afdResult$svd^2
      prop_var <- eigenvals / sum(eigenvals) * 100
      afd_plot_data <- data.frame(LD1 = afd_predict$x[,1], LD2 = if(ncol(afd_predict$x)>1) afd_predict$x[,2] else 0,
                                  Group = values$filteredData[[input$afdFactor]], Individual = rownames(values$filteredData))
      
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
      
      ggsave(file, plot = p_ind, device = "png", width = 10, height = 8)
    }
  )
  
  output$downloadAfdVarPlot <- downloadHandler(
    filename = function() { paste("afd_var_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      loadings <- values$afdResult$scaling
      var_contrib <- data.frame(Variable = rownames(loadings), LD1 = loadings[,1], LD2 = if(ncol(loadings)>1) loadings[,2] else 0)
      
      eigenvals <- values$afdResult$svd^2
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
      
      ggsave(file, plot = p_var, device = "png", width = 10, height = 8)
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
          paste0("✓ Graphique exporté avec succès\n",
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
      
      showNotification("✓ Données exportées avec succès!", type = "message", duration = 3)
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