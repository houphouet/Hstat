################################################################################
#
#             INTERFACE UTILISATEUR (UI) DE L'APPLICATION HStat
#
################################################################################

# ---- Charger les dépendances----
source("utils.R")

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
                  
                  # Options graphiques
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
                  
                  # Visualisation des valeurs manquantes
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
                             
                             # BOXPLOT
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
                    
                    # Sélection Y 
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
                                                "Aires empilées" = "area",
                                                "Camembert" = "pie",
                                                "Donut" = "donut",
                                                "Treemap" = "treemap"),
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
                            
                            # Position des barres
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
                    
                    # Options pour barres
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
                    
                    # Boutons d'action 
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
                                                      
                                                      # Option de palette
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
                                         
                                         # Personnalisation de la légende
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
                                                     strong(icon("lightbulb"), " Exemple:"), br(),
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
                                                     strong(icon("lightbulb"), " Exemple d'usage:"), br(),
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
                # -------- Configuration 
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
                    
                    # Section Interactions
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
                    
                    # Bouton d'exécution
                    actionButton("runMultiple", 
                                 HTML("<h5 style='margin: 5px 0;'><i class='fa fa-play'></i> LANCER L'ANALYSE</h5>"), 
                                 class = "btn-success btn-lg", 
                                 style = "width: 100%; height: 70px; font-weight: bold; box-shadow: 0 4px 6px rgba(0,0,0,0.2);"),
                    
                    br(), br(),
                    
                    # Section Options graphiques
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
                                downloadButton("downloadPlot", "Télécharger PNG", 
                                               class = "btn-success", 
                                               style = "width: 100%; height: 50px; font-weight: bold;",
                                               icon = icon("download"))
                            )
                        )
                    )
                ),
                
                # -------- Pnel droit 
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
                                    downloadButton("downloadMainEffects", 
                                                   "Télécharger effets principaux (.xlsx)", 
                                                   class = "btn-success", 
                                                   style = "width: 100%; height: 50px; font-weight: bold;",
                                                   icon = icon("download"))
                                  )
                                ),
                                
                                # ---- TAB 2: Effets simples
                                tabPanel(
                                  title = div(icon("project-diagram"), " Effets simples"),
                                  value = "simpleEffects",
                                  br(),
                                  conditionalPanel(
                                    condition = "output.showSimpleEffects",
                                    # Explication 
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
                                    
                                    # Filtres s
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
                                  
                                  # Navigation des variables - 
                                  conditionalPanel(
                                    condition = "output.showVariableNavigation",
                                    wellPanel(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; color: white;",
                                              div(style = "display: flex; align-items: center; justify-content: center;",
                                                  uiOutput("variableNavigation")
                                              )
                                    )
                                  ),
                                  
                                  # Sélection du type de graphique
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
                                  
                                  # Boutons de téléchargement 
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
                    
                    # Dimensions des barres
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
                    
                    # Légende  
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
                    
                    # Section Export 
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
                    
                    # Guide des formats
                    div(style = "background-color: #e3f2fd; padding: 12px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #2196f3;",
                        icon("question-circle", style = "color: #1976d2;"),
                        strong(" Guide des formats : "), br(),
                        tags$ul(style = "margin-bottom: 0; padding-left: 20px; line-height: 1.8;",
                                tags$li(tags$strong("PNG :"), " Idéal pour PowerPoint, web, réseaux sociaux. Sans perte, fond transparent possible."),
                                tags$li(tags$strong("JPEG :"), " Fichier plus petit mais avec compression. Pour emails, partage rapide."),
                                tags$li(tags$strong("TIFF :"), " Maximum de qualité. Pour impression professionnelle (magazines, posters)."),
                                tags$li(tags$strong("SVG :"), " Vectoriel, redimensionnable à l'infini ! Parfait pour web et édition ultérieure."),
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
