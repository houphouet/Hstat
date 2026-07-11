#  Module Shiny : Tableaux croises dynamiques

mod_crosstab_ui <- function(id) {
  ns <- NS(id)
  tagList(
              
              
              fluidRow(
                box(
                  title       = tagList(icon("info-circle"), " Guide d'utilisation"),
                  status      = "info", width = 12,
                  solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  
                  div(style = "padding:10px 20px;",
                      tags$ol(
                        tags$li(tags$strong("Sélectionnez"), " les variables en lignes et en colonnes."),
                        tags$li(tags$strong("Appliquez"), " un filtre si nécessaire -- sur la variable en ligne/colonne ou sur une variable tierce (filtre additionnel)."),
                        tags$li(tags$strong("Choisissez"), " les analyses et le type de graphique souhaités."),
                        tags$li(tags$strong("Cliquez"), " sur ", tags$em("« Générer l'analyse »"), " pour lancer le calcul."),
                        tags$li(tags$strong("Explorez"), " les résultats dans les onglets dédiés."),
                        tags$li(tags$strong("Personnalisez"), " les graphiques (ordre des niveaux, renommage, couleurs, axes...)."),
                        tags$li(tags$strong("Téléchargez"), " tableaux et graphiques aux formats souhaités (jusqu'à 20 000 DPI).")
                      )
                  )
                )
              ),
              
              
              fluidRow(
                
                box(
                  title       = tagList(icon("cogs"), " Configuration de l'analyse"),
                  status      = "primary", width = 4,
                  solidHeader = TRUE, collapsible = TRUE,
                  
                  div(
                    tags$label("Variables à croiser",
                               style = "font-weight:bold; color:#2c3e50; font-size:15px;"),
                    br(),
                    uiOutput(ns("crosstabRowVarSelect")),
                    uiOutput(ns("crosstabColVarSelect"))
                  ),
                  
                  hr(style = "border-top:2px solid #3498db; margin:18px 0;"),
                  
                  # · Filtre principal (sur var. en lignes ou colonnes) ·
                  div(
                    class = "well well-sm",
                    style = "background:#f0f8ff; border-left:4px solid #3498db; padding:12px;",
                    
                    tags$h5(icon("filter"), " Filtre principal",
                            style = "font-weight:bold; color:#3498db; margin-top:0;"),
                    
                    tags$label("Filtrer par :", style = "font-weight:bold; font-size:13px;"),
                    uiOutput(ns("crosstabFilterTypeUI")),
                    
                    conditionalPanel(
              ns = ns,
                      condition = "input.crosstabFilterType !== 'none'",
                      div(
                        style = "margin-top:10px; max-height:200px; overflow-y:auto;
                     border:1px solid #d1ecf1; border-radius:4px; padding:8px;
                     background:#fff;",
                        uiOutput(ns("crosstabFilterValueSelect"))
                      )
                    ),
                    tags$small(class = "text-muted", icon("lightbulb"),
                               " Décochez les modalités à exclure.")
                  ),
                  
                  hr(style = "border-top:2px dashed #8e44ad; margin:18px 0;"),
                  
                  div(
                    class = "well well-sm",
                    style = "background:#faf5ff; border-left:4px solid #8e44ad; padding:12px;",
                    
                    tags$h5(icon("filter"), " Filtre additionnel",
                            style = "font-weight:bold; color:#8e44ad; margin-top:0;"),
                    tags$small(class = "text-muted d-block mb-2",
                               "Restreindre l'analyse à un sous-ensemble défini par une",
                               tags$strong("autre variable"), " (non sélectionnée en ligne/colonne)."),
                    
                    uiOutput(ns("additionalFilterVarUI")),
                    
                    div(
                      style = "margin-top:4px;",
                      uiOutput(ns("additionalFilterValuesUI"))
                    ),
                    
                    tags$small(class = "text-muted", icon("lightbulb"),
                               " Décochez les valeurs à exclure.")
                  ),
                  
                  hr(style = "border-top:2px solid #27ae60; margin:18px 0;"),
                  
                  div(
                    class = "well well-sm",
                    style = "background:#f6fffa; border-left:4px solid #27ae60; padding:12px;",
                    
                    tags$h5(icon("chart-bar"), " Analyses à produire",
                            style = "font-weight:bold; color:#27ae60; margin-top:0;"),
                    
                    checkboxGroupInput(ns("analysisOptions"), label = NULL,
                      choices = list(
                        "Proportions en lignes (%)"   = "row_prop",
                        "Proportions en colonnes (%)" = "col_prop",
                        "Proportions totales (%)"     = "total_prop",
                        "Test du Chi-deux (χ²)"       = "chi_test",
                        "Test exact de Fisher"        = "fisher_test",
                        "Résidus standardisés"        = "residuals"
                      ),
                      selected = c("row_prop", "col_prop", "chi_test")
                    ),
                    tags$small(class = "text-muted", icon("lightbulb"),
                               " Les résidus nécessitent le test du Chi-deux.")
                  ),
                  
                  hr(style = "border-top:2px solid #e74c3c; margin:18px 0;"),
                  
                  div(
                    class = "well well-sm",
                    style = "background:#fff8f0; border-left:4px solid #e67e22; padding:12px;",
                    
                    tags$h5(icon("paint-brush"), " Graphique principal",
                            style = "font-weight:bold; color:#e67e22; margin-top:0;"),
                    
                    tags$label("Type de représentation :", style = "font-weight:bold; font-size:13px;"),
                    radioButtons(ns("plotType"), label = NULL,
                      choices = c(
                        "Barres groupées"        = "bar",
                        "Barres empilées"        = "stacked_bar",
                        "Mosaïque (proportions)" = "mosaic"
                      ),
                      selected = "bar"
                    ),
                    
                    tags$label("Données à représenter :", style = "font-weight:bold; font-size:13px;"),
                    radioButtons(ns("plotDataType"), label = NULL,
                      choices = c(
                        "Effectifs bruts"             = "counts",
                        "Proportions en lignes (%)"   = "row_prop",
                        "Proportions en colonnes (%)" = "col_prop"
                      ),
                      selected = "counts"
                    ),
                    tags$small(class = "text-muted", icon("info-circle"),
                               " Non applicable au mosaïque (toujours en proportions)."),
                    
                    br(),
                    selectInput(ns("colorPalette"), "Palette de couleurs :",
                      choices = c(
                        "Défaut ggplot2"  = "ggplot_default",
                        "Viridis"         = "viridis",
                        "Plasma"          = "plasma",
                        "Inferno"         = "inferno",
                        "Magma"           = "magma",
                        "Set1"            = "Set1",
                        "Set2"            = "Set2",
                        "Pastel"          = "Pastel1",
                        "Spectral"        = "Spectral",
                        "Niveaux de gris" = "grey",
                        "Noir uniforme"   = "black"
                      ),
                      selected = "ggplot_default", width = "100%"
                    )
                  ),
                  
                  hr(style = "border-top:2px solid #8e44ad; margin:18px 0;"),
                  
                  # Option : calcul sur le jeu complet (mode hors-memoire)
                  conditionalPanel(
              ns = ns,
                    condition = "output.hstatBigData == true",
                    div(style = "padding:10px; background:#fff4e5; border:1px solid #ed6c02; border-radius:8px; margin-bottom:12px;",
                      checkboxInput(ns("crosstabFullData"),
                        HTML("<b><i class='fa fa-server'></i> Calculer sur le jeu complet</b>"),
                        value = FALSE),
                      tags$small(style = "color:#7a4a1a;",
                        "Le tableau croisé sera calculé sur toutes les lignes (DuckDB), pas sur l'échantillon. Les filtres ci-dessus sont alors ignores."))
                  ),
                  
                  actionButton(ns("generateCrosstab"), "Générer l'analyse complète",
                    class = "btn-lg btn-block",
                    icon  = icon("play-circle"),
                    style = paste0("font-weight:bold; font-size:16px; padding:15px;",
                                   "color:#fff;border:none;",
                                   "background:linear-gradient(to right,#27ae60,#2ecc71);",
                                   "box-shadow:0 4px 6px rgba(0,0,0,.15);")
                  )
                ),
                
                box(
                  title       = tagList(icon("table"), " Résultats"),
                  status      = "primary", width = 8,
                  solidHeader = TRUE, collapsible = TRUE,
                  
                  tabBox(
                    title = NULL, id = "crosstabTabs", width = 12,
                    
                    tabPanel(
                      tagList(icon("table"), " Effectifs"),
                      value = "tab_effectifs",
                      br(),
                      DTOutput(ns("crosstabTable")),
                      br(),
                      fluidRow(
                        column(6, downloadButton(ns("downloadCrosstabExcel"), "Excel",
                                                 class = "btn-success btn-block",
                                                 icon  = icon("file-excel"))),
                        column(6, downloadButton(ns("downloadCrosstabCSV"),   "CSV",
                                                 class = "btn-info btn-block",
                                                 icon  = icon("file-csv")))
                      )
                    ),
                    
                    tabPanel(
                      tagList(icon("chart-pie"), " Distribution (1 variable)"),
                      value = "tab_distribution",
                      br(),
                      div(class = "alert alert-info",
                          icon("info-circle"),
                          tags$strong(" Distribution d'une variable : "),
                          "Effectif et proportion relative de chaque modalité ",
                          "(part de chaque modalité sur le total de l'ensemble des modalités). ",
                          "Choisissez la variable a analyser ci-dessous."),
                      uiOutput(ns("distribVarSelect")),
                      DTOutput(ns("crosstabDistribution")),
                      br(),
                      fluidRow(
                        column(6, downloadButton(ns("downloadDistribExcel"), "Excel",
                                                 class = "btn-success btn-block",
                                                 icon  = icon("file-excel"))),
                        column(6, downloadButton(ns("downloadDistribCSV"),   "CSV",
                                                 class = "btn-info btn-block",
                                                 icon  = icon("file-csv")))
                      )
                    ),

                    tabPanel(
                      tagList(icon("percent"), " Prop. lignes"),
                      value = "tab_prop_lignes",
                      br(),
                      div(class = "alert alert-info",
                          icon("info-circle"),
                          tags$strong(" Interprétation : "),
                          "Pourcentages calculés sur le total de chaque ligne (somme = 100 % par ligne)."),
                      DTOutput(ns("crosstabRowProp")),
                      br(),
                      fluidRow(
                        column(6, downloadButton(ns("downloadRowPropExcel"), "Excel",
                                                 class = "btn-success btn-block",
                                                 icon  = icon("file-excel"))),
                        column(6, downloadButton(ns("downloadRowPropCSV"),   "CSV",
                                                 class = "btn-info btn-block",
                                                 icon  = icon("file-csv")))
                      )
                    ),
                    
                    tabPanel(
                      tagList(icon("percent"), " Prop. colonnes"),
                      value = "tab_prop_colonnes",
                      br(),
                      div(class = "alert alert-info",
                          icon("info-circle"),
                          tags$strong(" Interprétation : "),
                          "Pourcentages calculés sur le total de chaque colonne (somme = 100 % par colonne)."),
                      DTOutput(ns("crosstabColProp")),
                      br(),
                      fluidRow(
                        column(6, downloadButton(ns("downloadColPropExcel"), "Excel",
                                                 class = "btn-success btn-block",
                                                 icon  = icon("file-excel"))),
                        column(6, downloadButton(ns("downloadColPropCSV"),   "CSV",
                                                 class = "btn-info btn-block",
                                                 icon  = icon("file-csv")))
                      )
                    ),
                    
                    tabPanel(
                      tagList(icon("percent"), " Prop. totales"),
                      value = "tab_prop_totales",
                      br(),
                      div(class = "alert alert-info",
                          icon("info-circle"),
                          tags$strong(" Interprétation : "),
                          "Pourcentages calculés sur le total général (somme globale = 100 %)."),
                      DTOutput(ns("crosstabTotalProp")),
                      br(),
                      fluidRow(
                        column(6, downloadButton(ns("downloadTotalPropExcel"), "Excel",
                                                 class = "btn-success btn-block",
                                                 icon  = icon("file-excel"))),
                        column(6, downloadButton(ns("downloadTotalPropCSV"),   "CSV",
                                                 class = "btn-info btn-block",
                                                 icon  = icon("file-csv")))
                      )
                    ),
                    
                    tabPanel(
                      tagList(icon("calculator"), " Tests"),
                      value = "tab_tests",
                      br(),
                      div(class = "alert alert-success",
                          icon("check-circle"),
                          tags$strong(" Objectif : "),
                          "Tester l'existence d'une association significative entre les deux variables."),
                      div(
                        class = "well",
                        style = "background:#f8f9fa; border-left:4px solid #3498db; padding:20px;",
                        DTOutput(ns("crosstabTests"))
                      ),
                      br(),
                      fluidRow(
                        column(6, downloadButton(ns("downloadTestsExcel"), "Excel",
                                                 class = "btn-success btn-block",
                                                 icon  = icon("file-excel"))),
                        column(6, downloadButton(ns("downloadTestsCSV"),   "CSV",
                                                 class = "btn-info btn-block",
                                                 icon  = icon("file-csv")))
                      )
                    ),
                    
                    tabPanel(
                      tagList(icon("chart-area"), " Résidus"),
                      value = "tab_residus",
                      br(),
                      div(class = "alert alert-info",
                          icon("info-circle"),
                          tags$strong(" Interprétation : "),
                          "Les résidus standardisés mesurent l'écart à l'indépendance.",
                          tags$ul(
                            tags$li(tags$strong("|valeur| > 2 :"), " contribution significative"),
                            tags$li(tags$strong("Valeur positive :"), " sur-représentation (plus qu'attendu)"),
                            tags$li(tags$strong("Valeur négative :"), " sous-représentation (moins qu'attendu)")
                          )),
                      DTOutput(ns("crosstabResiduals")),
                      br(),
                      fluidRow(
                        column(6, downloadButton(ns("downloadResidualsExcel"), "Excel",
                                                 class = "btn-success btn-block",
                                                 icon  = icon("file-excel"))),
                        column(6, downloadButton(ns("downloadResidualsCSV"),   "CSV",
                                                 class = "btn-info btn-block",
                                                 icon  = icon("file-csv")))
                      )
                    )
                  )
                )
              ),
              
              
              fluidRow(
                box(
                  title       = tagList(icon("sliders-h"), " Personnalisation des graphiques"),
                  status      = "warning", width = 12,
                  solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  
                  fluidRow(
                    
                    column(3,
                           div(class = "well",
                               style = "background:#fff9e6; border-left:4px solid #f39c12; padding:12px;",
                               tags$h5(icon("heading"), " Titres et étiquettes",
                                       style = "font-weight:bold; color:#f39c12; margin-top:0;"),
                               textInput(ns("crosstabTitle"),       "Titre principal :",
                                         placeholder = "Automatique si vide"),
                               textInput(ns("crosstabXLabel"),      "Étiquette axe X :",
                                         placeholder = "Automatique si vide"),
                               textInput(ns("crosstabYLabel"),      "Étiquette axe Y :",
                                         placeholder = "Automatique si vide"),
                               # BUG FIX : champ manquant dans l'UI d'origine
                               textInput(ns("crosstabLegendTitle"), "Titre de la légende :",
                                         placeholder = "Automatique si vide"),
                               hr(style = "margin:8px 0;"),
                               tags$strong("Graphique en secteurs :", style = "font-size:12px;"),
                               # BUG FIX : champs manquants dans l'UI d'origine
                               textInput(ns("piePlotTitle"),   "Titre (secteurs) :",
                                         placeholder = "Automatique si vide"),
                               textInput(ns("pieLegendTitle"), "Légende (secteurs) :",
                                         placeholder = "Automatique si vide")
                           )
                    ),
                    
                    column(3,
                           div(class = "well",
                               style = "background:#e8f4f8; border-left:4px solid #3498db; padding:12px;",
                               tags$h5(icon("font"), " Tailles de police",
                                       style = "font-weight:bold; color:#3498db; margin-top:0;"),
                               sliderInput(ns("titleSize"),      "Titre :",
                                           min = 8, max = 32, value = 16, step = 1, post = " pt"),
                               sliderInput(ns("axisLabelSize"),  "Titres des axes :",
                                           min = 8, max = 24, value = 12, step = 1, post = " pt"),
                               sliderInput(ns("axisTextSize"),   "Graduations :",
                                           min = 6, max = 20, value = 10, step = 1, post = " pt"),
                               sliderInput(ns("legendTextSize"), "Légende :",
                                           min = 6, max = 20, value = 10, step = 1, post = " pt"),
                               # BUG FIX : labelSize manquant dans l'UI d'origine
                               sliderInput(ns("labelSize"),      "Étiquettes sur barres :",
                                           min = 1, max = 10, value = 3.5, step = 0.5, post = " pt")
                           )
                    ),
                    
                    column(3,
                           div(class = "well",
                               style = "background:#fdeaea; border-left:4px solid #e74c3c; padding:12px;",
                               tags$h5(icon("bold"), " Style du texte",
                                       style = "font-weight:bold; color:#e74c3c; margin-top:0;"),
                               tags$strong("Titres des axes :"),
                               checkboxInput(ns("axisTitleBold"),   "Gras",     value = TRUE),
                               checkboxInput(ns("axisTitleItalic"), "Italique", value = FALSE),
                               hr(style = "margin:8px 0;"),
                               tags$strong("Valeurs des axes :"),
                               checkboxInput(ns("axisTextBold"),    "Gras",     value = FALSE),
                               checkboxInput(ns("axisTextItalic"),  "Italique", value = FALSE)
                           )
                    ),
                    
                    column(3,
                           div(class = "well",
                               style = "background:#e8f8f5; border-left:4px solid #1abc9c; padding:12px;",
                               tags$h5(icon("adjust"), " Options d'affichage",
                                       style = "font-weight:bold; color:#1abc9c; margin-top:0;"),
                               sliderInput(ns("xAxisRotation"), "Rotation axe X :",
                                           min = 0, max = 90, value = 45, step = 5, post = "°"),
                               checkboxInput(ns("showPercentages"), "Afficher les valeurs sur les barres",
                                             value = TRUE),
                               checkboxInput(ns("showGridLines"),   "Afficher la grille de fond",
                                             value = TRUE),
                               hr(style = "margin:8px 0;"),
                               # NOUVEAU (Feature 4) : axes et graduations en noir
                               tags$strong("Axes et graduations :",
                                           style = "font-size:13px; color:#117a65;"),
                               checkboxInput(ns("blackAxes"),
                                             tagList(icon("minus"), " Tracer les axes en noir"),
                                             value = FALSE),
                               checkboxInput(ns("blackTicks"),
                                             tagList(icon("grip-lines-vertical"), " Graduations en noir"),
                                             value = FALSE),
                               tags$small(class = "text-muted",
                                          icon("info-circle"),
                                          " « Graduations en noir » colore également les labels des axes.")
                           )
                    )
                  )
                )
              ),
              
              
              fluidRow(
                box(
                  title       = tagList(icon("sort-alpha-down"), " Ordre et renommage des niveaux"),
                  status      = "primary", width = 12,
                  solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  
                  tags$p(class = "text-muted",
                         style = "margin-bottom:16px;",
                         icon("info-circle"),
                         " Ces options sont disponibles après avoir cliqué sur",
                         tags$strong(" « Générer l'analyse ».")),
                  
                  fluidRow(
                    
                    column(3,
                           div(class = "well",
                               style = "background:#eaf4fb; border-left:4px solid #2e86c1; padding:12px; min-height:180px;",
                               uiOutput(ns("xLevelsOrderUI"))
                           )
                    ),
                    
                    column(3,
                           div(class = "well",
                               style = "background:#eaf4fb; border-left:4px solid #2e86c1; padding:12px; min-height:180px;",
                               uiOutput(ns("colLevelsOrderUI"))
                           )
                    ),
                    
                    # - NOUVEAU (Feature 5) : Renommer niveaux axe X -
                    column(3,
                           div(class = "well",
                               style = "background:#eaf7ff; border-left:4px solid #1a5276; padding:12px;
                       max-height:340px; overflow-y:auto;",
                               uiOutput(ns("xLevelRenameUI"))
                           )
                    ),
                    
                    # - NOUVEAU (Feature 2) : Renommer niveaux légende -
                    column(3,
                           div(class = "well",
                               style = "background:#eaf7ff; border-left:4px solid #1a5276; padding:12px;
                       max-height:340px; overflow-y:auto;",
                               uiOutput(ns("legendLevelRenameUI"))
                           )
                    )
                  )
                )
              ),
              
              
              fluidRow(
                
                box(
                  title       = tagList(icon("chart-bar"), " Graphique principal"),
                  status      = "success", width = 6,
                  solidHeader = TRUE, collapsible = TRUE,
                  
                  plotOutput(ns("crosstabPlot"), height = "520px"),
                  
                  hr(style = "border-top:2px solid #27ae60; margin:20px 0;"),
                  
                  uiOutput(ns("plotDownloadSection"))
                ),
                
                box(
                  title       = tagList(icon("chart-pie"), " Graphique en secteurs"),
                  status      = "info", width = 6,
                  solidHeader = TRUE, collapsible = TRUE,
                  
                  fluidRow(
                    column(6,
                           selectInput(ns("pieVariable"), "Variable à représenter :",
                                       choices = c("Variable en lignes"   = "row",
                                                   "Variable en colonnes" = "col"),
                                       width = "100%")),
                    column(6,
                           selectInput(ns("pieColorPalette"), "Palette de couleurs :",
                                       choices = c(
                                         "Défaut ggplot2"  = "ggplot_default",
                                         "Viridis"         = "viridis",
                                         "Plasma"          = "plasma",
                                         "Inferno"         = "inferno",
                                         "Set1"            = "Set1",
                                         "Set2"            = "Set2",
                                         "Pastel"          = "Pastel1",
                                         "Spectral"        = "Spectral",
                                         "Niveaux de gris" = "grey"
                                       ),
                                       selected = "ggplot_default",
                                       width = "100%"))
                  ),
                  
                  # Ordre des niveaux du graphique en secteurs
                  # BUG FIX : cet uiOutput n'était jamais appelé dans l'UI d'origine
                  div(
                    style = "background:#f4f6f7; border-radius:6px; padding:10px 14px; margin-bottom:14px;",
                    uiOutput(ns("pieLevelsOrderUI"))
                  ),
                  
                  plotOutput(ns("crosstabPiePlot"), height = "420px"),
                  
                  hr(style = "border-top:2px solid #3498db; margin:20px 0;"),
                  
                  uiOutput(ns("pieDownloadSection"))
                )
              )
  )
}

mod_crosstab_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Indicateur local de mode hors-memoire (pour les conditionalPanel du module)
    output$hstatBigData <- reactive({
      identical(values$dataMode, "duckdb") && !is.null(values$dbCon)
    })
    outputOptions(output, "hstatBigData", suspendWhenHidden = FALSE)
  # ---- Tableaux croisés dynamiques  ----
  
  # Opérateur null-coalescing (défini localement si absent du scope global)
  if (!exists("%||%", mode = "function")) {
    `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
  }
  
  
  crosstab_values <- reactiveValues(
    contingency_table  = NULL,
    raw_table          = NULL,   # table sans addmargins (pour calculs internes)
    row_proportions    = NULL,
    col_proportions    = NULL,
    total_proportions  = NULL,
    chi_test           = NULL,
    fisher_test        = NULL,
    residuals          = NULL,
    current_plot       = NULL,
    current_pie_plot   = NULL
  )
  
  # 2. SÉLECTION DYNAMIQUE DES VARIABLES (renderUI)
  
  output$crosstabRowVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput(ns("crosstabRowVar"), "Variable en lignes :",
                choices  = all_cols,
                selected = all_cols[1],
                width    = "100%")
  })
  
  output$crosstabColVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput(ns("crosstabColVar"), "Variable en colonnes :",
                choices  = all_cols,
                selected = if (length(all_cols) > 1) all_cols[2] else all_cols[1],
                width    = "100%")
  })
  
  # - Filtre principal (sur variable en lignes ou en colonnes) -
  output$crosstabFilterTypeUI <- renderUI({
    req(values$filteredData)
    radioButtons(
      ns("crosstabFilterType"),
      label    = NULL,
      choices  = c("Aucun filtre"                        = "none",
                   "Filtrer sur la variable en lignes"   = "row",
                   "Filtrer sur la variable en colonnes" = "col"),
      selected = "none"
    )
  })
  
  output$crosstabFilterValueSelect <- renderUI({
    req(values$filteredData, input$crosstabFilterType)
    if (input$crosstabFilterType == "none") return(NULL)
    
    filter_var <- if (input$crosstabFilterType == "row") {
      req(input$crosstabRowVar); input$crosstabRowVar
    } else {
      req(input$crosstabColVar); input$crosstabColVar
    }
    
    vals <- sort(unique(as.character(values$filteredData[[filter_var]])))
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)
    
    tagList(
      tags$label(
        paste0("Modalités de « ", filter_var, " » à conserver :"),
        style = "font-weight:bold; color:#2c3e50; margin-top:8px;"
      ),
      div(style = "max-height:200px; overflow-y:auto; border:1px solid #d1ecf1;
                 border-radius:4px; padding:8px; background:#fff;",
          checkboxGroupInput(ns("crosstabFilterValues"), label = NULL,
                             choices = vals, selected = vals)
      )
    )
  })
  
  # - NOUVEAU (Feature 1) : Filtre additionnel sur une variable tierce -
  output$additionalFilterVarUI <- renderUI({
    req(values$filteredData, input$crosstabRowVar, input$crosstabColVar)
    all_cols   <- names(values$filteredData)
    other_cols <- setdiff(all_cols, c(input$crosstabRowVar, input$crosstabColVar))
    
    if (length(other_cols) == 0) {
      return(tags$p(class = "text-muted",
                    style = "font-size:12px; margin:4px 0;",
                    icon("info-circle"), " Aucune autre variable disponible."))
    }
    
    selectInput(ns("additionalFilterVar"),
                label   = "Filtrer l'analyse par :",
                choices  = c("-- Aucun filtre additionnel --" = "", other_cols),
                selected = "",
                width    = "100%")
  })
  
  output$additionalFilterValuesUI <- renderUI({
    req(values$filteredData)
    var <- input$additionalFilterVar
    if (is.null(var) || !nzchar(var)) return(NULL)
    
    vals <- sort(unique(as.character(values$filteredData[[var]])))
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)
    
    tagList(
      tags$label(
        paste0("Valeurs de « ", var, " » à conserver :"),
        style = "font-weight:bold; color:#6c3483; margin-top:8px; font-size:13px;"
      ),
      div(style = "max-height:160px; overflow-y:auto; border:1px solid #d7bde2;
                 border-radius:4px; padding:8px; background:#faf5ff;",
          checkboxGroupInput(ns("additionalFilterValues"), label = NULL,
                             choices = vals, selected = vals)
      )
    )
  })
  
  # - Ordre des niveaux : axe X -
  output$xLevelsOrderUI <- renderUI({
    req(crosstab_values$raw_table)
    lvls <- rownames(crosstab_values$raw_table)
    tagList(
      tags$label("Ordre des niveaux -- Axe X :",
                 style = "font-weight:bold; color:#2c3e50; font-size:13px;"),
      tags$small(class = "text-muted d-block mb-1",
                 icon("hand-pointer"), " Glissez-déposez pour réordonner."),
      selectizeInput(
        ns("xLevelsOrder"), label = NULL,
        choices = lvls, selected = lvls,
        multiple = TRUE, width = "100%",
        options = list(plugins = list("drag_drop"),
                       placeholder = "Glisser pour réordonner...")
      )
    )
  })
  
  # - Ordre des niveaux : légende / fill -
  output$colLevelsOrderUI <- renderUI({
    req(crosstab_values$raw_table)
    lvls <- colnames(crosstab_values$raw_table)
    tagList(
      tags$label("Ordre des niveaux -- Légende :",
                 style = "font-weight:bold; color:#2c3e50; font-size:13px;"),
      tags$small(class = "text-muted d-block mb-1",
                 icon("hand-pointer"), " Glissez-déposez pour réordonner."),
      selectizeInput(
        ns("colLevelsOrder"), label = NULL,
        choices = lvls, selected = lvls,
        multiple = TRUE, width = "100%",
        options = list(plugins = list("drag_drop"),
                       placeholder = "Glisser pour réordonner...")
      )
    )
  })
  
  # - Ordre des niveaux : graphique en secteurs -
  output$pieLevelsOrderUI <- renderUI({
    req(crosstab_values$raw_table, input$pieVariable)
    lvls <- if (input$pieVariable == "row") {
      rownames(crosstab_values$raw_table)
    } else {
      colnames(crosstab_values$raw_table)
    }
    tagList(
      tags$label("Ordre des niveaux -- Légende (secteurs) :",
                 style = "font-weight:bold; color:#2c3e50; font-size:13px;"),
      tags$small(class = "text-muted d-block mb-1",
                 icon("hand-pointer"), " Glissez-déposez pour réordonner."),
      selectizeInput(
        ns("pieLevelsOrder"), label = NULL,
        choices = lvls, selected = lvls,
        multiple = TRUE, width = "100%",
        options = list(plugins = list("drag_drop"),
                       placeholder = "Glisser pour réordonner...")
      )
    )
  })
  
  # - NOUVEAU (Feature 5) : Renommer les niveaux de l'axe X -
  output$xLevelRenameUI <- renderUI({
    req(crosstab_values$raw_table)
    lvls <- rownames(crosstab_values$raw_table)
    
    tagList(
      tags$label("Renommer les niveaux -- Axe X :",
                 style = "font-weight:bold; color:#1a5276; font-size:13px;"),
      tags$small(class = "text-muted d-block mb-2",
                 icon("info-circle"),
                 " Laissez vide pour conserver le nom d'origine."),
      lapply(lvls, function(lv) {
        div(style = "margin-bottom:6px;",
            textInput(
              inputId     = ns(paste0("xRename_", make.names(lv))),
              label       = tags$span(style = "font-size:11px; color:#555;", lv),
              value       = "",
              placeholder = lv,
              width       = "100%"
            )
        )
      })
    )
  })
  
  # - NOUVEAU (Feature 2) : Renommer les niveaux de la légende -
  output$legendLevelRenameUI <- renderUI({
    req(crosstab_values$raw_table)
    lvls <- colnames(crosstab_values$raw_table)
    
    tagList(
      tags$label("Renommer les niveaux -- Légende :",
                 style = "font-weight:bold; color:#1a5276; font-size:13px;"),
      tags$small(class = "text-muted d-block mb-2",
                 icon("info-circle"),
                 " Laissez vide pour conserver le nom d'origine."),
      lapply(lvls, function(lv) {
        div(style = "margin-bottom:6px;",
            textInput(
              inputId     = ns(paste0("colRename_", make.names(lv))),
              label       = tags$span(style = "font-size:11px; color:#555;", lv),
              value       = "",
              placeholder = lv,
              width       = "100%"
            )
        )
      })
    )
  })
  
  
  create_text_element <- function(size, bold = FALSE, italic = FALSE, color = NULL) {
    face <- dplyr::case_when(
      bold & italic ~ "bold.italic",
      bold          ~ "bold",
      italic        ~ "italic",
      TRUE          ~ "plain"
    )
    args <- list(size = size, face = face)
    if (!is.null(color)) args$color <- color
    do.call(element_text, args)
  }
  
  # Applique une palette de couleurs à un objet ggplot
  apply_color_palette <- function(p, palette_choice, n_colors = NULL) {
    if (is.null(palette_choice) || palette_choice == "ggplot_default") return(p)
    
    if (palette_choice == "grey") {
      return(p + scale_fill_grey(start = 0.3, end = 0.9))
    }
    
    if (palette_choice == "black") {
      n <- if (!is.null(n_colors)) n_colors else 10
      return(p + scale_fill_manual(values = rep("#2c3e50", n)))
    }
    
    brewer_palettes <- c("Set1", "Set2", "Set3", "Pastel1", "Pastel2",
                         "Dark2", "Accent", "Paired", "Spectral", "RdYlBu")
    if (palette_choice %in% brewer_palettes) {
      return(p + scale_fill_brewer(palette = palette_choice))
    }
    
    viridis_options <- c(viridis = "viridis", plasma = "plasma",
                         inferno = "inferno", magma = "magma", cividis = "cividis")
    if (palette_choice %in% names(viridis_options)) {
      return(p + scale_fill_viridis_d(option = viridis_options[[palette_choice]]))
    }
    
    p
  }
  
  # Construit le thème ggplot commun à partir des inputs utilisateur
  # BUG FIX: ajout axes noirs (Feature 4), correction panel.grid (major/minor séparés)
  build_base_theme <- function(input) {
    x_rotation  <- input$xAxisRotation %||% 45
    x_hjust     <- if (x_rotation > 0) 1 else 0.5
    
    # Feature 4 : options axes et graduations en noir
    black_axes  <- input$blackAxes  %||% FALSE
    black_ticks <- input$blackTicks %||% FALSE
    
    # Couleur du texte des axes selon l'option graduations en noir
    axis_text_color <- if (black_ticks) "black" else "grey30"
    
    face_ax_text <- {
      b <- input$axisTextBold   %||% FALSE
      i <- input$axisTextItalic %||% FALSE
      if (b && i) "bold.italic" else if (b) "bold" else if (i) "italic" else "plain"
    }
    
    theme_minimal() +
      theme(
        plot.title   = element_markdown(
          size  = input$titleSize %||% 16,
          hjust = 0.5,
          face  = "bold"
        ),
        axis.line = if (black_axes) {
          element_line(color = "black", linewidth = 0.75)
        } else {
          element_blank()
        },
        axis.ticks = if (black_ticks) {
          element_line(color = "black", linewidth = 0.5)
        } else {
          element_blank()
        },
        axis.ticks.length = if (black_ticks) grid::unit(0.2, "cm") else grid::unit(0, "cm"),
        
        axis.title.x = create_text_element(
          size   = input$axisLabelSize   %||% 12,
          bold   = input$axisTitleBold   %||% TRUE,
          italic = input$axisTitleItalic %||% FALSE
        ),
        axis.title.y = create_text_element(
          size   = input$axisLabelSize   %||% 12,
          bold   = input$axisTitleBold   %||% TRUE,
          italic = input$axisTitleItalic %||% FALSE
        ),
        axis.text.x = element_text(
          angle  = x_rotation,
          hjust  = x_hjust,
          size   = input$axisTextSize %||% 10,
          face   = face_ax_text,
          color  = axis_text_color   # Feature 4
        ),
        axis.text.y = element_text(
          size   = input$axisTextSize %||% 10,
          face   = face_ax_text,
          color  = axis_text_color   # Feature 4
        ),
        legend.text  = element_text(size = input$legendTextSize %||% 10),
        legend.title = element_markdown(size = input$legendTextSize %||% 10, face = "bold"),
        # BUG FIX: panel.grid.major / minor plutôt que panel.grid (plus précis)
        panel.grid.major = if (input$showGridLines %||% TRUE) {
          element_line(color = "gray90", linewidth = 0.4)
        } else {
          element_blank()
        },
        panel.grid.minor = element_blank()
      )
  }
  
  # Résout l'ordre des niveaux demandé par l'utilisateur (gestion robuste)
  resolve_level_order <- function(requested, available) {
    if (is.null(requested) || length(requested) == 0) return(available)
    valid   <- requested[requested %in% available]
    missing <- setdiff(available, valid)
    c(valid, missing)
  }
  
  # Helper pour le renommage dynamique des niveaux via les inputs
  rename_levels <- function(lvls, prefix, input) {
    vapply(lvls, function(lv) {
      key <- paste0(prefix, make.names(lv))
      val <- input[[key]]
      if (!is.null(val) && nzchar(trimws(val))) trimws(val) else lv
    }, character(1))
  }
  
  # 4. GÉNÉRATION DES ANALYSES STATISTIQUES
  
  observeEvent(input$generateCrosstab, {
    req(input$crosstabRowVar, input$crosstabColVar)
    
    showNotification("Génération des analyses en cours...",
                     type = "message", duration = 2, id = "gen_notif")
    
    tryCatch({
      df <- values$filteredData
      
      filter_type   <- input$crosstabFilterType %||% "none"
      filter_values <- input$crosstabFilterValues
      
      if (filter_type != "none" && !is.null(filter_values) && length(filter_values) > 0) {
        filter_var <- if (filter_type == "row") input$crosstabRowVar else input$crosstabColVar
        df <- df[as.character(df[[filter_var]]) %in% filter_values, ]
      }
      
      # - Feature 1 : Filtre additionnel (variable tierce) -
      add_var <- input$additionalFilterVar
      if (!is.null(add_var) && nzchar(add_var)) {
        add_vals <- input$additionalFilterValues
        if (!is.null(add_vals) && length(add_vals) > 0) {
          df <- df[as.character(df[[add_var]]) %in% add_vals, ]
        }
      }
      
      # - Suppression des NA sur les variables d'analyse -
      df <- df[!is.na(df[[input$crosstabRowVar]]) & !is.na(df[[input$crosstabColVar]]), ]
      
      if (nrow(df) == 0) {
        showNotification("Aucune donnée disponible après filtrage !",
                         type = "error", duration = 5)
        return()
      }
      
      df[[input$crosstabRowVar]] <- droplevels(as.factor(df[[input$crosstabRowVar]]))
      df[[input$crosstabColVar]] <- droplevels(as.factor(df[[input$crosstabColVar]]))
      
      # - Table de contingence brute -
      # En mode hors-memoire avec l'option activee : table EXACTE sur le jeu complet (DuckDB).
      use_full <- isTRUE(input$crosstabFullData) &&
                  identical(values$dataMode, "duckdb") && !is.null(values$dbCon)
      if (use_full) {
        raw_ct <- tryCatch(
          hstat_duckdb_crosstab(values$dbCon, values$dbTable,
                                input$crosstabRowVar, input$crosstabColVar),
          error = function(e) NULL)
        if (is.null(raw_ct)) {
          showNotification("Echec du calcul sur le jeu complet -- repli sur l'échantillon.",
                           type = "warning", duration = 5)
          raw_ct <- table(df[[input$crosstabRowVar]], df[[input$crosstabColVar]])
        } else {
          showNotification(
            sprintf("Tableau croisé calculé sur le jeu complet (%s lignes).",
                    format(values$fullNrow %||% 0, big.mark = " ")),
            type = "message", duration = 5)
        }
      } else {
        raw_ct <- table(df[[input$crosstabRowVar]], df[[input$crosstabColVar]])
      }
      
      if (any(dim(raw_ct) == 0)) {
        showNotification("Une variable ne contient aucune modalité après filtrage !",
                         type = "error", duration = 5)
        return()
      }
      
      crosstab_values$raw_table         <- raw_ct
      crosstab_values$contingency_table <- addmargins(raw_ct)
      
      if ("row_prop" %in% input$analysisOptions) {
        row_prop <- prop.table(raw_ct, margin = 1) * 100
        crosstab_values$row_proportions <- addmargins(row_prop, margin = 2)
      } else {
        crosstab_values$row_proportions <- NULL
      }
      
      if ("col_prop" %in% input$analysisOptions) {
        col_prop <- prop.table(raw_ct, margin = 2) * 100
        crosstab_values$col_proportions <- addmargins(col_prop, margin = 1)
      } else {
        crosstab_values$col_proportions <- NULL
      }
      
      if ("total_prop" %in% input$analysisOptions) {
        total_prop <- prop.table(raw_ct) * 100
        row_totals <- apply(total_prop, 1, sum)
        tp_margins <- cbind(total_prop, Sum = row_totals)
        col_totals <- apply(tp_margins, 2, sum)
        tp_margins <- rbind(tp_margins, Sum = col_totals)
        crosstab_values$total_proportions <- tp_margins
      } else {
        crosstab_values$total_proportions <- NULL
      }
      
      if ("chi_test" %in% input$analysisOptions) {
        if (all(raw_ct >= 5)) {
          crosstab_values$chi_test <- chisq.test(raw_ct)
        } else {
          crosstab_values$chi_test <- tryCatch(
            chisq.test(raw_ct, correct = (all(dim(raw_ct) == 2))),
            error = function(e) paste("Erreur Chi-deux :", e$message)
          )
          if (is.list(crosstab_values$chi_test) &&
              any(crosstab_values$chi_test$expected < 5)) {
            showNotification(
              "Attention : certains effectifs théoriques sont < 5. Résultats à interpréter avec précaution.",
              type = "warning", duration = 6)
          } else if (!is.list(crosstab_values$chi_test)) {
            showNotification(
              "Attention : certains effectifs théoriques sont < 5. Résultats à interpréter avec précaution.",
              type = "warning", duration = 6)
          }
        }
      } else {
        crosstab_values$chi_test <- NULL
      }
      
      # - Test exact de Fisher -
      if ("fisher_test" %in% input$analysisOptions) {
        if (all(dim(raw_ct) == 2)) {
          crosstab_values$fisher_test <- fisher.test(raw_ct)
        } else {
          crosstab_values$fisher_test <-
            "Test de Fisher disponible uniquement pour les tableaux 2x2."
        }
      } else {
        crosstab_values$fisher_test <- NULL
      }
      
      if ("residuals" %in% input$analysisOptions) {
        if (is.list(crosstab_values$chi_test) &&
            !is.null(crosstab_values$chi_test$stdres)) {
          crosstab_values$residuals <- crosstab_values$chi_test$stdres
        } else {
          crosstab_values$residuals <- NULL
          showNotification(
            "Les résidus standardisés nécessitent un test du Chi-deux valide. Veuillez cocher « Test du Chi-deux ».",
            type = "warning", duration = 6)
        }
      } else {
        crosstab_values$residuals <- NULL
      }
      
      showNotification("Analyses générées avec succès !", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'analyse :", e$message),
                       type = "error", duration = 10)
    })
  })
  
  # 5. AFFICHAGE DES TABLEAUX DE RÉSULTATS
  
  # Helper : options DT communes
  dt_options <- list(
    scrollX    = TRUE,
    pageLength = 25,
    dom        = "Bfrtip",
    buttons    = .hstat_dt_buttons("tableau_croise")
  )
  
  dt_caption <- function(text) {
    htmltools::tags$caption(
      style = paste0("caption-side:top; text-align:center;",
                     "color:#333; font-size:16px; font-weight:bold;"),
      text
    )
  }
  
  output$crosstabTable <- renderDT({
    req(crosstab_values$contingency_table)
    datatable(
      as.data.frame.matrix(crosstab_values$contingency_table),
      options = dt_options,
      class   = "cell-border stripe hover",
      caption = dt_caption("Tableau de contingence -- Effectifs")
    )
  })

  output$distribVarSelect <- renderUI({
    req(values$filteredData)
    cols <- names(values$filteredData)
    selectInput(ns("distribVar"), "Variable a analyser :",
                choices = cols,
                selected = input$crosstabRowVar %||% cols[1])
  })

  # Distribution d'une seule variable : effectif + proportion relative de chaque
  # modalite (part sur le total de l'ensemble des modalites).
  distribution_df <- reactive({
    req(values$filteredData)
    var <- input$distribVar %||% input$crosstabRowVar
    req(var, var %in% names(values$filteredData))
    x <- values$filteredData[[var]]
    tab <- table(x, useNA = "no")
    if (length(tab) == 0) return(NULL)
    total <- sum(tab)
    df <- data.frame(
      Modalite = names(tab),
      Effectif = as.integer(tab),
      `Proportion (%)` = round(as.numeric(tab) / total * 100, 2),
      `Proportion cumulee (%)` = round(cumsum(as.numeric(tab)) / total * 100, 2),
      check.names = FALSE, stringsAsFactors = FALSE)
    # ligne Total
    df <- rbind(df, data.frame(
      Modalite = "Total", Effectif = total,
      `Proportion (%)` = 100, `Proportion cumulee (%)` = 100,
      check.names = FALSE, stringsAsFactors = FALSE))
    df
  })

  output$crosstabDistribution <- renderDT({
    df <- distribution_df()
    validate(need(!is.null(df), "Sélectionnez une variable a analyser."))
    datatable(df, rownames = FALSE, options = dt_options,
              class = "cell-border stripe hover",
              caption = dt_caption(
                sprintf("Distribution de la variable %s -- effectifs et proportions relatives",
                        input$distribVar %||% input$crosstabRowVar %||% "")))
  })

  output$downloadDistribExcel <- downloadHandler(
    filename = function() paste0("distribution_", input$distribVar %||% "var", ".xlsx"),
    content = function(file) {
      df <- distribution_df(); req(!is.null(df))
      if (requireNamespace("writexl", quietly = TRUE)) writexl::write_xlsx(df, file)
      else utils::write.csv(df, sub("\\.xlsx$", ".csv", file), row.names = FALSE)
    })
  output$downloadDistribCSV <- downloadHandler(
    filename = function() paste0("distribution_", input$distribVar %||% "var", ".csv"),
    content = function(file) {
      df <- distribution_df(); req(!is.null(df))
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  
  output$crosstabRowProp <- renderDT({
    req(crosstab_values$row_proportions)
    datatable(
      round(as.data.frame.matrix(crosstab_values$row_proportions), 2),
      options = dt_options,
      class   = "cell-border stripe hover",
      caption = dt_caption("Proportions en lignes (%)")
    )
  })
  
  output$crosstabColProp <- renderDT({
    req(crosstab_values$col_proportions)
    datatable(
      round(as.data.frame.matrix(crosstab_values$col_proportions), 2),
      options = dt_options,
      class   = "cell-border stripe hover",
      caption = dt_caption("Proportions en colonnes (%)")
    )
  })
  
  output$crosstabTotalProp <- renderDT({
    req(crosstab_values$total_proportions)
    datatable(
      round(as.data.frame.matrix(crosstab_values$total_proportions), 2),
      options = dt_options,
      class   = "cell-border stripe hover",
      caption = dt_caption("Proportions totales (%)")
    )
  })
  
  output$crosstabTests <- renderDT({
    has_chi    <- !is.null(crosstab_values$chi_test)
    has_fisher <- !is.null(crosstab_values$fisher_test)
    req(has_chi || has_fisher)

    rows <- list()

    # --- Test du Chi-deux ---
    if (has_chi && is.list(crosstab_values$chi_test)) {
      ct <- crosstab_values$chi_test
      interpretation <- if (ct$p.value < 0.05) {
        "Association significative (p < 0,05)"
      } else {
        "Pas d'association significative (p >= 0,05)"
      }
      warn_low <- if (any(ct$expected < 5)) {
        " -- Attention : effectifs théoriques < 5 (test peu fiable, préférer Fisher)."
      } else ""

      rows[[length(rows) + 1]] <- data.frame(
        Test           = "Chi-deux (\u03c7\u00b2)",
        Statistique    = sprintf("%.4f", ct$statistic),
        `Degrés de liberté` = as.character(ct$parameter),
        `p-value`      = format.pval(ct$p.value, digits = 4),
        `Interprétation` = paste0(interpretation, warn_low),
        check.names    = FALSE,
        stringsAsFactors = FALSE
      )
    } else if (has_chi) {
      rows[[length(rows) + 1]] <- data.frame(
        Test = "Chi-deux (\u03c7\u00b2)", Statistique = "-",
        `Degrés de liberté` = "-", `p-value` = "-",
        `Interprétation` = as.character(crosstab_values$chi_test),
        check.names = FALSE, stringsAsFactors = FALSE)
    }

    # --- Test exact de Fisher ---
    if (has_fisher && is.list(crosstab_values$fisher_test)) {
      ft <- crosstab_values$fisher_test
      interpretation <- if (ft$p.value < 0.05) {
        "Association significative (p < 0,05)"
      } else {
        "Pas d'association significative (p >= 0,05)"
      }
      extra <- character(0)
      if (!is.null(ft$estimate))
        extra <- c(extra, paste0("Odds ratio = ", round(ft$estimate, 4)))
      if (!is.null(ft$conf.int))
        extra <- c(extra, paste0("IC 95% [", round(ft$conf.int[1], 4),
                                 " ; ", round(ft$conf.int[2], 4), "]"))
      stat_col <- if (length(extra) > 0) paste(extra, collapse = " ; ") else "-"

      rows[[length(rows) + 1]] <- data.frame(
        Test           = "Exact de Fisher",
        Statistique    = stat_col,
        `Degrés de liberté` = "-",
        `p-value`      = format.pval(ft$p.value, digits = 4),
        `Interprétation` = interpretation,
        check.names    = FALSE,
        stringsAsFactors = FALSE
      )
    } else if (has_fisher) {
      rows[[length(rows) + 1]] <- data.frame(
        Test = "Exact de Fisher", Statistique = "-",
        `Degrés de liberté` = "-", `p-value` = "-",
        `Interprétation` = as.character(crosstab_values$fisher_test),
        check.names = FALSE, stringsAsFactors = FALSE)
    }

    df_tests <- do.call(rbind, rows)

    datatable(
      df_tests,
      rownames = FALSE,
      options  = list(dom = "Bfrtip", buttons = .hstat_dt_buttons("tests_crosstab"),
                      ordering = FALSE, paging = FALSE, searching = FALSE,
                      scrollX = TRUE),
      class    = "cell-border stripe hover",
      caption  = dt_caption("Tests d'association entre les deux variables")
    ) |>
      formatStyle("p-value", fontWeight = "bold")
  })
  
  output$crosstabResiduals <- renderDT({
    req(crosstab_values$residuals)
    df_res <- round(as.data.frame.matrix(crosstab_values$residuals), 2)
    datatable(
      df_res,
      options = dt_options,
      class   = "cell-border stripe hover",
      caption = dt_caption("Résidus standardisés  (|valeur| > 2 -> contribution significative)")
    ) |>
      formatStyle(
        columns         = names(df_res),
        backgroundColor = styleInterval(
          c(-2, 2),
          c("#cce5ff", "#ffffff", "#f8d7da")
        )
      )
  })
  
  
  # - 6a. Graphique principal -
  
  output$crosstabPlot <- renderPlot({
    req(crosstab_values$contingency_table,
        input$crosstabRowVar,
        input$crosstabColVar)
    
    df_plot <- as.data.frame(crosstab_values$contingency_table)
    df_plot <- df_plot[df_plot$Var1 != "Sum" & df_plot$Var2 != "Sum", ]
    names(df_plot) <- c("Row_Var", "Col_Var", "Freq")
    df_plot <- data.frame(
      Row_Var = as.character(df_plot$Row_Var),
      Col_Var = as.character(df_plot$Col_Var),
      Freq    = as.numeric(df_plot$Freq),
      stringsAsFactors = FALSE
    )
    
    if (nrow(df_plot) == 0 || any(is.na(df_plot$Freq))) {
      showNotification("Données invalides pour le graphique !", type = "error", duration = 5)
      return(NULL)
    }
    
    x_available   <- unique(df_plot$Row_Var)
    col_available <- unique(df_plot$Col_Var)
    x_order   <- resolve_level_order(input$xLevelsOrder,   x_available)
    col_order <- resolve_level_order(input$colLevelsOrder, col_available)
    
    df_plot$Row_Var <- factor(df_plot$Row_Var, levels = x_order)
    df_plot$Col_Var <- factor(df_plot$Col_Var, levels = col_order)
    
    # - Feature 5 : Renommer les niveaux de l'axe X -
    x_renamed <- rename_levels(levels(df_plot$Row_Var), "xRename_", input)
    levels(df_plot$Row_Var) <- x_renamed
    
    # - Feature 2 : Renommer les niveaux de la légende -
    col_renamed <- rename_levels(levels(df_plot$Col_Var), "colRename_", input)
    levels(df_plot$Col_Var) <- col_renamed
    
    # - Choix des données à représenter (effectifs ou proportions) -
    plot_data_type <- input$plotDataType %||% "counts"
    
    if (plot_data_type == "row_prop") {
      df_plot$Value <- ave(df_plot$Freq, df_plot$Row_Var,
                           FUN = function(x) x / sum(x) * 100)
      y_label_auto  <- "Proportions en lignes (%)"
      label_fmt     <- function(v) paste0(round(v, 1), "%")
    } else if (plot_data_type == "col_prop") {
      df_plot$Value <- ave(df_plot$Freq, df_plot$Col_Var,
                           FUN = function(x) x / sum(x) * 100)
      y_label_auto  <- "Proportions en colonnes (%)"
      label_fmt     <- function(v) paste0(round(v, 1), "%")
    } else {
      df_plot$Value <- df_plot$Freq
      y_label_auto  <- "Effectifs"
      label_fmt     <- function(v) as.character(round(v))
    }
    
    title <- if (!is.null(input$crosstabTitle) && nzchar(input$crosstabTitle)) {
      input$crosstabTitle
    } else {
      paste("Analyse croisée :", input$crosstabRowVar, "x", input$crosstabColVar)
    }
    
    x_label <- if (!is.null(input$crosstabXLabel) && nzchar(input$crosstabXLabel)) {
      input$crosstabXLabel
    } else {
      input$crosstabRowVar
    }
    
    y_label <- if (!is.null(input$crosstabYLabel) && nzchar(input$crosstabYLabel)) {
      input$crosstabYLabel
    } else {
      y_label_auto
    }
    
    # BUG FIX : crosstabLegendTitle maintenant présent dans l'UI
    legend_title <- if (!is.null(input$crosstabLegendTitle) && nzchar(input$crosstabLegendTitle)) {
      input$crosstabLegendTitle
    } else {
      input$crosstabColVar
    }
    
    base_theme  <- build_base_theme(input)
    show_labels <- input$showPercentages %||% TRUE
    # BUG FIX : labelSize maintenant présent dans l'UI
    label_sz    <- input$labelSize %||% 3.5
    n_col_var   <- length(levels(df_plot$Col_Var))
    
    p <- switch(
      input$plotType,
      
      "bar" = {
        g <- ggplot(df_plot, aes(x = Row_Var, y = Value, fill = Col_Var)) +
          geom_bar(stat = "identity", position = "dodge",
                   alpha = 0.88, color = "white", linewidth = 0.3) +
          labs(title = title, x = x_label, y = y_label, fill = legend_title) +
          base_theme
        if (show_labels)
          g <- g + geom_text(aes(label = label_fmt(Value)),
                             position = position_dodge(0.9),
                             vjust = -0.5, size = label_sz, fontface = "bold")
        g
      },
      
      "stacked_bar" = {
        g <- ggplot(df_plot, aes(x = Row_Var, y = Value, fill = Col_Var)) +
          geom_bar(stat = "identity", position = "stack",
                   alpha = 0.88, color = "white", linewidth = 0.3) +
          labs(title = title, x = x_label, y = y_label, fill = legend_title) +
          base_theme
        if (show_labels)
          g <- g + geom_text(aes(label = label_fmt(Value)),
                             position = position_stack(vjust = 0.5),
                             size = label_sz, fontface = "bold", color = "white")
        g
      },
      
      "mosaic" = {
        df_plot$prop <- ave(df_plot$Freq, df_plot$Row_Var,
                            FUN = function(x) x / sum(x))
        g <- ggplot(df_plot, aes(x = Row_Var, y = prop, fill = Col_Var)) +
          geom_bar(stat = "identity", position = "fill",
                   alpha = 0.88, color = "white", linewidth = 0.3) +
          labs(title = title, x = x_label, y = "Proportions (%)",
               fill = legend_title) +
          base_theme +
          scale_y_continuous(labels = scales::percent)
        if (show_labels)
          g <- g + geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
                             position = position_fill(vjust = 0.5),
                             size = label_sz, fontface = "bold", color = "white")
        g
      },
      
      NULL
    )
    
    if (is.null(p)) return(NULL)
    
    p <- apply_color_palette(p, input$colorPalette, n_colors = n_col_var)
    
    crosstab_values$current_plot <- p
    print(p)
  })
  
  # Section d'export -- graphique principal
  # BUG FIX + Feature 3 : DPI max porté à 20 000
  output$plotDownloadSection <- renderUI({
    req(crosstab_values$current_plot)
    
    div(
      style = "background-color:#eafaf1; padding:20px; border-radius:8px; border:1px solid #27ae60;",
      tags$h5(icon("download"), " Paramètres d'exportation",
              style = "font-weight:bold; color:#27ae60; margin-top:0; margin-bottom:15px;"),
      fluidRow(
        column(6, numericInput(ns("mainPlotDPI"), "Résolution (DPI) :",
                               value = 300, min = 72, max = 20000, step = 50,
                               width = "100%")),
        column(6, selectInput(ns("mainPlotFormat"), "Format d'export :",
                              choices = c("PNG (recommandé)" = "png",
                                          "JPEG"             = "jpeg",
                                          "TIFF"             = "tiff",
                                          "PDF (vectoriel)"  = "pdf",
                                          "SVG (web)"        = "svg",
                                          "EPS (publication)"= "eps",
                                          "BMP"              = "bmp"),
                              selected = "png", width = "100%"))
      ),
      tags$small(class = "text-muted",
                 icon("info-circle"),
                 " Au-delà de 600 DPI : réservé à l'impression haute définition."),
      br(),
      downloadButton(ns("downloadPlot"), "Télécharger le graphique",
                     class = "btn-success btn-lg btn-block",
                     icon  = icon("download"),
                     style = paste0("font-weight:bold; font-size:16px; padding:15px;",
                                    "margin-top:12px; background:linear-gradient(to right,#27ae60,#219653);",
                                    "box-shadow:0 4px 6px rgba(0,0,0,.1);"))
    )
  })
  
  # - 6b. Graphique en secteurs -
  
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
      Count    = as.numeric(pie_data),
      stringsAsFactors = FALSE
    )
    df_pie$Percentage <- round(df_pie$Count / sum(df_pie$Count) * 100, 1)
    
    pie_lvl_order <- resolve_level_order(input$pieLevelsOrder, df_pie$Category)
    df_pie$Category <- factor(df_pie$Category, levels = pie_lvl_order)
    
    # BUG FIX : piePlotTitle maintenant présent dans l'UI
    title <- if (!is.null(input$piePlotTitle) && nzchar(input$piePlotTitle)) {
      input$piePlotTitle
    } else if (!is.null(input$crosstabTitle) && nzchar(input$crosstabTitle)) {
      paste0(input$crosstabTitle, " -- ", var_name)
    } else {
      paste("Répartition de :", var_name)
    }
    
    # BUG FIX : pieLegendTitle maintenant présent dans l'UI
    pie_legend_title <- if (!is.null(input$pieLegendTitle) && nzchar(input$pieLegendTitle)) {
      input$pieLegendTitle
    } else {
      var_name
    }
    
    label_sz <- input$labelSize %||% 3.5
    
    p <- ggplot(df_pie, aes(x = "", y = Count, fill = Category)) +
      geom_bar(stat = "identity", width = 1,
               alpha = 0.9, color = "white", linewidth = 0.8) +
      coord_polar(theta = "y") +
      labs(title = title, fill = pie_legend_title) +
      theme_void() +
      theme(
        plot.title   = element_markdown(
          size   = input$titleSize %||% 16,
          hjust  = 0.5,
          face   = "bold",
          margin = margin(b = 20)
        ),
        legend.text  = element_text(size = input$legendTextSize %||% 10),
        legend.title = element_markdown(size = input$legendTextSize %||% 10, face = "bold"),
        legend.position = "right"
      )
    
    if (input$showPercentages %||% TRUE) {
      p <- p + geom_text(aes(label = paste0(Percentage, "%")),
                         position = position_stack(vjust = 0.5),
                         size = label_sz, fontface = "bold", color = "white")
    }
    
    n_cats <- nrow(df_pie)
    p      <- apply_color_palette(p, input$pieColorPalette, n_colors = n_cats)
    
    crosstab_values$current_pie_plot <- p
    print(p)
  })
  
  # Section d'export -- graphique en secteurs
  # BUG FIX + Feature 3 : DPI max porté à 20 000
  output$pieDownloadSection <- renderUI({
    req(crosstab_values$current_pie_plot)
    
    div(
      style = "background-color:#ebf5fb; padding:20px; border-radius:8px; border:1px solid #3498db;",
      tags$h5(icon("download"), " Paramètres d'exportation",
              style = "font-weight:bold; color:#3498db; margin-top:0; margin-bottom:15px;"),
      fluidRow(
        column(6, numericInput(ns("piePlotDPI"), "Résolution (DPI) :",
                               value = 300, min = 72, max = 20000, step = 50,
                               width = "100%")),
        column(6, selectInput(ns("piePlotFormat"), "Format d'export :",
                              choices = c("PNG (recommandé)" = "png",
                                          "JPEG"             = "jpeg",
                                          "TIFF"             = "tiff",
                                          "PDF (vectoriel)"  = "pdf",
                                          "SVG (web)"        = "svg",
                                          "EPS (publication)"= "eps",
                                          "BMP"              = "bmp"),
                              selected = "png", width = "100%"))
      ),
      tags$small(class = "text-muted",
                 icon("info-circle"),
                 " Au-delà de 600 DPI : réservé à l'impression haute définition."),
      br(),
      downloadButton(ns("downloadPiePlot"), "Télécharger le graphique",
                     class = "btn-info btn-lg btn-block",
                     icon  = icon("download"),
                     style = "font-weight:bold; font-size:16px; padding:15px; margin-top:12px; box-shadow:0 4px 6px rgba(0,0,0,.1);")
    )
  })
  
  
  # Helper : sauvegarde ggplot selon format et DPI
  save_ggplot <- function(file, plot, dpi, format,
                          width_in = 10, height_in = 7.5) {
    args <- list(filename = file, plot = plot,
                 width = width_in, height = height_in, device = format)
    
    if (format %in% c("png", "tiff", "bmp", "jpeg")) {
      args$dpi <- dpi
      args$bg  <- "white"
    }
    if (format == "jpeg") args$quality <- 95
    
    do.call(ggsave, args)
  }
  
  
  output$downloadCrosstabExcel <- downloadHandler(
    filename = function() paste0("effectifs_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(crosstab_values$contingency_table)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Effectifs")
      openxlsx::writeData(wb, "Effectifs",
                          as.data.frame.matrix(crosstab_values$contingency_table),
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Effectifs téléchargés (Excel)", type = "message", duration = 3)
    }
  )
  
  output$downloadCrosstabCSV <- downloadHandler(
    filename = function() paste0("effectifs_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(crosstab_values$contingency_table)
      write.csv(as.data.frame.matrix(crosstab_values$contingency_table),
                file, row.names = TRUE)
      showNotification("Effectifs téléchargés (CSV)", type = "message", duration = 3)
    }
  )
  
  output$downloadRowPropExcel <- downloadHandler(
    filename = function() paste0("prop_lignes_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(crosstab_values$row_proportions)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Prop_Lignes")
      openxlsx::writeData(wb, "Prop_Lignes",
                          round(as.data.frame.matrix(crosstab_values$row_proportions), 2),
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Proportions lignes téléchargées (Excel)", type = "message", duration = 3)
    }
  )
  
  output$downloadRowPropCSV <- downloadHandler(
    filename = function() paste0("prop_lignes_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(crosstab_values$row_proportions)
      write.csv(round(as.data.frame.matrix(crosstab_values$row_proportions), 2),
                file, row.names = TRUE)
      showNotification("Proportions lignes téléchargées (CSV)", type = "message", duration = 3)
    }
  )
  
  output$downloadColPropExcel <- downloadHandler(
    filename = function() paste0("prop_colonnes_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(crosstab_values$col_proportions)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Prop_Colonnes")
      openxlsx::writeData(wb, "Prop_Colonnes",
                          round(as.data.frame.matrix(crosstab_values$col_proportions), 2),
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Proportions colonnes téléchargées (Excel)", type = "message", duration = 3)
    }
  )
  
  output$downloadColPropCSV <- downloadHandler(
    filename = function() paste0("prop_colonnes_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(crosstab_values$col_proportions)
      write.csv(round(as.data.frame.matrix(crosstab_values$col_proportions), 2),
                file, row.names = TRUE)
      showNotification("Proportions colonnes téléchargées (CSV)", type = "message", duration = 3)
    }
  )
  
  output$downloadTotalPropExcel <- downloadHandler(
    filename = function() paste0("prop_totales_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(crosstab_values$total_proportions)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Prop_Totales")
      openxlsx::writeData(wb, "Prop_Totales",
                          round(as.data.frame.matrix(crosstab_values$total_proportions), 2),
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Proportions totales téléchargées (Excel)", type = "message", duration = 3)
    }
  )
  
  output$downloadTotalPropCSV <- downloadHandler(
    filename = function() paste0("prop_totales_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(crosstab_values$total_proportions)
      write.csv(round(as.data.frame.matrix(crosstab_values$total_proportions), 2),
                file, row.names = TRUE)
      showNotification("Proportions totales téléchargées (CSV)", type = "message", duration = 3)
    }
  )
  
  # Helper interne : construction du data frame des tests pour l'export
  build_tests_df <- function() {
    tests_df <- data.frame(
      Test           = character(),
      Statistique    = numeric(),
      DDL            = integer(),
      p_value        = numeric(),
      Interpretation = character(),
      stringsAsFactors = FALSE
    )
    
    if (is.list(crosstab_values$chi_test)) {
      ct <- crosstab_values$chi_test
      tests_df <- rbind(tests_df, data.frame(
        Test           = "Chi-deux",
        Statistique    = round(ct$statistic, 4),
        DDL            = ct$parameter,
        p_value        = ct$p.value,
        Interpretation = ifelse(ct$p.value < 0.05, "Significatif", "Non significatif"),
        stringsAsFactors = FALSE
      ))
    }
    
    if (is.list(crosstab_values$fisher_test)) {
      ft <- crosstab_values$fisher_test
      tests_df <- rbind(tests_df, data.frame(
        Test           = "Fisher exact",
        Statistique    = NA_real_,
        DDL            = NA_integer_,
        p_value        = ft$p.value,
        Interpretation = ifelse(ft$p.value < 0.05, "Significatif", "Non significatif"),
        stringsAsFactors = FALSE
      ))
    }
    tests_df
  }
  
  output$downloadTestsExcel <- downloadHandler(
    filename = function() paste0("tests_statistiques_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Tests")
      openxlsx::writeData(wb, "Tests", build_tests_df())
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Tests statistiques téléchargés (Excel)", type = "message", duration = 3)
    }
  )
  
  output$downloadTestsCSV <- downloadHandler(
    filename = function() paste0("tests_statistiques_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(build_tests_df(), file, row.names = FALSE)
      showNotification("Tests statistiques téléchargés (CSV)", type = "message", duration = 3)
    }
  )
  
  output$downloadResidualsExcel <- downloadHandler(
    filename = function() paste0("residus_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(crosstab_values$residuals)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Residus")
      openxlsx::writeData(wb, "Residus",
                          round(as.data.frame.matrix(crosstab_values$residuals), 2),
                          rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Résidus téléchargés (Excel)", type = "message", duration = 3)
    }
  )
  
  output$downloadResidualsCSV <- downloadHandler(
    filename = function() paste0("residus_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(crosstab_values$residuals)
      write.csv(round(as.data.frame.matrix(crosstab_values$residuals), 2),
                file, row.names = TRUE)
      showNotification("Résidus téléchargés (CSV)", type = "message", duration = 3)
    }
  )
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      fmt <- input$mainPlotFormat %||% "png"
      dpi <- input$mainPlotDPI    %||% 300
      paste0("graphique_croisé_", Sys.Date(), "_", dpi, "dpi.", fmt)
    },
    content = function(file) {
      req(crosstab_values$current_plot)
      dpi <- input$mainPlotDPI    %||% 300
      fmt <- input$mainPlotFormat %||% "png"
      tryCatch({
        save_ggplot(file, crosstab_values$current_plot, dpi, fmt,
                    width_in = 10, height_in = 7.5)
        showNotification(
          paste0("Graphique téléchargé (", round(10 * dpi), "x",
                 round(7.5 * dpi), " px, ", dpi, " DPI, ", toupper(fmt), ")"),
          type = "message", duration = 5
        )
      }, error = function(e) {
        showNotification(paste("Erreur export :", e$message), type = "error", duration = 10)
      })
    }
  )
  
  output$downloadPiePlot <- downloadHandler(
    filename = function() {
      fmt <- input$piePlotFormat %||% "png"
      dpi <- input$piePlotDPI    %||% 300
      paste0("graphique_secteurs_", Sys.Date(), "_", dpi, "dpi.", fmt)
    },
    content = function(file) {
      req(crosstab_values$current_pie_plot)
      dpi <- input$piePlotDPI    %||% 300
      fmt <- input$piePlotFormat %||% "png"
      tryCatch({
        save_ggplot(file, crosstab_values$current_pie_plot, dpi, fmt,
                    width_in = 8, height_in = 8)
        showNotification(
          paste0("Graphique en secteurs téléchargé (", round(8 * dpi), "x",
                 round(8 * dpi), " px, ", dpi, " DPI, ", toupper(fmt), ")"),
          type = "message", duration = 5
        )
      }, error = function(e) {
        showNotification(paste("Erreur export :", e$message), type = "error", duration = 10)
      })
    }
  )
  })
}
