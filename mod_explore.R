################################################################################
#
#  Module Shiny : Exploration des donnees
#
#  Onglet migre en module Shiny (3e migration). Lecture seule sur les donnees
#  partagees ; n'ecrit rien dans le reactiveValues global.
#  - mod_explore_ui(id)             : contenu de l'onglet (dans un tabItem)
#  - mod_explore_server(id, values) : 'values' = reactiveValues GLOBAL
#    (lit data, dataMode, dbCon, dbTable).
################################################################################

mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
              .hstat_scope_banner(exact = FALSE),
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
                  verbatimTextOutput(ns("dataStructure")),
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
                  verbatimTextOutput(ns("dataSummary")),
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
                             uiOutput(ns("corrVarSelect"))
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
                                   selectInput(ns("corrMethod"), 
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
                                   selectInput(ns("corrDisplay"), 
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
                                   selectInput(ns("corrType"), 
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
                                   sliderInput(ns("corrTextSize"), 
                                     "Taille des valeurs:", 
                                     min = 0.3, max = 2, value = 0.8, step = 0.1,
                                     ticks = FALSE
                                   ),
                                   sliderInput(ns("corrLabelSize"), 
                                     "Taille des labels:", 
                                     min = 0.3, max = 2, value = 0.8, step = 0.1,
                                     ticks = FALSE
                                   )
                            ),
                            
                            column(3,
                                   h5(icon("heading"), "Titre", style = "color: #27ae60; font-weight: bold;"),
                                   textInput(ns("corrTitle"), 
                                     "Titre personnalisé:", 
                                     placeholder = "Laisser vide pour titre auto"
                                   ),
                                   checkboxInput(ns("corrCenterTitle"), 
                                     tagList(icon("align-center"), " Centrer le titre"), 
                                     value = TRUE
                                   ),
                                   numericInput(ns("corrDPI"), 
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
                             downloadButton(ns("downloadCorrPlot"), 
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
                      plotOutput(ns("corrPlot"), height = "600px"),
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
                  
                  uiOutput(ns("distVarSelect")),
                  
                  # Selection des options
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
                                   sliderInput(ns("distTitleSize"), "Taille titre:", 
                                               min = 8, max = 24, value = 14, ticks = FALSE),
                                   sliderInput(ns("distAxisTitleSize"), "Taille titres axes:", 
                                               min = 8, max = 20, value = 12, ticks = FALSE),
                                   sliderInput(ns("distAxisTextSize"), "Taille texte axes:", 
                                               min = 6, max = 16, value = 10, ticks = FALSE),
                                   sliderInput(ns("distLegendTextSize"), "Taille texte légende:",
                                               min = 6, max = 16, value = 10, ticks = FALSE)
                            ),
                            column(6,
                                   h5(icon("heading"), "Personnalisation", style = "color: #3498db; font-weight: bold;"),
                                   textInput(ns("distTitle"), "Titre personnalisé:", 
                                             placeholder = "Laisser vide pour titre auto"),
                                   checkboxInput(ns("distCenterTitle"), 
                                                 tagList(icon("align-center"), " Centrer le titre"), 
                                                 value = TRUE),
                                   checkboxInput(ns("distShowDensity"), 
                                                 tagList(icon("wave-square"), " Afficher courbe densité"), 
                                                 value = TRUE),
                                   numericInput(ns("distDPI"), tagList(icon("image"), " DPI export:"), 
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
                    downloadButton(ns("downloadDistPlot"), 
                                   tagList(icon("download"), " Télécharger PNG"), 
                                   class = "btn-info")
                  ),
                  
                  div(
                    style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    withSpinner(
                      plotOutput(ns("distPlot"), height = "500px"),
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
                  
                  # Selection des options
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
                                   sliderInput(ns("missingTitleSize"), "Taille titre:", 
                                               min = 8, max = 24, value = 14, ticks = FALSE),
                                   sliderInput(ns("missingAxisTitleSize"), "Taille titres axes:", 
                                               min = 8, max = 20, value = 12, ticks = FALSE)
                            ),
                            column(4,
                                   h5(icon("palette"), "Affichage", style = "color: #f39c12; font-weight: bold;"),
                                   sliderInput(ns("missingAxisTextSize"), "Taille texte axes:", 
                                               min = 6, max = 16, value = 10, ticks = FALSE),
                                   checkboxInput(ns("missingRotateLabels"), 
                                                 tagList(icon("sync-alt"), " Incliner labels X"), 
                                                 value = TRUE)
                            ),
                            column(4,
                                   h5(icon("heading"), "Personnalisation", style = "color: #f39c12; font-weight: bold;"),
                                   textInput(ns("missingTitle"), "Titre personnalisé:", 
                                             placeholder = "Laisser vide pour titre auto"),
                                   checkboxInput(ns("missingCenterTitle"), 
                                                 tagList(icon("align-center"), " Centrer le titre"), 
                                                 value = TRUE),
                                   numericInput(ns("missingDPI"), tagList(icon("image"), " DPI export:"), 
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
                    downloadButton(ns("downloadMissingPlot"), 
                                   tagList(icon("download"), " Télécharger PNG"), 
                                   class = "btn-info")
                  ),
                  
                  div(
                    style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    withSpinner(
                      plotOutput(ns("missingPlot"), height = "400px"),
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
  )
}

mod_explore_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
        inputId = ns("corrVars"),
        label = "Sélectionnez les variables numériques:", 
        choices = num_cols,
        multiple = TRUE,
        selected = num_cols[1:min(5, length(num_cols))],
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      actionButton(ns("selectAllCorrVars"), "Tout sélectionner", 
                   class = "btn-success btn-sm", icon = icon("check-square")),
      actionButton(ns("deselectAllCorrVars"), "Tout désélectionner", 
                   class = "btn-danger btn-sm", icon = icon("square")),
      if (identical(values$dataMode, "duckdb") && !is.null(values$dbCon))
        div(style = "margin-top:10px; padding:8px 10px; background:#fff4e5; border:1px solid #ed6c02; border-radius:6px;",
          checkboxInput(ns("corrFullData"),
            HTML("<b><i class='fa fa-server'></i> Corrélation exacte sur le jeu complet</b>"),
            value = FALSE),
          tags$small(style = "color:#7a4a1a;",
            "Calcul exact via DuckDB (méthode de Pearson uniquement)."))
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
                                 title = NULL, center_title = TRUE, full_cor = FALSE) {
    
    if (is.null(vars) || length(vars) < 2) {
      return(NULL)
    }
    
    cor_data <- data[, vars, drop = FALSE]
    # Supprimer colonnes à variance nulle avant cor() pour éviter warnings
    cor_data <- remove_zero_var_cols(cor_data)
    if (ncol(cor_data) < 2) {
      showNotification("Moins de 2 variables avec variance non nulle pour la corrélation.", type="warning", duration=5)
      return(NULL)
    }
    cor_matrix <- NULL
    # Mode hors-memoire + option activee : correlation EXACTE sur le jeu complet.
    # DuckDB CORR() calcule la correlation de Pearson uniquement.
    if (isTRUE(full_cor) && identical(values$dataMode, "duckdb") &&
        !is.null(values$dbCon) && method == "pearson") {
      cor_matrix <- tryCatch(
        hstat_duckdb_cor(values$dbCon, values$dbTable, names(cor_data)),
        error = function(e) NULL)
    }
    if (is.null(cor_matrix))
      cor_matrix <- suppressWarnings(cor(cor_data, use = "complete.obs", method = method))
    
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
        center_title = params$center_title,
        full_cor = isTRUE(input$corrFullData)
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
          center_title = params$center_title,
          full_cor = isTRUE(input$corrFullData)
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
    
    selectInput(ns("distVar"), "Sélectionnez une variable:", 
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
    
    p <- ggplot(data, aes(x = .data[[var]])) +
      geom_histogram(aes(y = after_stat(density)), fill = "lightblue", 
                     color = "black", alpha = 0.7, bins = 30)
    
    if (show_density) {
      p <- p + geom_density(color = "red", linewidth = 1.2)
    }
    
    p <- p + theme_minimal() +
      labs(title = plot_title, x = var, y = "Densité") +
      theme(
        plot.title = element_markdown(size = title_size, hjust = if (center_title) 0.5 else 0),
        axis.title = element_markdown(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        legend.text = element_text(size = legend_text_size),
        legend.title = element_markdown(size = legend_text_size)
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
        ggsave(file, plot = p, width = 10, height = 8, dpi = dpi)
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
        plot.title = element_markdown(size = title_size, hjust = if (center_title) 0.5 else 0),
        axis.title = element_markdown(size = axis_title_size),
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
        ggsave(file, plot = p, width = 12, height = 8, dpi = dpi)
        showNotification("Graphique téléchargé avec succès!", type = "message", duration = 3)
      }
    }
  )
  })
}
