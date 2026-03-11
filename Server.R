server <- function(input, output, session) {

  auto_quote_colnames <- function(formula_str, col_names) {
    # ── 1. Protéger les noms de colonnes spéciaux
    cols_sorted <- col_names[order(nchar(col_names), decreasing = TRUE)]
    for (col in cols_sorted) {
      if (grepl("[-/ +*^()%$@!?]|^[0-9]", col, perl = TRUE)) {
        backtick_col <- paste0("`", col, "`")
        if (!grepl(backtick_col, formula_str, fixed = TRUE)) {
          formula_str <- gsub(col, backtick_col, formula_str, fixed = TRUE)
        }
      }
    }
    
    # ── 2. Ajouter des variables à partir des functions
    # Cas : mean(c(A, B, C))
    formula_str <- gsub(
      "\\bmean\\s*\\(\\s*c\\s*\\(([^)]+)\\)\\s*\\)",
      "rowMeans(cbind(\\1), na.rm=TRUE)",
      formula_str, perl = TRUE
    )
    # Cas : mean(A, B) ou mean(A, B, C) 
    formula_str <- gsub(
      "\\bmean\\s*\\(([^)]+,[^)]+)\\)",
      "rowMeans(cbind(\\1), na.rm=TRUE)",
      formula_str, perl = TRUE
    )
    
    # ── 3. Transformer sum(c(...)) 
    # Cas : sum(c(A, B, C))
    formula_str <- gsub(
      "\\bsum\\s*\\(\\s*c\\s*\\(([^)]+)\\)\\s*\\)",
      "rowSums(cbind(\\1), na.rm=TRUE)",
      formula_str, perl = TRUE
    )
    # Cas : sum(A, B) ou sum(A, B, C)
    formula_str <- gsub(
      "\\bsum\\s*\\(([^)]+,[^)]+)\\)",
      "rowSums(cbind(\\1), na.rm=TRUE)",
      formula_str, perl = TRUE
    )
    
    formula_str
  }

  options(shiny.error = function() {
    msg <- tryCatch(conditionMessage(sys.call()), error = function(e) "Erreur inconnue")
    shinyjs::logjs(paste("[HStat] Erreur non capturée:", msg))
  })

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
    allTestResults = list(),  
    allPostHocResults = list(),  
    modelsList = list(),  
    normalityResultsPerVar = list(), homogeneityResultsPerVar = list(),  
    currentDiagVar = 1, currentResidVar = 1,
    customXOrder = NULL,
    y2Vars = NULL,
    dualAxisActive = FALSE,
    y2VarsActive = NULL,
    postHocSyncTrigger = NULL
  )
  
  values$customXLevels <- reactiveVal(NULL)
  

  # Format d'affichage des dates
  get_date_display_fmt <- function() {
    fmt <- input$xDateDisplayFormat %||% "%d-%m-%Y"
    if (viz_valid_date_fmt(fmt)) fmt else "%d-%m-%Y"
  }

  # Thème ggplot2 
  get_plot_theme <- function(base_size = 12) {
    viz_get_theme(input$plotTheme %||% "minimal", base_size = base_size)
  }

  get_x_scale <- function(data, x_var) {
    viz_get_x_scale(
      x_col      = data[[x_var]],
      disp_fmt   = get_date_display_fmt(),                       # format d'affichage
      label_map  = values$storedLevelLabels[[x_var]],            # renommage étiquettes
      custom_ord = values$customXOrder                           # ordre personnalisé
    )
  }
  
  
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
    filename = function() { "exemple_donnees.csv" },
    content  = function(file) {
      set.seed(123)
      n  <- 120
      # Données agronomiques réalistes avec noms de colonnes variés
      exemple <- data.frame(
        Traitement       = factor(rep(c("A","B","C","D"), each = n/4)),
        Bloc             = factor(rep(1:4, n/4)),
        Genre            = factor(sample(c("M","F"), n, replace = TRUE)),
        Annee            = factor(sample(2020:2022, n, replace = TRUE)),
        Zone             = factor(sample(c("Nord","Sud","Est","Ouest"), n, replace = TRUE)),
        Age              = sample(18:65, n, replace = TRUE),
        Variable1        = c(rnorm(n/4,10,2), rnorm(n/4,12,2), rnorm(n/4,15,2), rnorm(n/4,11,2)),
        Variable2        = c(rnorm(n/4,20,3), rnorm(n/4,22,3), rnorm(n/4,25,3), rnorm(n/4,21,3)),
        Variable3        = c(rnorm(n/4, 5,1), rnorm(n/4, 6,1), rnorm(n/4, 7,1), rnorm(n/4,5.5,1)),
        Variable4        = c(rnorm(n/4,100,10),rnorm(n/4,110,10),rnorm(n/4,120,10),rnorm(n/4,105,10)),
        Rendement        = c(rnorm(n/4,30,5), rnorm(n/4,35,5), rnorm(n/4,40,5), rnorm(n/4,32,5)),
        Taux_germination = round(runif(n, 60, 99), 1),
        stringsAsFactors = FALSE
      )
      # Introduire des NAs réalistes
      for (col in c("Variable1","Variable2","Variable3","Variable4","Rendement")) {
        exemple[sample(n, 5), col] <- NA
      }
      tryCatch(
        write.csv(exemple, file, row.names = FALSE, fileEncoding = "UTF-8"),
        error = function(e) write.csv(exemple, file, row.names = FALSE)
      )
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
    # Supprimer colonnes à variance nulle avant cor() pour éviter warnings
    cor_data <- remove_zero_var_cols(cor_data)
    if (ncol(cor_data) < 2) {
      showNotification("Moins de 2 variables avec variance non nulle pour la corrélation.", type="warning", duration=5)
      return(NULL)
    }
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
            showNotification(paste("Erreur pour `", col, "` :", e$message), 
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
      ui = tagList(icon("trash"), paste(" Variable `", var_name, "` supprimée avec succès")),
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
        ui = tagList(icon("plus"), paste(" Variable `", input$newVarName, "` ajoutée avec succès")),
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
    # Backtick-quoting automatique si le nom contient des espaces ou caractères spéciaux
    col_safe <- if (grepl("[/+*^()%$@!? -]", input$colInsert, perl = TRUE) || grepl("^[0-9]", input$colInsert)) {
      paste0("`", input$colInsert, "`")
    } else {
      input$colInsert
    }
    new_formula <- paste0(current_formula,
                          ifelse(nchar(current_formula) > 0, " ", ""),
                          col_safe)
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
  observeEvent(input$insertLog10, { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " log10()")) })
  observeEvent(input$insertAbs,   { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " abs()")) })
  observeEvent(input$insertRound, { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " round(,2)")) })
  observeEvent(input$insertExp,   { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " exp()")) })
  observeEvent(input$insertMean,  { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " mean()")) })
  observeEvent(input$insertSum,   { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " sum()")) })
  observeEvent(input$insertPow,   { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", "^")) })
  observeEvent(input$insertParen, { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", "()")) })
  observeEvent(input$insertIfelse,{ updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " ifelse(, , )")) })
  observeEvent(input$insertIsNA,  { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " is.na()")) })
  
  # Insérer une condition sur lignes dans la formule
  output$rowCondPicker <- renderUI({
    req(values$cleanData)
    tagList(
      selectInput("rowCondCol", "Colonne :", choices = c("", names(values$cleanData))),
      conditionalPanel(
        condition = "input.rowCondCol != ''",
        fluidRow(
          column(6, selectInput("rowCondOp", "Opérateur :",
            choices = c("==" = "==", "!=" = "!=", ">" = ">", ">=" = ">=", "<" = "<", "<=" = "<=", "is.na" = "is.na"),
            selected = "==")),
          column(6, textInput("rowCondVal", "Valeur :", placeholder = "ex: 'A' ou 10"))
        ),
        actionButton("insertRowCond", tagList(icon("filter"), " Insérer condition"),
                     class = "btn-success btn-sm btn-block")
      )
    )
  })
  
  observeEvent(input$insertRowCond, {
    req(input$rowCondCol, input$rowCondCol != "")
    col_safe_cond <- if (grepl("[-/ +*^()%$@!?]|^[0-9]", input$rowCondCol, perl = TRUE)) {
      paste0("`", input$rowCondCol, "`")
    } else { input$rowCondCol }
    cond <- if (input$rowCondOp == "is.na") {
      paste0("is.na(", col_safe_cond, ")")
    } else {
      paste0(col_safe_cond, " ", input$rowCondOp, " ", input$rowCondVal)
    }
    cur <- input$calcFormula %||% ""
    updateTextInput(session, "calcFormula",
      value = paste0("ifelse(", cond, ", ", cur, ", NA)"))
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
      # Auto-protection des noms de colonnes avec espaces/caractères spéciaux
      formula_safe <- auto_quote_colnames(input$calcFormula, names(values$cleanData))
      new_col <- with(values$cleanData, eval(parse(text = formula_safe)))
      
      if (length(new_col) == 1) {
        
        new_col <- rep(new_col, nrow(values$cleanData))
      }
      
      if (length(new_col) != nrow(values$cleanData)) {
        showNotification(
          tagList(
            icon("exclamation-triangle"),
            " Formule incorrecte : le résultat a ", length(new_col), " valeur(s) ",
            "au lieu de ", nrow(values$cleanData), ". ",
            tags$br(),
            tags$small("Astuce : utilisez rowMeans(cbind(Var1, Var2)) pour la moyenne ligne par ligne.")
          ),
          type = "error", duration = 8)
        return()
      }
      
      values$cleanData[[input$calcVarName]] <- new_col
      values$filteredData <- values$cleanData
      
      showNotification(
        ui = tagList(icon("calculator"), paste(" Variable `", input$calcVarName, "` créée avec succès!")),
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
              showNotification(paste("La variable `", col, "` n'est pas numérique. Moyenne impossible."), 
                               type = "warning", duration = 3)
            }
          } else if (input$naMethod == "median") {
            if (is.numeric(data_temp[[col]])) {
              median_val <- median(data_temp[[col]], na.rm = TRUE)
              data_temp[[col]][is.na(data_temp[[col]])] <- median_val
            } else {
              showNotification(paste("La variable `", col, "` n'est pas numérique. Médiane impossible."), 
                               type = "warning", duration = 3)
            }
          } else if (input$naMethod == "value") {
            data_temp[[col]][is.na(data_temp[[col]])] <- input$naValue
          }
        }, error = function(e) {
          showNotification(paste("Erreur pour `", col, "` :", e$message), 
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
  
  
  #  Filtre par plage de lignes 
  
  output$rowRangeUI <- renderUI({
    req(values$cleanData)
    max_rows <- nrow(values$cleanData)
    tagList(
      textAreaInput("rowSelection", "Sélection de lignes :",
                    placeholder = "Exemples :\n1 à 10\n1,3,4,5,10,15,20\n1,3,4,5,10,15,20 à 30",
                    rows = 3),
      helpText(HTML(paste0(
        "<b>Formats acceptés :</b><br>",
        "• Plage : <code>1 à 10</code> ou <code>5-15</code><br>",
        "• Liste : <code>1,3,4,5,10,15,20</code><br>",
        "• Combinaison : <code>1,3,5,10 à 15,20,25 à 30</code><br>",
        "Total de lignes disponibles : ", max_rows
      )))
    )
  })
  
  # Fonction pour parser la sélection de lignes
  parseRowSelection <- function(selection_text, max_rows) {
    if (is.null(selection_text) || nchar(trimws(selection_text)) == 0) {
      stop("Veuillez entrer une sélection de lignes valide.")
    }
    
    # Nettoyer le texte
    text <- trimws(selection_text)
    text <- gsub("\\s+", " ", text)  
    
    # Remplacer différents formats de plage par un format uniforme
    text <- gsub("à", "-", text, ignore.case = TRUE)
    text <- gsub("a", "-", text, ignore.case = TRUE)
    
    # Séparer par virgules
    parts <- strsplit(text, ",")[[1]]
    parts <- trimws(parts)
    
    all_rows <- c()
    
    for (part in parts) {
      if (grepl("-", part)) {
        # C'est une plage (ex: "1-10" ou "20-30")
        range_parts <- strsplit(part, "-")[[1]]
        range_parts <- trimws(range_parts)
        
        if (length(range_parts) != 2) {
          stop(paste("Format de plage invalide :", part))
        }
        
        start <- as.numeric(range_parts[1])
        end <- as.numeric(range_parts[2])
        
        if (is.na(start) || is.na(end)) {
          stop(paste("Plage invalide :", part))
        }
        
        if (start > end) {
          stop(paste("La ligne de début doit être <= ligne de fin dans :", part))
        }
        
        if (start < 1 || end > max_rows) {
          stop(paste("Plage hors limites :", part, "(min: 1, max:", max_rows, ")"))
        }
        
        all_rows <- c(all_rows, start:end)
        
      } else {
        # C'est un numéro individuel
        row_num <- as.numeric(part)
        
        if (is.na(row_num)) {
          stop(paste("Numéro de ligne invalide :", part))
        }
        
        if (row_num < 1 || row_num > max_rows) {
          stop(paste("Ligne hors limites :", row_num, "(min: 1, max:", max_rows, ")"))
        }
        
        all_rows <- c(all_rows, row_num)
      }
    }
    
    # Enlever les doublons et trier
    all_rows <- unique(sort(all_rows))
    
    return(all_rows)
  }
  
  observeEvent(input$applyRowRange, {
    req(values$cleanData, input$rowSelection)
    
    tryCatch({
      max_rows <- nrow(values$cleanData)
      selected_rows <- parseRowSelection(input$rowSelection, max_rows)
      
      filtered <- values$cleanData[selected_rows, ]
      values$filteredData <- filtered
      
      showNotification(
        paste("Filtre par plage appliqué.", length(selected_rows), "lignes sélectionnées"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Erreur filtre par plage:", e$message), type = "error", duration = 10)
    })
  })
  
  # ── Suppression de lignes 
  
  # Aperçu des lignes qui seront supprimées
  output$deleteRowsPreview <- renderUI({
    req(values$cleanData)
    txt <- input$deleteRowsInput %||% ""
    if (nchar(trimws(txt)) == 0) return(NULL)

    max_rows <- nrow(values$cleanData)
    rows <- tryCatch(
      parseRowSelection(txt, max_rows),
      error = function(e) NULL
    )
    if (is.null(rows)) {
      return(div(
        style = "margin-top: 8px; padding: 8px; background-color: #ffcdd2; border-radius: 4px;",
        icon("times-circle", style = "color: #c62828;"),
        tags$span(style = "color: #c62828; font-size: 12px; margin-left: 5px;",
                  "Format invalide — vérifiez votre saisie.")
      ))
    }
    n <- length(rows)
    preview_ids <- head(rows, 8)
    preview_str <- paste(preview_ids, collapse = ", ")
    if (n > 8) preview_str <- paste0(preview_str, " ... (", n - 8, " de plus)")

    div(
      style = "margin-top: 8px; padding: 10px; background-color: #fff3e0; border-radius: 4px; border-left: 3px solid #f57c00;",
      icon("info-circle", style = "color: #f57c00;"),
      tags$span(style = "color: #e65100; font-size: 12px; font-weight: bold; margin-left: 5px;",
                paste0(n, " ligne(s) à supprimer : ")),
      tags$code(style = "font-size: 11px;", preview_str)
    )
  })

  # Supprimer les lignes sélectionnées de cleanData et filteredData
  observeEvent(input$applyDeleteRows, {
    req(values$cleanData, input$deleteRowsInput)
    tryCatch({
      max_rows <- nrow(values$cleanData)
      rows_to_delete <- parseRowSelection(input$deleteRowsInput, max_rows)

      if (length(rows_to_delete) == 0) {
        showNotification("Aucune ligne sélectionnée.", type = "warning", duration = 4)
        return()
      }
      if (length(rows_to_delete) >= max_rows) {
        showNotification("Impossible de supprimer toutes les lignes.", type = "error", duration = 5)
        return()
      }

      values$cleanData    <- values$cleanData[-rows_to_delete, ]
      values$filteredData <- values$cleanData

      showNotification(
        paste0(length(rows_to_delete), " ligne(s) supprimée(s). ",
               nrow(values$cleanData), " lignes restantes."),
        type = "message", duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Erreur suppression :", e$message), type = "error", duration = 8)
    })
  })
  
  # ── Tableau interactif pour sélection des lignes à supprimer
  output$deleteRowsTable <- DT::renderDataTable({
    req(values$cleanData)
    DT::datatable(
      head(values$cleanData, 500),   
      selection  = "multiple",
      extensions = "Scroller",
      options    = list(
        scrollY    = "280px",
        scroller   = TRUE,
        pageLength = 25,
        dom        = "fti",
        language   = list(
          search      = "Rechercher :",
          info        = "Lignes _START_ à _END_ sur _TOTAL_",
          infoEmpty   = "Aucune ligne",
          emptyTable  = "Aucune donnée disponible"
        )
      ),
      rownames = TRUE,
      class    = "cell-border stripe hover compact"
    )
  })
  
  output$deleteRowsInteractivePreview <- renderUI({
    sel <- input$deleteRowsTable_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(div(style = "padding: 8px; color: #6c757d; font-size: 12px;",
                 icon("info-circle"), " Aucune ligne sélectionnée — cliquez sur des lignes dans le tableau."))
    }
    div(style = "padding: 10px; background-color: #fff3e0; border-radius: 4px; border-left: 3px solid #f57c00;",
        icon("info-circle", style = "color: #f57c00;"),
        tags$span(style = "color: #e65100; font-size: 12px; font-weight: bold; margin-left: 5px;",
                  paste0(length(sel), " ligne(s) sélectionnée(s) : ")),
        tags$code(style = "font-size: 11px;", paste(head(sel, 10), collapse = ", "),
                  if (length(sel) > 10) paste0(" ... +", length(sel)-10, " autres") else "")
    )
  })
  
  observeEvent(input$applyDeleteRowsInteractive, {
    sel <- input$deleteRowsTable_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Aucune ligne sélectionnée dans le tableau.", type = "warning", duration = 4)
      return()
    }
    req(values$cleanData)
    if (length(sel) >= nrow(values$cleanData)) {
      showNotification("Impossible de supprimer toutes les lignes.", type = "error", duration = 5)
      return()
    }
    values$cleanData    <- values$cleanData[-sel, ]
    values$filteredData <- values$cleanData
    showNotification(
      paste0(length(sel), " ligne(s) supprimée(s). ", nrow(values$cleanData), " lignes restantes."),
      type = "message", duration = 5)
  })

  #  Filtre par valeur(s) 
  
  # Permet de rechercher des lignes contenant une ou plusieurs valeurs spécifiques
  output$valueFilterUI <- renderUI({
    req(values$cleanData)
    col_names <- names(values$cleanData)
    tagList(
      selectInput("valueFilterCol", "Colonne à filtrer :",
                  choices = col_names, selected = col_names[1]),
      textAreaInput("valueFilterText", "Valeur(s) à rechercher (une par ligne) :",
                    placeholder = "Aout 04-10\nSeptembre 01-07\nJuillet 28-03",
                    rows = 4),
      checkboxInput("valueFilterExact", "Correspondance exacte", FALSE),
      checkboxInput("valueFilterCaseSensitive", "Sensible à la casse", FALSE),
      helpText("Entrez une ou plusieurs valeurs, une par ligne. Les lignes contenant au moins une de ces valeurs seront conservées.")
    )
  })
  
  observeEvent(input$applyValueFilter, {
    req(values$cleanData, input$valueFilterCol, input$valueFilterText)
    
    validate(need(nchar(trimws(input$valueFilterText)) > 0, "Veuillez entrer au moins une valeur à rechercher."))
    
    tryCatch({
      df <- values$cleanData
      col_name <- input$valueFilterCol
      
      # Séparer les valeurs par ligne et nettoyer
      search_values <- strsplit(input$valueFilterText, "\n")[[1]]
      search_values <- trimws(search_values)
      search_values <- search_values[nchar(search_values) > 0]  
      
      validate(need(length(search_values) > 0, "Veuillez entrer au moins une valeur valide."))
      
      # Convertir en caractère pour la recherche
      col_data <- as.character(df[[col_name]])
      
      # Créer un masque pour chaque valeur de recherche
      mask <- rep(FALSE, length(col_data))
      
      for (search_value in search_values) {
        if (input$valueFilterExact) {
          # Correspondance exacte
          if (input$valueFilterCaseSensitive) {
            mask <- mask | (col_data == search_value)
          } else {
            mask <- mask | (tolower(col_data) == tolower(search_value))
          }
        } else {
          # Correspondance partielle 
          if (input$valueFilterCaseSensitive) {
            mask <- mask | grepl(search_value, col_data, fixed = TRUE)
          } else {
            mask <- mask | grepl(search_value, col_data, ignore.case = TRUE)
          }
        }
      }
      
      # Gérer les NA
      mask[is.na(mask)] <- FALSE
      
      filtered <- df[mask, ]
      values$filteredData <- filtered
      
      showNotification(
        paste("Filtre par valeur appliqué:", nrow(filtered), "lignes trouvées avec", length(search_values), "valeur(s)"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Erreur filtre par valeur:", e$message), type = "error", duration = 10)
    })
  })
  
  #  Filtre par sélection de colonnes 

  output$columnSelectUI <- renderUI({
    req(values$cleanData)
    col_names <- names(values$cleanData)
    
    tagList(
      checkboxGroupInput("selectedColumns", "Sélectionner les colonnes à conserver :",
                         choices = col_names,
                         selected = col_names,
                         inline = FALSE),
      fluidRow(
        column(6,
               actionButton("selectAllCols", "Tout sélectionner", 
                            class = "btn-sm btn-default btn-block", icon = icon("check-square"))
        ),
        column(6,
               actionButton("deselectAllCols", "Tout désélectionner", 
                            class = "btn-sm btn-default btn-block", icon = icon("square"))
        )
      )
    )
  })
  
  observeEvent(input$selectAllCols, {
    req(values$cleanData)
    col_names <- names(values$cleanData)
    updateCheckboxGroupInput(session, "selectedColumns", selected = col_names)
  })
  
  observeEvent(input$deselectAllCols, {
    updateCheckboxGroupInput(session, "selectedColumns", selected = character(0))
  })
  
  observeEvent(input$applyColumnFilter, {
    req(values$cleanData, input$selectedColumns)
    
    validate(need(length(input$selectedColumns) > 0, "Veuillez sélectionner au moins une colonne."))
    
    tryCatch({
      filtered <- values$cleanData[, input$selectedColumns, drop = FALSE]
      values$filteredData <- filtered
      
      showNotification(
        paste("Filtre par colonnes appliqué:", length(input$selectedColumns), "colonnes sélectionnées"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Erreur filtre par colonnes:", e$message), type = "error", duration = 10)
    })
  })
  
  #  Filtres croisés 
  
  # Filtres pour croisements complets entre facteurs
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
      showNotification(paste("Filtrage (2 facteurs) appliqué. Lignes:", nrow(filtered)), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Erreur filtrage:", e$message), type = "error", duration = 10)
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
      showNotification(paste("Filtrage (N facteurs) appliqué. Lignes:", nrow(filtered)), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Erreur filtrage N facteurs:", e$message), type = "error", duration = 10)
    })
  })
  
  #  Réinitialisation globale 
  observeEvent(input$resetFilter, {
    values$filteredData <- values$cleanData
    showNotification("Tous les filtres réinitialisés", type = "message", duration = 5)
  })
  
  #  Indicateurs de performance 
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
  
  output$columnsCount <- renderValueBox({
    req(values$filteredData)
    valueBox(
      ncol(values$filteredData), "Colonnes actives", icon = icon("columns"),
      color = "purple"
    )
  })
  
  #  Affichage des données filtrées 
  output$filteredData <- renderDT({
    req(values$filteredData)
    datatable(values$filteredData, 
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100),
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              filter = "top",
              rownames = TRUE,
              class = 'cell-border stripe')
  })
  
  #  Export des données filtrées 
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      paste("donnees_filtrees_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$filteredData)
      write.csv(values$filteredData, file, row.names = FALSE)
    }
  )
  # ---- Analyse descriptives ----
  
  # Sélection des variables numériques
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
  
  # Sélection des facteurs
  output$descFactorUI <- renderUI({
    req(values$filteredData)
    fac_cols <- get_all_factor_candidates(values$filteredData)
    tagList(
      pickerInput("descFactors", "Calcul par facteurs (optionnel)",
                  choices  = fac_cols,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE, `live-search` = TRUE)),
      helpText(icon("info-circle"),
               " Tous types acceptés (facteur, texte, date, numérique ≤30 niveaux).",
               " Laissez vide pour des descriptives globales.")
    )
  })
  
  # Sélectionner toutes les variables
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
    
    # Nombre de décimales 
    use_round <- !is.null(input$descRoundResults) && input$descRoundResults
    dec <- if (use_round && !is.null(input$descDecimals)) input$descDecimals else 4
    
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
      formatRound(columns = which(sapply(values$descStats, is.numeric)), digits = dec)
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
  
  # Téléchargement EXCEL 
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
  
  # Sélection de la variable à visualiser
  output$descPlotVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    selectInput("descPlotVar", "Variable à visualiser:", choices = num_cols, width = "100%")
  })
  
  # Sélection du facteur de groupement
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
          axis.title.x = element_markdown(face = x_label_font_face, size = 13, margin = margin(t = 10)),
          axis.title.y = element_markdown(face = y_label_font_face, size = 13, margin = margin(r = 10)),
          axis.text.x = element_text(
            face = x_tick_font_face, 
            angle = x_angle, 
            hjust = x_hjust,
            vjust = x_vjust,
            size = 11
          ),
          axis.text.y = element_text(face = y_tick_font_face, size = 11),
          plot.title = element_markdown(
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
          axis.title.x = element_markdown(face = x_label_font_face, size = 13, margin = margin(t = 10)),
          axis.title.y = element_markdown(face = y_label_font_face, size = 13, margin = margin(r = 10)),
          axis.text.x = element_text(
            face = x_tick_font_face, 
            angle = x_angle, 
            hjust = x_hjust,
            vjust = x_vjust,
            size = 11
          ),
          axis.text.y = element_text(face = y_tick_font_face, size = 11),
          plot.title = element_markdown(
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
    req(values$filteredData)
    p <- generate_desc_plot()
    print(p)
  }, res = 96)
  

  # Téléchargement graphique descriptif 
  output$downloadDescPlot <- downloadHandler(
    filename = function() paste0("graphique_descriptif_", Sys.Date(), ".", input$descPlot_format),
    content = function(file) {
      dpi       <- input$descPlot_dpi
      auto_dims <- calculate_dimensions_from_dpi(dpi, 25, 18)
      p <- generate_desc_plot()
      suppressWarnings(ggsave(file, plot = p, device = input$descPlot_format,
                              width = auto_dims$width, height = auto_dims$height,
                              dpi = dpi, units = "cm"))
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
      
      # Proportions en lignes
      if ("row_prop" %in% input$analysisOptions) {
        row_prop <- prop.table(contingency_table, margin = 1) * 100
        crosstab_values$row_proportions <- addmargins(row_prop, margin = 2)
      } else {
        crosstab_values$row_proportions <- NULL
      }
      
      # Proportions en colonnes
      if ("col_prop" %in% input$analysisOptions) {
        col_prop <- prop.table(contingency_table, margin = 2) * 100  
        crosstab_values$col_proportions <- addmargins(col_prop, margin = 1)
      } else {
        crosstab_values$col_proportions <- NULL
      }
      
      # Proportions totales
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
      
      # Calcul des residus standardises
      if ("residuals" %in% input$analysisOptions) {
        
        # Vérifier que le test du Chi2 a été calculé et qu'il est valide
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
    req(crosstab_values$chi_test)
    if (!is.null(crosstab_values$chi_test)) {
      if (is.list(crosstab_values$chi_test)) {
        cat("╔═══════════════════════════════════╗\n")
        cat("       TEST DU CHI-DEUX (χ²)          \n")
        cat("╚═══════════════════════════════════╝\n\n")
        cat("Statistique X2 :", round(crosstab_values$chi_test$statistic, 4), "\n")
        cat("Degres de liberte :", crosstab_values$chi_test$parameter, "\n") 
        cat("p-value :", format.pval(crosstab_values$chi_test$p.value, digits = 4), "\n")
        cat("\nInterpretation : ", 
            ifelse(crosstab_values$chi_test$p.value < 0.05, 
                   "[SIGNIFICATIF] Association SIGNIFICATIVE (p < 0.05)", 
                   "[NON SIGNIFICATIF] Pas d'association significative (p >= 0.05)"), "\n\n")
      } else {
        cat("╔═══════════════════════════════════╗\n")
        cat("       TEST DU CHI-DEUX (χ²)          \n")
        cat("╚═══════════════════════════════════╝\n\n")
        cat(crosstab_values$chi_test, "\n\n")
      }
    }
    
    if (!is.null(crosstab_values$fisher_test)) {
      if (is.list(crosstab_values$fisher_test)) {
        cat("╔═══════════════════════════════════╗\n")
        cat("     TEST EXACT DE FISHER             \n")
        cat("╚═══════════════════════════════════╝\n\n")
        cat("p-value :", format.pval(crosstab_values$fisher_test$p.value, digits = 4), "\n")
        cat("\nInterpretation : ", 
            ifelse(crosstab_values$fisher_test$p.value < 0.05, 
                   "[SIGNIFICATIF] Association SIGNIFICATIVE (p < 0.05)", 
                   "[NON SIGNIFICATIF] Pas d'association significative (p >= 0.05)"), "\n\n")
      } else {
        cat("╔═══════════════════════════════════╗\n")
        cat("     TEST EXACT DE FISHER             \n")
        cat("╚═══════════════════════════════════╝\n\n")
        cat(crosstab_values$fisher_test, "\n\n")
      }
    }
    
    if (is.null(crosstab_values$chi_test) && is.null(crosstab_values$fisher_test)) {
      cat("Aucun test statistique selectionne.\n")
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
    
    # Créer un dataFrame à partir du tableau de contingence
    df_plot <- as.data.frame(crosstab_values$contingency_table)
    df_plot <- df_plot[df_plot$Var1 != "Sum" & df_plot$Var2 != "Sum", ]
    names(df_plot) <- c("Row_Var", "Col_Var", "Freq")
    
    # Vérifier que les données sont valides
    if (nrow(df_plot) == 0 || any(is.na(df_plot$Freq))) {
      showNotification("Données invalides pour le graphique!", type = "error", duration = 5)
      return(NULL)
    }
    
    # Convertir explicitement en data frame
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
        plot.title = element_markdown(size = input$titleSize, hjust = 0.5, face = "bold"),
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
        legend.title = element_markdown(size = input$legendTextSize, face = "bold"),
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
  
  # Rendu conditionnel de la section de téléchargement du graphique principal 
  output$plotDownloadSection <- renderUI({
    req(crosstab_values$current_plot)
    
    div(style = "background-color: #eafaf1; padding: 20px; border-radius: 8px; border: 1px solid #27ae60;",
        tags$h5(
          icon("download"), 
          "Paramètres d'exportation",
          style = "font-weight: bold; color: #27ae60; margin-top: 0; margin-bottom: 15px;"
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
        
        downloadButton("downloadPlot", 
                       "Télécharger le graphique", 
                       class = "btn-success btn-lg btn-block", 
                       icon = icon("download"),
                       style = "font-weight: bold; font-size: 16px; padding: 15px; margin-top: 15px; background: linear-gradient(to right, #27ae60, #219653); box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
    )
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
        plot.title = element_markdown(size = input$titleSize, hjust = 0.5, 
                                      face = "bold", margin = margin(b = 20)),
        legend.text = element_text(size = input$legendTextSize),
        legend.title = element_markdown(size = input$legendTextSize, face = "bold"),
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
  
  # Rendu conditionnel de la section de téléchargement du graphique en secteurs 
  output$pieDownloadSection <- renderUI({
    req(crosstab_values$current_pie_plot)
    
    div(style = "background-color: #ebf5fb; padding: 20px; border-radius: 8px; border: 1px solid #3498db;",
        tags$h5(
          icon("download"), 
          "Paramètres d'exportation",
          style = "font-weight: bold; color: #3498db; margin-top: 0; margin-bottom: 15px;"
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
        
        downloadButton("downloadPiePlot", 
                       "Télécharger le graphique", 
                       class = "btn-info btn-lg btn-block", 
                       icon = icon("download"),
                       style = "font-weight: bold; font-size: 16px; padding: 15px; margin-top: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
    )
  })
  
  # Téléchargement des tableaux - Effectifs
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
  
  # Téléchargement des tableaux - Proportions lignes
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
  
  # Téléchargement des tableaux - Proportions colonnes
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
  
  # Téléchargement des tableaux - Proportions totales
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
  
  # Téléchargement des tableaux - Tests statistiques
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
  
  # Téléchargement des tableaux - Résidus
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
  
  # Téléchargement du graphique principal
  output$downloadPlot <- downloadHandler(
    filename = function() {
      ext <- if(!is.null(input$mainPlotFormat)) input$mainPlotFormat else "png"
      dpi <- if(!is.null(input$mainPlotDPI)) input$mainPlotDPI else 300
      paste0("graphique_croise_", Sys.Date(), "_", dpi, "dpi.", ext)
    },
    content = function(file) {
      req(crosstab_values$current_plot)
      
      # Calcul automatique des dimensions basé sur le DPI
      dpi <- if(!is.null(input$mainPlotDPI)) input$mainPlotDPI else 300
      format <- if(!is.null(input$mainPlotFormat)) input$mainPlotFormat else "png"
      
      # Dimensions en pouces 
    
      width_in <- 10
      height_in <- 7.5
      
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
        
        width_px <- round(width_in * dpi)
        height_px <- round(height_in * dpi)
        
        showNotification(
          paste0("Graphique téléchargé avec succès! (", width_px, "x", height_px, 
                 " px, ", dpi, " DPI, ", toupper(format), ")"), 
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
  
  # Téléchargement du graphique en secteurs
  output$downloadPiePlot <- downloadHandler(
    filename = function() {
      ext <- if(!is.null(input$piePlotFormat)) input$piePlotFormat else "png"
      dpi <- if(!is.null(input$piePlotDPI)) input$piePlotDPI else 300
      paste0("graphique_secteurs_", Sys.Date(), "_", dpi, "dpi.", ext)
    },
    content = function(file) {
      req(crosstab_values$current_pie_plot)
      
      # Calcul automatique des dimensions basé sur le DPI
      dpi <- if(!is.null(input$piePlotDPI)) input$piePlotDPI else 300
      format <- if(!is.null(input$piePlotFormat)) input$piePlotFormat else "png"
      
      # Dimensions carrées pour le graphique en secteurs (8x8 pouces)
      width_in <- 8
      height_in <- 8
      
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
        
        width_px <- round(width_in * dpi)
        height_px <- round(height_in * dpi)
        
        showNotification(
          paste0("Graphique secteurs téléchargé avec succès! (", width_px, "x", height_px, 
                 " px, ", dpi, " DPI, ", toupper(format), ")"), 
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
  
  
  # SÉLECTION DES VARIABLES
  
  
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
  
  # Sélection des variables pour l'axe Y secondaire
  output$vizY2VarSelect <- renderUI({
    req(values$filteredData)
    
    # Seulement les variables numériques parmi celles sélectionnées en Y1
    num_y_vars <- input$vizYVar[sapply(input$vizYVar %||% character(0), function(v) {
      v %in% names(values$filteredData) && is.numeric(values$filteredData[[v]])
    })]
    
    # Si moins de 2 variables Y numériques, afficher un message d'aide
    if (length(num_y_vars) < 2) {
      return(div(
        style = "margin-top:8px; padding:8px 10px; background:#fff3e0; border-left:3px solid #ff9800; border-radius:4px;",
        tags$small(
          style = "color:#e65100; font-size:11px;",
          icon("info-circle"),
          " Sélectionnez ", tags$b("≥ 2 variables Y numériques"), " pour activer l'axe secondaire."
        )
      ))
    }
    
    # Palette couleurs par défaut pour les courbes
    default_colors <- c("#2196F3","#4CAF50","#9C27B0","#FF5722","#00BCD4",
                        "#FFC107","#E91E63","#607D8B","#795548","#009688")
    
    # Sélecteurs de couleur pour CHAQUE variable Y
    color_pickers <- lapply(seq_along(num_y_vars), function(i) {
      v      <- num_y_vars[i]
      col_id <- paste0("curveColor_", make.names(v))
      col_def <- default_colors[((i - 1L) %% length(default_colors)) + 1L]
      div(
        style = "display:flex; align-items:center; gap:8px; margin-bottom:5px;",
        div(
          style = "flex:1; font-size:12px; color:#333; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
          tags$b(v)
        ),
        colourInput(col_id, label = NULL, value = col_def,
                    showColour = "background", palette = "square")
      )
    })
    
    tagList(
      div(
        style = "margin-top:10px; padding:12px; background:#fff8e1; border-left:4px solid #ff9800; border-radius:6px;",
        h5(
          icon("arrows-alt-v"), " Axe Y secondaire (droite)",
          style = "color:#e65100; font-size:13px; font-weight:bold; margin:0 0 8px 0;"
        ),
        checkboxInput(
          "enableDualAxis",
          tagList(icon("toggle-on"), " Activer l'axe Y secondaire"),
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.enableDualAxis == true",
          div(
            style = "margin-top:8px;",
            pickerInput(
              "vizY2Vars",
              label = div(
                style = "font-size:12px; color:#555; margin-bottom:4px;",
                icon("chart-line"), " Variables sur l'axe Y2 (droite) :"
              ),
              choices  = num_y_vars,
              selected = num_y_vars[length(num_y_vars)],
              multiple = TRUE,
              options  = list(
                `actions-box`           = TRUE,
                `selected-text-format`  = "count > 2",
                `none-selected-text`    = "Sélectionnez des variables..."
              )
            ),
            tags$small(
              style = "color:#888; font-size:11px;",
              icon("info-circle"), " Label, limites et graduations Y2 disponibles dans 'Options Avancées de Personnalisation'."
            )
          )
        ),
        # Sélecteurs de couleurs — toujours visibles (Y1 et Y2)
        hr(style = "margin:10px 0; border-color:#ffe0b2;"),
        tags$small(
          style = "color:#e65100; font-weight:600; display:block; margin-bottom:6px; font-size:11px;",
          icon("palette"), " Couleur de chaque courbe :"
        ),
        color_pickers
      )
    )
  })

  # Observer enableDualAxis
  observeEvent(input$enableDualAxis, {
    if (isTRUE(input$enableDualAxis)) {
      values$dualAxisActive <- TRUE
    } else {
      values$dualAxisActive <- FALSE
      values$y2Vars <- NULL
    }
  })
  
  # Observer vizY2Vars
  observe({
    req(isTRUE(input$enableDualAxis))
    values$y2Vars <- input$vizY2Vars
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
    req(isTRUE(input$useAggregation))
    
    all_cols <- names(values$filteredData)
    
    excluded_vars <- unique(as.character(unlist(c(input$vizXVar, input$vizYVar))))
    excluded_vars <- excluded_vars[!is.na(excluded_vars) & nchar(excluded_vars) > 0]
    available_cols <- setdiff(all_cols, excluded_vars)
    
    if (length(available_cols) == 0) {
      return(helpText(icon("info-circle"), "Aucune variable disponible pour le regroupement."))
    }
    
    # Priorité aux variables catégorielles mais inclure aussi les numériques à faible cardinalité
    cat_cols <- available_cols[sapply(seq_along(available_cols), function(i) {
      x <- values$filteredData[[ available_cols[i] ]]
      is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 20)
    })]
    
    # Si pas de variables catégorielles, utiliser toutes les variables disponibles
    if (length(cat_cols) == 0) cat_cols <- available_cols
    
    x_default <- if (!is.null(input$vizXVar) && nchar(input$vizXVar) > 0) input$vizXVar else cat_cols[1]
    
    div(
      selectizeInput(
        "groupVars",
        "Variables de regroupement:",
        choices = c(setNames(x_default, paste0("Variable X (", x_default, ")")), cat_cols),
        selected = x_default,
        multiple = TRUE,
        options = list(
          placeholder = "Sélectionnez les variables...",
          maxItems = 5,
          plugins = list("remove_button")
        )
      ),
      helpText(
        icon("info-circle", style = "color: #17a2b8;"),
        "Sélectionnez les variables qui définissent les groupes pour l'agrégation. Par défaut, X est utilisée."
      )
    )
  })
  
  
  
  # DÉTECTION AUTOMATIQUE DES VARIABLES
  
  
  # Détection automatique du type de variable X 
  observe({
    req(values$filteredData, input$vizXVar)
    
    if(input$xVarType == "auto") {
      data <- values$filteredData
      x_var_data <- data[[input$vizXVar]]
      
      detected_type <- viz_detect_x_type(x_var_data)
      
      values$detectedXType <- detected_type
    }
  })
  
  # Détection du mode multi-Y 
  observe({
    req(input$vizYVar)
    
    if(length(input$vizYVar) > 1) {
      values$multipleY <- TRUE
      values$yVarNames <- input$vizYVar
    } else {
      values$multipleY <- FALSE
      values$yVarNames <- NULL
      values$y2VarsActive <- NULL
      values$dualAxisActive <- FALSE
    }
  })
  
  
  # GESTION DES NIVEAUX X 
  
  # Initialiser le stockage des labels personnalisés
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
    
    data    <- values$filteredData
    x_var   <- input$vizXVar
    x_type  <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    # Forcer un type résolu non-NULL
    if (is.null(x_type) || x_type == "auto") x_type <- "text"
    
    unique_vals <- if (x_type == "date" ||
                       inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))) {
      # Dates : représenter sous forme de chaîne pour l'édition des labels
      as.character(sort(unique(data[[x_var]])))
    } else if (x_type == "numeric" || is.numeric(data[[x_var]])) {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.factor(data[[x_var]])) {
      levels(data[[x_var]])
    } else {
      sort(unique(as.character(data[[x_var]])))
    }
    
    values$currentXLevels <- unique_vals
    
    # Initialiser les labels 
    if (!is.null(input$vizXVar) && is.null(values$storedLevelLabels[[input$vizXVar]])) {
      values$storedLevelLabels[[input$vizXVar]] <- setNames(unique_vals, unique_vals)
    }
  })
  
  # Éditeur de niveaux pour la variable X 
  output$xLevelsEditor <- renderUI({
    req(values$filteredData, input$vizXVar)
    req(values$filteredData, input$vizXVar)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    
  
    if (is.null(x_type)) return(NULL)
    
    # Générer les valeurs uniques selon le type
    unique_vals <- if (inherits(data[[x_var]], c("Date","POSIXct","POSIXlt")) ||
                       x_type == "date") {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.numeric(data[[x_var]]) || x_type == "numeric") {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.factor(data[[x_var]])) {
      levels(droplevels(data[[x_var]]))
    } else {
      sort(unique(as.character(data[[x_var]])))
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
            # Récupérer le label stocké s'il existe
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
  
  # Appliquer et stocker les labels
  observeEvent(input$applyLabels, {
    req(values$currentXLevels, input$vizXVar)
    
    # Créer le mapping original → nouveau label
    level_mapping <- sapply(values$currentXLevels, function(lvl) {
      new_label <- input[[paste0("xLevel_", make.names(lvl))]]
      if(is.null(new_label) || trimws(new_label) == "") lvl else trimws(new_label)
    })
    names(level_mapping) <- values$currentXLevels
    
    # Stocker le mapping de manière persistante
    values$storedLevelLabels[[input$vizXVar]] <- level_mapping
    
    # Reconstruire l'ordre en fonction du mapping
    if(!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
      inv_map <- setNames(names(level_mapping), as.character(level_mapping))
      values$customXOrder <- sapply(values$customXOrder, function(v) {
        new_v <- as.character(level_mapping[v])
        if(!is.na(new_v)) new_v else v
      })
    } else {
      # Pas d'ordre existant : définir l'ordre par les nouveaux labels
      values$customXOrder <- as.character(level_mapping[values$currentXLevels])
    }
    
    # Forcer le recalcul immédiat du graphique 
    values$plotUpdateTrigger <- runif(1)
    invalidateLater(80)
    
    showNotification(
      paste0("Labels appliqués : ", length(level_mapping), " niveaux mis à jour."),
      type = "message", duration = 2
    )
  })
  
  # Réinitialiser les niveaux avec notification
  observeEvent(input$resetLevels, {
    req(values$currentXLevels, input$vizXVar)
    
    # Réinitialiser tous les inputs de niveau
    lapply(values$currentXLevels, function(lvl) {
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), value = lvl)
    })
    
    # Supprimer le stockage persistant
    values$storedLevelLabels[[input$vizXVar]] <- NULL
    
    # Réinitialiser aussi l'ordre personnalisé
    values$customXOrder <- NULL
    
    # Forcer le recalcul du graphique
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Niveaux et ordre réinitialisés", type = "message", duration = 2)
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
  
  
  # GESTION DE L'ORDRE X
  
  
  # Éditeur d'ordre pour X catégoriel 
  output$xOrderEditor <- renderUI({
    req(values$filteredData, input$vizXVar)
    req(values$filteredData, input$vizXVar)
    # Disponible pour tous les types sauf histogramme/density/pie/donut/treemap
    excluded_types <- c("histogram", "density", "pie", "donut", "treemap")
    if(isTRUE(input$vizType %in% excluded_types)) return(NULL)
    
    x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    if (is.null(x_type)) x_type <- "text"
    
    data  <- values$filteredData
    x_var <- input$vizXVar
    
    # Générer les valeurs affichables selon le type
    unique_vals <- if (inherits(data[[x_var]], c("Date","POSIXct","POSIXlt")) ||
                       x_type == "date") {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.numeric(data[[x_var]]) || x_type == "numeric") {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.factor(data[[x_var]])) {
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
    
    # Utiliser les labels personnalisés si disponibles
    display_vals <- unique_vals
    if(!is.null(values$storedLevelLabels[[x_var]])) {
      level_mapping <- values$storedLevelLabels[[x_var]]
      display_vals <- sapply(unique_vals, function(val) {
        if(val %in% names(level_mapping)) {
          as.character(level_mapping[[val]])
        } else {
          val
        }
      })
    }
    
    # Si un ordre personnalisé existe, l'utiliser
    if(!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
      # Filtrer pour ne garder que les valeurs qui existent
      ordered_vals <- values$customXOrder[values$customXOrder %in% display_vals]
      if(length(ordered_vals) > 0) {
        display_vals <- ordered_vals
      }
    }
    
    div(
      div(style = "margin-bottom: 10px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              span(paste(length(display_vals), "catégories détectées"), 
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
          style = if(length(display_vals) > 15) "max-height: 500px; overflow-y: auto; padding: 10px; background-color: #f9f9f9; border-radius: 5px;" else "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          lapply(seq_along(display_vals), function(i) {
            val <- display_vals[i]
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
    req(values$currentXLevels, input$vizXVar)
    
    #  Utiliser les labels personnalisés si disponibles
    if(!is.null(values$storedLevelLabels[[input$vizXVar]])) {
      level_mapping <- values$storedLevelLabels[[input$vizXVar]]
      sorted_labels <- sort(as.character(level_mapping[values$currentXLevels]))
      values$customXOrder <- sorted_labels
    } else {
      sorted_levels <- sort(values$currentXLevels)
      values$customXOrder <- sorted_levels
    }
    
    # Forcer le recalcul du graphique
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Ordre trié alphabétiquement", type = "message", duration = 2)
  })
  
  # Inverser l'ordre X
  observeEvent(input$reverseOrderX, {
    req(values$currentXLevels, input$vizXVar)
    
    #  Utiliser les labels personnalisés si disponibles
    if(!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
      # Inverser l'ordre actuel
      values$customXOrder <- rev(values$customXOrder)
    } else if(!is.null(values$storedLevelLabels[[input$vizXVar]])) {
      level_mapping <- values$storedLevelLabels[[input$vizXVar]]
      values$customXOrder <- rev(as.character(level_mapping[values$currentXLevels]))
    } else {
      values$customXOrder <- rev(values$currentXLevels)
    }
    
    # Forcer le recalcul du graphique
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Ordre inversé", type = "message", duration = 2)
  })
  
  # Réinitialiser l'ordre X
  observeEvent(input$resetOrderX, {
    values$customXOrder <- NULL
    
    # Forcer le recalcul du graphique
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Ordre réinitialisé", type = "message", duration = 2)
  })
  
  # Capturer l'ordre personnalisé depuis le sortable
  observeEvent(input$xLevelOrder, {
    if(!is.null(input$xLevelOrder) && length(input$xLevelOrder) > 0) {
      values$customXOrder <- input$xLevelOrder
      # Double invalidation pour garantir la mise à jour
      values$plotUpdateTrigger <- runif(1)
      invalidateLater(50)
    }
  })
  
  # Forcer le recalcul après invalidation du xLevelOrder
  observe({
    req(values$customXOrder)
    isolate({
      values$plotUpdateTrigger <- runif(1)
    })
  })
  
  
  # AGRÉGATION DES DONNÉES
  
  
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
      # S'assurer que les groupVars existent dans les données
      group_vars <- intersect(group_vars, names(data))
      if (length(group_vars) == 0) group_vars <- x_var
      
      # Ajouter la variable de couleur au groupement si présente et pas en mode multi-Y
      if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun" && 
         (is.null(values$multipleY) || !values$multipleY) &&
         input$vizColorVar %in% names(data)) {
        group_vars <- unique(c(group_vars, input$vizColorVar))
      }
      
      # Vérifier que les variables Y existent dans les données
      y_vars <- intersect(y_vars, names(data))
      if (length(y_vars) == 0) return(data)
      
      # Séparer les Y numériques et non-numériques
      y_numeric <- y_vars[sapply(y_vars, function(v) is.numeric(data[[v]]))]
      y_non_num <- setdiff(y_vars, y_numeric)
      
      # Si aucune Y n'est numérique et la fonction n'est pas "count",
      # basculer automatiquement sur "count"
      effective_func <- input$aggFunction
      if (length(y_numeric) == 0 && effective_func != "count") {
        effective_func <- "count"
        showNotification(
          "Variable Y non numérique : agrégation forcée en Comptage.",
          type = "warning", duration = 4
        )
      }
      
      # Fonction d'agrégation
      agg_func <- switch(effective_func,
                         "mean"   = function(x) mean(as.numeric(x), na.rm = TRUE),
                         "median" = function(x) median(as.numeric(x), na.rm = TRUE),
                         "sum"    = function(x) sum(as.numeric(x), na.rm = TRUE),
                         "count"  = function(x) sum(!is.na(x)),
                         "min"    = function(x) min(as.numeric(x), na.rm = TRUE),
                         "max"    = function(x) max(as.numeric(x), na.rm = TRUE),
                         "sd"     = function(x) sd(as.numeric(x), na.rm = TRUE),
                         function(x) mean(as.numeric(x), na.rm = TRUE))
      
      # Variables à agréger : Y numériques, ou toutes les Y si on fait du count
      vars_to_agg <- if (effective_func == "count") y_vars else y_numeric
      
      # Agréger pour chaque variable Y
      if(length(vars_to_agg) == 1) {
        # Mode Y simple : ne sélectionner que les colonnes utiles avant group_by
        cols_needed <- unique(c(group_vars, vars_to_agg[1]))
        cols_needed <- intersect(cols_needed, names(data))
        data <- data %>%
          dplyr::select(dplyr::all_of(cols_needed)) %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(!!vars_to_agg[1] := agg_func(.data[[vars_to_agg[1]]]),
                    .groups = "drop")
      } else if (length(vars_to_agg) > 1) {
        # Mode multi-Y - agréger chaque variable Y séparément
        agg_list <- list()
        for(y_var in vars_to_agg) {
          cols_needed <- unique(c(group_vars, y_var))
          cols_needed <- intersect(cols_needed, names(data))
          temp_data <- data %>%
            dplyr::select(dplyr::all_of(cols_needed)) %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(value = agg_func(.data[[y_var]]), .groups = "drop") %>%
            mutate(Variable = y_var)
          agg_list[[y_var]] <- temp_data
        }
        data <- bind_rows(agg_list)
      } else {
        # Aucune variable Y agrégeable : retourner les données originales
        showNotification(
          "Aucune variable Y compatible avec la fonction d'agrégation choisie.",
          type = "warning", duration = 4
        )
        return(data)
      }
      
      showNotification(
        paste("Données agrégées:", nrow(data), "observations"),
        type = "message",
        duration = 2
      )
    }
    
    return(data)
  })
  
  
  # PRÉPARATION DES DONNÉES
  
  # Expression réactive pour préparer les données du graphique
  plotData <- reactive({
    req(values$filteredData, input$vizXVar, input$vizYVar)
    
    # Forcer la réactivité à customXOrder, storedLevelLabels, format date et thème
    values$customXOrder
    values$storedLevelLabels
    values$plotUpdateTrigger
    input$xDateDisplayFormat
    input$plotTheme
    input$enableDualAxis
    input$vizY2Vars
    input$y2AxisLabel
    values$y2VarsActive
    values$dualAxisActive
    
    # Utiliser les données agrégées si disponibles
    data <- if(isTRUE(input$useAggregation)) {
      aggregatedData()
    } else {
      values$filteredData
    }
    
    x_var <- input$vizXVar
    y_vars <- input$vizYVar
    
    # Appliquer les renommages de niveaux X si disponibles 
    x_type <- if(input$xVarType == "auto") {
      if (!is.null(values$detectedXType)) values$detectedXType else {
        # Détecter directement si pas encore fait
        viz_detect_x_type(data[[x_var]])
      }
    } else input$xVarType
    
    x_is_date_type    <- x_type == "date" || inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))
    x_is_numeric_type <- x_type == "numeric" || (is.numeric(data[[x_var]]) && !x_is_date_type)
    x_is_cat_type     <- x_type %in% c("factor","categorical","text") || is.factor(data[[x_var]])
    
    # Récupérer le mapping de labels pour cette variable
    level_mapping <- values$storedLevelLabels[[x_var]]
    has_custom_labels <- !is.null(level_mapping) &&
      any(as.character(level_mapping) != names(level_mapping))
    
    if (x_is_cat_type || x_is_date_type || x_is_numeric_type) {
      # Représenter X comme chaîne de caractères pour le mapping
      orig_vals <- as.character(data[[x_var]])
      
      if (!is.null(level_mapping) && length(level_mapping) > 0) {
        valid_keys <- names(level_mapping)[names(level_mapping) %in% unique(orig_vals)]
        if (length(valid_keys) > 0) {
          mapped_vals <- ifelse(orig_vals %in% names(level_mapping),
                                as.character(level_mapping[orig_vals]),
                                orig_vals)
          if (x_is_cat_type) {
            # Facteur : conserver l'ordre du mapping
            new_lvls <- as.character(level_mapping[valid_keys])
            # Appliquer l'ordre personnalisé si défini
            if (!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
              ord <- values$customXOrder[values$customXOrder %in% new_lvls]
              if (length(ord) > 0) new_lvls <- ord
            }
            data[[x_var]] <- factor(mapped_vals, levels = new_lvls)
          } else {
            # Date/numérique : le mapping est lu directement via values$storedLevelLabels
            
          }
        }
      } else if (x_is_cat_type) {
        # Pas de mapping : juste facteur avec ordre personnalisé
        current_levels <- sort(unique(orig_vals))
        if (!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
          ord <- values$customXOrder[values$customXOrder %in% current_levels]
          if (length(ord) > 0) current_levels <- ord
        }
        data[[x_var]] <- factor(data[[x_var]], levels = current_levels)
      }
      
      
    }
    
    # Conversion de date si nécessaire
    if(x_type == "date" && !is.null(x_var) && x_var %in% names(data) &&
       !inherits(data[[x_var]], "Date")) {
      date_format <- input$xDateFormat %||% "%Y-%m-%d"
      converted <- tryCatch({
        result <- as.Date(data[[x_var]], format = date_format)
        result
      }, error = function(e) {
        showNotification("Erreur de conversion de date. Vérifiez le format.", type = "error")
        NULL
      })
      # Appliquer uniquement si le résultat a la même longueur que les données
      if (!is.null(converted) && length(converted) == nrow(data)) {
        data[[x_var]] <- converted
      }
    }
    
    # Gestion du mode multi-Y 
    if(!is.null(values$multipleY) && values$multipleY) {
      if(isTRUE(input$useAggregation) && "Variable" %in% names(data)) {
        data_long <- data %>%
          rename(Value = value)
      } else {
        # Transformer en format long pour multi-Y
        # Séparer Y1 (axe gauche) et Y2 (axe droit si dual axis actif)
        y2_vars   <- if (isTRUE(values$dualAxisActive) && !is.null(values$y2Vars))
                       intersect(values$y2Vars, y_vars) else character(0)
        y1_vars   <- setdiff(y_vars, y2_vars)
        valid_y1  <- y1_vars[y1_vars %in% names(data)]
        
        if (length(valid_y1) == 0) {
          showNotification("Erreur: Aucune variable Y1 disponible dans les données", type = "error")
          return(data)
        }
        if (!x_var %in% names(data)) {
          showNotification("Erreur: Variable X non disponible", type = "error")
          return(data)
        }
        
        # Pivot uniquement les variables Y1
        cols_to_keep <- c(x_var, valid_y1)
        # Garder aussi les vars Y2 en wide pour le dual axis
        valid_y2 <- y2_vars[y2_vars %in% names(data)]
        cols_wide <- unique(c(cols_to_keep, valid_y2))
        
        data_wide <- data %>% dplyr::select(dplyr::any_of(cols_wide))
        
        data_long <- data_wide %>%
          tidyr::pivot_longer(
            cols    = dplyr::any_of(valid_y1),
            names_to  = "Variable",
            values_to = "Value"
          )
        
        # Stocker les infos Y2 pour createPlot
        values$y2VarsActive <- valid_y2
      }
      return(data_long)
    }
    
    # Mode Y simple
    cols_needed <- unique(c(x_var, y_vars[1], input$vizColorVar, input$vizFacetVar))
    cols_needed <- cols_needed[cols_needed != "Aucun"]
    cols_needed <- cols_needed[cols_needed %in% names(data)]
    
    return(data[, cols_needed, drop = FALSE])
  })
  
  # Stocker les données préparées dans values pour accès global
  observe({
    values$plotData <- plotData()
  })
  
  
  # GESTION DES LABELS DE LÉGENDE
  
  # Créer une interface pour personnaliser les labels de légende
  observeEvent(input$customizeLegendLabels, {
    # Vérification douce (pas de req() bloquant)
    if (is.null(values$plotData)) {
      showNotification(
        tagList(icon("chart-bar"), " Générez d'abord un graphique avant de modifier la légende."),
        type = "warning", duration = 4
      )
      return()
    }
    
    # Déterminer quels sont les niveaux de la légende
    legend_levels <- NULL
    legend_var_name <- NULL
    
    if(!is.null(values$multipleY) && values$multipleY) {
      legend_levels  <- values$yVarNames
      legend_var_name <- "Variables Y"
    } else if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      color_data <- values$plotData[[input$vizColorVar]]
      if (is.factor(color_data)) {
        legend_levels <- levels(color_data)
      } else if (is.numeric(color_data) || is.integer(color_data)) {
        # Couleur continue 
        legend_levels <- as.character(sort(unique(color_data)))
        if (length(legend_levels) > 20) {
          showNotification(
            "Variable continue avec trop de valeurs distinctes (> 20). Utilisez une variable catégorielle pour personnaliser les labels.",
            type = "warning", duration = 5)
          return()
        }
      } else if (inherits(color_data, c("Date","POSIXct","POSIXlt"))) {
        legend_levels <- as.character(sort(unique(color_data)))
      } else {
        legend_levels <- sort(unique(as.character(color_data)))
      }
      legend_var_name <- input$vizColorVar
    }
    
    if(is.null(legend_levels) || length(legend_levels) == 0) {
      showNotification(
        tagList(
          icon("exclamation-circle"), " Aucune légende à personnaliser.",
          tags$br(),
          tags$small(
            "Sélectionnez une ", tags$b("variable couleur"), " ou activez le ",
            tags$b("mode multi-Y"), " (≥2 variables Y) pour utiliser cet éditeur."
          )
        ),
        type = "warning", duration = 5
      )
      return()
    }
    
    # Créer l'interface modale pour éditer les labels de légende
    showModal(modalDialog(
      title = tagList(icon("tags"), " Personnaliser les labels de la légende"),
      size = "m",
      
      div(
        # En-tête informatif
        div(
          style = "background:#f0f4ff; border-left:4px solid #3498db; padding:10px 14px; border-radius:4px; margin-bottom:14px;",
          tags$b(style="color:#2980b9;", icon("info-circle"), " Variable : "),
          tags$span(style="color:#34495e; font-size:13px;", legend_var_name),
          tags$br(),
          tags$small(style="color:#7f8c8d;",
            "Saisissez les nouveaux noms à afficher dans la légende. ",
            "Laissez vide pour conserver l'original."
          )
        ),
        
        # Grille des niveaux
        div(style = if(length(legend_levels) > 8) "max-height: 420px; overflow-y: auto;" else "",
            lapply(seq_along(legend_levels), function(i) {
              lvl <- legend_levels[i]
              storage_key <- if(legend_var_name == "Variables Y") "multiY_legend" else legend_var_name
              
              current_label <- if(!is.null(values$legendLabels[[storage_key]]) && 
                                  !is.null(values$legendLabels[[storage_key]][[lvl]])) {
                values$legendLabels[[storage_key]][[lvl]]
              } else { lvl }
              
              # Couleur ggplot2 pour ce niveau
              n_total <- length(legend_levels)
              lvl_color <- tryCatch(scales::hue_pal()(n_total)[i], error=function(e) "#cccccc")
              
              div(
                style = "display:flex; align-items:center; gap:10px; margin-bottom:8px; padding:8px 10px; background:#fafafa; border-radius:6px; border:1px solid #e8e8e8;",
                # Swatch couleur
                div(style = paste0("width:18px; height:18px; border-radius:50%; background:", lvl_color,
                                   "; flex-shrink:0; border:2px solid #fff; box-shadow:0 0 0 1px #ccc;")),
                # Label original
                div(style = "min-width:100px; max-width:120px; font-size:11px; color:#666; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
                    tags$span(title=lvl, lvl),
                    tags$br(),
                    tags$span(style="color:#bbb; font-size:10px;", paste0("Niveau ", i))),
                # Flèche
                div(style="color:#bbb; font-size:14px;", "→"),
                # Input
                div(style="flex:1;",
                  textInput(
                    inputId = paste0("legendLevel_", make.names(lvl)),
                    label   = NULL,
                    value   = current_label,
                    placeholder = paste0("ex: Groupe ", i),
                    width   = "100%"
                  )
                )
              )
            })
        )
      ),
      
      footer = tagList(
        actionButton("applyLegendLabels",  tagList(icon("check"), " Appliquer"),       class = "btn-primary"),
        actionButton("previewLegendLabels",tagList(icon("eye"),   " Prévisualiser"),   class = "btn-info"),
        actionButton("resetLegendLabels",  tagList(icon("undo"),  " Réinitialiser"),   class = "btn-default"),
        modalButton(tagList(icon("times"), " Fermer"))
      )
    ))
  })
  
  # Indicateur des labels personnalisés
  output$legendLabelsStatus <- renderUI({
    if (is.null(values$legendLabels) || length(values$legendLabels) == 0) return(NULL)
    # Compter le nombre de labels modifiés
    n_custom <- sum(sapply(values$legendLabels, function(m) {
      if (is.null(m)) return(0L)
      sum(names(m) != as.character(m))
    }))
    if (n_custom == 0) return(NULL)
    tags$small(
      style = "color: #e67e22; font-size: 11px; display: block; margin-top: 3px;",
      icon("check-circle"), " ", n_custom, " label(s) personnalisé(s) actif(s)"
    )
  })
  
  # Appliquer les labels de légende personnalisés
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
      if (is.factor(color_data)) levels(color_data)
      else if (is.numeric(color_data) || is.integer(color_data)) as.character(sort(unique(color_data)))
      else if (inherits(color_data, c("Date","POSIXct","POSIXlt"))) as.character(sort(unique(color_data)))
      else sort(unique(as.character(color_data)))
    }
    
    # Créer le mapping
    legend_mapping <- sapply(legend_levels, function(lvl) {
      new_label <- input[[paste0("legendLevel_", make.names(lvl))]]
      if(is.null(new_label) || new_label == "") lvl else new_label
    })
    names(legend_mapping) <- legend_levels
    
    # Stocker
    values$legendLabels[[storage_key]] <- legend_mapping
    
    # Forcer le recalcul du graphique
    values$plotUpdateTrigger <- runif(1)
    
    removeModal()
    showNotification("Labels de légende appliqués", type = "message", duration = 2)
  })
  
  # Prévisualiser les labels 
  observeEvent(input$previewLegendLabels, {
    storage_key <- if (!is.null(values$multipleY) && values$multipleY) {
      "multiY_legend"
    } else if (!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      input$vizColorVar
    } else { return() }
    
    legend_levels <- if (storage_key == "multiY_legend") {
      values$yVarNames
    } else {
      color_data <- values$plotData[[input$vizColorVar]]
      if (is.factor(color_data)) levels(color_data)
      else if (is.numeric(color_data)||is.integer(color_data)) as.character(sort(unique(color_data)))
      else if (inherits(color_data, c("Date","POSIXct","POSIXlt"))) as.character(sort(unique(color_data)))
      else sort(unique(as.character(color_data)))
    }
    
    legend_mapping <- sapply(legend_levels, function(lvl) {
      new_label <- input[[paste0("legendLevel_", make.names(lvl))]]
      if (is.null(new_label) || new_label == "") lvl else new_label
    })
    names(legend_mapping) <- legend_levels
    values$legendLabels[[storage_key]] <- legend_mapping
    values$plotUpdateTrigger <- runif(1)
    showNotification("Prévisualisation appliquée au graphique", type = "message", duration = 2)
  })
  
  # Réinitialiser les labels de légende
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
      if (is.factor(color_data)) levels(color_data)
      else if (is.numeric(color_data)||is.integer(color_data)) as.character(sort(unique(color_data)))
      else if (inherits(color_data, c("Date","POSIXct","POSIXlt"))) as.character(sort(unique(color_data)))
      else sort(unique(as.character(color_data)))
    }
    
    # Réinitialiser les inputs
    lapply(legend_levels, function(lvl) {
      updateTextInput(session, paste0("legendLevel_", make.names(lvl)), value = lvl)
    })
    
    # Supprimer le stockage
    values$legendLabels[[storage_key]] <- NULL
    
    # Forcer le recalcul du graphique
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Labels de légende réinitialisés", type = "message", duration = 2)
  })
  
  
  # CRÉATION DU GRAPHIQUE
  
  # Expression réactive pour créer le graphique avec mise à jour automatique
  createPlot <- reactive({
    req(values$plotData, input$vizXVar, input$vizYVar, input$vizType)
    
    # Réactivité aux options de personnalisation ET à l'ordre personnalisé
    input$plotTitle
    input$xAxisLabel
    input$yAxisLabel
    input$legendTitle
    input$legendTitleSize
    input$legendTextSize
    input$legendKeySize
    input$pointSize
    input$lineWidth
    input$pointAlpha
    input$barWidth
    input$barPosition
    input$titleSize
    input$axisLabelSize
    input$baseFontSize
    input$xAxisBold
    input$xAxisItalic
    input$yAxisBold
    input$yAxisItalic
    input$xTickBold
    input$xTickItalic
    input$xAxisAngle
    input$legendPosition
    input$showValues
    input$valueLabelSize
    input$valueLabelPosition
    input$valueLabelColor
    input$valueLabelBold
    input$valueLabelItalic
    input$valueLabelDigits
    input$showTrendLine
    input$showPoints
    input$customAxisBreaks
    input$yAxisBreakStep
    input$xAxisBreakStep
    input$xAxisMin
    input$xAxisMax
    input$yAxisMin
    input$yAxisMax
    input$yTickBold
    input$yTickItalic
    input$xTickSize
    input$yTickSize
    values$customXOrder
    values$storedLevelLabels
    values$plotUpdateTrigger
    input$xDateDisplayFormat
    input$plotTheme
    input$y2AxisLabel
    input$enableDualAxis
    input$y2AxisMin
    input$y2AxisMax
    input$y2AxisStep
    input$y2AxisBreakStep
    input$y2TickBold
    input$y2TickItalic
    input$y2TickSize
    input$y2AxisBold
    input$y2AxisItalic
    input$y2CurveWidth
    input$plotMarginTop
    input$plotMarginRight
    input$plotMarginBottom
    input$plotMarginLeft
    
    # Couleurs des courbes (colourInput dynamiques)
    if (!is.null(values$yVarNames)) {
      lapply(values$yVarNames, function(v) input[[paste0("curveColor_", make.names(v))]])
    }
    
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
    
    # ── Appliquer les couleurs personnalisées Y1 (colourInput) en mode multi-Y ──
    if (isTRUE(values$multipleY) && !is.null(values$yVarNames)) {
      default_colors_y1 <- c("#2196F3","#4CAF50","#9C27B0","#FF5722","#00BCD4",
                              "#FFC107","#E91E63","#607D8B","#795548","#009688")
      y1_only <- setdiff(values$yVarNames,
                         if (isTRUE(values$dualAxisActive)) values$y2VarsActive else character(0))
      if (length(y1_only) > 0) {
        y1_cols <- vapply(seq_along(y1_only), function(i) {
          col_id   <- paste0("curveColor_", make.names(y1_only[i]))
          user_col <- input[[col_id]]
          if (!is.null(user_col) && nchar(trimws(user_col)) > 0) return(user_col)
          default_colors_y1[((i - 1L) %% length(default_colors_y1)) + 1L]
        }, character(1))
        tryCatch(
          p <- p + scale_color_manual(
            values = setNames(y1_cols, y1_only),
            breaks = y1_only, labels = y1_only,
            name = NULL, drop = FALSE
          ),
          error = function(e) invisible(NULL)
        )
      }
    }
    
    # Superposer les vars Y2 avec axe secondaire 
    y2_active_vars <- if (isTRUE(values$dualAxisActive) && !is.null(values$y2VarsActive))
                        values$y2VarsActive else character(0)
    
    if (length(y2_active_vars) > 0 &&
        !viz_type %in% c("pie","donut","treemap","heatmap","histogram","density")) {
      
      tryCatch({
        raw_data <- values$filteredData
        
        # Plages Y1
        y1_vals  <- if ("Value" %in% names(data)) data[["Value"]] else data[[y_var]]
        y1_min   <- min(y1_vals, na.rm = TRUE)
        y1_max   <- max(y1_vals, na.rm = TRUE)
        y1_range <- y1_max - y1_min
        if (y1_range == 0) y1_range <- 1
        
        # Palette couleurs par défaut
        default_colors <- c("#2196F3","#4CAF50","#9C27B0","#FF5722","#00BCD4",
                            "#FFC107","#E91E63","#607D8B","#795548","#009688")
        
        # Toutes les variables Y
        all_y_vars <- if (!is.null(values$yVarNames)) values$yVarNames else y_var
        y1_vars    <- setdiff(all_y_vars, y2_active_vars)
        
        # Lecture couleur depuis colourInput ou valeur par défaut
        get_curve_color <- function(var_name, idx) {
          col_id   <- paste0("curveColor_", make.names(var_name))
          user_col <- input[[col_id]]
          if (!is.null(user_col) && nchar(trimws(user_col)) > 0) return(user_col)
          default_colors[((idx - 1L) %% length(default_colors)) + 1L]
        }
        
        # Mapping couleurs Y1
        y1_color_map <- setNames(
          vapply(seq_along(y1_vars), function(i) get_curve_color(y1_vars[i], i), character(1)),
          y1_vars
        )
        
        # Détecter si x est une date
        x_is_date <- inherits(raw_data[[x_var]], c("Date","POSIXct","POSIXlt"))
        
        y2_idx_start <- length(y1_vars) + 1L
        y2_color_map <- list()
        
        for (i in seq_along(y2_active_vars)) {
          yv     <- y2_active_vars[i]
          if (!yv %in% names(raw_data)) next
          col_y2 <- get_curve_color(yv, y2_idx_start + i - 1L)
          y2_color_map[[yv]] <- col_y2
          
          y2_vals  <- raw_data[[yv]]
          y2_min   <- min(y2_vals, na.rm = TRUE)
          y2_max   <- max(y2_vals, na.rm = TRUE)
          y2_range <- y2_max - y2_min
          if (y2_range == 0) y2_range <- 1
          
          sf  <- y1_range / y2_range
          off <- y1_min - y2_min * sf
          
          d2 <- raw_data[!is.na(raw_data[[x_var]]) & !is.na(raw_data[[yv]]), ]
          # Assurer le bon type de x pour éviter le warning Date/numeric
          if (x_is_date && !inherits(d2[[x_var]], c("Date","POSIXct","POSIXlt"))) {
            d2[[x_var]] <- as.Date(d2[[x_var]], origin = "1970-01-01")
          }
          d2$.y2_scaled    <- d2[[yv]] * sf + off
          d2$.curve_label  <- yv   
          
          # Trait PLEIN avec aes(color=)
          y2_lw <- input$y2CurveWidth %||% 1.2
          if (viz_type %in% c("line","seasonal_smooth","seasonal_evolution","area")) {
            p <- p + geom_line(
              data      = d2,
              aes(x = .data[[x_var]], y = .data$.y2_scaled,
                  color = .data$.curve_label, group = .data$.curve_label),
              linewidth = y2_lw, linetype = "solid", na.rm = TRUE, show.legend = TRUE
            )
          } else if (viz_type == "scatter") {
            p <- p + geom_point(
              data      = d2,
              aes(x = .data[[x_var]], y = .data$.y2_scaled,
                  color = .data$.curve_label),
              size = 3, shape = 17, alpha = 0.9, na.rm = TRUE, show.legend = TRUE
            )
          } else if (viz_type == "bar") {
            p <- p +
              geom_line(
                data      = d2,
                aes(x = .data[[x_var]], y = .data$.y2_scaled,
                    color = .data$.curve_label, group = .data$.curve_label),
                linewidth = y2_lw, linetype = "solid", na.rm = TRUE, show.legend = TRUE
              ) + geom_point(
                data  = d2,
                aes(x = .data[[x_var]], y = .data$.y2_scaled,
                    color = .data$.curve_label),
                size = 2.5, na.rm = TRUE, show.legend = FALSE
              )
          }
        }
        
        # scale_color_manual unifié (Y1 + Y2) → légende complète
        all_color_map <- c(y1_color_map, unlist(y2_color_map))
        if (length(all_color_map) > 0) {
          tryCatch(
            p <- p + scale_color_manual(
              values = all_color_map,
              breaks = names(all_color_map),
              labels = names(all_color_map),
              name   = NULL, drop = FALSE
            ),
            error = function(e) invisible(NULL)
          )
        }
        
        # Axe Y secondaire (sec.axis)
        yv_first <- y2_active_vars[1]
        if (yv_first %in% names(raw_data)) {
          y2_v   <- raw_data[[yv_first]]
          y2_mn  <- min(y2_v, na.rm = TRUE)
          y2_mx  <- max(y2_v, na.rm = TRUE)
          y2_rng <- y2_mx - y2_mn; if (y2_rng == 0) y2_rng <- 1
          
          # Appliquer les limites personnalisées de l'utilisateur pour Y2
          user_y2_min  <- if (!is.null(input$y2AxisMin)  && !is.na(input$y2AxisMin))  input$y2AxisMin  else y2_mn
          user_y2_max  <- if (!is.null(input$y2AxisMax)  && !is.na(input$y2AxisMax))  input$y2AxisMax  else y2_mx
          user_y2_step <- {
            s <- input$y2AxisBreakStep %||% input$y2AxisStep
            if (!is.null(s) && !is.na(s) && is.numeric(s) && s > 0) s else NULL
          }
          
          # Recalculer sf2/off2 avec les limites utilisateur
          effective_y2_mn  <- user_y2_min
          effective_y2_mx  <- user_y2_max
          effective_y2_rng <- effective_y2_mx - effective_y2_mn
          if (effective_y2_rng == 0) effective_y2_rng <- 1
          sf2  <- y1_range / effective_y2_rng
          off2 <- y1_min - effective_y2_mn * sf2
          
          y2_label <- if (!is.null(input$y2AxisLabel) && nchar(trimws(input$y2AxisLabel)) > 0)
                        input$y2AxisLabel
                      else paste(y2_active_vars, collapse = " / ")
          
          # Construire les breaks pour l'axe Y2 si un pas est défini
          y2_breaks_fn <- if (!is.null(user_y2_step)) {
            # Transformer les breaks Y2 dans l'espace Y1 pour sec_axis
            y2_breaks_raw <- seq(effective_y2_mn, effective_y2_mx, by = user_y2_step)
            # Les breaks sont dans l'espace Y1 (transformés), sec_axis les ré-affiche en Y2
            y2_breaks_y1space <- y2_breaks_raw * sf2 + off2
            waiver()   # sec_axis gère lui-même les breaks via la transformation
          } else {
            waiver()
          }
          
          # Breaks Y1 mis à l'échelle pour couvrir la plage Y2 visible
          y1_display_min <- effective_y2_mn * sf2 + off2
          y1_display_max <- effective_y2_mx * sf2 + off2
          
          # Breaks de l'axe secondaire (dans l'espace Y2)
          sec_breaks <- if (!is.null(user_y2_step)) {
            seq(effective_y2_mn, effective_y2_mx, by = user_y2_step)
          } else {
            waiver()
          }
          
          tryCatch(
            p <- p + suppressMessages(scale_y_continuous(
              limits   = c(min(y1_min, y1_display_min), max(y1_max, y1_display_max)),
              sec.axis = sec_axis(
                transform = ~ (. - off2) / sf2,
                name      = y2_label,
                breaks    = sec_breaks
              )
            )),
            error = function(e) tryCatch(
              p <<- suppressWarnings(suppressMessages(p + scale_y_continuous(
                sec.axis = sec_axis(
                  transform = ~ (. - off2) / sf2,
                  name      = y2_label,
                  breaks    = sec_breaks
                )
              ))),
              error = function(e2) invisible(NULL)
            )
          )
        }
        
      }, error = function(e) {
        showNotification(paste("Axe Y2 :", e$message), type = "warning", duration = 4)
      })
    }
    
    # Ajouter le facetting si demandé
    if(!is.null(input$vizFacetVar) && input$vizFacetVar != "Aucun") {
      facet_var_safe <- if (grepl("[/+*^()%$@!? -]|^[0-9]", input$vizFacetVar, perl = TRUE)) {
        paste0("`", input$vizFacetVar, "`")
      } else { input$vizFacetVar }
      p <- p + facet_wrap(as.formula(paste("~", facet_var_safe)),
                          scales = if(isTRUE(input$facetScalesFree)) "free" else "fixed")
    }
    
    # ── Appliquer les labels de légende personnalisés 
    if (!is.null(color_var)) {
      storage_key <- if (color_var == "Variable") "multiY_legend" else color_var
      legend_map  <- values$legendLabels[[storage_key]]
      
      if (!is.null(legend_map) && length(legend_map) > 0) {
        # Niveaux actuels dans les données
        if (color_var %in% names(data)) {
          current_levels <- unique(as.character(data[[color_var]]))
        } else {
          current_levels <- names(legend_map)
        }
        
        lm_filt <- legend_map[names(legend_map) %in% current_levels]
        
        if (length(lm_filt) > 0) {
          orig_keys  <- names(lm_filt)
          new_labels <- as.character(lm_filt)
          
          # Couleurs : utiliser colourInput si disponibles, sinon hue_pal()
          default_pal <- c("#2196F3","#4CAF50","#9C27B0","#FF5722","#00BCD4",
                           "#FFC107","#E91E63","#607D8B","#795548","#009688")
          color_vals  <- setNames(
            vapply(seq_along(orig_keys), function(i) {
              col_id   <- paste0("curveColor_", make.names(orig_keys[i]))
              usr_col  <- input[[col_id]]
              if (!is.null(usr_col) && nchar(trimws(usr_col)) > 0) return(usr_col)
              # Couleur hue_pal par défaut
              tryCatch(scales::hue_pal()(length(orig_keys))[i],
                       error = function(e) default_pal[((i-1L) %% length(default_pal)) + 1L])
            }, character(1)),
            orig_keys
          )
          
          # Appliquer scale_color_manual + scale_fill_manual 
          tryCatch({
            p <- p +
              scale_color_manual(
                values = color_vals,
                breaks = orig_keys,
                labels = new_labels,
                name   = NULL, drop = FALSE
              ) +
              scale_fill_manual(
                values = color_vals,
                breaks = orig_keys,
                labels = new_labels,
                name   = NULL, drop = FALSE
              )
          }, error = function(e) invisible(NULL))
        }
      }
    }
    
    # Personnalisation du thème pour tous les types de graphiques
    # Créer les paramètres de formatage des axes
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
    
    y_tick_face <- if(isTRUE(input$yTickBold) && isTRUE(input$yTickItalic)) {
      "bold.italic"
    } else if(isTRUE(input$yTickBold)) {
      "bold"
    } else if(isTRUE(input$yTickItalic)) {
      "italic"
    } else {
      "plain"
    }
    
    x_tick_size <- input$xTickSize %||% 12
    y_tick_size <- input$yTickSize %||% 12
    
    # Obtenir l'angle des labels X
    x_angle <- input$xAxisAngle %||% 0
    x_hjust <- if(x_angle > 0) 1 else 0.5
    x_vjust <- if(x_angle > 0) 1 else 0.5
    
    # Appliquer le thème pour tous les graphiques (arrière-plan choisi)
    axis_lw <- input$axisLineSize %||% 0.8
    
    # Marges du graphique (en points — 1pt ≈ 1px écran)
    pm_top    <- input$plotMarginTop    %||% 10
    pm_right  <- input$plotMarginRight  %||% 30
    pm_bottom <- input$plotMarginBottom %||% 10
    pm_left   <- input$plotMarginLeft   %||% 10
    
    p <- p +
      get_plot_theme(base_size = input$baseFontSize %||% 12) +
      theme(
        plot.title    = element_markdown(hjust = 0.5, face = "bold", size = input$titleSize %||% 14),
        axis.title.x  = element_markdown(face = x_axis_face, size = input$axisLabelSize %||% 12),
        axis.title.y  = element_markdown(face = y_axis_face, size = input$axisLabelSize %||% 12),
        axis.text.x   = element_text(face = x_tick_face, size = x_tick_size, angle = x_angle, hjust = x_hjust, vjust = x_vjust),
        axis.text.y   = element_text(face = y_tick_face, size = y_tick_size),
        legend.position    = input$legendPosition %||% "right",
        legend.title       = element_markdown(size = input$legendTitleSize %||% 12, face = "bold"),
        legend.text        = element_text(size = input$legendTextSize  %||% 12),
        legend.key.size    = unit(input$legendKeySize %||% 1, "lines"),
        legend.margin      = margin(t = 6, r = 6, b = 6, l = 6),
        legend.box.margin  = margin(t = 10, r = 0, b = 0, l = 0),
        axis.line   = element_line(color = "black", linewidth = axis_lw),
        axis.ticks  = element_line(color = "black", linewidth = axis_lw * 0.75),
        plot.margin = margin(t = pm_top, r = pm_right, b = pm_bottom, l = pm_left, unit = "pt")
      )
    
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
    
    # Ajouter le titre de légende personnalisé
    if(!is.null(input$legendTitle) && input$legendTitle != "" && !is.null(color_var)) {
      p <- p + labs(color = input$legendTitle, fill = input$legendTitle)
    }
    
    # ---- Graduations et limites des axes ----
    # Calculer les breaks Y si demandé
    y_breaks_custom <- NULL
    x_breaks_custom <- NULL
    
    if(isTRUE(input$customAxisBreaks)) {
      y_step <- input$yAxisBreakStep
      if(!is.null(y_step) && !is.na(y_step) && is.numeric(y_step) && y_step > 0) {
        tryCatch({
          y_data <- data[[y_var]]
          y_data <- y_data[is.finite(y_data)]
          if(length(y_data) > 0) {
            y_b_min <- floor(min(y_data, na.rm = TRUE) / y_step) * y_step
            y_b_max <- ceiling(max(y_data, na.rm = TRUE) / y_step) * y_step
            y_breaks_custom <- seq(y_b_min, y_b_max, by = y_step)
          }
        }, error = function(e) NULL)
      }
      
      x_step <- input$xAxisBreakStep
      if(!is.null(x_step) && !is.na(x_step) && is.numeric(x_step) && x_step > 0) {
        tryCatch({
          x_data <- data[[x_var]]
          if(is.numeric(x_data)) {
            x_data <- x_data[is.finite(x_data)]
            if(length(x_data) > 0) {
              x_b_min <- floor(min(x_data, na.rm = TRUE) / x_step) * x_step
              x_b_max <- ceiling(max(x_data, na.rm = TRUE) / x_step) * x_step
              x_breaks_custom <- seq(x_b_min, x_b_max, by = x_step)
            }
          }
        }, error = function(e) NULL)
      }
    }
    
    # Appliquer les breaks 
    if(!is.null(y_breaks_custom) && !viz_type %in% c("pie", "donut", "treemap")) {
      tryCatch({ p <- p + scale_y_continuous(breaks = y_breaks_custom) }, error = function(e) NULL)
    }

    if(!is.null(x_breaks_custom) && !viz_type %in% c(
        "pie", "donut", "treemap", "bar", "box", "violin",
        "histogram", "density", "line", "scatter", "area",
        "seasonal_smooth", "seasonal_evolution")) {
      tryCatch({ p <- p + scale_x_continuous(breaks = x_breaks_custom) }, error = function(e) NULL)
    }
    
    # ---- Limites des axes via coord_cartesian uniquement 
    y_min_val <- input$yAxisMin
    y_max_val <- input$yAxisMax
    x_min_val <- input$xAxisMin
    x_max_val <- input$xAxisMax
    
    has_y_limits <- (!is.null(y_min_val) && !is.na(y_min_val)) || (!is.null(y_max_val) && !is.na(y_max_val))
    has_x_limits <- (!is.null(x_min_val) && !is.na(x_min_val)) || (!is.null(x_max_val) && !is.na(x_max_val))
    
    y_lim <- NULL
    x_lim <- NULL
    
    if(has_y_limits && !viz_type %in% c("pie", "donut", "treemap")) {
      y_lim <- c(
        if(!is.null(y_min_val) && !is.na(y_min_val)) as.numeric(y_min_val) else NA_real_,
        if(!is.null(y_max_val) && !is.na(y_max_val)) as.numeric(y_max_val) else NA_real_
      )
    }
    
    if(has_x_limits && !viz_type %in% c("pie", "donut", "treemap", "bar", "box", "violin", "histogram", "density")) {
      x_data_check <- data[[x_var]]
      if(is.numeric(x_data_check)) {
        x_lim <- c(
          if(!is.null(x_min_val) && !is.na(x_min_val)) as.numeric(x_min_val) else NA_real_,
          if(!is.null(x_max_val) && !is.na(x_max_val)) as.numeric(x_max_val) else NA_real_
        )
      }
    }
    
    if(!is.null(y_lim) || !is.null(x_lim)) {
      tryCatch({
        p <- p + coord_cartesian(
          ylim = y_lim,
          xlim = x_lim,
          expand = TRUE
        )
      }, error = function(e) NULL)
    }
    
    # ── Appliquer les contrôles ggplot thème pour l'axe Y secondaire ──────────
 
    # Tous les paramètres visuels MIROIR de l'axe Y principal.
    if (isTRUE(values$dualAxisActive) && length(values$y2VarsActive %||% character(0)) > 0) {
      # Miroir exact de Y1 : mêmes inputs
      y2_lw_gg    <- axis_lw                 
      y2_tick_sz  <- y_tick_size             
      y2_tick_fce <- y_tick_face             
      y2_lbl_sz   <- input$axisLabelSize %||% 12  
      y2_lbl_fce  <- y_axis_face             
      tryCatch({
        p <- p + theme(
          # Texte des graduations Y2 
          axis.text.y.right  = element_text(
            size  = y2_tick_sz,
            face  = y2_tick_fce,
            color = "black"
          ),
          # Label (titre) axe Y2 
          axis.title.y.right = element_text(
            size   = y2_lbl_sz,
            face   = y2_lbl_fce,
            color  = "black",
            margin = margin(l = 8)
          ),
          # Ligne axe Y2 
          axis.line.y.right  = element_line(
            color     = "black",
            linewidth = y2_lw_gg
          ),
          # Graduations (ticks) Y2 
          axis.ticks.y.right = element_line(
            color     = "black",
            linewidth = y2_lw_gg * 0.75
          ),
          # Longueur des ticks Y2 identique à Y1
          axis.ticks.length.y.right = unit(4, "pt")
        )
      }, error = function(e) invisible(NULL))
    }
    
    return(p)
  })
  
  
  # INFORMATIONS ET STATS
  
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
  
  # Information pour les courbes saisonnière 
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
  output$seasonalDuplicateWarning <- renderUI({
    req(input$vizType %in% c("seasonal_smooth","seasonal_evolution"))
    req(values$filteredData, input$vizXVar, input$vizYVar)
    tryCatch({
      d  <- if (isTRUE(input$useAggregation)) aggregatedData() else values$filteredData
      xv <- input$vizXVar; yv <- input$vizYVar[1]
      if (!xv %in% names(d) || !yv %in% names(d)) return(NULL)
      d_valid <- d[!is.na(d[[yv]]), ]
      x_total <- nrow(d_valid); x_uniq <- length(unique(d_valid[[xv]]))
      if (x_total==0) return(div(class="alert alert-danger",
        style="padding:10px;margin-bottom:10px;font-size:13px;",
        icon("times-circle"), strong(" Aucune donnée valide — toutes les Y sont NA.")))
      if (x_total>x_uniq && !isTRUE(input$useAggregation))
        return(div(class="alert alert-warning",
          style="padding:10px;margin-bottom:10px;font-size:13px;",
          icon("exclamation-triangle"), strong(" Valeurs X répétées"),
          sprintf(" (%d obs. → %d valeurs uniques).", x_total, x_uniq), tags$br(),
          "Agrégation automatique par ", strong("moyenne"), " appliquée.",tags$br(),
          "Pour une autre fonction, activez ", strong("l'agrégation manuelle"), "."))
      return(div(class="alert alert-success",
        style="padding:8px 12px;margin-bottom:10px;font-size:12px;",
        icon("check-circle"), sprintf(" %d valeurs X uniques — courbe directe.", x_uniq)))
    }, error=function(e) NULL)
  })
  
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
  
  
  # INDICATEURS RÉACTIFS
  
  # Indicateur multi-Y
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
  
  # Bouton Personnaliser
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
  
  # Appliquer la personnalisation rapide
  observeEvent(input$applyQuickCustom, {
    updateTextInput(session, "plotTitle", value = input$quickPlotTitle)
    updateTextInput(session, "xAxisLabel", value = input$quickXLabel)
    updateTextInput(session, "yAxisLabel", value = input$quickYLabel)
    updateSliderInput(session, "pointSize", value = input$quickPointSize)
    updateSliderInput(session, "lineWidth", value = input$quickLineWidth)
    updateSelectInput(session, "legendPosition", selected = input$quickLegendPos)
    
    # Forcer le recalcul du graphique
    values$plotUpdateTrigger <- runif(1)
    
    removeModal()
    showNotification("Personnalisation appliquée", type = "message", duration = 2)
  })
  
  
  # FONCTIONS DE CRÉATION
  
  # Paramètres pour les étiquettes de valeurs 
  # Wrapper réactif → viz_label_params() de Utils.R
  get_label_params <- function() {
    viz_label_params(
      size     = input$valueLabelSize     %||% 3,
      color    = input$valueLabelColor    %||% "#333333",
      bold     = isTRUE(input$valueLabelBold),
      italic   = isTRUE(input$valueLabelItalic),
      digits   = input$valueLabelDigits   %||% 2,
      position = input$valueLabelPosition %||% "above"
    )
  }
  
  
  # Fonction pour créer un scatter plot
  create_scatter_plot <- function(data, x_var, y_var, color_var = NULL) {
    data <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]), ]
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    if(!is.null(color_var)) {
      p <- p + geom_point(aes(color = .data[[color_var]]),
                          size = input$pointSize %||% 3,
                          alpha = input$pointAlpha %||% 0.7, na.rm=TRUE)
    } else {
      p <- p + geom_point(size = input$pointSize %||% 3,
                          alpha = input$pointAlpha %||% 0.7, na.rm=TRUE)
    }
    
    if(isTRUE(input$showTrendLine)) {
      p <- p + geom_smooth(method = input$trendMethod %||% "lm", 
                           formula = y ~ x,
                           se = isTRUE(input$showConfidenceInterval))
    }
    
    if(isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                         vjust = lp$vjust, hjust = lp$hjust,
                         size = lp$size, color = lp$color,
                         fontface = lp$fontface, check_overlap = TRUE)
    }
    return(p)
  }
  
  # Fonction pour créer un line plot
  create_line_plot <- function(data, x_var, y_var, color_var = NULL) {
    connect_na  <- isTRUE(input$lineConnectNA)
    show_na_mk  <- isTRUE(input$lineShowNAMarker)
    lw          <- input$lineWidth  %||% 1
    ps          <- input$pointSize  %||% 2
    pa          <- input$pointAlpha %||% 0.8
    show_pts    <- isTRUE(input$showPoints)
    fixed_color <- input$lineFixedColor %||% "#2196F3"
    x_is_date    <- inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))
    x_is_numeric <- is.numeric(data[[x_var]])
    data <- tryCatch({
      if (x_is_date || x_is_numeric) data[order(data[[x_var]], na.last=TRUE), ]
      else data
    }, error=function(e) data)
    data_line <- if (connect_na) data[!is.na(data[[y_var]]), ] else data
    data_pts  <- data[!is.na(data[[y_var]]), ]
    p <- ggplot(data_line, aes(x=.data[[x_var]], y=.data[[y_var]]))
    if (!is.null(color_var)) {
      p <- p + geom_line(aes(color=.data[[color_var]], group=.data[[color_var]]),
                         linewidth=lw, na.rm=TRUE)
      if (show_pts)
        p <- p + geom_point(data=data_pts,
                            aes(x=.data[[x_var]], y=.data[[y_var]], color=.data[[color_var]]),
                            size=ps, alpha=pa, na.rm=TRUE)
    } else {
      p <- p + geom_line(color=fixed_color, linewidth=lw, na.rm=TRUE)
      if (show_pts)
        p <- p + geom_point(data=data_pts,
                            aes(x=.data[[x_var]], y=.data[[y_var]]),
                            color=fixed_color, size=ps, alpha=pa, na.rm=TRUE)
    }
    if (show_na_mk && !connect_na) {
      data_na <- data[is.na(data[[y_var]]), ]
      if (nrow(data_na) > 0) {
        y_ref <- if (nrow(data_pts)>0) min(data_pts[[y_var]],na.rm=TRUE) else 0
        data_na[[y_var]] <- y_ref
        if (!is.null(color_var) && color_var %in% names(data_na)) {
          p <- p + geom_point(data=data_na, aes(x=.data[[x_var]], y=.data[[y_var]],
                                                color=.data[[color_var]]),
                              shape=4, size=ps+1.5, alpha=0.5, na.rm=TRUE)
        } else {
          p <- p + geom_point(data=data_na, aes(x=.data[[x_var]], y=.data[[y_var]]),
                              shape=4, size=ps+1.5, color="grey55", alpha=0.5, na.rm=TRUE)
        }
      }
    }
    # Axe X : tous les types + labels + ordre 
    p <- p + get_x_scale(data, x_var)
    if (isTRUE(input$showTrendLine)) {
      p <- p + geom_smooth(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]]),
                           method=input$trendMethod %||% "lm", formula=y~x,
                           se=isTRUE(input$showConfidenceInterval), na.rm=TRUE)
    }
    if (isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]],
                                            label=round(.data[[y_var]], lp$digits)),
                         vjust=lp$vjust, hjust=lp$hjust, size=lp$size, color=lp$color,
                         fontface=lp$fontface, check_overlap=TRUE, na.rm=TRUE)
    }
    return(p)
  }
  
  # Fonction pour créer un bar plot
  create_bar_plot <- function(data, x_var, y_var, color_var = NULL) {
    if (nrow(data) == 0 || !x_var %in% names(data) || !y_var %in% names(data)) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Données insuffisantes") +
               theme_void())
    }
    
    if (!is.factor(data[[x_var]])) {
      data[[x_var]] <- factor(data[[x_var]], levels = unique(as.character(data[[x_var]])))
    }
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_col(aes(fill = .data[[color_var]]), 
                        position = input$barPosition %||% "dodge",
                        width = input$barWidth %||% 0.8)
    } else {
      p <- p + geom_col(width = input$barWidth %||% 0.8)
    }
    
    if(isTRUE(input$showValues)) {
      lp <- get_label_params()
      bar_pos <- input$barPosition %||% "dodge"
      bar_width <- input$barWidth %||% 0.8
      
      if(bar_pos == "dodge" && !is.null(color_var)) {
        p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                           vjust = lp$vjust, hjust = lp$hjust,
                           size = lp$size, color = lp$color, fontface = lp$fontface,
                           position = position_dodge(width = bar_width))
      } else if(bar_pos == "stack") {
        p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                           size = lp$size, color = lp$color, fontface = lp$fontface,
                           position = position_stack(vjust = if(lp$vjust == 0.5) 0.5 else 0.9))
      } else if(bar_pos == "fill") {
        p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                           size = lp$size, color = lp$color, fontface = lp$fontface,
                           position = position_fill(vjust = 0.5))
      } else {
        p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                           vjust = lp$vjust, hjust = lp$hjust,
                           size = lp$size, color = lp$color, fontface = lp$fontface)
      }
    }
    return(p)
  }
  
  # Fonction pour créer un box plot
  create_box_plot <- function(data, x_var, y_var, color_var = NULL) {
    if (nrow(data) == 0 || !x_var %in% names(data) || !y_var %in% names(data)) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Données insuffisantes") +
               theme_void())
    }
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_boxplot(aes(fill = .data[[color_var]]), alpha = 0.7)
    } else {
      p <- p + geom_boxplot(alpha = 0.7)
    }
    
    if(isTRUE(input$showOutliers))
      p <- p + geom_jitter(width = 0.2, alpha = 0.3, size = 1)
    
    return(p)
  }
  
  # Fonction pour créer un violin plot
  create_violin_plot <- function(data, x_var, y_var, color_var = NULL) {
    
    # Filtrer les groupes ayant moins de 2 observations (évite le crash stat_ydensity)
    group_var <- if(!is.null(color_var)) color_var else x_var
    
    if(group_var %in% names(data)) {
      group_counts <- table(data[[group_var]])
      valid_groups <- names(group_counts[group_counts >= 2])
      
      if(length(valid_groups) == 0) {
        showNotification(
          "Violin: aucun groupe ne contient au moins 2 observations. Passez en Boxplot.",
          type = "warning", duration = 4
        )
        return(create_box_plot(data, x_var, y_var, color_var))
      }
      
      removed <- setdiff(names(group_counts), valid_groups)
      if(length(removed) > 0) {
        showNotification(
          paste("Violin: groupes ignorés (< 2 obs.):", paste(removed, collapse = ", ")),
          type = "warning", duration = 4
        )
      }
      
      data <- data[data[[group_var]] %in% valid_groups, ]
      if(is.factor(data[[group_var]])) {
        data[[group_var]] <- droplevels(data[[group_var]])
      }
    }
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_violin(aes(fill = .data[[color_var]]), alpha = 0.7, drop = FALSE)
    } else {
      p <- p + geom_violin(alpha = 0.7, drop = FALSE)
    }
    
    if(isTRUE(input$showBoxInsideViolin)) {
      p <- p + geom_boxplot(width = 0.1)
    }
    
    return(p)
  }
  
  # Fonction pour créer un seasonal smooth plot
  create_seasonal_smooth_plot <- function(data, x_var, y_var, color_var = NULL) {
    x_is_date    <- inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))
    x_is_numeric <- is.numeric(data[[x_var]]) && !x_is_date
    x_is_factor  <- is.factor(data[[x_var]]) || is.character(data[[x_var]])
    # Garder toutes les X avant filtrage NA 
    all_x_orig <- data[[x_var]]
    
    co_ord <- values$customXOrder
    # Pour les X catégoriels : convertir en facteur ordonné
    if (x_is_factor) {
      lvls_orig <- if (is.factor(data[[x_var]])) levels(data[[x_var]])
                   else sort(unique(as.character(data[[x_var]])))
      if (!is.null(co_ord) && length(co_ord) > 0) {
        ord <- co_ord[co_ord %in% lvls_orig]
        if (length(ord) > 0) lvls_orig <- ord
      }
      data[[x_var]] <- factor(as.character(data[[x_var]]), levels = lvls_orig)
      all_x_orig <- data[[x_var]]
    }
    
    group_cols <- if (!is.null(color_var) && color_var %in% names(data)) c(x_var, color_var) else x_var
    x_counts <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(n=dplyr::n(), .groups="drop")
    has_duplicates <- any(x_counts$n > 1)
    if (has_duplicates && !isTRUE(input$useAggregation)) {
      data <- data %>%
        dplyr::filter(!is.na(.data[[y_var]])) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::summarise(!!y_var := mean(.data[[y_var]], na.rm=TRUE), .groups="drop")
      showNotification(paste0("Seasonal Smooth : agrégation auto par moyenne (",nrow(data)," pts)."),
                       type="message", duration=4)
    } else {
      data <- data[!is.na(data[[y_var]]), ]
    }
    if (x_is_date || x_is_numeric) data <- data[order(data[[x_var]]), ]
    n_pts <- if (!is.null(color_var) && color_var %in% names(data)) min(table(data[[color_var]])) else nrow(data)
    smooth_method <- input$smoothMethod %||% "loess"
    if (n_pts < 4 && smooth_method=="loess") {
      showNotification("Trop peu de points pour LOESS. Méthode lm utilisée.", type="warning", duration=4)
      smooth_method <- "lm"
    }
    lw <- input$lineWidth %||% 1
    ps <- input$pointSize %||% 2
    fixed_color <- input$lineFixedColor %||% "#2196F3"
    p <- ggplot(data, aes(x=.data[[x_var]], y=.data[[y_var]]))
    if (!is.null(color_var) && color_var %in% names(data)) {
      p <- p +
        geom_line(aes(color=.data[[color_var]], group=.data[[color_var]]),
                  linewidth=lw, alpha=0.5, na.rm=TRUE) +
        geom_smooth(aes(color=.data[[color_var]], group=.data[[color_var]]),
                    method=smooth_method, formula=y~x, span=input$smoothSpan %||% 0.75,
                    se=isTRUE(input$showConfidenceInterval), na.rm=TRUE)
      if (isTRUE(input$showPoints))
        p <- p + geom_point(aes(color=.data[[color_var]]), size=ps, alpha=0.7, na.rm=TRUE)
    } else {
      p <- p +
        geom_line(color=fixed_color, linewidth=lw, alpha=0.5, na.rm=TRUE) +
        geom_smooth(color=fixed_color, fill=fixed_color,
                    method=smooth_method, formula=y~x, span=input$smoothSpan %||% 0.75,
                    se=isTRUE(input$showConfidenceInterval), na.rm=TRUE)
      if (isTRUE(input$showPoints))
        p <- p + geom_point(color=fixed_color, size=ps, alpha=0.7, na.rm=TRUE)
    }
    # Axe X : tous les types + labels + ordre 

    tmp_df_scale <- data.frame(I(all_x_orig))
    names(tmp_df_scale) <- x_var
    p <- p + get_x_scale(tmp_df_scale, x_var)
    if (isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(aes(label=round(.data[[y_var]], lp$digits)),
                         vjust=lp$vjust, hjust=lp$hjust, size=lp$size, color=lp$color,
                         fontface=lp$fontface, check_overlap=TRUE, na.rm=TRUE)
    }
    return(p)
  }
  
  # Fonction pour créer un seasonal evolution plot
  create_seasonal_evolution_plot <- function(data, x_var, y_var, color_var = NULL) {
    x_is_date    <- inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))
    x_is_numeric <- is.numeric(data[[x_var]]) && !x_is_date
    x_is_factor  <- is.factor(data[[x_var]]) || is.character(data[[x_var]])
    # Garder toutes les X avant filtrage 
    all_x_orig <- data[[x_var]]
    # Lire l'ordre personnalisé 
    co_ord <- values$customXOrder

    group_cols <- if (!is.null(color_var) && color_var %in% names(data)) c(x_var, color_var) else x_var

    # Pour les X catégoriels/texte : convertir en facteur ordonné d'abord
    if (x_is_factor) {
      lvls_orig <- if (is.factor(data[[x_var]])) levels(data[[x_var]])
                   else sort(unique(as.character(data[[x_var]])))
      if (!is.null(co_ord) && length(co_ord) > 0) {
        ord <- co_ord[co_ord %in% lvls_orig]
        if (length(ord) > 0) lvls_orig <- ord
      }
      data[[x_var]] <- factor(as.character(data[[x_var]]), levels = lvls_orig)
      all_x_orig <- data[[x_var]]
    }
    
    x_counts <- data %>%
      dplyr::filter(!is.na(.data[[y_var]])) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(n=dplyr::n(), .groups="drop")
    has_duplicates <- nrow(x_counts)>0 && any(x_counts$n>1)
    if (has_duplicates && !isTRUE(input$useAggregation)) {
      data_valid <- data %>%
        dplyr::filter(!is.na(.data[[y_var]])) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::summarise(!!y_var := mean(.data[[y_var]],na.rm=TRUE), .groups="drop")
      showNotification(paste0("Évolution : agrégation auto par moyenne. ",
                              nrow(data_valid)," pts uniques."), type="message", duration=5)
      data_plot <- data_valid; data_pts <- data_valid
    } else {
      data_plot <- data[!is.na(data[[y_var]]), ]
      data_pts  <- data_plot
    }
    # Trier : dates et numériques par valeur, facteurs par ordre des niveaux
    # Guard : données vides après filtrage/agrégation
    if (nrow(data_plot) == 0) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Aucune donnée valide",
                                 color="#e74c3c", size=5) + theme_void())
    }
    if (x_is_date || x_is_numeric) {
      data_plot <- data_plot[order(data_plot[[x_var]]), ]
      data_pts  <- data_pts[order(data_pts[[x_var]]),  ]
    } else if (x_is_factor) {
      if (is.factor(data_plot[[x_var]])) {
        data_plot <- data_plot[order(as.integer(data_plot[[x_var]])), ]
        data_pts  <- data_pts[order(as.integer(data_pts[[x_var]])),  ]
      }
    }
    # Pour le geom_line sur X catégoriel : nécessite group aesthetic
    needs_group_aes <- x_is_factor && is.null(color_var)
    lw          <- input$lineWidth %||% 1.2
    ps          <- input$pointSize %||% 3
    fixed_color <- input$lineFixedColor %||% "#2196F3"
    p <- ggplot(data_plot, aes(x=.data[[x_var]], y=.data[[y_var]]))
    if (!is.null(color_var) && color_var %in% names(data_plot)) {
      p <- p +
        geom_line(aes(color=.data[[color_var]], group=.data[[color_var]]),
                  linewidth=lw, na.rm=TRUE) +
        geom_point(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]],
                                      color=.data[[color_var]]), size=ps, na.rm=TRUE)
    } else {
      # Pour X catégoriel sans color_var : group=1 requis par geom_line
      if (isTRUE(needs_group_aes)) {
        p <- p +
          geom_line(aes(group=1), color=fixed_color, linewidth=lw, na.rm=TRUE) +
          geom_point(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]]),
                     color=fixed_color, size=ps, na.rm=TRUE)
      } else {
        p <- p +
          geom_line(color=fixed_color, linewidth=lw, na.rm=TRUE) +
          geom_point(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]]),
                     color=fixed_color, size=ps, na.rm=TRUE)
      }
    }
    if (isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]],
                                            label=round(.data[[y_var]], lp$digits)),
                         vjust=lp$vjust, hjust=lp$hjust, size=lp$size, color=lp$color,
                         fontface=lp$fontface, check_overlap=TRUE, na.rm=TRUE)
    }
    # Axe X : tous les types + labels + ordre 
    tmp_df_scale <- data.frame(I(all_x_orig))
    names(tmp_df_scale) <- x_var
    p <- p + get_x_scale(tmp_df_scale, x_var)
    # NOTE : thème, scale_y et limites sont appliqués par createPlot après cet appel.
    # On ajoute uniquement la grille spécifique aux courbes d'évolution.
    p <- p + theme(
      panel.grid.major.y = element_line(color = "#e0e0e0", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.margin      = margin(t = 15)
    )
    if (!is.null(color_var) && color_var %in% names(data_plot))
      p <- p + guides(color = guide_legend(override.aes = list(size = 2), ncol = 2))
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
    
    # Ajouter les valeurs si demandé
    if(isTRUE(input$showValues)) {
      p <- p + geom_text(aes(label = count), color = "black", size = 3)
    }
    
    return(p)
  }
  
  # Fonction pour créer un area plot
  create_area_plot <- function(data, x_var, y_var, color_var = NULL) {
    data <- tryCatch({
      if (inherits(data[[x_var]], c("Date","POSIXct","POSIXlt")) ||
          is.numeric(data[[x_var]])) data[order(data[[x_var]], na.last=TRUE), ]
      else data
    }, error=function(e) data)
    data <- data[!is.na(data[[y_var]]), ]
    lw <- input$lineWidth %||% 1
    p <- ggplot(data, aes(x=.data[[x_var]], y=.data[[y_var]]))
    if (!is.null(color_var)) {
      p <- p + geom_area(aes(fill=.data[[color_var]], group=.data[[color_var]]),
                         position=input$areaPosition %||% "stack", alpha=0.7, na.rm=TRUE) +
               geom_line(aes(color=.data[[color_var]], group=.data[[color_var]]),
                         linewidth=lw, na.rm=TRUE)
    } else {
      p <- p + geom_area(fill="steelblue", alpha=0.7, na.rm=TRUE) +
               geom_line(linewidth=lw, color="steelblue4", na.rm=TRUE)
    }
    if (isTRUE(input$showPoints)) {
      if (!is.null(color_var))
        p <- p + geom_point(aes(color=.data[[color_var]]), size=input$pointSize %||% 2, na.rm=TRUE)
      else
        p <- p + geom_point(size=input$pointSize %||% 2, na.rm=TRUE)
    }
    if (isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(aes(label=round(.data[[y_var]], lp$digits)),
                         vjust=lp$vjust, hjust=lp$hjust, size=lp$size, color=lp$color,
                         fontface=lp$fontface, check_overlap=TRUE, na.rm=TRUE)
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
    
    # Toujours afficher les valeurs/pourcentages pour les pie charts
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
    
    # Toujours afficher les valeurs/pourcentages pour les donut charts
    p <- p + geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                       position = position_stack(vjust = 0.5))
    
    return(p)
  }
  
  # Fonction pour créer un treemap
  create_treemap_plot <- function(data, x_var, y_var) {
    
    treemap_data <- data %>%
      group_by(across(all_of(x_var))) %>%
      summarise(total = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(treemap_data, aes(area = total, fill = .data[[x_var]], label = .data[[x_var]])) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(colour = "white", place = "centre")
    
    return(p)
  }
  
  
  # EXPORT AVANCÉ DES GRAPHIQUES
  
  # Calcul des dimensions selon DPI 
  output$calculatedDimensions <- renderUI({
    req(input$exportDPI)
    dpi <- input$exportDPI
    if (is.null(dpi) || is.na(dpi)) dpi <- 300
    dpi <- max(300, min(20000, dpi))
    
    if (dpi <= 600) { w <- 12; h <- 8 }
    else if (dpi <= 1200) { w <- 10; h <- 6.67 }
    else if (dpi <= 2400) { w <- 8; h <- 5.33 }
    else if (dpi <= 5000) { w <- 7; h <- 4.67 }
    else { w <- 6; h <- 4 }
    
    px_w <- round(w * dpi)
    px_h <- round(h * dpi)
    
    div(
      style = "padding: 8px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
             border-radius: 5px; color: white; font-size: 13px;",
      div(style = "font-weight: bold;", sprintf("%.1f × %.1f pouces", w, h)),
      div(style = "font-size: 11px; opacity: 0.9;",
          sprintf("%s × %s px", format(px_w, big.mark = " "), format(px_h, big.mark = " ")))
    )
  })
  
  # Fonction helper pour créer le graphique 
  getPlotForDownload <- function() {
    # Vérifier si les données existent
    if (is.null(values$plotData) || 
        is.null(input$vizXVar) || 
        is.null(input$vizYVar) || 
        is.null(input$vizType)) {
      return(NULL)
    }
    
    tryCatch({
      return(createPlot())
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Téléchargement du graphique 
  observeEvent(input$downloadPlotBtn, {
    
    # Désactiver le bouton pendant le traitement
    shinyjs::disable("downloadPlotBtn")
    
    tryCatch({
      # Notification de début
      showNotification("Génération du graphique en cours...", type = "message", duration = 2, id = "download_notif")
      
      # Créer le graphique
      p <- getPlotForDownload()
      
      # Si pas de graphique, créer un placeholder
      if (is.null(p)) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Veuillez d'abord charger des données\net créer un graphique", 
                   size = 5) + 
          theme_void() +
          theme(plot.background = element_rect(fill = "white", color = NA))
      }
      
      # Récupérer les paramètres
      fmt <- input$exportFormat
      if (is.null(fmt)) fmt <- "png"
      
      dpi <- input$exportDPI
      if (is.null(dpi) || is.na(dpi)) dpi <- 300
      dpi <- as.integer(max(300, min(20000, dpi)))
      
      # Calculer les dimensions
      if (dpi <= 600) { w <- 12; h <- 8 }
      else if (dpi <= 1200) { w <- 10; h <- 6.67 }
      else if (dpi <= 2400) { w <- 8; h <- 5.33 }
      else if (dpi <= 5000) { w <- 7; h <- 4.67 }
      else { w <- 6; h <- 4 }
      
      # Créer un fichier temporaire avec la bonne extension
      ext <- switch(fmt,
                    "jpeg" = "jpg",
                    "tiff" = "tif",
                    fmt)
      
      temp_file <- tempfile(fileext = paste0(".", ext))
      
      # Fermer les devices ouverts
      while(length(dev.list()) > 0) try(dev.off(), silent = TRUE)
      
      # Sauvegarder le graphique
      ggplot2::ggsave(
        filename = temp_file,
        plot = p,
        device = fmt,
        width = w,
        height = h,
        units = "in",
        dpi = dpi,
        bg = "white"
      )
      
      # Vérifier que le fichier existe
      if (!file.exists(temp_file)) {
        showNotification("Erreur: Le fichier n'a pas pu être créé", type = "error", duration = 5)
        shinyjs::enable("downloadPlotBtn")
        return()
      }
      
      # Lire le fichier en binaire et encoder en base64
      file_content <- readBin(temp_file, "raw", file.info(temp_file)$size)
      base64_content <- base64enc::base64encode(file_content)
      
      # Déterminer le type MIME
      mime_type <- switch(fmt,
                          "png" = "image/png",
                          "jpeg" = "image/jpeg",
                          "tiff" = "image/tiff",
                          "bmp" = "image/bmp",
                          "pdf" = "application/pdf",
                          "svg" = "image/svg+xml",
                          "eps" = "application/postscript",
                          "application/octet-stream")
      
      # Générer le nom du fichier
      filename <- paste0("graphique_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      
      # Injecter le JavaScript pour télécharger 
      js_code <- sprintf(
        "
      (function() {
        var link = document.createElement('a');
        link.href = 'data:%s;base64,%s';
        link.download = '%s';
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      })();
      ",
        mime_type,
        base64_content,
        filename
      )
      
      shinyjs::runjs(js_code)
      
      # Supprimer le fichier temporaire
      unlink(temp_file)
      
      # Notification de succès
      removeNotification(id = "download_notif")
      showNotification(
        paste("Téléchargement réussi:", filename), 
        type = "message", 
        duration = 4
      )
      
    }, error = function(e) {
      showNotification(
        paste("Erreur lors du téléchargement:", e$message), 
        type = "error", 
        duration = 8
      )
    })
    
    # Réactiver le bouton
    shinyjs::enable("downloadPlotBtn")
    
  })
  
  # Rendu du graphique interactif 
  output$interactivePlot <- renderPlotly({
    req(createPlot())
    
    p <- createPlot()
    
    #  famille de police bold/italic pour plotly ----
    make_font_family <- function(bold, italic) {
      if (isTRUE(bold) && isTRUE(italic)) "Arial Bold Italic, Helvetica Bold Italic, sans-serif"
      else if (isTRUE(bold))              "Arial Bold, Helvetica Bold, sans-serif"
      else if (isTRUE(italic))            "Arial Italic, Helvetica Oblique, sans-serif"
      else                                "Arial, Helvetica, sans-serif"
    }
    
    # ---- Lire tous les inputs ici pour garantir la réactivité ----
    x_tick_size   <- input$xTickSize    %||% 10
    y_tick_size   <- input$yTickSize    %||% 10
    x_tick_bold   <- isTRUE(input$xTickBold)
    x_tick_italic <- isTRUE(input$xTickItalic)
    y_tick_bold   <- isTRUE(input$yTickBold)
    y_tick_italic <- isTRUE(input$yTickItalic)
    x_axis_bold   <- isTRUE(input$xAxisBold)
    x_axis_italic <- isTRUE(input$xAxisItalic)
    y_axis_bold   <- isTRUE(input$yAxisBold)
    y_axis_italic <- isTRUE(input$yAxisItalic)
    
    show_values <- isTRUE(input$showValues)
    val_pos_key <- input$valueLabelPosition %||% "above"
    val_bold    <- isTRUE(input$valueLabelBold)
    val_italic  <- isTRUE(input$valueLabelItalic)
    val_color   <- input$valueLabelColor  %||% "#333333"
    val_size_px <- round((input$valueLabelSize %||% 3) * 4)
    
    # Mapping position utilisateur -> textposition plotly natif
    val_pos <- switch(val_pos_key,
                      "above"  = "top center",
                      "below"  = "bottom center",
                      "center" = "middle center",
                      "right"  = "middle right",
                      "left"   = "middle left",
                      "top center"
    )
    val_family <- make_font_family(val_bold, val_italic)
    
    # ---- Conversion ggplot -> plotly ----
    plotly_obj <- tryCatch({
      ggplotly(p, tooltip = "all")
    }, error = function(e) {
      showNotification("Erreur lors de la conversion en graphique interactif",
                       type = "error", duration = 3)
      return(NULL)
    })
    
    if (is.null(plotly_obj)) return(NULL)
    
    #  position + police des valeurs sur les traces text ----
    
    if (show_values) {
      for (i in seq_along(plotly_obj$x$data)) {
        tr <- plotly_obj$x$data[[i]]
        if (!is.null(tr$mode) && grepl("text", tr$mode, fixed = TRUE)) {
          plotly_obj$x$data[[i]]$textposition <- val_pos
          plotly_obj$x$data[[i]]$textfont <- list(
            family = val_family,
            size   = val_size_px,
            color  = val_color
          )
        }
      }
    }
    
    #  police des niveaux (tickfont) et labels (titlefont) des axes ----
    
    # ── SECOND AXE Y PLOTLY : remplacer les valeurs mises-à-l'échelle ggplot 
    
    y2_active_plotly <- if (isTRUE(values$dualAxisActive) &&
                            !is.null(values$y2VarsActive) &&
                            length(values$y2VarsActive) > 0)
                          values$y2VarsActive else character(0)
    
    if (length(y2_active_plotly) > 0) {
      raw_df     <- values$filteredData
      x_var_ply  <- input$vizXVar
      
      # Label axe Y2
      y2_label_ply <- if (!is.null(input$y2AxisLabel) && nchar(trimws(input$y2AxisLabel)) > 0)
                        input$y2AxisLabel
                      else paste(y2_active_plotly, collapse = " / ")
      
      # Limites
      user_y2_min <- if (!is.null(input$y2AxisMin) && !is.na(input$y2AxisMin)) input$y2AxisMin else NULL
      user_y2_max <- if (!is.null(input$y2AxisMax) && !is.na(input$y2AxisMax)) input$y2AxisMax else NULL
      
      # Vraies valeurs Y2 pour la plage de l'axe
      all_y2_real <- unlist(lapply(y2_active_plotly, function(yv)
        if (yv %in% names(raw_df)) raw_df[[yv]] else NULL))
      real_y2_min <- if (!is.null(user_y2_min)) user_y2_min else min(all_y2_real, na.rm = TRUE)
      real_y2_max <- if (!is.null(user_y2_max)) user_y2_max else max(all_y2_real, na.rm = TRUE)
      y2_margin   <- (real_y2_max - real_y2_min) * 0.05
      y2_rng_cfg  <- list(real_y2_min - y2_margin, real_y2_max + y2_margin)
      
      # Lookup: nom → vraies valeurs
      y2_lookup <- lapply(y2_active_plotly, function(yv) {
        if (!yv %in% names(raw_df)) return(NULL)
        d2 <- raw_df[!is.na(raw_df[[x_var_ply]]) & !is.na(raw_df[[yv]]), ]
        list(x = d2[[x_var_ply]], y = d2[[yv]])
      })
      names(y2_lookup) <- y2_active_plotly
      
      # Parcourir les traces et remplacer les y mis-à-l'échelle par les vraies valeurs
      for (i in seq_along(plotly_obj$x$data)) {
        tr       <- plotly_obj$x$data[[i]]
        tr_raw   <- tr$name %||% ""
        # Nettoyer suffixes plotly (ex: "(groupe)" ou "<br>")
        tr_clean <- trimws(gsub("<[^>]*>.*|\\s*\\(.*$", "", tr_raw))
        
        matched <- NULL
        for (yv in y2_active_plotly) {
          if (tr_clean == yv || tr_raw == yv) { matched <- yv; break }
        }
        
        if (!is.null(matched) && !is.null(y2_lookup[[matched]])) {
          rv <- y2_lookup[[matched]]
          plotly_obj$x$data[[i]]$y     <- rv$y
          plotly_obj$x$data[[i]]$yaxis <- "y2"
        }
      }
      
      # ── Contrôles axe Y2 plotly : miroir exact de Y1 ──
      y2_tick_bold    <- isTRUE(input$yTickBold)       # miroir Y1
      y2_tick_italic  <- isTRUE(input$yTickItalic)     # miroir Y1
      y2_tick_sz      <- input$yTickSize  %||% 12      # miroir Y1
      y2_axis_bold    <- isTRUE(input$yAxisBold)       # miroir Y1
      y2_axis_italic  <- isTRUE(input$yAxisItalic)     # miroir Y1
      y2_label_sz_ply <- input$axisLabelSize %||% 12   # miroir Y1
      # Épaisseur ligne : miroir axisLineSize (converti en pixels plotly ×2)
      y2_line_sz_ply  <- (input$axisLineSize %||% 0.8)
      
      # Configurer yaxis2
      y2_cfg <- list(
        title           = list(
          text = y2_label_ply,
          font = list(
            family = make_font_family(y2_axis_bold, y2_axis_italic),
            size   = y2_label_sz_ply,
            color  = "black"
          )
        ),
        linewidth       = max(1, y2_line_sz_ply * 2),   # plotly en pixels
        overlaying      = "y",
        side            = "right",
        showgrid        = FALSE,
        zeroline        = FALSE,
        range           = y2_rng_cfg,
        tickfont        = list(
          size   = y2_tick_sz,
          family = make_font_family(y2_tick_bold, y2_tick_italic),
          color  = "black"
        ),
        showline        = TRUE,
        linecolor       = "black",
        ticks           = "outside",
        ticklen         = 5,
        tickwidth       = 1,
        tickcolor       = "black",
        showticklabels  = TRUE,
        mirror          = FALSE,
        layer           = "above traces"
      )
      
      # Pas personnalisé (lit y2AxisBreakStep ou l'ancien y2AxisStep)
      y2_step_val <- input$y2AxisBreakStep %||% input$y2AxisStep
      if (!is.null(y2_step_val) && !is.na(y2_step_val) && is.numeric(y2_step_val) && y2_step_val > 0) {
        y2_ticks <- seq(real_y2_min, real_y2_max, by = y2_step_val)
        y2_cfg$tickvals <- y2_ticks
        y2_cfg$ticktext <- as.character(round(y2_ticks, 8))
      }
      
      # Marges plotly — marge droite élargie pour que le label Y2 soit visible
      pm_r_ply <- max(60, (input$plotMarginRight  %||% 30) + 40)
      pm_l_ply <- max(20,  input$plotMarginLeft   %||% 10)
      pm_t_ply <- max(20,  input$plotMarginTop    %||% 10)
      pm_b_ply <- max(40,  input$plotMarginBottom %||% 10)
      
      layout_args <- list(
        hovermode = "closest", dragmode = "zoom",
        margin = list(r = pm_r_ply, l = pm_l_ply, t = pm_t_ply, b = pm_b_ply, pad = 4),
        xaxis  = list(tickfont  = list(size = x_tick_size, family = make_font_family(x_tick_bold, x_tick_italic)),
                      titlefont = list(family = make_font_family(x_axis_bold, x_axis_italic))),
        yaxis  = list(tickfont  = list(size = y_tick_size, family = make_font_family(y_tick_bold, y_tick_italic)),
                      titlefont = list(family = make_font_family(y_axis_bold, y_axis_italic))),
        yaxis2 = y2_cfg
      )
    } else {
      pm_r_ply <- max(20,  input$plotMarginRight  %||% 30)
      pm_l_ply <- max(20,  input$plotMarginLeft   %||% 10)
      pm_t_ply <- max(20,  input$plotMarginTop    %||% 10)
      pm_b_ply <- max(40,  input$plotMarginBottom %||% 10)
      
      layout_args <- list(
        hovermode = "closest", dragmode = "zoom",
        margin = list(r = pm_r_ply, l = pm_l_ply, t = pm_t_ply, b = pm_b_ply, pad = 4),
        xaxis = list(tickfont  = list(size = x_tick_size, family = make_font_family(x_tick_bold, x_tick_italic)),
                     titlefont = list(family = make_font_family(x_axis_bold, x_axis_italic))),
        yaxis = list(tickfont  = list(size = y_tick_size, family = make_font_family(y_tick_bold, y_tick_italic)),
                     titlefont = list(family = make_font_family(y_axis_bold, y_axis_italic)))
      )
    }
    
    plotly_obj <- do.call(plotly::layout, c(list(plotly_obj), layout_args)) %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"),
             displaylogo = FALSE)
    
    # Stocker le graphique actuel
    values$currentInteractivePlot <- plotly_obj
    
    return(plotly_obj)
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
    # Tous les types utilisables comme facteur
    fac_cols <- get_all_factor_candidates(values$filteredData)
    tagList(
      pickerInput("factorVar", "Facteur(s):",
                  choices  = fac_cols,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE)),
      tags$small(style = "color:#6c757d; font-size:11px;",
        icon("info-circle"), " Facteur, texte, date et numérique (≤ 30 niveaux) acceptés"),
      actionButton("selectAllFactors",   "Tout sélectionner",   class = "btn-success btn-sm"),
      actionButton("deselectAllFactors", "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllFactors, {
    updatePickerInput(session, "factorVar", selected = get_all_factor_candidates(values$filteredData))
  })
  
  observeEvent(input$deselectAllFactors, {
    updatePickerInput(session, "factorVar", selected = character(0))
  })
  
  # Tests de normalité et homogénéité sur données brutes
  observeEvent(input$testNormalityRaw, {
    req(input$responseVar)
    
    results_list <- list()
    
    # Convertir les facteurs si nécessaire
    df_norm <- values$filteredData
    if (length(input$factorVar) > 0) {
      for (f in input$factorVar) {
        if (!is.null(df_norm[[f]]) && !is.factor(df_norm[[f]])) {
          df_norm[[f]] <- factor(
            if (inherits(df_norm[[f]], c("Date","POSIXct","POSIXlt")))
              format(df_norm[[f]], "%Y-%m-%d")
            else as.character(df_norm[[f]])
          )
          df_norm[[f]] <- droplevels(df_norm[[f]])
        }
      }
    }
    
    for (var in input$responseVar) {
      tryCatch({
        data_values <- df_norm[[var]]
        data_values <- data_values[!is.na(data_values)]
        
        if (length(data_values) >= 3 && length(data_values) <= 5000) {
          norm_test <- shapiro.test(data_values)
          results_list[[var]] <- data.frame(
            Test = "Normalité (données brutes)",
            Variable = var,
            Facteur = "Global",
            Statistique = round(norm_test$statistic, 4),
            ddl = NA,
            p_value = norm_test$p.value,
            Interpretation = interpret_test_results("shapiro", norm_test$p.value),
            stringsAsFactors = FALSE
          )
        } else {
          results_list[[var]] <- data.frame(
            Test = "Normalité (données brutes)",
            Variable = var,
            Facteur = "Global",
            Statistique = NA,
            ddl = NA,
            p_value = NA,
            Interpretation = "Échantillon trop petit/grand pour Shapiro-Wilk",
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "Normalité (données brutes)",
          Variable = var,
          Facteur = "Global",
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
    } else {
      showNotification("Aucun résultat de normalité généré", type = "warning")
    }
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
        data_hom <- values$filteredData
        if (!is.factor(data_hom[[fvar]])) {
          if (inherits(data_hom[[fvar]], c("Date","POSIXct","POSIXlt"))) {
            data_hom[[fvar]] <- factor(format(data_hom[[fvar]], "%Y-%m-%d"))
          } else {
            data_hom[[fvar]] <- factor(as.character(data_hom[[fvar]]))
          }
          data_hom[[fvar]] <- droplevels(data_hom[[fvar]])
        }
        formula_str <- as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        levene_test <- car::leveneTest(formula_str, data = data_hom)
        
        results_list[[var]] <- data.frame(
          Test = "Homogénéité (données brutes)",
          Variable = var,
          Facteur = fvar,
          Statistique = round(levene_test$`F value`[1], 4),
          ddl = paste(levene_test$Df[1], ",", levene_test$Df[2]),
          p_value = levene_test$`Pr(>F)`[1],
          Interpretation = interpret_test_results("levene", levene_test$`Pr(>F)`[1]),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "Homogénéité (données brutes)",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
    } else {
      showNotification("Aucun résultat d'homogénéité généré", type = "warning")
    }
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
        factor_levels <- levels(factor(as.character(values$filteredData[[fvar]])))
        
        if (length(factor_levels) != 2) {
          next
        }
        
        # Tests de validation
        group1_data <- values$filteredData[values$filteredData[[fvar]] == factor_levels[1], var]
        group2_data <- values$filteredData[values$filteredData[[fvar]] == factor_levels[2], var]
        
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
        normality_results[[var]] <- list(
          group1 = normality_group1,
          group2 = normality_group2,
          group1_name = factor_levels[1],
          group2_name = factor_levels[2]
        )
        
        homogeneity_results[[var]] <- homogeneity_test
        
        # Exécuter le t-test et le modèle pour les diagnostics
        formula_str <- as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        test_result <- t.test(formula_str, data = values$filteredData)
        
        # Créer un modèle lm pour les diagnostics
        lm_model <- lm(formula_str, data = values$filteredData)
        model_list[[var]] <- lm_model
        
        # Créer le dataframe de résultats
        results_list[[var]] <- data.frame(
          Test = "t-test",
          Variable = var,
          Facteur = fvar,
          Statistique = round(test_result$statistic, 4),
          ddl = round(test_result$parameter, 2),
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("t.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
        
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "t-test",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- normality_results
      values$homogeneityResults <- homogeneity_results
      values$currentValidationVar <- 1
      values$modelList <- model_list
      values$currentModelVar <- 1
      values$currentTestType <- "parametric"
    } else {
      showNotification("Aucun résultat t-test généré", type = "warning")
    }
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
        formula_str <- as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        test_result <- wilcox.test(formula_str, data = values$filteredData, exact = FALSE)
        
        results_list[[var]] <- data.frame(
          Test = "Wilcoxon",
          Variable = var,
          Facteur = fvar,
          Statistique = round(test_result$statistic, 4),
          ddl = NA,
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("wilcox.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "Wilcoxon",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
    } else {
      showNotification("Aucun résultat Wilcoxon généré", type = "warning")
    }
  })
  
  # Test de Kruskal-Wallis
  observeEvent(input$testKruskal, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Kruskal-Wallis nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    df_kw <- values$filteredData
    fvar_kw <- input$factorVar[1]
    if (!is.factor(df_kw[[fvar_kw]])) {
      df_kw[[fvar_kw]] <- factor(
        if (inherits(df_kw[[fvar_kw]], c("Date","POSIXct","POSIXlt")))
          format(df_kw[[fvar_kw]], "%Y-%m-%d")
        else as.character(df_kw[[fvar_kw]])
      )
      df_kw[[fvar_kw]] <- droplevels(df_kw[[fvar_kw]])
    }
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- fvar_kw
        formula_str <- as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        test_result <- kruskal.test(formula_str, data = df_kw)
        
        results_list[[var]] <- data.frame(
          Test = "Kruskal-Wallis",
          Variable = var,
          Facteur = fvar,
          Statistique = round(test_result$statistic, 4),
          ddl = test_result$parameter,
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("kruskal.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "Kruskal-Wallis",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
    } else {
      showNotification("Aucun résultat Kruskal-Wallis généré", type = "warning")
    }
  })
  
  # Test Scheirer-Ray-Hare
  observeEvent(input$testScheirerRayHare, {
    req(input$responseVar, input$factorVar)
    
    if (length(input$factorVar) < 2) {
      showNotification("Scheirer-Ray-Hare nécessite au moins 2 facteurs", type = "warning")
      return()
    }
    
    results_list <- list()
    error_messages <- c()
    
    for (var in input$responseVar) {
      tryCatch({
        # Vérifier que les données sont valides + convertir les facteurs 
        test_data <- values$filteredData[, c(var, input$factorVar), drop = FALSE]
        for (f in input$factorVar) {
          if (!is.factor(test_data[[f]])) {
            test_data[[f]] <- factor(
              if (inherits(test_data[[f]], c("Date","POSIXct","POSIXlt")))
                format(test_data[[f]], "%Y-%m-%d")
              else as.character(test_data[[f]])
            )
            test_data[[f]] <- droplevels(test_data[[f]])
          }
        }
        test_data <- na.omit(test_data)
        
        if (nrow(test_data) < 3) {
          error_messages <- c(error_messages, paste(var, ": Pas assez de données après suppression des NA"))
          next
        }
        
        # ── Renommer colonnes en noms sûrs 
        safe_resp    <- "resp_var_srh"
        safe_factors <- paste0("factor_srh_", seq_along(input$factorVar))
        # Table de correspondance pour restaurer les noms dans les résultats
        factor_label_map <- setNames(input$factorVar, safe_factors)
        if (length(input$factorVar) == 2) {
          factor_label_map[paste0(safe_factors[1], ":", safe_factors[2])] <-
            paste0(input$factorVar[1], ":", input$factorVar[2])
        }
        
        safe_data <- test_data
        names(safe_data)[names(safe_data) == var] <- safe_resp
        for (fi in seq_along(input$factorVar)) {
          names(safe_data)[names(safe_data) == input$factorVar[fi]] <- safe_factors[fi]
        }
        
        # Préparer la formule avec noms sûrs
        if (isTRUE(input$interaction) && length(input$factorVar) == 2) {
          formula_str <- as.formula(paste0(safe_resp, " ~ ", paste(safe_factors, collapse = "*")))
        } else {
          formula_str <- as.formula(paste0(safe_resp, " ~ ", paste(safe_factors, collapse = "+")))
        }
        
        # Exécuter le test Scheirer-Ray-Hare avec noms sûrs
        test_result <- rcompanion::scheirerRayHare(formula_str, data = safe_data)
        
        # Restaurer les noms originaux dans les rownames du résultat
        orig_rnames <- rownames(test_result)
        for (sf in names(factor_label_map)) {
          orig_rnames <- gsub(sf, factor_label_map[sf], orig_rnames, fixed = TRUE)
        }
        rownames(test_result) <- orig_rnames
        
        # Vérifier si le résultat est valide
        if (is.null(test_result) || nrow(test_result) == 0) {
          error_messages <- c(error_messages, paste(var, ": Test n'a produit aucun résultat"))
          next
        }
        
        # Extraire les résultats pour chaque effet
        effects_found <- 0
        for (i in 1:nrow(test_result)) {
          effect_name <- rownames(test_result)[i]
          if (!is.null(effect_name) && effect_name != "Residuals" && !is.na(effect_name)) {
            results_list[[paste(var, effect_name, sep = "_")]] <- data.frame(
              Test = "Scheirer-Ray-Hare",
              Variable = var,
              Facteur = effect_name,
              Statistique = round(test_result$H[i], 4),
              ddl = test_result$Df[i],
              p_value = test_result$`p.value`[i],
              Interpretation = interpret_test_results("scheirerRayHare", test_result$`p.value`[i]),
              stringsAsFactors = FALSE
            )
            effects_found <- effects_found + 1
          }
        }
        
        if (effects_found == 0) {
          error_messages <- c(error_messages, paste(var, ": Aucun effet trouvé (uniquement des résidus)"))
        }
        
        # Stocker le résultat complet pour les post-hoc
        values$scheirerResults <- test_result
        
      }, error = function(e) {
        error_msg <- paste(var, ":", e$message)
        error_messages <<- c(error_messages, error_msg)
        
        results_list[[var]] <- data.frame(
          Test = "Scheirer-Ray-Hare",
          Variable = var,
          Facteur = paste(input$factorVar, collapse = " + "),
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    # Afficher les messages d'erreur détaillés
    if (length(error_messages) > 0) {
      showNotification(
        paste("Problèmes détectés:\n", paste(error_messages, collapse = "\n")),
        type = "warning",
        duration = 10
      )
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
      
      showNotification(
        paste("Test Scheirer-Ray-Hare terminé:", length(results_list), "résultat(s) généré(s)"),
        type = "message",
        duration = 3
      )
    } else {
      showNotification("Aucun résultat Scheirer-Ray-Hare généré. Vérifiez vos données et facteurs.", type = "error", duration = 10)
    }
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
        if (!is.factor(df[[f]])) df[[f]] <- factor(df[[f]])
      }
      
      for (var in input$responseVar) {
        if (!is.numeric(df[[var]])) df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
        if (all(is.na(df[[var]]))) {
          showNotification(paste0("ANOVA : '", var, "' non numérique — ignorée."), type = "warning", duration = 5)
          next
        }
        df_clean <- df[, c(var, input$factorVar), drop = FALSE]
        df_clean <- df_clean[complete.cases(df_clean), ]
        if (nrow(df_clean) < 4) { showNotification(paste0("ANOVA : trop peu d'obs pour '", var, "'."), type = "warning", duration = 4); next }
        for (f in input$factorVar) {
          # Conversion universelle: tous les types vers facteur
          if (!is.factor(df_clean[[f]])) {
            df_clean[[f]] <- tryCatch(
              factor(as.character(df_clean[[f]])),
              error = function(e) factor(df_clean[[f]])
            )
          }
          df_clean[[f]] <- droplevels(df_clean[[f]])
        }
        formula_str <- paste0("`", var, "` ~ ", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = ifelse(input$interaction, "*", "+")))
        model <- aov(as.formula(formula_str), data = df_clean)
        anova_table <- summary(model)[[1]]
        
        # Stocker le modèle
        model_list[[var]] <- model
        
        # Créer le dataframe de résultats pour chaque effet
        for (i in 1:(nrow(anova_table) - 1)) {
          effect_name <- rownames(anova_table)[i]
          results_list[[paste(var, effect_name, sep = "_")]] <- data.frame(
            Test = "ANOVA",
            Variable = var,
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
          normality_results[[var]] <- shapiro.test(residuals_data)
        }
        
        fitted_data <- fitted(model)
        fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
        test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
        homogeneity_results[[var]] <- car::leveneTest(residuals ~ fitted_group, data = test_data)
      }
      
      if (length(results_list) > 0) {
        values$testResultsDF <- do.call(rbind, results_list)
        values$anovaModel <- model
        values$currentModel <- model
        values$modelList <- model_list
        values$currentModelVar <- 1
        values$normalityResults <- normality_results
        values$homogeneityResults <- homogeneity_results
        values$currentValidationVar <- 1
        values$currentTestType <- "parametric"
      } else {
        showNotification("Aucun résultat ANOVA généré", type = "warning")
      }
      
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
        formula_str <- paste0("`", var, "` ~ ", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+"))
        model <- lm(as.formula(formula_str), data = df)
        summary_model <- summary(model)
        
        # Stocker le modèle
        model_list[[var]] <- model
        
        # Résultat global du modèle
        results_list[[paste(var, "global", sep = "_")]] <- data.frame(
          Test = "Régression linéaire",
          Variable = var,
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
          results_list[[paste(var, rownames(coef_table)[i], sep = "_")]] <- data.frame(
            Test = "Régression linéaire",
            Variable = var,
            Facteur = rownames(coef_table)[i],
            Statistique = round(coef_table[i, "t value"], 4),
            ddl = summary_model$df[2],
            p_value = coef_table[i, "Pr(>|t|)"],
            Interpretation = interpret_test_results("lm", coef_table[i, "Pr(>|t|)"]),
            stringsAsFactors = FALSE
          )
        }
      }
      
      if (length(results_list) > 0) {
        values$testResultsDF <- do.call(rbind, results_list)
        values$currentModel <- model
        values$modelList <- model_list
        values$currentModelVar <- 1
        values$currentTestType <- "parametric"
      } else {
        showNotification("Aucun résultat de régression généré", type = "warning")
      }
      
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
        formula_str <- paste0("`", var, "` ~ ", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+"))
        model <- glm(as.formula(formula_str), data = df, family = gaussian())
        summary_model <- summary(model)
        
        # Stocker le modèle
        model_list[[var]] <- model
        
        # Coefficients
        coef_table <- summary_model$coefficients
        # Détection dynamique des colonnes (t value pour gaussian, z value pour autres familles)
        stat_col  <- if ("z value"   %in% colnames(coef_table)) "z value"   else "t value"
        pval_col  <- if ("Pr(>|z|)"  %in% colnames(coef_table)) "Pr(>|z|)"  else "Pr(>|t|)"
        # Degrés de liberté résiduel et nombre de paramètres
        df_resid   <- model$df.residual
        df_null    <- model$df.null
        n_params   <- nrow(coef_table)
        if (nrow(coef_table) < 1) {
          showNotification(
            paste0("GLM (", var, ") : le modèle est vide."),
            type = "warning"
          )
        } else {
          # Inclure l'intercept (ligne 1) ET tous les coefficients suivants
          for (i in 1:nrow(coef_table)) {
            label <- rownames(coef_table)[i]
            # ddl : résiduel pour l'intercept, df null - df résiduel pour les facteurs
            ddl_i <- if (i == 1) df_resid else (df_null - df_resid)
            results_list[[paste(var, label, sep = "_")]] <- data.frame(
              Test = "GLM",
              Variable = var,
              Facteur = label,
              Statistique = round(coef_table[i, stat_col], 4),
              ddl = ddl_i,
              p_value = coef_table[i, pval_col],
              Interpretation = if (i == 1) paste0("Intercept : ", round(coef_table[i, "Estimate"], 4))
              else interpret_test_results("glm", coef_table[i, pval_col]),
              stringsAsFactors = FALSE
            )
          }
        }
      }
      
      if (length(results_list) > 0) {
        values$testResultsDF <- do.call(rbind, results_list)
        values$currentModel <- model
        values$modelList <- model_list
        values$currentModelVar <- 1
        values$currentTestType <- "parametric"
      } else {
        showNotification("Aucun résultat GLM généré", type = "warning")
      }
      
    }, error = function(e) {
      showNotification(paste("Erreur GLM :", e$message), type = "error")
    })
  })
  
  # Affichage du dataframe des résultats
  output$testResultsDF <- renderDT({
    req(values$testResultsDF)
    
    df <- values$testResultsDF
    
    # Appliquer l'arrondissement si l'option est cochée
    use_round <- !is.null(input$testsRoundResults) && input$testsRoundResults
    if (use_round) {
      dec <- if (!is.null(input$testsDecimals)) input$testsDecimals else 2
      # Arrondir les colonnes numériques
      num_cols <- sapply(df, is.numeric)
      df[, num_cols] <- lapply(df[, num_cols, drop = FALSE], function(x) round(x, dec))
    }
    
    datatable(df, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Contrôle de l'affichage de la validation
  output$showValidation <- reactive({
    !is.null(values$normalityResults) || !is.null(values$homogeneityResults)
  })
  outputOptions(output, "showValidation", suspendWhenHidden = FALSE)
  
  # Contrôle de l'affichage des diagnostics 
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
    norm <- values$normalityResults[[current_var]]
    
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
    norm <- values$normalityResults[[current_var]]
    
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
    hom <- values$homogeneityResults[[current_var]]
    
    if (is.null(hom)) {
      cat("Aucun résultat d'homogénéité disponible pour cette variable.\n")
    } else {
      cat("p = ", hom$`Pr(>F)`[1], "\n")
    }
  })
  
  output$homogeneityInterpretation <- renderUI({
    req(values$homogeneityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    hom <- values$homogeneityResults[[current_var]]
    
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
    
    tryCatch({
      # Vérifier si le modèle a des problèmes de leverage
      model <- values$currentModel
      h <- hatvalues(model)
      
      # Si tous les leverage sont 0 ou très proche de 0
      if (all(h < 1e-10) || sum(h > 0) < 3) {
        par(mfrow = c(1, 1))
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Ajustement parfait détecté\nLes diagnostics graphiques standards ne sont pas disponibles\nVoir les tests numériques ci-dessous", 
             cex = 1.2, col = "red")
        return()
      }
      
      # Essayer les diagnostics standards
      par(mfrow = c(2, 2))
      plot(model, which = 1:4)
      
    }, error = function(e) {
      # En cas d'erreur, afficher un message informatif
      par(mfrow = c(1, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Erreur dans les diagnostics graphiques:\n", 
                       substr(e$message, 1, 100), 
                       "\n\nVoir les tests numériques ci-dessous"), 
           cex = 1, col = "red")
    })
  })
  
  output$modelDiagnosticsInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      model <- values$currentModel
      h <- hatvalues(model)
      
      if (all(h < 1e-10) || sum(h > 0) < 3) {
        interp_text <- "<span style='color: orange;'><strong>⚠ Ajustement parfait ou quasi-parfait détecté.</strong></span><br>
      Le modèle s'ajuste parfaitement aux données (leverage = 0 pour la plupart des observations).
      Cela peut indiquer :<br>
      - Nombre d'observations = nombre de paramètres<br>
      - Données avec structure particulière<br>
      - Surparamétrage du modèle<br>
      Les diagnostics graphiques standards ne sont pas fiables dans ce cas.<br>
      <strong>Recommandation :</strong> Vérifiez les tests numériques ci-dessous et considérez simplifier le modèle."
      } else {
        interp_text <- "Vérifiez les graphiques pour les violations des hypothèses :<br>
      - <strong>Residuals vs Fitted :</strong> Les résidus doivent être répartis aléatoirement autour de 0<br>
      - <strong>Normal Q-Q :</strong> Les points doivent suivre la ligne diagonale<br>
      - <strong>Scale-Location :</strong> La ligne rouge doit être approximativement horizontale<br>
      - <strong>Residuals vs Leverage :</strong> Identifie les points influents"
      }
      
      HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
      
    }, error = function(e) {
      HTML("<div class='interpretation-box'><span style='color: red;'>Erreur dans l'interprétation des diagnostics</span></div>")
    })
  })
  
  # Téléchargement des diagnostics de modèles
  output$downloadModelDiagnostics <- downloadHandler(
    filename = function() {
      paste0("diagnostics_modele_", Sys.Date(), ".png")
    },
    content = function(file) {
      tryCatch({
        model <- values$currentModel
        h <- hatvalues(model)
        
        png(file, width = 3200, height = 2400, res = 300, type = "cairo")
        
        if (all(h < 1e-10) || sum(h > 0) < 3) {
          # Afficher un message si ajustement parfait
          par(mfrow = c(1, 1))
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Ajustement parfait détecté\nLes diagnostics graphiques ne sont pas disponibles", 
               cex = 1.5, col = "red")
        } else {
          # Diagnostics standards
          par(mfrow = c(2, 2))
          plot(model, which = 1:4)
        }
        
        dev.off()
      }, error = function(e) {
        png(file, width = 3200, height = 2400, res = 300, type = "cairo")
        par(mfrow = c(1, 1))
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Erreur:", substr(e$message, 1, 50)), cex = 1.2, col = "red")
        dev.off()
      })
    }
  )
  
  # Téléchargement du QQ-plot
  output$downloadQQPlot <- downloadHandler(
    filename = function() {
      paste0("qqplot_residus_", Sys.Date(), ".png")
    },
    content = function(file) {
      tryCatch({
        req(values$currentModel)
        residuals_data <- residuals(values$currentModel)
        residuals_data <- residuals_data[!is.na(residuals_data)]
        
        if (length(residuals_data) < 3 || sd(residuals_data) < 1e-10) {
          png(file, width = 2000, height = 1600, res = 300, type = "cairo-png")
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "QQ-plot non disponible\n(résidus constants ou insuffisants)", 
               cex = 1.2, col = "orange")
          dev.off()
          return()
        }
        
        # Calculer les quantiles théoriques et observés
        n <- length(residuals_data)
        theoretical_quantiles <- qnorm(ppoints(n))
        sample_quantiles <- sort(residuals_data)
        
        # Calculer les bandes de confiance (approximation)
        se <- (sd(residuals_data) / sqrt(n)) * sqrt(theoretical_quantiles^2 + 1)
        upper_band <- theoretical_quantiles + 1.96 * se
        lower_band <- theoretical_quantiles - 1.96 * se
        
        df <- data.frame(
          theoretical = theoretical_quantiles,
          sample = sample_quantiles,
          upper = upper_band,
          lower = lower_band
        )
        
        p <- ggplot(df, aes(x = theoretical, y = sample)) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
          geom_point(shape = 1, size = 2) +
          theme_minimal() +
          labs(title = "QQ-plot des résidus", 
               x = "Quantiles théoriques", 
               y = "Quantiles observés") +
          theme(plot.title = element_markdown(hjust = 0.5))
        
        ggsave(file, plot = p, width = 10, height = 8, dpi = 300, type = "cairo-png")
        
      }, error = function(e) {
        png(file, width = 2000, height = 1600, res = 300, type = "cairo-png")
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Erreur:", substr(e$message, 1, 50)), cex = 1, col = "red")
        dev.off()
      })
    }
  )
  
  # QQ-plot des résidus
  output$qqPlotResiduals <- renderPlot({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Pas assez de résidus pour le QQ-plot (n < 3)", cex = 1.2, col = "red")
        return()
      }
      
      if (sd(residuals_data) < 1e-10) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Résidus constants (ajustement parfait)\nQQ-plot non applicable", cex = 1.2, col = "orange")
        return()
      }
      
      # Calculer les quantiles théoriques et observés
      n <- length(residuals_data)
      theoretical_quantiles <- qnorm(ppoints(n))
      sample_quantiles <- sort(residuals_data)
      
      # Calculer les bandes de confiance (approximation)
      se <- (sd(residuals_data) / sqrt(n)) * sqrt(theoretical_quantiles^2 + 1)
      upper_band <- theoretical_quantiles + 1.96 * se
      lower_band <- theoretical_quantiles - 1.96 * se
      
      df <- data.frame(
        theoretical = theoretical_quantiles,
        sample = sample_quantiles,
        upper = upper_band,
        lower = lower_band
      )
      
      ggplot(df, aes(x = theoretical, y = sample)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        geom_point(shape = 1, size = 2) +
        theme_minimal() +
        labs(title = "QQ-plot des résidus", 
             x = "Quantiles théoriques", 
             y = "Quantiles observés") +
        theme(plot.title = element_markdown(hjust = 0.5))
      
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Erreur QQ-plot:", substr(e$message, 1, 50)), cex = 1, col = "red")
    })
  })
  
  output$qqPlotInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        interp_text <- "<span style='color: red;'>Pas assez de résidus pour évaluer la normalité.</span>"
      } else if (sd(residuals_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Résidus constants (ajustement parfait). Normalité non évaluable.</span>"
      } else {
        interp_text <- "Les points devraient suivre la ligne droite pour une normalité des résidus.<br>
      <strong>Déviations acceptables :</strong> Légères aux extrémités<br>
      <strong>Problèmes :</strong> Courbure prononcée, points très éloignés de la ligne"
      }
      
      HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
      
    }, error = function(e) {
      HTML("<div class='interpretation-box'><span style='color: red;'>Erreur dans l'interprétation du QQ-plot</span></div>")
    })
  })
  
  # Normalité des résidus
  output$normalityResult <- renderPrint({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        cat("Nombre d'observations insuffisant pour le test de Shapiro-Wilk (n < 3).\n")
      } else if (length(residuals_data) > 5000) {
        cat("Trop d'observations pour le test de Shapiro-Wilk (n > 5000).\n")
        cat("Utilisez le QQ-plot ci-dessus pour évaluer visuellement la normalité.\n")
      } else if (sd(residuals_data) < 1e-10) {
        cat("Résidus constants ou quasi-constants (ajustement parfait).\n")
        cat("Le test de normalité n'est pas applicable.\n")
      } else {
        shapiro.test(residuals_data)
      }
    }, error = function(e) {
      cat("Erreur dans le test de normalité:", e$message, "\n")
    })
  })
  
  output$normalityResidInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        interp_text <- "Nombre d'observations insuffisant pour le test de Shapiro-Wilk."
      } else if (length(residuals_data) > 5000) {
        interp_text <- "Trop d'observations pour Shapiro-Wilk. Référez-vous au QQ-plot."
      } else if (sd(residuals_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Résidus constants (ajustement parfait). Test non applicable.</span>"
      } else {
        norm_test <- shapiro.test(residuals_data)
        interp_text <- interpret_normality_resid(norm_test$p.value)
      }
      
      HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
    }, error = function(e) {
      HTML("<div class='interpretation-box'><span style='color: red;'>Erreur dans le test de normalité</span></div>")
    })
  })
  
  # Homogénéité des résidus
  output$leveneResidResult <- renderPrint({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      fitted_data <- fitted(values$currentModel)
      
      # Vérifier s'il y a une variation dans les valeurs ajustées
      if (sd(fitted_data) < 1e-10) {
        cat("Les valeurs ajustées sont constantes (ajustement parfait).\n")
        cat("Le test d'homogénéité des résidus n'est pas applicable.\n")
        return()
      }
      
      # Vérifier qu'on a assez de valeurs uniques pour cut()
      n_unique <- length(unique(fitted_data))
      if (n_unique < 2) {
        cat("Pas assez de valeurs uniques dans les prédictions.\n")
        cat("Le test d'homogénéité n'est pas applicable.\n")
        return()
      }
      
      # Créer les groupes
      fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
      
      # Vérifier que les deux groupes existent
      if (length(levels(fitted_factor)) < 2 || any(table(fitted_factor) < 2)) {
        cat("Impossible de créer deux groupes équilibrés.\n")
        cat("Le test d'homogénéité n'est pas applicable.\n")
        return()
      }
      
      test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
      car::leveneTest(residuals ~ fitted_group, data = test_data)
      
    }, error = function(e) {
      cat("Erreur dans le test d'homogénéité:", e$message, "\n")
    })
  })
  
  output$homogeneityResidInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      fitted_data <- fitted(values$currentModel)
      
      # Vérifier les mêmes conditions que ci-dessus
      if (sd(fitted_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Valeurs ajustées constantes (ajustement parfait). Test non applicable.</span>"
        return(HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>")))
      }
      
      n_unique <- length(unique(fitted_data))
      if (n_unique < 2) {
        interp_text <- "<span style='color: orange;'>Pas assez de variation dans les prédictions. Test non applicable.</span>"
        return(HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>")))
      }
      
      fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
      
      if (length(levels(fitted_factor)) < 2 || any(table(fitted_factor) < 2)) {
        interp_text <- "<span style='color: orange;'>Impossible de créer deux groupes équilibrés. Test non applicable.</span>"
        return(HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>")))
      }
      
      test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
      hom_test <- car::leveneTest(residuals ~ fitted_group, data = test_data)
      interp_text <- interpret_homogeneity_resid(hom_test$`Pr(>F)`[1])
      
      HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
      
    }, error = function(e) {
      HTML("<div class='interpretation-box'><span style='color: red;'>Erreur dans le test d'homogénéité</span></div>")
    })
  })
  
  # Autocorrélation
  output$autocorrResult <- renderPrint({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      
      if (length(residuals_data) < 3) {
        cat("Nombre d'observations insuffisant pour le test de Durbin-Watson (n < 3).\n")
        return()
      }
      
      if (sd(residuals_data) < 1e-10) {
        cat("Résidus constants (ajustement parfait).\n")
        cat("Le test d'autocorrélation n'est pas applicable.\n")
        return()
      }
      
      lmtest::dwtest(values$currentModel)
      
    }, error = function(e) {
      cat("Erreur dans le test de Durbin-Watson:", e$message, "\n")
    })
  })
  
  output$autocorrInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      
      if (length(residuals_data) < 3) {
        interp_text <- "Nombre d'observations insuffisant pour le test de Durbin-Watson."
        return(HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>")))
      }
      
      if (sd(residuals_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Résidus constants (ajustement parfait). Test non applicable.</span>"
        return(HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>")))
      }
      
      dw_test <- lmtest::dwtest(values$currentModel)
      interp_text <- if (dw_test$p.value > 0.05) {
        "Pas d'autocorrélation significative des résidus (p > 0.05)."
      } else {
        "Autocorrélation significative des résidus (p < 0.05). Vérifiez l'indépendance des observations."
      }
      
      HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
      
    }, error = function(e) {
      HTML("<div class='interpretation-box'><span style='color: red;'>Erreur dans le test d'autocorrélation</span></div>")
    })
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
  
  # Post-hoc des effets simples 
  perform_simple_effect_posthoc <- function(df, var, factor1, factor2, level, test_type, test_method) {
    
    # Filtrer les données pour ce niveau spécifique (en excluant les NA)
    df_subset <- df[!is.na(df[[factor2]]) & as.character(df[[factor2]]) == as.character(level), ]
    
    if (is.null(df_subset) || nrow(df_subset) < 3) return(NULL)
    
    # Nettoyage variable réponse
    if (!is.numeric(df_subset[[var]])) {
      df_subset[[var]] <- suppressWarnings(as.numeric(df_subset[[var]]))
    }
    df_subset <- df_subset[!is.na(df_subset[[var]]), ]
    if (nrow(df_subset) < 3) return(NULL)
    
    # Conversion universelle du facteur
    df_subset[[factor1]] <- tryCatch(
      factor(as.character(df_subset[[factor1]])),
      error = function(e) factor(df_subset[[factor1]])
    )
    df_subset[[factor1]] <- droplevels(df_subset[[factor1]])
    
    # Guard : au moins 2 niveaux pour effectuer des comparaisons
    if (nlevels(df_subset[[factor1]]) < 2) return(NULL)
    
    tryCatch({
      groups <- NULL
      
      if (test_type == "param") {
        bt <- function(x) paste0("`", x, "`")
        model <- aov(as.formula(paste0(bt(var), " ~ ", bt(factor1))), data = df_subset)
        
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
          emm <- emmeans::emmeans(model, as.formula(paste0("~ `", factor1, "`")))
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
          emm <- emmeans::emmeans(model, as.formula(paste0("~ `", factor1, "`")))
          groups_cld <- multcomp::cld(emm, Letters = letters)
          groups <- as.data.frame(groups_cld)
          groups <- groups[, c(factor1, ".group")]
          colnames(groups) <- c(factor1, "groups")
          groups$groups <- trimws(groups$groups)
        } else if (test_method == "games") {
          bt_loc <- function(x) paste0("`", x, "`")
          mc <- PMCMRplus::gamesHowellTest(as.formula(paste0(bt_loc(var), " ~ ", bt_loc(factor1))), data = df_subset)
          pmat <- as.matrix(mc$p.value)
          pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
          diag(pmat) <- 1
          groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
          groups <- data.frame(groups = groups_letters)
          groups[[factor1]] <- names(groups_letters)
        }
      } else {  # NON-PARAMÉTRIQUE 
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
  
  # Fonction principale d'analyse 
  
    observeEvent(values$testResultsDF, {
    req(values$testResultsDF)
    
    # Synchroniser le type de test (paramétrique / non-param)
    if (!is.null(values$currentTestType)) {
      new_type <- if (values$currentTestType == "parametric") "param" else "nonparam"
      updateRadioButtons(session, "testType", selected = new_type)
    }
    
    # Forcer le re-render des sélecteurs
    values$postHocSyncTrigger <- runif(1)
    
    # Notification
    showNotification(
      tagList(
        icon("link"), " PostHoc mis à jour avec les résultats des tests (",
        nrow(values$testResultsDF), " résultats)"
      ),
      type = "message", duration = 3
    )
  }, ignoreInit = TRUE)
  
  # ── Tableau récapitulatif des p-values pour guider le PostHoc
  output$testResultsSummaryForPostHoc <- renderUI({
    if (is.null(values$testResultsDF) || nrow(values$testResultsDF) == 0) {
      return(div(
        style = "padding:10px; background:#fff8e1; border-radius:4px; border-left:3px solid #ff9800; font-size:12px;",
        icon("exclamation-triangle", style="color:#e65100;"),
        " Aucun résultat de test disponible.",
        tags$br(),
        tags$small(style="color:#bf360c;",
          "Allez dans 'Réalisation des tests statistiques' et lancez un test avant de faire le PostHoc.")
      ))
    }
    
    df <- values$testResultsDF
    df$p_value <- as.numeric(df$p_value)
    
    # Créer des lignes colorées selon significativité
    rows <- lapply(seq_len(nrow(df)), function(i) {
      row   <- df[i, ]
      p_val <- row$p_value
      sig   <- if (!is.na(p_val)) {
        if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else "ns"
      } else "?"
      
      bg_col <- if (!is.na(p_val) && p_val < 0.05) "#e8f5e9" else "#fafafa"
      p_col  <- if (!is.na(p_val) && p_val < 0.05) "#2e7d32" else "#666"
      
      tags$tr(
        style = paste0("background:", bg_col, "; border-bottom:1px solid #e0e0e0;"),
        tags$td(style = "padding:4px 8px; font-size:11.5px; font-weight:600;", row$Variable),
        tags$td(style = "padding:4px 8px; font-size:11.5px;", row$Facteur %||% "-"),
        tags$td(style = "padding:4px 8px; font-size:11.5px;", row$Test),
        tags$td(style = paste0("padding:4px 8px; font-size:12px; font-weight:bold; color:", p_col, ";"),
          if (!is.na(p_val)) formatC(p_val, format="e", digits=3) else "NA"),
        tags$td(style = paste0("padding:4px 8px; font-size:13px; font-weight:bold; color:", p_col, ";"), sig)
      )
    })
    
    n_sig <- sum(!is.na(df$p_value) & df$p_value < 0.05)
    
    tagList(
      div(
        style = "margin-bottom:8px; padding:8px 10px; background:#e3f2fd; border-left:4px solid #1976d2; border-radius:4px;",
        icon("table", style="color:#1565c0;"),
        tags$b(style="color:#0d47a1; font-size:12px;",
          paste0(" Résultats des tests — ", n_sig, "/", nrow(df), " significatif(s)")),
        tags$br(),
        tags$small(style="color:#1976d2;",
          "Les variables en vert (p < 0.05) sont pré-sélectionnées dans l'analyse PostHoc.")
      ),
      div(
        style = "max-height:200px; overflow-y:auto; border:1px solid #e0e0e0; border-radius:4px;",
        tags$table(
          style = "width:100%; border-collapse:collapse;",
          tags$thead(
            tags$tr(
              style = "background:#1976d2; color:white;",
              tags$th(style="padding:5px 8px; font-size:11px;", "Variable"),
              tags$th(style="padding:5px 8px; font-size:11px;", "Facteur"),
              tags$th(style="padding:5px 8px; font-size:11px;", "Test"),
              tags$th(style="padding:5px 8px; font-size:11px;", "p-value"),
              tags$th(style="padding:5px 8px; font-size:11px;", "Sig.")
            )
          ),
          tags$tbody(rows)
        )
      )
    )
  })
  
  observeEvent(input$runMultiple, {
    req(input$multiResponse, input$multiFactor)
    
    showNotification("Analyse en cours...", type = "message", duration = NULL, id = "loading")
    
    multi_results_list <- list()
    simple_effects_list <- list()
    df <- values$filteredData
    
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Aucune donnée disponible pour l'analyse.", type="error", duration=5)
      removeNotification("loading")
      return()
    }
    # Assurer que les facteurs sont bien des factors avec levels valides
    for (fv in input$multiFactor) {
      if (!is.null(df[[fv]])) {
        # Conversion universelle: factor, character, date, numeric → factor
        if (!is.factor(df[[fv]])) {
          df[[fv]] <- tryCatch(
            factor(as.character(df[[fv]])),
            error = function(e) factor(df[[fv]])
          )
        }
        df[[fv]] <- droplevels(df[[fv]])
      }
    }
    
    for (var in input$multiResponse) {
      
      # EFFETS PRINCIPAUX 
      for (fvar in input$multiFactor) {
        tryCatch({
          # Guard: variable réponse doit être numérique
          if (!is.numeric(df[[var]])) {
            df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
            if (all(is.na(df[[var]]))) {
              showNotification(paste0("Variable '", var, "' non convertible en numérique."), type="warning", duration=4)
              next
            }
          }
          # Supprimer les NA dans la variable réponse
          df_var <- df[!is.na(df[[var]]), ]
          if (!is.factor(df_var[[fvar]])) df_var[[fvar]] <- factor(as.character(df_var[[fvar]]))
          df_var[[fvar]] <- droplevels(df_var[[fvar]])
          if (nlevels(df_var[[fvar]]) < 2) {
            showNotification(paste0("PostHoc '", fvar, "': moins de 2 niveaux après nettoyage."), type="warning", duration=4)
            next
          }
          if (nrow(df_var) < 4) { next }
          if (input$testType == "param") {
            bt <- function(x) paste0("`", x, "`")
            model <- aov(as.formula(paste0(bt(var), " ~ ", bt(fvar))), data = df_var)
            
            if (input$multiTest %in% c("lsd", "tukey", "duncan", "snk", "scheffe", "regw", "waller")) {
              mc_func <- switch(input$multiTest,
                                "lsd" = agricolae::LSD.test,
                                "tukey" = agricolae::HSD.test,
                                "duncan" = agricolae::duncan.test,
                                "snk" = agricolae::SNK.test,
                                "scheffe" = agricolae::scheffe.test,
                                "regw" = agricolae::REGW.test,
                                "waller" = agricolae::waller.test)
              if (length(levels(df[[fvar]])) < 2) {
                showNotification(paste0("PostHoc '", fvar, "': moins de 2 niveaux, test ignoré."), type="warning", duration=4)
                next
              }
              mc <- tryCatch(mc_func(model, fvar, group = TRUE), error = function(e) NULL)
              if (is.null(mc)) { next }
              groups <- mc$groups
              colnames(groups)[1:2] <- c("means", "groups")
              groups[[fvar]] <- rownames(groups)
            } else if (input$multiTest == "bonferroni") {
              emm <- emmeans::emmeans(model, as.formula(paste0("~ `", fvar, "`")))
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
              emm <- emmeans::emmeans(model, as.formula(paste0("~ `", fvar, "`")))
              groups_cld <- multcomp::cld(emm, Letters = letters)
              groups <- as.data.frame(groups_cld)
              groups <- groups[, c(fvar, ".group")]
              colnames(groups) <- c(fvar, "groups")
              groups$groups <- trimws(groups$groups)
            } else if (input$multiTest == "games") {
              mc <- PMCMRplus::gamesHowellTest(as.formula(paste0("`", var, "` ~ `", fvar, "`")), data = df)
              pmat <- as.matrix(mc$p.value)
              pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
              diag(pmat) <- 1
              groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
              groups <- data.frame(groups = groups_letters)
              groups[[fvar]] <- names(groups_letters)
            }
          } else {  # NON-PARAMÉTRIQUE
            df_var <- df[!is.na(df[[var]]), ]
            if (!is.numeric(df_var[[var]])) df_var[[var]] <- suppressWarnings(as.numeric(df_var[[var]]))
            if (!is.factor(df_var[[fvar]])) df_var[[fvar]] <- factor(as.character(df_var[[fvar]]))
            df_var[[fvar]] <- droplevels(df_var[[fvar]])
            if (nrow(df_var) < 4) { next }
            if (input$multiTestNonParam == "kruskal") {
              mc <- agricolae::kruskal(df_var[[var]], df_var[[fvar]], group = TRUE)
              groups <- mc$groups
              colnames(groups) <- c("means", "groups")
              groups[[fvar]] <- rownames(groups)
            } else if (input$multiTestNonParam == "dunn") {
              tryCatch({
                mc <- PMCMRplus::dunnTest(df_var[[var]], df_var[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }, error = function(e) {
                mc <- agricolae::kruskal(df_var[[var]], df_var[[fvar]], group = TRUE)
                groups <<- mc$groups
                colnames(groups)[1:2] <- c("means", "groups")
                groups[[fvar]] <<- rownames(groups)
              })
            } else if (input$multiTestNonParam == "conover") {
              tryCatch({
                mc <- PMCMRplus::kwAllPairsConoverTest(df_var[[var]], df_var[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }, error = function(e) {
                mc <- agricolae::kruskal(df_var[[var]], df_var[[fvar]], group = TRUE)
                groups <<- mc$groups
                colnames(groups)[1:2] <- c("means", "groups")
                groups[[fvar]] <<- rownames(groups)
              })
            } else if (input$multiTestNonParam == "nemenyi") {
              tryCatch({
                mc <- PMCMRplus::kwAllPairsNemenyiTest(df_var[[var]], df_var[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }, error = function(e) {
                mc <- agricolae::kruskal(df_var[[var]], df_var[[fvar]], group = TRUE)
                groups <<- mc$groups
                colnames(groups)[1:2] <- c("means", "groups")
                groups[[fvar]] <<- rownames(groups)
              })
            }
          }
          
          # Statistiques descriptives
          # Utiliser df_var (sans NA dans var) pour les stats descriptives
          df_desc <- if (exists("df_var") && !is.null(df_var)) df_var else df
          desc <- df_desc %>%
            group_by(across(all_of(fvar))) %>%
            summarise(
              Moyenne    = mean(.data[[var]], na.rm = TRUE),
              Ecart_type = sd(.data[[var]], na.rm = TRUE),
              N          = n(),
              Erreur_type = ifelse(N > 1, Ecart_type / sqrt(N), NA_real_),
              CV         = calc_cv(.data[[var]]),
              .groups    = "drop"
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
      
      # ANALYSE DES INTERACTIONS ET EFFETS SIMPLES BIDIRECTIONNELS 
      if (input$posthocInteraction && length(input$multiFactor) > 1) {
        factor_combinations <- combn(input$multiFactor, 2, simplify = FALSE)
        
        for (fcomb in factor_combinations) {
          fvar1 <- fcomb[1]
          fvar2 <- fcomb[2]
          interaction_term <- paste(fvar1, fvar2, sep = ":")
          formula_str <- paste(var, "~", fvar1, "*", fvar2)
          
          tryCatch({
            # Nettoyage NA + facteurs avant interaction
            cols_inter <- c(var, fvar1, fvar2)
            df_temp <- df[, intersect(cols_inter, names(df)), drop = FALSE]
            df_temp <- df_temp[complete.cases(df_temp), ]
            for (.f in c(fvar1, fvar2)) {
              if (!is.factor(df_temp[[.f]])) df_temp[[.f]] <- factor(df_temp[[.f]])
              df_temp[[.f]] <- droplevels(df_temp[[.f]])
            }
            if (!is.numeric(df_temp[[var]])) df_temp[[var]] <- suppressWarnings(as.numeric(df_temp[[var]]))
            if (nrow(df_temp) < 4 || all(is.na(df_temp[[var]]))) {
              showNotification(paste0("Interaction ", fvar1, ":", fvar2, " — données insuffisantes."), type="warning", duration=4)
              return(NULL)
            }
            interaction_pvalue <- NA
            
            # TEST D'INTERACTION
            if (input$testType == "param") {
              formula_inter <- paste0("`", var, "` ~ `", fvar1, "` * `", fvar2, "`")
              model <- tryCatch(aov(as.formula(formula_inter), data = df_temp), error = function(e) NULL)
              if (is.null(model)) return(NULL)
              anova_res <- tryCatch(summary(model)[[1]], error = function(e) NULL)
              if (!is.null(anova_res)) {
                interaction_row <- paste(fvar1, fvar2, sep = ":")
                if (interaction_row %in% rownames(anova_res)) {
                  interaction_pvalue <- anova_res[interaction_row, "Pr(>F)"]
                }
              }
            } else { 
              
              
              # TEST NON-PARAMÉTRIQUE 
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
            
            # SI INTERACTION SIGNIFICATIVE : DÉCOMPOSITION BIDIRECTIONNELLE 
            if (!is.na(interaction_pvalue) && interaction_pvalue < 0.05) {
              showNotification(
                paste0("✓ Interaction significative détectée: ", fvar1, " × ", fvar2, 
                       " (p = ", round(interaction_pvalue, 4), ")\n",
                       "↳ Décomposition bidirectionnelle en cours..."),
                type = "warning", duration = 5
              )
              
              # ── Effets simples : utiliser df_temp (données nettoyées sans NA) ──
              
              test_m <- ifelse(input$testType == "param", input$multiTest, input$multiTestNonParam)
              
              # Comparer fvar1 à chaque niveau de fvar2 (niveaux de df_temp, pas df brut)
              levels_fvar2 <- levels(df_temp[[fvar2]])
              if (is.null(levels_fvar2) || length(levels_fvar2) == 0)
                levels_fvar2 <- unique(as.character(df_temp[[fvar2]]))
              levels_fvar2 <- levels_fvar2[!is.na(levels_fvar2)]
              
              for (level2 in levels_fvar2) {
                res <- tryCatch(
                  perform_simple_effect_posthoc(df_temp, var, fvar1, fvar2, level2,
                                                input$testType, test_m),
                  error = function(e) NULL
                )
                if (!is.null(res) && nrow(res) > 0) {
                  res <- tryCatch(res %>%
                    mutate(
                      Moyenne = round(as.numeric(Moyenne), 2),
                      Ecart_type = round(as.numeric(Ecart_type), 2),
                      Erreur_type = round(as.numeric(Erreur_type), 2),
                      CV = round(as.numeric(CV), 2),
                      `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                      `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                      Variable = var,
                      Facteur = paste0(fvar1, " | ", fvar2, "=", level2),
                      Type = "simple_effect",
                      Interaction_base = interaction_term,
                      P_interaction = round(interaction_pvalue, 4),
                      Direction = paste0(fvar1, " vers ", fvar2)
                    ), error = function(e) NULL)
                  if (!is.null(res))
                    simple_effects_list[[paste(var, fvar1, fvar2, level2, sep = "_")]] <- res
                }
              }
              
              # Comparer fvar2 à chaque niveau de fvar1 
              levels_fvar1 <- levels(df_temp[[fvar1]])
              if (is.null(levels_fvar1) || length(levels_fvar1) == 0)
                levels_fvar1 <- unique(as.character(df_temp[[fvar1]]))
              levels_fvar1 <- levels_fvar1[!is.na(levels_fvar1)]
              
              for (level1 in levels_fvar1) {
                res <- tryCatch(
                  perform_simple_effect_posthoc(df_temp, var, fvar2, fvar1, level1,
                                                input$testType, test_m),
                  error = function(e) NULL
                )
                if (!is.null(res) && nrow(res) > 0) {
                  res <- tryCatch(res %>%
                    mutate(
                      Moyenne = round(as.numeric(Moyenne), 2),
                      Ecart_type = round(as.numeric(Ecart_type), 2),
                      Erreur_type = round(as.numeric(Erreur_type), 2),
                      CV = round(as.numeric(CV), 2),
                      `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                      `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                      Variable = var,
                      Facteur = paste0(fvar2, " | ", fvar1, "=", level1),
                      Type = "simple_effect",
                      Interaction_base = interaction_term,
                      P_interaction = round(interaction_pvalue, 4),
                      Direction = paste0(fvar2, " vers ", fvar1)
                    ), error = function(e) NULL)
                  if (!is.null(res))
                    simple_effects_list[[paste(var, fvar2, fvar1, level1, sep = "_")]] <- res
                }
              }
              
              showNotification(
                paste0("✓ Décomposition complétée pour ", fvar1, " × ", fvar2),
                type = "message", duration = 3
              )
            } else if (!is.na(interaction_pvalue)) {
              showNotification(
                paste0("○ Interaction non significative: ", fvar1, " × ", fvar2, 
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
    
    # ASSEMBLAGE FINAL DES RÉSULTATS 
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
          "<b>✓ ANALYSE TERMINÉE</b><br/>",
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
  
  # Multi-Selection 
  output$multiResponseSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    
    # Pré-sélectionner depuis les résultats de tests
    pre_selected <- if (!is.null(values$testResultsDF) && "Variable" %in% names(values$testResultsDF)) {
      sig_vars <- unique(values$testResultsDF$Variable[
        !is.na(values$testResultsDF$p_value) & values$testResultsDF$p_value < 0.05
      ])
      if (length(sig_vars) == 0) unique(values$testResultsDF$Variable) else sig_vars
    } else { character(0) }
    pre_selected <- intersect(pre_selected, num_cols)
    
    tagList(
      pickerInput("multiResponse", "Variable(s) réponse:",
                  choices  = num_cols,
                  selected = if (length(pre_selected) > 0) pre_selected else NULL,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      div(style = "display: flex; gap: 10px;",
          actionButton("selectAllMultiResponse", "Tout sélectionner",
                       class = "btn-success btn-sm", style = "flex: 1; height: 40px;"),
          actionButton("deselectAllMultiResponse", "Tout désélectionner",
                       class = "btn-danger btn-sm", style = "flex: 1; height: 40px;")
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
    fac_cols <- get_all_factor_candidates(values$filteredData)
    
    # Pré-sélectionner les facteurs des résultats de tests si disponibles
    pre_fac <- if (!is.null(values$testResultsDF) && "Facteur" %in% names(values$testResultsDF)) {
      unique(values$testResultsDF$Facteur[!is.na(values$testResultsDF$Facteur)])
    } else { character(0) }
    pre_fac <- intersect(pre_fac, fac_cols)
    
    tagList(
      pickerInput("multiFactor", "Facteur(s):",
                  choices  = fac_cols,
                  selected = if (length(pre_fac) > 0) pre_fac else NULL,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      tags$small(style = "color:#6c757d; font-size:11px;",
        icon("info-circle"), " Facteur, texte, date et numérique (≤ 30 niveaux) acceptés"),
      div(style = "display: flex; gap: 10px;",
          actionButton("selectAllMultiFactors",   "Tout sélectionner",
                       class = "btn-success btn-sm", style = "flex: 1; height: 40px;"),
          actionButton("deselectAllMultiFactors", "Tout désélectionner",
                       class = "btn-danger btn-sm",  style = "flex: 1; height: 40px;")
      )
    )
  })
  
  observeEvent(input$selectAllMultiFactors, {
    updatePickerInput(session, "multiFactor", selected = get_all_factor_candidates(values$filteredData))
  })
  
  observeEvent(input$deselectAllMultiFactors, {
    updatePickerInput(session, "multiFactor", selected = character(0))
  })
  
  # Tables de résultats 
  output$mainEffectsTable <- renderDT({
    req(values$multiResultsMain)
    
    main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
    
    if (nrow(main_data) == 0) return(NULL)
    
    cols_to_show <- c("Variable", "Facteur", "Moyenne", "Ecart_type", "Erreur_type", "CV", "groups", "N", "Moyenne±Ecart_type", "Moyenne±Erreur_type")
    
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
                      "Moyenne", "Ecart_type", "Erreur_type", "CV", "groups", "N", "Moyenne±Ecart_type", "Moyenne±Erreur_type")
    
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
  
  # Indicateurs 
  output$showPosthocResults <- reactive({
    !is.null(values$multiResultsMain) && nrow(values$multiResultsMain) > 0
  })
  outputOptions(output, "showPosthocResults", suspendWhenHidden = FALSE)
  
  output$showSimpleEffects <- reactive({
    !is.null(values$multiResultsMain) && 
      any(values$multiResultsMain$Type == "simple_effect", na.rm = TRUE)
  })
  outputOptions(output, "showSimpleEffects", suspendWhenHidden = FALSE)
  
  # Résumés 
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
  
  # Filtres dynamiques 
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
  
  # Sélection pour graphiques effets simples 
  output$selectSimpleEffectPlot <- renderUI({
    req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
    }
    
    if (is.null(input$multiResponse) || length(input$multiResponse) == 0) {
      return(div(style = "color: #e74c3c; font-style: italic;", 
                 "Aucune variable sélectionnée"))
    }
    
    current_var_idx <- values$currentVarIndex
    max_idx <- length(input$multiResponse)
    
    if (current_var_idx < 1 || current_var_idx > max_idx) {
      current_var_idx <- 1
      values$currentVarIndex <- 1
    }
    
    resp_var <- tryCatch({
      input$multiResponse[[current_var_idx]]
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(resp_var) || is.na(resp_var) || resp_var == "") {
      return(div(style = "color: #e74c3c; font-style: italic;", 
                 "Erreur d'accès à la variable"))
    }
    
    simple_var_data <- simple_data[simple_data$Variable == resp_var, ]
    
    if (nrow(simple_var_data) == 0) {
      return(div(style = "color: #e74c3c; font-style: italic;", 
                 paste("Aucun effet simple pour", resp_var)))
    }
    
    factors <- unique(simple_var_data$Facteur)
    
    if (length(factors) == 0) {
      return(div(style = "color: #e74c3c; font-style: italic;", 
                 "Aucun facteur disponible"))
    }
    
    selectInput("selectedSimpleEffect", 
                "Sélectionner l'effet simple:",
                choices = factors,
                width = "100%")
  })
  
  # Rapport complet 
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
  
  # Navigation entre variables 
  output$showVariableNavigation <- reactive({
    length(input$multiResponse) > 1
  })
  outputOptions(output, "showVariableNavigation", suspendWhenHidden = FALSE)
  
  output$variableNavigation <- renderUI({
    req(input$multiResponse)
    if (is.null(input$multiResponse) || length(input$multiResponse) <= 1) {
      return(NULL)
    }
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
    }
    
    current_idx <- values$currentVarIndex
    total_vars <- length(input$multiResponse)
    
    if (current_idx < 1 || current_idx > total_vars) {
      current_idx <- 1
      values$currentVarIndex <- 1
    }
    
    current_var_name <- tryCatch({
      var_temp <- input$multiResponse[[current_idx]]
      if (is.null(var_temp) || is.na(var_temp) || var_temp == "") {
        paste("Variable", current_idx)
      } else {
        var_temp
      }
    }, error = function(e) {
      paste("Variable", current_idx)
    })
    
    div(style = "display: flex; align-items: center; gap: 15px;",
        actionButton("prevMultiVar", "", 
                     icon = icon("chevron-left"), 
                     class = "btn-light btn-lg",
                     style = "font-size: 1.5em; padding: 10px 20px; height: 60px; width: 60px;"),
        div(style = "color: white; font-size: 1.2em; font-weight: bold; text-align: center;",
            span(style = "display: block; font-size: 0.8em; opacity: 0.8;", 
                 paste("Variable", current_idx, "/", total_vars)),
            span(current_var_name)
        ),
        actionButton("nextMultiVar", "", 
                     icon = icon("chevron-right"), 
                     class = "btn-light btn-lg",
                     style = "font-size: 1.5em; padding: 10px 20px; height: 60px; width: 60px;")
    )
  })
  
  observeEvent(input$prevMultiVar, {
    if (is.null(input$multiResponse) || length(input$multiResponse) == 0) {
      return(NULL)
    }
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
      return(NULL)
    }
    
    tryCatch({
      current <- as.integer(values$currentVarIndex)
      total <- length(input$multiResponse)
      
      new_idx <- if (current > 1) current - 1 else total
      
      if (new_idx >= 1 && new_idx <= total) {
        values$currentVarIndex <- new_idx
      }
    }, error = function(e) {
      NULL
    })
  })
  
  observeEvent(input$nextMultiVar, {
    if (is.null(input$multiResponse) || length(input$multiResponse) == 0) {
      return(NULL)
    }
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
      return(NULL)
    }
    
    tryCatch({
      current <- as.integer(values$currentVarIndex)
      total <- length(input$multiResponse)
      
      new_idx <- if (current < total) current + 1 else 1
      
      if (new_idx >= 1 && new_idx <= total) {
        values$currentVarIndex <- new_idx
      }
    }, error = function(e) {
      NULL
    })
  })
  
  # Graphiques 
  
  output$xAxisOrderUI <- renderUI({
    req(input$multiResponse, input$multiFactor, values$filteredData)
    
    # Déterminer le facteur actuel
    fvar <- NULL
    if (input$plotDisplayType == "main") {
      fvar <- tryCatch({
        input$multiFactor[[1]]
      }, error = function(e) NULL)
    } else {
      # Pour les effets simples, extraire le facteur du nom
      if (!is.null(input$selectedSimpleEffect) && input$selectedSimpleEffect != "") {
        parse_result <- tryCatch({
          parts <- strsplit(input$selectedSimpleEffect, " | ", fixed = TRUE)[[1]]
          if (length(parts) >= 1) trimws(parts[1]) else NULL
        }, error = function(e) NULL)
        fvar <- parse_result
      }
    }
    
    if (is.null(fvar) || !fvar %in% colnames(values$filteredData)) return(NULL)
    
    # Obtenir les niveaux actuels
    current_levels <- if (!is.null(values$customXLevels())) {
      values$customXLevels()
    } else if (is.factor(values$filteredData[[fvar]])) {
      levels(values$filteredData[[fvar]])
    } else {
      unique(as.character(values$filteredData[[fvar]]))
    }
    
    if (length(current_levels) == 0) return(NULL)
    
    # Interface de réorganisation
    tagList(
      h6("Ordre actuel des catégories :", style = "font-weight: bold; color: #27ae60;"),
      div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; border-radius: 5px; padding: 5px;",
          lapply(seq_along(current_levels), function(i) {
            level_name <- current_levels[i]
            div(style = "background-color: white; padding: 8px; margin-bottom: 5px; border-radius: 4px; border: 1px solid #e0e0e0;",
                fluidRow(
                  column(2, 
                         strong(paste0(i, ".")),
                         style = "text-align: center; padding-top: 5px;"
                  ),
                  column(6, 
                         span(level_name, style = "font-weight: bold; color: #2c3e50;")
                  ),
                  column(4,
                         div(style = "text-align: right;",
                             if (i > 1) {
                               actionButton(paste0("moveUp_", i), "", 
                                            icon = icon("arrow-up"),
                                            class = "btn-sm btn-primary",
                                            style = "margin-right: 5px; padding: 2px 8px;")
                             },
                             if (i < length(current_levels)) {
                               actionButton(paste0("moveDown_", i), "", 
                                            icon = icon("arrow-down"),
                                            class = "btn-sm btn-primary",
                                            style = "padding: 2px 8px;")
                             }
                         )
                  )
                )
            )
          })
      ),
      hr(),
      actionButton("resetXOrder", "Réinitialiser l'ordre", 
                   class = "btn-warning btn-sm", 
                   icon = icon("undo"),
                   style = "width: 100%;")
    )
  })
  
  # Observateurs pour les boutons de déplacement
  
  observe({
    req(input$multiResponse, input$multiFactor, values$filteredData)
    
    # Déterminer le facteur actuel
    fvar <- NULL
    if (!is.null(input$plotDisplayType) && input$plotDisplayType == "main") {
      fvar <- tryCatch({
        input$multiFactor[[1]]
      }, error = function(e) NULL)
    } else {
      if (!is.null(input$selectedSimpleEffect) && input$selectedSimpleEffect != "") {
        parse_result <- tryCatch({
          parts <- strsplit(input$selectedSimpleEffect, " | ", fixed = TRUE)[[1]]
          if (length(parts) >= 1) trimws(parts[1]) else NULL
        }, error = function(e) NULL)
        fvar <- parse_result
      }
    }
    
    if (is.null(fvar) || !fvar %in% colnames(values$filteredData)) return(NULL)
    
    # Obtenir les niveaux actuels
    current_levels <- if (!is.null(values$customXLevels())) {
      values$customXLevels()
    } else if (is.factor(values$filteredData[[fvar]])) {
      levels(values$filteredData[[fvar]])
    } else {
      unique(as.character(values$filteredData[[fvar]]))
    }
    
    if (length(current_levels) == 0) return(NULL)
    
    # Boutons "Monter"
    lapply(seq_along(current_levels), function(i) {
      observeEvent(input[[paste0("moveUp_", i)]], {
        if (i > 1) {
          new_order <- current_levels
          new_order[c(i-1, i)] <- new_order[c(i, i-1)]
          values$customXLevels(new_order)
        }
      }, ignoreInit = TRUE)
    })
    
    # Boutons "Descendre"
    lapply(seq_along(current_levels), function(i) {
      observeEvent(input[[paste0("moveDown_", i)]], {
        if (i < length(current_levels)) {
          new_order <- current_levels
          new_order[c(i, i+1)] <- new_order[c(i+1, i)]
          values$customXLevels(new_order)
        }
      }, ignoreInit = TRUE)
    })
  })
  
  # Bouton de réinitialisation
  
  observeEvent(input$resetXOrder, {
    values$customXLevels(NULL)
    showNotification("Ordre des catégories réinitialisé", type = "message", duration = 2)
  })
  
  # FONCTION RÉACTIVE POUR CRÉER LE GRAPHIQUE 
  create_posthoc_plot <- reactive({
    
    plot_type <- input$plotType
    error_type <- input$errorType
    color_by_groups <- input$colorByGroups
    box_color <- input$boxColor
    rotate_labels <- input$rotateXLabels
    
    # Titres et labels
    custom_title <- input$customTitle
    custom_subtitle <- input$customSubtitle
    custom_x_label <- input$customXLabel
    custom_y_label <- input$customYLabel
    custom_legend_title <- input$customLegendTitle
    
    # Tailles de police
    title_size <- input$titleSize
    subtitle_size <- input$subtitleSize
    axis_title_size <- input$axisTitleSize
    axis_text_size <- input$axisTextSize
    graph_value_size <- input$graphValueSize
    mean_value_size <- input$meanValueSize  
    legend_title_size <- input$legendTitleSize
    legend_text_size <- input$legendTextSize
    legend_spacing <- input$legendSpacing
    
    # Styles de police
    title_font_style <- input$titleFontStyle
    subtitle_font_style <- input$subtitleFontStyle
    axis_title_font_style <- input$axisTitleFontStyle
    axis_text_x_font_style <- input$axisTextXFontStyle
    axis_text_y_font_style <- input$axisTextYFontStyle
    graph_value_font_style <- input$graphValueFontStyle
    legend_title_font_style <- input$legendTitleFontStyle
    legend_text_font_style <- input$legendTextFontStyle
    
    # Axes et graduations
    custom_axis_limits <- input$customAxisLimits
    y_axis_min <- input$yAxisMin
    y_axis_max <- input$yAxisMax
    x_axis_min <- input$xAxisMin
    x_axis_max <- input$xAxisMax
    custom_axis_breaks <- input$customAxisBreaks
    y_axis_break_step <- input$yAxisBreakStep
    x_axis_break_step <- input$xAxisBreakStep
    
    # Ordre personnalisé
    custom_x_order <- input$customXOrder
    custom_x_levels <- values$customXLevels()
    
    # Position du sous-titre
    subtitle_position <- input$subtitlePosition
    
    
    # Validations de base
    req(values$multiResultsMain, input$multiResponse, input$multiFactor, values$filteredData)
    
    if (nrow(values$multiResultsMain) == 0 || length(input$multiResponse) == 0 || length(input$multiFactor) == 0) {
      return(NULL)
    }
    
    # Gestion de l'index
    if (is.null(values$currentVarIndex)) values$currentVarIndex <- 1
    
    max_idx <- length(input$multiResponse)
    current_var_idx <- as.integer(values$currentVarIndex)
    
    if (is.na(current_var_idx) || current_var_idx < 1 || current_var_idx > max_idx) {
      current_var_idx <- 1
      values$currentVarIndex <- 1
    }
    
    # Extraction sécurisée de la variable
    resp_var <- tryCatch({
      input$multiResponse[[current_var_idx]]
    }, error = function(e) NULL)
    
    if (is.null(resp_var) || is.na(resp_var) || resp_var == "" || !resp_var %in% colnames(values$filteredData)) {
      return(NULL)
    }
    
    # Préparation selon le type
    fvar <- NULL
    plot_data <- NULL
    agg <- NULL
    
    if (input$plotDisplayType == "main") {
      fvar <- tryCatch({
        input$multiFactor[[1]]
      }, error = function(e) NULL)
      
      if (is.null(fvar) || is.na(fvar) || !fvar %in% colnames(values$filteredData)) {
        return(NULL)
      }
      
      plot_data <- values$filteredData
      
      agg <- values$multiResultsMain[
        !is.na(values$multiResultsMain$Variable) &
          !is.na(values$multiResultsMain$Facteur) &
          !is.na(values$multiResultsMain$Type) &
          values$multiResultsMain$Variable == resp_var & 
          values$multiResultsMain$Facteur == fvar &
          values$multiResultsMain$Type == "main", 
      ]
      
      if (nrow(agg) == 0) return(NULL)
      
    } else {
      if (is.null(input$selectedSimpleEffect) || input$selectedSimpleEffect == "") {
        return(NULL)
      }
      
      parse_result <- tryCatch({
        parts <- strsplit(input$selectedSimpleEffect, " | ", fixed = TRUE)[[1]]
        if (length(parts) != 2) return(list(success = FALSE))
        
        main_factor <- trimws(parts[1])
        condition <- trimws(parts[2])
        cond_parts <- strsplit(condition, "=", fixed = TRUE)[[1]]
        if (length(cond_parts) != 2) return(list(success = FALSE))
        
        cond_factor <- trimws(cond_parts[1])
        cond_level <- trimws(cond_parts[2])
        
        if (!cond_factor %in% colnames(values$filteredData)) return(list(success = FALSE))
        
        filtered_data <- values$filteredData[values$filteredData[[cond_factor]] == cond_level, ]
        if (nrow(filtered_data) == 0) return(list(success = FALSE))
        
        agg_data <- values$multiResultsMain[
          !is.na(values$multiResultsMain$Variable) &
            !is.na(values$multiResultsMain$Facteur) &
            !is.na(values$multiResultsMain$Type) &
            values$multiResultsMain$Variable == resp_var & 
            values$multiResultsMain$Facteur == input$selectedSimpleEffect &
            values$multiResultsMain$Type == "simple_effect", 
        ]
        
        if (nrow(agg_data) == 0) return(list(success = FALSE))
        
        list(success = TRUE, plot_data = filtered_data, agg = agg_data, fvar = main_factor)
        
      }, error = function(e) list(success = FALSE))
      
      if (!parse_result$success) return(NULL)
      
      plot_data <- parse_result$plot_data
      agg <- parse_result$agg
      fvar <- parse_result$fvar
    }
    
    # Vérifications finales
    if (is.null(agg) || is.null(fvar) || is.null(plot_data) || nrow(agg) == 0 || nrow(plot_data) == 0) {
      return(NULL)
    }
    
    required_cols <- c(fvar, "Moyenne", "Ecart_type", "Erreur_type", "groups")
    if (!all(required_cols %in% colnames(agg))) return(NULL)
    if (!fvar %in% colnames(plot_data) || !resp_var %in% colnames(plot_data)) return(NULL)
    
    
    # Configuration thème avec sous-titre
    
    base_theme <- theme_minimal() +
      theme(
        plot.title = element_markdown(
          size = title_size, 
          face = title_font_style, 
          hjust = 0.5
        ),
        
        # Configuration du sous-titre
        plot.subtitle = if (!is.null(custom_subtitle) && custom_subtitle != "") {
          element_text(
            size = if (!is.null(subtitle_size)) subtitle_size else 12,
            face = if (!is.null(subtitle_font_style)) subtitle_font_style else "italic",
            hjust = as.numeric(if (!is.null(subtitle_position)) subtitle_position else 0.5),
            color = "gray30",
            margin = margin(t = 5, b = 10)
          )
        } else {
          element_blank()
        },
        
        axis.title = element_markdown(
          size = axis_title_size, 
          face = axis_title_font_style
        ),
        axis.text.x = if (rotate_labels) {
          element_text(
            angle = 45, 
            hjust = 1, 
            size = axis_text_size, 
            face = axis_text_x_font_style
          )
        } else {
          element_text(
            size = axis_text_size, 
            face = axis_text_x_font_style
          )
        },
        axis.text.y = element_text(
          size = axis_text_size, 
          face = axis_text_y_font_style
        ),
        legend.position = if (color_by_groups) "right" else "none",
        legend.title = element_markdown(
          size = legend_title_size, 
          face = legend_title_font_style
        ),
        legend.text = element_text(
          size = legend_text_size, 
          face = legend_text_font_style
        ),
        # Utilisation des paramètres d'espacement de légende
        legend.key.height = unit(1.2, "lines"),  
        legend.key.width = unit(1.5, "lines"),   
        legend.spacing.y = unit(legend_spacing, "lines"),  
        legend.key.spacing.y = unit(legend_spacing, "lines"),  
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.box.spacing = unit(0.5, "lines"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    
    # Configuration des labels avec sous-titre
    
    plot_title <- if (!is.null(custom_title) && custom_title != "") {
      custom_title
    } else {
      if (input$plotDisplayType == "main") {
        paste("Effet principal:", resp_var, "par", fvar)
      } else {
        paste("Effet simple:", resp_var, "-", input$selectedSimpleEffect)
      }
    }
    
    base_labels <- labs(
      title = plot_title,
      # Ajout du sous-titre
      subtitle = if (!is.null(custom_subtitle) && custom_subtitle != "") {
        custom_subtitle
      } else {
        NULL
      },
      x = if (!is.null(custom_x_label) && custom_x_label != "") custom_x_label else fvar,
      y = if (!is.null(custom_y_label) && custom_y_label != "") custom_y_label else resp_var
    )
    
    legend_title <- if (is.null(custom_legend_title) || custom_legend_title == "") {
      "Groupes statistiques"
    } else {
      custom_legend_title
    }
    
    # Création du graphique
    p <- NULL
    
    tryCatch({
      # Harmoniser les facteurs et créer des colonnes simples
      if (!is.factor(plot_data[[fvar]])) plot_data[[fvar]] <- as.factor(plot_data[[fvar]])
      if (!is.factor(agg[[fvar]])) agg[[fvar]] <- as.factor(agg[[fvar]])
      
      common_levels <- intersect(levels(plot_data[[fvar]]), levels(agg[[fvar]]))
      if (length(common_levels) == 0) return(NULL)
      
      plot_data[[fvar]] <- factor(plot_data[[fvar]], levels = common_levels)
      agg[[fvar]] <- factor(agg[[fvar]], levels = common_levels)
      
      
      # Application de l'ordre personnalisé des catégories X
      
      if (!is.null(custom_x_order) && custom_x_order && !is.null(custom_x_levels)) {
        custom_order <- custom_x_levels
        
        # Vérifier que l'ordre personnalisé est valide
        if (all(common_levels %in% custom_order)) {
          plot_data[[fvar]] <- factor(plot_data[[fvar]], levels = custom_order)
          agg[[fvar]] <- factor(agg[[fvar]], levels = custom_order)
          common_levels <- custom_order
        }
      }
      
      # Créer des colonnes avec noms simples pour éviter les problèmes plotly
      plot_data$x_var <- plot_data[[fvar]]
      plot_data$y_var <- plot_data[[resp_var]]
      agg$x_var <- agg[[fvar]]
      
      # Calcul position texte
      y_max <- max(plot_data$y_var, na.rm = TRUE)
      y_min <- min(plot_data$y_var, na.rm = TRUE)
      y_range <- y_max - y_min
      
      
      # Valeurs par défaut sécurisées pour la taille et le style
      
      
      # Valeurs par défaut si les inputs sont NULL ou non définis
      safe_graph_value_size <- if (!is.null(graph_value_size) && !is.na(graph_value_size)) {
        graph_value_size
      } else {
        5  # Valeur par défaut
      }
      
      safe_graph_value_font_style <- if (!is.null(graph_value_font_style) && graph_value_font_style != "") {
        graph_value_font_style
      } else {
        "bold"  # Valeur par défaut
      }
      
      # Valeur sécurisée pour la taille des moyennes
      safe_mean_value_size <- if (!is.null(mean_value_size) && !is.na(mean_value_size)) {
        mean_value_size
      } else {
        4  # Valeur par défaut
      }
      
      
      # CRÉATION DES GRAPHIQUES 
      
      
      if (plot_type == "box") {
        if (color_by_groups) {
          agg_subset <- agg[, c(fvar, "groups"), drop = FALSE]
          names(agg_subset)[1] <- "x_var"
          plot_data_merged <- merge(plot_data, agg_subset, by = "x_var", all.x = TRUE)
          
          p <- ggplot(plot_data_merged, aes(x = x_var, y = y_var, fill = groups)) +
            geom_boxplot(alpha = 0.7) +
            scale_fill_discrete(name = legend_title)
        } else {
          p <- ggplot(plot_data, aes(x = x_var, y = y_var, fill = x_var)) +
            geom_boxplot(alpha = 0.7) +
            #  Utilisation des valeurs sécurisées
            annotate("text", 
                     x = 1:nrow(agg), 
                     y = y_max + y_range * 0.05, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
      } else if (plot_type == "violin") {
        if (color_by_groups) {
          agg_subset <- agg[, c(fvar, "groups"), drop = FALSE]
          names(agg_subset)[1] <- "x_var"
          plot_data_merged <- merge(plot_data, agg_subset, by = "x_var", all.x = TRUE)
          
          p <- ggplot(plot_data_merged, aes(x = x_var, y = y_var, fill = groups)) +
            geom_violin(alpha = 0.7) +
            geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
            scale_fill_discrete(name = legend_title)
        } else {
          p <- ggplot(plot_data, aes(x = x_var, y = y_var, fill = x_var)) +
            geom_violin(alpha = 0.7) +
            geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
            #  Utilisation des valeurs sécurisées
            annotate("text", 
                     x = 1:nrow(agg), 
                     y = y_max + y_range * 0.05, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
      } else if (plot_type == "point") {
        error_val <- if (error_type == "se") agg$Erreur_type
        else if (error_type == "sd") agg$Ecart_type
        else if (error_type == "ci") 1.96 * agg$Erreur_type
        else 0
        
        agg$y_err_max <- agg$Moyenne + error_val
        y_text_pos <- max(agg$y_err_max, na.rm = TRUE) * 1.05
        
        if (color_by_groups) {
          p <- ggplot(agg, aes(x = x_var, y = Moyenne, fill = groups, color = groups)) +
            geom_point(size = 4, shape = 21, stroke = 2) +
            scale_fill_discrete(name = legend_title) +
            scale_color_discrete(name = legend_title)
        } else {
          p <- ggplot(agg, aes(x = x_var, y = Moyenne, fill = x_var, color = x_var)) +
            geom_point(size = 4, shape = 21, stroke = 2) +
            #  Utilisation des valeurs sécurisées
            annotate("text", 
                     x = 1:nrow(agg), 
                     y = y_text_pos, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
        if (error_type == "se") {
          p <- p + geom_errorbar(aes(ymin = Moyenne - Erreur_type, ymax = Moyenne + Erreur_type), 
                                 width = 0.2, color = "black")
        } else if (error_type == "sd") {
          p <- p + geom_errorbar(aes(ymin = Moyenne - Ecart_type, ymax = Moyenne + Ecart_type), 
                                 width = 0.2, color = "black")
        } else if (error_type == "ci") {
          agg$ci_margin <- 1.96 * agg$Erreur_type
          p <- p + geom_errorbar(aes(ymin = Moyenne - ci_margin, ymax = Moyenne + ci_margin), 
                                 width = 0.2, color = "black")
        }
        
      } else if (plot_type == "hist") {
        if (color_by_groups) {
          p <- ggplot(agg, aes(x = x_var, y = Moyenne, fill = groups)) +
            geom_col(alpha = 0.7, color = "black") +
            scale_fill_discrete(name = legend_title)
        } else {
          p <- ggplot(agg, aes(x = x_var, y = Moyenne, fill = x_var)) +
            geom_col(alpha = 0.7, color = "black") +
            #  Utilisation des valeurs sécurisées
            annotate("text", 
                     x = 1:nrow(agg), 
                     y = agg$Moyenne * 0.8, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
        p <- p + geom_text(aes(y = Moyenne/2, label = round(Moyenne, 2)),
                           size = safe_mean_value_size,  # Taille personnalisable
                           fontface = "bold", color = "white")
        
        if (error_type != "none") {
          if (error_type == "se") {
            p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + Erreur_type), 
                                   width = 0.2, color = "black")
          } else if (error_type == "sd") {
            p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + Ecart_type), 
                                   width = 0.2, color = "black")
          } else if (error_type == "ci") {
            agg$ci_margin <- 1.96 * agg$Erreur_type
            p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + ci_margin), 
                                   width = 0.2, color = "black")
          }
        }
      }
      
      # Appliquer le thème et les labels
      p <- p + base_theme + base_labels
      
      # Palette
      if (!is.null(p) && box_color != "default" && !color_by_groups) {
        p <- p + scale_fill_brewer(palette = box_color) +
          scale_color_brewer(palette = box_color)
      }
      
      
      # Application des limites d'axes personnalisées
      
      if (!is.null(p) && !is.null(custom_axis_limits) && custom_axis_limits) {
        ylim_min <- if (!is.null(y_axis_min) && !is.na(y_axis_min)) y_axis_min else NA
        ylim_max <- if (!is.null(y_axis_max) && !is.na(y_axis_max)) y_axis_max else NA
        xlim_min <- if (!is.null(x_axis_min) && !is.na(x_axis_min)) x_axis_min else NA
        xlim_max <- if (!is.null(x_axis_max) && !is.na(x_axis_max)) x_axis_max else NA
        
        if (!is.na(ylim_min) || !is.na(ylim_max) || !is.na(xlim_min) || !is.na(xlim_max)) {
          p <- p + coord_cartesian(
            xlim = if (!is.na(xlim_min) || !is.na(xlim_max)) c(xlim_min, xlim_max) else NULL,
            ylim = if (!is.na(ylim_min) || !is.na(ylim_max)) c(ylim_min, ylim_max) else NULL,
            expand = TRUE
          )
        }
      }
      
      
      # Application des graduations personnalisées
      
      if (!is.null(p) && !is.null(custom_axis_breaks) && custom_axis_breaks) {
        
        # Graduations Y
        if (!is.null(y_axis_break_step) && !is.na(y_axis_break_step) && y_axis_break_step > 0) {
          y_data_min <- min(plot_data$y_var, na.rm = TRUE)
          y_data_max <- max(plot_data$y_var, na.rm = TRUE)
          
          # Si limites personnalisées, les utiliser
          if (!is.null(custom_axis_limits) && custom_axis_limits) {
            if (!is.null(y_axis_min) && !is.na(y_axis_min)) y_data_min <- y_axis_min
            if (!is.null(y_axis_max) && !is.na(y_axis_max)) y_data_max <- y_axis_max
          }
          
          y_breaks <- seq(
            from = floor(y_data_min / y_axis_break_step) * y_axis_break_step,
            to = ceiling(y_data_max / y_axis_break_step) * y_axis_break_step,
            by = y_axis_break_step
          )
          
          p <- p + scale_y_continuous(breaks = y_breaks)
        }
        
        # Graduations X 
        if (!is.null(x_axis_break_step) && !is.na(x_axis_break_step) && x_axis_break_step > 0) {
          if (is.numeric(plot_data$x_var)) {
            x_data_min <- min(plot_data$x_var, na.rm = TRUE)
            x_data_max <- max(plot_data$x_var, na.rm = TRUE)
            
            if (!is.null(custom_axis_limits) && custom_axis_limits) {
              if (!is.null(x_axis_min) && !is.na(x_axis_min)) x_data_min <- x_axis_min
              if (!is.null(x_axis_max) && !is.na(x_axis_max)) x_data_max <- x_axis_max
            }
            
            x_breaks <- seq(
              from = floor(x_data_min / x_axis_break_step) * x_axis_break_step,
              to = ceiling(x_data_max / x_axis_break_step) * x_axis_break_step,
              by = x_axis_break_step
            )
            
            p <- p + scale_x_continuous(breaks = x_breaks)
          }
        }
      }
      
      # Sauvegarde
      if (!is.null(p)) {
        values$currentPlot <- p
        return(p)
      }
      
    }, error = function(e) {
      showNotification(paste("Erreur graphique:", e$message), type = "error", duration = 10)
      return(NULL)
    })
    
    return(NULL)
  })
  
  # AFFICHAGE DU GRAPHIQUE AVEC PLOTLY 
  output$multiPlot <- renderPlotly({
    req(values$filteredData)
    p <- create_posthoc_plot()
    
    if (is.null(p)) return(NULL)
    
    tryCatch({
      return(ggplotly(p) %>% 
               layout(showlegend = if (input$colorByGroups) TRUE else FALSE))
    }, error = function(e_plotly) {
      # Si plotly échoue, afficher en ggplot classique
      return(renderPlot({ print(p) }))
    })
  })
  
  output$plotTitle <- renderUI({
    req(values$filteredData)
    if (is.null(input$multiResponse) || length(input$multiResponse) == 0) {
      return("Visualisations")
    }
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
    }
    
    current_var_idx <- values$currentVarIndex
    max_idx <- length(input$multiResponse)
    
    if (current_var_idx < 1 || current_var_idx > max_idx) {
      current_var_idx <- 1
      values$currentVarIndex <- 1
    }
    
    current_var <- tryCatch({
      input$multiResponse[[current_var_idx]]
    }, error = function(e) {
      return("Variable")
    })
    
    if (is.null(current_var) || is.na(current_var) || current_var == "") {
      current_var <- "Variable"
    }
    
    type_text <- if (!is.null(input$plotDisplayType) && input$plotDisplayType == "simple") {
      "Effets simples"
    } else {
      "Effets principaux"
    }
    
    tags$span(type_text, " - ", current_var)
  })
  
  # Téléchargements 
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
  
  # Supprimer les warnings de deprecation 
  options(lifecycle_verbosity = "quiet")
  suppressMessages(suppressWarnings(library(ggplot2)))
  
  # Fonction helper pour ecrire CSV avec encodage UTF-8
  write_csv_utf8 <- function(data, file, ...) {
    write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8", ...)
  }
  
  # FONCTION POUR CALCULER LES DIMENSIONS AUTOMATIQUES SELON LE DPI
  calculate_dimensions_from_dpi <- function(dpi, base_width_cm = 20, base_height_cm = 15) {
    # Définir les dimensions standard pour différents usages selon le DPI
    if (dpi <= 100) {
      # Dimensions plus grandes pour compensation
      width <- base_width_cm * 1.2
      height <- base_height_cm * 1.2
    } else if (dpi <= 150) {
      # Présentation - dimensions standard
      width <- base_width_cm
      height <- base_height_cm
    } else if (dpi <= 300) {
      # Impression standard - dimensions standards
      width <- base_width_cm
      height <- base_height_cm
    } else {
      # Haute résolution/Publication - dimensions légèrement réduites
      width <- base_width_cm * 0.9
      height <- base_height_cm * 0.9
    }
    
    return(list(width = width, height = height))
  }
  
  # Fonction helper pour Télécharger les graphiques avec options avancees
  createPlotDownloadHandler <- function(plot_func, default_name) {
    downloadHandler(
      filename = function() {
        fmt <- tolower(trimws(input[[paste0(default_name, "_format")]] %||% "png"))
        fmt <- switch(fmt, "jpg" = "jpeg", "htm" = "png", "html" = "png", fmt)
        if (!fmt %in% c("png","jpeg","tiff","bmp","svg","pdf","eps")) fmt <- "png"
        paste0(default_name, "_", Sys.Date(), ".", fmt)
      },
      contentType = "application/octet-stream",
      content = function(file) {
        fmt <- tolower(trimws(input[[paste0(default_name, "_format")]] %||% "png"))
        fmt <- switch(fmt, "jpg" = "jpeg", "htm" = "png", "html" = "png", fmt)
        if (!fmt %in% c("png","jpeg","tiff","bmp","svg","pdf","eps")) fmt <- "png"

        dpi_val <- as.integer(input[[paste0(default_name, "_dpi")]] %||% 300)
        if (is.na(dpi_val) || dpi_val < 72) dpi_val <- 300

        # Les inputs _width/_height sont en PIXELS → conversion px -> cm
        w_px <- as.numeric(input[[paste0(default_name, "_width")]]  %||% 0)
        h_px <- as.numeric(input[[paste0(default_name, "_height")]] %||% 0)
        w_cm <- if (!is.na(w_px) && w_px > 0) w_px / dpi_val * 2.54 else 25
        h_cm <- if (!is.na(h_px) && h_px > 0) h_px / dpi_val * 2.54 else 20

        p <- tryCatch(plot_func(), error = function(e) {
          showNotification(paste("Erreur génération graphique:", conditionMessage(e)),
                           type = "error", duration = 5)
          NULL
        })
        req(!is.null(p))

        tryCatch(
          suppressWarnings(ggsave(
            filename = file, plot = p, device = fmt,
            width = w_cm, height = h_cm, dpi = dpi_val, units = "cm"
          )),
          error = function(e) {
            showNotification(paste("Erreur export", toupper(fmt), ":", e$message),
                             type = "error", duration = 10)
          }
        )
      }
    )
  }
  
  # ACP (Analyse en Composantes Principales)
  
  # Selecteurs d'interface pour l'ACP
  output$pcaMeansGroupSelect <- renderUI({
    req(values$filteredData)
    # Détecter factor ET character
    fac_cols <- get_categorical_cols(values$filteredData)
    
    if (length(fac_cols) == 0) {
      return(div(
        style = "background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 10px 0;",
        p(style = "margin: 0; font-size: 12px; color: #721c24;",
          icon("exclamation-triangle"), 
          HTML(" <strong>Attention:</strong> Aucune variable categorielle (facteur ou texte) disponible pour le groupement."))
      ))
    }
    
    tagList(
      selectInput("pcaMeansGroup", "Variable de groupement pour les moyennes:", 
                  choices = fac_cols,
                  selected = fac_cols[1]),
      p(style = "margin: 5px 0 10px 0; font-size: 11px; color: #6c757d;",
        icon("lightbulb"), 
        " L'ACP sera calculee sur les moyennes de chaque groupe.")
    )
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

  # ── Panel colinéarité VIF
  output$pcaCollinearityPanel <- renderUI({
    req(values$filteredData, input$pcaVars)
    if (length(input$pcaVars) < 2) return(NULL)

    pca_data <- tryCatch(
      { d <- values$filteredData[, input$pcaVars, drop = FALSE]
        d[, sapply(d, is.numeric), drop = FALSE] },
      error = function(e) NULL
    )
    if (is.null(pca_data) || ncol(pca_data) < 2) return(NULL)

    # Calculer la matrice de corrélation et détecter les paires colinéaires
    R_mat <- safe_cor(pca_data)
    if (is.null(R_mat) || anyNA(R_mat)) return(NULL)

    # Paires avec |cor| >= 0.80 (colinéarité modérée à forte)
    pairs_high <- list()
    for (i in seq_len(nrow(R_mat))) {
      for (j in seq_len(ncol(R_mat))) {
        if (i < j && abs(R_mat[i,j]) >= 0.80) {
          pairs_high[[length(pairs_high)+1]] <- list(
            v1 = rownames(R_mat)[i],
            v2 = colnames(R_mat)[j],
            cor = round(R_mat[i,j], 3)
          )
        }
      }
    }

    if (length(pairs_high) == 0) return(NULL)

    # Calculer le VIF pour chaque variable (régression multiple)
    vif_vals <- tryCatch({
      if (ncol(pca_data) >= 3) {
        pca_data_nzv <- remove_zero_var_cols(pca_data)
        if (ncol(pca_data_nzv) < 2) { rep(NA, ncol(pca_data)) } else {
        sapply(seq_len(ncol(pca_data_nzv)), function(i) {
          y <- pca_data_nzv[[i]]
          x <- remove_zero_var_cols(pca_data_nzv[, -i, drop = FALSE])
          if (ncol(x) == 0) return(Inf)
          r2 <- tryCatch(suppressWarnings(summary(lm(y ~ ., data = x))$r.squared), error = function(e) NA)
          if (is.na(r2) || r2 >= 1) Inf else 1 / (1 - r2)
        }) }
      } else {
        rep(NA, ncol(pca_data))
      }
    }, error = function(e) rep(NA, ncol(pca_data)))
    names(vif_vals) <- names(pca_data)

    # Identifier les variables avec VIF > 5
    high_vif <- names(vif_vals[!is.na(vif_vals) & is.finite(vif_vals) & vif_vals > 5])
    # Variables parfaitement colinéaires (|cor|>0.9999)
    perfect_collinear <- unique(unlist(lapply(pairs_high, function(p) {
      if (abs(p$cor) > 0.9999) p$v2 else NULL
    })))

    # Suggestions de suppression (union des variables problématiques, priorité VIF > 10)
    suggest_remove <- unique(c(
      perfect_collinear,
      names(vif_vals[!is.na(vif_vals) & is.finite(vif_vals) & vif_vals > 10])
    ))

    severity_color <- if (length(perfect_collinear) > 0) "#dc3545"
                      else if (length(high_vif) > 0) "#fd7e14"
                      else "#ffc107"
    severity_label <- if (length(perfect_collinear) > 0) "Colinéarité parfaite détectée"
                      else if (length(high_vif) > 0) "Colinéarité forte (VIF > 5)"
                      else "Colinéarité modérée (|r| ≥ 0.80)"

    tagList(
      div(
        style = paste0(
          "border: 2px solid ", severity_color, "; border-radius: 6px; ",
          "padding: 12px; margin: 8px 0; background-color: white;"
        ),
        div(
          style = paste0(
            "display: flex; align-items: center; gap: 8px; margin-bottom: 10px; ",
            "color: ", severity_color, ";"
          ),
          icon("exclamation-triangle"),
          tags$strong(severity_label)
        ),

        # Tableau des paires colinéaires
        tags$small(
          style = "color: #495057; font-weight: bold; display: block; margin-bottom: 4px;",
          "Paires de variables corrélées :"
        ),
        tags$table(
          class = "table table-sm table-condensed",
          style = "font-size: 11px; margin-bottom: 8px;",
          tags$thead(
            tags$tr(
              tags$th("Variable 1"), tags$th("Variable 2"),
              tags$th("r"), tags$th("Interprétation")
            )
          ),
          tags$tbody(
            lapply(pairs_high, function(p) {
              level <- if (abs(p$cor) > 0.9999) "Parfaite"
                       else if (abs(p$cor) >= 0.90) "Très forte"
                       else if (abs(p$cor) >= 0.80) "Forte"
                       else "Modérée"
              col   <- if (abs(p$cor) > 0.9999) "#dc3545"
                       else if (abs(p$cor) >= 0.90) "#fd7e14"
                       else "#6c757d"
              tags$tr(
                tags$td(tags$code(p$v1)),
                tags$td(tags$code(p$v2)),
                tags$td(tags$strong(style = paste0("color:", col), p$cor)),
                tags$td(style = paste0("color:", col), level)
              )
            })
          )
        ),

        # VIF si disponible
        if (!all(is.na(vif_vals))) {
          tagList(
            tags$small(
              style = "color: #495057; font-weight: bold; display: block; margin-bottom: 4px;",
              "Facteur d'Inflation de la Variance (VIF) :"
            ),
            div(
              style = "display: flex; flex-wrap: wrap; gap: 4px; margin-bottom: 10px;",
              lapply(names(vif_vals), function(v) {
                vv <- vif_vals[v]
                col <- if (is.infinite(vv) || vv > 10) "#dc3545"
                       else if (vv > 5) "#fd7e14"
                       else "#28a745"
                lbl <- if (is.infinite(vv)) "∞" else round(vv, 1)
                tags$span(
                  style = paste0(
                    "background:", col, "; color:white; border-radius:3px; ",
                    "padding:2px 6px; font-size:11px;"
                  ),
                  paste0(v, ": ", lbl)
                )
              })
            ),
            tags$small(
              style = "color: #6c757d; display: block; margin-bottom: 8px;",
              "VIF < 5 : acceptable · VIF 5–10 : élevé · VIF > 10 : très élevé (rouge)"
            )
          )
        },

        hr(style = "margin: 8px 0;"),

        # Options de correction
        tags$strong(style = "font-size: 12px; color: #495057;",
                    icon("tools"), " Corriger la multicolinéarité :"),
        div(
          style = "margin-top: 8px; display: flex; flex-wrap: wrap; gap: 6px;",

          #  supprimer les variables suggérées automatiquement
          if (length(suggest_remove) > 0) {
            actionButton(
              "pcaAutoRemoveCollinear",
              tagList(
                icon("magic"),
                paste0(" Supprimer automatiquement (",
                       paste(suggest_remove, collapse = ", "), ")")
              ),
              class = "btn-sm btn-warning",
              style = "font-size: 11px;"
            )
          },

          #  standardiser les variables (si pas encore coché)
          actionButton(
            "pcaForceStandardize",
            tagList(icon("balance-scale"), " Forcer la standardisation"),
            class = "btn-sm btn-outline-secondary",
            style = "font-size: 11px;"
          )
        ),

        if (length(suggest_remove) > 0) {
          div(
            style = "margin-top: 6px; font-size: 11px; color: #6c757d;",
            icon("info-circle"),
            paste0(" Variables suggérées à retirer : ",
                   paste(suggest_remove, collapse = ", "),
                   ". Vous pouvez aussi les désélectionner manuellement ci-dessus.")
          )
        }
      )
    )
  })

  # Supprimer automatiquement les variables colinéaires de la sélection ACP
  observeEvent(input$pcaAutoRemoveCollinear, {
    req(values$filteredData, input$pcaVars)
    pca_data <- values$filteredData[, input$pcaVars, drop = FALSE]
    pca_data <- pca_data[, sapply(pca_data, is.numeric), drop = FALSE]

    R_mat <- safe_cor(pca_data)
    if (is.null(R_mat)) return()

    # VIF et colinéarité parfaite
    to_remove <- c()
    for (i in seq_len(nrow(R_mat))) {
      for (j in seq_len(ncol(R_mat))) {
        if (i < j && abs(R_mat[i,j]) > 0.9999)
          to_remove <- c(to_remove, colnames(R_mat)[j])
      }
    }
    if (ncol(pca_data) >= 3) {
      pca_nzv <- remove_zero_var_cols(pca_data)
      vif_v <- sapply(seq_len(ncol(pca_nzv)), function(i) {
        y <- pca_nzv[[i]]; x <- remove_zero_var_cols(pca_nzv[,-i, drop=FALSE])
        if (ncol(x) == 0) return(Inf)
        r2 <- tryCatch(suppressWarnings(summary(lm(y~., data=x))$r.squared), error=function(e) NA)
        if (is.na(r2)||r2>=1) Inf else 1/(1-r2)
      })
      names(vif_v) <- names(pca_nzv)
      to_remove <- unique(c(to_remove, names(vif_v[is.finite(vif_v) & vif_v > 10])))
    }

    new_vars <- setdiff(input$pcaVars, to_remove)
    if (length(new_vars) < 2) {
      showNotification(
        "Impossible de supprimer toutes ces variables (minimum 2 requis). Désélectionnez manuellement.",
        type = "warning", duration = 6)
      return()
    }

    updatePickerInput(session, "pcaVars", selected = new_vars)
    showNotification(
      paste0("Variables retirées : ", paste(to_remove, collapse = ", "),
             ". Relancez l'ACP."),
      type = "message", duration = 6)
  })

  # Forcer la standardisation
  observeEvent(input$pcaForceStandardize, {
    updateCheckboxInput(session, "pcaScale", value = TRUE)
    showNotification(
      "Standardisation activée. La standardisation réduit l'impact des différences d'échelle mais ne résout pas la colinéarité.",
      type = "message", duration = 6)
  })
  
  output$pcaQualiSupSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- get_categorical_cols(values$filteredData)
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
  
  # Mise à jour automatique
  pcaResultReactive <- reactive({
    req(values$filteredData, input$pcaVars)
    
    input$pcaScale
    input$pcaUseMeans
    input$pcaMeansGroup
    input$pcaQualiSup
    input$pcaIndSup
    input$pcaComponents
    input$pcaLabelSource
    input$pcaRefresh  
    
    tryCatch({
      # Vérifier si les moyennes doivent être utilisées
      use_means <- !is.null(input$pcaUseMeans) && input$pcaUseMeans && 
        !is.null(input$pcaMeansGroup) && input$pcaMeansGroup != ""
      
      if (use_means) {
        pca_data <- calculate_group_means(values$filteredData, input$pcaVars, input$pcaMeansGroup)
        # Notification pour confirmer l'utilisation des moyennes
        n_groups <- nrow(pca_data)
        showNotification(
          paste0("ACP sur moyennes: ", n_groups, " groupes (", input$pcaMeansGroup, ")"), 
          type = "message", 
          duration = 3,
          id = "pca_means_notif"
        )
      } else {
        pca_data <- values$filteredData[, input$pcaVars, drop = FALSE]
      }
      
      quali_sup_indices <- NULL
      if (!is.null(input$pcaQualiSup) && !use_means) {
        quali_sup_indices <- which(names(values$filteredData) %in% input$pcaQualiSup)
      }
      
      ind_sup_indices <- NULL
      if (!is.null(input$pcaIndSup) && !use_means) {
        ind_sup_indices <- which(rownames(values$filteredData) %in% input$pcaIndSup)
      }
      
      if (!use_means) {
        all_data <- cbind(pca_data, values$filteredData[, input$pcaQualiSup, drop = FALSE])
        
        if (!is.null(input$pcaLabelSource) && input$pcaLabelSource != "rownames") {
          custom_labels <- as.character(values$filteredData[[input$pcaLabelSource]])
          rownames(all_data) <- make.unique(custom_labels)
        }
      } else {
        all_data <- pca_data
      }
      
      # ── Garde contre la singularité : supprimer les colonnes numériques
      # quasi-colinéaires avant de passer à PCA() pour éviter solve.default crash
      num_cols <- sapply(all_data, is.numeric)
      if (sum(num_cols) >= 2) {
        R_mat <- safe_cor(all_data[, num_cols, drop = FALSE])
        if (!is.null(R_mat) && !anyNA(R_mat)) {
          # Détecter les colonnes parfaitement colinéaires 
          to_drop <- c()
          for (ci in seq_len(ncol(R_mat))) {
            for (cj in seq_len(ncol(R_mat))) {
              if (ci < cj && !names(all_data[, num_cols, drop=FALSE])[ci] %in% to_drop) {
                if (abs(R_mat[ci, cj]) > 0.9999) to_drop <- c(to_drop,
                  names(all_data[, num_cols, drop=FALSE])[cj])
              }
            }
          }
          if (length(to_drop) > 0) {
            showNotification(
              paste0("ACP : variables colinéaires supprimées automatiquement — ",
                     paste(to_drop, collapse = ", "),
                     ". Désélectionnez-les pour supprimer cet avertissement."),
              type = "warning", duration = 8)
            all_data <- all_data[, !names(all_data) %in% to_drop, drop = FALSE]
            # Recalculer les indices quali.sup après suppression
            if (!is.null(quali_sup_indices) && !use_means) {
              quali_sup_indices <- which(names(all_data) %in% input$pcaQualiSup)
              if (length(quali_sup_indices) == 0) quali_sup_indices <- NULL
            }
          }
          # Vérifier que la matrice reste inversible (det ≠ 0)
          num_only <- all_data[, sapply(all_data, is.numeric), drop = FALSE]
          if (ncol(num_only) >= 2) {
            det_val <- tryCatch(det(suppressWarnings(safe_cor(num_only, use = "complete.obs")) %||% diag(ncol(num_only))), error = function(e) NA)
            if (!is.na(det_val) && abs(det_val) < 1e-10) {
              showNotification(
                "ACP : la matrice de corrélation est singulière même après nettoyage. Essayez de réduire le nombre de variables.",
                type = "warning", duration = 8)
            }
          }
        }
      }
      
      # S'assurer qu'il reste au moins 2 variables numériques
      n_num_remaining <- sum(sapply(all_data[, !names(all_data) %in%
                               if (!is.null(input$pcaQualiSup)) input$pcaQualiSup else c(),
                               drop = FALSE], is.numeric))
      if (n_num_remaining < 2) {
        showNotification("ACP : au moins 2 variables numériques sont nécessaires.", type = "error", duration = 6)
        return(NULL)
      }
      
      res.pca <- suppressWarnings(suppressMessages(
        PCA(all_data,
            scale.unit = ifelse(is.null(input$pcaScale), TRUE, input$pcaScale),
            quali.sup  = quali_sup_indices,
            ind.sup    = ind_sup_indices,
            ncp        = ifelse(is.null(input$pcaComponents), 5, input$pcaComponents),
            graph      = FALSE)
      ))
      
      return(res.pca)
      
    }, error = function(e) {
      msg <- e$message
      if (grepl("singular|singulier|invertible|dgesv", msg, ignore.case = TRUE)) {
        showNotification(
          paste0("ACP : matrice singulière — variables trop colinéaires. ",
                 "Réduisez le nombre de variables ou désactivez l'option 'Centrer/Réduire'."),
          type = "error", duration = 10)
      } else {
        showNotification(paste("Erreur ACP :", msg), type = "error")
      }
      return(NULL)
    })
  })
  
  observe({
    res <- pcaResultReactive()
    if (!is.null(res)) {
      values$pcaResult <- res
    }
  })
  
  # Dataframes des résultats de l'ACP
  pcaDataframes <- reactive({
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    
    tryCatch({
      # Valeurs propres
      eigenvalues_df <- as.data.frame(get_eigenvalue(res.pca))
      eigenvalues_df <- cbind(Dimension = rownames(eigenvalues_df), eigenvalues_df)
      rownames(eigenvalues_df) <- NULL
      
      # Coordonnees individus
      ind_coords_df <- as.data.frame(res.pca$ind$coord)
      ind_coords_df <- cbind(Individual = rownames(ind_coords_df), ind_coords_df)
      rownames(ind_coords_df) <- NULL
      
      # Contributions individus
      ind_contrib_df <- as.data.frame(res.pca$ind$contrib)
      ind_contrib_df <- cbind(Individual = rownames(ind_contrib_df), ind_contrib_df)
      rownames(ind_contrib_df) <- NULL
      
      # Cos2 individus
      ind_cos2_df <- as.data.frame(res.pca$ind$cos2)
      ind_cos2_df <- cbind(Individual = rownames(ind_cos2_df), ind_cos2_df)
      rownames(ind_cos2_df) <- NULL
      
      # Coordonnees variables
      var_coords_df <- as.data.frame(res.pca$var$coord)
      var_coords_df <- cbind(Variable = rownames(var_coords_df), var_coords_df)
      rownames(var_coords_df) <- NULL
      
      # Contributions variables
      var_contrib_df <- as.data.frame(res.pca$var$contrib)
      var_contrib_df <- cbind(Variable = rownames(var_contrib_df), var_contrib_df)
      rownames(var_contrib_df) <- NULL
      
      # Cos2 variables
      var_cos2_df <- as.data.frame(res.pca$var$cos2)
      var_cos2_df <- cbind(Variable = rownames(var_cos2_df), var_cos2_df)
      rownames(var_cos2_df) <- NULL
      
      # Correlations variables
      var_cor_df <- as.data.frame(res.pca$var$cor)
      var_cor_df <- cbind(Variable = rownames(var_cor_df), var_cor_df)
      rownames(var_cor_df) <- NULL
      
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
      showNotification(paste("Erreur dataframes ACP:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Stocker les dataframes de l'ACP dans pour un acces fiable
  observe({
    req(pcaResultReactive())
    tryCatch({
      dfs <- pcaDataframes()
      if (!is.null(dfs)) {
        values$pcaDataframes <- dfs
        showNotification("Dataframes ACP mis en cache", type = "message", duration = 2)
      }
    }, error = function(e) {
      showNotification(paste("Erreur stockage ACP:", e$message), type = "warning")
    })
  })
  
  output$pcaCTRAxisSelect <- renderUI({
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims  <- ncol(res.pca$ind$coord)
    selectInput("pcaCTRAxis", "Composante à analyser:",
                choices = setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = 1)
  })
  
  # Selectionner les axes
  output$pcaAxisXSelect <- renderUI({
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims <- ncol(res.pca$ind$coord)
    
    selectInput("pcaAxisX", "Axe X:",
                choices = setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = 1)
  })
  
  output$pcaAxisYSelect <- renderUI({
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims <- ncol(res.pca$ind$coord)
    
    selectInput("pcaAxisY", "Axe Y:",
                choices = setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = min(2, n_dims))
  })
  
  # Fonction pour créer le plot PCA
  createPcaPlot <- function(res.pca) {
    
    # Axes sélectionnés (par défaut 1 et 2)
    axis_x <- if (!is.null(input$pcaAxisX)) as.numeric(input$pcaAxisX) else 1
    axis_y <- if (!is.null(input$pcaAxisY)) as.numeric(input$pcaAxisY) else 2
    
    plot_title <- if (!is.null(input$pcaPlotTitle) && input$pcaPlotTitle != "") {
      input$pcaPlotTitle
    } else {
      "ACP - Analyse en Composantes Principales"
    }
    
    if (input$pcaPlotType == "var") {
      p <- fviz_pca_var(res.pca,
                        axes = c(axis_x, axis_y),
                        col.var = "contrib",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE,
                        ggtheme = theme_minimal(),
                        title = plot_title)
    } else if (input$pcaPlotType == "ind") {
      p <- fviz_pca_ind(res.pca,
                        axes = c(axis_x, axis_y),
                        col.ind = "contrib",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE,
                        ggtheme = theme_minimal(),
                        title = plot_title)
    } else {
      p <- fviz_pca_biplot(res.pca,
                           axes = c(axis_x, axis_y),
                           repel = TRUE,
                           col.var = "#2E9FDF",
                           col.ind = "#696969",
                           ggtheme = theme_minimal(),
                           title = plot_title)
    }
    
    eigenvals <- get_eigenvalue(res.pca)
    pc_x_var <- round(eigenvals[axis_x, "variance.percent"], 1)
    pc_y_var <- round(eigenvals[axis_y, "variance.percent"], 1)
    
    x_label <- if (!is.null(input$pcaXLabel) && input$pcaXLabel != "") {
      input$pcaXLabel
    } else {
      paste0("PC", axis_x, " (", pc_x_var, "%)")
    }
    
    y_label <- if (!is.null(input$pcaYLabel) && input$pcaYLabel != "") {
      input$pcaYLabel
    } else {
      paste0("PC", axis_y, " (", pc_y_var, "%)")
    }
    
    p <- p + labs(x = x_label, y = y_label)
    
    if (!is.null(input$pcaCenterAxes) && input$pcaCenterAxes) {
      if (input$pcaPlotType == "var") {
        coords <- res.pca$var$coord[, c(axis_x, axis_y)]
      } else {
        coords <- res.pca$ind$coord[, c(axis_x, axis_y)]
      }
      max_range <- max(abs(range(coords, na.rm = TRUE)))
      p <- p + xlim(-max_range, max_range) + ylim(-max_range, max_range)
    }
    
    return(p)
  }
  
  output$pcaPlot <- renderPlotly({
    req(values$pcaResult)
    p <- tryCatch(
      suppressWarnings(suppressMessages(createPcaPlot(pcaResultReactive()))),
      error = function(e) {
        showNotification(paste("Erreur graphique ACP :", e$message), type = "error", duration = 8)
        NULL
      }
    )
    req(!is.null(p))
    # Vérifier que le ggplot n'est pas vide (évite "image not found" dans plotly)
    pl <- tryCatch(
      suppressWarnings(ggplotly(p) %>% layout(showlegend = TRUE)),
      error = function(e) {
        showNotification(
          "Impossible de convertir le graphique ACP en interactif. Vérifiez vos données (colinéarité, valeurs manquantes).",
          type = "warning", duration = 8)
        NULL
      }
    )
    req(!is.null(pl))
    pl
  })
  
  output$pcaSummary <- renderPrint({
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    
    # Nombre de décimales 
    use_round <- !is.null(input$pcaRoundResults) && input$pcaRoundResults
    dec <- if (use_round && !is.null(input$pcaDecimals)) input$pcaDecimals else 4
    
    cat("=== ANALYSE EN COMPOSANTES PRINCIPALES (ACP) ===\n\n")
    
    eigenvals <- get_eigenvalue(res.pca)
    cat("Variance expliquee par les composantes principales:\n")
    print(round(eigenvals, dec))
    cat("\n")
    
    cat("Contribution des variables aux composantes principales:\n")
    print(round(res.pca$var$contrib, dec))
    cat("\n")
    
    cat("Qualite de representation (cos2) des variables:\n")
    print(round(res.pca$var$cos2, dec))
  })
  
  
  # ACP - MÉTRIQUES DE VALIDATION SUPPLÉMENTAIRES
  
  
  # -- 1. Bartlett + KMO (Adéquation des données à l'ACP) --
  output$pcaBartlettKMO <- renderUI({
    req(pcaResultReactive(), values$filteredData, input$pcaVars)
    tryCatch({
      pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
      pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
      pca_data_raw <- na.omit(pca_data_raw)
      pca_data_raw <- remove_zero_var_cols(pca_data_raw)
      pca_data_raw <- remove_zero_var_cols(pca_data_raw)
      pca_data_raw <- remove_zero_var_cols(pca_data_raw)
      pca_data_raw <- remove_zero_var_cols(pca_data_raw)
      
      if (ncol(pca_data_raw) < 2 || nrow(pca_data_raw) < 4) {
        return(div(class = "callout callout-warning",
                   h4(icon("exclamation-triangle"), " Données insuffisantes"),
                   p("Au moins 2 variables et 4 observations sont nécessaires.")))
      }
      
      R <- safe_cor(pca_data_raw)
      if (is.null(R)) return(div(class="callout callout-warning", h4(icon("exclamation-triangle"), " Données insuffisantes"), p("Variables à variance nulle détectées — vérifiez vos données.")))
      n <- nrow(pca_data_raw)
      p <- ncol(pca_data_raw)
      
      # Test de sphéricité de Bartlett
      bartlett <- suppressWarnings(cortest.bartlett(R, n = n))
      chi2_val  <- round(bartlett$chisq, 3)
      df_val    <- bartlett$df
      p_val     <- round(bartlett$p.value, 4)
      
      # KMO (suppressWarnings évite "FUN(min) Inf" quand corrélations ≤ 0)
      kmo_res  <- suppressWarnings(KMO(R))
      kmo_val  <- round(kmo_res$MSA, 3)
      
      kmo_label <- if (kmo_val <= 0.5) {
        list(txt = "Inacceptable (< 0,5) — L'ACP n'est pas recommandée sur ces données.", color = "#dc3545", icon = "times-circle")
      } else if (kmo_val <= 0.6) {
        list(txt = "Médiocre (0,5 – 0,6) — L'ACP est déconseillée.", color = "#e67e22", icon = "exclamation-circle")
      } else if (kmo_val <= 0.7) {
        list(txt = "Acceptable (0,6 – 0,7) — L'ACP est utilisable avec prudence.", color = "#f39c12", icon = "exclamation-triangle")
      } else if (kmo_val <= 0.8) {
        list(txt = "Souhaitable (0,7 – 0,8) — L'ACP est appropriée.", color = "#3498db", icon = "check-circle")
      } else if (kmo_val <= 0.9) {
        list(txt = "Bon (0,8 – 0,9) — L'ACP est bien adaptée.", color = "#27ae60", icon = "check-circle")
      } else {
        list(txt = "Excellent (> 0,9) — L'ACP est parfaitement adaptée.", color = "#1a5276", icon = "star")
      }
      
      bartlett_interp <- if (p_val < 0.05) {
        list(txt = paste0("Significatif (p = ", p_val, ") — La matrice de corrélation n'est pas une matrice identité : l'ACP est justifiée."), 
             color = "#27ae60", icon = "check-circle")
      } else {
        list(txt = paste0("Non significatif (p = ", p_val, ") — Les variables semblent indépendantes. L'ACP n'apportera pas de structure factorielle utile."), 
             color = "#dc3545", icon = "times-circle")
      }
      
      tagList(
        div(style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 8px; padding: 18px; margin-bottom: 15px; border: 1px solid #dee2e6;",
            h5(style = "color: #2c3e50; font-weight: bold; margin-top: 0; border-bottom: 2px solid #3498db; padding-bottom: 8px;",
               icon("flask"), " Étape 1 — Test de sphéricité de Bartlett"),
            p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 10px;",
              "Ce test vérifie si la matrice de corrélation est significativement différente d'une matrice identité. Un résultat significatif (p < 0,05) confirme que les variables sont corrélées entre elles et que l'ACP est pertinente."),
            fluidRow(
              column(4, div(style = "text-align: center; background: white; border-radius: 6px; padding: 10px; border: 1px solid #dee2e6;",
                            p(style = "margin: 0; font-size: 11px; color: #888; text-transform: uppercase;", "Chi² de Bartlett"),
                            h4(style = "margin: 4px 0; color: #2c3e50; font-weight: bold;", chi2_val))),
              column(4, div(style = "text-align: center; background: white; border-radius: 6px; padding: 10px; border: 1px solid #dee2e6;",
                            p(style = "margin: 0; font-size: 11px; color: #888; text-transform: uppercase;", "Degrés de liberté"),
                            h4(style = "margin: 4px 0; color: #2c3e50; font-weight: bold;", df_val))),
              column(4, div(style = "text-align: center; background: white; border-radius: 6px; padding: 10px; border: 1px solid #dee2e6;",
                            p(style = "margin: 0; font-size: 11px; color: #888; text-transform: uppercase;", "p-value"),
                            h4(style = paste0("margin: 4px 0; font-weight: bold; color: ", bartlett_interp$color, ";"), p_val)))
            ),
            div(style = paste0("margin-top: 10px; padding: 8px 12px; border-left: 4px solid ", bartlett_interp$color, "; background-color: white; border-radius: 0 4px 4px 0;"),
                p(style = paste0("margin: 0; font-size: 12px; color: ", bartlett_interp$color, ";"),
                  icon(bartlett_interp$icon), " ", bartlett_interp$txt))
        ),
        div(style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 8px; padding: 18px; margin-bottom: 15px; border: 1px solid #dee2e6;",
            h5(style = "color: #2c3e50; font-weight: bold; margin-top: 0; border-bottom: 2px solid #8e44ad; padding-bottom: 8px;",
               icon("sliders"), " Étape 2 — Indice KMO (Kaiser-Meyer-Olkin)"),
            p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 10px;",
              "Le KMO mesure l'adéquation de l'échantillon. Il compare les corrélations partielles aux corrélations totales. Plus il est proche de 1, plus les données sont adaptées à une ACP."),
            div(style = "text-align: center; padding: 15px;",
                div(style = paste0("display: inline-block; background: white; border-radius: 50%; width: 100px; height: 100px; line-height: 100px; font-size: 28px; font-weight: bold; color: white; background-color: ", kmo_label$color, "; box-shadow: 0 4px 8px rgba(0,0,0,0.2);"),
                    kmo_val)
            ),
            div(style = paste0("margin-top: 10px; padding: 8px 12px; border-left: 4px solid ", kmo_label$color, "; background-color: white; border-radius: 0 4px 4px 0;"),
                p(style = paste0("margin: 0; font-size: 12px; color: ", kmo_label$color, ";"),
                  icon(kmo_label$icon), " ", kmo_label$txt))
        )
      )
    }, error = function(e) {
      div(class = "callout callout-danger",
          tags$p(tagList(icon("exclamation-triangle"), " Erreur lors du calcul Bartlett/KMO : ", e$message)))
    })
  })
  
  # -- 2. Scree Plot (Graphique des éboulis) --
  # Fonction interne réutilisable pour créer le scree plot ggplot
  createScreePlot <- function(res.pca) {
    eig_mat <- res.pca$eig  # matrice FactoMineR : colonnes "eigenvalue", "percentage of variance", "cumulative percentage of variance"
    df_eig <- data.frame(
      PC            = factor(rownames(eig_mat), levels = rownames(eig_mat)),
      Valeur_propre = as.numeric(eig_mat[, 1]),
      Variance      = as.numeric(eig_mat[, 2])
    )
    kaiser_threshold <- 1
    n_kaiser <- sum(df_eig$Valeur_propre >= kaiser_threshold)
    
    ggplot(df_eig, aes(x = PC, y = Valeur_propre, group = 1)) +
      geom_line(color = "#2E86AB", linewidth = 1.2) +
      geom_point(aes(color = Valeur_propre >= kaiser_threshold), size = 4) +
      scale_color_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#dc3545"),
                         labels = c("TRUE" = "Retenue (>= 1)", "FALSE" = "Exclue (< 1)"),
                         name = "Critère de Kaiser") +
      geom_hline(yintercept = kaiser_threshold, linetype = "dashed", color = "#e74c3c", size = 0.8) +
      annotate("text", x = 1, y = kaiser_threshold + 0.05 * max(df_eig$Valeur_propre),
               label = "Seuil de Kaiser (\u03bb = 1)", hjust = 0, color = "#e74c3c", size = 3.5, fontface = "italic") +
      geom_text(aes(label = round(Valeur_propre, 2)), vjust = -0.8, size = 3.5, fontface = "bold", color = "#2c3e50") +
      labs(
        title    = "Graphique des \u00e9boulis (Scree Plot)",
        subtitle = paste0(n_kaiser, " composante(s) retenue(s) selon le crit\u00e8re de Kaiser (\u03bb \u2265 1)"),
        x        = "Composante principale",
        y        = "Valeur propre (\u03bb)",
        caption  = "Les composantes en vert ont une valeur propre \u2265 1 et sont retenues pour interpr\u00e9tation."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_markdown(hjust = 0.5, face = "bold", size = 14, color = "#2c3e50"),
        plot.subtitle = element_text(hjust = 0.5, color = "#555", size = 11),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }
  
  output$pcaScreePlot <- renderPlot({
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    createScreePlot(res.pca)
  })
  
  output$downloadPcaScreePlot <- downloadHandler(
    filename = function() paste0("acp_screeplot_", Sys.Date(), ".", input$pcaScree_format),
    content = function(file) {
      dpi <- input$pcaScree_dpi
      auto_dims <- calculate_dimensions_from_dpi(dpi, 25, 18)
      p <- createScreePlot(pcaResultReactive())
      suppressWarnings(ggsave(file, plot = p, device = input$pcaScree_format,
                              width = auto_dims$width, height = auto_dims$height, dpi = dpi, units = "cm"))
    }
  )
  
  # -- 3. Analyse parallèle --
  createParallelPlot <- function(pca_data_raw, res.pca) {
    n <- nrow(pca_data_raw)
    p <- ncol(pca_data_raw)
    n_iter <- 100
    
    eig_mat      <- res.pca$eig
    eigenvals_obs <- as.numeric(eig_mat[, 1])
    
    sim_eigenvals <- matrix(0, nrow = n_iter, ncol = p)
    for (i in 1:n_iter) {
      random_data <- matrix(rnorm(n * p), nrow = n, ncol = p)
      random_pca  <- prcomp(random_data, scale. = TRUE)
      sim_eigenvals[i, ] <- random_pca$sdev^2
    }
    
    mean_sim   <- colMeans(sim_eigenvals)
    perc95_sim <- apply(sim_eigenvals, 2, quantile, probs = 0.95)
    n_retain   <- sum(eigenvals_obs > perc95_sim)
    
    df_plot <- data.frame(
      PC           = 1:p,
      Observees    = eigenvals_obs,
      Aleatoire_moy = mean_sim,
      Aleatoire_p95 = perc95_sim
    )
    
    ggplot(df_plot, aes(x = PC)) +
      geom_line(aes(y = Aleatoire_p95, color = "Simulation al\u00e9atoire (p95)"), linetype = "dashed", size = 1.2) +
      geom_line(aes(y = Aleatoire_moy, color = "Simulation al\u00e9atoire (moy)"), linetype = "dotted", size = 0.9) +
      geom_line(aes(y = Observees, color = "Valeurs propres observ\u00e9es"), linewidth = 1.4) +
      geom_point(aes(y = Observees, color = "Valeurs propres observ\u00e9es"), size = 3) +
      geom_hline(yintercept = 1, linetype = "solid", color = "#e74c3c", size = 0.6, alpha = 0.5) +
      scale_color_manual(values = c(
        "Valeurs propres observ\u00e9es" = "#2E86AB",
        "Simulation al\u00e9atoire (p95)"  = "#e74c3c",
        "Simulation al\u00e9atoire (moy)"  = "#f39c12"
      ), name = NULL) +
      scale_x_continuous(breaks = 1:p) +
      labs(
        title    = "Analyse parall\u00e8le de Horn",
        subtitle = paste0(n_retain, " composante(s) \u00e0 retenir (valeurs observ\u00e9es > percentile 95 des simulations, ",
                          n_iter, " it\u00e9rations)"),
        x       = "Num\u00e9ro de la composante",
        y       = "Valeur propre",
        caption = "Les composantes dont la valeur propre observ\u00e9e d\u00e9passe la courbe rouge (p95 al\u00e9atoire) sont \u00e0 retenir."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_markdown(hjust = 0.5, face = "bold", size = 14, color = "#2c3e50"),
        plot.subtitle = element_text(hjust = 0.5, color = "#555", size = 11),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  }
  
  output$pcaParallelPlot <- renderPlot({
    req(pcaResultReactive(), values$filteredData, input$pcaVars)
    tryCatch({
      pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
      pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
      pca_data_raw <- na.omit(pca_data_raw)
      
      if (ncol(pca_data_raw) < 2 || nrow(pca_data_raw) < 10) {
        plot.new()
        text(0.5, 0.5, "Données insuffisantes pour l'analyse parallèle\n(minimum 10 observations requises)",
             cex = 1.2, col = "#e74c3c", adj = c(0.5, 0.5))
        return()
      }
      
      createParallelPlot(pca_data_raw, pcaResultReactive())
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Erreur analyse parallèle :", e$message),
           cex = 1, col = "#e74c3c", adj = c(0.5, 0.5))
    })
  })
  
  output$downloadPcaParallelPlot <- downloadHandler(
    filename = function() paste0("acp_analyse_parallele_", Sys.Date(), ".", input$pcaParallel_format),
    content = function(file) {
      pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
      pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
      pca_data_raw <- na.omit(pca_data_raw)
      dpi  <- input$pcaParallel_dpi
      auto <- calculate_dimensions_from_dpi(dpi, 25, 18)
      p    <- createParallelPlot(pca_data_raw, pcaResultReactive())
      suppressWarnings(ggsave(file, plot = p, device = input$pcaParallel_format,
                              width = auto$width, height = auto$height, dpi = dpi, units = "cm"))
    }
  )
  
  
  # -- 4. Rotation orthogonale --
  output$pcaRotationResult <- renderPrint({
    req(pcaResultReactive(), values$filteredData, input$pcaVars, input$pcaRotationMethod, input$pcaRotationNFactors)
    tryCatch({
      pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
      pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
      pca_data_raw <- na.omit(pca_data_raw)
      
      # ── Gardes avant fa() 
      
      n_vars    <- ncol(pca_data_raw)
      n_obs     <- nrow(pca_data_raw)
      
      if (n_vars < 2) {
        cat("⚠ Moins de 2 variables avec variance non nulle — rotation impossible.
")
        cat("  Vérifiez que vos variables ne sont pas constantes.
")
        return(invisible(NULL))
      }
      
      # Limiter le nombre de facteurs
      n_factors <- max(1, min(input$pcaRotationNFactors, n_vars - 1, max(1, n_obs - 1)))
      method    <- input$pcaRotationMethod
      
      # Vérifier que la matrice de corrélation est inversible
      R_mat <- safe_cor(pca_data_raw, use = "complete.obs")
      if (is.null(R_mat) || anyNA(R_mat)) {
        cat("⚠ Impossible de calculer la matrice de corrélation (NA persistants après na.omit).
")
        cat("  Conseil : vérifiez vos données (valeurs manquantes, variables constantes).
")
        return(invisible(NULL))
      }
      det_val <- tryCatch(det(R_mat), error = function(e) NA)
      
      is_singular <- !is.na(det_val) && abs(det_val) < 1e-8
      # Si singulière, utiliser "minres" qui est plus robuste que "pa" (pas de SMC)
      fm_method <- if (is_singular) "minres" else "pa"
      
      cat("=== ROTATION", toupper(method), "—", n_factors, "FACTEUR(S) ===

")
      cat("La rotation", method, "est une rotation orthogonale qui conserve l'indépendance des axes.
")
      cat("Elle simplifie la structure factorielle pour faciliter l'interprétation.

")
      if (is_singular) {
        cat("⚠ Note : matrice de corrélation quasi-singulière détectée.
")
        cat("  Méthode de factorisation automatiquement ajustée à 'minres' (plus robuste).
")
        cat("  Conseil : réduisez le nombre de variables ou supprimez les doublons.

")
      }
      
      # Appel fa() robuste : suppressWarnings() supprime les warnings C-level de psych
      # (FUN(min) Inf, SMC<0, cov2cor non-fini) qui échappent à withCallingHandlers
      fa_res <- tryCatch({
        suppressWarnings(
          fa(pca_data_raw,
             nfactors = n_factors,
             rotate   = method,
             fm       = fm_method,
             scores   = "regression",
             warnings = FALSE)
        )
      }, error = function(e) {
        # Repli avec fm="minres" si la méthode initiale échoue
        tryCatch(
          suppressWarnings(
            fa(pca_data_raw, nfactors = n_factors, rotate = method, fm = "minres",
               scores = "regression", warnings = FALSE)
          ),
          error = function(e2) {
            cat("⚠ Échec de la rotation :", e2$message, "
")
            cat("  Conseil : réduisez le nombre de facteurs ou changez de méthode.
")
            NULL
          }
        )
      })
      
      if (is.null(fa_res)) return(invisible(NULL))
      
      if (n_factors >= n_vars) {
        cat("⚠ Avertissement : nombre de facteurs trop élevé par rapport aux variables — cas Heywood possible.
")
        cat("  Conseil : réduisez le nombre de facteurs (essayez", max(1, n_vars - 1), "ou moins).

")
      }
      
      cat("Loadings après rotation", method, ":\n")
      cat("(Seuil de lecture : |loading| >= 0,40 indique une contribution significative)\n\n")
      print(fa_res$loadings, cutoff = 0.0, digits = 3)
      
      cat("\nVariance expliquée après rotation :\n")
      var_tab <- data.frame(
        Facteur = colnames(fa_res$loadings),
        SS_Loadings = round(fa_res$Vaccounted["SS loadings", ], 3),
        Prop_Variance = round(fa_res$Vaccounted["Proportion Var", ], 3),
        Cumul_Variance = round(fa_res$Vaccounted["Cumulative Var", ], 3)
      )
      print(var_tab, row.names = FALSE)
      
      cat("\nIndice de simplicité de la rotation (objectif : simplifier la lecture) :\n")
      cat("  - Plus les loadings sont proches de 0 ou 1 (en valeur absolue), meilleure est la structure.\n")
      cat("  - Les loadings |x| >= 0,70 : contribution forte.\n")
      cat("  - Les loadings 0,40 <= |x| < 0,70 : contribution modérée.\n")
      cat("  - Les loadings |x| < 0,40 : contribution faible (souvent ignorée).\n")
      
    }, error = function(e) {
      cat("Erreur lors de la rotation :", e$message, "\n")
      cat("Conseil : Réduisez le nombre de facteurs ou changez de méthode de rotation.\n")
    })
  })
  
  # -- 5. Contributions absolues (CTR) --
  createCTRPlot <- function(res.pca, axis_num) {
    ctr_vars <- res.pca$var$contrib[, axis_num, drop = FALSE]
    df_ctr <- data.frame(
      Variable = rownames(ctr_vars),
      CTR = ctr_vars[, 1]
    )
    df_ctr <- df_ctr[order(df_ctr$CTR, decreasing = TRUE), ]
    df_ctr$Variable <- factor(df_ctr$Variable, levels = rev(df_ctr$Variable))
    threshold <- 100 / nrow(df_ctr)
    
    ggplot(df_ctr, aes(x = Variable, y = CTR, fill = CTR >= threshold)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_hline(yintercept = threshold, linetype = "dashed", color = "#e74c3c", size = 0.9) +
      annotate("text", x = 0.6, y = threshold + 0.2,
               label = paste0("Seuil th\u00e9orique (", round(threshold, 1), "%)"),
               hjust = 0, color = "#e74c3c", size = 3.5, fontface = "italic") +
      scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#bdc3c7"),
                        labels = c("TRUE" = "Contribution notable", "FALSE" = "Contribution faible"),
                        name = NULL) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        title    = paste0("Contributions absolues (CTR) \u2014 Composante PC", axis_num),
        subtitle = paste0("Seuil th\u00e9orique = 100% / ", nrow(df_ctr), " variables = ",
                          round(threshold, 1), "%. Les variables en vert contribuent au-del\u00e0 du seuil."),
        x       = NULL,
        y       = "Contribution (%)",
        caption = "Une contribution sup\u00e9rieure au seuil th\u00e9orique indique que la variable influence significativement la composante."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_markdown(hjust = 0.5, face = "bold", size = 13, color = "#2c3e50"),
        plot.subtitle = element_text(hjust = 0.5, color = "#555", size = 10),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  }
  
  output$pcaCTRPlot <- renderPlot({
    req(pcaResultReactive(), input$pcaCTRAxis)
    createCTRPlot(pcaResultReactive(), as.numeric(input$pcaCTRAxis))
  })
  
  output$downloadPcaCTRPlot <- downloadHandler(
    filename = function() paste0("acp_CTR_PC", input$pcaCTRAxis, "_", Sys.Date(), ".", input$pcaCTR_format),
    content = function(file) {
      dpi  <- input$pcaCTR_dpi
      auto <- calculate_dimensions_from_dpi(dpi, 25, 18)
      p    <- createCTRPlot(pcaResultReactive(), as.numeric(input$pcaCTRAxis))
      suppressWarnings(ggsave(file, plot = p, device = input$pcaCTR_format,
                              width = auto$width, height = auto$height, dpi = dpi, units = "cm"))
    }
  )
  
  
  
  
  # EXPORTS SÉPARÉS PAR ANALYSE
  
  
  # ---- Helpers internes 
  .write_xlsx_sheets <- function(wb, sheet_data_list) {
    for (nm in names(sheet_data_list)) {
      addWorksheet(wb, nm)
      writeData(wb, nm, sheet_data_list[[nm]])
    }
  }
  
  .write_csv_zip <- function(file, named_df_list) {
    temp_dir  <- tempdir()
    csv_files <- character(0)
    for (nm in names(named_df_list)) {
      path <- file.path(temp_dir, paste0(nm, ".csv"))
      write_csv_utf8(named_df_list[[nm]], path)
      csv_files <- c(csv_files, paste0(nm, ".csv"))
    }
    zip(file, file.path(temp_dir, csv_files), flags = "-j")
    csv_files
  }
  
  
  # ACP — Export métriques
  
  output$downloadPcaMetricsXlsx <- downloadHandler(
    filename = function() paste0("acp_metriques_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      tryCatch({
        req(pcaResultReactive(), values$filteredData, input$pcaVars)
        pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
        pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
        pca_data_raw <- na.omit(pca_data_raw)
        
        dfs <- build_pca_metrics_df(pcaResultReactive(), pca_data_raw)
        if (is.null(dfs)) { showNotification("Erreur : données ACP indisponibles.", type = "error"); return() }
        
        wb <- createWorkbook()
        .write_xlsx_sheets(wb, list(
          "Valeurs_propres"  = dfs$valeurs_propres,
          "Bartlett_KMO"     = dfs$bartlett_kmo,
          "CTR_variables"    = dfs$ctr_variables,
          "Cos2_variables"   = dfs$cos2_variables
        ))
        saveWorkbook(wb, file, overwrite = TRUE)
        showNotification("Export Excel ACP réussi !", type = "message", duration = 3)
      }, error = function(e) showNotification(paste("Erreur Excel ACP :", e$message), type = "error", duration = 8))
    }
  )
  
  output$downloadPcaMetricsCsv <- downloadHandler(
    filename = function() paste0("acp_metriques_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content = function(file) {
      tryCatch({
        req(pcaResultReactive(), values$filteredData, input$pcaVars)
        pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
        pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
        pca_data_raw <- na.omit(pca_data_raw)
        
        dfs <- build_pca_metrics_df(pcaResultReactive(), pca_data_raw)
        if (is.null(dfs)) { showNotification("Erreur : données ACP indisponibles.", type = "error"); return() }
        
        n <- .write_csv_zip(file, list(
          "acp_valeurs_propres" = dfs$valeurs_propres,
          "acp_bartlett_kmo"    = dfs$bartlett_kmo,
          "acp_ctr_variables"   = dfs$ctr_variables,
          "acp_cos2_variables"  = dfs$cos2_variables
        ))
        showNotification(paste0("Export CSV ACP réussi (", length(n), " fichiers) !"), type = "message", duration = 3)
      }, error = function(e) showNotification(paste("Erreur CSV ACP :", e$message), type = "error", duration = 8))
    }
  )
  
  
  # HCPC — Export métriques
  
  output$downloadHcpcMetricsXlsx <- downloadHandler(
    filename = function() paste0("hcpc_metriques_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      tryCatch({
        vd <- hcpcValidationData()
        if (is.null(vd)) { showNotification("Erreur : données HCPC indisponibles.", type = "error"); return() }
        
        dfs <- build_hcpc_metrics_df(vd)
        if (is.null(dfs)) { showNotification("Erreur : calcul métriques HCPC échoué.", type = "error"); return() }
        
        wb <- createWorkbook()
        .write_xlsx_sheets(wb, list(
          "Indices_validation"   = dfs$indices_validation,
          "Affectation_clusters" = dfs$affectation_clusters
        ))
        saveWorkbook(wb, file, overwrite = TRUE)
        showNotification("Export Excel HCPC réussi !", type = "message", duration = 3)
      }, error = function(e) showNotification(paste("Erreur Excel HCPC :", e$message), type = "error", duration = 8))
    }
  )
  
  output$downloadHcpcMetricsCsv <- downloadHandler(
    filename = function() paste0("hcpc_metriques_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content = function(file) {
      tryCatch({
        vd <- hcpcValidationData()
        if (is.null(vd)) { showNotification("Erreur : données HCPC indisponibles.", type = "error"); return() }
        
        dfs <- build_hcpc_metrics_df(vd)
        if (is.null(dfs)) { showNotification("Erreur : calcul métriques HCPC échoué.", type = "error"); return() }
        
        n <- .write_csv_zip(file, list(
          "hcpc_indices_validation"   = dfs$indices_validation,
          "hcpc_affectation_clusters" = dfs$affectation_clusters
        ))
        showNotification(paste0("Export CSV HCPC réussi (", length(n), " fichiers) !"), type = "message", duration = 3)
      }, error = function(e) showNotification(paste("Erreur CSV HCPC :", e$message), type = "error", duration = 8))
    }
  )
  
  
  # AFD — Export métriques
  
  output$downloadAfdMetricsXlsx <- downloadHandler(
    filename = function() paste0("afd_metriques_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      tryCatch({
        req(afdResultReactive())
        dfs <- build_afd_metrics_df(afdResultReactive())
        if (is.null(dfs)) { showNotification("Erreur : calcul métriques AFD échoué.", type = "error"); return() }
        
        wb <- createWorkbook()
        .write_xlsx_sheets(wb, list(
          "Variance_Eta2"         = dfs$variance_eta2,
          "Classification_globale" = dfs$classification_globale,
          "Classification_groupe"  = dfs$classification_groupe,
          "Matrice_confusion"      = dfs$matrice_confusion
        ))
        saveWorkbook(wb, file, overwrite = TRUE)
        showNotification("Export Excel AFD réussi !", type = "message", duration = 3)
      }, error = function(e) showNotification(paste("Erreur Excel AFD :", e$message), type = "error", duration = 8))
    }
  )
  
  output$downloadAfdMetricsCsv <- downloadHandler(
    filename = function() paste0("afd_metriques_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content = function(file) {
      tryCatch({
        req(afdResultReactive())
        dfs <- build_afd_metrics_df(afdResultReactive())
        if (is.null(dfs)) { showNotification("Erreur : calcul métriques AFD échoué.", type = "error"); return() }
        
        n <- .write_csv_zip(file, list(
          "afd_variance_eta2"           = dfs$variance_eta2,
          "afd_classification_globale"  = dfs$classification_globale,
          "afd_classification_groupe"   = dfs$classification_groupe,
          "afd_matrice_confusion"       = dfs$matrice_confusion
        ))
        showNotification(paste0("Export CSV AFD réussi (", length(n), " fichiers) !"), type = "message", duration = 3)
      }, error = function(e) showNotification(paste("Erreur CSV AFD :", e$message), type = "error", duration = 8))
    }
  )
  
  # Helper: construire le dataframe des métriques ACP
  build_pca_metrics_df <- function(res.pca, pca_data_raw) {
    tryCatch({
      eig_mat <- res.pca$eig
      df_eig  <- data.frame(
        Composante           = rownames(eig_mat),
        Valeur_propre        = round(as.numeric(eig_mat[, 1]), 4),
        Variance_pct         = round(as.numeric(eig_mat[, 2]), 4),
        Variance_cumulee_pct = round(as.numeric(eig_mat[, 3]), 4),
        Kaiser_retenue       = as.numeric(eig_mat[, 1]) >= 1
      )
      
      R <- safe_cor(pca_data_raw)
      if (is.null(R)) { return(data.frame()) }
      n <- nrow(pca_data_raw)
      bartlett  <- suppressWarnings(cortest.bartlett(R, n = n))
      kmo_res   <- suppressWarnings(KMO(R))
      
      df_bartlett_kmo <- data.frame(
        Test                     = c("Bartlett Chi2", "Bartlett df", "Bartlett p-value", "KMO MSA"),
        Valeur                   = c(round(bartlett$chisq, 3), bartlett$df,
                                     round(bartlett$p.value, 4), round(kmo_res$MSA, 3)),
        Interpretation           = c(
          ifelse(bartlett$p.value < 0.05, "Significatif — ACP justifiée", "Non significatif — ACP non justifiée"),
          "",
          ifelse(bartlett$p.value < 0.05, "p < 0.05 : matrice ≠ identité", "p >= 0.05 : variables indépendantes"),
          ifelse(kmo_res$MSA > 0.9, "Excellent",
                 ifelse(kmo_res$MSA > 0.8, "Bon",
                        ifelse(kmo_res$MSA > 0.7, "Souhaitable",
                               ifelse(kmo_res$MSA > 0.6, "Acceptable",
                                      ifelse(kmo_res$MSA > 0.5, "Médiocre", "Inacceptable")))))
        )
      )
      
      df_ctr_all <- as.data.frame(res.pca$var$contrib)
      df_ctr_all <- cbind(Variable = rownames(df_ctr_all), round(df_ctr_all, 4))
      rownames(df_ctr_all) <- NULL
      
      df_cos2_vars <- as.data.frame(res.pca$var$cos2)
      df_cos2_vars <- cbind(Variable = rownames(df_cos2_vars), round(df_cos2_vars, 4))
      rownames(df_cos2_vars) <- NULL
      
      list(
        valeurs_propres  = df_eig,
        bartlett_kmo     = df_bartlett_kmo,
        ctr_variables    = df_ctr_all,
        cos2_variables   = df_cos2_vars
      )
    }, error = function(e) NULL)
  }
  
  # Helper: construire le dataframe des métriques AFD
  build_afd_metrics_df <- function(afd_res) {
    tryCatch({
      afd_result  <- afd_res$model
      afd_predict <- afd_res$predictions
      afd_data    <- afd_res$data
      
      eigenvals <- afd_result$svd^2
      prop_var  <- eigenvals / sum(eigenvals) * 100
      can_cor   <- sqrt(eigenvals / (1 + eigenvals))
      eta2_vals <- eigenvals / (1 + eigenvals)
      
      df_var <- data.frame(
        Fonction             = paste0("LD", seq_along(eigenvals)),
        Valeur_propre        = round(eigenvals, 4),
        Variance_pct         = round(prop_var, 4),
        Variance_cumulee_pct = round(cumsum(prop_var), 4),
        Correlation_canonique = round(can_cor, 4),
        Eta2                 = round(eta2_vals, 4),
        Appreciation_eta2    = sapply(eta2_vals, function(e) {
          if (e >= 0.64) "Excellent" else if (e >= 0.25) "Fort" else if (e >= 0.09) "Modere" else "Faible"
        })
      )
      
      confusion_matrix <- table(Reel   = afd_data[[input$afdFactor]],
                                Predit = afd_predict$class)
      accuracy  <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      n_total   <- sum(confusion_matrix)
      expected  <- sum(rowSums(confusion_matrix) * colSums(confusion_matrix)) / (n_total^2)
      kappa_val <- (accuracy - expected) / (1 - expected)
      
      kappa_interp <- if (kappa_val >= 0.8) "Quasi-parfait"
      else if (kappa_val >= 0.6) "Substantiel"
      else if (kappa_val >= 0.4) "Modere"
      else if (kappa_val >= 0.2) "Passable"
      else "Faible"
      
      df_classif <- data.frame(
        Metrique       = c("Accuracy globale (%)", "Kappa de Cohen", "Appreciation Kappa"),
        Valeur         = c(round(accuracy * 100, 2), round(kappa_val, 3), kappa_interp)
      )
      
      df_group_acc <- data.frame(
        Groupe = rownames(confusion_matrix),
        Taux_classif_pct = sapply(1:nrow(confusion_matrix), function(i) {
          round(confusion_matrix[i, i] / sum(confusion_matrix[i, ]) * 100, 2)
        })
      )
      
      confusion_df <- as.data.frame.matrix(confusion_matrix)
      confusion_df <- cbind(Groupe_reel = rownames(confusion_df), confusion_df)
      rownames(confusion_df) <- NULL
      
      list(
        variance_eta2        = df_var,
        classification_globale = df_classif,
        classification_groupe  = df_group_acc,
        matrice_confusion      = confusion_df
      )
    }, error = function(e) NULL)
  }
  
  # Helper: construire le dataframe des métriques HCPC
  build_hcpc_metrics_df <- function(vd) {
    tryCatch({
      coords   <- vd$coords
      clusters <- vd$clusters
      k        <- vd$k
      n        <- nrow(coords)
      res.hcpc <- vd$hcpc
      res.pca  <- vd$pca
      
      grand_mean <- colMeans(coords)
      SS_B <- SS_W <- 0
      for (cl in unique(clusters)) {
        idx <- which(clusters == cl)
        nc  <- length(idx)
        cm  <- colMeans(coords[idx, , drop = FALSE])
        SS_B <- SS_B + nc * sum((cm - grand_mean)^2)
        for (i in idx) SS_W <- SS_W + sum((coords[i, ] - cm)^2)
      }
      CH <- if (SS_W > 0) round((SS_B / (k - 1)) / (SS_W / (n - k)), 2) else NA
      
      cluster_list <- lapply(unique(sort(clusters)), function(cl) coords[clusters == cl, , drop = FALSE])
      centroids    <- lapply(cluster_list, colMeans)
      dispersions  <- sapply(seq_along(cluster_list), function(i) {
        mean(sqrt(rowSums(sweep(cluster_list[[i]], 2, centroids[[i]])^2)))
      })
      DB_vals <- sapply(seq_along(centroids), function(i) {
        max(sapply(seq_along(centroids), function(j) {
          if (i == j) return(0)
          d_ij <- sqrt(sum((centroids[[i]] - centroids[[j]])^2))
          if (d_ij == 0) return(0)
          (dispersions[i] + dispersions[j]) / d_ij
        }))
      })
      DB <- round(mean(DB_vals), 3)
      
      sil_vals <- numeric(n)
      for (i in 1:n) {
        cl_i <- clusters[i]; same <- which(clusters == cl_i); same <- same[same != i]
        a_i <- if (length(same) == 0) 0 else mean(sqrt(rowSums(sweep(coords[same, , drop = FALSE], 2, coords[i, ])^2)))
        b_i <- min(sapply(unique(clusters[clusters != cl_i]), function(cl) {
          other <- which(clusters == cl)
          mean(sqrt(rowSums(sweep(coords[other, , drop = FALSE], 2, coords[i, ])^2)))
        }))
        sil_vals[i] <- if (max(a_i, b_i) == 0) 0 else (b_i - a_i) / max(a_i, b_i)
      }
      sil_mean <- round(mean(sil_vals), 3)
      
      tree      <- res.hcpc$call$t$tree
      dist_orig <- dist(res.pca$ind$coord)
      coph_corr <- round(cor(dist_orig, cophenetic(tree)), 3)
      
      df_indices <- data.frame(
        Indice         = c("Calinski-Harabasz (CH)", "Davies-Bouldin (DB)", "Silhouette moyenne", "Correlation cophenétique"),
        Valeur         = c(CH, DB, sil_mean, coph_corr),
        Objectif       = c("Maximiser", "Minimiser", "Maximiser (entre -1 et 1)", "Maximiser (entre 0 et 1)"),
        Seuils         = c("Plus c'est élevé, mieux c'est",
                           "Excellent < 0,5 | Acceptable 0,5-1,0 | Faible > 1,0",
                           ">= 0,70 excellent | 0,50-0,70 raisonnable | 0,25-0,50 faible | < 0,25 pas de structure",
                           "> 0,80 très bonne | 0,75-0,80 acceptable | < 0,75 médiocre"),
        Appreciation   = c(
          paste0("CH = ", CH, " (à comparer sur différents k)"),
          if (DB < 0.5) "Excellent" else if (DB < 1.0) "Acceptable" else "Faible",
          if (sil_mean >= 0.7) "Excellent" else if (sil_mean >= 0.5) "Raisonnable" else if (sil_mean >= 0.25) "Faible" else "Pas de structure",
          if (coph_corr >= 0.80) "Très bonne représentation" else if (coph_corr >= 0.75) "Acceptable" else "Médiocre"
        )
      )
      
      # Affectation clusters
      df_clusters <- data.frame(
        Individual = rownames(res.hcpc$data.clust),
        Cluster    = as.character(res.hcpc$data.clust$clust)
      )
      
      list(
        indices_validation = df_indices,
        affectation_clusters = df_clusters
      )
    }, error = function(e) NULL)
  }
  
  # Téléchargement du de l'graphique ACP avec dimensions automatiques
  output$downloadPcaPlot <- downloadHandler(
    filename = function() paste0("acp_", Sys.Date(), ".", input$pcaPlot_format),
    content = function(file) {
      dpi       <- input$pcaPlot_dpi
      auto_dims <- calculate_dimensions_from_dpi(dpi, 25, 20)
      p <- withCallingHandlers(
        suppressMessages(createPcaPlot(pcaResultReactive())),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      withCallingHandlers(
        suppressWarnings(ggsave(file, plot = p, device = input$pcaPlot_format,
                                width = auto_dims$width, height = auto_dims$height,
                                dpi = dpi, units = "cm")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
    }
  )
  
  # Téléchargement donnees ACP - Excel
  output$downloadPcaDataXlsx <- downloadHandler(
    filename = function() {
      paste0("acp_resultats_", Sys.Date(), ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      # Utiliser pcaDataframes() ou values$pcaDataframes
      dfs <- if (!is.null(values$pcaDataframes)) {
        values$pcaDataframes
      } else {
        pcaDataframes()
      }
      
      if (is.null(dfs)) {
        showNotification("Erreur : aucune donnee ACP disponible pour le Téléchargement", type = "error")
        return(NULL)
      }
      
      tryCatch({
        wb <- createWorkbook()
        
        addWorksheet(wb, "Valeurs_propres")
        writeData(wb, "Valeurs_propres", dfs$eigenvalues)
        
        addWorksheet(wb, "Coordonnees_individus")
        writeData(wb, "Coordonnees_individus", dfs$ind_coords)
        
        addWorksheet(wb, "Contributions_individus")
        writeData(wb, "Contributions_individus", dfs$ind_contrib)
        
        addWorksheet(wb, "Cos2_individus")
        writeData(wb, "Cos2_individus", dfs$ind_cos2)
        
        addWorksheet(wb, "Coordonnees_variables")
        writeData(wb, "Coordonnees_variables", dfs$var_coords)
        
        addWorksheet(wb, "Contributions_variables")
        writeData(wb, "Contributions_variables", dfs$var_contrib)
        
        addWorksheet(wb, "Cos2_variables")
        writeData(wb, "Cos2_variables", dfs$var_cos2)
        
        addWorksheet(wb, "Correlations_variables")
        writeData(wb, "Correlations_variables", dfs$var_cor)
        
        saveWorkbook(wb, file, overwrite = TRUE)
        showNotification("Fichier Excel ACP telecharge avec succes!", type = "message")
      }, error = function(e) {
        showNotification(paste("Erreur lors du Téléchargement Excel :", e$message), type = "error")
      })
    }
  )
  
  # Téléchargement donnees ACP - CSV
  output$downloadPcaDataCsv <- downloadHandler(
    filename = function() {
      paste0("acp_resultats_", Sys.Date(), ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      # Utiliser pcaDataframes() ou values$pcaDataframes
      dfs <- if (!is.null(values$pcaDataframes)) {
        values$pcaDataframes
      } else {
        pcaDataframes()
      }
      
      if (is.null(dfs)) {
        showNotification("Erreur : aucune donnee ACP disponible pour le Téléchargement", type = "error")
        return(NULL)
      }
      
      tryCatch({
        temp_dir <- tempdir()
        csv_files <- c()
        
        # Nettoyer les fichiers CSV 
        old_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
        if (length(old_files) > 0) {
          file.remove(old_files)
        }
        
        write_csv_utf8(dfs$eigenvalues, file.path(temp_dir, "valeurs_propres.csv"))
        csv_files <- c(csv_files, "valeurs_propres.csv")
        
        write_csv_utf8(dfs$ind_coords, file.path(temp_dir, "coordonnees_individus.csv"))
        csv_files <- c(csv_files, "coordonnees_individus.csv")
        
        write_csv_utf8(dfs$ind_contrib, file.path(temp_dir, "contributions_individus.csv"))
        csv_files <- c(csv_files, "contributions_individus.csv")
        
        write_csv_utf8(dfs$ind_cos2, file.path(temp_dir, "cos2_individus.csv"))
        csv_files <- c(csv_files, "cos2_individus.csv")
        
        write_csv_utf8(dfs$var_coords, file.path(temp_dir, "coordonnees_variables.csv"))
        csv_files <- c(csv_files, "coordonnees_variables.csv")
        
        write_csv_utf8(dfs$var_contrib, file.path(temp_dir, "contributions_variables.csv"))
        csv_files <- c(csv_files, "contributions_variables.csv")
        
        write_csv_utf8(dfs$var_cos2, file.path(temp_dir, "cos2_variables.csv"))
        csv_files <- c(csv_files, "cos2_variables.csv")
        
        write_csv_utf8(dfs$var_cor, file.path(temp_dir, "correlations_variables.csv"))
        csv_files <- c(csv_files, "correlations_variables.csv")
        
        zip(file, file.path(temp_dir, csv_files), flags = "-j")
        showNotification("Fichiers CSV ACP telecharges avec succes!", type = "message")
      }, error = function(e) {
        showNotification(paste("Erreur lors du Téléchargement CSV :", e$message), type = "error")
      })
    }
  )
  
  
  # HCPC (Classification Hierarchique sur Composantes Principales)
  
  hcpcResultReactive <- reactive({
    req(pcaResultReactive(), input$hcpcClusters)
    
    tryCatch({
      res.pca <- pcaResultReactive()
      res.hcpc <- HCPC(res.pca, nb.clust = input$hcpcClusters, graph = FALSE)
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
  
  # DATAFRAMES DES RESULTATS HCPC 
  hcpcDataframes <- reactive({
    req(hcpcResultReactive())
    res.hcpc <- hcpcResultReactive()
    
    tryCatch({
      # Fonction helper pour valider un element de resultat HCPC
      is_valid_hcpc_element <- function(elem) {
        # Retourne TRUE si l'element est valide pour traitement
        if (is.null(elem)) return(FALSE)
        
        # Verifier si c'est un dataframe ou peut etre converti
        if (!is.data.frame(elem)) {
          # Tenter conversion si c'est une matrice
          if (is.matrix(elem)) {
            elem <- as.data.frame(elem)
          } else {
            return(FALSE)
          }
        }
        
        # Verifier qu'il y a des lignes
        if (nrow(elem) == 0) return(FALSE)
        
        return(TRUE)
      }
      
      # Affectation des clusters
      cluster_assign_df <- data.frame(
        Individual = rownames(res.hcpc$data.clust),
        Cluster = as.character(res.hcpc$data.clust$clust),
        stringsAsFactors = FALSE
      )
      
      # Description variables par cluster
      desc_var_df <- NULL
      if (!is.null(res.hcpc$desc.var) && !is.null(res.hcpc$desc.var$quanti)) {
        desc_var_list <- list()
        for (i in seq_along(res.hcpc$desc.var$quanti)) {
          elem <- res.hcpc$desc.var$quanti[[i]]
          
          if (is_valid_hcpc_element(elem)) {
            df <- as.data.frame(elem)
            df <- cbind(Cluster = i, Variable = rownames(df), df, stringsAsFactors = FALSE)
            rownames(df) <- NULL
            desc_var_list[[length(desc_var_list) + 1]] <- df
          }
        }
        if (length(desc_var_list) > 0) {
          desc_var_df <- do.call(rbind, desc_var_list)
        }
      }
      
      # Description axes par cluster
      desc_axes_df <- NULL
      if (!is.null(res.hcpc$desc.axes) && !is.null(res.hcpc$desc.axes$quanti)) {
        desc_axes_list <- list()
        for (i in seq_along(res.hcpc$desc.axes$quanti)) {
          elem <- res.hcpc$desc.axes$quanti[[i]]
          
          if (is_valid_hcpc_element(elem)) {
            df <- as.data.frame(elem)
            df <- cbind(Cluster = i, Axe = rownames(df), df, stringsAsFactors = FALSE)
            rownames(df) <- NULL
            desc_axes_list[[length(desc_axes_list) + 1]] <- df
          }
        }
        if (length(desc_axes_list) > 0) {
          desc_axes_df <- do.call(rbind, desc_axes_list)
        }
      }
      
      # Parangons (individus typiques)
      parangons_df <- NULL
      if (!is.null(res.hcpc$desc.ind) && !is.null(res.hcpc$desc.ind$para)) {
        parangons_list <- list()
        for (i in seq_along(res.hcpc$desc.ind$para)) {
          elem <- res.hcpc$desc.ind$para[[i]]
          
          if (is_valid_hcpc_element(elem)) {
            df <- as.data.frame(elem)
            df <- cbind(Cluster = i, Individual = rownames(df), df, stringsAsFactors = FALSE)
            rownames(df) <- NULL
            parangons_list[[length(parangons_list) + 1]] <- df
          }
        }
        if (length(parangons_list) > 0) {
          parangons_df <- do.call(rbind, parangons_list)
        }
      }
      
      # Individus distants
      dist_df <- NULL
      if (!is.null(res.hcpc$desc.ind) && !is.null(res.hcpc$desc.ind$dist)) {
        dist_list <- list()
        for (i in seq_along(res.hcpc$desc.ind$dist)) {
          elem <- res.hcpc$desc.ind$dist[[i]]
          
          if (is_valid_hcpc_element(elem)) {
            df <- as.data.frame(elem)
            df <- cbind(Cluster = i, Individual = rownames(df), df, stringsAsFactors = FALSE)
            rownames(df) <- NULL
            dist_list[[length(dist_list) + 1]] <- df
          }
        }
        if (length(dist_list) > 0) {
          dist_df <- do.call(rbind, dist_list)
        }
      }
      
      result <- list(
        cluster_assignment = cluster_assign_df,
        desc_variables = desc_var_df,
        desc_axes = desc_axes_df,
        parangons = parangons_df,
        distant_individuals = dist_df
      )
      
      return(result)
      
    }, error = function(e) {
      showNotification(paste("Erreur creation dataframes HCPC :", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
  
  # Stocker les dataframes de HCPC dans values$ pour un acces fiable
  observe({
    req(hcpcResultReactive())
    
    # Attendre un peu pour laisser le temps au reactive de s'exécuter complètement
    isolate({
      tryCatch({
        dfs <- hcpcDataframes()
        
        if (!is.null(dfs)) {
          # Vérifier qu'au moins cluster_assignment existe
          if (!is.null(dfs$cluster_assignment) && nrow(dfs$cluster_assignment) > 0) {
            values$hcpcDataframes <- dfs
            
            # Message de succès détaillé
            n_clusters <- length(unique(dfs$cluster_assignment$Cluster))
            n_individus <- nrow(dfs$cluster_assignment)
            
            msg <- paste0("HCPC: ", n_individus, " individus classés en ", n_clusters, " clusters")
            showNotification(msg, type = "message", duration = 3)
          } else {
            showNotification("Avertissement: HCPC dataframes créés mais vides", type = "warning", duration = 5)
          }
        } else {
          showNotification("Erreur: Impossible de créer les dataframes HCPC", type = "error", duration = 5)
        }
      }, error = function(e) {
        showNotification(paste("Erreur stockage HCPC:", e$message), type = "error", duration = 5)
      })
    })
  })
  
  createHcpcDendPlot <- function(res.hcpc) {
    
    dend_title <- if (!is.null(input$hcpcDendTitle) && input$hcpcDendTitle != "") {
      input$hcpcDendTitle
    } else {
      "Dendrogramme HCPC"
    }
    
    # Générer les mêmes couleurs que pour la carte des clusters
    n_clusters <- length(unique(res.hcpc$data.clust$clust))
    cluster_colors <- generate_distinct_colors(n_clusters)
    
    # Calculer une taille de texte adaptative selon le nombre d'individus
    n_individus <- nrow(res.hcpc$data.clust)
    cex_labels <- if (n_individus <= 20) {
      0.7
    } else if (n_individus <= 50) {
      0.5
    } else if (n_individus <= 100) {
      0.4
    } else {
      0.3
    }
    
    # Créer le dendrogramme avec les couleurs personnalisées
    p_dend <- fviz_dend(res.hcpc,
                        cex = cex_labels,
                        palette = cluster_colors,
                        rect = TRUE,
                        rect_fill = TRUE,
                        rect_border = "jco",
                        main = dend_title,
                        sub = paste("Nombre de clusters:", n_clusters),
                        labels_track_height = 0.8,  
                        ggtheme = theme_minimal())
    
    # Améliorer l'affichage des labels
    p_dend <- p_dend + 
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        plot.margin = margin(10, 10, 50, 10),  # Marges : top, right, bottom, left
        axis.title.x = element_blank()
      )
    
    # Si trop d'individus (>100), masquer les labels pour éviter la surcharge
    if (n_individus > 100) {
      p_dend <- p_dend + 
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        labs(caption = paste("Note: Labels masqués (", n_individus, "individus). 
                           Consultez les résultats détaillés pour l'affectation complète."))
    }
    
    return(p_dend)
  }
  
  # Selectin des axes pour le HCPC
  output$hcpcAxisXSelect <- renderUI({
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims <- ncol(res.pca$ind$coord)
    
    selectInput("hcpcAxisX", "Axe X:",
                choices = setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = 1)
  })
  
  output$hcpcAxisYSelect <- renderUI({
    req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims <- ncol(res.pca$ind$coord)
    
    selectInput("hcpcAxisY", "Axe Y:",
                choices = setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = min(2, n_dims))
  })
  
  createHcpcClusterPlot <- function(res.hcpc, res.pca) {
    
    # Axes sélectionnés (par défaut 1 et 2)
    axis_x <- if (!is.null(input$hcpcAxisX)) as.numeric(input$hcpcAxisX) else 1
    axis_y <- if (!is.null(input$hcpcAxisY)) as.numeric(input$hcpcAxisY) else 2
    
    cluster_title <- if (!is.null(input$hcpcClusterTitle) && input$hcpcClusterTitle != "") {
      input$hcpcClusterTitle
    } else {
      "Carte des clusters HCPC"
    }
    
    eigenvals <- get_eigenvalue(res.pca)
    pc_x_var <- round(eigenvals[axis_x, "variance.percent"], 1)
    pc_y_var <- round(eigenvals[axis_y, "variance.percent"], 1)
    
    x_label <- if (!is.null(input$hcpcClusterXLabel) && input$hcpcClusterXLabel != "") {
      input$hcpcClusterXLabel
    } else {
      paste0("PC", axis_x, " (", pc_x_var, "%)")
    }
    
    y_label <- if (!is.null(input$hcpcClusterYLabel) && input$hcpcClusterYLabel != "") {
      input$hcpcClusterYLabel
    } else {
      paste0("PC", axis_y, " (", pc_y_var, "%)")
    }
    
    # Générer des couleurs distinctives pour tous les clusters
    n_clusters <- length(unique(res.hcpc$data.clust$clust))
    cluster_colors <- generate_distinct_colors(n_clusters)
    
    p_cluster <- fviz_cluster(res.hcpc,
                              axes = c(axis_x, axis_y),
                              repel = TRUE,
                              show.clust.cent = TRUE,
                              palette = cluster_colors,
                              ggtheme = theme_minimal(),
                              main = cluster_title) +
      labs(x = x_label, y = y_label) +
      theme(legend.position = "right",
            legend.title = element_markdown(size = 10, face = "bold"),
            legend.text = element_text(size = 9))
    
    if (!is.null(input$hcpcCenterAxes) && input$hcpcCenterAxes) {
      coords <- res.pca$ind$coord[, c(axis_x, axis_y)]
      max_range <- max(abs(range(coords, na.rm = TRUE)))
      p_cluster <- p_cluster + xlim(-max_range, max_range) + ylim(-max_range, max_range)
    }
    
    return(p_cluster)
  }
  
  output$hcpcDendPlot <- renderPlotly({
    req(values$pcaResult)
    p_dend <- suppressWarnings(suppressMessages(createHcpcDendPlot(hcpcResultReactive())))
    suppressWarnings({
      ggplotly(p_dend) %>% layout(margin = list(b = 100))
    })
  })
  
  output$hcpcClusterPlot <- renderPlotly({
    req(values$pcaResult)
    p_cluster <- suppressWarnings(suppressMessages(createHcpcClusterPlot(hcpcResultReactive(), pcaResultReactive())))
    suppressWarnings({
      ggplotly(p_cluster) %>% layout(showlegend = TRUE)
    })
  })
  
  output$hcpcSummary <- renderPrint({
    req(hcpcResultReactive())
    res.hcpc <- hcpcResultReactive()
    
    # Nombre de décimales (seulement si l'utilisateur a coché l'option)
    use_round <- !is.null(input$hcpcRoundResults) && input$hcpcRoundResults
    dec <- if (use_round && !is.null(input$hcpcDecimals)) input$hcpcDecimals else 4
    
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
          print(round(res.hcpc$desc.var$quanti[[i]], dec))
        }
      }
    }
    
    if (!is.null(res.hcpc$desc.axes$quanti)) {
      cat("\n=== DESCRIPTION DES AXES PAR CLUSTER ===\n")
      for (i in 1:length(res.hcpc$desc.axes$quanti)) {
        if (!is.null(res.hcpc$desc.axes$quanti[[i]])) {
          cat("\n--- CLUSTER", i, " - AXES ---\n")
          print(round(res.hcpc$desc.axes$quanti[[i]], dec))
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
  
  
  # HCPC - MÉTRIQUES DE VALIDATION SUPPLÉMENTAIRES
  
  
  # Helper: extraire les données numériques des clusters HCPC
  hcpcValidationData <- reactive({
    req(hcpcResultReactive(), pcaResultReactive())
    res.hcpc <- hcpcResultReactive()
    res.pca  <- pcaResultReactive()
    tryCatch({
      # Coordonnées des individus dans l'espace ACP
      coords    <- res.pca$ind$coord
      clusters  <- as.integer(res.hcpc$data.clust$clust)
      k         <- length(unique(clusters))
      list(coords = coords, clusters = clusters, k = k,
           hcpc = res.hcpc, pca = res.pca)
    }, error = function(e) NULL)
  })
  
  # -- 1. Graphique des hauteurs de fusion (indices d'agrégation) --
  createHcpcHeightsPlot <- function(res.hcpc) {
    tree        <- res.hcpc$call$t$tree
    heights     <- rev(tree$height)
    n_show      <- min(length(heights), 20)
    heights_sub <- heights[1:n_show]
    
    df_h <- data.frame(
      Etape   = 1:n_show,
      Hauteur = heights_sub,
      Saut    = c(NA, diff(heights_sub))
    )
    df_h_valid  <- df_h[!is.na(df_h$Saut), ]
    best_step   <- df_h_valid$Etape[which.max(df_h_valid$Saut)]
    n_clust_opt <- best_step
    
    ggplot(df_h, aes(x = Etape, y = Hauteur)) +
      geom_line(color = "#2E86AB", linewidth = 1.3) +
      geom_point(aes(color = Etape == best_step), size = 4) +
      scale_color_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#27ae60"),
                         labels = c("TRUE" = "Coupure suggérée", "FALSE" = "Autre fusion"),
                         name = NULL) +
      geom_vline(xintercept = best_step, linetype = "dashed", color = "#e74c3c", size = 0.9) +
      annotate("text", x = best_step + 0.2, y = max(heights_sub, na.rm = TRUE) * 0.95,
               label = paste0("Coupure suggérée\n(", n_clust_opt, " clusters)"),
               hjust = 0, color = "#e74c3c", size = 3.5, fontface = "bold") +
      scale_x_continuous(breaks = 1:n_show) +
      labs(
        title    = "Graphique des indices d'agrégation (hauteurs de fusion)",
        subtitle = "Un saut important sur la courbe indique le nombre optimal de clusters (règle du coude)",
        x        = "Nombre de clusters (ordre de fusion, de k le plus grand au plus petit)",
        y        = "Hauteur d'agrégation",
        caption  = "La coupure optimale (point rouge) correspond au plus grand saut entre deux fusions successives."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title       = element_markdown(hjust = 0.5, face = "bold", size = 13, color = "#2c3e50"),
        plot.subtitle    = element_text(hjust = 0.5, color = "#555", size = 10),
        legend.position  = "bottom",
        panel.grid.minor = element_blank()
      )
  }
  
  output$hcpcHeightsPlot <- renderPlot({
    req(hcpcResultReactive())
    tryCatch(
      createHcpcHeightsPlot(hcpcResultReactive()),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Erreur :", e$message), cex = 1, col = "#e74c3c")
      }
    )
  })
  
  output$downloadHcpcHeightsPlot <- downloadHandler(
    filename = function() paste0("hcpc_hauteurs_fusion_", Sys.Date(), ".", input$hcpcHeights_format),
    content = function(file) {
      dpi  <- input$hcpcHeights_dpi
      auto <- calculate_dimensions_from_dpi(dpi, base_width_cm = 25, base_height_cm = 18)
      p    <- createHcpcHeightsPlot(hcpcResultReactive())
      suppressWarnings(ggsave(file, plot = p, device = input$hcpcHeights_format,
                              width = auto$width, height = auto$height, dpi = dpi, units = "cm"))
    }
  )
  
  
  # -- 2. Métriques de validation des clusters (CH, DB, Silhouette, Cophénétique) --
  output$hcpcMetricsUI <- renderUI({
    vd <- hcpcValidationData()
    req(vd)
    tryCatch({
      coords   <- vd$coords
      clusters <- vd$clusters
      k        <- vd$k
      res.hcpc <- vd$hcpc
      res.pca  <- vd$pca
      
      # ---- Calinski-Harabasz ----
      n <- nrow(coords)
      grand_mean <- colMeans(coords)
      
      SS_B <- 0
      SS_W <- 0
      for (cl in unique(clusters)) {
        idx <- which(clusters == cl)
        nc  <- length(idx)
        cm  <- colMeans(coords[idx, , drop = FALSE])
        SS_B <- SS_B + nc * sum((cm - grand_mean)^2)
        for (i in idx) {
          SS_W <- SS_W + sum((coords[i, ] - cm)^2)
        }
      }
      CH <- if (SS_W > 0) (SS_B / (k - 1)) / (SS_W / (n - k)) else NA
      CH <- round(CH, 2)
      
      # ---- Davies-Bouldin ----
      cluster_list <- lapply(unique(sort(clusters)), function(cl) coords[clusters == cl, , drop = FALSE])
      centroids    <- lapply(cluster_list, colMeans)
      dispersions  <- sapply(seq_along(cluster_list), function(i) {
        mean(sqrt(rowSums(sweep(cluster_list[[i]], 2, centroids[[i]])^2)))
      })
      DB_vals <- sapply(seq_along(centroids), function(i) {
        max(sapply(seq_along(centroids), function(j) {
          if (i == j) return(0)
          d_ij <- sqrt(sum((centroids[[i]] - centroids[[j]])^2))
          if (d_ij == 0) return(0)
          (dispersions[i] + dispersions[j]) / d_ij
        }))
      })
      DB <- round(mean(DB_vals), 3)
      
      # ---- Silhouette ----
      sil_vals <- numeric(n)
      for (i in 1:n) {
        cl_i <- clusters[i]
        same <- which(clusters == cl_i)
        same <- same[same != i]
        a_i  <- if (length(same) == 0) 0 else mean(sqrt(rowSums(sweep(coords[same, , drop = FALSE], 2, coords[i, ])^2)))
        b_i  <- min(sapply(unique(clusters[clusters != cl_i]), function(cl) {
          other <- which(clusters == cl)
          mean(sqrt(rowSums(sweep(coords[other, , drop = FALSE], 2, coords[i, ])^2)))
        }))
        sil_vals[i] <- if (max(a_i, b_i) == 0) 0 else (b_i - a_i) / max(a_i, b_i)
      }
      sil_mean <- round(mean(sil_vals), 3)
      
      # ---- Corrélation cophénétique ----
      tree        <- res.hcpc$call$t$tree
      dist_orig   <- dist(res.pca$ind$coord)
      coph_dist   <- cophenetic(tree)
      coph_corr   <- round(cor(dist_orig, coph_dist), 3)
      
      # ---- Interprétations ----
      ch_interp <- div(
        style = "font-size: 11px; margin-top: 5px;",
        p(style = "margin: 0; color: #555; font-style: italic;",
          icon("info-circle"), " Maximiser cet indice. Plus il est élevé, meilleure est la séparation inter-classes par rapport à la compacité intra-classe.")
      )
      
      db_color <- if (DB < 0.5) "#27ae60" else if (DB < 1.0) "#f39c12" else "#dc3545"
      db_interp <- if (DB < 0.5) "Excellent (< 0,5) — clusters bien séparés et compacts"
      else if (DB < 1.0) "Acceptable (0,5 – 1,0) — séparation modérée"
      else "Faible (> 1,0) — clusters qui se chevauchent"
      
      sil_color <- if (sil_mean >= 0.7) "#27ae60" else if (sil_mean >= 0.5) "#3498db" 
      else if (sil_mean >= 0.25) "#f39c12" else "#dc3545"
      sil_interp <- if (sil_mean >= 0.7) "Excellente structure (>= 0,70)"
      else if (sil_mean >= 0.5) "Structure raisonnable (0,50 – 0,70)"
      else if (sil_mean >= 0.25) "Structure faible (0,25 – 0,50)"
      else "Pas de structure substantielle (< 0,25)"
      
      coph_color <- if (coph_corr >= 0.80) "#27ae60" else if (coph_corr >= 0.75) "#3498db" else "#dc3545"
      coph_interp <- if (coph_corr >= 0.80) "Très bonne représentation (>= 0,80) — dendrogramme fidèle"
      else if (coph_corr >= 0.75) "Représentation acceptable (0,75 – 0,80)"
      else "Représentation médiocre (< 0,75) — le dendrogramme déforme les distances"
      
      tagList(
        # CH
        div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; margin-bottom:12px; border:1px solid #dee2e6;",
            h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #3498db; padding-bottom:6px;",
               icon("chart-bar"), " Indice de Calinski-Harabasz (CH)"),
            p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
              "Ratio variance inter-classes / variance intra-classes. Un indice élevé indique une bonne séparation des groupes. À comparer sur plusieurs valeurs de k pour choisir le nombre optimal de clusters."),
            div(style = "text-align:center; padding:8px;",
                h3(style = "color:#2c3e50; margin:0; font-weight:bold;", CH)
            ),
            ch_interp
        ),
        # DB
        div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; margin-bottom:12px; border:1px solid #dee2e6;",
            h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #8e44ad; padding-bottom:6px;",
               icon("compress"), " Indice de Davies-Bouldin (DB)"),
            p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
              "Rapport moyen entre la dispersion intra-classe et la séparation inter-classes. À minimiser : un faible DB indique des clusters compacts et bien séparés."),
            div(style = "text-align:center; padding:8px;",
                h3(style = paste0("color:", db_color, "; margin:0; font-weight:bold;"), DB)
            ),
            div(style = paste0("margin-top:8px; padding:6px 10px; border-left:4px solid ", db_color, "; background:white; border-radius:0 4px 4px 0;"),
                p(style = paste0("margin:0; font-size:12px; color:", db_color, ";"), icon("check-circle"), " ", db_interp))
        ),
        # Silhouette
        div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; margin-bottom:12px; border:1px solid #dee2e6;",
            h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #27ae60; padding-bottom:6px;",
               icon("star"), " Silhouette moyenne"),
            p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
              "Mesure pour chaque individu son appartenance à son cluster par rapport au cluster voisin. Varie de -1 à 1. Une valeur positive élevée indique que l'individu est bien classé."),
            div(style = "text-align:center; padding:8px;",
                h3(style = paste0("color:", sil_color, "; margin:0; font-weight:bold;"), sil_mean)
            ),
            div(style = paste0("margin-top:8px; padding:6px 10px; border-left:4px solid ", sil_color, "; background:white; border-radius:0 4px 4px 0;"),
                p(style = paste0("margin:0; font-size:12px; color:", sil_color, ";"), icon("check-circle"), " ", sil_interp)),
            p(style = "font-size:11px; color:#888; margin-top:6px; margin-bottom:0;",
              "Seuils : >= 0,70 excellent | 0,50-0,70 raisonnable | 0,25-0,50 faible | < 0,25 pas de structure")
        ),
        # Cophénétique
        div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; border:1px solid #dee2e6;",
            h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #e67e22; padding-bottom:6px;",
               icon("project-diagram"), " Corrélation cophénétique"),
            p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
              "Corrélation entre les distances originales entre individus et les distances de fusion dans le dendrogramme. Indique si le dendrogramme représente fidèlement la structure des données."),
            div(style = "text-align:center; padding:8px;",
                h3(style = paste0("color:", coph_color, "; margin:0; font-weight:bold;"), coph_corr)
            ),
            div(style = paste0("margin-top:8px; padding:6px 10px; border-left:4px solid ", coph_color, "; background:white; border-radius:0 4px 4px 0;"),
                p(style = paste0("margin:0; font-size:12px; color:", coph_color, ";"), icon("check-circle"), " ", coph_interp)),
            p(style = "font-size:11px; color:#888; margin-top:6px; margin-bottom:0;",
              "Seuils : > 0,80 très bonne | 0,75 – 0,80 acceptable | < 0,75 médiocre")
        )
      )
    }, error = function(e) {
      div(class = "callout callout-danger",
          tags$p(tagList(icon("exclamation-triangle"), " Erreur calcul métriques HCPC : ", e$message)))
    })
  })
  
  # -- 3. Stabilité par sous-échantillonnage --
  output$hcpcStabilityUI <- renderUI({
    req(hcpcValidationData())
    vd <- hcpcValidationData()
    tryCatch({
      coords   <- vd$coords
      clusters <- vd$clusters
      k        <- vd$k
      n        <- nrow(coords)
      
      n_boot  <- 30
      prop    <- 0.8
      rand_indices <- numeric(n_boot)
      
      for (b in 1:n_boot) {
        idx_b   <- sample(1:n, size = floor(n * prop), replace = FALSE)
        sub_coords <- coords[idx_b, , drop = FALSE]
        d_sub   <- dist(sub_coords)
        hc_sub  <- hclust(d_sub, method = "ward.D2")
        clust_sub <- cutree(hc_sub, k = k)
        clust_ref <- clusters[idx_b]
        
        # Calcul d'un Rand index simplifié
        n_sub <- length(clust_sub)
        pairs <- combn(n_sub, 2)
        agree <- mean(apply(pairs, 2, function(p) {
          (clust_sub[p[1]] == clust_sub[p[2]]) == (clust_ref[p[1]] == clust_ref[p[2]])
        }))
        rand_indices[b] <- agree
      }
      
      mean_rand <- round(mean(rand_indices), 3)
      sd_rand   <- round(sd(rand_indices), 3)
      
      color_stab <- if (mean_rand >= 0.9) "#27ae60" else if (mean_rand >= 0.8) "#3498db" 
      else if (mean_rand >= 0.7) "#f39c12" else "#dc3545"
      stab_interp <- if (mean_rand >= 0.9) "Excellente stabilité (>= 0,90) — la partition est très reproductible."
      else if (mean_rand >= 0.8) "Bonne stabilité (0,80 – 0,90) — la partition est fiable."
      else if (mean_rand >= 0.7) "Stabilité modérée (0,70 – 0,80) — à interpréter avec prudence."
      else "Faible stabilité (< 0,70) — la partition est sensible à la composition de l'échantillon."
      
      div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; border:1px solid #dee2e6;",
          h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #16a085; padding-bottom:6px;",
             icon("sync"), " Stabilité par sous-échantillonnage (Rand Index)"),
          p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
            paste0("Évaluation de la robustesse de la partition sur ", n_boot, " sous-échantillons aléatoires (",
                   round(prop * 100), "% des données). L'indice de Rand mesure la concordance entre la partition de référence et celle obtenue sur chaque sous-échantillon (0 = aucun accord, 1 = accord parfait).")),
          fluidRow(
            column(6,
                   div(style = "text-align:center; background:white; border-radius:6px; padding:10px; border:1px solid #dee2e6;",
                       p(style = "margin:0; font-size:11px; color:#888; text-transform:uppercase;", "Rand Index moyen"),
                       h3(style = paste0("margin:4px 0; font-weight:bold; color:", color_stab, ";"), mean_rand)
                   )
            ),
            column(6,
                   div(style = "text-align:center; background:white; border-radius:6px; padding:10px; border:1px solid #dee2e6;",
                       p(style = "margin:0; font-size:11px; color:#888; text-transform:uppercase;", "Ecart-type"),
                       h3(style = "margin:4px 0; font-weight:bold; color:#2c3e50;", sd_rand)
                   )
            )
          ),
          div(style = paste0("margin-top:10px; padding:8px 12px; border-left:4px solid ", color_stab, "; background:white; border-radius:0 4px 4px 0;"),
              p(style = paste0("margin:0; font-size:12px; color:", color_stab, ";"),
                icon("check-circle"), " ", stab_interp))
      )
    }, error = function(e) {
      div(style = "padding:10px; background:#f8d7da; border-radius:6px;",
          p(style = "margin:0; color:#721c24; font-size:12px;",
            icon("exclamation-triangle"), " Erreur stabilité : ", e$message))
    })
  })
  
  # Téléchargement graphique HCPC Dendrogramme avec dimensions automatiques
  output$downloadHcpcDendPlot <- downloadHandler(
    filename = function() paste0("hcpc_dendrogramme_", Sys.Date(), ".", input$hcpcDend_format),
    content = function(file) {
      dpi       <- input$hcpcDend_dpi
      auto_dims <- calculate_dimensions_from_dpi(dpi, 30, 20)
      p <- withCallingHandlers(
        suppressMessages(createHcpcDendPlot(hcpcResultReactive())),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      withCallingHandlers(
        suppressWarnings(ggsave(file, plot = p, device = input$hcpcDend_format,
                                width = auto_dims$width, height = auto_dims$height,
                                dpi = dpi, units = "cm")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
    }
  )
  
  # Téléchargement graphique HCPC Cluster avec dimensions automatiques
  output$downloadHcpcClusterPlot <- downloadHandler(
    filename = function() paste0("hcpc_clusters_", Sys.Date(), ".", input$hcpcCluster_format),
    content = function(file) {
      dpi       <- input$hcpcCluster_dpi
      auto_dims <- calculate_dimensions_from_dpi(dpi, 25, 20)
      p <- withCallingHandlers(
        suppressMessages(createHcpcClusterPlot(hcpcResultReactive(), pcaResultReactive())),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      withCallingHandlers(
        suppressWarnings(ggsave(file, plot = p, device = input$hcpcCluster_format,
                                width = auto_dims$width, height = auto_dims$height,
                                dpi = dpi, units = "cm")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
    }
  )
  
  # Téléchargement donnees HCPC 
  output$downloadHcpcDataXlsx <- downloadHandler(
    filename = function() {
      paste0("hcpc_resultats_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # S'assurer que l'extension est .xlsx
      if (!grepl("\\.xlsx$", file, ignore.case = TRUE)) {
        file <- paste0(tools::file_path_sans_ext(file), ".xlsx")
      }
      
      # Utiliser hcpcDataframes() ou values$hcpcDataframes
      dfs <- if (!is.null(values$hcpcDataframes)) {
        values$hcpcDataframes
      } else {
        hcpcDataframes()
      }
      
      if (is.null(dfs)) {
        showNotification("Erreur : aucune donnee HCPC disponible pour le Téléchargement", type = "error", duration = 10)
        return(NULL)
      }
      
      # Verifier que cluster_assignment existe et n'est pas vide
      if (is.null(dfs$cluster_assignment) || nrow(dfs$cluster_assignment) == 0) {
        showNotification("Erreur : donnees HCPC vides ou invalides", type = "error", duration = 10)
        return(NULL)
      }
      
      tryCatch({
        wb <- createWorkbook()
        
        # Ajouter cluster assignment (toujours present)
        addWorksheet(wb, "Affectation_clusters")
        writeData(wb, "Affectation_clusters", dfs$cluster_assignment)
        
        # Ajouter les autres feuilles seulement si elles existent
        if (!is.null(dfs$desc_variables) && nrow(dfs$desc_variables) > 0) {
          addWorksheet(wb, "Desc_variables")
          writeData(wb, "Desc_variables", dfs$desc_variables)
        }
        
        if (!is.null(dfs$desc_axes) && nrow(dfs$desc_axes) > 0) {
          addWorksheet(wb, "Desc_axes")
          writeData(wb, "Desc_axes", dfs$desc_axes)
        }
        
        if (!is.null(dfs$parangons) && nrow(dfs$parangons) > 0) {
          addWorksheet(wb, "Parangons")
          writeData(wb, "Parangons", dfs$parangons)
        }
        
        if (!is.null(dfs$distant_individuals) && nrow(dfs$distant_individuals) > 0) {
          addWorksheet(wb, "Individus_eloignes")
          writeData(wb, "Individus_eloignes", dfs$distant_individuals)
        }
        
        # Sauvegarder avec overwrite
        saveWorkbook(wb, file, overwrite = TRUE)
        
        # Verifier que le fichier existe
        if (file.exists(file)) {
          showNotification("Fichier Excel HCPC telecharge avec succes!", type = "message", duration = 3)
        } else {
          showNotification("Erreur : fichier non cree", type = "error", duration = 10)
        }
        
      }, error = function(e) {
        showNotification(paste("Erreur lors du Téléchargement Excel :", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # Téléchargement donnees HCPC 
  output$downloadHcpcDataCsv <- downloadHandler(
    filename = function() {
      paste0("hcpc_resultats_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # S'assurer que l'extension est .zip
      if (!grepl("\\.zip$", file, ignore.case = TRUE)) {
        file <- paste0(tools::file_path_sans_ext(file), ".zip")
      }
      
      # Utiliser hcpcDataframes() ou values$hcpcDataframes
      dfs <- if (!is.null(values$hcpcDataframes)) {
        values$hcpcDataframes
      } else {
        hcpcDataframes()
      }
      
      if (is.null(dfs)) {
        showNotification("Erreur : aucune donnee HCPC disponible pour le Téléchargement", type = "error", duration = 10)
        return(NULL)
      }
      
      # Verifier que cluster_assignment existe et n'est pas vide
      if (is.null(dfs$cluster_assignment) || nrow(dfs$cluster_assignment) == 0) {
        showNotification("Erreur : donnees HCPC vides ou invalides", type = "error", duration = 10)
        return(NULL)
      }
      
      tryCatch({
        temp_dir <- tempdir()
        csv_files <- c()
        
        # Nettoyer les fichiers CSV precedents dans tempdir
        old_files <- list.files(temp_dir, pattern = "^(affectation|desc|parangons|individus).*\\.csv$", full.names = TRUE)
        if (length(old_files) > 0) {
          file.remove(old_files)
        }
        
        # Toujours inclure cluster assignment
        csv_path <- file.path(temp_dir, "affectation_clusters.csv")
        write_csv_utf8(dfs$cluster_assignment, csv_path)
        csv_files <- c(csv_files, "affectation_clusters.csv")
        
        # Ajouter les autres fichiers s'ils existent
        if (!is.null(dfs$desc_variables) && nrow(dfs$desc_variables) > 0) {
          csv_path <- file.path(temp_dir, "desc_variables.csv")
          write_csv_utf8(dfs$desc_variables, csv_path)
          csv_files <- c(csv_files, "desc_variables.csv")
        }
        
        if (!is.null(dfs$desc_axes) && nrow(dfs$desc_axes) > 0) {
          csv_path <- file.path(temp_dir, "desc_axes.csv")
          write_csv_utf8(dfs$desc_axes, csv_path)
          csv_files <- c(csv_files, "desc_axes.csv")
        }
        
        if (!is.null(dfs$parangons) && nrow(dfs$parangons) > 0) {
          csv_path <- file.path(temp_dir, "parangons.csv")
          write_csv_utf8(dfs$parangons, csv_path)
          csv_files <- c(csv_files, "parangons.csv")
        }
        
        if (!is.null(dfs$distant_individuals) && nrow(dfs$distant_individuals) > 0) {
          csv_path <- file.path(temp_dir, "individus_eloignes.csv")
          write_csv_utf8(dfs$distant_individuals, csv_path)
          csv_files <- c(csv_files, "individus_eloignes.csv")
        }
        
        # Créer le ZIP
        zip(file, file.path(temp_dir, csv_files), flags = "-j")
        
        # Vérifier que le fichier existe
        if (file.exists(file)) {
          showNotification(paste0("Fichiers CSV HCPC (", length(csv_files), " fichiers) telecharges avec succes!"), 
                           type = "message", duration = 3)
        } else {
          showNotification("Erreur : fichier ZIP non cree", type = "error", duration = 10)
        }
        
      }, error = function(e) {
        showNotification(paste("Erreur lors du Téléchargement CSV :", e$message), type = "error", duration = 10)
      })
    }
  )
  
  
  # SECTION 3: AFD (Analyse Factorielle Discriminante) 
  
  output$afdFactorSelect <- renderUI({
    req(values$filteredData)
    df <- values$filteredData
    
    # ── Regrouper les colonnes par type avec étiquettes claires 
    build_group_choices <- function(df) {
      cols_factor  <- names(df)[sapply(df, is.factor)]
      cols_char    <- names(df)[sapply(df, is.character)]
      cols_date    <- names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
      # Numériques avec ≤ 30 valeurs uniques → utilisables comme groupes
      cols_num_few <- names(df)[sapply(df, function(x) {
        is.numeric(x) && length(unique(na.omit(x))) <= 30
      })]
      # Logiques
      cols_logi    <- names(df)[sapply(df, is.logical)]
      
      groups <- list()
      if (length(cols_factor)  > 0) groups[["Facteur"]]                             <- cols_factor
      if (length(cols_char)    > 0) groups[["Texte / Caractère"]]                   <- cols_char
      if (length(cols_date)    > 0) groups[["Date"]]                                <- cols_date
      if (length(cols_num_few) > 0) groups[["Numérique (≤ 30 niveaux))"]]           <- cols_num_few
      if (length(cols_logi)    > 0) groups[["Logique (TRUE/FALSE)"]]                <- cols_logi
      groups
    }
    
    grouped <- build_group_choices(df)
    
    if (length(grouped) == 0) {
      return(div(
        style = "padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;",
        p(style = "margin: 0; color: #721c24;",
          icon("exclamation-triangle"),
          " Aucune colonne utilisable comme variable discriminante.")
      ))
    }
    
    # Valeur par défaut : premier facteur/char, sinon premier disponible
    all_cols <- unlist(grouped, use.names = FALSE)
    default_col <- if (!is.null(grouped[["Facteur"]]))     grouped[["Facteur"]][1]   else
                   if (!is.null(grouped[["Texte / Caractère"]])) grouped[["Texte / Caractère"]][1] else
                   all_cols[1]
    
    tagList(
      selectInput(
        "afdFactor",
        label = div(icon("bullseye"), " Variable à discriminer :"),
        choices  = grouped,
        selected = default_col
      ),
      uiOutput("afdFactorTypeHint")
    )
  })
  
  # Hint sur le type de la variable sélectionnée
  output$afdFactorTypeHint <- renderUI({
    req(values$filteredData, input$afdFactor)
    col <- values$filteredData[[input$afdFactor]]
    n_lvl <- length(unique(na.omit(col)))
    type_str <- if (is.factor(col))      paste0("Facteur — ", n_lvl, " niveaux")
                else if (is.character(col)) paste0("Texte — ", n_lvl, " valeurs uniques")
                else if (inherits(col,"Date") || inherits(col,"POSIXt")) paste0("Date — ", n_lvl, " valeurs uniques")
                else if (is.numeric(col))  paste0("Numérique — converti en facteur (", n_lvl, " niveaux)")
                else if (is.logical(col))  "Logique — 2 niveaux (TRUE / FALSE)"
                else paste0("Type : ", class(col)[1], " — ", n_lvl, " niveaux")
    
    col_bg <- if (is.factor(col)) "#28a745"
               else if (is.character(col)) "#17a2b8"
               else if (is.numeric(col)) "#ffc107"
               else if (inherits(col,"Date")||inherits(col,"POSIXt")) "#6f42c1"
               else "#6c757d"
    col_tc <- if (is.numeric(col)) "#212529" else "white"
    
    div(style = "margin-top: 4px;",
        tags$span(
          style = paste0("display:inline-block; padding:3px 8px; border-radius:4px; ",
                         "background:", col_bg, "; color:", col_tc, "; font-size:11px; font-weight:500;"),
          icon("info-circle"), " ", type_str
        ))
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
  
  output$afdQualiSupSelect <- renderUI({
    req(values$filteredData, input$afdFactor)
    if (is.null(values$filteredData) || is.null(input$afdFactor)) {
      return(NULL)
    }
    
    fac_cols <- get_categorical_cols(values$filteredData)
    
    # Exclure le facteur discriminant et le groupe de moyennes
    fac_cols <- fac_cols[fac_cols != input$afdFactor]
    if (!is.null(input$afdMeansGroup) && !is.null(input$afdUseMeans) && input$afdUseMeans) {
      fac_cols <- fac_cols[fac_cols != input$afdMeansGroup]
    }
    
    if (length(fac_cols) == 0) {
      return(p(style = "margin: 10px 0; font-size: 11px; color: #6c757d; font-style: italic;",
               icon("info-circle"), 
               " Aucune variable categorielle supplementaire disponible."))
    }
    
    pickerInput(
      inputId = "afdQualiSup",
      label = "Variables categorielles supplementaires (optionnel):",
      choices = fac_cols,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  # ── Panel colinéarité AFD 
  output$afdCollinearityPanel <- renderUI({
    req(values$filteredData, input$afdVars)
    if (length(input$afdVars) < 2) return(NULL)
    
    afd_data <- tryCatch(
      { d <- values$filteredData[, input$afdVars, drop = FALSE]
        d[, sapply(d, is.numeric), drop = FALSE] },
      error = function(e) NULL
    )
    if (is.null(afd_data) || ncol(afd_data) < 2) return(NULL)
    
    R_mat <- safe_cor(afd_data)
    if (is.null(R_mat) || anyNA(R_mat)) return(NULL)
    
    # Paires avec |cor| >= 0.80
    pairs_high <- list()
    for (i in seq_len(nrow(R_mat))) {
      for (j in seq_len(ncol(R_mat))) {
        if (i < j && abs(R_mat[i,j]) >= 0.80)
          pairs_high[[length(pairs_high)+1]] <- list(
            v1 = rownames(R_mat)[i], v2 = colnames(R_mat)[j],
            cor = round(R_mat[i,j], 3))
      }
    }
    if (length(pairs_high) == 0) return(NULL)
    
    # VIF
    vif_vals <- tryCatch({
      if (ncol(afd_data) >= 3) {
        afd_nzv <- remove_zero_var_cols(afd_data)
        v <- sapply(seq_len(ncol(afd_nzv)), function(i) {
          y <- afd_nzv[[i]]; x <- remove_zero_var_cols(afd_nzv[,-i, drop=FALSE])
          if (ncol(x) == 0) return(Inf)
          r2 <- tryCatch(suppressWarnings(summary(lm(y~., data=x))$r.squared), error=function(e) NA)
          if (is.na(r2)||r2>=1) Inf else 1/(1-r2)
        })
        setNames(v, names(afd_nzv))
      } else rep(NA, ncol(afd_data))
    }, error=function(e) rep(NA, ncol(afd_data)))
    
    high_vif    <- names(vif_vals[!is.na(vif_vals) & is.finite(vif_vals) & vif_vals > 5])
    perfect_col <- unique(unlist(lapply(pairs_high, function(p) if (abs(p$cor)>0.9999) p$v2 else NULL)))
    suggest_remove <- unique(c(perfect_col, names(vif_vals[!is.na(vif_vals) & is.finite(vif_vals) & vif_vals>10])))
    
    sev_col   <- if (length(perfect_col)>0) "#dc3545" else if (length(high_vif)>0) "#fd7e14" else "#ffc107"
    sev_label <- if (length(perfect_col)>0) "Colinéarité parfaite" else if (length(high_vif)>0) "Colinéarité forte (VIF > 5)" else "Colinéarité modérée (|r| ≥ 0.80)"
    
    tagList(div(
      style = paste0("border: 2px solid ", sev_col, "; border-radius: 6px; padding: 12px; margin: 8px 0;"),
      div(style = paste0("display:flex; align-items:center; gap:8px; margin-bottom:10px; color:", sev_col, ";"),
          icon("exclamation-triangle"), tags$strong(sev_label)),
      tags$small(style="color:#495057;font-weight:bold;display:block;margin-bottom:4px;", "Paires colinéaires :"),
      tags$table(class="table table-sm table-condensed", style="font-size:11px; margin-bottom:8px;",
        tags$thead(tags$tr(tags$th("Var 1"), tags$th("Var 2"), tags$th("r"), tags$th("Niveau"))),
        tags$tbody(lapply(pairs_high, function(p) {
          lv  <- if(abs(p$cor)>0.9999) "Parfaite" else if(abs(p$cor)>=0.90) "Très forte" else "Forte"
          col <- if(abs(p$cor)>0.9999) "#dc3545" else if(abs(p$cor)>=0.90) "#fd7e14" else "#6c757d"
          tags$tr(tags$td(tags$code(p$v1)), tags$td(tags$code(p$v2)),
                  tags$td(tags$strong(style=paste0("color:",col), p$cor)),
                  tags$td(style=paste0("color:",col), lv))
        }))
      ),
      if (!all(is.na(vif_vals))) tagList(
        tags$small(style="color:#495057;font-weight:bold;display:block;margin-bottom:4px;", "VIF :"),
        div(style="display:flex;flex-wrap:wrap;gap:4px;margin-bottom:8px;",
            lapply(names(vif_vals), function(v) {
              vv <- vif_vals[v]
              col <- if(is.infinite(vv)||vv>10) "#dc3545" else if(vv>5) "#fd7e14" else "#28a745"
              lbl <- if(is.infinite(vv)) "∞" else round(vv,1)
              tags$span(style=paste0("background:",col,";color:white;border-radius:3px;padding:2px 6px;font-size:11px;"), paste0(v,": ",lbl))
            })),
        tags$small(style="color:#6c757d;display:block;margin-bottom:8px;",
                   "VIF < 5 : acceptable · 5–10 : élevé · > 10 : très élevé")
      ),
      hr(style="margin:8px 0;"),
      tags$strong(style="font-size:12px;color:#495057;", icon("tools"), " Corriger :"),
      div(style="margin-top:8px;display:flex;flex-wrap:wrap;gap:6px;",
          if (length(suggest_remove)>0)
            actionButton("afdAutoRemoveCollinear",
                         tagList(icon("magic"), paste0(" Supprimer auto (", paste(suggest_remove, collapse=", "), ")")),
                         class="btn-sm btn-warning", style="font-size:11px;")
      ),
      if (length(suggest_remove)>0)
        div(style="margin-top:6px;font-size:11px;color:#6c757d;",
            icon("info-circle"), paste0(" Variables suggérées : ", paste(suggest_remove, collapse=", ")))
    ))
  })
  
  observeEvent(input$afdAutoRemoveCollinear, {
    req(values$filteredData, input$afdVars)
    afd_d <- values$filteredData[, input$afdVars, drop=FALSE]
    afd_d <- afd_d[, sapply(afd_d, is.numeric), drop=FALSE]
    R_mat <- safe_cor(afd_d)
    if (is.null(R_mat)) return()
    to_rem <- c()
    for (i in seq_len(nrow(R_mat))) for (j in seq_len(ncol(R_mat)))
      if (i<j && abs(R_mat[i,j])>0.9999) to_rem <- c(to_rem, colnames(R_mat)[j])
    if (ncol(afd_d)>=3) {
      afd_d_nzv <- remove_zero_var_cols(afd_d)
      vif_v <- sapply(seq_len(ncol(afd_d_nzv)), function(i) {
        y <- afd_d_nzv[[i]]; x <- remove_zero_var_cols(afd_d_nzv[,-i,drop=FALSE])
        if (ncol(x)==0) return(Inf)
        r2 <- tryCatch(suppressWarnings(summary(lm(y~.,data=x))$r.squared), error=function(e) NA)
        if(is.na(r2)||r2>=1) Inf else 1/(1-r2)
      })
      names(vif_v) <- names(afd_d_nzv)
      to_rem <- unique(c(to_rem, names(vif_v[is.finite(vif_v) & vif_v>10])))
    }
    new_vars <- setdiff(input$afdVars, to_rem)
    if (length(new_vars) < 1) {
      showNotification("Impossible de supprimer toutes les variables (minimum 1 requis).", type="warning", duration=6)
      return()
    }
    updatePickerInput(session, "afdVars", selected=new_vars)
    showNotification(paste0("Variables retirées : ", paste(to_rem, collapse=", ")), type="message", duration=6)
  })

  output$afdMeansGroupSelect <- renderUI({
    # Vérification explicite au lieu de req()
    if (is.null(values$filteredData) || nrow(values$filteredData) == 0) {
      return(p(style = "color: #856404; font-size: 11px;", 
               icon("exclamation-triangle"), " Chargez d'abord des donnees."))
    }
    
    fac_cols <- get_categorical_cols(values$filteredData)
    
    if (length(fac_cols) == 0) {
      return(div(
        style = "background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 10px 0;",
        p(style = "margin: 0; font-size: 12px; color: #721c24;",
          icon("exclamation-triangle"), 
          HTML(" <strong>Attention:</strong> Aucune variable categorielle disponible."))
      ))
    }
    
    # Par défaut, sélectionner le facteur de discrimination si disponible
    selected_val <- if (!is.null(input$afdFactor) && input$afdFactor %in% fac_cols) {
      input$afdFactor
    } else {
      fac_cols[1]
    }
    
    selectInput("afdMeansGroup", 
                "Variable de groupement pour les moyennes:", 
                choices = fac_cols,
                selected = selected_val)
  })
  
  # Gestion correcte des moyennes par groupe
  afdResultReactive <- reactive({
    req(values$filteredData, input$afdVars, input$afdFactor)
    
    input$afdUseMeans
    input$afdMeansGroup
    input$afdCrossValidation
    input$afdQualiSup
    input$afdRefresh  
    
    tryCatch({
      # Vérifier si les moyennes doivent être utilisées
      use_means <- !is.null(input$afdUseMeans) && input$afdUseMeans && 
        !is.null(input$afdMeansGroup) && input$afdMeansGroup != ""
      
      if (use_means) {
        # Vérifier que le facteur de discrimination est différent du groupe de moyennes
        # ou qu'il y a plusieurs observations par combinaison
        if (input$afdMeansGroup == input$afdFactor) {
          # Calculer les moyennes par groupe - chaque groupe aura une seule ligne
          # L'AFD n'est pas possible avec une seule observation par groupe
          showNotification(
            "Attention: Avec les moyennes par groupe, chaque groupe n'a qu'une observation. L'AFD sera effectuée sur les données individuelles.",
            type = "warning",
            duration = 5
          )
          # Utiliser les données individuelles à la place
          # Conversion de la variable à discriminer en facteur (accepte tous les types)
      if (!is.null(afd_data[[input$afdFactor]])) {
        col_vals <- afd_data[[input$afdFactor]]
        if (inherits(col_vals, c("Date","POSIXct","POSIXlt"))) {
          afd_data[[input$afdFactor]] <- factor(format(col_vals, "%Y-%m-%d"))
        } else if (is.numeric(col_vals) || is.integer(col_vals)) {
          afd_data[[input$afdFactor]] <- factor(as.character(col_vals))
        } else {
          afd_data[[input$afdFactor]] <- factor(as.character(col_vals))
        }
        afd_data[[input$afdFactor]] <- droplevels(afd_data[[input$afdFactor]])
      }
      cols_to_select <- c(input$afdVars, input$afdFactor)
          afd_data <- values$filteredData[, cols_to_select, drop = FALSE]
          use_means <- FALSE
        } else {
          # Groupement différent du facteur - calculer les moyennes
          afd_data_numeric <- calculate_group_means(values$filteredData, 
                                                    input$afdVars, 
                                                    input$afdMeansGroup)
          afd_data <- afd_data_numeric
          afd_data[[input$afdFactor]] <- factor(rownames(afd_data_numeric))
          
          # Vérifier qu'il y a au moins 2 observations par groupe
          group_counts <- table(afd_data[[input$afdFactor]])
          if (any(group_counts < 2)) {
            showNotification(
              "Attention: Certains groupes ont moins de 2 observations. L'AFD sera effectuée sur les données individuelles.",
              type = "warning",
              duration = 5
            )
            cols_to_select <- c(input$afdVars, input$afdFactor)
            afd_data <- values$filteredData[, cols_to_select, drop = FALSE]
            use_means <- FALSE
          } else {
            # Notification pour confirmer l'utilisation des moyennes
            n_groups <- nrow(afd_data)
            showNotification(
              paste0("AFD sur moyennes: ", n_groups, " groupes (", input$afdMeansGroup, ")"), 
              type = "message", 
              duration = 3,
              id = "afd_means_notif"
            )
          }
        }
      } else {
        # Inclure les variables quali sup si pas de moyennes
        cols_to_select <- c(input$afdVars, input$afdFactor)
        if (!is.null(input$afdQualiSup) && length(input$afdQualiSup) > 0) {
          cols_to_select <- c(cols_to_select, input$afdQualiSup)
        }
        afd_data <- values$filteredData[, cols_to_select, drop = FALSE]
      }
      
      # S'assurer que la variable de groupement est un facteur 
      if (!is.factor(afd_data[[input$afdFactor]])) {
        afd_data[[input$afdFactor]] <- tryCatch(
          factor(as.character(afd_data[[input$afdFactor]])),
          error = function(e) factor(afd_data[[input$afdFactor]])
        )
      }
      # Supprimer les niveaux vides et les NA dans le facteur discriminant
      afd_data <- afd_data[!is.na(afd_data[[input$afdFactor]]), ]
      afd_data[[input$afdFactor]] <- droplevels(afd_data[[input$afdFactor]])
      
      # Vérifier qu'il y a au moins 2 observations par groupe
      group_counts <- table(afd_data[[input$afdFactor]])
      if (any(group_counts < 2)) {
        groups_with_one <- names(group_counts)[group_counts < 2]
        showNotification(
          paste0("Certains groupes n'ont qu'une observation: ", 
                 paste(groups_with_one, collapse = ", "), 
                 ". L'AFD nécessite au moins 2 observations par groupe."),
          type = "error",
          duration = 10
        )
        return(NULL)
      }
      
      # Vérifier que les variables ne sont pas constantes dans les groupes
      # Une variable est considérée constante si sa variance intra-groupe est nulle
      constant_vars <- c()
      for (var in input$afdVars) {
        # Calculer la variance intra-groupe
        var_by_group <- tapply(afd_data[[var]], afd_data[[input$afdFactor]], function(x) {
          if (length(x) < 2) return(0)
          var(x, na.rm = TRUE)
        })
        # Si toutes les variances intra-groupe sont nulles ou NA, la variable est constante
        if (all(is.na(var_by_group) | var_by_group == 0)) {
          constant_vars <- c(constant_vars, var)
        }
      }
      
      if (length(constant_vars) > 0 && length(constant_vars) == length(input$afdVars)) {
        showNotification(
          paste0("Toutes les variables sont constantes à l'intérieur des groupes. ",
                 "L'AFD n'est pas possible. Vérifiez que vos données ont suffisamment de variabilité."),
          type = "error",
          duration = 10
        )
        return(NULL)
      }
      
      # Si certaines variables sont constantes, les exclure
      vars_to_use <- setdiff(input$afdVars, constant_vars)
      if (length(constant_vars) > 0) {
        showNotification(
          paste0("Variables exclues (constantes dans les groupes): ", 
                 paste(constant_vars, collapse = ", ")),
          type = "warning",
          duration = 8
        )
      }
      
      if (length(vars_to_use) < 1) {
        showNotification("Pas assez de variables non-constantes pour l'AFD.", type = "error", duration = 5)
        return(NULL)
      }
      
      # ── Détecter et éliminer les variables colinéaires avant lda() 
      # lda() plante avec solve.default si la matrice intra-groupe est singulière
      if (length(vars_to_use) >= 2) {
        num_afd <- afd_data[, vars_to_use, drop = FALSE]
        num_afd <- num_afd[, sapply(num_afd, is.numeric), drop = FALSE]
        if (ncol(num_afd) >= 2) {
          R_afd <- safe_cor(num_afd)
          if (!is.null(R_afd) && !anyNA(R_afd)) {
            drop_afd <- c()
            for (ci in seq_len(ncol(R_afd))) {
              for (cj in seq_len(ncol(R_afd))) {
                if (ci < cj && colnames(R_afd)[ci] %in% vars_to_use &&
                    !colnames(R_afd)[cj] %in% drop_afd) {
                  if (abs(R_afd[ci, cj]) > 0.9999)
                    drop_afd <- c(drop_afd, colnames(R_afd)[cj])
                }
              }
            }
            if (length(drop_afd) > 0) {
              showNotification(
                paste0("AFD : variables colinéaires exclues automatiquement — ",
                       paste(drop_afd, collapse = ", "), "."),
                type = "warning", duration = 8)
              vars_to_use <- setdiff(vars_to_use, drop_afd)
            }
          }
        }
      }
      
      if (length(vars_to_use) < 1) {
        showNotification("AFD : aucune variable non-colinéaire disponible. Vérifiez vos données.", 
                         type = "error", duration = 8)
        return(NULL)
      }
      
      factor_safe <- paste0("`", input$afdFactor, "`")
      vars_safe <- paste0("`", vars_to_use, "`", collapse = " + ")
      afd_formula <- as.formula(paste(factor_safe, "~", vars_safe))
      
      afd_result <- withCallingHandlers(
        lda(afd_formula, data = afd_data),
        warning = function(w) {
          if (grepl("collinear|colinéaire", conditionMessage(w), ignore.case = TRUE)) {
            showNotification(
              paste0("AFD : variables encore partiellement colinéaires (warning LDA). ",
                     "Les résultats peuvent être instables."),
              type = "warning", duration = 6)
          }
          invokeRestart("muffleWarning")
        }
      )
      afd_predict <- tryCatch(
        predict(afd_result, afd_data[, vars_to_use, drop = FALSE]),
        error = function(e) {
          showNotification(paste("AFD predict() :", e$message), type = "error", duration = 8)
          NULL
        }
      )
      if (is.null(afd_predict)) return(NULL)
      
      cv_results <- NULL
      if (!is.null(input$afdCrossValidation) && input$afdCrossValidation && !use_means) {
        # Récupérer les niveaux du facteur
        factor_levels <- levels(afd_data[[input$afdFactor]])
        
        cv_predictions <- character(nrow(afd_data))
        for (i in 1:nrow(afd_data)) {
          train_data <- afd_data[-i, ]
          test_data  <- afd_data[i, , drop = FALSE]
          cv_pred_class <- tryCatch({
            cv_model <- withCallingHandlers(
              lda(afd_formula, data = train_data),
              warning = function(w) invokeRestart("muffleWarning")
            )
            pred <- predict(cv_model, test_data)
            as.character(pred$class)
          }, error = function(e) NA_character_)
          cv_predictions[i] <- if (is.na(cv_pred_class)) "" else cv_pred_class
        }
        cv_predictions <- factor(cv_predictions, levels = factor_levels)
        cv_results <- list(predictions = cv_predictions)
      }
      
      return(list(
        model = afd_result,
        predictions = afd_predict,
        data = afd_data,
        vars_used = vars_to_use,
        factor_name = input$afdFactor,
        cv_results = cv_results
      ))
      
    }, error = function(e) {
      # Message d'erreur plus explicite
      err_msg <- e$message
      if (grepl("constant", err_msg, ignore.case = TRUE)) {
        showNotification(
          "Erreur AFD: Certaines variables sont constantes à l'intérieur des groupes. Essayez de sélectionner d'autres variables ou vérifiez vos données.",
          type = "error",
          duration = 10
        )
      } else if (grepl("collinear", err_msg, ignore.case = TRUE)) {
        showNotification(
          "Erreur AFD: Certaines variables sont colinéaires. Essayez de réduire le nombre de variables.",
          type = "error",
          duration = 10
        )
      } else {
        showNotification(paste("Erreur AFD:", err_msg), type = "error", duration = 10)
      }
      return(NULL)
    })
  })
  
  observe({
    res <- afdResultReactive()
    if (!is.null(res)) {
      values$afdResult <- res$model
    }
  })
  
  # Renommage de 'loadings' en 'coefficients' pour inclure les coefficients discriminants
  afdDataframes <- reactive({
    req(afdResultReactive())
    afd_res <- afdResultReactive()
    afd_result <- afd_res$model
    afd_predict <- afd_res$predictions
    afd_data <- afd_res$data
    
    tryCatch({
      # Coordonnees individus
      ind_coords_df <- as.data.frame(afd_predict$x)
      ind_coords_df <- cbind(
        Individual = rownames(ind_coords_df),
        Groupe_reel = as.character(afd_data[[input$afdFactor]]),
        Groupe_predit = as.character(afd_predict$class),
        ind_coords_df,
        stringsAsFactors = FALSE
      )
      rownames(ind_coords_df) <- NULL
      
      # Coefficients des fonctions discriminantes
      loadings_df <- as.data.frame(afd_result$scaling)
      loadings_df <- cbind(Variable = rownames(loadings_df), loadings_df)
      rownames(loadings_df) <- NULL
      
      # Matrice de structure
      afd_vars_nzv2 <- intersect(input$afdVars,
                                 names(remove_zero_var_cols(afd_data[, input$afdVars, drop=FALSE])))
      if (length(afd_vars_nzv2) == 0) afd_vars_nzv2 <- input$afdVars
      X_std <- scale(afd_data[, afd_vars_nzv2, drop=FALSE])
      X_std[is.nan(X_std)] <- 0
      scaling_sub2 <- afd_result$scaling[rownames(afd_result$scaling) %in% afd_vars_nzv2, , drop=FALSE]
      scores <- tryCatch(as.matrix(X_std) %*% scaling_sub2, error=function(e) matrix(0, nrow(X_std), ncol(afd_result$scaling)))
      structure_matrix <- tryCatch(suppressWarnings(cor(X_std, scores)), error=function(e) matrix(NA, ncol(X_std), ncol(scores)))
      structure_df <- as.data.frame(structure_matrix)
      structure_df <- cbind(Variable = rownames(structure_df), structure_df)
      rownames(structure_df) <- NULL
      
      # Tests F univaries avec protection des noms de variables
      f_tests_df <- data.frame(Variable = input$afdVars, stringsAsFactors = FALSE)
      for (var in input$afdVars) {
        # Utiliser backticks pour proteger les noms avec caracteres speciaux
        var_safe <- paste0("`", var, "`")
        factor_safe <- paste0("`", input$afdFactor, "`")
        formula_str <- paste(var_safe, "~", factor_safe)
        
        tryCatch({
          aov_result <- aov(as.formula(formula_str), data = afd_data)
          f_stat <- summary(aov_result)[[1]][1, "F value"]
          p_val <- summary(aov_result)[[1]][1, "Pr(>F)"]
          f_tests_df[f_tests_df$Variable == var, "F_statistic"] <- round(f_stat, 4)
          f_tests_df[f_tests_df$Variable == var, "p_value"] <- round(p_val, 4)
        }, error = function(e) {
          # En cas d'erreur, mettre NA
          f_tests_df[f_tests_df$Variable == var, "F_statistic"] <- NA
          f_tests_df[f_tests_df$Variable == var, "p_value"] <- NA
        })
      }
      
      # Matrice de confusion
      confusion_matrix <- table(Reel = afd_data[[input$afdFactor]], 
                                Predit = afd_predict$class)
      confusion_df <- as.data.frame.matrix(confusion_matrix)
      confusion_df <- cbind(Groupe_reel = rownames(confusion_df), confusion_df)
      rownames(confusion_df) <- NULL
      
      # Taux de classification
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      group_acc_df <- data.frame(
        Groupe = rownames(confusion_matrix),
        Taux_classification = numeric(nrow(confusion_matrix)),
        stringsAsFactors = FALSE
      )
      for (i in 1:nrow(confusion_matrix)) {
        group_acc_df$Taux_classification[i] <- round(confusion_matrix[i,i] / sum(confusion_matrix[i,]) * 100, 2)
      }
      
      # Centroides
      centroids_df <- as.data.frame(afd_result$means)
      centroids_df <- cbind(Groupe = rownames(centroids_df), centroids_df)
      rownames(centroids_df) <- NULL
      
      # Variance expliquee
      eigenvals <- afd_result$svd^2
      prop_var <- eigenvals / sum(eigenvals) * 100
      can_cor <- sqrt(eigenvals / (1 + eigenvals))
      
      variance_df <- data.frame(
        Fonction = paste0("LD", 1:length(eigenvals)),
        Valeur_propre = eigenvals,
        Variance_expliquee = round(prop_var, 2),
        Variance_cumulee = round(cumsum(prop_var), 2),
        Correlation_canonique = round(can_cor, 4),
        stringsAsFactors = FALSE
      )
      
      # Validation croisee (si disponible)
      cv_confusion_df <- NULL
      cv_accuracy_df <- NULL
      if (!is.null(afd_res$cv_results)) {
        cv_confusion <- table(Reel = afd_data[[input$afdFactor]], 
                              Predit = afd_res$cv_results$predictions)
        cv_confusion_df <- as.data.frame.matrix(cv_confusion)
        cv_confusion_df <- cbind(Groupe_reel = rownames(cv_confusion_df), cv_confusion_df)
        rownames(cv_confusion_df) <- NULL
        
        cv_accuracy <- sum(diag(cv_confusion)) / sum(cv_confusion)
        cv_accuracy_df <- data.frame(
          Metrique = "Taux_classification_global",
          Valeur = round(cv_accuracy * 100, 2),
          stringsAsFactors = FALSE
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
      showNotification(paste("Erreur dataframes AFD:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Stocker les dataframes de l'AFD dans values$ pour un acces fiable
  observe({
    req(afdResultReactive())
    
    # Attendre un peu pour laisser le temps au reactive de s'exécuter complètement
    isolate({
      tryCatch({
        dfs <- afdDataframes()
        
        if (!is.null(dfs)) {
          # Vérifier qu'au moins ind_coords existe
          if (!is.null(dfs$ind_coords) && nrow(dfs$ind_coords) > 0) {
            values$afdDataframes <- dfs
            
            # Message de succès détaillé
            n_individus <- nrow(dfs$ind_coords)
            n_groupes <- length(unique(dfs$ind_coords$Groupe_reel))
            
            msg <- paste0("AFD: ", n_individus, " individus, ", n_groupes, " groupes")
            showNotification(msg, type = "message", duration = 3)
          } else {
            showNotification("Avertissement: AFD dataframes créés mais vides", type = "warning", duration = 5)
          }
        } else {
          showNotification("Erreur: Impossible de créer les dataframes AFD", type = "error", duration = 5)
        }
      }, error = function(e) {
        showNotification(paste("Erreur stockage AFD:", e$message), type = "error", duration = 5)
      })
    })
  })
  
  # Sectionner les axes de l'AFD
  
  generate_distinct_colors <- function(n) {
    # Fonction pour générer n couleurs distinctives
    if (n <= 0) return(character(0))
    
    if (n <= 12) {
      # Pour 12 couleurs ou moins, utiliser des palettes prédéfinies combinées
      colors <- c(
        "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
        "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB"
      )
      return(colors[1:n])
    } else if (n <= 24) {
      # Pour 13-24 couleurs, combiner plusieurs palettes
      pal1 <- RColorBrewer::brewer.pal(min(n, 12), "Set3")
      pal2 <- RColorBrewer::brewer.pal(min(n - 12, 12), "Paired")
      return(c(pal1, pal2)[1:n])
    } else {
      # Pour plus de 24 couleurs, utiliser une génération automatique
      # avec des teintes espacées uniformément
      hcl.colors(n, palette = "Dynamic", alpha = 0.8)
    }
  }
  
  output$afdAxisXSelect <- renderUI({
    req(afdResultReactive())
    afd_res <- afdResultReactive()
    n_dims <- ncol(afd_res$predictions$x)
    
    if (n_dims == 1) {
      return(div(style = "padding: 10px; background-color: #fff3cd; border-radius: 4px;",
                 p(style = "margin: 0; color: #856404; font-size: 12px;",
                   icon("info-circle"), " Une seule fonction discriminante disponible")))
    }
    
    selectInput("afdAxisX", "Axe X:",
                choices = setNames(1:n_dims, paste0("LD", 1:n_dims)),
                selected = 1)
  })
  
  output$afdAxisYSelect <- renderUI({
    req(afdResultReactive())
    afd_res <- afdResultReactive()
    n_dims <- ncol(afd_res$predictions$x)
    
    if (n_dims == 1) {
      return(div(style = "padding: 10px; background-color: #fff3cd; border-radius: 4px;",
                 p(style = "margin: 0; color: #856404; font-size: 12px;",
                   icon("info-circle"), " Une seule fonction discriminante disponible")))
    }
    
    selectInput("afdAxisY", "Axe Y:",
                choices = setNames(1:n_dims, paste0("LD", 1:n_dims)),
                selected = min(2, n_dims))
  })
  
  createAfdIndPlot <- function(afd_res) {
    afd_predict <- afd_res$predictions
    afd_data    <- afd_res$data
    # Utiliser le nom du facteur stocké dans afd_res (pas input$afdFactor qui est NULL hors réactif)
    factor_name <- afd_res$factor_name %||%
                   names(afd_data)[sapply(afd_data, is.factor)][1] %||%
                   names(afd_data)[ncol(afd_data)]

    n_dims <- ncol(afd_predict$x)
    
    # Axes sélectionnés (par défaut 1 et 2, ou 1 et densité si une seule dimension)
    axis_x <- if (!is.null(input$afdAxisX)) as.numeric(input$afdAxisX) else 1
    axis_y <- if (!is.null(input$afdAxisY) && n_dims > 1) as.numeric(input$afdAxisY) else NULL
    
    ind_title <- if (!is.null(input$afdIndTitle) && input$afdIndTitle != "") {
      input$afdIndTitle
    } else {
      "AFD - Projection des individus"
    }
    
    afd_df <- as.data.frame(afd_predict$x)
    afd_df$Groupe     <- afd_data[[factor_name]]
    afd_df$Individual <- rownames(afd_data)
    
    x_label <- if (!is.null(input$afdIndXLabel) && input$afdIndXLabel != "") {
      input$afdIndXLabel
    } else {
      paste0("LD", axis_x)
    }
    
    y_label <- if (!is.null(input$afdIndYLabel) && input$afdIndYLabel != "") {
      input$afdIndYLabel
    } else {
      if (n_dims > 1 && !is.null(axis_y)) paste0("LD", axis_y) else "Densité"
    }
    
    # Générer des couleurs distinctives pour tous les groupes
    n_groups <- length(unique(afd_df$Groupe))
    group_colors <- generate_distinct_colors(n_groups)
    names(group_colors) <- levels(factor(afd_df$Groupe))
    
    # Construire le graphique selon le nombre de dimensions
    if (n_dims > 1 && !is.null(axis_y)) {
      # Graphique 2D avec deux axes sélectionnés
      x_col <- paste0("LD", axis_x)
      y_col <- paste0("LD", axis_y)
      
      p_ind <- ggplot(afd_df, aes_string(x = x_col, y = y_col, 
                                         color = "Groupe", label = "Individual")) +
        geom_point(size = 3, alpha = 0.7) +
        geom_text(vjust = -0.5, hjust = 0.5, size = 3, check_overlap = TRUE) +
        theme_minimal() +
        labs(title = ind_title, x = x_label, y = y_label) +
        scale_color_manual(values = group_colors) +
        theme(legend.position = "right",
              legend.title = element_markdown(size = 10, face = "bold"),
              legend.text = element_text(size = 9))
      
      if (!is.null(input$afdIndCenterAxes) && input$afdIndCenterAxes) {
        coords <- afd_predict$x[, c(axis_x, axis_y)]
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p_ind <- p_ind + xlim(-max_range, max_range) + ylim(-max_range, max_range)
      }
    } else {
      # Graphique 1D avec densité
      x_col <- paste0("LD", axis_x)
      
      p_ind <- ggplot(afd_df, aes_string(x = x_col, fill = "Groupe", color = "Groupe")) +
        geom_density(alpha = 0.5) +
        theme_minimal() +
        labs(title = ind_title, x = x_label, y = y_label) +
        scale_color_manual(values = group_colors) +
        scale_fill_manual(values = group_colors) +
        theme(legend.position = "right",
              legend.title = element_markdown(size = 10, face = "bold"),
              legend.text = element_text(size = 9))
    }
    
    return(p_ind)
  }
  
  
  createAfdVarPlot <- function(afd_res) {
    afd_result <- afd_res$model
    afd_data <- afd_res$data
    
    n_dims <- length(afd_result$svd)
    
    # Axes sélectionnés (par défaut 1 et 2, ou 1 et corrélation si une seule dimension)
    axis_x <- if (!is.null(input$afdAxisX)) as.numeric(input$afdAxisX) else 1
    axis_y <- if (!is.null(input$afdAxisY) && n_dims > 1) as.numeric(input$afdAxisY) else NULL
    
    var_title <- if (!is.null(input$afdVarTitle) && input$afdVarTitle != "") {
      input$afdVarTitle
    } else {
      "AFD - Contribution des variables"
    }
    
    # Utiliser les variables stockées dans afd_res (pas input$afdVars qui est NULL hors réactif)
    vars_used <- afd_res$vars_used
    if (is.null(vars_used) || length(vars_used) == 0) {
      # Fallback : colonnes numériques de afd_data (hors facteur)
      vars_used <- names(afd_data)[sapply(afd_data, is.numeric)]
    }
    if (is.null(vars_used) || length(vars_used) == 0) stop("Aucune variable disponible pour le graphique AFD.")
    
    X_std <- scale(afd_data[, vars_used, drop = FALSE])
    scores <- as.matrix(X_std) %*% afd_result$scaling
    structure_matrix <- cor(X_std, scores)
    
    var_df <- as.data.frame(structure_matrix)
    var_df$Variable <- rownames(structure_matrix)
    
    x_label <- if (!is.null(input$afdVarXLabel) && input$afdVarXLabel != "") {
      input$afdVarXLabel
    } else {
      paste0("LD", axis_x)
    }
    
    y_label <- if (!is.null(input$afdVarYLabel) && input$afdVarYLabel != "") {
      input$afdVarYLabel
    } else {
      if (n_dims > 1 && !is.null(axis_y)) paste0("LD", axis_y) else "Corrélation"
    }
    
    # Construire le graphique selon le nombre de dimensions
    if (n_dims > 1 && !is.null(axis_y)) {
      # Graphique 2D avec deux axes sélectionnés
      x_col <- paste0("LD", axis_x)
      y_col <- paste0("LD", axis_y)
      
      p_var <- ggplot(var_df, aes_string(x = x_col, y = y_col, label = "Variable")) +
        geom_segment(aes_string(xend = x_col, yend = y_col), 
                     x = 0, y = 0, arrow = arrow(length = unit(0.3, "cm")), 
                     color = "#2E86AB", size = 1.2) +
        geom_text(vjust = -0.5, hjust = 0.5, size = 4, fontface = "bold") +
        theme_minimal() +
        labs(title = var_title, x = x_label, y = y_label) +
        theme(plot.title = element_markdown(hjust = 0.5, face = "bold"))
      
      if (!is.null(input$afdVarCenterAxes) && input$afdVarCenterAxes) {
        coords <- structure_matrix[, c(axis_x, axis_y)]
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p_var <- p_var + xlim(-max_range, max_range) + ylim(-max_range, max_range)
      }
    } else {
      # Graphique 1D avec corrélation
      x_col <- paste0("LD", axis_x)
      
      var_df_ordered <- var_df[order(abs(var_df[[x_col]]), decreasing = TRUE), ]
      var_df_ordered$Variable <- factor(var_df_ordered$Variable, 
                                        levels = var_df_ordered$Variable)
      
      # Créer une colonne pour la couleur basée sur le signe
      var_df_ordered$Color <- ifelse(var_df_ordered[[x_col]] > 0, "Positive", "Négative")
      
      p_var <- ggplot(var_df_ordered, aes_string(x = "Variable", y = x_col, fill = "Color")) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal() +
        labs(title = var_title, x = "Variable", y = y_label) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        scale_fill_manual(values = c("Positive" = "#2E86AB", "Négative" = "#A23B72"),
                          name = "Corrélation") +
        theme(plot.title = element_markdown(hjust = 0.5, face = "bold"),
              legend.position = "bottom")
    }
    
    return(p_var)
  }
  
  output$afdIndPlot <- renderPlotly({
    req(values$filteredData, input$afdFactor)
    p_ind <- suppressWarnings(suppressMessages(createAfdIndPlot(afdResultReactive())))
    suppressWarnings({
      ggplotly(p_ind) %>% layout(showlegend = TRUE)
    })
  })
  
  output$afdVarPlot <- renderPlotly({
    req(values$filteredData, input$afdFactor)
    p_var <- suppressWarnings(suppressMessages(createAfdVarPlot(afdResultReactive())))
    suppressWarnings({
      ggplotly(p_var) %>% layout(showlegend = FALSE)
    })
  })
  
  output$afdSummary <- renderUI({
    req(afdResultReactive())
    afd_res     <- afdResultReactive()
    afd_result  <- afd_res$model
    afd_predict <- afd_res$predictions
    afd_data    <- afd_res$data
    vars_used   <- if (!is.null(afd_res$vars_used)) afd_res$vars_used else input$afdVars
    use_round   <- !is.null(input$afdRoundResults) && input$afdRoundResults
    dec         <- if (use_round && !is.null(input$afdDecimals)) input$afdDecimals else 3
    
    tryCatch({
      # ── Calculs centraux 
      eigenvals <- afd_result$svd^2
      prop_var  <- eigenvals / sum(eigenvals) * 100
      can_cor   <- sqrt(eigenvals / (1 + eigenvals))
      eta2_vals <- eigenvals / (1 + eigenvals)
      
      confusion_matrix     <- table(Reel = afd_data[[input$afdFactor]], Predit = afd_predict$class)
      accuracy             <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      n_total              <- sum(confusion_matrix)
      expected_agreement   <- sum(rowSums(confusion_matrix) * colSums(confusion_matrix)) / (n_total^2)
      kappa_val            <- (accuracy - expected_agreement) / (1 - expected_agreement)
      
      X_std            <- scale(afd_data[, vars_used, drop = FALSE])
      scores           <- as.matrix(X_std) %*% afd_result$scaling
      structure_matrix <- cor(X_std, scores)
      
      # ── Helpers de style 
      card <- function(..., border_color = "#dee2e6", bg = "white") {
        div(style = paste0(
          "background:", bg, "; border-radius:8px; padding:16px; margin-bottom:14px;",
          "border:1px solid ", border_color, "; box-shadow: 0 1px 4px rgba(0,0,0,.06);"
        ), ...)
      }
      section_header <- function(num, label, color = "#3a6186", icon_name = "chart-bar") {
        div(style = paste0(
          "background: linear-gradient(135deg,", color, " 0%, ", color, "cc 100%);",
          "border-radius:6px; padding:11px 16px; margin-bottom:12px;"
        ),
        h5(style = "color:white; margin:0; font-weight:bold; font-size:15px;",
           icon(icon_name), paste0("  ", num, ". ", label))
        )
      }
      badge <- function(val, label, color) {
        div(style = "text-align:center;",
            div(style = paste0(
              "display:inline-block; background:", color, "; color:white;",
              "border-radius:8px; padding:8px 16px; font-size:26px; font-weight:bold;",
              "min-width:80px; margin-bottom:6px;"
            ), val),
            p(style = "margin:2px 0 0 0; font-size:13px; color:#444; text-transform:uppercase;", label)
        )
      }
      interp_bar <- function(text, color) {
        div(style = paste0(
          "margin-top:8px; padding:8px 14px; border-left:4px solid ", color, ";",
          "background:#f4f6f8; border-radius:0 4px 4px 0;"
        ),
        p(style = "margin:0; font-size:13px; color:#2c3e50; font-weight:500;",
          icon("info-circle"), " ", text)
        )
      }
      info_note <- function(text) {
        p(style = "font-size:13px; color:#5a6a7a; font-style:italic; margin:6px 0 0 0;", text)
      }
      kv_row <- function(label, val) {
        tags$tr(
          tags$td(style = "padding:4px 10px 4px 0; color:#555; font-size:12px; white-space:nowrap;", label),
          tags$td(style = "padding:4px 0; font-weight:bold; font-size:12px; color:#2c3e50;", val)
        )
      }
      
      # ── Couleurs dynamiques 
      acc_color <- if (accuracy >= .9) "#3a7d5c" else if (accuracy >= .8) "#4a7fa5" else if (accuracy >= .7) "#b07d2a" else "#c0392b"
      acc_interp <- if (accuracy >= .9) "Excellent — la discrimination est quasi-parfaite."
      else if (accuracy >= .8) "Bon — la discrimination est fiable."
      else if (accuracy >= .7) "Acceptable — des erreurs subsistent."
      else "Faible — le modele discrimine mal les groupes."
      
      kappa_color <- if (kappa_val >= .8) "#3a7d5c" else if (kappa_val >= .6) "#4a7fa5" else if (kappa_val >= .4) "#b07d2a" else "#c0392b"
      kappa_interp <- if (kappa_val >= .8) "Quasi-parfait (>= 0,80) — accord excellent au-dela du hasard."
      else if (kappa_val >= .6) "Substantiel (0,60 – 0,80) — bon accord."
      else if (kappa_val >= .4) "Modere (0,40 – 0,60) — accord partiel."
      else if (kappa_val >= .2) "Passable (0,20 – 0,40) — accord faible."
      else "Faible (< 0,20) — accord proche du hasard."
      
      eta2_colors <- sapply(eta2_vals, function(e)
        if (e >= .64) "#3a5f7d" else if (e >= .25) "#3a7d5c" else if (e >= .09) "#b07d2a" else "#c0392b")
      eta2_interps <- sapply(eta2_vals, function(e)
        if (e >= .64) "Excellent (>= 0,64)" else if (e >= .25) "Fort (0,25 – 0,64)"
        else if (e >= .09) "Modere (0,09 – 0,25)" else "Faible (< 0,09)")
      
      # ── RENDU UI 
      tagList(
        
        
        
        # ══════════════════════════════════════════════════════════════
        # SECTION 1 — VARIANCE EXPLIQUÉE + ETA²
        # ══════════════════════════════════════════════════════════════
        card(border_color = "#4a7fa5",
             section_header("1", "Variance expliquée & force de discrimination (eta\u00b2)", "#4a7fa5", "chart-pie"),
             info_note("Chaque fonction discriminante (LD) est évaluée par sa valeur propre, la variance qu'elle explique et son eta\u00b2."),
             tags$table(style = "width:100%; border-collapse:collapse; margin-top:10px;",
                        tags$thead(
                          tags$tr(style = "background:#4a7fa5; color:white;",
                                  tags$th(style = "padding:7px 8px; text-align:left; font-size:11px; border-radius:4px 0 0 0;", "Fonction"),
                                  tags$th(style = "padding:7px 8px; text-align:center; font-size:11px;", "Val. propre"),
                                  tags$th(style = "padding:7px 8px; text-align:center; font-size:11px;", "Variance (%)"),
                                  tags$th(style = "padding:7px 8px; text-align:center; font-size:11px;", "Cumul (%)"),
                                  tags$th(style = "padding:7px 8px; text-align:center; font-size:11px;", "Corr. canonique"),
                                  tags$th(style = "padding:7px 8px; text-align:center; font-size:11px; border-radius:0 4px 0 0;", "eta\u00b2")
                          )
                        ),
                        tags$tbody(
                          lapply(seq_along(eigenvals), function(i) {
                            bg <- if (i %% 2 == 0) "#f4f6f8" else "white"
                            tags$tr(style = paste0("background:", bg, ";"),
                                    tags$td(style = "padding:7px 8px; font-weight:bold; font-size:13px; color:#2c3e50;",
                                            paste0("LD", i)),
                                    tags$td(style = "padding:7px 8px; text-align:center; font-size:13px;",
                                            round(eigenvals[i], dec)),
                                    tags$td(style = "padding:7px 8px; text-align:center; font-size:13px;",
                                            paste0(round(prop_var[i], 1), "%")),
                                    tags$td(style = "padding:7px 8px; text-align:center; font-size:13px;",
                                            paste0(round(cumsum(prop_var)[i], 1), "%")),
                                    tags$td(style = "padding:7px 8px; text-align:center; font-size:13px;",
                                            round(can_cor[i], dec)),
                                    tags$td(style = paste0(
                                      "padding:7px 8px; text-align:center; font-size:13px;",
                                      "font-weight:bold; color:", eta2_colors[i], ";"
                                    ),
                                    paste0(round(eta2_vals[i], dec), " — ", eta2_interps[i]))
                            )
                          })
                        )
             ),
             div(style = "margin-top:10px; padding:8px; background:#eef4f9; border-radius:6px;",
                 p(style = "margin:0; font-size:13px; color:#3a6186;",
                   icon("ruler"), "  Seuils eta\u00b2 : ",
                   tags$strong("Excellent"), " \u2265 0,64  |  ",
                   tags$strong("Fort"), " 0,25–0,64  |  ",
                   tags$strong("Modéré"), " 0,09–0,25  |  ",
                   tags$strong("Faible"), " < 0,09"
                 )
             )
        ),
        
        # ══════════════════════════════════════════════════════════════
        # SECTION 2 — ACCURACY & KAPPA
        # ══════════════════════════════════════════════════════════════
        card(border_color = "#4a8c6f",
             section_header("2", "Qualité de classification — Accuracy & Kappa de Cohen", "#4a8c6f", "bullseye"),
             info_note("L'accuracy mesure la proportion d'individus bien classés. Le Kappa corrige ce taux en tenant compte de l'accord dû au seul hasard."),
             fluidRow(
               column(4,
                      card(border_color = acc_color, bg = "#f8f9fa",
                           badge(paste0(round(accuracy * 100, 1), "%"), "Accuracy globale", acc_color),
                           interp_bar(acc_interp, acc_color)
                      )
               ),
               column(4,
                      card(border_color = kappa_color, bg = "#f8f9fa",
                           badge(round(kappa_val, 3), "Kappa de Cohen", kappa_color),
                           interp_bar(kappa_interp, kappa_color)
                      )
               ),
               column(4,
                      card(border_color = "#6c5b8e", bg = "#f8f9fa",
                           div(style = "text-align:center;",
                               div(style = "font-size:22px; font-weight:bold; color:#6c5b8e;",
                                   paste0(nrow(confusion_matrix), " groupes")),
                               p(style = "font-size:13px; color:#444; margin:4px 0 0 0; text-transform:uppercase;",
                                 "Groupes discriminés"),
                               p(style = "font-size:13px; color:#555; margin:8px 0 0 0;",
                                 paste0(n_total, " individus classés"))
                           ),
                           interp_bar("Seuils Kappa : \u2265 0,80 quasi-parfait | 0,60–0,80 substantiel | 0,40–0,60 modéré", "#6c5b8e")
                      )
               )
             ),
             
             # Taux par groupe
             h6(style = "color:#2c3e50; font-weight:bold; margin:14px 0 4px 0; font-size:14px;",
                icon("list"), "  Taux de classification par groupe"),
             div(style = "padding:6px 10px; background:#f0f4f8; border-radius:6px; margin-bottom:8px;",
                 p(style = "margin:0; font-size:12px; color:#3a6186;",
                   icon("ruler"),
                   tags$strong(" Seuils : "),
                   tags$span(style = "color:#3a7d5c;", "\u2265 90% — Excellent"), " | ",
                   tags$span(style = "color:#b07d2a;", "70–90% — Acceptable"), " | ",
                   tags$span(style = "color:#c0392b;", "< 70% — Faible")
                 )
             ),
             fluidRow(
               lapply(1:nrow(confusion_matrix), function(i) {
                 g_acc <- confusion_matrix[i, i] / sum(confusion_matrix[i, ])
                 g_col <- if (g_acc >= .9) "#3a7d5c" else if (g_acc >= .7) "#b07d2a" else "#c0392b"
                 g_bg  <- if (g_acc >= .9) "#eaf5ef" else if (g_acc >= .7) "#fef9ec" else "#fdf0ef"
                 column(max(2, floor(12 / nrow(confusion_matrix))),
                        div(style = paste0(
                          "background:", g_bg, "; border:2px solid ", g_col, ";",
                          "border-radius:8px; padding:10px; text-align:center; margin-bottom:8px;"
                        ),
                        p(style = "margin:0; font-size:13px; color:#2c3e50; font-weight:bold; word-break:break-word;",
                          rownames(confusion_matrix)[i]),
                        div(style = paste0("font-size:24px; font-weight:bold; color:", g_col, "; margin-top:4px;"),
                            paste0(round(g_acc * 100, 1), "%"))
                        )
                 )
               })
             )
        ),
        
        # ══════════════════════════════════════════════════════════════
        # SECTION 3 — MATRICE DE CONFUSION
        # ══════════════════════════════════════════════════════════════
        card(border_color = "#b07840",
             section_header("3", "Matrice de confusion", "#b07840", "th"),
             info_note("Lecture : lignes = groupes réels, colonnes = groupes prédits. La diagonale représente les classifications correctes."),
             div(style = "overflow-x:auto;",
                 tags$table(style = "border-collapse:collapse; min-width:200px;",
                            tags$thead(
                              tags$tr(
                                tags$th(style = "padding:7px 10px; background:#f4f6f8; border:1px solid #dee2e6; font-size:13px;",
                                        "Réel \\ Prédit"),
                                lapply(colnames(confusion_matrix), function(cn)
                                  tags$th(style = "padding:7px 10px; background:#b07840; color:white; border:1px solid #dee2e6; font-size:13px; text-align:center;",
                                          cn)
                                )
                              )
                            ),
                            tags$tbody(
                              lapply(1:nrow(confusion_matrix), function(i) {
                                tags$tr(
                                  tags$td(style = "padding:7px 10px; background:#f9f2eb; border:1px solid #dee2e6; font-weight:bold; font-size:13px; color:#2c3e50;",
                                          rownames(confusion_matrix)[i]),
                                  lapply(1:ncol(confusion_matrix), function(j) {
                                    is_diag <- i == j
                                    bg <- if (is_diag) "#eaf5ef" else "white"
                                    fw <- if (is_diag) "bold" else "normal"
                                    col <- if (is_diag) "#3a7d5c" else "#2c3e50"
                                    tags$td(style = paste0(
                                      "padding:7px 10px; background:", bg, "; border:1px solid #dee2e6;",
                                      "text-align:center; font-weight:", fw, "; color:", col, "; font-size:13px;"
                                    ), confusion_matrix[i, j])
                                  })
                                )
                              })
                            )
                 )
             )
        ),
        
        # ══════════════════════════════════════════════════════════════
        # SECTION 4 — MATRICE DE STRUCTURE
        # ══════════════════════════════════════════════════════════════
        card(border_color = "#6c5b8e",
             section_header("4", "Matrice de structure — Corrélations variables–fonctions", "#6c5b8e", "project-diagram"),
             info_note("Les corrélations indiquent la contribution de chaque variable aux fonctions discriminantes. |r| \u2265 0,70 : forte | 0,40–0,70 : modérée | < 0,40 : faible."),
             div(style = "overflow-x:auto;",
                 tags$table(style = "border-collapse:collapse; width:100%;",
                            tags$thead(
                              tags$tr(
                                tags$th(style = "padding:7px 10px; background:#6c5b8e; color:white; border:1px solid #dee2e6; font-size:13px; text-align:left;",
                                        "Variable"),
                                lapply(colnames(structure_matrix), function(cn)
                                  tags$th(style = "padding:7px 10px; background:#6c5b8e; color:white; border:1px solid #dee2e6; font-size:13px; text-align:center;",
                                          cn)
                                )
                              )
                            ),
                            tags$tbody(
                              lapply(1:nrow(structure_matrix), function(i) {
                                bg <- if (i %% 2 == 0) "#f4f6f8" else "white"
                                tags$tr(style = paste0("background:", bg, ";"),
                                        tags$td(style = "padding:7px 10px; border:1px solid #dee2e6; font-weight:bold; font-size:13px; color:#2c3e50;",
                                                rownames(structure_matrix)[i]),
                                        lapply(1:ncol(structure_matrix), function(j) {
                                          v   <- structure_matrix[i, j]
                                          av  <- abs(v)
                                          col <- if (av >= .7) "#3a5f7d" else if (av >= .4) "#4a7fa5" else "#888"
                                          fw  <- if (av >= .4) "bold" else "normal"
                                          tags$td(style = paste0(
                                            "padding:7px 10px; border:1px solid #dee2e6;",
                                            "text-align:center; color:", col, "; font-weight:", fw, "; font-size:13px;"
                                          ), round(v, dec))
                                        })
                                )
                              })
                            )
                 )
             )
        ),
        
        # ══════════════════════════════════════════════════════════════
        # SECTION 5 — VALIDATION CROISÉE
        # ══════════════════════════════════════════════════════════════
        card(border_color = "#3a8070",
             section_header("5", "Validation croisée Leave-One-Out (LOO)", "#3a8070", "sync-alt"),
             if (!is.null(afd_res$cv_results)) {
               cv_conf    <- table(Reel = afd_data[[input$afdFactor]], Predit = afd_res$cv_results$predictions)
               cv_acc     <- sum(diag(cv_conf)) / sum(cv_conf)
               cv_col     <- if (cv_acc >= .9) "#3a7d5c" else if (cv_acc >= .8) "#4a7fa5" else if (cv_acc >= .7) "#b07d2a" else "#c0392b"
               cv_interp  <- if (cv_acc >= .9) "Excellente capacité prédictive (>= 90%)."
               else if (cv_acc >= .8) "Bonne capacité prédictive (80–90%)."
               else if (cv_acc >= .7) "Capacité prédictive acceptable (70–80%)."
               else "Capacité prédictive faible (< 70%) — risque de sur-ajustement."
               bias       <- accuracy - cv_acc
               bias_interp <- if (abs(bias) < 0.03) "Différence négligeable — modèle stable."
               else if (abs(bias) < 0.08) "Légère différence — sur-ajustement modéré."
               else "Différence importante — attention au sur-ajustement."
               tagList(
                 info_note("La LOO exclut un individu à la fois pour tester la prédiction. Elle évalue la capacité généralisatrice du modèle."),
                 fluidRow(
                   column(4, badge(paste0(round(cv_acc * 100, 1), "%"), "Accuracy LOO", cv_col)),
                   column(4, badge(paste0(round(accuracy * 100, 1), "%"), "Accuracy entrainement", acc_color)),
                   column(4, badge(paste0(if (bias > 0) "+" else "", round(bias * 100, 1), "%"), "Biais (train - LOO)",
                                   if (abs(bias) < 0.03) "#3a7d5c" else if (abs(bias) < 0.08) "#b07d2a" else "#c0392b"))
                 ),
                 interp_bar(cv_interp, cv_col),
                 interp_bar(bias_interp, if (abs(bias) < 0.03) "#3a7d5c" else if (abs(bias) < 0.08) "#b07d2a" else "#c0392b")
               )
             } else {
               div(style = "padding:10px; background:#fff3cd; border-radius:6px;",
                   p(style = "margin:0; font-size:12px; color:#856404;",
                     icon("exclamation-triangle"),
                     if (!is.null(input$afdUseMeans) && input$afdUseMeans)
                       "  Non disponible avec les moyennes par groupe. La validation croisée LOO nécessite des observations individuelles."
                     else
                       "  Non activée. Cochez l'option 'Activer la validation croisée (LOO)' dans le panneau de gauche pour l'utiliser."
                   )
               )
             }
        ),
        
        # ══════════════════════════════════════════════════════════════
        # SECTION 6 — CENTROIDES & PROBABILITÉS A PRIORI
        # ══════════════════════════════════════════════════════════════
        card(border_color = "#2c3e50",
             section_header("6", "Centroides des groupes & Probabilités a priori", "#2c3e50", "map-marker-alt"),
             info_note("Les centroïdes sont les moyennes des variables par groupe dans l'espace original. Les probabilités a priori reflètent les proportions de chaque groupe."),
             fluidRow(
               column(8,
                      h6(style = "color:#2c3e50; font-weight:bold; margin-bottom:6px;", "Centroïdes des groupes"),
                      div(style = "overflow-x:auto;",
                          tags$table(style = "border-collapse:collapse; width:100%;",
                                     tags$thead(tags$tr(
                                       tags$th(style = "padding:5px 8px; background:#2c3e50; color:white; border:1px solid #dee2e6; font-size:10px;", "Groupe"),
                                       lapply(colnames(afd_result$means), function(cn)
                                         tags$th(style = "padding:5px 8px; background:#2c3e50; color:white; border:1px solid #dee2e6; font-size:10px; text-align:center;", cn)
                                       )
                                     )),
                                     tags$tbody(lapply(1:nrow(afd_result$means), function(i) {
                                       bg <- if (i %% 2 == 0) "#f8f9fa" else "white"
                                       tags$tr(style = paste0("background:", bg, ";"),
                                               tags$td(style = "padding:5px 8px; border:1px solid #dee2e6; font-weight:bold; font-size:10px;",
                                                       rownames(afd_result$means)[i]),
                                               lapply(afd_result$means[i, ], function(v)
                                                 tags$td(style = "padding:5px 8px; border:1px solid #dee2e6; text-align:center; font-size:10px;",
                                                         round(v, dec))
                                               )
                                       )
                                     }))
                          )
                      )
               ),
               column(4,
                      h6(style = "color:#2c3e50; font-weight:bold; margin-bottom:6px;", "Probabilités a priori"),
                      lapply(names(afd_result$prior), function(g) {
                        pct <- round(afd_result$prior[[g]] * 100, 1)
                        div(style = "margin-bottom:8px;",
                            div(style = "display:flex; justify-content:space-between; font-size:11px; margin-bottom:2px;",
                                tags$span(style = "font-weight:bold; color:#2c3e50;", g),
                                tags$span(style = "color:#555;", paste0(pct, "%"))
                            ),
                            div(style = "background:#dee2e6; border-radius:4px; height:8px; overflow:hidden;",
                                div(style = paste0(
                                  "background:#2c3e50; height:100%; width:", pct, "%; border-radius:4px;"
                                ))
                            )
                        )
                      })
               )
             )
        )
      )
      
    }, error = function(e) {
      div(style = "padding:12px; background:#f8d7da; border-radius:6px;",
          p(style = "margin:0; color:#721c24;",
            icon("exclamation-triangle"), " Erreur lors du rendu des métriques AFD : ", e$message))
    })
  })
  
  # Téléchargement graphique AFD Individus avec dimensions automatiques
  output$downloadAfdIndPlot <- downloadHandler(
    filename = function() paste0("afd_individus_", Sys.Date(), ".", input$afdInd_format),
    content = function(file) {
      dpi       <- input$afdInd_dpi
      auto_dims <- calculate_dimensions_from_dpi(dpi, 25, 20)
      p <- withCallingHandlers(
        suppressMessages(createAfdIndPlot(afdResultReactive())),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      withCallingHandlers(
        suppressWarnings(ggsave(file, plot = p, device = input$afdInd_format,
                                width = auto_dims$width, height = auto_dims$height,
                                dpi = dpi, units = "cm")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
    }
  )
  
  # Téléchargement graphique AFD Variables avec dimensions automatiques
  output$downloadAfdVarPlot <- downloadHandler(
    filename = function() paste0("afd_variables_", Sys.Date(), ".", input$afdVar_format),
    content = function(file) {
      dpi       <- input$afdVar_dpi
      auto_dims <- calculate_dimensions_from_dpi(dpi, 25, 20)
      p <- withCallingHandlers(
        suppressMessages(createAfdVarPlot(afdResultReactive())),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      withCallingHandlers(
        suppressWarnings(ggsave(file, plot = p, device = input$afdVar_format,
                                width = auto_dims$width, height = auto_dims$height,
                                dpi = dpi, units = "cm")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
    }
  )
  
  # Téléchargement donnees AFD - Excel avec nom correct
  output$downloadAfdDataXlsx <- downloadHandler(
    filename = function() {
      "afd_resultats.xlsx"  # Nom fixe sans date pour éviter problèmes
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      dfs <- if (!is.null(values$afdDataframes)) {
        values$afdDataframes
      } else {
        afdDataframes()
      }
      
      if (is.null(dfs) || is.null(dfs$ind_coords) || nrow(dfs$ind_coords) == 0) {
        showNotification("Erreur : aucune donnee AFD disponible", type = "error", duration = 10)
        return(NULL)
      }
      
      tryCatch({
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
        
        if (!is.null(dfs$cv_confusion) && nrow(dfs$cv_confusion) > 0) {
          addWorksheet(wb, "CV_confusion")
          writeData(wb, "CV_confusion", dfs$cv_confusion)
          
          if (!is.null(dfs$cv_accuracy)) {
            addWorksheet(wb, "CV_taux")
            writeData(wb, "CV_taux", dfs$cv_accuracy)
          }
        }
        
        saveWorkbook(wb, file, overwrite = TRUE)
        
        showNotification("Fichier Excel AFD telecharge avec succes!", type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Erreur Excel AFD:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # Téléchargement donnees AFD 
  output$downloadAfdDataCsv <- downloadHandler(
    filename = function() {
      "afd_resultats.zip"  # Nom fixe sans date
    },
    contentType = "application/zip",
    content = function(file) {
      dfs <- if (!is.null(values$afdDataframes)) {
        values$afdDataframes
      } else {
        afdDataframes()
      }
      
      if (is.null(dfs) || is.null(dfs$ind_coords) || nrow(dfs$ind_coords) == 0) {
        showNotification("Erreur : aucune donnee AFD disponible", type = "error", duration = 10)
        return(NULL)
      }
      
      tryCatch({
        temp_dir <- tempdir()
        csv_files <- c()
        
        # Nettoyer les anciens fichiers
        old_files <- list.files(temp_dir, pattern = "^afd_.*\\.csv$", full.names = TRUE)
        if (length(old_files) > 0) file.remove(old_files)
        
        # Créer les CSV
        write_csv_utf8(dfs$ind_coords, file.path(temp_dir, "afd_coordonnees_individus.csv"))
        csv_files <- c(csv_files, "afd_coordonnees_individus.csv")
        
        write_csv_utf8(dfs$coefficients, file.path(temp_dir, "afd_coefficients_discriminants.csv"))
        csv_files <- c(csv_files, "afd_coefficients_discriminants.csv")
        
        write_csv_utf8(dfs$structure_matrix, file.path(temp_dir, "afd_matrice_structure.csv"))
        csv_files <- c(csv_files, "afd_matrice_structure.csv")
        
        write_csv_utf8(dfs$f_tests, file.path(temp_dir, "afd_tests_F.csv"))
        csv_files <- c(csv_files, "afd_tests_F.csv")
        
        write_csv_utf8(dfs$confusion_matrix, file.path(temp_dir, "afd_matrice_confusion.csv"))
        csv_files <- c(csv_files, "afd_matrice_confusion.csv")
        
        write_csv_utf8(dfs$classification_rates, file.path(temp_dir, "afd_taux_classification.csv"))
        csv_files <- c(csv_files, "afd_taux_classification.csv")
        
        write_csv_utf8(dfs$centroids, file.path(temp_dir, "afd_centroides.csv"))
        csv_files <- c(csv_files, "afd_centroides.csv")
        
        write_csv_utf8(dfs$variance_explained, file.path(temp_dir, "afd_variance_expliquee.csv"))
        csv_files <- c(csv_files, "afd_variance_expliquee.csv")
        
        if (!is.null(dfs$cv_confusion) && nrow(dfs$cv_confusion) > 0) {
          write_csv_utf8(dfs$cv_confusion, file.path(temp_dir, "afd_cv_confusion.csv"))
          csv_files <- c(csv_files, "afd_cv_confusion.csv")
          
          if (!is.null(dfs$cv_accuracy)) {
            write_csv_utf8(dfs$cv_accuracy, file.path(temp_dir, "afd_cv_taux.csv"))
            csv_files <- c(csv_files, "afd_cv_taux.csv")
          }
        }
        
        zip(file, file.path(temp_dir, csv_files), flags = "-j")
        
        showNotification(paste0("CSV AFD telecharges (", length(csv_files), " fichiers)"), 
                         type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Erreur CSV AFD:", e$message), type = "error", duration = 10)
      })
    }
  )
  # AFD - Selecteur de variables categorielles pour la prediction
  output$afdPredictVarsSelect <- renderUI({
    req(values$filteredData, input$afdFactor)
    if (is.null(values$filteredData) || is.null(input$afdFactor)) {
      return(NULL)
    }
    
    fac_cols <- get_categorical_cols(values$filteredData)
    
    # Exclure le facteur discriminant des choix
    fac_cols <- fac_cols[fac_cols != input$afdFactor]
    
    # Exclure aussi le groupe de moyennes si utilise
    if (!is.null(input$afdMeansGroup) && !is.null(input$afdUseMeans) && input$afdUseMeans) {
      fac_cols <- fac_cols[fac_cols != input$afdMeansGroup]
    }
    
    if (length(fac_cols) == 0) {
      return(NULL)  # Ne pas afficher de message car le panneau d'info est déjà présent
    }
    
    pickerInput(
      inputId = "afdPredictVars",
      label = "Variables categorielles pour la prediction (optionnel):",
      choices = fac_cols,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
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
          plot.title = element_markdown(size = input$thresholdTitleSize %||% 16, 
                                        hjust = 0.5, face = "bold"),
          axis.title.x = element_markdown(size = input$thresholdAxisTitleSize %||% 14, 
                                          face = x_label_face,
                                          color = axis_color),
          axis.title.y = element_markdown(size = input$thresholdAxisTitleSize %||% 14, 
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
          legend.title = element_markdown(size = input$thresholdLegendSize %||% 10, face = legend_title_face),
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
          paste0("Erreur lors de l'export: ", e$message,
                 "\n\nConseils:",
                 "\n- Réduisez les dimensions ou le DPI",
                 "\n- Utilisez un format vectoriel (SVG, PDF) pour haute résolution",
                 "\n- Maximum recommandé: 5000×5000 px à 600 DPI"), 
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
}
