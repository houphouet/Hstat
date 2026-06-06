server <- function(input, output, session) {
  
  auto_quote_colnames <- function(formula_str, col_names) {
    # - 1. Protéger les noms de colonnes spéciaux
    cols_sorted <- col_names[order(nchar(col_names), decreasing = TRUE)]
    for (col in cols_sorted) {
      if (grepl("[-/ +*^()%$@!?]|^[0-9]", col, perl = TRUE)) {
        backtick_col <- paste0("`", col, "`")
        if (!grepl(backtick_col, formula_str, fixed = TRUE)) {
          formula_str <- gsub(col, backtick_col, formula_str, fixed = TRUE)
        }
      }
    }
    
    # - 2. Ajouter des variables à partir des functions
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
    
    # - 3. Transformer sum(c(...)) 
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
    y2RangeForAxis = NULL,
    y2UnifiedColorMap = NULL,
    postHocSyncTrigger = NULL,
    transformationLog  = list(),   # Journal des transformations appliquées
    chiSqResults       = NULL,     # Résultat du test chi2 / multinomial
    chiSqFreqData      = NULL,     # Données fréquences/pct traitées
    chiSqPostHocData   = NULL,     # Post-hoc chi2 (lettres + paires)
    chiSqPlotObj       = NULL,     # Graphique chi2 courant
    # ---- Moteur de donnees (memoire / hors-memoire DuckDB) ----
    dbCon        = NULL,           # Connexion DuckDB (NULL en mode memoire)
    dbTable      = NULL,           # Nom de la table/vue DuckDB
    dataMode     = "memory",       # "memory" ou "duckdb"
    fullNrow     = NULL,           # Nombre de lignes du jeu COMPLET
    fullNcol     = NULL,           # Nombre de colonnes du jeu complet
    fullNA       = NULL,           # Total de valeurs manquantes (jeu complet)
    isSampled    = FALSE,          # TRUE si values$data est un echantillon
    sourceKind   = NULL,           # Type de fichier source
    sourceSize   = NULL            # Taille du fichier source (octets)
  )
  
  values$customXLevels <- reactiveVal(NULL)
  
  
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
          values$transformationLog <- list()   # reset transformations
          values$chiSqResults     <- NULL
          values$chiSqFreqData    <- NULL
          values$chiSqPostHocData <- NULL
          values$chiSqPlotObj     <- NULL
          values$chiSqRawObs      <- NULL
          values$chiSqModalites   <- NULL
          values$chiSqPGlobal     <- NULL
          # Fermeture du moteur hors-memoire
          if (!is.null(values$dbCon)) hstat_duckdb_close(values$dbCon)
          values$dbCon     <- NULL
          values$dbTable   <- NULL
          values$dataMode  <- "memory"
          values$fullNrow  <- NULL
          values$fullNcol  <- NULL
          values$fullNA    <- NULL
          values$isSampled <- FALSE
          values$sourceKind <- NULL
          values$sourceSize <- NULL
          
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
    kind <- hstat_file_kind(input$file$datapath)
    if (kind == "inconnu") {
      showNotification("Format de fichier non pris en charge.", type = "error")
      return(invisible(NULL))
    }
    tryCatch({
      # Fermer une eventuelle connexion DuckDB precedente
      if (!is.null(values$dbCon)) {
        hstat_duckdb_close(values$dbCon)
        values$dbCon <- NULL
      }
      res <- NULL
      withProgress(message = 'Chargement des données', value = 0, {
        incProgress(0.2, detail = "Analyse du fichier")
        hstat_cache_clear()   # vide le cache d'agregations du fichier precedent
        thr <- (input$bigDataThreshold %||% 500) * 1024^2
        smp <- as.integer(input$sampleSize %||% HSTAT_SAMPLE_SIZE)
        incProgress(0.3, detail = "Lecture")
        res <- hstat_load_data(
          path = input$file$datapath, kind = kind,
          header = input$header %||% TRUE, sep = input$sep %||% ",",
          sheet = input$sheet %||% 1,
          threshold = thr, sample_size = smp)
        incProgress(0.8, detail = "Préparation")

        values$data        <- res$data
        values$cleanData   <- res$data
        values$filteredData <- res$data
        values$dbCon       <- res$con
        values$dbTable     <- res$table
        values$dataMode    <- res$mode
        values$fullNrow    <- res$full_nrow
        values$fullNcol    <- res$full_ncol
        values$isSampled   <- res$is_sampled
        values$sourceKind  <- res$kind
        values$sourceSize  <- res$size
        # Total des NA : direct en memoire, calcule en SQL en mode DuckDB
        values$fullNA <- if (res$mode == "duckdb")
          tryCatch(hstat_duckdb_na_total(res$con, res$table),
                   error = function(e) NA_real_)
        else res$full_na
        incProgress(1)
      })
      if (isTRUE(res$is_sampled)) {
        showNotification(
          sprintf("Fichier volumineux (%s) : mode hors-mémoire activé. Analyse sur un échantillon de %s lignes (sur %s au total).",
                  hstat_format_size(res$size),
                  format(nrow(res$data), big.mark = " "),
                  format(res$full_nrow, big.mark = " ")),
          type = "warning", duration = 12)
      } else {
        showNotification("Données chargées avec succès.", type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Erreur de chargement :", conditionMessage(e)), type = "error")
    })
  })

  # Liberer la connexion DuckDB a la fermeture de la session
  session$onSessionEnded(function() {
    isolate({
      if (!is.null(values$dbCon)) hstat_duckdb_close(values$dbCon)
    })
  })

  # Banniere de mode (memoire vs hors-memoire)
  # Indicateur global : l'application est-elle en mode hors-memoire ?
  # (utilise par les conditionalPanel "calculer sur le jeu complet" au niveau
  # racine de l'UI ; chaque module definit aussi le sien pour ses propres panels)
  output$hstatBigData <- reactive({
    identical(values$dataMode, "duckdb") && !is.null(values$dbCon)
  })
  outputOptions(output, "hstatBigData", suspendWhenHidden = FALSE)

  output$dataModeBanner <- renderUI({
    req(values$data)
    if (identical(values$dataMode, "duckdb")) {
      div(class = "callout callout-warning", style = "margin-bottom:16px;",
        h4(style = "margin:0 0 5px 0; font-weight:600;",
           icon("database"), " Mode hors-mémoire (out-of-core)"),
        p(style = "margin:0; font-size:13px;",
          HTML(sprintf(
            "Fichier de <b>%s</b> (%s lignes). Le jeu complet reste sur disque (DuckDB) ; les analyses portent sur un <b>échantillon représentatif de %s lignes</b>. Les compteurs ci-dessous reflètent le jeu complet.",
            hstat_format_size(values$sourceSize %||% 0),
            format(values$fullNrow %||% 0, big.mark = " "),
            format(nrow(values$data), big.mark = " ")))))
    } else {
      div(class = "callout callout-success", style = "margin-bottom:16px;",
        p(style = "margin:0; font-size:13px;",
          icon("memory"), HTML(sprintf(
            " Mode en mémoire — jeu de données entièrement chargé (%s lignes).",
            format(values$fullNrow %||% nrow(values$data), big.mark = " ")))))
    }
  })

  output$nrowBox <- renderValueBox({
    req(values$data)
    valueBox(
      format(values$fullNrow %||% nrow(values$data), big.mark = " "),
      if (isTRUE(values$isSampled)) "Lignes (jeu complet)" else "Lignes",
      icon = icon("list"), color = "purple"
    )
  })

  output$ncolBox <- renderValueBox({
    req(values$data)
    valueBox(
      values$fullNcol %||% ncol(values$data), "Colonnes", icon = icon("columns"),
      color = "purple"
    )
  })

  output$naBox <- renderValueBox({
    req(values$data)
    na_count <- values$fullNA
    if (is.null(na_count) || is.na(na_count)) na_count <- sum(is.na(values$data))
    valueBox(
      format(na_count, big.mark = " "), "Valeurs manquantes", icon = icon("question"),
      color = ifelse(na_count > 0, "red", "green")
    )
  })

  output$memBox <- renderValueBox({
    req(values$data)
    if (identical(values$dataMode, "duckdb")) {
      valueBox(hstat_format_size(values$sourceSize %||% 0), "Taille du fichier",
               icon = icon("hard-drive"), color = "blue")
    } else {
      valueBox(format(object.size(values$data), units = "auto"), "Taille mémoire",
               icon = icon("memory"), color = "blue")
    }
  })
  
  output$preview <- renderDT({
    req(values$data)
    datatable(head(values$data, 50), options = list(scrollX = TRUE))
  })
  
  # ---- Exploration (module Shiny) ----
  mod_explore_server("explore", values)

  # ---- Nettoyage (module Shiny) ----
  mod_clean_server("clean", values)

  # ---- Filtrage (module Shiny) ----
  mod_filter_server("filter", values)

  # ---- Gestion echantillon de travail (UI dans onglet Chargement) ----
  # Ligne d'information sur l'echantillon courant
  output$sampleInfoLine <- renderUI({    req(values$data)
    if (!identical(values$dataMode, "duckdb")) return(NULL)
    full <- values$fullNrow %||% 0
    cur  <- nrow(values$data)
    pct  <- if (full > 0) round(100 * cur / full, 2) else 0
    div(style = "margin-top:8px; padding:8px 12px; background:#f4f6f8; border-radius:6px;",
      p(style = "margin:0; font-size:13px; color:#2c3e50;",
        icon("circle-info"),
        HTML(sprintf(" Échantillon courant : <b>%s</b> lignes sur <b>%s</b> (%s %% du jeu complet).",
                     format(cur, big.mark = " "), format(full, big.mark = " "), pct))))
  })

  # Re-tirage de l'echantillon de travail (mode hors-memoire)
  observeEvent(input$redrawSample, {
    if (!identical(values$dataMode, "duckdb") || is.null(values$dbCon)) {
      showNotification("Le re-tirage n'est disponible qu'en mode hors-mémoire.",
                       type = "warning")
      return(invisible(NULL))
    }
    n <- as.integer(input$sampleSizeLive %||% HSTAT_SAMPLE_SIZE)
    if (is.na(n) || n < 1000) {
      showNotification("Taille d'échantillon invalide (minimum 1000).", type = "warning")
      return(invisible(NULL))
    }
    tryCatch({
      withProgress(message = "Tirage d'un nouvel échantillon (DuckDB)", value = 0.4, {
        hstat_set_seed(input$globalSeed)
        smp <- hstat_duckdb_sample(values$dbCon, values$dbTable, n)
        incProgress(0.8)
        values$data         <- smp
        values$cleanData    <- smp
        values$filteredData <- smp
        values$isSampled    <- (values$fullNrow %||% nrow(smp)) > nrow(smp)
        incProgress(1)
      })
      showNotification(
        sprintf("Nouvel échantillon de %s lignes. Relancez vos analyses pour en tenir compte.",
                format(nrow(values$data), big.mark = " ")),
        type = "message", duration = 7)
    }, error = function(e) {
      showNotification(paste("Erreur de tirage :", conditionMessage(e)),
                       type = "error", duration = 6)
    })
  })

  # ---- Analyses descriptives (module Shiny) ----
  mod_descriptive_server("descriptive", values)

  # ---- Tableaux croises (module Shiny) ----
  mod_crosstab_server("crosstab", values)

  # ---- Visualisation des donnees (module Shiny) ----
  mod_viz_server("visualization", values)

  # ---- Tests statistiques + Post-hoc (module Shiny combine) ----
  mod_tests_server("tests", values)

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
        
        # Les inputs _width/_height sont en PIXELS -> conversion px -> cm
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
  
  # - Panel colinéarité VIF
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
    else "Colinéarité modérée (|r| >= 0.80)"
    
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
                lbl <- if (is.infinite(vv)) "Inf" else round(vv, 1)
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
              "VIF < 5 : acceptable · VIF 5-10 : élevé · VIF > 10 : très élevé (rouge)"
            )
          )
        },
        
        hr(style = "margin: 8px 0;"),
        
        # Options de correction
        tags$strong(style = "font-size: 12px; color: #495057;",
                    icon("tools"), " Corriger la multicolinéarité :"),
        div(
          style = "margin-top: 8px; display: flex; flex-direction: column; gap: 6px;",
          
          #  supprimer les variables suggérées automatiquement
          if (length(suggest_remove) > 0) {
            actionButton(
              "pcaAutoRemoveCollinear",
              tagList(icon("magic"),
                      sprintf(" Supprimer automatiquement les %d variable(s) suggérée(s)",
                              length(suggest_remove))),
              class = "btn-sm btn-warning btn-block",
              style = "font-size: 11px; white-space: normal; text-align: left;"
            )
          },
          
          #  standardiser les variables (si pas encore coché)
          actionButton(
            "pcaForceStandardize",
            tagList(icon("balance-scale"), " Forcer la standardisation"),
            class = "btn-sm btn-outline-secondary btn-block",
            style = "font-size: 11px; white-space: normal; text-align: left;"
          )
        ),
        
        if (length(suggest_remove) > 0) {
          div(
            style = "margin-top: 8px; padding: 8px 10px; background: #fff8e1; border-radius: 6px; font-size: 11px; color: #6c757d; word-break: break-word;",
            icon("info-circle"),
            tags$b(" Variables suggérées à retirer : "),
            tags$span(paste(suggest_remove, collapse = ", ")),
            tags$br(),
            tags$span(style = "font-style: italic;",
              "Vous pouvez aussi les désélectionner manuellement ci-dessus.")
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
      
      # - Garde contre la singularité : supprimer les colonnes numériques
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
              paste0("ACP : variables colinéaires supprimées automatiquement -- ",
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
          # Vérifier que la matrice reste inversible (det != 0)
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
          paste0("ACP : matrice singulière -- variables trop colinéaires. ",
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
  
  # - Légende dynamique de la coloration ACP -
  output$pcaColorByLegend <- renderUI({
    color_choice <- if (!is.null(input$pcaColorBy)) input$pcaColorBy else "contrib"
    desc <- switch(color_choice,
                   "contrib" = list(
                     txt  = "Contribution (%) : part de chaque élément dans la construction de la composante. Plus la contribution est élevée (rouge), plus l'élément structure l'axe.",
                     col  = "#e65100", icon = "percentage"
                   ),
                   "cos2" = list(
                     txt  = "Cos² (qualité de représentation) : indique dans quelle mesure l'élément est bien représenté sur le plan factoriel (0 = mal représenté, 1 = parfaitement représenté).",
                     col  = "#1565c0", icon = "bullseye"
                   ),
                   "sat" = list(
                     txt  = "Indice de saturation (|corrélation|) : corrélation absolue entre la variable et les axes affichés. Proche de 1 = variable fortement liée au plan, proche de 0 = variable indépendante du plan.",
                     col  = "#4a148c", icon = "link"
                   ),
                   list(txt = "", col = "#555", icon = "info-circle")
    )
    div(style = paste0("margin-top:4px; padding:6px 10px; background:white; border-radius:4px; border-left:3px solid ", desc$col, "; font-size:11px; color:#444;"),
        icon(desc$icon), " ", desc$txt)
  })
  
  # - Vérification des conditions ACP -
  output$pcaConditionsCheck <- renderUI({
    req(values$filteredData)
    
    n_obs  <- nrow(values$filteredData)
    p_vars <- if (!is.null(input$pcaVars)) length(input$pcaVars) else
      sum(sapply(values$filteredData, is.numeric))
    
    cond_n_min <- 30
    cond_n_rec <- max(50, 5 * max(p_vars, 1))
    cond_p_min <- 2
    cond_p_rec <- 3
    
    make_badge <- function(ok, warn, label) {
      if (ok)   div(style="display:inline-block;background:#27ae60;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", icon("check"), label)
      else if (warn) div(style="display:inline-block;background:#f39c12;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", icon("exclamation-triangle"), label)
      else      div(style="display:inline-block;background:#e74c3c;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", icon("times-circle"), label)
    }
    
    n_ok   <- n_obs >= cond_n_rec
    n_warn <- !n_ok && n_obs >= cond_n_min
    n_err  <- n_obs < cond_n_min
    
    p_ok   <- p_vars >= cond_p_rec
    p_warn <- !p_ok && p_vars >= cond_p_min
    p_err  <- p_vars < cond_p_min
    
    all_ok <- n_ok && p_ok
    any_err <- n_err || p_err
    
    border_col <- if (any_err) "#e74c3c" else if (!all_ok) "#f39c12" else "#27ae60"
    bg_col     <- if (any_err) "#fdf0ef" else if (!all_ok) "#fef9ec" else "#eafaf1"
    
    msgs <- list()
    if (n_err)   msgs <- c(msgs, list(tagList(icon("times-circle", style="color:#c0392b;"), paste0(" Effectif critique : n=", n_obs, " < ", cond_n_min, " (minimum absolu). L'ACP peut être instable ou non interprétable."))))
    else if (!n_ok) msgs <- c(msgs, list(tagList(icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Effectif faible : n=", n_obs, " (recommandé min. ", cond_n_rec, " = 5xp). Résultats à interpréter avec prudence."))))
    if (p_err)   msgs <- c(msgs, list(tagList(icon("times-circle", style="color:#c0392b;"), paste0(" Variables insuffisantes : p=", p_vars, " < ", cond_p_min, " minimum. Sélectionnez au moins 2 variables."))))
    else if (!p_ok) msgs <- c(msgs, list(tagList(icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Peu de variables : p=", p_vars, " (recommandé min. ", cond_p_rec, "). L'ACP sera limitée."))))
    
    tagList(
      hr(style="margin:8px 0;"),
      div(style = paste0("border:2px solid ", border_col, "; border-radius:6px; padding:8px 12px; background:", bg_col, ";"),
          div(style="margin-bottom:6px;",
              tags$b(style="font-size:12px; color:#2c3e50;", icon("clipboard-check"), " Vérification des conditions -- ACP"),
              tags$br(),
              make_badge(n_ok, n_warn, paste0("n = ", n_obs, " observations")),
              make_badge(p_ok, p_warn, paste0("p = ", p_vars, " variables"))
          ),
          if (length(msgs) > 0)
            tagList(
              lapply(msgs, function(m)
                p(style="margin:3px 0; font-size:11px; color:#555;", m)
              ),
              if (any_err)
                div(style="margin-top:6px; padding:5px 10px; background:rgba(231,76,60,0.1); border-radius:4px;",
                    p(style="margin:0; font-size:11px; color:#c0392b; font-weight:bold;",
                      icon("exclamation-triangle"),
                      " Conditions non remplies -- vous pouvez tout de même lancer l'analyse, mais les résultats seront à interpréter avec précaution.")
                )
            )
      )
    )
  })
  
  # - Vérification des conditions HCPC -
  output$hcpcConditionsCheck <- renderUI({
    req(values$filteredData)
    
    n_obs <- nrow(values$filteredData)
    k     <- if (!is.null(input$hcpcClusters)) input$hcpcClusters else 3
    
    # Nombre de composantes retenues si ACP dispo
    n_comp_retained <- tryCatch({
      res <- pcaResultReactive()
      if (is.null(res)) return(NA_integer_)
      sum(get_eigenvalue(res)[, 1] >= 1)
    }, error = function(e) NA_integer_)
    
    cond_n_min <- 2 * k
    cond_n_rec <- 10 * k
    
    make_badge <- function(ok, warn, label) {
      if (ok)   div(style="display:inline-block;background:#27ae60;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", icon("check"), label)
      else if (warn) div(style="display:inline-block;background:#f39c12;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", icon("exclamation-triangle"), label)
      else      div(style="display:inline-block;background:#e74c3c;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", icon("times-circle"), label)
    }
    
    n_ok   <- n_obs >= cond_n_rec
    n_warn <- !n_ok && n_obs >= cond_n_min
    n_err  <- n_obs < cond_n_min
    
    comp_ok   <- !is.na(n_comp_retained) && n_comp_retained >= 2
    comp_warn <- !is.na(n_comp_retained) && n_comp_retained == 1
    
    msgs <- list()
    if (n_err)  msgs <- c(msgs, list(tagList(icon("times-circle", style="color:#c0392b;"), paste0(" Effectif critique : n=", n_obs, " < ", cond_n_min, " = 2xk. Classification impossible."))))
    else if (!n_ok) msgs <- c(msgs, list(tagList(icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Effectif faible : n=", n_obs, " (recommandé min. ", cond_n_rec, " = 10xk). Stabilité réduite."))))
    if (!is.na(n_comp_retained)) {
      if (comp_warn) msgs <- c(msgs, list(tagList(icon("exclamation-triangle", style="color:#b7770d;"), " Seulement 1 composante ACP retenue (valeur propre min. 1). Recommandé : min. 2 composantes pour une classification robuste.")))
      if (!comp_ok && !comp_warn) msgs <- c(msgs, list(tagList(icon("times-circle", style="color:#c0392b;"), " Aucune composante ACP disponible. Lancez d'abord l'ACP.")))
    }
    
    any_err  <- n_err
    border_col <- if (any_err) "#e74c3c" else if (!n_ok || comp_warn) "#f39c12" else "#27ae60"
    bg_col     <- if (any_err) "#fdf0ef" else if (!n_ok || comp_warn) "#fef9ec" else "#eafaf1"
    
    tagList(
      hr(style="margin:8px 0;"),
      div(style = paste0("border:2px solid ", border_col, "; border-radius:6px; padding:8px 12px; background:", bg_col, ";"),
          div(style="margin-bottom:6px;",
              tags$b(style="font-size:12px; color:#2c3e50;", icon("clipboard-check"), " Vérification des conditions -- HCPC"),
              tags$br(),
              make_badge(n_ok, n_warn, paste0("n = ", n_obs, " obs.")),
              make_badge(TRUE, FALSE, paste0("k = ", k, " clusters")),
              if (!is.na(n_comp_retained))
                make_badge(comp_ok, comp_warn, paste0(n_comp_retained, " comp. retenue(s)"))
          ),
          if (length(msgs) > 0)
            tagList(
              lapply(msgs, function(m) p(style="margin:3px 0; font-size:11px; color:#555;", m)),
              if (any_err)
                div(style="margin-top:6px; padding:5px 10px; background:rgba(231,76,60,0.1); border-radius:4px;",
                    p(style="margin:0; font-size:11px; color:#c0392b; font-weight:bold;",
                      icon("exclamation-triangle"),
                      " Conditions non remplies -- vous pouvez continuer, mais les résultats peuvent être non fiables."))
            )
      )
    )
  })
  
  # - Vérification des conditions AFD -
  output$afdConditionsCheck <- renderUI({
    req(values$filteredData)
    
    df <- values$filteredData
    n_obs  <- nrow(df)
    p_vars <- if (!is.null(input$afdVars)) length(input$afdVars) else
      sum(sapply(df, is.numeric))
    
    # Nombre de groupes
    n_groups <- tryCatch({
      if (!is.null(input$afdFactor) && input$afdFactor %in% names(df)) {
        length(unique(na.omit(df[[input$afdFactor]])))
      } else NA_integer_
    }, error = function(e) NA_integer_)
    
    g <- if (!is.na(n_groups)) n_groups else 2  # estimation par défaut
    
    # Conditions
    cond_n_abs  <- p_vars + g     # n > p + g - 1
    cond_n_grp  <- 20 * g         # >= 20 obs par groupe
    cond_p_min  <- 1
    cond_ratio  <- 10 * p_vars    # n/p >= 10
    
    make_badge <- function(ok, warn, label) {
      if (ok)   div(style="display:inline-block;background:#27ae60;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", icon("check"), label)
      else if (warn) div(style="display:inline-block;background:#f39c12;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", icon("exclamation-triangle"), label)
      else      div(style="display:inline-block;background:#e74c3c;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", icon("times-circle"), label)
    }
    
    n_ok_abs  <- n_obs > cond_n_abs
    n_ok_rec  <- n_obs >= cond_n_grp
    n_warn    <- n_ok_abs && !n_ok_rec
    n_err     <- !n_ok_abs
    
    g_ok      <- !is.na(n_groups) && n_groups >= 2
    p_ok      <- p_vars >= 1
    ratio_ok  <- n_obs >= cond_ratio
    ratio_warn<- !ratio_ok && n_obs >= cond_ratio / 2
    
    msgs <- list()
    if (n_err)  msgs <- c(msgs, list(tagList(icon("times-circle", style="color:#c0392b;"), paste0(" Effectif insuffisant : n=", n_obs, " inferieur ou egal a p+g=", cond_n_abs, ". L'AFD necessite n > p + g - 1."))))
    else if (!n_ok_rec) msgs <- c(msgs, list(tagList(icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Effectif faible par groupe : n=", n_obs, " pour ", g, " groupes (recommande min. 20 obs/groupe = ", cond_n_grp, " au total)."))))
    if (!is.na(n_groups) && n_groups < 2) msgs <- c(msgs, list(tagList(icon("times-circle", style="color:#c0392b;"), " Variable discriminante : moins de 2 groupes detectes. L'AFD requiert g min. 2 groupes distincts.")))
    if (!ratio_ok && !n_err) msgs <- c(msgs, list(tagList(icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Ratio n/p faible : n/p = ", round(n_obs/max(p_vars,1),1), " (recommande min. 10). Risque de sur-ajustement."))))
    
    any_err    <- n_err || (!is.na(n_groups) && n_groups < 2)
    border_col <- if (any_err) "#e74c3c" else if (!n_ok_rec || !ratio_ok) "#f39c12" else "#27ae60"
    bg_col     <- if (any_err) "#fdf0ef" else if (!n_ok_rec || !ratio_ok) "#fef9ec" else "#eafaf1"
    
    tagList(
      hr(style="margin:8px 0;"),
      div(style = paste0("border:2px solid ", border_col, "; border-radius:6px; padding:8px 12px; background:", bg_col, ";"),
          div(style="margin-bottom:6px;",
              tags$b(style="font-size:12px; color:#2c3e50;", icon("clipboard-check"), " Vérification des conditions -- AFD"),
              tags$br(),
              make_badge(n_ok_abs, n_warn, paste0("n = ", n_obs, " observations")),
              make_badge(p_ok, FALSE, paste0("p = ", p_vars, " variables")),
              if (!is.na(n_groups))
                make_badge(g_ok, FALSE, paste0("g = ", n_groups, " groupes")),
              make_badge(ratio_ok, ratio_warn, paste0("n/p = ", round(n_obs/max(p_vars,1),1)))
          ),
          if (length(msgs) > 0)
            tagList(
              lapply(msgs, function(m) p(style="margin:3px 0; font-size:11px; color:#555;", m)),
              div(
                style = paste0("margin-top:6px; padding:5px 10px; border-radius:4px; background:", if (any_err) "rgba(231,76,60,0.1);" else "rgba(243,156,18,0.1);"),
                p(style = paste0("margin:0; font-size:11px; font-weight:bold; color:", if(any_err) "#c0392b;" else "#856404;"),
                  icon("exclamation-triangle"),
                  " Conditions non remplies -- vous pouvez tout de meme lancer l'AFD, mais les resultats sont a interpreter avec grande prudence.")
              )
            )
      )
    )
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
    
    # - Choix de la coloration -
    color_choice <- if (!is.null(input$pcaColorBy)) input$pcaColorBy else "contrib"
    
    # Calcul de l'indice de saturation (|corrélation| moyenne sur les 2 axes)
    compute_sat_var <- function(res, ax_x, ax_y) {
      cor_mat <- res$var$cor
      n_dim   <- ncol(cor_mat)
      ax_x_s  <- min(ax_x, n_dim)
      ax_y_s  <- min(ax_y, n_dim)
      if (ax_x_s == ax_y_s) return(abs(cor_mat[, ax_x_s]))
      rowMeans(abs(cor_mat[, c(ax_x_s, ax_y_s), drop = FALSE]))
    }
    compute_sat_ind <- function(res, ax_x, ax_y) {
      cos2_mat <- res$ind$cos2
      n_dim    <- ncol(cos2_mat)
      ax_x_s   <- min(ax_x, n_dim)
      ax_y_s   <- min(ax_y, n_dim)
      if (ax_x_s == ax_y_s) return(cos2_mat[, ax_x_s])
      rowSums(cos2_mat[, c(ax_x_s, ax_y_s), drop = FALSE])
    }
    
    col_var <- switch(color_choice,
                      "contrib" = "contrib",
                      "cos2"    = "cos2",
                      "sat"     = compute_sat_var(res.pca, axis_x, axis_y),
                      "contrib"
    )
    col_ind <- switch(color_choice,
                      "contrib" = "contrib",
                      "cos2"    = "cos2",
                      "sat"     = compute_sat_ind(res.pca, axis_x, axis_y),
                      "contrib"
    )
    
    gradient_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
    
    if (input$pcaPlotType == "var") {
      p <- fviz_pca_var(res.pca,
                        axes = c(axis_x, axis_y),
                        col.var = col_var,
                        gradient.cols = gradient_cols,
                        repel = TRUE,
                        ggtheme = theme_minimal(),
                        title = plot_title)
    } else if (input$pcaPlotType == "ind") {
      p <- fviz_pca_ind(res.pca,
                        axes = c(axis_x, axis_y),
                        col.ind = col_ind,
                        gradient.cols = gradient_cols,
                        repel = TRUE,
                        ggtheme = theme_minimal(),
                        title = plot_title)
    } else {
      # Biplot : on colore les variables selon le critère choisi
      if (is.numeric(col_var)) {
        # Pour saturation (vecteur numérique), on passe la moyenne comme couleur fixe
        # et on ajoute une annotation dans le titre
        p <- fviz_pca_biplot(res.pca,
                             axes = c(axis_x, axis_y),
                             repel = TRUE,
                             col.var = col_var,
                             col.ind = col_ind,
                             gradient.cols = gradient_cols,
                             ggtheme = theme_minimal(),
                             title = plot_title)
      } else {
        p <- fviz_pca_biplot(res.pca,
                             axes = c(axis_x, axis_y),
                             repel = TRUE,
                             col.var = col_var,
                             col.ind = col_ind,
                             gradient.cols = gradient_cols,
                             ggtheme = theme_minimal(),
                             title = plot_title)
      }
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
    
    # Affiche un tableau numerique avec EXACTEMENT 'dec' decimales pour
    # chaque valeur (alignement homogene, aucune troncature).
    print_fixed <- function(m, digits) {
      m <- as.matrix(m)
      out <- format(round(m, digits), nsmall = digits, scientific = FALSE,
                    trim = FALSE)
      dimnames(out) <- dimnames(m)
      print(noquote(out))
    }
    
    cat("=== ANALYSE EN COMPOSANTES PRINCIPALES (ACP) ===\n\n")
    
    eigenvals <- get_eigenvalue(res.pca)
    cat("Variance expliquee par les composantes principales:\n")
    print_fixed(eigenvals, dec)
    cat("\n")
    
    cat("Contribution des variables aux composantes principales:\n")
    print_fixed(res.pca$var$contrib, dec)
    cat("\n")
    
    cat("Indice de saturation (correlation variables-axes):\n")
    cat("------------------------------------------------------------\n")
    cat("Correlation de chaque variable avec chaque axe principal.\n")
    cat("Seuils de lecture (en valeur absolue) :\n")
    cat("  - |saturation| >= 0.70 : variable fortement liee a l'axe\n")
    cat("  - |saturation| 0.50 a 0.70 : liaison moderee\n")
    cat("  - |saturation| < 0.50 : liaison faible\n")
    cat("------------------------------------------------------------\n\n")
    saturations <- res.pca$var$coord
    print_fixed(saturations, dec)
    cat("\n")
    
    # Variable la mieux corrélée a chacun des deux premiers axes
    n_axes <- min(2, ncol(saturations))
    for (ax in seq_len(n_axes)) {
      sat_ax <- saturations[, ax]
      best   <- names(which.max(abs(sat_ax)))
      cat(sprintf("  Axe %d : variable la plus saturante = %s (saturation = %s)\n",
                  ax, best,
                  format(round(sat_ax[best], dec), nsmall = dec, scientific = FALSE)))
    }
    cat("\n")
    
    cat("Qualite de representation (cos2) des variables:\n")
    print_fixed(res.pca$var$cos2, dec)
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
      if (is.null(R)) return(div(class="callout callout-warning", h4(icon("exclamation-triangle"), " Données insuffisantes"), p("Variables à variance nulle détectées -- vérifiez vos données.")))
      n <- nrow(pca_data_raw)
      p <- ncol(pca_data_raw)
      
      # Test de sphéricité de Bartlett
      bartlett <- suppressWarnings(cortest.bartlett(R, n = n))
      chi2_val  <- round(bartlett$chisq, 3)
      df_val    <- bartlett$df
      p_val     <- bartlett$p.value          # valeur brute, non arrondie
      # Affichage de la p-value : jamais un "0" trompeur pour les tres
      # petites valeurs ; notation decimale complete ou scientifique.
      p_display <- if (is.na(p_val)) {
        "n/d"
      } else if (p_val == 0 || p_val < 1e-15) {
        "< 1e-15"
      } else if (p_val < 1e-4) {
        format(p_val, scientific = TRUE, digits = 4)
      } else {
        format(round(p_val, 6), scientific = FALSE, nsmall = 6)
      }
      
      # KMO (suppressWarnings évite "FUN(min) Inf" quand corrélations <= 0)
      kmo_res  <- suppressWarnings(KMO(R))
      kmo_val  <- round(kmo_res$MSA, 3)
      
      kmo_label <- if (kmo_val <= 0.5) {
        list(txt = "Inacceptable (< 0,5) -- L'ACP n'est pas recommandée sur ces données.", color = "#dc3545", icon = "times-circle")
      } else if (kmo_val <= 0.6) {
        list(txt = "Médiocre (0,5 - 0,6) -- L'ACP est déconseillée.", color = "#e67e22", icon = "exclamation-circle")
      } else if (kmo_val <= 0.7) {
        list(txt = "Acceptable (0,6 - 0,7) -- L'ACP est utilisable avec prudence.", color = "#f39c12", icon = "exclamation-triangle")
      } else if (kmo_val <= 0.8) {
        list(txt = "Souhaitable (0,7 - 0,8) -- L'ACP est appropriée.", color = "#3498db", icon = "check-circle")
      } else if (kmo_val <= 0.9) {
        list(txt = "Bon (0,8 - 0,9) -- L'ACP est bien adaptée.", color = "#27ae60", icon = "check-circle")
      } else {
        list(txt = "Excellent (> 0,9) -- L'ACP est parfaitement adaptée.", color = "#1a5276", icon = "star")
      }
      
      bartlett_interp <- if (p_val < 0.05) {
        list(txt = paste0("Significatif (p = ", p_display, ") -- La matrice de corrélation n'est pas une matrice identité : l'ACP est justifiée."), 
             color = "#27ae60", icon = "check-circle")
      } else {
        list(txt = paste0("Non significatif (p = ", p_display, ") -- Les variables semblent indépendantes. L'ACP n'apportera pas de structure factorielle utile."), 
             color = "#dc3545", icon = "times-circle")
      }
      
      tagList(
        div(style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 8px; padding: 18px; margin-bottom: 15px; border: 1px solid #dee2e6;",
            h5(style = "color: #2c3e50; font-weight: bold; margin-top: 0; border-bottom: 2px solid #3498db; padding-bottom: 8px;",
               icon("flask"), " Test de sphéricité de Bartlett"),
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
                            h4(style = paste0("margin: 4px 0; font-weight: bold; color: ", bartlett_interp$color, ";"), p_display)))
            ),
            div(style = paste0("margin-top: 10px; padding: 8px 12px; border-left: 4px solid ", bartlett_interp$color, "; background-color: white; border-radius: 0 4px 4px 0;"),
                p(style = paste0("margin: 0; font-size: 12px; color: ", bartlett_interp$color, ";"),
                  icon(bartlett_interp$icon), " ", bartlett_interp$txt))
        ),
        div(style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 8px; padding: 18px; margin-bottom: 15px; border: 1px solid #dee2e6;",
            h5(style = "color: #2c3e50; font-weight: bold; margin-top: 0; border-bottom: 2px solid #8e44ad; padding-bottom: 8px;",
               icon("sliders"), " Indice KMO (Kaiser-Meyer-Olkin)"),
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
    
    eig_mat       <- res.pca$eig
    eigenvals_obs <- as.numeric(eig_mat[, 1])
    
    # FactoMineR peut renvoyer moins (ncp) ou un nombre de valeurs propres
    # different de p (colonnes a variance nulle, etc.). On aligne tout sur
    # le nombre commun de composantes pour eviter tout decalage de longueur.
    n_pc <- min(length(eigenvals_obs), p)
    if (n_pc < 2)
      stop("Nombre de composantes insuffisant pour l'analyse parallèle (minimum 2).")
    eigenvals_obs <- eigenvals_obs[seq_len(n_pc)]
    
    hstat_set_seed(input$globalSeed)
    sim_eigenvals <- matrix(0, nrow = n_iter, ncol = n_pc)
    for (i in 1:n_iter) {
      random_data <- matrix(rnorm(n * p), nrow = n, ncol = p)
      random_pca  <- prcomp(random_data, scale. = TRUE)
      ev <- random_pca$sdev^2
      sim_eigenvals[i, ] <- ev[seq_len(n_pc)]
    }
    
    mean_sim   <- colMeans(sim_eigenvals)
    perc95_sim <- apply(sim_eigenvals, 2, quantile, probs = 0.95)
    n_retain   <- sum(eigenvals_obs > perc95_sim)
    
    df_plot <- data.frame(
      PC            = seq_len(n_pc),
      Observees     = eigenvals_obs,
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
      scale_x_continuous(breaks = seq_len(n_pc)) +
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
      
      # - Gardes avant fa() 
      
      n_vars    <- ncol(pca_data_raw)
      n_obs     <- nrow(pca_data_raw)
      
      if (n_vars < 2) {
        cat("Moins de 2 variables avec variance non nulle -- rotation impossible.
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
        cat("Impossible de calculer la matrice de corrélation (NA persistants après na.omit).
")
        cat("  Conseil : vérifiez vos données (valeurs manquantes, variables constantes).
")
        return(invisible(NULL))
      }
      det_val <- tryCatch(det(R_mat), error = function(e) NA)
      
      is_singular <- !is.na(det_val) && abs(det_val) < 1e-8
      # Si singulière, utiliser "minres" qui est plus robuste que "pa" (pas de SMC)
      fm_method <- if (is_singular) "minres" else "pa"
      
      cat("=== ROTATION", toupper(method), "--", n_factors, "FACTEUR(S) ===

")
      cat("La rotation", method, "est une rotation orthogonale qui conserve l'indépendance des axes.
")
      cat("Elle simplifie la structure factorielle pour faciliter l'interprétation.

")
      if (is_singular) {
        cat("Note : matrice de corrélation quasi-singulière détectée.
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
            cat("Échec de la rotation :", e2$message, "
")
            cat("  Conseil : réduisez le nombre de facteurs ou changez de méthode.
")
            NULL
          }
        )
      })
      
      if (is.null(fa_res)) return(invisible(NULL))
      
      if (n_factors >= n_vars) {
        cat("Avertissement : nombre de facteurs trop élevé par rapport aux variables -- cas Heywood possible.
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
  
  # ACP -- Export métriques
  
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
  
  # HCPC -- Export métriques
  
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
  
  # AFD -- Export métriques
  
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
                                     signif(bartlett$p.value, 6), round(kmo_res$MSA, 3)),
        Interpretation           = c(
          ifelse(bartlett$p.value < 0.05, "Significatif -- ACP justifiée", "Non significatif -- ACP non justifiée"),
          "",
          ifelse(bartlett$p.value < 0.05, "p < 0.05 : matrice != identité", "p >= 0.05 : variables indépendantes"),
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
      hstat_set_seed(input$globalSeed)
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
      db_interp <- if (DB < 0.5) "Excellent (< 0,5) -- clusters bien séparés et compacts"
      else if (DB < 1.0) "Acceptable (0,5 - 1,0) -- séparation modérée"
      else "Faible (> 1,0) -- clusters qui se chevauchent"
      
      sil_color <- if (sil_mean >= 0.7) "#27ae60" else if (sil_mean >= 0.5) "#3498db" 
      else if (sil_mean >= 0.25) "#f39c12" else "#dc3545"
      sil_interp <- if (sil_mean >= 0.7) "Excellente structure (>= 0,70)"
      else if (sil_mean >= 0.5) "Structure raisonnable (0,50 - 0,70)"
      else if (sil_mean >= 0.25) "Structure faible (0,25 - 0,50)"
      else "Pas de structure substantielle (< 0,25)"
      
      coph_color <- if (coph_corr >= 0.80) "#27ae60" else if (coph_corr >= 0.75) "#3498db" else "#dc3545"
      coph_interp <- if (coph_corr >= 0.80) "Très bonne représentation (>= 0,80) -- dendrogramme fidèle"
      else if (coph_corr >= 0.75) "Représentation acceptable (0,75 - 0,80)"
      else "Représentation médiocre (< 0,75) -- le dendrogramme déforme les distances"
      
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
              "Seuils : > 0,80 très bonne | 0,75 - 0,80 acceptable | < 0,75 médiocre")
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
        hstat_set_seed(input$globalSeed)
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
      stab_interp <- if (mean_rand >= 0.9) "Excellente stabilité (>= 0,90) -- la partition est très reproductible."
      else if (mean_rand >= 0.8) "Bonne stabilité (0,80 - 0,90) -- la partition est fiable."
      else if (mean_rand >= 0.7) "Stabilité modérée (0,70 - 0,80) -- à interpréter avec prudence."
      else "Faible stabilité (< 0,70) -- la partition est sensible à la composition de l'échantillon."
      
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
    
    # - Regrouper les colonnes par type avec étiquettes claires 
    build_group_choices <- function(df) {
      cols_factor  <- names(df)[sapply(df, is.factor)]
      cols_char    <- names(df)[sapply(df, is.character)]
      cols_date    <- names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
      # Numériques avec <= 30 valeurs uniques -> utilisables comme groupes
      cols_num_few <- names(df)[sapply(df, function(x) {
        is.numeric(x) && length(unique(na.omit(x))) <= 30
      })]
      # Logiques
      cols_logi    <- names(df)[sapply(df, is.logical)]
      
      groups <- list()
      if (length(cols_factor)  > 0) groups[["Facteur"]]                             <- cols_factor
      if (length(cols_char)    > 0) groups[["Texte / Caractère"]]                   <- cols_char
      if (length(cols_date)    > 0) groups[["Date"]]                                <- cols_date
      if (length(cols_num_few) > 0) groups[["Numérique (<= 30 niveaux))"]]           <- cols_num_few
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
    type_str <- if (is.factor(col))      paste0("Facteur -- ", n_lvl, " niveaux")
    else if (is.character(col)) paste0("Texte -- ", n_lvl, " valeurs uniques")
    else if (inherits(col,"Date") || inherits(col,"POSIXt")) paste0("Date -- ", n_lvl, " valeurs uniques")
    else if (is.numeric(col))  paste0("Numérique -- converti en facteur (", n_lvl, " niveaux)")
    else if (is.logical(col))  "Logique -- 2 niveaux (TRUE / FALSE)"
    else paste0("Type : ", class(col)[1], " -- ", n_lvl, " niveaux")
    
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
  
  # - Panel colinéarité AFD 
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
    sev_label <- if (length(perfect_col)>0) "Colinéarité parfaite" else if (length(high_vif)>0) "Colinéarité forte (VIF > 5)" else "Colinéarité modérée (|r| >= 0.80)"
    
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
              lbl <- if(is.infinite(vv)) "Inf" else round(vv,1)
              tags$span(style=paste0("background:",col,";color:white;border-radius:3px;padding:2px 6px;font-size:11px;"), paste0(v,": ",lbl))
            })),
        tags$small(style="color:#6c757d;display:block;margin-bottom:8px;",
                   "VIF < 5 : acceptable · 5-10 : élevé · > 10 : très élevé")
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
      
      # - Détecter et éliminer les variables colinéaires avant lda() 
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
                paste0("AFD : variables colinéaires exclues automatiquement -- ",
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
      vars_used <- names(afd_data)[sapply(afd_data, is.numeric)]
    }
    if (is.null(vars_used) || length(vars_used) == 0) stop("Aucune variable disponible pour le graphique AFD.")
    
    X_std <- scale(afd_data[, vars_used, drop = FALSE])
    scores <- as.matrix(X_std) %*% afd_result$scaling
    structure_matrix <- cor(X_std, scores)
    
    var_df <- as.data.frame(structure_matrix)
    var_df$Variable <- rownames(structure_matrix)
    
    # - Importance discriminatoire globale -
    # Pondération par la variance expliquée de chaque fonction LD
    eigenvals  <- afd_result$svd^2
    prop_var   <- eigenvals / sum(eigenvals)   # poids de chaque LD
    n_ld_use   <- min(n_dims, ncol(structure_matrix))
    # Score d'importance = somme pondérée de |corrélation|² sur toutes les fonctions
    importance <- sapply(seq_len(nrow(structure_matrix)), function(i) {
      sqrt(sum(prop_var[seq_len(n_ld_use)] * structure_matrix[i, seq_len(n_ld_use)]^2))
    })
    var_df$Importance <- importance
    
    # Niveaux de discrimination (tranches fixes)
    var_df$Niveau <- cut(
      var_df$Importance,
      breaks = c(-Inf, 0.30, 0.50, 0.70, Inf),
      labels = c("Faible (< 0.30)", "Modérée (0.30-0.50)", "Forte (0.50-0.70)", "Très forte (> 0.70)")
    )
    disc_colors <- c(
      "Faible (< 0.30)"       = "#bdc3c7",
      "Modérée (0.30-0.50)"   = "#3498db",
      "Forte (0.50-0.70)"     = "#e67e22",
      "Très forte (> 0.70)"   = "#c0392b"
    )
    
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
      # - Graphique 2D : flèches colorées par importance -
      x_col <- paste0("LD", axis_x)
      y_col <- paste0("LD", axis_y)
      
      p_var <- ggplot(var_df, aes_string(x = x_col, y = y_col,
                                         color = "Niveau", label = "Variable")) +
        geom_segment(aes_string(xend = x_col, yend = y_col, color = "Niveau"),
                     x = 0, y = 0,
                     arrow = arrow(length = unit(0.28, "cm"), type = "closed"),
                     linewidth = 1.3) +
        geom_point(aes_string(size = "Importance"), alpha = 0.85) +
        geom_text(vjust = -0.6, hjust = 0.5, size = 3.8, fontface = "bold",
                  color = "grey20") +
        scale_color_manual(values = disc_colors, name = "Importance\ndiscriminatoire",
                           drop = FALSE) +
        scale_size_continuous(range = c(2, 6), guide = "none") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
        theme_minimal(base_size = 12) +
        labs(title = var_title,
             subtitle = "Couleur = importance discriminatoire globale (corrélation pondérée par la variance de chaque LD)",
             x = x_label, y = y_label) +
        theme(
          plot.title    = element_markdown(hjust = 0.5, face = "bold", size = 13),
          plot.subtitle = element_text(hjust = 0.5, color = "#555", size = 10),
          legend.position  = "right",
          legend.title     = element_text(size = 10, face = "bold"),
          panel.grid.minor = element_blank()
        )
      
      if (!is.null(input$afdVarCenterAxes) && input$afdVarCenterAxes) {
        coords    <- structure_matrix[, c(axis_x, axis_y)]
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p_var     <- p_var + xlim(-max_range, max_range) + ylim(-max_range, max_range)
      }
      
    } else {
      # - Graphique 1D : barres colorées par importance -
      x_col <- paste0("LD", axis_x)
      
      var_df_ordered <- var_df[order(var_df$Importance, decreasing = TRUE), ]
      var_df_ordered$Variable <- factor(var_df_ordered$Variable,
                                        levels = rev(var_df_ordered$Variable))
      
      p_var <- ggplot(var_df_ordered,
                      aes_string(x = "Variable", y = x_col, fill = "Niveau")) +
        geom_bar(stat = "identity", color = "white", linewidth = 0.4) +
        coord_flip() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey40",
                   linewidth = 0.7) +
        geom_hline(yintercept =  0.30, linetype = "dotted", color = "#3498db",
                   linewidth = 0.5, alpha = 0.7) +
        geom_hline(yintercept = -0.30, linetype = "dotted", color = "#3498db",
                   linewidth = 0.5, alpha = 0.7) +
        annotate("text", x = 0.6, y = 0.32, label = "seuil 0.30",
                 hjust = 0, color = "#3498db", size = 3, fontface = "italic") +
        scale_fill_manual(values = disc_colors, name = "Importance\ndiscriminatoire",
                          drop = FALSE) +
        scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
        labs(title = var_title,
             subtitle = "Couleur = niveau d'importance discriminatoire (|corrélation| pondérée par la variance expliquée)",
             x = NULL, y = y_label) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title    = element_markdown(hjust = 0.5, face = "bold", size = 13),
          plot.subtitle = element_text(hjust = 0.5, color = "#555", size = 10),
          legend.position  = "right",
          legend.title     = element_text(size = 10, face = "bold"),
          panel.grid.minor = element_blank()
        )
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
      # - Calculs centraux 
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
      
      # - Helpers de style 
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
           icon(icon_name), paste0("  ", label))
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
      
      # - Couleurs dynamiques 
      acc_color <- if (accuracy >= .9) "#3a7d5c" else if (accuracy >= .8) "#4a7fa5" else if (accuracy >= .7) "#b07d2a" else "#c0392b"
      acc_interp <- if (accuracy >= .9) "Excellent -- la discrimination est quasi-parfaite."
      else if (accuracy >= .8) "Bon -- la discrimination est fiable."
      else if (accuracy >= .7) "Acceptable -- des erreurs subsistent."
      else "Faible -- le modele discrimine mal les groupes."
      
      kappa_color <- if (kappa_val >= .8) "#3a7d5c" else if (kappa_val >= .6) "#4a7fa5" else if (kappa_val >= .4) "#b07d2a" else "#c0392b"
      kappa_interp <- if (kappa_val >= .8) "Quasi-parfait (>= 0,80) -- accord excellent au-dela du hasard."
      else if (kappa_val >= .6) "Substantiel (0,60 - 0,80) -- bon accord."
      else if (kappa_val >= .4) "Modere (0,40 - 0,60) -- accord partiel."
      else if (kappa_val >= .2) "Passable (0,20 - 0,40) -- accord faible."
      else "Faible (< 0,20) -- accord proche du hasard."
      
      eta2_colors <- sapply(eta2_vals, function(e)
        if (e >= .64) "#3a5f7d" else if (e >= .25) "#3a7d5c" else if (e >= .09) "#b07d2a" else "#c0392b")
      eta2_interps <- sapply(eta2_vals, function(e)
        if (e >= .64) "Excellent (>= 0,64)" else if (e >= .25) "Fort (0,25 - 0,64)"
        else if (e >= .09) "Modere (0,09 - 0,25)" else "Faible (< 0,09)")
      
      # - RENDU UI 
      tagList(
        
        # -
        # SECTION 1 -- VARIANCE EXPLIQUÉE + ETA²
        # -
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
                                    paste0(round(eta2_vals[i], dec), " -- ", eta2_interps[i]))
                            )
                          })
                        )
             ),
             div(style = "margin-top:10px; padding:8px; background:#eef4f9; border-radius:6px;",
                 p(style = "margin:0; font-size:13px; color:#3a6186;",
                   icon("ruler"), "  Seuils eta\u00b2 : ",
                   tags$strong("Excellent"), " \u2265 0,64  |  ",
                   tags$strong("Fort"), " 0,25-0,64  |  ",
                   tags$strong("Modéré"), " 0,09-0,25  |  ",
                   tags$strong("Faible"), " < 0,09"
                 )
             )
        ),
        
        # -
        # SECTION 2 -- ACCURACY & KAPPA
        # -
        card(border_color = "#4a8c6f",
             section_header("2", "Qualité de classification -- Accuracy & Kappa de Cohen", "#4a8c6f", "bullseye"),
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
                           interp_bar("Seuils Kappa : \u2265 0,80 quasi-parfait | 0,60-0,80 substantiel | 0,40-0,60 modéré", "#6c5b8e")
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
                   tags$span(style = "color:#3a7d5c;", "\u2265 90% -- Excellent"), " | ",
                   tags$span(style = "color:#b07d2a;", "70-90% -- Acceptable"), " | ",
                   tags$span(style = "color:#c0392b;", "< 70% -- Faible")
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
        
        # -
        # SECTION 3 -- MATRICE DE CONFUSION
        # -
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
        
        # -
        # SECTION 4 -- MATRICE DE STRUCTURE
        # -
        card(border_color = "#6c5b8e",
             section_header("4", "Matrice de structure -- Corrélations variables-fonctions", "#6c5b8e", "project-diagram"),
             info_note("Les corrélations indiquent la contribution de chaque variable aux fonctions discriminantes. |r| \u2265 0,70 : forte | 0,40-0,70 : modérée | < 0,40 : faible."),
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
        
        # -
        # SECTION 5 -- VALIDATION CROISÉE
        # -
        card(border_color = "#3a8070",
             section_header("5", "Validation croisée Leave-One-Out (LOO)", "#3a8070", "sync-alt"),
             if (!is.null(afd_res$cv_results)) {
               cv_conf    <- table(Reel = afd_data[[input$afdFactor]], Predit = afd_res$cv_results$predictions)
               cv_acc     <- sum(diag(cv_conf)) / sum(cv_conf)
               cv_col     <- if (cv_acc >= .9) "#3a7d5c" else if (cv_acc >= .8) "#4a7fa5" else if (cv_acc >= .7) "#b07d2a" else "#c0392b"
               cv_interp  <- if (cv_acc >= .9) "Excellente capacité prédictive (>= 90%)."
               else if (cv_acc >= .8) "Bonne capacité prédictive (80-90%)."
               else if (cv_acc >= .7) "Capacité prédictive acceptable (70-80%)."
               else "Capacité prédictive faible (< 70%) -- risque de sur-ajustement."
               bias       <- accuracy - cv_acc
               bias_interp <- if (abs(bias) < 0.03) "Différence négligeable -- modèle stable."
               else if (abs(bias) < 0.08) "Légère différence -- sur-ajustement modéré."
               else "Différence importante -- attention au sur-ajustement."
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
        
        # -
        # SECTION 6 -- CENTROIDES & PROBABILITÉS A PRIORI
        # -
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
  # ============================================================================
  # ---- Analyses multivariees etendues (quanti / quali / mixtes) ----
  # ============================================================================

  # -- Packages optionnels (installation differee) --
  local({
    opt <- c("lavaan", "pls", "klaR", "poLCA", "clustMixType", "nnet")
    inst <- tryCatch(rownames(installed.packages()), error = function(e) character(0))
    miss <- opt[!opt %in% inst]
    if (length(miss) > 0)
      try(install.packages(miss, repos = "https://cran.r-project.org", quiet = TRUE),
          silent = TRUE)
  })
  mv_has <- function(p) isTRUE(requireNamespace(p, quietly = TRUE))

  # -- Stockage des resultats --
  mv_res <- reactiveValues()

  # -- Helpers donnees --
  mv_data <- reactive(values$filteredData)
  mv_num_cols <- reactive({
    d <- mv_data(); if (is.null(d)) return(character(0))
    names(d)[sapply(d, is.numeric)]
  })
  mv_cat_cols <- reactive({
    d <- mv_data(); if (is.null(d)) return(character(0))
    names(d)[sapply(d, function(x) is.factor(x) || is.character(x) || is.logical(x))]
  })
  mv_subsample <- function(df, cap = 5000) {
    if (nrow(df) <= cap) return(df)
    df[sort(sample(seq_len(nrow(df)), cap)), , drop = FALSE]
  }

  # -- Couleurs / icones d'etat --
  mv_col <- function(st) switch(st,
    ok = "#27ae60", warn = "#f39c12", err = "#e74c3c", info = "#2980b9", "#7f8c8d")
  mv_ic <- function(st) switch(st,
    ok = "check-circle", warn = "exclamation-triangle", err = "times-circle",
    info = "info-circle", "circle")

  # -- Carte stylisee (style ACP/AFD) --
  mv_card <- function(..., border_color = "#dee2e6", bg = "white") {
    div(style = paste0("background:", bg, "; border-radius:8px; padding:16px; margin-bottom:14px;",
                       " border:1px solid ", border_color, "; box-shadow:0 1px 4px rgba(0,0,0,.06);"),
        ...)
  }
  mv_section_header <- function(label, color = "#3a6186", icon_name = "chart-bar") {
    div(style = paste0("background:linear-gradient(135deg,", color, " 0%,", color,
                       "cc 100%); border-radius:6px; padding:11px 16px; margin-bottom:12px;"),
        h5(style = "color:white; margin:0; font-weight:bold; font-size:15px;",
           icon(icon_name), paste0("  ", label)))
  }
  mv_info_note <- function(text) {
    p(style = "font-size:13px; color:#5a6a7a; font-style:italic; margin:6px 0 10px 0;", text)
  }
  mv_interp_bar <- function(text, color) {
    div(style = paste0("margin-top:8px; padding:8px 14px; border-left:4px solid ", color,
                       "; background:#f4f6f8; border-radius:0 4px 4px 0;"),
        p(style = "margin:0; font-size:13px; color:#2c3e50; font-weight:500;",
          icon("info-circle"), " ", text))
  }

  # -- Tableau de metriques structure (Metrique / Valeur / Seuil / Interpretation) --
  # df : Metrique, Valeur, Seuil, Interpretation, Statut
  mv_metrics_table <- function(df, accent = "#3a6186") {
    if (is.null(df) || nrow(df) == 0)
      return(p(style = "color:#888; font-style:italic;",
               icon("info-circle"), " Aucune metrique disponible."))
    rows <- lapply(seq_len(nrow(df)), function(i) {
      st <- df$Statut[i]; bg <- if (i %% 2 == 0) "#f4f6f8" else "white"
      tags$tr(style = paste0("background:", bg, ";"),
        tags$td(style = "padding:8px 10px; font-weight:bold; font-size:12px; color:#2c3e50; border-bottom:1px solid #ecf0f1;",
                df$Metrique[i]),
        tags$td(style = "padding:8px 10px; font-family:monospace; font-size:13px; text-align:center; border-bottom:1px solid #ecf0f1;",
                df$Valeur[i]),
        tags$td(style = "padding:8px 10px; font-size:11px; color:#666; border-bottom:1px solid #ecf0f1;",
                df$Seuil[i]),
        tags$td(style = paste0("padding:8px 10px; font-size:12px; font-weight:bold; border-bottom:1px solid #ecf0f1; color:",
                               mv_col(st), ";"),
                icon(mv_ic(st)), " ", df$Interpretation[i]))
    })
    tags$table(style = "width:100%; border-collapse:collapse; margin-top:6px;",
      tags$thead(tags$tr(style = paste0("background:", accent, "; color:white;"),
        tags$th(style = "padding:8px 10px; text-align:left; font-size:11px; border-radius:4px 0 0 0;", "Metrique"),
        tags$th(style = "padding:8px 10px; text-align:center; font-size:11px;", "Valeur"),
        tags$th(style = "padding:8px 10px; text-align:left; font-size:11px;", "Seuil de reference"),
        tags$th(style = "padding:8px 10px; text-align:left; font-size:11px; border-radius:0 4px 0 0;", "Interpretation"))),
      tags$tbody(rows))
  }

  # -- Tableau generique (data.frame -> table HTML compacte) --
  mv_data_table <- function(df, accent = "#3a6186", digits = 3) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    nums <- sapply(df, is.numeric)
    df2 <- df
    df2[nums] <- lapply(df2[nums], function(x) round(x, digits))
    rows <- lapply(seq_len(nrow(df2)), function(i) {
      bg <- if (i %% 2 == 0) "#f4f6f8" else "white"
      tags$tr(style = paste0("background:", bg, ";"),
        lapply(seq_len(ncol(df2)), function(j)
          tags$td(style = "padding:6px 10px; font-size:12px; text-align:center; border-bottom:1px solid #ecf0f1;",
                  as.character(df2[i, j]))))
    })
    tags$table(style = "width:100%; border-collapse:collapse; margin-top:6px;",
      tags$thead(tags$tr(style = paste0("background:", accent, "; color:white;"),
        lapply(names(df2), function(nm)
          tags$th(style = "padding:7px 10px; text-align:center; font-size:11px;", nm)))),
      tags$tbody(rows))
  }

  # -- Message de statut --
  mv_status_box <- function(type, text) {
    col <- mv_col(if (type == "err") "err" else if (type == "warn") "warn" else "info")
    bg  <- if (type == "err") "#fdf0ef" else if (type == "warn") "#fef9ec" else "#eaf2f8"
    div(style = paste0("border-left:4px solid ", col, "; background:", bg,
                       "; padding:10px 14px; margin:8px 0; border-radius:0 4px 4px 0;"),
        p(style = paste0("margin:0; color:", col, "; font-size:13px; font-weight:bold;"),
          icon(mv_ic(if (type == "err") "err" else if (type == "warn") "warn" else "info")),
          " ", text))
  }

  # -- Badge de condition --
  mv_badge <- function(st, label) {
    div(style = paste0("display:inline-block; background:", mv_col(st),
                       "; color:white; border-radius:4px; padding:2px 8px;",
                       " font-size:11px; margin:2px;"),
        icon(mv_ic(st)), " ", label)
  }
  mv_cond_render <- function(title, badges, msgs, level) {
    border <- mv_col(level); bg <- switch(level, ok = "#eafaf1", warn = "#fef9ec", "#fdf0ef")
    tagList(
      hr(style = "margin:8px 0;"),
      div(style = paste0("border:2px solid ", border,
                         "; border-radius:6px; padding:8px 12px; background:", bg, ";"),
        div(style = "margin-bottom:6px;",
          tags$b(style = "font-size:12px; color:#2c3e50;", icon("clipboard-check"), " ", title),
          tags$br(), badges),
        if (length(msgs) > 0)
          tagList(lapply(msgs, function(m)
            p(style = "margin:3px 0; font-size:11px; color:#555;", m)),
            if (level == "err")
              div(style = "margin-top:6px; padding:5px 10px; background:rgba(231,76,60,0.1); border-radius:4px;",
                  p(style = "margin:0; font-size:11px; color:#c0392b; font-weight:bold;",
                    icon("exclamation-triangle"),
                    " Conditions non remplies -- resultats a interpreter avec prudence."))))
    )
  }

  # -- Constructeur de ligne de metrique --
  mv_row <- function(metrique, valeur, seuil, interpretation, statut)
    data.frame(Metrique = metrique, Valeur = as.character(valeur),
               Seuil = seuil, Interpretation = interpretation,
               Statut = statut, stringsAsFactors = FALSE)

  # -- Theme ggplot2 commun (style ACP/HCPC/AFD) --
  mv_gg_theme <- function() {
    theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold", size = 14, color = "#2c3e50"),
        plot.subtitle = element_text(hjust = 0.5, color = "#555", size = 11),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
      )
  }
  mv_empty_plot <- function(msg = "Lancez l'analyse pour afficher le graphique.") {
    ggplot() + annotate("text", x = 0, y = 0, label = msg, size = 5, color = "#888") +
      theme_void()
  }


  # -- Enregistrement generique des sorties --
  mv_register <- function(key, accent = "#3a6186") {
    output[[paste0("mv_", key, "_status")]] <- renderUI({
      r <- mv_res[[key]]
      if (is.null(r)) return(mv_status_box("info",
        "Configurez les parametres puis cliquez sur 'Lancer l'analyse'."))
      if (isFALSE(r$ok)) return(mv_status_box("err", r$error))
      mv_status_box("info", if (!is.null(r$note)) r$note else "Analyse realisee avec succes.")
    })
    output[[paste0("mv_", key, "_metrics")]] <- renderUI({
      r <- mv_res[[key]]
      if (is.null(r))
        return(p(style = "color:#888; font-style:italic; padding:20px; text-align:center;",
                 icon("hourglass-half"), " En attente du lancement de l'analyse."))
      if (isFALSE(r$ok)) return(mv_status_box("err", r$error))
      r$render
    })
    output[[paste0("mv_", key, "_plot")]] <- renderPlot({
      r <- mv_res[[key]]
      if (is.null(r) || isFALSE(r$ok) || is.null(r$plotfn)) return(mv_empty_plot())
      r$plotfn()
    })
    output[[paste0("mv_", key, "_summary")]] <- renderPrint({
      r <- mv_res[[key]]
      if (is.null(r)) { cat("En attente du lancement de l'analyse.\n"); return(invisible()) }
      if (isFALSE(r$ok)) { cat("Erreur :", r$error, "\n"); return(invisible()) }
      cat(paste(r$summary, collapse = "\n"), "\n")
    })
    output[[paste0("mv_", key, "_dl_xlsx")]] <- downloadHandler(
      filename = function() paste0("HStat_", key, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        r <- mv_res[[key]]
        wb <- openxlsx::createWorkbook()
        exps <- if (!is.null(r) && !is.null(r$exports)) r$exports else
          list(Info = data.frame(Message = "Aucun resultat disponible"))
        for (nm in names(exps)) {
          sh <- substr(gsub("[^A-Za-z0-9_]", "_", nm), 1, 31)
          openxlsx::addWorksheet(wb, sh)
          openxlsx::writeData(wb, sh, exps[[nm]])
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      })
    output[[paste0("mv_", key, "_dl_csv")]] <- downloadHandler(
      filename = function() paste0("HStat_", key, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        r <- mv_res[[key]]
        d <- if (!is.null(r) && !is.null(r$metrics)) r$metrics else
          data.frame(Message = "Aucun resultat disponible")
        utils::write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8")
      })
  }
  for (k in c("kmeans","efa","cfa","pls","regmult","afc","mca",
              "kmodes","lca","logit","famd","mfa","kproto"))
    mv_register(k)

  # ============================ CONTROLES =====================================
  mv_pick_num <- function(id, label, multiple = TRUE, selected = NULL) {
    nc <- mv_num_cols()
    pickerInput(id, label, choices = nc, multiple = multiple,
                selected = if (is.null(selected)) (if (multiple) nc else nc[1]) else selected,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  }
  mv_pick_cat <- function(id, label, multiple = TRUE, selected = NULL) {
    cc <- mv_cat_cols()
    pickerInput(id, label, choices = cc, multiple = multiple,
                selected = if (is.null(selected)) (if (multiple) cc else cc[1]) else selected,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  }
  mv_opt_box <- function(...) div(
    style = "background:#f8f9fa; border-left:4px solid #6c757d; padding:10px; margin:8px 0;", ...)

  output$mv_kmeans_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("chart-line"), " Variables numeriques"),
        mv_pick_num("mv_kmeans_vars", "Variables a partitionner :")),
      fluidRow(
        column(4, numericInput("mv_kmeans_k", tagList(icon("object-group"), " Nombre de clusters k :"),
                               value = 3, min = 2, max = 15)),
        column(4, numericInput("mv_kmeans_nstart", tagList(icon("redo"), " Initialisations :"),
                               value = 25, min = 1, max = 100)),
        column(4, div(style = "margin-top:25px;",
          checkboxInput("mv_kmeans_scale", tagList(icon("balance-scale"), " Standardiser"), TRUE)))))
  })

  output$mv_efa_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("chart-line"), " Variables numeriques"),
        mv_pick_num("mv_efa_vars", "Variables observees :")),
      fluidRow(
        column(4, numericInput("mv_efa_nf", tagList(icon("hashtag"), " Nombre de facteurs :"),
                               value = 2, min = 1, max = 15)),
        column(4, selectInput("mv_efa_rot", tagList(icon("sync-alt"), " Rotation :"),
                              choices = c("Varimax (orthogonale)" = "varimax",
                                          "Oblimin (oblique)" = "oblimin",
                                          "Promax (oblique)" = "promax",
                                          "Aucune" = "none"), selected = "varimax")),
        column(4, selectInput("mv_efa_fm", tagList(icon("cogs"), " Extraction :"),
                              choices = c("Maximum de vraisemblance" = "ml",
                                          "Axes principaux" = "pa",
                                          "Moindres carres" = "minres"), selected = "ml"))))
  })

  output$mv_cfa_controls <- renderUI({
    req(mv_data())
    nc <- mv_num_cols()
    ex <- if (length(nc) >= 4)
      paste0("F1 =~ ", paste(nc[1:2], collapse = " + "), "\n",
             "F2 =~ ", paste(nc[3:min(4,length(nc))], collapse = " + "))
    else "F1 =~ var1 + var2 + var3"
    tagList(
      mv_opt_box(
        h5(icon("project-diagram"), " Modele de mesure (syntaxe lavaan)"),
        p(style = "font-size:11px; color:#555;",
          "Un facteur par ligne. Exemple : ", tags$code("F1 =~ x1 + x2 + x3")),
        textAreaInput("mv_cfa_model", NULL, value = ex, rows = 5, width = "100%"),
        p(style = "font-size:11px; color:#888;", icon("lightbulb"),
          " Variables numeriques disponibles : ", paste(nc, collapse = ", "))),
      selectInput("mv_cfa_est", tagList(icon("cogs"), " Estimateur :"),
                  choices = c("ML (max. vraisemblance)" = "ML", "MLR (robuste)" = "MLR"),
                  selected = "MLR"))
  })

  output$mv_pls_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(
        h5(icon("bullseye"), " Variable reponse Y"),
        selectInput("mv_pls_y", "Reponse a predire :", choices = names(mv_data())),
        p(style = "font-size:11px; color:#555;",
          "Y numerique => PLS | Y categorielle => PLS-DA (detection automatique)")),
      mv_opt_box(h5(icon("chart-line"), " Predicteurs X"),
        mv_pick_num("mv_pls_x", "Predicteurs numeriques :")),
      fluidRow(
        column(6, numericInput("mv_pls_ncomp", tagList(icon("hashtag"), " Composantes :"),
                               value = 3, min = 1, max = 15)),
        column(6, div(style = "margin-top:25px;",
          checkboxInput("mv_pls_cv", tagList(icon("redo"), " Validation croisee"), TRUE)))))
  })

  output$mv_regmult_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("bullseye"), " Variable reponse Y (numerique)"),
        selectInput("mv_regmult_y", "Reponse a expliquer :", choices = mv_num_cols())),
      mv_opt_box(h5(icon("chart-line"), " Predicteurs X"),
        pickerInput("mv_regmult_x", "Predicteurs :", choices = names(mv_data()),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))))
  })

  output$mv_afc_controls <- renderUI({
    req(mv_data())
    cc <- mv_cat_cols()
    tagList(
      mv_opt_box(h5(icon("shapes"), " Deux variables qualitatives"),
        fluidRow(
          column(6, selectInput("mv_afc_row", tagList(icon("grip-lines"), " Variable-ligne :"),
                                choices = cc)),
          column(6, selectInput("mv_afc_col", tagList(icon("grip-lines-vertical"), " Variable-colonne :"),
                                choices = cc, selected = if (length(cc) > 1) cc[2] else cc[1])))))
  })

  output$mv_mca_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("shapes"), " Variables qualitatives"),
        mv_pick_cat("mv_mca_vars", "Variables a analyser :")),
      numericInput("mv_mca_ncp", tagList(icon("hashtag"), " Dimensions a retenir :"),
                   value = 5, min = 2, max = 15))
  })

  output$mv_kmodes_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("shapes"), " Variables qualitatives"),
        mv_pick_cat("mv_kmodes_vars", "Variables a partitionner :")),
      fluidRow(
        column(6, numericInput("mv_kmodes_k", tagList(icon("object-group"), " Nombre de clusters k :"),
                               value = 3, min = 2, max = 15)),
        column(6, numericInput("mv_kmodes_iter", tagList(icon("redo"), " Iterations max :"),
                               value = 20, min = 5, max = 100))))
  })

  output$mv_lca_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("shapes"), " Variables qualitatives (indicateurs)"),
        mv_pick_cat("mv_lca_vars", "Indicateurs categoriels :")),
      fluidRow(
        column(6, numericInput("mv_lca_nclass", tagList(icon("object-group"), " Nombre de classes :"),
                               value = 2, min = 2, max = 10)),
        column(6, numericInput("mv_lca_rep", tagList(icon("redo"), " Repetitions EM :"),
                               value = 5, min = 1, max = 30))))
  })

  output$mv_logit_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("bullseye"), " Variable reponse Y (categorielle)"),
        selectInput("mv_logit_y", "Reponse a predire :", choices = mv_cat_cols()),
        p(style = "font-size:11px; color:#555;",
          "2 modalites => logistique binaire | >2 => logistique multinomiale")),
      mv_opt_box(h5(icon("chart-line"), " Predicteurs X"),
        pickerInput("mv_logit_x", "Predicteurs :", choices = names(mv_data()),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))))
  })

  output$mv_famd_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("layer-group"), " Variables mixtes (quanti + quali)"),
        pickerInput("mv_famd_vars", "Variables a analyser :",
                    choices = names(mv_data()), multiple = TRUE,
                    selected = names(mv_data()),
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))),
      numericInput("mv_famd_ncp", tagList(icon("hashtag"), " Dimensions a retenir :"),
                   value = 5, min = 2, max = 15))
  })

  output$mv_mfa_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("ruler-combined"), " Bloc QUANTITATIF"),
        mv_pick_num("mv_mfa_quanti", "Variables numeriques :")),
      mv_opt_box(h5(icon("shapes"), " Bloc QUALITATIF"),
        mv_pick_cat("mv_mfa_quali", "Variables qualitatives :")),
      numericInput("mv_mfa_ncp", tagList(icon("hashtag"), " Dimensions a retenir :"),
                   value = 5, min = 2, max = 15))
  })

  output$mv_kproto_controls <- renderUI({
    req(mv_data())
    tagList(
      mv_opt_box(h5(icon("layer-group"), " Variables mixtes (quanti + quali)"),
        pickerInput("mv_kproto_vars", "Variables a partitionner :",
                    choices = names(mv_data()), multiple = TRUE,
                    selected = names(mv_data()),
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))),
      fluidRow(
        column(6, numericInput("mv_kproto_k", tagList(icon("object-group"), " Nombre de clusters k :"),
                               value = 3, min = 2, max = 15)),
        column(6, numericInput("mv_kproto_iter", tagList(icon("redo"), " Iterations max :"),
                               value = 20, min = 5, max = 100))))
  })

  # ====================== VERIFICATION DES CONDITIONS =========================
  output$mv_kmeans_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data()); k <- input$mv_kmeans_k %||% 3
    p <- length(input$mv_kmeans_vars %||% character(0))
    n_st <- if (n >= 10*k) "ok" else if (n >= 2*k) "warn" else "err"
    p_st <- if (p >= 3) "ok" else if (p >= 2) "warn" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif critique : n=", n, " < 2k=", 2*k, "."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (recommande 10k=", 10*k, ")."))
    if (p_st == "err") msgs <- c(msgs, "Selectionnez au moins 2 variables numeriques.")
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- k-means",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " var.")),
              .mv_badge("info", paste0("k = ", k))), msgs, lvl)
  })

  # ---- AFE -----------------------------------------------------------------
  output$mv_efa_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_efa_vars %||% character(0))
    n_st <- if (n >= 200) "ok" else if (n >= 100 || n >= 5*p) "warn" else "err"
    p_st <- if (p >= 3) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 100, ideal 200)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modere : n=", n, " (ideal n>=200, >=10 ind./var.)."))
    if (p_st == "err") msgs <- c(msgs, "L'AFE requiert au moins 3 variables observees.")
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- AFE",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " var.")),
              .mv_badge("info", "KMO & Bartlett verifies au lancement")), msgs, lvl)
  })

  # ---- AFC confirmatoire ---------------------------------------------------
  output$mv_cfa_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data())
    n_st <- if (n >= 200) "ok" else if (n >= 100) "warn" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 100, recommande 200)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modere : n=", n, " (recommande n>=200)."))
    if (!mv_has("lavaan")) msgs <- c(msgs, "Package 'lavaan' indisponible -- installation requise.")
    lvl <- if (n_st == "err" || !mv_has("lavaan")) "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- AFC confirmatoire",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(if (mv_has("lavaan")) "ok" else "err", "lavaan")), msgs, lvl)
  })

  # ---- PLS -----------------------------------------------------------------
  output$mv_pls_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_pls_x %||% character(0))
    n_st <- if (n >= 20) "ok" else "warn"
    p_st <- if (p >= 2) "ok" else "err"
    msgs <- list()
    if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (PLS reste valide mais validation croisee conseillee)."))
    if (p_st == "err") msgs <- c(msgs, "Selectionnez au moins 2 predicteurs numeriques.")
    if (!mv_has("pls")) msgs <- c(msgs, "Package 'pls' indisponible -- installation requise.")
    lvl <- if (p_st == "err" || !mv_has("pls")) "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- PLS / PLS-DA",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " predicteurs")),
              .mv_badge(if (mv_has("pls")) "ok" else "err", "pls")), msgs, lvl)
  })

  # ---- Regression lineaire multiple ----------------------------------------
  output$mv_regmult_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_regmult_x %||% character(0))
    ratio <- if (p > 0) n / p else NA
    n_st <- if (!is.na(ratio) && ratio >= 15) "ok" else if (!is.na(ratio) && ratio >= 10) "warn" else "err"
    p_st <- if (p >= 1) "ok" else "err"
    msgs <- list()
    if (p_st == "err") msgs <- c(msgs, "Selectionnez au moins 1 predicteur.")
    else if (n_st == "err") msgs <- c(msgs, paste0("Ratio n/p faible : ", round(ratio,1), " (recommande >=10, ideal >=20)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Ratio n/p modere : ", round(ratio,1), " (ideal >=20)."))
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- Regression lineaire multiple",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " predicteurs")),
              .mv_badge(n_st, paste0("n/p = ", if (is.na(ratio)) "-" else round(ratio,1)))), msgs, lvl)
  })

  # ---- AFC (correspondances) ----------------------------------------------
  output$mv_afc_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data())
    diff_ok <- !is.null(input$mv_afc_row) && !is.null(input$mv_afc_col) &&
      input$mv_afc_row != input$mv_afc_col
    n_st <- if (n >= 50) "ok" else "warn"
    v_st <- if (diff_ok) "ok" else "err"
    msgs <- list()
    if (!diff_ok) msgs <- c(msgs, "Choisissez deux variables qualitatives DIFFERENTES.")
    if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (recommande >=50, effectifs theoriques >=5)."))
    lvl <- if (v_st == "err") "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- AFC",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(v_st, "2 variables distinctes")), msgs, lvl)
  })

  # ---- ACM -----------------------------------------------------------------
  output$mv_mca_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_mca_vars %||% character(0))
    n_st <- if (n >= 100) "ok" else if (n >= 50) "warn" else "err"
    p_st <- if (p >= 3) "ok" else if (p >= 2) "warn" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 50, recommande 100)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modere : n=", n, " (recommande >=100)."))
    if (p_st == "err") msgs <- c(msgs, "L'ACM requiert au moins 2 variables qualitatives.")
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- ACM",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " var. quali"))), msgs, lvl)
  })

  # ---- k-modes -------------------------------------------------------------
  output$mv_kmodes_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data()); k <- input$mv_kmodes_k %||% 3
    p <- length(input$mv_kmodes_vars %||% character(0))
    n_st <- if (n >= 10*k) "ok" else if (n >= 2*k) "warn" else "err"
    p_st <- if (p >= 3) "ok" else if (p >= 2) "warn" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif critique : n=", n, " < 2k=", 2*k, "."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (recommande 10k=", 10*k, ")."))
    if (p_st == "err") msgs <- c(msgs, "Selectionnez au moins 2 variables qualitatives.")
    if (!mv_has("klaR")) msgs <- c(msgs, "Package 'klaR' indisponible -- installation requise.")
    lvl <- if ("err" %in% c(n_st,p_st) || !mv_has("klaR")) "err"
           else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- k-modes",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " var. quali")),
              .mv_badge(if (mv_has("klaR")) "ok" else "err", "klaR")), msgs, lvl)
  })

  # ---- LCA -----------------------------------------------------------------
  output$mv_lca_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_lca_vars %||% character(0))
    n_st <- if (n >= 300) "ok" else if (n >= 100) "warn" else "err"
    p_st <- if (p >= 3) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 100, recommande 300)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modere : n=", n, " (recommande >=300)."))
    if (p_st == "err") msgs <- c(msgs, "La LCA requiert au moins 3 indicateurs categoriels.")
    if (!mv_has("poLCA")) msgs <- c(msgs, "Package 'poLCA' indisponible -- installation requise.")
    lvl <- if (p_st == "err" || n_st == "err" || !mv_has("poLCA")) "err"
           else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- LCA",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " indicateurs")),
              .mv_badge(if (mv_has("poLCA")) "ok" else "err", "poLCA")), msgs, lvl)
  })

  # ---- Regression logistique ----------------------------------------------
  output$mv_logit_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_logit_x %||% character(0))
    epp <- if (p > 0) n / (10 * p) else NA  # >=10 evenements par predicteur
    n_st <- if (!is.na(epp) && epp >= 1) "ok" else if (!is.na(epp) && epp >= 0.5) "warn" else "err"
    p_st <- if (p >= 1) "ok" else "err"
    msgs <- list()
    if (p_st == "err") msgs <- c(msgs, "Selectionnez au moins 1 predicteur.")
    else if (n_st == "err") msgs <- c(msgs, paste0("Effectif faible vs nb predicteurs (regle : >=10 evenements/predicteur)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif limite (regle des 10 evenements/predicteur a surveiller)."))
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- Regression logistique",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " predicteurs"))), msgs, lvl)
  })

  # ---- AFDM ----------------------------------------------------------------
  output$mv_famd_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data())
    sel <- input$mv_famd_vars %||% character(0)
    d <- mv_data()
    nq <- sum(sapply(sel, function(v) v %in% names(d) && is.numeric(d[[v]])))
    nc <- length(sel) - nq
    n_st <- if (n >= 100) "ok" else if (n >= 50) "warn" else "err"
    mix_st <- if (nq >= 1 && nc >= 1) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 50)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modere : n=", n, " (recommande >=100)."))
    if (mix_st == "err") msgs <- c(msgs, "L'AFDM requiert au moins 1 variable quantitative ET 1 qualitative.")
    lvl <- if ("err" %in% c(n_st,mix_st)) "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- AFDM",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(mix_st, paste0(nq, " quanti / ", nc, " quali"))), msgs, lvl)
  })

  # ---- AFM -----------------------------------------------------------------
  output$mv_mfa_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data())
    nq <- length(input$mv_mfa_quanti %||% character(0))
    nc <- length(input$mv_mfa_quali %||% character(0))
    n_st <- if (n >= 100) "ok" else if (n >= 50) "warn" else "err"
    b_st <- if (nq >= 1 && nc >= 1) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 50)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modere : n=", n, " (recommande >=100)."))
    if (b_st == "err") msgs <- c(msgs, "Definissez au moins 1 variable dans chaque bloc (quanti et quali).")
    lvl <- if ("err" %in% c(n_st,b_st)) "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- AFM",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(b_st, paste0("bloc quanti : ", nq)),
              .mv_badge(b_st, paste0("bloc quali : ", nc))), msgs, lvl)
  })

  # ---- k-prototypes --------------------------------------------------------
  output$mv_kproto_conditions <- renderUI({
    req(mv_data())
    n <- nrow(mv_data()); k <- input$mv_kproto_k %||% 3
    sel <- input$mv_kproto_vars %||% character(0)
    d <- mv_data()
    nq <- sum(sapply(sel, function(v) v %in% names(d) && is.numeric(d[[v]])))
    nc <- length(sel) - nq
    n_st <- if (n >= 10*k) "ok" else if (n >= 2*k) "warn" else "err"
    mix_st <- if (nq >= 1 && nc >= 1) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif critique : n=", n, " < 2k=", 2*k, "."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (recommande 10k=", 10*k, ")."))
    if (mix_st == "err") msgs <- c(msgs, "k-prototypes requiert au moins 1 variable quantitative ET 1 qualitative.")
    if (!mv_has("clustMixType")) msgs <- c(msgs, "Package 'clustMixType' indisponible -- installation requise.")
    lvl <- if ("err" %in% c(n_st,mix_st) || !mv_has("clustMixType")) "err"
           else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- k-prototypes",
      tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(mix_st, paste0(nq, " quanti / ", nc, " quali")),
              .mv_badge(if (mv_has("clustMixType")) "ok" else "err", "clustMixType")), msgs, lvl)
  })


  # ====================== MOTEURS D'ANALYSE (QUANTI) ==========================

  # ---- k-means -------------------------------------------------------------
  observeEvent(input$mv_kmeans_run, {
    mv_res[["kmeans"]] <- local({
      tryCatch({
        d <- mv_data(); vars <- input$mv_kmeans_vars
        if (is.null(vars) || length(vars) < 2)
          return(list(ok = FALSE, error = "Selectionnez au moins 2 variables numeriques."))
        k <- input$mv_kmeans_k %||% 3
        X <- d[, vars, drop = FALSE]; X <- X[stats::complete.cases(X), , drop = FALSE]
        if (nrow(X) < 2*k)
          return(list(ok = FALSE, error = "Effectif insuffisant apres suppression des valeurs manquantes."))
        Xs <- if (isTRUE(input$mv_kmeans_scale)) scale(X) else as.matrix(X)
        hstat_set_seed(input$globalSeed)
        Xc <- mv_subsample(as.data.frame(Xs), 8000)
        km <- stats::kmeans(Xc, centers = k, nstart = input$mv_kmeans_nstart %||% 25, iter.max = 50)
        bss <- km$betweenss / km$totss
        sil <- NA
        if (mv_has("cluster") && nrow(Xc) <= 5000)
          sil <- mean(cluster::silhouette(km$cluster, dist(Xc))[, 3])
        ch <- (km$betweenss/(k-1)) / (km$tot.withinss/(nrow(Xc)-k))
        st_bss <- if (bss >= .5) "ok" else if (bss >= .3) "warn" else "err"
        st_sil <- if (is.na(sil)) "info" else if (sil >= .5) "ok" else if (sil >= .25) "warn" else "err"
        st_bal <- if (min(km$size) >= .05*nrow(Xc)) "ok" else "warn"
        metrics <- rbind(
          mv_row("Inertie inter/totale (R2)", paste0(round(100*bss,1)," %"),
                 ">= 50 % nette ; 30-50 % moderee ; < 30 % faible",
                 if (st_bss=="ok") "Partition nette" else if (st_bss=="warn") "Partition moderee" else "Partition peu separee",
                 st_bss),
          mv_row("Silhouette moyenne", if (is.na(sil)) "n/d" else round(sil,3),
                 "> 0,50 forte ; 0,25-0,50 raisonnable ; < 0,25 faible",
                 if (st_sil=="ok") "Structure forte" else if (st_sil=="warn") "Structure raisonnable"
                   else if (st_sil=="err") "Structure faible" else "Non calculee (n>5000)",
                 st_sil),
          mv_row("Calinski-Harabasz", round(ch,1),
                 "Plus eleve = meilleure separation (comparatif)",
                 "A maximiser entre solutions k", "info"),
          mv_row("Equilibre des clusters", paste(km$size, collapse=" / "),
                 "Eviter clusters vides ou tres minoritaires",
                 if (st_bal=="ok") "Repartition acceptable" else "Cluster tres minoritaire",
                 st_bal))
        pc <- stats::prcomp(Xc)
        coord <- as.data.frame(pc$x[, 1:2]); names(coord) <- c("Dim1","Dim2")
        coord$Cluster <- factor(km$cluster)
        cen <- as.data.frame(stats::predict(pc, km$centers)[, 1:2]); names(cen) <- c("Dim1","Dim2")
        cen$Cluster <- factor(seq_len(k))
        var_pc <- round(100*pc$sdev^2/sum(pc$sdev^2), 1)
        render <- tagList(
          mv_card(border_color = "#4a7fa5",
            mv_section_header("Qualite de la partition", "#4a7fa5", "object-group"),
            mv_info_note("Chaque metrique de separation des clusters est confrontee a son seuil de reference."),
            mv_metrics_table(metrics, "#4a7fa5"),
            mv_interp_bar(paste0("Solution a ", k, " clusters sur ", nrow(Xc),
              " individus. Inertie inter-classes = ", round(100*bss,1), " %."),
              mv_col(st_bss))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Centres des clusters (variables d'origine)", "#6c757d", "crosshairs"),
            mv_data_table(data.frame(Cluster = seq_len(k), round(km$centers,3),
                                     check.names = FALSE), "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("k-means : ", k, " clusters sur ", nrow(Xc), " individus."),
          summary = c("=== Classification k-means ===",
            paste0("Variables : ", paste(vars, collapse=", ")),
            paste0("k = ", k, " | nstart = ", input$mv_kmeans_nstart),
            paste0("Inertie inter/totale = ", round(100*bss,2), " %"),
            "", "Centres :", paste(utils::capture.output(round(km$centers,3)), collapse="\n")),
          plotfn = function() {
            ggplot(coord, aes(Dim1, Dim2, color = Cluster)) +
              geom_point(size = 2.4, alpha = .75) +
              geom_point(data = cen, aes(Dim1, Dim2, fill = Cluster),
                         shape = 23, size = 5, color = "black", stroke = 1.1, show.legend = FALSE) +
              labs(title = "Classification k-means -- projection ACP",
                   subtitle = paste0(k, " clusters | inertie inter = ", round(100*bss,1), " %"),
                   x = paste0("Dim 1 (", var_pc[1], " %)"),
                   y = paste0("Dim 2 (", var_pc[2], " %)"),
                   caption = "Les losanges noirs marquent les centres de cluster.") +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics,
            Centres = data.frame(Cluster = seq_len(k), round(km$centers,4), check.names = FALSE),
            Effectifs = data.frame(Cluster = seq_len(k), Effectif = km$size)))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- AFE -----------------------------------------------------------------
  observeEvent(input$mv_efa_run, {
    mv_res[["efa"]] <- local({
      tryCatch({
        d <- mv_data(); vars <- input$mv_efa_vars
        if (is.null(vars) || length(vars) < 3)
          return(list(ok = FALSE, error = "Selectionnez au moins 3 variables."))
        X <- d[, vars, drop = FALSE]; X <- X[stats::complete.cases(X), , drop = FALSE]
        nf <- input$mv_efa_nf %||% 2
        if (nrow(X) < 3*ncol(X))
          return(list(ok = FALSE, error = "Effectif trop faible pour une AFE fiable."))
        R <- stats::cor(X)
        kmo <- psych::KMO(R)$MSA
        bart <- psych::cortest.bartlett(R, n = nrow(X))
        fa <- psych::fa(X, nfactors = nf, rotate = input$mv_efa_rot %||% "varimax",
                        fm = input$mv_efa_fm %||% "ml")
        load <- unclass(fa$loadings)
        var_exp <- sum(fa$Vaccounted["Proportion Var", ])
        comm <- fa$communality
        n_low <- sum(comm < .40)
        cross <- sum(apply(abs(load), 1, function(r) sum(r > .30) > 1))
        st_kmo <- if (kmo >= .80) "ok" else if (kmo >= .60) "warn" else "err"
        st_bar <- if (bart$p.value < .05) "ok" else "err"
        st_var <- if (var_exp >= .60) "ok" else if (var_exp >= .50) "warn" else "err"
        st_com <- if (n_low == 0) "ok" else "warn"
        st_cro <- if (cross == 0) "ok" else "warn"
        metrics <- rbind(
          mv_row("Indice KMO", round(kmo,3),
                 "> 0,80 tres bon ; 0,60-0,80 acceptable ; < 0,60 inadequat",
                 if (st_kmo=="ok") "Donnees tres adaptees" else if (st_kmo=="warn") "Adequation acceptable" else "Donnees peu factorisables",
                 st_kmo),
          mv_row("Test de Bartlett (p)", format.pval(bart$p.value, digits=3),
                 "p < 0,05 : correlations exploitables",
                 if (st_bar=="ok") "Factorisation justifiee" else "Matrice proche de l'identite",
                 st_bar),
          mv_row("Variance cumulee expliquee", paste0(round(100*var_exp,1)," %"),
                 ">= 60 % bon ; 50-60 % acceptable ; < 50 % faible",
                 if (st_var=="ok") "Bonne restitution" else if (st_var=="warn") "Restitution acceptable" else "Restitution insuffisante",
                 st_var),
          mv_row("Communautes < 0,40", n_low,
                 "0 ideal : chaque variable communaute >= 0,40",
                 if (st_com=="ok") "Variables bien restituees" else paste0(n_low, " variable(s) mal restituee(s)"),
                 st_com),
          mv_row("Cross-loadings (|charge|>0,30)", cross,
                 "0 souhaite : structure simple",
                 if (st_cro=="ok") "Structure factorielle simple" else paste0(cross, " variable(s) ambigue(s)"),
                 st_cro))
        ld <- as.data.frame(load); ld$Variable <- rownames(load)
        ldl <- stats::reshape(ld, direction = "long",
                 varying = list(colnames(load)), v.names = "Saturation",
                 timevar = "Facteur", times = colnames(load), idvar = "Variable")
        comm_df <- data.frame(Variable = names(comm), Communaute = round(comm,3))
        render <- tagList(
          mv_card(border_color = "#1565c0",
            mv_section_header("Adequation & qualite factorielle", "#1565c0", "sliders"),
            mv_info_note("Verification de la factorisabilite (KMO, Bartlett) et de la qualite de restitution."),
            mv_metrics_table(metrics, "#1565c0"),
            mv_interp_bar(paste0(nf, " facteurs extraits, rotation ", input$mv_efa_rot,
              ". Variance expliquee = ", round(100*var_exp,1), " %."), mv_col(st_var))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Saturations (loadings)", "#6c757d", "table"),
            mv_data_table(data.frame(Variable = rownames(load), round(load,3),
                                     check.names = FALSE), "#6c757d")),
          mv_card(border_color = "#6c757d",
            mv_section_header("Communautes", "#6c757d", "percentage"),
            mv_data_table(comm_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("AFE : ", nf, " facteurs, rotation ", input$mv_efa_rot, "."),
          summary = c("=== Analyse Factorielle Exploratoire ===",
            paste0("KMO = ", round(kmo,3), " | Bartlett p = ", format.pval(bart$p.value,digits=3)),
            "", "Saturations :", paste(utils::capture.output(round(load,3)), collapse="\n"),
            "", "Communautes :", paste(utils::capture.output(round(comm,3)), collapse="\n")),
          plotfn = function() {
            ggplot(ldl, aes(Facteur, Variable, fill = Saturation)) +
              geom_tile(color = "white", linewidth = .6) +
              geom_text(aes(label = round(Saturation, 2)), size = 3.4,
                        color = ifelse(abs(ldl$Saturation) > .5, "white", "#2c3e50")) +
              scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#1565c0",
                                   midpoint = 0, limits = c(-1, 1)) +
              labs(title = "AFE -- carte des saturations",
                   subtitle = paste0(nf, " facteurs | rotation ", input$mv_efa_rot),
                   x = "Facteur", y = "Variable",
                   caption = "Saturation |x| > 0,40 = variable bien associee au facteur.") +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics,
            Saturations = data.frame(Variable = rownames(load), round(load,4), check.names = FALSE),
            Communautes = comm_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- AFC confirmatoire ---------------------------------------------------
  observeEvent(input$mv_cfa_run, {
    mv_res[["cfa"]] <- local({
      tryCatch({
        if (!mv_has("lavaan"))
          return(list(ok = FALSE, error = "Package 'lavaan' indisponible."))
        d <- mv_data(); mdl <- input$mv_cfa_model
        if (is.null(mdl) || !nzchar(trimws(mdl)))
          return(list(ok = FALSE, error = "Renseignez la syntaxe du modele de mesure."))
        fit <- lavaan::cfa(mdl, data = d, estimator = input$mv_cfa_est %||% "MLR",
                           missing = "fiml")
        fm <- lavaan::fitMeasures(fit)
        gv <- function(a,b) if (a %in% names(fm)) fm[[a]] else if (b %in% names(fm)) fm[[b]] else NA
        cfi <- gv("cfi.robust","cfi"); tli <- gv("tli.robust","tli")
        rmsea <- gv("rmsea.robust","rmsea"); srmr <- gv("srmr","srmr")
        chisq <- gv("chisq.scaled","chisq"); df <- gv("df.scaled","df")
        ratio <- if (!is.na(df) && df > 0) chisq/df else NA
        st_r <- if (!is.na(ratio) && ratio < 2) "ok" else if (!is.na(ratio) && ratio < 3) "warn" else "err"
        st_c <- if (cfi >= .95) "ok" else if (cfi >= .90) "warn" else "err"
        st_t <- if (tli >= .95) "ok" else if (tli >= .90) "warn" else "err"
        st_m <- if (rmsea < .05) "ok" else if (rmsea < .08) "warn" else "err"
        st_s <- if (srmr < .08) "ok" else "warn"
        metrics <- rbind(
          mv_row("Chi2 / ddl", if (is.na(ratio)) "n/d" else round(ratio,2),
                 "< 2 bon ; < 3 acceptable ; >= 3 mediocre",
                 if (st_r=="ok") "Tres bon ajustement" else if (st_r=="warn") "Ajustement acceptable" else "Ajustement mediocre",
                 st_r),
          mv_row("CFI", round(cfi,3),
                 "> 0,95 bon ; 0,90-0,95 acceptable ; < 0,90 insuffisant",
                 if (st_c=="ok") "Bon ajustement" else if (st_c=="warn") "Acceptable" else "Insuffisant", st_c),
          mv_row("TLI", round(tli,3),
                 "> 0,95 bon ; 0,90-0,95 acceptable ; < 0,90 insuffisant",
                 if (st_t=="ok") "Bon ajustement" else if (st_t=="warn") "Acceptable" else "Insuffisant", st_t),
          mv_row("RMSEA", round(rmsea,3),
                 "< 0,05 bon ; 0,05-0,08 acceptable ; > 0,10 mediocre",
                 if (st_m=="ok") "Erreur d'approximation faible" else if (st_m=="warn") "Erreur acceptable" else "Erreur trop elevee",
                 st_m),
          mv_row("SRMR", round(srmr,3), "< 0,08 ajustement acceptable",
                 if (st_s=="ok") "Residus standardises faibles" else "Residus eleves", st_s))
        std <- lavaan::standardizedSolution(fit)
        lt <- std[std$op == "=~", c("lhs","rhs","est.std","pvalue")]
        names(lt) <- c("Facteur","Indicateur","Charge_std","p_value")
        lt$Lien <- paste(lt$Facteur, lt$Indicateur, sep = " <- ")
        render <- tagList(
          mv_card(border_color = "#1565c0",
            mv_section_header("Indices d'ajustement du modele", "#1565c0", "check-double"),
            mv_info_note("Le modele de mesure pre-specifie est confronte aux seuils d'ajustement usuels."),
            mv_metrics_table(metrics, "#1565c0"),
            mv_interp_bar(paste0("Estimateur ", input$mv_cfa_est,
              ". CFI = ", round(cfi,3), " | RMSEA = ", round(rmsea,3), "."),
              mv_col(st_c))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Saturations standardisees", "#6c757d", "table"),
            mv_data_table(data.frame(Facteur = lt$Facteur, Indicateur = lt$Indicateur,
              Charge_std = round(lt$Charge_std,3), p_value = round(lt$p_value,4)), "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = "AFC confirmatoire estimee.",
          summary = c("=== Analyse Factorielle Confirmatoire ===",
            paste(utils::capture.output(lavaan::summary(fit, fit.measures = TRUE,
                  standardized = TRUE)), collapse = "\n")),
          plotfn = function() {
            ggplot(lt, aes(stats::reorder(Lien, Charge_std), Charge_std,
                           fill = Charge_std >= .7)) +
              geom_col(width = .65) +
              geom_hline(yintercept = c(.5,.7), linetype = "dashed",
                         color = c("#f39c12","#27ae60")) +
              geom_text(aes(label = round(Charge_std,2)), hjust = -0.2, size = 3.4) +
              scale_fill_manual(values = c("TRUE"="#27ae60","FALSE"="#e67e22"),
                                labels = c("TRUE"=">= 0,70","FALSE"="< 0,70"),
                                name = "Charge std") +
              coord_flip(ylim = c(0, 1.05)) +
              labs(title = "AFC -- saturations standardisees",
                   x = NULL, y = "Charge standardisee",
                   caption = "Lignes : seuils 0,50 (orange) et 0,70 (vert).") +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics,
            Saturations = data.frame(Facteur = lt$Facteur, Indicateur = lt$Indicateur,
              Charge_std = round(lt$Charge_std,4), p_value = round(lt$p_value,4))))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })


  # ---- PLS / PLS-DA --------------------------------------------------------
  observeEvent(input$mv_pls_run, {
    mv_res[["pls"]] <- local({
      tryCatch({
        if (!mv_has("pls"))
          return(list(ok = FALSE, error = "Package 'pls' indisponible."))
        d <- mv_data(); yv <- input$mv_pls_y; xv <- input$mv_pls_x
        if (is.null(yv) || is.null(xv) || length(xv) < 2)
          return(list(ok = FALSE, error = "Definissez Y et au moins 2 predicteurs X."))
        sub <- d[, c(yv, xv), drop = FALSE]
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        is_da <- !is.numeric(sub[[yv]])
        ncomp <- min(input$mv_pls_ncomp %||% 3, length(xv), nrow(sub)-1)
        X <- as.matrix(sub[, xv, drop = FALSE])
        valid <- if (isTRUE(input$mv_pls_cv)) "CV" else "none"

        if (is_da) {
          yf <- factor(sub[[yv]])
          Ydum <- stats::model.matrix(~ yf - 1)
          dat <- data.frame(Y = I(Ydum), X = I(scale(X)))
          hstat_set_seed(input$globalSeed)
          fit <- pls::plsr(Y ~ X, ncomp = ncomp, data = dat, validation = valid)
          pred <- predict(fit, ncomp = ncomp)
          cls <- factor(levels(yf)[apply(pred[,,1], 1, which.max)], levels = levels(yf))
          acc <- mean(cls == yf)
          st_acc <- if (acc >= .80) "ok" else if (acc >= .60) "warn" else "err"
          metrics <- rbind(
            mv_row("Taux de bon classement", paste0(round(100*acc,1)," %"),
                   ">= 80 % bon ; 60-80 % modere ; < 60 % faible",
                   if (st_acc=="ok") "Bonne discrimination" else if (st_acc=="warn") "Discrimination moderee" else "Discrimination faible",
                   st_acc),
            mv_row("Composantes latentes", ncomp,
                   "Choisir le minimum optimisant la prediction",
                   "Modele PLS-DA", "info"),
            mv_row("Classes de Y", paste(levels(yf), collapse=" / "),
                   "Verifier l'equilibre des effectifs", "Reponse categorielle", "info"))
          sc <- as.data.frame(fit$scores[, 1:min(2,ncomp), drop = FALSE])
          if (ncol(sc) < 2) sc$Comp2 <- 0
          names(sc)[1:2] <- c("Comp1","Comp2"); sc$Classe <- yf
          plotfn <- function() {
            ggplot(sc, aes(Comp1, Comp2, color = Classe)) +
              geom_point(size = 2.6, alpha = .8) +
              stat_ellipse(level = .9, linewidth = .7) +
              labs(title = "PLS-DA -- scores des individus",
                   subtitle = paste0(ncomp, " composantes | ", nlevels(yf), " classes"),
                   x = "Composante 1", y = "Composante 2") +
              mv_gg_theme()
          }
          exports <- list(Metriques = metrics,
            Predictions = data.frame(Observe = yf, Predit = cls))
          note <- paste0("PLS-DA : ", ncomp, " composantes, ", nlevels(yf), " classes.")
          extra_card <- NULL
        } else {
          y <- sub[[yv]]
          dat <- data.frame(Y = y, X = I(scale(X)))
          hstat_set_seed(input$globalSeed)
          fit <- pls::plsr(Y ~ X, ncomp = ncomp, data = dat, validation = valid)
          r2y <- pls::R2(fit)$val[1,1,ncomp+1]
          q2 <- if (valid == "CV") {
            press <- fit$validation$PRESS[1, ncomp]
            1 - press/sum((y-mean(y))^2)
          } else NA
          st_r2 <- if (!is.na(r2y) && r2y >= .5) "ok" else if (!is.na(r2y) && r2y >= .3) "warn" else "err"
          st_q2 <- if (is.na(q2)) "info" else if (q2 >= .5) "ok" else if (q2 >= 0) "warn" else "err"
          gap <- if (!is.na(q2)) abs(r2y-q2) else NA
          st_g <- if (is.na(gap)) "info" else if (gap < .3) "ok" else "warn"
          metrics <- rbind(
            mv_row("R2Y (variance Y expliquee)", round(r2y,3),
                   ">= 0,50 bon ; 0,30-0,50 modere ; < 0,30 faible",
                   if (st_r2=="ok") "Bonne explication" else if (st_r2=="warn") "Explication moderee" else "Explication faible",
                   st_r2),
            mv_row("Q2 (predictivite, CV)", if (is.na(q2)) "n/d" else round(q2,3),
                   "> 0,50 bon ; > 0 acceptable ; < 0 nul",
                   if (st_q2=="ok") "Bon pouvoir predictif" else if (st_q2=="warn") "Predictivite limitee"
                     else if (st_q2=="err") "Aucun pouvoir predictif" else "Validation croisee desactivee",
                   st_q2),
            mv_row("Ecart R2Y - Q2", if (is.na(gap)) "n/d" else round(gap,3),
                   "< 0,30 : pas de sur-ajustement",
                   if (st_g=="ok") "Modele non sur-ajuste" else if (st_g=="warn") "Risque de sur-ajustement" else "Validation croisee desactivee",
                   st_g),
            mv_row("Composantes latentes", ncomp,
                   "Choisir le minimum optimisant Q2", "Modele PLS", "info"))
          pr <- predict(fit, ncomp = ncomp)[,1,1]
          pdf <- data.frame(Observe = y, Predit = pr)
          plotfn <- function() {
            ggplot(pdf, aes(Observe, Predit)) +
              geom_point(size = 2.4, alpha = .75, color = "#1565c0") +
              geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#c0392b") +
              labs(title = "PLS -- valeurs predites vs observees",
                   subtitle = paste0("R2Y = ", round(r2y,3),
                     if (!is.na(q2)) paste0(" | Q2 = ", round(q2,3)) else ""),
                   x = "Y observe", y = "Y predit") +
              mv_gg_theme()
          }
          exports <- list(Metriques = metrics)
          note <- paste0("PLS : ", ncomp, " composantes, reponse continue.")
          extra_card <- NULL
        }
        vip <- tryCatch({
          W <- fit$loading.weights[, 1:ncomp, drop = FALSE]
          SS <- colSums(fit$Yloadings[, 1:ncomp, drop = FALSE]^2) *
                colSums(fit$scores[, 1:ncomp, drop = FALSE]^2)
          sqrt(nrow(W) * rowSums(sweep(W^2, 2, SS, "*")) / sum(SS))
        }, error = function(e) NULL)
        if (!is.null(vip)) {
          n_imp <- sum(vip > 1)
          metrics <- rbind(metrics,
            mv_row("Variables VIP > 1", n_imp,
                   "VIP > 1 : variable importante ; 0,8-1 zone grise",
                   paste0(n_imp, " predicteur(s) influent(s)"), "info"))
          exports$VIP <- data.frame(Variable = names(vip), VIP = round(vip,4))
          extra_card <- mv_card(border_color = "#6c757d",
            mv_section_header("Importance des variables (VIP)", "#6c757d", "ranking-star"),
            mv_data_table(data.frame(Variable = names(vip), VIP = round(vip,3)), "#6c757d"))
        }
        render <- tagList(
          mv_card(border_color = "#1565c0",
            mv_section_header("Qualite du modele PLS", "#1565c0", "diagram-project"),
            mv_info_note("Pouvoir explicatif (R2Y) et predictif (Q2) du modele a composantes latentes."),
            mv_metrics_table(metrics, "#1565c0"),
            mv_interp_bar(note, "#1565c0")),
          extra_card)
        list(ok = TRUE, render = render, metrics = metrics, note = note,
          summary = c("=== Regression PLS ===",
            paste(utils::capture.output(summary(fit)), collapse="\n")),
          plotfn = plotfn, exports = exports)
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- Regression lineaire multiple ----------------------------------------
  observeEvent(input$mv_regmult_run, {
    mv_res[["regmult"]] <- local({
      tryCatch({
        d <- mv_data(); yv <- input$mv_regmult_y; xv <- input$mv_regmult_x
        if (is.null(yv) || is.null(xv) || length(xv) < 1)
          return(list(ok = FALSE, error = "Definissez Y et au moins 1 predicteur X."))
        sub <- d[, c(yv, xv), drop = FALSE]
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        fml <- stats::as.formula(paste0("`", yv, "` ~ ",
          paste0("`", xv, "`", collapse = " + ")))
        fit <- stats::lm(fml, data = sub)
        s <- summary(fit)
        r2a <- s$adj.r.squared
        fs <- s$fstatistic
        fp <- stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
        vif_max <- tryCatch(if (length(xv) >= 2) max(car::vif(fit)) else 1,
                            error = function(e) NA)
        bp <- tryCatch(lmtest::bptest(fit)$p.value, error = function(e) NA)
        sw <- tryCatch(stats::shapiro.test(stats::residuals(fit)[1:min(5000,nrow(sub))])$p.value,
                       error = function(e) NA)
        dw <- tryCatch(lmtest::dwtest(fit)$statistic, error = function(e) NA)
        st_r2 <- if (r2a >= .5) "ok" else if (r2a >= .3) "warn" else "err"
        st_f  <- if (fp < .05) "ok" else "err"
        st_v  <- if (is.na(vif_max)) "info" else if (vif_max < 5) "ok" else if (vif_max < 10) "warn" else "err"
        st_bp <- if (is.na(bp)) "info" else if (bp > .05) "ok" else "warn"
        st_sw <- if (is.na(sw)) "info" else if (sw > .05) "ok" else "warn"
        st_dw <- if (is.na(dw)) "info" else if (dw > 1.5 && dw < 2.5) "ok" else "warn"
        metrics <- rbind(
          mv_row("R2 ajuste", round(r2a,3),
                 "Plus eleve = meilleur (>= 0,50 bon, selon domaine)",
                 if (st_r2=="ok") "Bon pouvoir explicatif" else if (st_r2=="warn") "Pouvoir explicatif modere" else "Pouvoir explicatif faible",
                 st_r2),
          mv_row("Test F global (p)", format.pval(fp, digits=3),
                 "p < 0,05 : modele globalement significatif",
                 if (st_f=="ok") "Modele significatif" else "Modele non significatif", st_f),
          mv_row("VIF maximal", if (is.na(vif_max)) "n/d" else round(vif_max,2),
                 "< 5 OK ; 5-10 a surveiller ; > 10 multicolinearite forte",
                 if (st_v=="ok") "Pas de multicolinearite" else if (st_v=="warn") "Multicolinearite moderee"
                   else if (st_v=="err") "Multicolinearite problematique" else "Non applicable (1 predicteur)",
                 st_v),
          mv_row("Breusch-Pagan (p)", if (is.na(bp)) "n/d" else format.pval(bp,digits=3),
                 "p > 0,05 : homoscedasticite respectee",
                 if (st_bp=="ok") "Variance des residus constante" else "Heteroscedasticite detectee",
                 st_bp),
          mv_row("Shapiro-Wilk residus (p)", if (is.na(sw)) "n/d" else format.pval(sw,digits=3),
                 "p > 0,05 : normalite des residus",
                 if (st_sw=="ok") "Residus normaux" else "Ecart a la normalite", st_sw),
          mv_row("Durbin-Watson", if (is.na(dw)) "n/d" else round(dw,2),
                 "Proche de 2 : independance des residus",
                 if (st_dw=="ok") "Residus independants" else "Autocorrelation possible", st_dw))
        coefs <- as.data.frame(s$coefficients)
        coef_df <- data.frame(Terme = rownames(coefs),
          Estimation = round(coefs[,1],4), Err_std = round(coefs[,2],4),
          t = round(coefs[,3],3), p_value = round(coefs[,4],4))
        res_df <- data.frame(Ajuste = stats::fitted(fit), Residu = stats::residuals(fit))
        render <- tagList(
          mv_card(border_color = "#1565c0",
            mv_section_header("Qualite d'ajustement & validite", "#1565c0", "chart-line"),
            mv_info_note("Pouvoir explicatif du modele et verification des hypotheses sur les residus."),
            mv_metrics_table(metrics, "#1565c0"),
            mv_interp_bar(paste0("R2 ajuste = ", round(r2a,3),
              ". Modele ", if (fp < .05) "globalement significatif." else "non significatif."),
              mv_col(st_r2))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Coefficients estimes", "#6c757d", "table"),
            mv_data_table(coef_df, "#6c757d", digits = 4)))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("Regression lineaire : R2 ajuste = ", round(r2a,3), "."),
          summary = c("=== Regression lineaire multiple ===",
            paste(utils::capture.output(s), collapse = "\n")),
          plotfn = function() {
            ggplot(res_df, aes(Ajuste, Residu)) +
              geom_point(size = 2.2, alpha = .7, color = "#1565c0") +
              geom_hline(yintercept = 0, linetype = "dashed", color = "#c0392b") +
              geom_smooth(method = "loess", se = FALSE, color = "#f39c12", linewidth = .8) +
              labs(title = "Regression -- residus vs valeurs ajustees",
                   subtitle = "Un nuage sans tendance confirme l'homoscedasticite",
                   x = "Valeurs ajustees", y = "Residus") +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics, Coefficients = coef_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })


  # ====================== MOTEURS D'ANALYSE (QUALI) ==========================

  # ---- AFC (correspondances) ----------------------------------------------
  observeEvent(input$mv_afc_run, {
    mv_res[["afc"]] <- local({
      tryCatch({
        d <- mv_data(); rv <- input$mv_afc_row; cv <- input$mv_afc_col
        if (is.null(rv) || is.null(cv) || rv == cv)
          return(list(ok = FALSE, error = "Choisissez deux variables qualitatives differentes."))
        tab <- table(d[[rv]], d[[cv]])
        tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
        if (nrow(tab) < 2 || ncol(tab) < 2)
          return(list(ok = FALSE, error = "Table de contingence insuffisante (>=2x2 requise)."))
        chi <- suppressWarnings(stats::chisq.test(tab))
        ca <- FactoMineR::CA(tab, graph = FALSE)
        eig <- ca$eig
        inertia <- sum(eig[,1])
        cramer <- sqrt(chi$statistic / (sum(tab) * (min(dim(tab)) - 1)))
        exp_low <- mean(chi$expected < 5) * 100
        dim12 <- sum(eig[1:min(2,nrow(eig)),2])
        st_chi <- if (chi$p.value < .05) "ok" else "err"
        st_exp <- if (exp_low <= 20) "ok" else "warn"
        st_dim <- if (dim12 >= 70) "ok" else if (dim12 >= 50) "warn" else "err"
        st_cr  <- if (cramer >= .3) "ok" else if (cramer >= .1) "warn" else "err"
        metrics <- rbind(
          mv_row("Test du Chi2 (p)", format.pval(chi$p.value, digits=3),
                 "p < 0,05 : association significative",
                 if (st_chi=="ok") "Association significative" else "Pas d'association detectee",
                 st_chi),
          mv_row("Inertie totale (Chi2/n)", round(inertia,4),
                 "Mesure l'intensite globale d'association",
                 "Intensite de l'association lignes/colonnes", "info"),
          mv_row("V de Cramer", round(cramer,3),
                 ">= 0,30 forte ; 0,10-0,30 moderee ; < 0,10 faible",
                 if (st_cr=="ok") "Association forte" else if (st_cr=="warn") "Association moderee" else "Association faible",
                 st_cr),
          mv_row("Inertie axes 1-2", paste0(round(dim12,1)," %"),
                 ">= 70 % bonne restitution ; 50-70 % acceptable",
                 if (st_dim=="ok") "Plan factoriel representatif" else if (st_dim=="warn") "Restitution acceptable" else "Restitution limitee",
                 st_dim),
          mv_row("Cases effectif theorique < 5", paste0(round(exp_low,1)," %"),
                 "<= 20 % conseille pour la validite du Chi2",
                 if (st_exp=="ok") "Validite du Chi2 satisfaisante" else "Trop de cases a faible effectif",
                 st_exp))
        rc <- as.data.frame(ca$row$coord[, 1:min(2,ncol(ca$row$coord)), drop = FALSE])
        if (ncol(rc) < 2) rc$D2 <- 0
        names(rc)[1:2] <- c("Dim1","Dim2"); rc$Label <- rownames(ca$row$coord); rc$Type <- "Ligne"
        cc <- as.data.frame(ca$col$coord[, 1:min(2,ncol(ca$col$coord)), drop = FALSE])
        if (ncol(cc) < 2) cc$D2 <- 0
        names(cc)[1:2] <- c("Dim1","Dim2"); cc$Label <- rownames(ca$col$coord); cc$Type <- "Colonne"
        biplot <- rbind(rc[,c("Dim1","Dim2","Label","Type")], cc[,c("Dim1","Dim2","Label","Type")])
        eig_df <- data.frame(Axe = rownames(eig), Valeur_propre = round(eig[,1],4),
                             Variance = round(eig[,2],2), Cumul = round(eig[,3],2))
        render <- tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Association & qualite factorielle", "#6a1b9a", "shapes"),
            mv_info_note("Significativite et intensite de l'association entre les deux variables qualitatives."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(paste0("Table ", nrow(tab), "x", ncol(tab),
              ". V de Cramer = ", round(cramer,3), "."), mv_col(st_cr))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Valeurs propres par axe", "#6c757d", "layer-group"),
            mv_data_table(eig_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("AFC : table ", nrow(tab), "x", ncol(tab), "."),
          summary = c("=== Analyse Factorielle des Correspondances ===",
            paste0("Variables : ", rv, " (lignes) x ", cv, " (colonnes)"),
            "", "Valeurs propres :", paste(utils::capture.output(round(eig,4)), collapse="\n")),
          plotfn = function() {
            ggplot(biplot, aes(Dim1, Dim2, color = Type, label = Label)) +
              geom_hline(yintercept = 0, color = "#bbb", linewidth = .4) +
              geom_vline(xintercept = 0, color = "#bbb", linewidth = .4) +
              geom_point(aes(shape = Type), size = 3) +
              geom_text(vjust = -0.8, size = 3.3, show.legend = FALSE) +
              scale_color_manual(values = c("Ligne"="#6a1b9a","Colonne"="#e67e22")) +
              labs(title = paste0("AFC : ", rv, " x ", cv),
                   subtitle = paste0("Inertie axes 1-2 = ", round(dim12,1), " %"),
                   x = paste0("Dim 1 (", round(eig[1,2],1), " %)"),
                   y = paste0("Dim 2 (", round(eig[min(2,nrow(eig)),2],1), " %)")) +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics, Valeurs_propres = eig_df,
            Table = as.data.frame.matrix(tab)))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- ACM -----------------------------------------------------------------
  observeEvent(input$mv_mca_run, {
    mv_res[["mca"]] <- local({
      tryCatch({
        d <- mv_data(); vars <- input$mv_mca_vars
        if (is.null(vars) || length(vars) < 2)
          return(list(ok = FALSE, error = "Selectionnez au moins 2 variables qualitatives."))
        sub <- d[, vars, drop = FALSE]
        for (v in vars) sub[[v]] <- droplevels(factor(sub[[v]]))
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        ncp <- min(input$mv_mca_ncp %||% 5, sum(sapply(sub, nlevels)) - length(vars))
        mca <- FactoMineR::MCA(sub, ncp = ncp, graph = FALSE)
        eig <- mca$eig
        n_mod <- sum(sapply(sub, nlevels)); Q <- length(vars)
        raw <- eig[,1]; adj <- raw[raw > 1/Q]
        adj_b <- (Q/(Q-1))^2 * (adj - 1/Q)^2
        adj_pct <- 100 * adj_b / sum(adj_b)
        dim12_raw <- sum(eig[1:min(2,nrow(eig)),2])
        dim12_adj <- if (length(adj_pct) >= 2) sum(adj_pct[1:2]) else sum(adj_pct)
        seuil_ctr <- 100 / n_mod
        st_dim <- if (dim12_adj >= 70) "ok" else if (dim12_adj >= 50) "warn" else "err"
        metrics <- rbind(
          mv_row("Inertie brute axes 1-2", paste0(round(dim12_raw,1)," %"),
                 "Sous-estime la structure (a corriger)",
                 "Valeur brute -- privilegier l'inertie ajustee", "info"),
          mv_row("Inertie ajustee axes 1-2 (Benzecri)", paste0(round(dim12_adj,1)," %"),
                 ">= 70 % bonne restitution ; 50-70 % acceptable",
                 if (st_dim=="ok") "Plan factoriel representatif" else if (st_dim=="warn") "Restitution acceptable" else "Restitution limitee",
                 st_dim),
          mv_row("Nombre de modalites", n_mod,
                 "Regrouper les modalites rares (< 5 %)",
                 "Total des modalites actives", "info"),
          mv_row("Seuil de contribution moyen", paste0(round(seuil_ctr,2)," %"),
                 "Une modalite contribue si CTR > 100/nb modalites",
                 "Reference pour reperer les modalites structurantes", "info"))
        vc <- as.data.frame(mca$var$coord[, 1:min(2,ncol(mca$var$coord)), drop = FALSE])
        if (ncol(vc) < 2) vc$D2 <- 0
        names(vc)[1:2] <- c("Dim1","Dim2"); vc$Modalite <- rownames(mca$var$coord)
        eig_df <- data.frame(Axe = rownames(eig), Valeur_propre = round(eig[,1],4),
                             Variance = round(eig[,2],2), Cumul = round(eig[,3],2))
        render <- tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Structure factorielle", "#6a1b9a", "shapes"),
            mv_info_note("L'inertie ajustee de Benzecri corrige la sous-estimation de l'inertie brute en ACM."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(paste0(length(vars), " variables, ", n_mod,
              " modalites. Inertie ajustee axes 1-2 = ", round(dim12_adj,1), " %."),
              mv_col(st_dim))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Valeurs propres par axe", "#6c757d", "layer-group"),
            mv_data_table(eig_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("ACM : ", length(vars), " variables, ", n_mod, " modalites."),
          summary = c("=== Analyse des Correspondances Multiples ===",
            "Valeurs propres :", paste(utils::capture.output(round(eig,4)), collapse="\n")),
          plotfn = function() {
            ggplot(vc, aes(Dim1, Dim2, label = Modalite)) +
              geom_hline(yintercept = 0, color = "#bbb", linewidth = .4) +
              geom_vline(xintercept = 0, color = "#bbb", linewidth = .4) +
              geom_point(size = 3, color = "#6a1b9a") +
              geom_text(vjust = -0.8, size = 3.3, color = "#2c3e50") +
              labs(title = "ACM -- plan des modalites",
                   subtitle = paste0("Inertie ajustee axes 1-2 = ", round(dim12_adj,1), " %"),
                   x = paste0("Dim 1 (", round(eig[1,2],1), " %)"),
                   y = paste0("Dim 2 (", round(eig[min(2,nrow(eig)),2],1), " %)")) +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics, Valeurs_propres = eig_df,
            Modalites = data.frame(Modalite = rownames(mca$var$contrib),
                                   round(mca$var$contrib,3), check.names = FALSE)))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- k-modes -------------------------------------------------------------
  observeEvent(input$mv_kmodes_run, {
    mv_res[["kmodes"]] <- local({
      tryCatch({
        if (!mv_has("klaR"))
          return(list(ok = FALSE, error = "Package 'klaR' indisponible."))
        d <- mv_data(); vars <- input$mv_kmodes_vars
        if (is.null(vars) || length(vars) < 2)
          return(list(ok = FALSE, error = "Selectionnez au moins 2 variables qualitatives."))
        k <- input$mv_kmodes_k %||% 3
        sub <- d[, vars, drop = FALSE]
        for (v in vars) sub[[v]] <- factor(sub[[v]])
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        hstat_set_seed(input$globalSeed)
        sub <- mv_subsample(sub, 8000)
        km <- klaR::kmodes(sub, modes = k, iter.max = input$mv_kmodes_iter %||% 20)
        tot_diff <- sum(km$withindiff)
        sizes <- km$size
        d_tot <- sum(sapply(sub, function(col) { tb <- table(col); sum(tb) - max(tb) }))
        pr2 <- if (d_tot > 0) 1 - tot_diff/d_tot else NA
        st_r2 <- if (!is.na(pr2) && pr2 >= .5) "ok" else if (!is.na(pr2) && pr2 >= .3) "warn" else "err"
        st_bal <- if (min(sizes) >= .05*nrow(sub)) "ok" else "warn"
        metrics <- rbind(
          mv_row("Dissimilarite intra totale", tot_diff,
                 "Plus faible = clusters plus homogenes",
                 "A minimiser entre solutions", "info"),
          mv_row("Pseudo-R2 (separation)", if (is.na(pr2)) "n/d" else round(pr2,3),
                 ">= 0,50 nette ; 0,30-0,50 moderee ; < 0,30 faible",
                 if (st_r2=="ok") "Partition nette" else if (st_r2=="warn") "Partition moderee" else "Partition peu separee",
                 st_r2),
          mv_row("Equilibre des clusters", paste(sizes, collapse=" / "),
                 "Eviter clusters vides ou tres minoritaires",
                 if (st_bal=="ok") "Repartition acceptable" else "Cluster tres minoritaire",
                 st_bal),
          mv_row("Nombre de clusters", k,
                 "Fixe a priori -- comparer plusieurs k",
                 "Parametre du partitionnement", "info"))
        sz_df <- data.frame(Cluster = factor(seq_len(k)), Effectif = sizes)
        render <- tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Qualite de la partition", "#6a1b9a", "object-group"),
            mv_info_note("Separation des clusters categoriels evaluee par le pseudo-R2."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(paste0("Solution a ", k, " clusters sur ", nrow(sub), " individus."),
              mv_col(st_r2))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Modes (centres) des clusters", "#6c757d", "crosshairs"),
            mv_data_table(data.frame(Cluster = seq_len(k), km$modes, check.names = FALSE), "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("k-modes : ", k, " clusters sur ", nrow(sub), " individus."),
          summary = c("=== Classification k-modes ===",
            paste0("Variables : ", paste(vars, collapse=", ")),
            "", "Modes :", paste(utils::capture.output(km$modes), collapse="\n")),
          plotfn = function() {
            ggplot(sz_df, aes(Cluster, Effectif, fill = Cluster)) +
              geom_col(width = .65) +
              geom_text(aes(label = Effectif), vjust = -0.5, size = 3.6, fontface = "bold") +
              labs(title = "k-modes -- effectifs par cluster",
                   subtitle = paste0(k, " clusters"),
                   x = "Cluster", y = "Nombre d'individus") +
              mv_gg_theme() + theme(legend.position = "none")
          },
          exports = list(Metriques = metrics,
            Modes = data.frame(Cluster = seq_len(k), km$modes, check.names = FALSE),
            Effectifs = sz_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })


  # ---- LCA -----------------------------------------------------------------
  observeEvent(input$mv_lca_run, {
    mv_res[["lca"]] <- local({
      tryCatch({
        if (!mv_has("poLCA"))
          return(list(ok = FALSE, error = "Package 'poLCA' indisponible."))
        d <- mv_data(); vars <- input$mv_lca_vars
        if (is.null(vars) || length(vars) < 3)
          return(list(ok = FALSE, error = "Selectionnez au moins 3 indicateurs categoriels."))
        sub <- d[, vars, drop = FALSE]
        for (v in vars) sub[[v]] <- as.integer(factor(sub[[v]]))
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        names(sub) <- vars
        nclass <- input$mv_lca_nclass %||% 2
        fml <- stats::as.formula(paste0("cbind(",
          paste0("`", vars, "`", collapse = ","), ") ~ 1"))
        hstat_set_seed(input$globalSeed)
        fit <- poLCA::poLCA(fml, data = sub, nclass = nclass,
                            nrep = input$mv_lca_rep %||% 5, verbose = FALSE)
        post <- fit$posterior
        ent <- -sum(post * log(post + 1e-12))
        N <- nrow(post); K <- ncol(post)
        ent_rel <- 1 - ent / (N * log(K))
        min_sz <- min(table(fit$predclass)) / N
        st_ent <- if (ent_rel >= .80) "ok" else if (ent_rel >= .60) "warn" else "err"
        st_sz  <- if (min_sz >= .05) "ok" else "warn"
        metrics <- rbind(
          mv_row("BIC", round(fit$bic,1),
                 "Plus bas = meilleur (comparer entre nb de classes)",
                 "Critere de selection du nombre de classes", "info"),
          mv_row("AIC", round(fit$aic,1),
                 "Plus bas = meilleur (penalise moins que le BIC)",
                 "Critere complementaire de selection", "info"),
          mv_row("Entropie relative", round(ent_rel,3),
                 ">= 0,80 bonne separation ; 0,60-0,80 moderee ; < 0,60 faible",
                 if (st_ent=="ok") "Classes bien separees" else if (st_ent=="warn") "Separation moderee" else "Classes mal separees",
                 st_ent),
          mv_row("G2 (rapport de vraisemblance)", round(fit$Gsq,1),
                 "Plus faible = meilleur ajustement du modele",
                 "Qualite d'ajustement", "info"),
          mv_row("Plus petite classe", paste0(round(100*min_sz,1)," %"),
                 ">= 5 % conseille pour une classe interpretable",
                 if (st_sz=="ok") "Classes de taille suffisante" else "Classe tres minoritaire",
                 st_sz))
        cl_df <- as.data.frame(table(Classe = fit$predclass))
        cl_df$Classe <- factor(cl_df$Classe)
        render <- tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Selection & qualite du modele", "#6a1b9a", "shapes"),
            mv_info_note("Criteres d'information (AIC/BIC) et separation des classes latentes (entropie)."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(paste0(nclass, " classes latentes. Entropie = ", round(ent_rel,3), "."),
              mv_col(st_ent))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Effectifs par classe latente", "#6c757d", "users"),
            mv_data_table(cl_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("LCA : ", nclass, " classes latentes."),
          summary = c("=== Analyse en Classes Latentes ===",
            paste(utils::capture.output(print(fit)), collapse = "\n")),
          plotfn = function() {
            ggplot(cl_df, aes(Classe, Freq, fill = Classe)) +
              geom_col(width = .65) +
              geom_text(aes(label = Freq), vjust = -0.5, size = 3.6, fontface = "bold") +
              labs(title = "LCA -- effectifs par classe latente",
                   subtitle = paste0(nclass, " classes | entropie = ", round(ent_rel,3)),
                   x = "Classe latente", y = "Nombre d'individus") +
              mv_gg_theme() + theme(legend.position = "none")
          },
          exports = list(Metriques = metrics, Effectifs = cl_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- Regression logistique ----------------------------------------------
  observeEvent(input$mv_logit_run, {
    mv_res[["logit"]] <- local({
      tryCatch({
        d <- mv_data(); yv <- input$mv_logit_y; xv <- input$mv_logit_x
        if (is.null(yv) || is.null(xv) || length(xv) < 1)
          return(list(ok = FALSE, error = "Definissez Y et au moins 1 predicteur X."))
        sub <- d[, c(yv, xv), drop = FALSE]
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        sub[[yv]] <- droplevels(factor(sub[[yv]]))
        nlev <- nlevels(sub[[yv]])
        if (nlev < 2)
          return(list(ok = FALSE, error = "La reponse doit avoir au moins 2 modalites."))
        fml <- stats::as.formula(paste0("`", yv, "` ~ ",
          paste0("`", xv, "`", collapse = " + ")))

        if (nlev == 2) {
          fit <- stats::glm(fml, data = sub, family = stats::binomial())
          ll0 <- stats::glm(stats::as.formula(paste0("`",yv,"` ~ 1")),
                            data = sub, family = stats::binomial())
          mcf <- as.numeric(1 - stats::logLik(fit)/stats::logLik(ll0))
          pp <- stats::predict(fit, type = "response")
          pc <- factor(ifelse(pp > .5, levels(sub[[yv]])[2], levels(sub[[yv]])[1]),
                       levels = levels(sub[[yv]]))
          acc <- mean(pc == sub[[yv]])
          yb <- as.integer(sub[[yv]]) - 1
          auc <- tryCatch({
            np <- sum(yb); nn <- sum(1-yb); r <- rank(pp)
            (sum(r[yb == 1]) - np*(np+1)/2) / (np*nn)
          }, error = function(e) NA)
          hl_p <- tryCatch({
            g <- 10
            grp <- cut(pp, stats::quantile(pp, probs = seq(0,1,1/g)), include.lowest = TRUE)
            obs <- tapply(yb, grp, sum); exp <- tapply(pp, grp, sum); cnt <- tapply(pp, grp, length)
            chi <- sum((obs-exp)^2 / (exp*(1-exp/cnt)), na.rm = TRUE)
            stats::pchisq(chi, g-2, lower.tail = FALSE)
          }, error = function(e) NA)
          aic <- stats::AIC(fit)
          st_m <- if (mcf >= .2 && mcf <= .4) "ok" else if (mcf > .4 || mcf >= .1) "warn" else "err"
          st_a <- if (is.na(auc)) "info" else if (auc >= .8) "ok" else if (auc >= .7) "warn" else "err"
          st_h <- if (is.na(hl_p)) "info" else if (hl_p > .05) "ok" else "warn"
          metrics <- rbind(
            mv_row("Pseudo-R2 McFadden", round(mcf,3),
                   "0,20-0,40 bon ajustement (echelle propre)",
                   if (st_m=="ok") "Bon ajustement" else if (st_m=="warn") "Ajustement a nuancer" else "Ajustement faible",
                   st_m),
            mv_row("AUC (courbe ROC)", if (is.na(auc)) "n/d" else round(auc,3),
                   ">= 0,80 bon ; 0,70-0,80 acceptable ; < 0,70 faible",
                   if (st_a=="ok") "Bon pouvoir discriminant" else if (st_a=="warn") "Discrimination acceptable" else "Discrimination faible",
                   st_a),
            mv_row("Taux de bon classement", paste0(round(100*acc,1)," %"),
                   "Plus eleve = meilleur (a comparer au hasard)",
                   "Precision globale du modele", "info"),
            mv_row("Hosmer-Lemeshow (p)", if (is.na(hl_p)) "n/d" else format.pval(hl_p,digits=3),
                   "p > 0,05 : ajustement acceptable",
                   if (st_h=="ok") "Calibration acceptable" else "Mauvaise calibration", st_h),
            mv_row("AIC", round(aic,1),
                   "Plus bas = meilleur (comparaison de modeles)",
                   "Critere de comparaison", "info"))
          ors <- exp(stats::coef(fit))
          or_df <- data.frame(Terme = names(ors), Odds_ratio = round(ors,4))
          o <- order(pp); yo <- yb[o]
          roc_df <- data.frame(
            FPR = c(0, cumsum(rev(1-yo))/sum(1-yo)),
            TPR = c(0, cumsum(rev(yo))/sum(yo)))
          plotfn <- function() {
            ggplot(roc_df, aes(FPR, TPR)) +
              geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#999") +
              geom_line(color = "#6a1b9a", linewidth = 1.1) +
              labs(title = "Regression logistique -- courbe ROC",
                   subtitle = paste0("AUC = ", if (is.na(auc)) "n/d" else round(auc,3)),
                   x = "Taux de faux positifs", y = "Taux de vrais positifs") +
              mv_gg_theme()
          }
          exports <- list(Metriques = metrics, Odds_ratios = or_df)
          extra <- mv_card(border_color = "#6c757d",
            mv_section_header("Rapports de cotes (odds ratios)", "#6c757d", "table"),
            mv_data_table(or_df, "#6c757d", digits = 4))
          summ <- utils::capture.output(summary(fit))
          note <- "Regression logistique binaire estimee."
        } else {
          if (!mv_has("nnet"))
            return(list(ok = FALSE, error = "Package 'nnet' requis pour la logistique multinomiale."))
          fit <- nnet::multinom(fml, data = sub, trace = FALSE)
          ll0 <- nnet::multinom(stats::as.formula(paste0("`",yv,"` ~ 1")),
                                data = sub, trace = FALSE)
          mcf <- 1 - as.numeric(stats::logLik(fit))/as.numeric(stats::logLik(ll0))
          pc <- stats::predict(fit)
          acc <- mean(pc == sub[[yv]])
          aic <- stats::AIC(fit)
          st_m <- if (mcf >= .2 && mcf <= .4) "ok" else if (mcf > .4 || mcf >= .1) "warn" else "err"
          st_a <- if (acc >= .7) "ok" else if (acc >= .5) "warn" else "err"
          metrics <- rbind(
            mv_row("Pseudo-R2 McFadden", round(mcf,3),
                   "0,20-0,40 bon ajustement (echelle propre)",
                   if (st_m=="ok") "Bon ajustement" else if (st_m=="warn") "Ajustement a nuancer" else "Ajustement faible",
                   st_m),
            mv_row("Taux de bon classement", paste0(round(100*acc,1)," %"),
                   ">= 70 % bon ; 50-70 % modere",
                   if (st_a=="ok") "Bonne prediction" else if (st_a=="warn") "Prediction moderee" else "Prediction faible",
                   st_a),
            mv_row("AIC", round(aic,1),
                   "Plus bas = meilleur (comparaison de modeles)",
                   "Critere de comparaison", "info"),
            mv_row("Modalites de Y", paste(levels(sub[[yv]]), collapse=" / "),
                   "Verifier l'equilibre des effectifs", "Reponse multinomiale", "info"))
          cm <- as.data.frame(table(Observe = sub[[yv]], Predit = pc))
          plotfn <- function() {
            ggplot(cm, aes(Predit, Observe, fill = Freq)) +
              geom_tile(color = "white", linewidth = .6) +
              geom_text(aes(label = Freq), size = 4,
                        color = ifelse(cm$Freq > max(cm$Freq)/2, "white", "#2c3e50")) +
              scale_fill_gradient(low = "#f3e5f5", high = "#6a1b9a") +
              labs(title = "Logistique multinomiale -- matrice de confusion",
                   subtitle = paste0("Taux de bon classement = ", round(100*acc,1), " %"),
                   x = "Classe predite", y = "Classe observee") +
              mv_gg_theme()
          }
          exports <- list(Metriques = metrics)
          extra <- NULL
          summ <- utils::capture.output(summary(fit))
          note <- "Regression logistique multinomiale estimee."
        }
        render <- tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Qualite d'ajustement & discrimination", "#6a1b9a", "shapes"),
            mv_info_note("Ajustement (pseudo-R2), discrimination (AUC) et calibration du modele logistique."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(note, "#6a1b9a")),
          extra)
        list(ok = TRUE, render = render, metrics = metrics, note = note,
          summary = c("=== Regression logistique ===", summ),
          plotfn = plotfn, exports = exports)
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ====================== MOTEURS D'ANALYSE (MIXTES) ========================

  # ---- AFDM ----------------------------------------------------------------
  observeEvent(input$mv_famd_run, {
    mv_res[["famd"]] <- local({
      tryCatch({
        d <- mv_data(); vars <- input$mv_famd_vars
        if (is.null(vars) || length(vars) < 3)
          return(list(ok = FALSE, error = "Selectionnez au moins 3 variables."))
        sub <- d[, vars, drop = FALSE]
        for (v in vars) if (!is.numeric(sub[[v]])) sub[[v]] <- droplevels(factor(sub[[v]]))
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        nq <- sum(sapply(sub, is.numeric)); nc <- ncol(sub) - nq
        if (nq < 1 || nc < 1)
          return(list(ok = FALSE, error = "L'AFDM exige au moins 1 variable quantitative ET 1 qualitative."))
        ncp <- input$mv_famd_ncp %||% 5
        famd <- FactoMineR::FAMD(sub, ncp = ncp, graph = FALSE)
        eig <- famd$eig
        dim12 <- sum(eig[1:min(2,nrow(eig)),2])
        kaiser <- sum(eig[,1] > 1)
        cum_ncp <- eig[min(ncp,nrow(eig)),3]
        st_dim <- if (dim12 >= 70) "ok" else if (dim12 >= 50) "warn" else "err"
        st_cum <- if (cum_ncp >= 70) "ok" else "warn"
        metrics <- rbind(
          mv_row("Inertie axes 1-2", paste0(round(dim12,1)," %"),
                 ">= 70 % bonne restitution ; 50-70 % acceptable",
                 if (st_dim=="ok") "Plan factoriel representatif" else if (st_dim=="warn") "Restitution acceptable" else "Restitution limitee",
                 st_dim),
          mv_row("Axes a valeur propre > 1", kaiser,
                 "Critere de Kaiser : axes informatifs",
                 paste0(kaiser, " axe(s) informatif(s)"), "info"),
          mv_row("Composition du tableau", paste0(nq, " quanti / ", nc, " quali"),
                 "Les deux types doivent etre presents",
                 "Tableau mixte equilibre par l'AFDM", "info"),
          mv_row("Inertie cumulee (ncp axes)", paste0(round(cum_ncp,1)," %"),
                 ">= 70-80 % conseille",
                 if (st_cum=="ok") "Bonne restitution cumulee" else "Restitution cumulee limitee",
                 st_cum))
        ic <- as.data.frame(famd$ind$coord[, 1:min(2,ncol(famd$ind$coord)), drop = FALSE])
        if (ncol(ic) < 2) ic$D2 <- 0
        names(ic)[1:2] <- c("Dim1","Dim2")
        eig_df <- data.frame(Axe = rownames(eig), Valeur_propre = round(eig[,1],4),
                             Variance = round(eig[,2],2), Cumul = round(eig[,3],2))
        render <- tagList(
          mv_card(border_color = "#00695c",
            mv_section_header("Structure factorielle (donnees mixtes)", "#00695c", "layer-group"),
            mv_info_note("L'AFDM equilibre l'influence des variables quantitatives et qualitatives."),
            mv_metrics_table(metrics, "#00695c"),
            mv_interp_bar(paste0(nq, " quanti + ", nc, " quali. Inertie axes 1-2 = ",
              round(dim12,1), " %."), mv_col(st_dim))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Valeurs propres par axe", "#6c757d", "chart-simple"),
            mv_data_table(eig_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("AFDM : ", nq, " quanti + ", nc, " quali."),
          summary = c("=== Analyse Factorielle de Donnees Mixtes ===",
            "Valeurs propres :", paste(utils::capture.output(round(eig,4)), collapse="\n")),
          plotfn = function() {
            ggplot(ic, aes(Dim1, Dim2)) +
              geom_hline(yintercept = 0, color = "#bbb", linewidth = .4) +
              geom_vline(xintercept = 0, color = "#bbb", linewidth = .4) +
              geom_point(size = 2.4, alpha = .7, color = "#00695c") +
              labs(title = "AFDM -- projection des individus",
                   subtitle = paste0("Inertie axes 1-2 = ", round(dim12,1), " %"),
                   x = paste0("Dim 1 (", round(eig[1,2],1), " %)"),
                   y = paste0("Dim 2 (", round(eig[min(2,nrow(eig)),2],1), " %)")) +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics, Valeurs_propres = eig_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- AFM -----------------------------------------------------------------
  observeEvent(input$mv_mfa_run, {
    mv_res[["mfa"]] <- local({
      tryCatch({
        d <- mv_data(); qv <- input$mv_mfa_quanti; cv <- input$mv_mfa_quali
        if (is.null(qv) || length(qv) < 1 || is.null(cv) || length(cv) < 1)
          return(list(ok = FALSE, error = "Definissez au moins 1 variable dans chaque bloc."))
        sub <- d[, c(qv, cv), drop = FALSE]
        for (v in cv) sub[[v]] <- droplevels(factor(sub[[v]]))
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        ncp <- input$mv_mfa_ncp %||% 5
        mfa <- FactoMineR::MFA(sub, group = c(length(qv), length(cv)),
          type = c("s","n"), name.group = c("Quantitatif","Qualitatif"),
          ncp = ncp, graph = FALSE)
        eig <- mfa$eig
        dim12 <- sum(eig[1:min(2,nrow(eig)),2])
        rv <- tryCatch(mfa$group$RV[1,2], error = function(e) NA)
        st_dim <- if (dim12 >= 70) "ok" else if (dim12 >= 50) "warn" else "err"
        st_rv  <- if (is.na(rv)) "info" else if (rv >= .5) "ok" else if (rv >= .3) "warn" else "err"
        metrics <- rbind(
          mv_row("Inertie axes 1-2", paste0(round(dim12,1)," %"),
                 ">= 70 % bonne restitution ; 50-70 % acceptable",
                 if (st_dim=="ok") "Plan factoriel representatif" else if (st_dim=="warn") "Restitution acceptable" else "Restitution limitee",
                 st_dim),
          mv_row("Coefficient RV entre blocs", if (is.na(rv)) "n/d" else round(rv,3),
                 ">= 0,50 forte concordance ; 0,30-0,50 moderee ; < 0,30 faible",
                 if (st_rv=="ok") "Blocs tres concordants" else if (st_rv=="warn") "Concordance moderee"
                   else if (st_rv=="err") "Blocs peu concordants" else "Non disponible",
                 st_rv),
          mv_row("Blocs definis", paste0(length(qv), " quanti / ", length(cv), " quali"),
                 "Chaque bloc equilibre par sa 1re valeur propre",
                 "Structure en groupes de variables", "info"))
        ic <- as.data.frame(mfa$ind$coord[, 1:min(2,ncol(mfa$ind$coord)), drop = FALSE])
        if (ncol(ic) < 2) ic$D2 <- 0
        names(ic)[1:2] <- c("Dim1","Dim2")
        eig_df <- data.frame(Axe = rownames(eig), Valeur_propre = round(eig[,1],4),
                             Variance = round(eig[,2],2), Cumul = round(eig[,3],2))
        render <- tagList(
          mv_card(border_color = "#00695c",
            mv_section_header("Integration des blocs de variables", "#00695c", "layer-group"),
            mv_info_note("L'AFM compare un bloc quantitatif et un bloc qualitatif sur un meme plan."),
            mv_metrics_table(metrics, "#00695c"),
            mv_interp_bar(paste0("Bloc quanti (", length(qv), ") + bloc quali (", length(cv),
              "). Inertie axes 1-2 = ", round(dim12,1), " %."), mv_col(st_dim))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Valeurs propres par axe", "#6c757d", "chart-simple"),
            mv_data_table(eig_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("AFM : bloc quanti (", length(qv), ") + bloc quali (", length(cv), ")."),
          summary = c("=== Analyse Factorielle Multiple ===",
            "Valeurs propres :", paste(utils::capture.output(round(eig,4)), collapse="\n")),
          plotfn = function() {
            ggplot(ic, aes(Dim1, Dim2)) +
              geom_hline(yintercept = 0, color = "#bbb", linewidth = .4) +
              geom_vline(xintercept = 0, color = "#bbb", linewidth = .4) +
              geom_point(size = 2.4, alpha = .7, color = "#00695c") +
              labs(title = "AFM -- projection des individus",
                   subtitle = paste0("Inertie axes 1-2 = ", round(dim12,1), " %"),
                   x = paste0("Dim 1 (", round(eig[1,2],1), " %)"),
                   y = paste0("Dim 2 (", round(eig[min(2,nrow(eig)),2],1), " %)")) +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics, Valeurs_propres = eig_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- k-prototypes --------------------------------------------------------
  observeEvent(input$mv_kproto_run, {
    mv_res[["kproto"]] <- local({
      tryCatch({
        if (!mv_has("clustMixType"))
          return(list(ok = FALSE, error = "Package 'clustMixType' indisponible."))
        d <- mv_data(); vars <- input$mv_kproto_vars
        if (is.null(vars) || length(vars) < 2)
          return(list(ok = FALSE, error = "Selectionnez au moins 2 variables."))
        k <- input$mv_kproto_k %||% 3
        sub <- d[, vars, drop = FALSE]
        for (v in vars) if (!is.numeric(sub[[v]])) sub[[v]] <- droplevels(factor(sub[[v]]))
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        nq <- sum(sapply(sub, is.numeric)); nc <- ncol(sub) - nq
        if (nq < 1 || nc < 1)
          return(list(ok = FALSE, error = "k-prototypes exige au moins 1 variable quantitative ET 1 qualitative."))
        for (v in names(sub)) if (is.numeric(sub[[v]])) sub[[v]] <- as.numeric(scale(sub[[v]]))
        hstat_set_seed(input$globalSeed)
        sub <- mv_subsample(sub, 8000)
        kp <- clustMixType::kproto(sub, k = k, iter.max = input$mv_kproto_iter %||% 20,
                                   verbose = FALSE)
        sizes <- as.integer(table(kp$cluster))
        tot_cost <- kp$tot.withinss
        sil <- tryCatch({
          v <- clustMixType::validation_kproto(method = "silhouette", object = kp, verbose = FALSE)
          as.numeric(v$index)
        }, error = function(e) NA)
        st_sil <- if (is.na(sil)) "info" else if (sil >= .5) "ok" else if (sil >= .25) "warn" else "err"
        st_bal <- if (min(sizes) >= .05*nrow(sub)) "ok" else "warn"
        metrics <- rbind(
          mv_row("Cout total intra (within SS)", round(tot_cost,1),
                 "Plus faible = clusters plus compacts",
                 "A minimiser entre solutions k", "info"),
          mv_row("Silhouette mixte", if (is.na(sil)) "n/d" else round(sil,3),
                 "> 0,50 forte ; 0,25-0,50 raisonnable ; < 0,25 faible",
                 if (st_sil=="ok") "Structure forte" else if (st_sil=="warn") "Structure raisonnable"
                   else if (st_sil=="err") "Structure faible" else "Non calculee",
                 st_sil),
          mv_row("Equilibre des clusters", paste(sizes, collapse=" / "),
                 "Eviter clusters vides ou tres minoritaires",
                 if (st_bal=="ok") "Repartition acceptable" else "Cluster tres minoritaire",
                 st_bal),
          mv_row("Composition du tableau", paste0(nq, " quanti / ", nc, " quali"),
                 "Les deux types doivent etre presents",
                 "Partitionnement mixte", "info"))
        sz_df <- data.frame(Cluster = factor(seq_len(k)), Effectif = sizes)
        render <- tagList(
          mv_card(border_color = "#00695c",
            mv_section_header("Qualite de la partition mixte", "#00695c", "object-group"),
            mv_info_note("k-prototypes combine distance euclidienne (quanti) et appariement (quali)."),
            mv_metrics_table(metrics, "#00695c"),
            mv_interp_bar(paste0("Solution a ", k, " clusters sur ", nrow(sub), " individus."),
              if (is.na(sil)) "#2980b9" else mv_col(st_sil))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Prototypes des clusters", "#6c757d", "crosshairs"),
            mv_data_table(data.frame(Cluster = seq_len(k), kp$centers, check.names = FALSE),
                          "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("k-prototypes : ", k, " clusters sur ", nrow(sub), " individus."),
          summary = c("=== Classification k-prototypes ===",
            paste0("Variables : ", paste(vars, collapse=", ")),
            "", "Prototypes :", paste(utils::capture.output(kp$centers), collapse="\n")),
          plotfn = function() {
            ggplot(sz_df, aes(Cluster, Effectif, fill = Cluster)) +
              geom_col(width = .65) +
              geom_text(aes(label = Effectif), vjust = -0.5, size = 3.6, fontface = "bold") +
              labs(title = "k-prototypes -- effectifs par cluster",
                   subtitle = paste0(k, " clusters | ", nq, " quanti + ", nc, " quali"),
                   x = "Cluster", y = "Nombre d'individus") +
              mv_gg_theme() + theme(legend.position = "none")
          },
          exports = list(Metriques = metrics, Effectifs = sz_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  invisible(NULL)

  # ---- Seuils d'efficacite (module Shiny) ----
  mod_threshold_server("threshold", values)
}
