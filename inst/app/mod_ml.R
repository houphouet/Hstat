#  Module Shiny : Machine Learning
#  Supervisé (régression & classification, détection automatique de la tâche) :
#  modèle linéaire/logistique, Ridge/Lasso/Elastic-Net (glmnet), arbre de
#  décision (rpart), forêt aléatoire (randomForest), gradient boosting
#  (xgboost), SVM (e1071), k plus proches voisins (kknn), Naïve Bayes (e1071),
#  réseau de neurones à une couche (nnet).
#  Non supervisé : k-means, CAH, PAM, DBSCAN, mélanges gaussiens (mclust).
#  Métriques interprétées, importance des variables, ROC/matrice de confusion,
#  simulateur de prédictions (saisie manuelle ou import), exports complets.

.ml_catalog <- function() c(
  "Modèle linéaire / logistique"        = "lmglm",
  "Ridge / Lasso / Elastic-Net (glmnet)" = "glmnet",
  "Arbre de décision (rpart)"           = "rpart",
  "Forêt aléatoire (randomForest)"      = "rf",
  "Gradient boosting (xgboost)"         = "xgb",
  "SVM (e1071)"                         = "svm",
  "k plus proches voisins (kknn)"       = "knn",
  "Naïve Bayes (e1071)"                 = "nb",
  "Réseau de neurones 1 couche (nnet)"  = "nnet")

mod_ml_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(id = ns("mlTabs"),
      # ================= APPRENTISSAGE SUPERVISÉ =================
      tabPanel(tagList(icon("bullseye"), " Supervisé (prédiction)"),
        div(style = "padding-top:12px;"),
        fluidRow(
          box(title = tagList(icon("sliders"), " Configuration"), status = "primary",
              width = 4, solidHeader = TRUE, collapsible = TRUE,
              selectInput(ns("mlTarget"), "Variable cible (à prédire)", choices = NULL),
              uiOutput(ns("mlTaskInfo")),
              selectizeInput(ns("mlPreds"), "Variables explicatives", choices = NULL,
                             multiple = TRUE),
              fluidRow(
                column(6, sliderInput(ns("mlSplit"), "Part d'entraînement (%)",
                                      min = 50, max = 90, value = 75, step = 5)),
                column(6, numericInput(ns("mlSeed"), "Graine aléatoire",
                                       value = 123, min = 1, step = 1))),
              checkboxGroupInput(ns("mlModels"), "Modèles à comparer",
                choices = .ml_catalog(),
                selected = c("lmglm", "rpart", "rf")),
              tags$details(
                tags$summary(style = "cursor:pointer; font-weight:600;",
                             icon("gears"), " Hyperparamètres"),
                checkboxInput(ns("hpAuto"),
                  "Recherche automatique des hyperparamètres (grille + validation) — recommandé",
                  value = TRUE),
                tags$small(style = "color:#6b7280; display:block; margin-bottom:8px;",
                  "Pour chaque modèle, une grille de réglages est évaluée sur une ",
                  "validation interne au jeu d'entraînement ; le meilleur réglage est ",
                  "retenu puis le modèle final est réentraîné avec. Le jeu de test ",
                  "reste totalement à l'écart de cette recherche. Les valeurs ",
                  "ci-dessous ne servent que si la recherche automatique est décochée."),
                fluidRow(
                  column(6, numericInput(ns("hpAlpha"), "glmnet : alpha (0=Ridge, 1=Lasso)",
                                         value = 0.5, min = 0, max = 1, step = 0.1)),
                  column(6, numericInput(ns("hpCp"), "rpart : complexité (cp)",
                                         value = 0.01, min = 0, max = 0.2, step = 0.005))),
                fluidRow(
                  column(6, numericInput(ns("hpTrees"), "Forêt : nb d'arbres",
                                         value = 300, min = 50, max = 2000, step = 50)),
                  column(6, numericInput(ns("hpRounds"), "xgboost : itérations",
                                         value = 150, min = 20, max = 2000, step = 10))),
                fluidRow(
                  column(6, numericInput(ns("hpDepth"), "xgboost : profondeur",
                                         value = 4, min = 1, max = 12, step = 1)),
                  column(6, numericInput(ns("hpEta"), "xgboost : taux d'apprentissage",
                                         value = 0.1, min = 0.01, max = 1, step = 0.01))),
                fluidRow(
                  column(6, selectInput(ns("hpKernel"), "SVM : noyau",
                           choices = c("radial", "linear", "polynomial", "sigmoid"))),
                  column(6, numericInput(ns("hpCost"), "SVM : coût C",
                                         value = 1, min = 0.01, step = 0.5))),
                fluidRow(
                  column(6, numericInput(ns("hpK"), "kNN : nombre de voisins k",
                                         value = 7, min = 1, max = 100, step = 2)),
                  column(6, numericInput(ns("hpSize"), "nnet : neurones cachés",
                                         value = 8, min = 1, max = 50, step = 1)))),
              actionButton(ns("mlRun"), "Entraîner et comparer",
                           icon = icon("play"), class = "btn-primary"),
              tags$small(style = "color:#6b7280; display:block; margin-top:8px;",
                sprintf("Les lignes incomplètes sont écartées. Au-delà de %s lignes, l'entraînement porte sur un échantillon aléatoire (HSTAT_ML_MAX_N).",
                        format(HSTAT_ML_MAX_N, big.mark = " ")))),
          box(title = tagList(icon("trophy"), " Comparaison des modèles"),
              status = "success", width = 8, solidHeader = TRUE,
              shinycssloaders::withSpinner(DT::dataTableOutput(ns("mlCompare"))),
              hstat_export_table_ui(ns, "mlComp"),
              uiOutput(ns("mlBest")))),
        fluidRow(
          box(title = tagList(icon("chart-area"), " Diagnostic du modèle sélectionné"),
              status = "primary", width = 8, solidHeader = TRUE,
              selectInput(ns("mlShow"), "Modèle affiché", choices = NULL),
              uiOutput(ns("mlDoc")),
              uiOutput(ns("mlHp")),
              shinycssloaders::withSpinner(plotOutput(ns("mlPlot"), height = "420px")),
              tabsetPanel(
                tabPanel("Téléchargement", div(style = "padding-top:10px;",
                         hstat_export_plot_ui(ns, "mlPl"))),
                tabPanel("Apparence", div(style = "padding-top:10px;",
                         hstat_plot_opts_ui(ns, "mlO"))))),
          box(title = tagList(icon("table"), " Métriques & interprétation"),
              status = "info", width = 4, solidHeader = TRUE,
              DT::dataTableOutput(ns("mlMetrics")),
              hstat_export_table_ui(ns, "mlMet"),
              uiOutput(ns("mlInterp")))),
        fluidRow(
          box(title = tagList(icon("ranking-star"), " Importance des variables"),
              status = "warning", width = 6, solidHeader = TRUE,
              plotOutput(ns("mlImp"), height = "340px"),
              hstat_export_plot_ui(ns, "mlIm", width = 9, height = 6),
              uiOutput(ns("mlImpNote"))),
          box(title = tagList(icon("border-all"), " Matrice de confusion / résidus"),
              status = "warning", width = 6, solidHeader = TRUE,
              plotOutput(ns("mlPlot2"), height = "340px"),
              hstat_export_plot_ui(ns, "mlP2", width = 9, height = 6))),
        fluidRow(
          box(title = tagList(icon("magic"), " Simulateur de prédictions"),
              status = "success", width = 12, solidHeader = TRUE,
              fluidRow(
                column(5,
                  h5(strong("1. Saisie manuelle d'un cas")),
                  fluidRow(uiOutput(ns("simForm"))),
                  actionButton(ns("simOne"), "Prédire ce cas",
                               icon = icon("bullseye"), class = "btn-success"),
                  uiOutput(ns("simOneOut"))),
                column(7,
                  h5(strong("2. Import d'un fichier de nouveaux cas (CSV/Excel)")),
                  fileInput(ns("simFile"), NULL, accept = c(".csv", ".xlsx")),
                  tags$small(style = "color:#6b7280;",
                    "Le fichier doit contenir les mêmes colonnes explicatives que ",
                    "le modèle ; la cible est inutile."),
                  DT::dataTableOutput(ns("simBatch")),
                  hstat_export_table_ui(ns, "simB"),
                  uiOutput(ns("simBatchInterp")))))))
      ,
      # ================= NON SUPERVISÉ =================
      tabPanel(tagList(icon("object-group"), " Non supervisé (clustering)"),
        div(style = "padding-top:12px;"),
        fluidRow(
          box(title = tagList(icon("sliders"), " Configuration"), status = "primary",
              width = 4, solidHeader = TRUE,
              selectizeInput(ns("clVars"), "Variables (numériques)",
                             choices = NULL, multiple = TRUE),
              selectInput(ns("clMethod"), "Méthode",
                choices = c("k-means" = "kmeans",
                            "Classification hiérarchique (CAH)" = "hclust",
                            "PAM (k-médoïdes)" = "pam",
                            "DBSCAN (densité)" = "dbscan",
                            "Mélanges gaussiens (mclust)" = "mclust")),
              conditionalPanel(
                condition = sprintf("input['%s'] != 'dbscan'", ns("clMethod")),
                numericInput(ns("clK"), "Nombre de groupes k",
                             value = 3, min = 2, max = 15, step = 1)),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'dbscan'", ns("clMethod")),
                fluidRow(
                  column(6, numericInput(ns("clEps"), "eps (rayon)",
                                         value = 0.5, min = 0.01, step = 0.1)),
                  column(6, numericInput(ns("clMinPts"), "minPts",
                                         value = 5, min = 2, step = 1)))),
              checkboxInput(ns("clScale"), "Standardiser les variables", TRUE),
              uiOutput(ns("clDoc")),
              actionButton(ns("clRun"), "Lancer le clustering",
                           icon = icon("play"), class = "btn-primary")),
          box(title = tagList(icon("chart-area"), " Visualisation des groupes"),
              status = "success", width = 8, solidHeader = TRUE,
              shinycssloaders::withSpinner(plotOutput(ns("clPlot"), height = "420px")),
              hstat_export_plot_ui(ns, "clPl"))),
        fluidRow(
          box(title = tagList(icon("chart-bar"), " Qualité : silhouette & coude"),
              status = "warning", width = 6, solidHeader = TRUE,
              plotOutput(ns("clQual"), height = "330px"),
              hstat_export_plot_ui(ns, "clQu", width = 9, height = 5)),
          box(title = tagList(icon("table"), " Résultats & interprétation"),
              status = "info", width = 6, solidHeader = TRUE,
              DT::dataTableOutput(ns("clTable")),
              hstat_export_table_ui(ns, "clTab"),
              uiOutput(ns("clInterp"))))))
  )
}

mod_ml_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      df <- values$cleanData
      req(df)
      updateSelectInput(session, "mlTarget", choices = names(df))
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      updateSelectizeInput(session, "clVars", choices = num,
                           selected = utils::head(num, min(4, length(num))))
    })
    observeEvent(input$mlTarget, {
      df <- values$cleanData; req(df, nzchar(input$mlTarget %||% ""))
      updateSelectizeInput(session, "mlPreds",
        choices = setdiff(names(df), input$mlTarget),
        selected = setdiff(names(df), input$mlTarget))
    })

    task_of <- function(x)
      if (is.numeric(x) && length(unique(x[!is.na(x)])) > 10) "regression" else "classification"

    output$mlTaskInfo <- renderUI({
      df <- values$cleanData; req(df, nzchar(input$mlTarget %||% ""))
      tk <- task_of(df[[input$mlTarget]])
      div(class = "callout callout-info", icon("circle-info"),
          if (tk == "regression")
            " Cible numérique continue : tâche de RÉGRESSION (prédire une valeur)."
          else " Cible catégorielle (ou peu de valeurs distinctes) : tâche de CLASSIFICATION (prédire une classe).")
    })

    # ---- Préparation commune --------------------------------------------------
    prepare <- function() {
      df <- values$cleanData
      validate(need(!is.null(df), "Chargez d'abord des données."),
               need(nzchar(input$mlTarget %||% ""), "Choisissez la variable cible."),
               need(length(input$mlPreds %||% character(0)) > 0,
                    "Choisissez au moins une variable explicative."))
      target <- input$mlTarget
      preds  <- setdiff(input$mlPreds, target)
      d <- df[, c(target, preds), drop = FALSE]
      d <- d[stats::complete.cases(d), , drop = FALSE]
      validate(need(nrow(d) >= 20, "Au moins 20 lignes complètes sont requises."))
      d <- hstat_cap_df_rows(d, max_n = HSTAT_ML_MAX_N, what = "Machine learning")
      task <- task_of(d[[target]])
      if (task == "classification") {
        d[[target]] <- factor(d[[target]])
        validate(need(nlevels(d[[target]]) >= 2, "La cible doit avoir au moins 2 classes."),
                 need(min(table(d[[target]])) >= 2,
                      "Chaque classe doit compter au moins 2 observations."))
      } else d[[target]] <- as.numeric(d[[target]])
      for (v in preds) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
      # Noms internes sûrs pour les formules (accents/espaces)
      map <- stats::setNames(make.names(names(d), unique = TRUE), names(d))
      di <- d; names(di) <- unname(map)
      ti <- unname(map[target]); pi_ <- unname(map[preds])
      set.seed(as.integer(hstat_finite(input$mlSeed, 123)))
      n <- nrow(di)
      idx <- sample.int(n, floor(n * hstat_finite(input$mlSplit, 75) / 100))
      if (task == "classification") { # garantir toutes les classes dans le train
        for (lv in levels(di[[ti]])) {
          w <- which(di[[ti]] == lv)
          if (!any(w %in% idx)) idx <- c(idx, w[1])
        }
      }
      list(train = di[idx, , drop = FALSE], test = di[-idx, , drop = FALSE],
           task = task, target = target, preds = preds, ti = ti, pi = pi_,
           map = map, ref = d,
           f = stats::as.formula(paste(ti, "~ .")))
    }

    # Matrice de design cohérente train/test/nouveaux cas ------------------------
    mk_mm <- function(p, newdf) {
      tt <- stats::terms(stats::as.formula(paste("~", paste(p$pi, collapse = "+"))))
      xlev <- lapply(p$train[p$pi], function(x) if (is.factor(x)) levels(x) else NULL)
      xlev <- xlev[!vapply(xlev, is.null, logical(1))]
      mm <- stats::model.matrix(tt, data = newdf, xlev = xlev)
      mm[, colnames(mm) != "(Intercept)", drop = FALSE]
    }

    # Renomme un data.frame externe (noms d'origine -> noms internes) et le type
    to_internal <- function(p, nd_original) {
      al <- hstat_align_newdata(nd_original, p$ref, p$preds)
      if (is.null(al$data)) return(al)
      names(al$data) <- unname(p$map[p$preds])
      al
    }

    # ---- Recherche d'hyperparamètres : validation interne au train --------------
    # Le jeu de test n'est JAMAIS utilisé ici : la grille est évaluée sur une
    # coupe de validation prélevée dans le jeu d'entraînement (plafonnée pour
    # les algorithmes coûteux), puis le modèle final est réentraîné sur tout
    # le train avec le meilleur réglage.
    tune_split <- function(tr, cap = 20000) {
      n <- nrow(tr)
      if (n > cap) tr <- tr[sample.int(n, cap), , drop = FALSE]
      n <- nrow(tr)
      ix <- sample.int(n, max(2, floor(n * 0.75)))
      list(fit = tr[ix, , drop = FALSE], val = tr[-ix, , drop = FALSE])
    }
    tune_score <- function(obs, pred, cls) {
      if (cls) mean(as.character(pred) != as.character(obs))
      else sqrt(mean((as.numeric(obs) - as.numeric(pred))^2))
    }

    # ---- Ajustement d'un modèle ------------------------------------------------
    fit_ml <- function(idm, p) {
      tr <- p$train; te <- p$test; ti <- p$ti; cls <- p$task == "classification"
      need_pkg <- function(pkg) if (!requireNamespace(pkg, quietly = TRUE))
        stop(sprintf("Le package '%s' est requis (install.packages(\"%s\")).", pkg, pkg))
      pf <- NULL; imp <- NULL; label <- names(.ml_catalog())[match(idm, .ml_catalog())]
      auto <- isTRUE(input$hpAuto); hp <- if (auto) NULL else "réglages manuels"

      if (idm == "lmglm") {
        if (auto) hp <- "aucun hyperparamètre à régler"
        if (!cls) { m <- stats::lm(p$f, data = tr)
          pf <- function(nd) list(pred = as.numeric(stats::predict(m, nd)), prob = NULL)
          co <- summary(m)$coefficients
          imp <- data.frame(Variable = rownames(co)[-1],
                            Importance = abs(co[-1, "t value"]))
        } else if (nlevels(tr[[ti]]) == 2) {
          m <- stats::glm(p$f, data = tr, family = stats::binomial())
          pf <- function(nd) { pr <- as.numeric(stats::predict(m, nd, type = "response"))
            list(pred = factor(levels(tr[[ti]])[1 + (pr > 0.5)], levels = levels(tr[[ti]])),
                 prob = pr) }
          co <- summary(m)$coefficients
          imp <- data.frame(Variable = rownames(co)[-1],
                            Importance = abs(co[-1, "z value"]))
        } else {
          need_pkg("nnet")
          m <- nnet::multinom(p$f, data = tr, trace = FALSE)
          pf <- function(nd) list(pred = stats::predict(m, nd),
                                  prob = stats::predict(m, nd, type = "probs"))
        }
      } else if (idm == "glmnet") {
        need_pkg("glmnet")
        x <- mk_mm(p, tr); y <- tr[[ti]]
        fam <- if (!cls) "gaussian" else if (nlevels(y) == 2) "binomial" else "multinomial"
        alpha <- hstat_finite(input$hpAlpha, 0.5)
        if (auto) {
          grid <- c(0, 0.25, 0.5, 0.75, 1)
          cvm <- vapply(grid, function(a)
            min(glmnet::cv.glmnet(x, y, alpha = a, family = fam, nfolds = 5)$cvm),
            numeric(1))
          alpha <- grid[which.min(cvm)]
          hp <- sprintf("alpha = %.2f (%s), lambda par validation croisée",
                        alpha, if (alpha == 0) "Ridge" else if (alpha == 1) "Lasso"
                               else "Elastic-Net")
        }
        m <- glmnet::cv.glmnet(x, y, alpha = alpha, family = fam)
        pf <- function(nd) {
          xn <- mk_mm(p, nd)
          if (!cls) list(pred = as.numeric(stats::predict(m, xn, s = "lambda.min")), prob = NULL)
          else {
            pr <- stats::predict(m, xn, s = "lambda.min", type = "response")
            cl <- stats::predict(m, xn, s = "lambda.min", type = "class")
            list(pred = factor(as.character(cl), levels = levels(y)),
                 prob = if (fam == "binomial") as.numeric(pr) else pr[, , 1])
          }
        }
        co <- as.matrix(stats::coef(m, s = "lambda.min"))
        if (is.list(co) || fam == "multinomial") co <- NULL
        if (!is.null(co))
          imp <- data.frame(Variable = rownames(co)[-1], Importance = abs(co[-1, 1]))
      } else if (idm == "rpart") {
        need_pkg("rpart")
        if (auto) {
          m0 <- rpart::rpart(p$f, data = tr, method = if (cls) "class" else "anova",
                             cp = 5e-4)
          ct <- m0$cptable
          cp_best <- as.numeric(ct[which.min(ct[, "xerror"]), "CP"])
          m <- rpart::prune(m0, cp = cp_best)
          hp <- sprintf("cp = %.4g (élagage au minimum d'erreur de validation croisée, %d feuille(s))",
                        cp_best, sum(m$frame$var == "<leaf>"))
        } else
        m <- rpart::rpart(p$f, data = tr, method = if (cls) "class" else "anova",
                          cp = hstat_finite(input$hpCp, 0.01))
        pf <- function(nd) {
          if (!cls) list(pred = as.numeric(stats::predict(m, nd)), prob = NULL)
          else list(pred = stats::predict(m, nd, type = "class"),
                    prob = stats::predict(m, nd, type = "prob"))
        }
        if (length(m$variable.importance))
          imp <- data.frame(Variable = names(m$variable.importance),
                            Importance = as.numeric(m$variable.importance))
      } else if (idm == "rf") {
        need_pkg("randomForest")
        ntree <- max(50, hstat_finite(input$hpTrees, 300))
        mtry_best <- NULL
        if (auto) {
          nv <- ncol(tr) - 1
          cands <- sort(unique(pmax(1, c(floor(sqrt(nv)), floor(nv / 3),
                                         floor(nv / 2)))))
          tsp <- tune_split(tr)
          sc <- vapply(cands, function(mt) {
            mm <- randomForest::randomForest(p$f, data = tsp$fit,
                                             ntree = 150, mtry = mt)
            tune_score(tsp$val[[ti]], stats::predict(mm, tsp$val), cls)
          }, numeric(1))
          mtry_best <- cands[which.min(sc)]
          ntree <- max(ntree, 500)
          hp <- sprintf("mtry = %d (validation), ntree = %d", mtry_best, ntree)
        }
        m <- if (is.null(mtry_best))
          randomForest::randomForest(p$f, data = tr, ntree = ntree)
        else randomForest::randomForest(p$f, data = tr, ntree = ntree,
                                        mtry = mtry_best)
        pf <- function(nd) {
          if (!cls) list(pred = as.numeric(stats::predict(m, nd)), prob = NULL)
          else list(pred = stats::predict(m, nd),
                    prob = stats::predict(m, nd, type = "prob"))
        }
        iv <- randomForest::importance(m)
        imp <- data.frame(Variable = rownames(iv), Importance = as.numeric(iv[, 1]))
      } else if (idm == "xgb") {
        need_pkg("xgboost")
        x <- mk_mm(p, tr)
        obj <- if (!cls) "reg:squarederror"
               else if (nlevels(tr[[ti]]) == 2) "binary:logistic" else "multi:softprob"
        lab <- if (!cls) tr[[ti]] else as.numeric(tr[[ti]]) - 1
        depth <- as.integer(hstat_finite(input$hpDepth, 4))
        eta <- hstat_finite(input$hpEta, 0.1)
        nrounds <- as.integer(hstat_finite(input$hpRounds, 150))
        if (auto) {
          tsp <- tune_split(tr)
          xf <- mk_mm(p, tsp$fit); xv <- mk_mm(p, tsp$val)
          xv <- xv[, colnames(xf), drop = FALSE]
          lf <- if (!cls) tsp$fit[[ti]] else as.numeric(tsp$fit[[ti]]) - 1
          grid <- expand.grid(depth = c(3L, 5L, 7L), eta = c(0.05, 0.1, 0.3))
          sc <- vapply(seq_len(nrow(grid)), function(i) {
            pi_ <- list(objective = obj, max_depth = grid$depth[i], eta = grid$eta[i])
            if (obj == "multi:softprob") pi_$num_class <- nlevels(tr[[ti]])
            mm <- xgboost::xgboost(data = xf, label = lf, params = pi_,
                                   nrounds = 150, verbose = 0)
            pr <- stats::predict(mm, xv)
            pv <- if (!cls) pr
              else if (obj == "binary:logistic")
                levels(tr[[ti]])[1 + (pr > 0.5)]
              else levels(tr[[ti]])[max.col(matrix(pr, ncol = nlevels(tr[[ti]]),
                                                   byrow = TRUE))]
            tune_score(tsp$val[[ti]], pv, cls)
          }, numeric(1))
          b <- which.min(sc)
          depth <- grid$depth[b]; eta <- grid$eta[b]
          nrounds <- max(nrounds, 300L)
          hp <- sprintf("max_depth = %d, eta = %.2f (validation), nrounds = %d",
                        depth, eta, nrounds)
        }
        pars <- list(objective = obj, max_depth = depth, eta = eta)
        if (obj == "multi:softprob") pars$num_class <- nlevels(tr[[ti]])
        m <- xgboost::xgboost(data = x, label = lab, params = pars,
                              nrounds = nrounds, verbose = 0)
        pf <- function(nd) {
          xn <- mk_mm(p, nd)
          miss <- setdiff(colnames(x), colnames(xn))
          if (length(miss)) { add <- matrix(0, nrow(xn), length(miss),
                                            dimnames = list(NULL, miss))
                              xn <- cbind(xn, add) }
          xn <- xn[, colnames(x), drop = FALSE]
          pr <- stats::predict(m, xn)
          if (!cls) list(pred = as.numeric(pr), prob = NULL)
          else if (nlevels(tr[[ti]]) == 2)
            list(pred = factor(levels(tr[[ti]])[1 + (pr > 0.5)], levels = levels(tr[[ti]])),
                 prob = as.numeric(pr))
          else {
            pm <- matrix(pr, ncol = nlevels(tr[[ti]]), byrow = TRUE,
                         dimnames = list(NULL, levels(tr[[ti]])))
            list(pred = factor(colnames(pm)[max.col(pm)], levels = levels(tr[[ti]])),
                 prob = pm)
          }
        }
        iv <- xgboost::xgb.importance(model = m)
        if (nrow(iv)) imp <- data.frame(Variable = iv$Feature, Importance = iv$Gain)
      } else if (idm == "svm") {
        need_pkg("e1071")
        cost <- max(0.01, hstat_finite(input$hpCost, 1))
        kern <- input$hpKernel %||% "radial"
        if (auto) {
          tsp <- tune_split(tr, cap = 4000)
          grid <- c(0.25, 1, 4, 16)
          sc <- vapply(grid, function(cc) {
            mm <- e1071::svm(p$f, data = tsp$fit, kernel = kern, cost = cc)
            tune_score(tsp$val[[ti]], stats::predict(mm, tsp$val), cls)
          }, numeric(1))
          cost <- grid[which.min(sc)]
          hp <- sprintf("noyau %s, C = %.2g (validation)", kern, cost)
        }
        m <- e1071::svm(p$f, data = tr, kernel = kern, cost = cost,
                        probability = cls)
        pf <- function(nd) {
          pr <- stats::predict(m, nd, probability = cls)
          if (!cls) list(pred = as.numeric(pr), prob = NULL)
          else {
            pm <- attr(pr, "probabilities")
            list(pred = pr,
                 prob = if (!is.null(pm)) pm[, levels(tr[[ti]]), drop = FALSE] else NULL)
          }
        }
      } else if (idm == "knn") {
        need_pkg("kknn")
        k <- max(1, as.integer(hstat_finite(input$hpK, 7)))
        if (auto) {
          tsp <- tune_split(tr, cap = 10000)
          grid <- c(3, 5, 7, 11, 15, 21, 31)
          grid <- grid[grid < nrow(tsp$fit)]
          sc <- vapply(grid, function(kk) {
            mm <- kknn::kknn(p$f, train = tsp$fit, test = tsp$val, k = kk)
            tune_score(tsp$val[[ti]], mm$fitted.values, cls)
          }, numeric(1))
          k <- grid[which.min(sc)]
          hp <- sprintf("k = %d voisins (validation)", k)
        }
        pf <- function(nd) {
          m <- kknn::kknn(p$f, train = tr, test = nd, k = k)
          if (!cls) list(pred = as.numeric(m$fitted.values), prob = NULL)
          else list(pred = m$fitted.values, prob = m$prob)
        }
      } else if (idm == "nb") {
        if (!cls) stop("Naïve Bayes ne s'applique qu'à la classification.")
        need_pkg("e1071")
        if (auto) hp <- "aucun hyperparamètre à régler"
        m <- e1071::naiveBayes(p$f, data = tr)
        pf <- function(nd) list(pred = stats::predict(m, nd),
                                prob = stats::predict(m, nd, type = "raw"))
      } else if (idm == "nnet") {
        need_pkg("nnet")
        num <- p$pi[vapply(tr[p$pi], is.numeric, logical(1))]
        ctr <- vapply(tr[num], mean, numeric(1))
        scl <- vapply(tr[num], stats::sd, numeric(1)); scl[!is.finite(scl) | scl == 0] <- 1
        sc <- function(d) { for (v in num) d[[v]] <- (d[[v]] - ctr[v]) / scl[v]; d }
        size <- max(1, as.integer(hstat_finite(input$hpSize, 8)))
        decay <- 5e-4
        if (auto) {
          tsp <- tune_split(tr, cap = 10000)
          grid <- expand.grid(size = c(4L, 8L, 16L), decay = c(1e-4, 1e-3, 1e-2))
          scv <- vapply(seq_len(nrow(grid)), function(i) {
            mm <- nnet::nnet(p$f, data = sc(tsp$fit), size = grid$size[i],
                             decay = grid$decay[i], maxit = 200, trace = FALSE,
                             linout = !cls, MaxNWts = 5000)
            pr <- stats::predict(mm, sc(tsp$val),
                                 type = if (cls) "class" else "raw")
            tune_score(tsp$val[[ti]], if (cls) pr else as.numeric(pr), cls)
          }, numeric(1))
          b <- which.min(scv)
          size <- grid$size[b]; decay <- grid$decay[b]
          hp <- sprintf("%d neurones cachés, decay = %.4g (validation)", size, decay)
        }
        m <- nnet::nnet(p$f, data = sc(tr), size = size,
                        decay = decay, maxit = 400, trace = FALSE,
                        linout = !cls, MaxNWts = 5000)
        pf <- function(nd) {
          nd <- sc(nd)
          if (!cls) list(pred = as.numeric(stats::predict(m, nd)), prob = NULL)
          else {
            pr <- stats::predict(m, nd)
            if (nlevels(tr[[ti]]) == 2)
              list(pred = factor(levels(tr[[ti]])[1 + (as.numeric(pr) > 0.5)],
                                 levels = levels(tr[[ti]])), prob = as.numeric(pr))
            else list(pred = factor(colnames(pr)[max.col(pr)], levels = levels(tr[[ti]])),
                      prob = pr)
          }
        }
      } else stop("Modèle inconnu : ", idm)

      out <- pf(te)
      mets <- if (cls) hstat_metrics_cls(te[[ti]], out$pred, out$prob)
              else hstat_metrics_reg(te[[ti]], out$pred)
      if (!is.null(imp)) imp <- imp[order(-imp$Importance), , drop = FALSE]
      list(ok = TRUE, label = label, predict_fun = pf, metrics = mets,
           pred = out$pred, prob = out$prob, imp = imp, hp = hp)
    }

    fits <- eventReactive(input$mlRun, {
      p <- prepare()
      ids <- input$mlModels %||% character(0)
      validate(need(length(ids) > 0, "Sélectionnez au moins un modèle."))
      res <- list()
      withProgress(message = "Entraînement des modèles", value = 0, {
        for (i in seq_along(ids)) {
          incProgress(1 / length(ids),
                      detail = names(.ml_catalog())[match(ids[i], .ml_catalog())])
          res[[ids[i]]] <- tryCatch(fit_ml(ids[i], p),
            error = function(e) list(ok = FALSE, err = conditionMessage(e),
              label = names(.ml_catalog())[match(ids[i], .ml_catalog())]))
        }
      })
      list(res = res, p = p)
    })

    comp_df <- reactive({
      f <- fits(); req(f)
      cls <- f$p$task == "classification"
      rows <- lapply(f$res, function(r) {
        if (!isTRUE(r$ok))
          return(data.frame(Modele = r$label, C1 = NA, C2 = NA, C3 = NA,
                            HP = "", Statut = paste("Échec :", r$err)))
        v <- function(m) { i <- match(m, r$metrics$Metrique)
                           if (is.na(i)) NA else r$metrics$Valeur[i] }
        if (cls) data.frame(Modele = r$label, C1 = v("Exactitude (accuracy)"),
                            C2 = v("F1-score (macro)"), C3 = v("AUC (ROC)"),
                            HP = r$hp %||% "", Statut = "OK")
        else data.frame(Modele = r$label, C1 = v("RMSE"), C2 = v("MAE"),
                        C3 = v("R2"), HP = r$hp %||% "", Statut = "OK")
      })
      out <- do.call(rbind, rows)
      names(out) <- if (cls) c("Modèle", "Exactitude", "F1 (macro)", "AUC",
                               "Hyperparamètres retenus", "Statut")
                    else c("Modèle", "RMSE", "MAE", "R2",
                           "Hyperparamètres retenus", "Statut")
      key <- if (cls) -out[[2]] else out[[2]]
      out[order(is.na(key), key), , drop = FALSE]
    })

    output$mlCompare <- DT::renderDataTable(
      DT::datatable(comp_df(), rownames = FALSE,
                    options = list(dom = "t", pageLength = 15, scrollX = TRUE)))
    hstat_export_table_handlers(output, "mlComp", function() comp_df(),
                                "ml_comparaison")

    output$mlBest <- renderUI({
      d <- comp_df(); d <- d[d$Statut == "OK", , drop = FALSE]
      if (nrow(d) == 0) return(NULL)
      cls <- fits()$p$task == "classification"
      div(class = "callout callout-info", style = "margin-top:10px;", icon("trophy"),
          strong(sprintf(" Meilleur modèle : %s ", d[["Modèle"]][1])),
          if (cls) sprintf("(exactitude = %.3f). ", d[[2]][1])
          else sprintf("(RMSE = %s). ", format(d[[2]][1], big.mark = " ")),
          "Classement établi sur le jeu de test, jamais vu à l'entraînement.")
    })

    observeEvent(fits(), {
      f <- fits()
      ok <- names(f$res)[vapply(f$res, function(r) isTRUE(r$ok), logical(1))]
      labs <- vapply(f$res[ok], function(r) r$label, character(1))
      updateSelectInput(session, "mlShow", choices = stats::setNames(ok, labs),
                        selected = if (length(ok)) ok[1] else NULL)
    })

    output$mlDoc <- renderUI({
      req(nzchar(input$mlShow %||% ""))
      hstat_model_doc_ui(input$mlShow)
    })
    output$mlHp <- renderUI({
      c0 <- tryCatch(cur(), error = function(e) NULL)
      if (is.null(c0) || is.null(c0$r$hp)) return(NULL)
      div(class = "callout callout-success", style = "margin-top:4px;",
          icon("gears"), strong(" Hyperparamètres retenus : "), c0$r$hp,
          " — les métriques et graphiques ci-dessous sont calculés avec ces réglages.")
    })
    output$clDoc <- renderUI(hstat_model_doc_ui(input$clMethod %||% "kmeans"))

    cur <- reactive({
      f <- fits(); req(f, nzchar(input$mlShow %||% ""))
      r <- f$res[[input$mlShow]]
      validate(need(isTRUE(r$ok), "Ce modèle a échoué sur ces données."))
      list(r = r, p = f$p)
    })

    # ---- Graphique principal : obs vs préd / ROC -------------------------------
    main_gg <- reactive({
      c0 <- cur(); r <- c0$r; p <- c0$p
      col <- hstat_plot_opt(input, "mlO", "Col", "#2c7fb8")
      g <- if (p$task == "regression") {
        d <- data.frame(obs = p$test[[p$ti]], pred = as.numeric(r$pred))
        ggplot2::ggplot(d, ggplot2::aes(obs, pred)) +
          ggplot2::geom_point(color = col, alpha = 0.55, size = 1.6) +
          ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                               color = "#e74c3c") +
          ggplot2::labs(title = sprintf("%s — observé vs prédit (test)", r$label),
                        x = "Valeur observée", y = "Valeur prédite")
      } else if (nlevels(p$test[[p$ti]]) == 2 && !is.null(r$prob) &&
                 requireNamespace("pROC", quietly = TRUE)) {
        pr <- if (is.matrix(r$prob) || is.data.frame(r$prob))
                as.numeric(r$prob[, ncol(r$prob)]) else as.numeric(r$prob)
        ro <- pROC::roc(p$test[[p$ti]], pr, quiet = TRUE,
                        levels = levels(p$test[[p$ti]]), direction = "<")
        d <- data.frame(fpr = 1 - ro$specificities, tpr = ro$sensitivities)
        ggplot2::ggplot(d, ggplot2::aes(fpr, tpr)) +
          ggplot2::geom_line(color = col, linewidth = 1) +
          ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
          ggplot2::labs(title = sprintf("%s — courbe ROC (AUC = %.3f)",
                                        r$label, as.numeric(pROC::auc(ro))),
                        x = "Taux de faux positifs", y = "Taux de vrais positifs")
      } else {
        d <- as.data.frame(table(Observe = p$test[[p$ti]], Predit = r$pred))
        tot <- stats::ave(d$Freq, d$Observe, FUN = sum)
        d$Pct <- ifelse(tot > 0, 100 * d$Freq / tot, 0)
        ggplot2::ggplot(d, ggplot2::aes(Predit, Observe, fill = Freq)) +
          ggplot2::geom_tile() +
          ggplot2::geom_text(ggplot2::aes(label = sprintf("%d\n(%.0f %%)", Freq, Pct)),
                             color = "white", lineheight = 0.9) +
          ggplot2::scale_fill_gradient(low = "#90a4ae", high = col) +
          ggplot2::labs(title = sprintf("%s — matrice de confusion (test)", r$label),
                        caption = "Pourcentages par ligne : part de chaque classe observée. Diagonale = bien classés.")
      }
      hstat_apply_plot_opts(g, input, "mlO")
    })
    output$mlPlot <- renderPlot(main_gg())
    output$mlPlDl <- hstat_export_plot_handler(input, "mlPl",
                       function() main_gg(), "ml_diagnostic")

    # ---- Graphique secondaire : confusion / résidus ----------------------------
    second_gg <- reactive({
      c0 <- cur(); r <- c0$r; p <- c0$p
      col <- hstat_plot_opt(input, "mlO", "Col", "#2c7fb8")
      g <- if (p$task == "regression") {
        d <- data.frame(res = p$test[[p$ti]] - as.numeric(r$pred))
        ggplot2::ggplot(d, ggplot2::aes(res)) +
          ggplot2::geom_histogram(bins = 30, fill = col, color = "white") +
          ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#e74c3c") +
          ggplot2::labs(title = "Distribution des erreurs (test)",
                        x = "Erreur (observé − prédit)", y = "Effectif")
      } else {
        d <- as.data.frame(table(Observe = p$test[[p$ti]], Predit = r$pred))
        tot <- stats::ave(d$Freq, d$Observe, FUN = sum)
        d$Pct <- ifelse(tot > 0, 100 * d$Freq / tot, 0)
        ggplot2::ggplot(d, ggplot2::aes(Predit, Observe, fill = Freq)) +
          ggplot2::geom_tile() +
          ggplot2::geom_text(ggplot2::aes(label = sprintf("%d\n(%.0f %%)", Freq, Pct)),
                             color = "white", lineheight = 0.9) +
          ggplot2::scale_fill_gradient(low = "#90a4ae", high = col) +
          ggplot2::labs(title = "Matrice de confusion (test)",
                        caption = "Pourcentages par ligne : part de chaque classe observée. Diagonale = bien classés.")
      }
      hstat_apply_plot_opts(g, input, "mlO")
    })
    output$mlPlot2 <- renderPlot(second_gg())
    output$mlP2Dl <- hstat_export_plot_handler(input, "mlP2",
                       function() second_gg(), "ml_confusion_ou_erreurs")

    # ---- Importance des variables ----------------------------------------------
    imp_gg <- reactive({
      c0 <- cur()
      validate(need(!is.null(c0$r$imp) && nrow(c0$r$imp) > 0,
        "Importance non disponible pour ce modèle (SVM, kNN, Naïve Bayes n'en fournissent pas nativement)."))
      d <- utils::head(c0$r$imp, 20)
      d$Variable <- factor(d$Variable, levels = rev(d$Variable))
      g <- ggplot2::ggplot(d, ggplot2::aes(Importance, Variable)) +
        ggplot2::geom_col(fill = hstat_plot_opt(input, "mlO", "Col", "#2c7fb8")) +
        ggplot2::labs(title = sprintf("Importance des variables — %s", c0$r$label),
                      y = NULL)
      hstat_apply_plot_opts(g, input, "mlO")
    })
    output$mlImp <- renderPlot(imp_gg())
    output$mlImDl <- hstat_export_plot_handler(input, "mlIm",
                       function() imp_gg(), "importance_variables")
    output$mlImpNote <- renderUI({
      c0 <- cur()
      if (is.null(c0$r$imp) || nrow(c0$r$imp) == 0) return(NULL)
      div(class = "callout callout-info", style = "margin-top:8px;", icon("lightbulb"),
          sprintf(" La variable la plus déterminante est « %s » : c'est elle que le modèle exploite le plus pour prédire « %s ». Les variables en bas de classement peuvent souvent être retirées sans perte de performance.",
                  c0$r$imp$Variable[1], c0$p$target))
    })

    # ---- Métriques + interprétation ---------------------------------------------
    output$mlMetrics <- DT::renderDataTable(
      DT::datatable(cur()$r$metrics, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE)))
    hstat_export_table_handlers(output, "mlMet",
      function() cur()$r$metrics, "ml_metriques")

    output$mlInterp <- renderUI({
      c0 <- cur()
      div(class = "callout callout-info", style = "margin-top:10px;",
          icon("lightbulb"), strong(" Interprétation : "),
          hstat_model_interpretation(c0$p$task, c0$r$metrics, c0$r$label,
                                     nrow(c0$p$train), nrow(c0$p$test),
            notes = if (!is.null(c0$r$hp) && !identical(c0$r$hp, "réglages manuels"))
              sprintf("Hyperparamètres retenus par la recherche automatique : %s.", c0$r$hp)
            else NULL))
    })

    # ---- Simulateur ---------------------------------------------------------------
    output$simForm <- renderUI({
      c0 <- tryCatch(cur(), error = function(e) NULL)
      if (is.null(c0)) return(tags$em("Entraînez d'abord un modèle."))
      hstat_sim_inputs_ui(ns, c0$p$ref, c0$p$preds, "simv")
    })

    observeEvent(input$simOne, {
      output$simOneOut <- renderUI({
        c0 <- cur()
        nd0 <- hstat_sim_collect(input, c0$p$ref, c0$p$preds, "simv")
        al <- to_internal(c0$p, nd0)
        validate(need(!is.null(al$data), al$warn %||% "Saisie invalide."))
        out <- tryCatch(c0$r$predict_fun(al$data), error = function(e) e)
        validate(need(!inherits(out, "error"),
                      if (inherits(out, "error")) conditionMessage(out) else ""))
        val <- if (c0$p$task == "regression")
          format(round(as.numeric(out$pred)[1], 4), big.mark = " ")
          else as.character(out$pred[1])
        conf <- if (c0$p$task == "classification" && !is.null(out$prob)) {
          pm <- if (is.matrix(out$prob) || is.data.frame(out$prob)) max(out$prob[1, ])
                else max(out$prob[1], 1 - out$prob[1])
          sprintf(" (confiance : %.1f %%)", 100 * pm)
        } else ""
        div(class = "callout callout-info", style = "margin-top:10px;",
            icon("bullseye"),
            strong(sprintf(" Prédiction de « %s » : %s%s. ", c0$p$target, val, conf)),
            if (c0$p$task == "regression")
              "Valeur estimée par le modèle pour le cas saisi ; sa fiabilité correspond aux métriques du jeu de test (voir RMSE/MAE)."
            else "Classe la plus probable selon le modèle pour le cas saisi.")
      })
    })

    sim_batch <- reactive({
      req(input$simFile$datapath)
      c0 <- cur()
      nd0 <- tryCatch({
        if (grepl("\\.xlsx$", input$simFile$name, ignore.case = TRUE))
          as.data.frame(readxl::read_excel(input$simFile$datapath))
        else utils::read.csv(input$simFile$datapath, check.names = FALSE)
      }, error = function(e) NULL)
      validate(need(!is.null(nd0), "Fichier importé illisible."))
      al <- to_internal(c0$p, nd0)
      validate(need(!is.null(al$data), al$warn %||% "Colonnes incompatibles."))
      if (!is.null(al$warn))
        showNotification(al$warn, type = "warning", duration = 8)
      keep <- stats::complete.cases(al$data)
      validate(need(any(keep), "Aucune ligne complète exploitable dans le fichier."))
      out <- c0$r$predict_fun(al$data[keep, , drop = FALSE])
      res <- nd0[keep, , drop = FALSE]
      res[[paste0("Prediction_", c0$p$target)]] <-
        if (c0$p$task == "regression") round(as.numeric(out$pred), 4)
        else as.character(out$pred)
      if (c0$p$task == "classification" && !is.null(out$prob)) {
        pm <- if (is.matrix(out$prob) || is.data.frame(out$prob))
                apply(as.matrix(out$prob), 1, max)
              else pmax(as.numeric(out$prob), 1 - as.numeric(out$prob))
        res$Confiance <- round(pm, 4)
      }
      res
    })
    output$simBatch <- DT::renderDataTable(
      DT::datatable(sim_batch(), rownames = FALSE,
                    options = list(pageLength = 8, scrollX = TRUE)))
    hstat_export_table_handlers(output, "simB", function() sim_batch(),
                                "predictions_nouveaux_cas")
    output$simBatchInterp <- renderUI({
      d <- tryCatch(sim_batch(), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      c0 <- cur()
      pc <- d[[paste0("Prediction_", c0$p$target)]]
      div(class = "callout callout-info", style = "margin-top:8px;", icon("lightbulb"),
          if (c0$p$task == "regression")
            sprintf(" %d cas prédits. Valeurs prédites : moyenne = %s, min = %s, max = %s.",
                    nrow(d), format(round(mean(as.numeric(pc)), 3), big.mark = " "),
                    format(round(min(as.numeric(pc)), 3), big.mark = " "),
                    format(round(max(as.numeric(pc)), 3), big.mark = " "))
          else sprintf(" %d cas prédits. Classe la plus fréquente : « %s » (%d cas).",
                       nrow(d), names(sort(table(pc), decreasing = TRUE))[1],
                       max(table(pc))))
    })

    # ================= NON SUPERVISÉ ============================================
    clres <- eventReactive(input$clRun, {
      df <- values$cleanData
      validate(need(!is.null(df), "Chargez d'abord des données."),
               need(length(input$clVars %||% character(0)) >= 2,
                    "Choisissez au moins 2 variables numériques."))
      d0 <- df[, input$clVars, drop = FALSE]
      d0 <- d0[stats::complete.cases(d0), , drop = FALSE]
      validate(need(nrow(d0) >= 10, "Au moins 10 lignes complètes sont requises."))
      d0 <- hstat_cap_df_rows(d0, max_n = HSTAT_DIST_MAX_N, what = "Clustering")
      x <- as.matrix(d0)
      if (isTRUE(input$clScale)) x <- scale(x)
      k <- max(2, min(15, as.integer(hstat_finite(input$clK, 3))))
      meth <- input$clMethod %||% "kmeans"
      set.seed(123)
      cl <- switch(meth,
        kmeans = stats::kmeans(x, centers = k, nstart = 20)$cluster,
        hclust = stats::cutree(stats::hclust(stats::dist(x), method = "ward.D2"), k = k),
        pam    = { if (!requireNamespace("cluster", quietly = TRUE))
                     stop("Package 'cluster' requis.")
                   cluster::pam(x, k = k)$clustering },
        dbscan = { if (!requireNamespace("dbscan", quietly = TRUE))
                     stop("Package 'dbscan' requis (install.packages(\"dbscan\")).")
                   dbscan::dbscan(x, eps = hstat_finite(input$clEps, 0.5),
                                  minPts = as.integer(hstat_finite(input$clMinPts, 5)))$cluster },
        mclust = { if (!requireNamespace("mclust", quietly = TRUE))
                     stop("Package 'mclust' requis (install.packages(\"mclust\")).")
                   # Mclust() resout mclustBIC dans l'environnement appelant :
                   # on le fournit localement pour rester robuste meme si le
                   # package n'est pas attache par library().
                   mclustBIC <- mclust::mclustBIC
                   mclust::Mclust(x, G = k, verbose = FALSE)$classification },
        stop("Méthode inconnue."))
      sil <- NA_real_
      if (length(unique(cl[cl > 0])) >= 2 &&
          requireNamespace("cluster", quietly = TRUE)) {
        keep <- cl > 0
        s <- cluster::silhouette(cl[keep], stats::dist(x[keep, , drop = FALSE]))
        sil <- mean(s[, 3])
      }
      list(x = x, cl = cl, d0 = d0, sil = sil, meth = meth, k = k)
    })

    cl_gg <- reactive({
      r <- clres(); req(r)
      pc <- stats::prcomp(r$x)
      d <- data.frame(PC1 = pc$x[, 1], PC2 = pc$x[, 2],
                      Groupe = factor(ifelse(r$cl == 0, "Bruit", r$cl)))
      pv <- 100 * summary(pc)$importance[2, 1:2]
      ggplot2::ggplot(d, ggplot2::aes(PC1, PC2, color = Groupe)) +
        ggplot2::geom_point(alpha = 0.6, size = 1.7) +
        ggplot2::stat_ellipse(data = subset(d, Groupe != "Bruit"), level = 0.9) +
        ggplot2::labs(title = sprintf("Groupes projetés sur le plan principal (%s)", r$meth),
                      x = sprintf("Dim 1 (%.1f %%)", pv[1]),
                      y = sprintf("Dim 2 (%.1f %%)", pv[2])) +
        ggplot2::theme_minimal(base_size = 13)
    })
    output$clPlot <- renderPlot(cl_gg())
    output$clPlDl <- hstat_export_plot_handler(input, "clPl",
                       function() cl_gg(), "clusters")

    cl_qual_gg <- reactive({
      r <- clres(); req(r)
      ks <- 2:min(10, nrow(r$x) - 1)
      wss <- vapply(ks, function(k)
        stats::kmeans(r$x, centers = k, nstart = 10)$tot.withinss, numeric(1))
      d <- data.frame(k = ks, wss = wss)
      ggplot2::ggplot(d, ggplot2::aes(k, wss)) +
        ggplot2::geom_line(color = "#2c7fb8") + ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(xintercept = r$k, linetype = "dashed", color = "#e74c3c") +
        ggplot2::labs(title = "Méthode du coude (inertie intra-groupe, k-means)",
                      x = "Nombre de groupes k", y = "Inertie intra-groupe") +
        ggplot2::theme_minimal(base_size = 13)
    })
    output$clQual <- renderPlot(cl_qual_gg())
    output$clQuDl <- hstat_export_plot_handler(input, "clQu",
                       function() cl_qual_gg(), "coude_clusters")

    cl_table <- reactive({
      r <- clres(); req(r)
      d <- r$d0; d$Groupe <- r$cl
      ag <- stats::aggregate(d[, setdiff(names(d), "Groupe"), drop = FALSE],
                             by = list(Groupe = d$Groupe), FUN = mean)
      ag$Effectif <- as.numeric(table(d$Groupe))
      cbind(ag[, c("Groupe", "Effectif")],
            round(ag[, setdiff(names(ag), c("Groupe", "Effectif")), drop = FALSE], 3))
    })
    output$clTable <- DT::renderDataTable(
      DT::datatable(cl_table(), rownames = FALSE,
                    options = list(dom = "t", pageLength = 15, scrollX = TRUE)))
    hstat_export_table_handlers(output, "clTab", function() {
      r <- clres(); d <- r$d0; d$Groupe <- r$cl; d
    }, "affectations_clusters")

    output$clInterp <- renderUI({
      r <- clres(); req(r)
      div(class = "callout callout-info", style = "margin-top:10px;",
          icon("lightbulb"), strong(" Interprétation : "),
          sprintf("La méthode %s a identifié %d groupe(s)%s. ", r$meth,
                  length(unique(r$cl[r$cl > 0])),
                  if (any(r$cl == 0)) sprintf(" (+ %d points de bruit)", sum(r$cl == 0)) else ""),
          if (is.finite(r$sil)) sprintf(
            "Silhouette moyenne = %.3f : %s Le tableau des moyennes par groupe (export CSV/Excel) sert de carte d'identité de chaque groupe : comparez les colonnes pour nommer les profils.",
            r$sil,
            if (r$sil >= 0.5) "structure de groupes nette et fiable."
            else if (r$sil >= 0.25) "structure réelle mais frontières floues."
            else "structure faible : les groupes se chevauchent, essayer un autre k ou une autre méthode.")
          else "Silhouette non calculable sur ce résultat.")
    })
  })
}
