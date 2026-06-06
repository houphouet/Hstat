################################################################################
#  Helpers UI -- Analyses multivariees etendues
################################################################################

# Bandeau "portee des donnees" : affiche sur chaque onglet d'analyse en mode
# hors-memoire pour rappeler que l'analyse porte sur un echantillon, avec acces
# rapide au reglage de l'echantillon. 'exact' = TRUE si l'onglet propose en plus
# un calcul exact sur le jeu complet.
.hstat_scope_banner <- function(exact = FALSE) {
  conditionalPanel(
    condition = "output.hstatBigData == true",
    div(class = "callout callout-warning", style = "margin-bottom:16px;",
      tags$p(style = "margin:0; font-size:13px;",
        icon("database"),
        if (exact)
          HTML(" <b>Mode hors-mémoire.</b> Cette analyse s'exécute sur l'échantillon de travail ; l'option <b>« calculer sur le jeu complet »</b> ci-dessous fournit un résultat exact lorsque c'est applicable.")
        else
          HTML(" <b>Mode hors-mémoire.</b> Cette analyse ajuste un modèle et s'exécute donc sur l'<b>échantillon de travail</b>. Pour gagner en fidélité, agrandissez l'échantillon dans l'onglet « Chargement » &rarr; « Échantillon de travail »."))
    )
  )
}

.mv_theme <- function(cat) {
  switch(cat,
    quanti = list(main = "#3c8dbc", bg = "#e8f2fc", border = "#b8daf0",
                  status = "primary", grad = "linear-gradient(135deg,#3c8dbc,#367fa9)"),
    quali  = list(main = "#605ca8", bg = "#efeef7", border = "#c7c4e0",
                  status = "primary", grad = "linear-gradient(135deg,#605ca8,#555299)"),
    mixte  = list(main = "#00a65a", bg = "#e6f5ee", border = "#aedec9",
                  status = "success", grad = "linear-gradient(135deg,#00a65a,#008d4c)"))
}

.mv_info_panel <- function(key, cat, principes, objectifs, taille, variables) {
  th <- .mv_theme(cat)
  bid <- paste0("mv-", key, "-info")
  div(style = "margin-bottom:10px;",
    div(
      style = paste0("cursor:pointer; background:", th$grad,
                     "; color:white; padding:8px 12px; border-radius:6px;",
                     " font-weight:bold; font-size:13px; user-select:none;"),
      onclick = sprintf("var c=document.getElementById('%s'); c.style.display=(c.style.display==='none'?'block':'none');", bid),
      icon("info-circle"), " Principes, objectifs & conditions ", icon("chevron-down")
    ),
    div(id = bid,
        style = paste0("display:none; background:", th$bg, "; border:1px solid ",
                       th$border, "; border-radius:0 0 6px 6px; padding:12px; font-size:12px;"),
      fluidRow(
        column(6,
          tags$b(style = paste0("color:", th$main, ";"), icon("drafting-compass"), " Principes :"),
          tags$p(style = "margin:2px 0 6px 0; color:#333;", principes),
          tags$b(style = paste0("color:", th$main, ";"), icon("bullseye"), " Objectifs :"),
          tags$p(style = "margin:2px 0 0 0; color:#333;", objectifs)
        ),
        column(6,
          tags$b(style = paste0("color:", th$main, ";"), icon("ruler"), " Taille necessaire :"),
          tags$ul(style = "margin:2px 0 6px 12px; padding:0; color:#333;",
                  lapply(taille, function(x) tags$li(HTML(x)))),
          tags$b(style = paste0("color:", th$main, ";"), icon("hashtag"), " Variables :"),
          tags$ul(style = "margin:2px 0 0 12px; padding:0; color:#333;",
                  lapply(variables, function(x) tags$li(HTML(x))))
        )
      ),
      uiOutput(paste0("mv_", key, "_conditions"))
    )
  )
}

.mv_category_header <- function(label, ic, color) {
  fluidRow(column(12,
    div(style = paste0("margin:18px 0 6px 0; padding:10px 16px; border-radius:8px;",
                       " background:", color, "; color:white;"),
        h3(style = "margin:0; font-weight:bold;", icon(ic), " ", label))
  ))
}

.mv_analysis_box <- function(key, title, cat, principes, objectifs, taille, variables,
                             intro = NULL) {
  th <- .mv_theme(cat)
  box(
    title = tagList(icon("project-diagram"), " ", title),
    status = th$status, width = 12, solidHeader = TRUE, collapsible = TRUE,
    collapsed = TRUE,

    if (!is.null(intro))
      p(style = "color:#555; font-style:italic; margin-bottom:8px;", icon("lightbulb"), " ", intro),

    .mv_info_panel(key, cat, principes, objectifs, taille, variables),

    uiOutput(paste0("mv_", key, "_controls")),

    div(style = "text-align:center; margin:12px 0;",
      actionButton(paste0("mv_", key, "_run"),
                   tagList(icon("play-circle"), " Lancer l'analyse"),
                   class = "btn-primary",
                   style = paste0("font-weight:bold; padding:8px 22px; background:",
                                  th$main, "; border-color:", th$main, ";"))
    ),

    uiOutput(paste0("mv_", key, "_status")),

    tabBox(width = 12,
      tabPanel(tagList(icon("table"), " Resultats & metriques"),
        withSpinner(uiOutput(paste0("mv_", key, "_metrics")), color = th$main)
      ),
      tabPanel(tagList(icon("chart-area"), " Graphique"),
        withSpinner(plotOutput(paste0("mv_", key, "_plot"), height = "560px"), color = th$main)
      ),
      tabPanel(tagList(icon("file-alt"), " Details techniques"),
        div(style = "max-height:520px; overflow-y:auto; font-family:'Courier New',monospace; font-size:12px; background:#fff; padding:14px; border-radius:5px;",
            verbatimTextOutput(paste0("mv_", key, "_summary")))
      )
    ),

    div(style = paste0("background:", th$grad, "; border-radius:10px; padding:14px; margin-top:8px;"),
      h4(style = "color:white; font-weight:bold; margin-top:0; text-align:center;",
         icon("file-export"), " Export des resultats"),
      fluidRow(column(12, style = "text-align:center;",
        downloadButton(paste0("mv_", key, "_dl_xlsx"),
                       HTML(paste0(as.character(icon("file-excel")), " <strong>Excel</strong>")),
                       class = "btn-success", style = "margin:4px; padding:7px 16px;"),
        downloadButton(paste0("mv_", key, "_dl_csv"),
                       HTML(paste0(as.character(icon("file-csv")), " <strong>CSV</strong>")),
                       class = "btn-warning", style = "margin:4px; padding:7px 16px;")
      ))
    )
  )
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(
      style = "display:inline-flex; align-items:baseline; gap:9px;",
      span(icon("flask"), style = "font-size:17px;"),
      span("HStat", style = "font-family:'Newsreader',serif; font-weight:600;"),
      span("/ analyse statistique",
           style = "font-family:'Archivo',sans-serif; font-weight:400; font-size:11px; letter-spacing:0.04em; opacity:0.75; text-transform:none;")
    ),
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
      tags$li(class = "header", "Données"),
      menuItem("Chargement", tabName = "load", icon = icon("upload")),
      menuItem("Exploration", tabName = "explore", icon = icon("binoculars")),
      menuItem("Nettoyage", tabName = "clean", icon = icon("broom")),
      menuItem("Filtrage", tabName = "filter", icon = icon("filter")),
      tags$li(class = "header", "Analyses"),
      menuItem("Analyses descriptives", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("Tableaux croisés", tabName = "crosstab", icon = icon("table")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Tests statistiques", tabName = "tests", icon = icon("calculator")),
      menuItem("Comparaisons post-hoc", tabName = "multiple", icon = icon("sort-amount-down")),
      menuItem("Analyses multivariées", tabName = "multivariate", icon = icon("project-diagram")),
      menuItem("Seuils d'efficacité", tabName = "threshold", icon = icon("gauge-high")),
      tags$li(class = "header", "Actions"),
      div(style = "padding:10px 14px;",
        div(style = "margin-bottom:10px;",
          tags$label(`for` = "globalSeed",
            style = "color:#b8c7ce; font-size:10px; font-weight:700; letter-spacing:0.08em; text-transform:uppercase;",
            icon("dice"), " Graine aléatoire"),
          numericInput("globalSeed", label = NULL, value = 123, min = 1, max = 1e9, step = 1),
          tags$small(style = "color:#8aa4b1; font-size:10.5px; line-height:1.4; display:block;",
            "Garantit des résultats reproductibles (k-means, LCA, échantillonnage…).")),
        actionButton("helpBtn", "Aide", icon = icon("question-circle"),
                     class = "btn-info btn-block"),
        actionButton("resetBtn", "Réinitialiser", icon = icon("redo"),
                     class = "btn-warning btn-block"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    useShinyalert(force = TRUE),
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('expandBox', function(boxId) {
          var wrap = document.getElementById(boxId);
          if (!wrap) return;
          var box = wrap.classList.contains('box') ? wrap : wrap.querySelector('.box');
          if (box && box.classList.contains('collapsed-box')) {
            var btn = box.querySelector('[data-widget=\"collapse\"]');
            if (btn) btn.click();
          }
        });
        Shiny.addCustomMessageHandler('collapseBox', function(boxId) {
          var attempt = function(tries) {
            var wrap = document.getElementById(boxId);
            if (!wrap) { if (tries > 0) setTimeout(function(){attempt(tries-1);}, 150); return; }
            var box = wrap.classList.contains('box') ? wrap : wrap.querySelector('.box');
            if (!box) { if (tries > 0) setTimeout(function(){attempt(tries-1);}, 150); return; }
            if (!box.classList.contains('collapsed-box')) {
              var btn = box.querySelector('[data-widget=\"collapse\"]');
              if (btn) btn.click();
            }
          };
          attempt(10);
        });
      ")),
      # ---- Polices professionnelles (locales, sans dependance reseau) ----
      # Newsreader : serif editoriale a fort contraste -> titres.
      # Archivo    : grotesque neo-suisse -> corps et interface.
      tags$style(HTML("
        @font-face { font-family:'Newsreader'; font-style:normal; font-weight:400;
          font-display:swap; src:url('fonts/newsreader-latin-400-normal.woff2') format('woff2'); }
        @font-face { font-family:'Newsreader'; font-style:italic; font-weight:400;
          font-display:swap; src:url('fonts/newsreader-latin-400-italic.woff2') format('woff2'); }
        @font-face { font-family:'Newsreader'; font-style:normal; font-weight:500;
          font-display:swap; src:url('fonts/newsreader-latin-500-normal.woff2') format('woff2'); }
        @font-face { font-family:'Newsreader'; font-style:normal; font-weight:600;
          font-display:swap; src:url('fonts/newsreader-latin-600-normal.woff2') format('woff2'); }
        @font-face { font-family:'Archivo'; font-style:normal; font-weight:400;
          font-display:swap; src:url('fonts/archivo-latin-400-normal.woff2') format('woff2'); }
        @font-face { font-family:'Archivo'; font-style:normal; font-weight:500;
          font-display:swap; src:url('fonts/archivo-latin-500-normal.woff2') format('woff2'); }
        @font-face { font-family:'Archivo'; font-style:normal; font-weight:600;
          font-display:swap; src:url('fonts/archivo-latin-600-normal.woff2') format('woff2'); }
        @font-face { font-family:'Archivo'; font-style:normal; font-weight:700;
          font-display:swap; src:url('fonts/archivo-latin-700-normal.woff2') format('woff2'); }
      ")),

      # ---- Typographie HStat (sans toucher aux couleurs natives de Shiny) ----
      # On conserve le theme bleu par defaut d'AdminLTE (skin = "blue") pour une
      # experience familiere et confortable ; on applique uniquement les polices
      # professionnelles (Newsreader pour les titres, Archivo pour le corps) et
      # quelques raffinements neutres (rythme, lisibilite).
      tags$style(HTML("
        /* -- Variables typographiques -- */
        :root {
          --serif: 'Newsreader', Georgia, 'Times New Roman', serif;
          --sans:  'Archivo', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          --mono:  ui-monospace, 'Cascadia Code', 'JetBrains Mono', Menlo, Consolas, monospace;
        }

        /* -- Corps en grotesque Archivo -- */
        body, .content-wrapper, .wrapper, .main-sidebar, .left-side,
        .main-header .logo, .main-header .navbar, .form-control, .btn,
        label, .selectize-input, .nav-tabs > li > a, table.dataTable,
        .box-body, .small-box p, .info-box-text {
          font-family: var(--sans);
          -webkit-font-smoothing: antialiased;
          -moz-osx-font-smoothing: grayscale;
        }
        body { font-size: 14px; line-height: 1.6; letter-spacing: -0.002em; }

        /* -- Titres en serif editoriale Newsreader -- */
        h1, h2, h3, h4,
        .box-header .box-title, .modal-title {
          font-family: var(--serif);
          font-weight: 600;
          letter-spacing: -0.01em;
        }
        /* h5/h6 : etiquettes en grotesque -- */
        h5, h6 {
          font-family: var(--sans);
          font-weight: 700;
          letter-spacing: 0.04em;
        }
        /* Chiffres-cles des value boxes en serif -- */
        .small-box h3, .info-box-number { font-family: var(--serif); font-weight: 600; }

        /* Logo de l'en-tete : nom en serif -- */
        .main-header .logo { font-family: var(--serif); font-weight: 600; }

        /* Code / sorties console en monospace -- */
        pre, code, .shiny-text-output, samp { font-family: var(--mono); font-size: 13px; }

        /* -- Quelques raffinements neutres de lisibilite -- */
        .box-header .box-title { font-size: 18px; }
        label, .control-label { font-weight: 600; }
        .nav-tabs > li > a { font-weight: 600; }
        .btn { font-weight: 600; letter-spacing: 0.01em; }

        /* -- Accessibilite : focus clavier visible -- */
        a:focus-visible, button:focus-visible, .btn:focus-visible,
        input:focus-visible, select:focus-visible, textarea:focus-visible,
        [tabindex]:focus-visible {
          outline: 2px solid #3c8dbc !important;
          outline-offset: 2px !important;
        }
      ")),
      tags$script(HTML("
        $(document).ready(function() {

          // Raccourcis clavier : Enter dans un champ titre/axe/legende
          // declenche immediatement la mise a jour du graphique
          var titleInputIds = [
            'plotTitle', 'legendTitle', 'xAxisLabel', 'yAxisLabel',
            'quickPlotTitle', 'quickXLabel', 'quickYLabel',
            'customTitle', 'customSubtitle', 'customLegendTitle',
            'descPlotTitle', 'crosstabTitle',
            'distTitle', 'missingTitle',
            'pcaPlotTitle', 'hcpcClusterTitle', 'hcpcDendTitle',
            'afdIndTitle', 'afdVarTitle',
            'thresholdPlotTitle', 'thresholdLegendTitle'
          ];

          titleInputIds.forEach(function(id) {
            $(document).on('keydown', '#' + id, function(e) {
              if (e.key === 'Enter') {
                e.preventDefault();
                $(this).trigger('change');
              }
            });
          });

          // Double-clic sur un champ texte/numerique : selectionner tout le contenu
          $(document).on('dblclick', 'input[type=text], input[type=number], textarea', function() {
            this.select();
          });

          // Ctrl+A dans un champ texte : selectionner tout le contenu du champ
          // (evite que Ctrl+A selectionne toute la page)
          $(document).on('keydown', 'input[type=text], textarea', function(e) {
            if ((e.ctrlKey || e.metaKey) && e.key === 'a') {
              e.stopPropagation();
              this.select();
            }
          });

          // ---- Raccourcis Gras (Ctrl+B) et Italique (Ctrl+I) ----
          // Wrap le texte selectionne (ou le mot courant) avec <b>...</b> ou <i>...</i>
          // Compatible avec ggtext::element_markdown() pour le rendu dans les graphiques.
          function wrapSelection(input, tagOpen, tagClose) {
            var start = input.selectionStart;
            var end   = input.selectionEnd;
            var val   = input.value;
            var selected = val.substring(start, end);

            // Si rien n'est selectionne, on tente de selectionner le mot courant
            if (start === end) {
              var left  = start;
              var right = end;
              while (left > 0 && val.charCodeAt(left - 1) > 32) left--;
              while (right < val.length && val.charCodeAt(right) > 32) right++;
              selected = val.substring(left, right);
              start = left;
              end   = right;
            }

            // Si deja entoure du meme tag -> on retire (toggle)
            var fullTag = tagOpen + selected + tagClose;
            var before  = val.substring(0, start);
            var after   = val.substring(end);

            // Detecter si la selection est deja wrappee (toggle off)
            var tagLen = tagOpen.length;
            if (before.endsWith(tagOpen) && after.startsWith(tagClose)) {
              // Retirer le tag
              var newVal = before.slice(0, before.length - tagLen) + selected + after.slice(tagClose.length);
              input.value = newVal;
              input.setSelectionRange(start - tagLen, start - tagLen + selected.length);
            } else {
              // Ajouter le tag
              var newVal = before + fullTag + after;
              input.value = newVal;
              input.setSelectionRange(start + tagLen, start + tagLen + selected.length);
            }

            // Notifier Shiny du changement
            $(input).trigger('input').trigger('change');
          }

          $(document).on('keydown', 'input[type=text], textarea', function(e) {
            if ((e.ctrlKey || e.metaKey) && e.key === 'b') {
              e.preventDefault();
              wrapSelection(this, '<b>', '</b>');
            }
            if ((e.ctrlKey || e.metaKey) && e.key === 'i') {
              e.preventDefault();
              wrapSelection(this, '<i>', '</i>');
            }
          });

        });
      "))
    ),
    tabItems(
      # ---- Chargement ----
      tabItem(tabName = "load",
              fluidRow(
                box(title = "Charger données", status = "primary", width = 12, solidHeader = TRUE,
                    fileInput("file", "Choisir un fichier",
                              accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv",
                                         ".sav", ".dta", ".rds", ".parquet", ".duckdb")),
                    tags$small(style = "color:#6b7280; display:block; margin:-8px 0 10px 0;",
                               icon("circle-info"),
                               " Formats : CSV, TXT, Excel, SPSS, Stata, RDS, Parquet, DuckDB. ",
                               "Les fichiers volumineux (CSV/Parquet) basculent automatiquement en mode hors-mémoire."),
                    uiOutput("sheetUI"),
                    radioButtons("sep", "Séparateur (CSV/TXT)",
                                 choices = c(Virgule = ",", `Point-virgule` = ";", Tab = "\t"), selected = ","),
                    checkboxInput("header", "Avec en-têtes", TRUE),
                    tags$details(
                      style = "margin:8px 0; padding:8px 12px; background:#f4f6f8; border-radius:8px;",
                      tags$summary(style = "cursor:pointer; font-weight:600; color:#4b5563; font-size:13px;",
                                   icon("sliders"), " Options avancées (gros fichiers)"),
                      div(style = "padding-top:10px;",
                        fluidRow(
                          column(6, numericInput("bigDataThreshold",
                                     "Seuil hors-mémoire (Mo)", value = 500, min = 50, max = 100000, step = 50)),
                          column(6, numericInput("sampleSize",
                                     "Taille de l'échantillon (lignes)", value = 100000, min = 1000, max = 5000000, step = 10000))
                        ),
                        tags$small(style = "color:#6b7280;",
                          "Au-delà du seuil, le fichier n'est pas chargé en RAM : DuckDB l'interroge sur disque ",
                          "et les analyses portent sur un échantillon représentatif de la taille indiquée."))
                    ),
                    actionButton("loadData", "Charger", class = "btn-primary", icon = icon("upload")),
                    hr(),
                    h4("Exemple de données"),
                    p("Vous pouvez utiliser vos propres données ou télécharger un exemple pour tester l'application:"),
                    tags$div(
                      style = "margin-top: 10px;",
                      downloadButton(
                        "downloadExample",
                        label  = tagList(icon("download"), " Télécharger l'exemple CSV"),
                        class  = "btn-info btn-block",
                        style  = "display: inline-block !important; width: auto;"
                      ),
                      tags$small(style="color:#7f8c8d; display:block; margin-top:5px;",
                                 icon("info-circle"),
                                 " 120 observations, 12 variables (numérique, facteur, NA inclus)"
                      )
                    )
                )
              ),
              fluidRow(column(12, uiOutput("dataModeBanner"))),
              # Controle global de l'echantillon (mode hors-memoire uniquement)
              conditionalPanel(
                condition = "output.hstatBigData == true",
                fluidRow(column(12,
                  box(title = tagList(icon("vials"), " Échantillon de travail"),
                      status = "warning", width = 12, solidHeader = TRUE,
                    p(style = "color:#5a6a7a; font-size:13px;",
                      "Les analyses qui ajustent un modèle (ANOVA, régression, ACP, classifications, multivariées…) ",
                      "ne peuvent pas s'exécuter sur la totalité d'un très gros fichier. Elles travaillent sur cet échantillon. ",
                      "Vous pouvez l'agrandir autant que la mémoire de votre machine le permet, puis le retirer."),
                    fluidRow(
                      column(5, numericInput("sampleSizeLive",
                               tagList(icon("arrows-up-down"), " Taille de l'échantillon (lignes)"),
                               value = 100000, min = 1000, max = 20000000, step = 50000)),
                      column(7, div(style = "margin-top:25px;",
                        actionButton("redrawSample",
                          tagList(icon("rotate"), " Re-tirer l'échantillon"),
                          class = "btn-warning", style = "font-weight:bold;"),
                        tags$span(style = "margin-left:12px; font-size:12px; color:#6b7280;",
                          "Un nouveau tirage aléatoire remplace l'échantillon courant ; relancez ensuite vos analyses.")))
                    ),
                    uiOutput("sampleInfoLine")
                  )
                ))
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
              mod_explore_ui("explore")
      ),
      # ---- Nettoyage ----
      
      tabItem(tabName = "clean",
              mod_clean_ui("clean")
      ),
      # ---- Filtrage ----
      
      tabItem(tabName = "filter",
              mod_filter_ui("filter")
      ),
      # ---- Analyse descriptives ----
      tabItem(tabName = "descriptive",
              mod_descriptive_ui("descriptive")
      ),
      
      # ---- Tableaux croisés dynamiques  ----
      
      tabItem(tabName = "crosstab",
              mod_crosstab_ui("crosstab")
      ),
      # ---- Visualisation des données ----
      tabItem(tabName = "visualization",
              mod_viz_ui("visualization")
      ),
      # ---- Tests statistiques ----
      mod_tests_ui("tests"),
      # ---- Comparaisons post-hoc ----
      mod_posthoc_ui("tests"),
      # ---- Analyses multivariees ----
      
      tabItem(tabName = "multivariate",
              .hstat_scope_banner(exact = FALSE),
              # ACP
              fluidRow(
                box(title = "Analyse en Composantes Principales (ACP)", status = "info", width = 6, solidHeader = TRUE,
                    
                    # - Panneau Info ACP -
                    div(style = "margin-bottom:10px;",
                        div(
                          style = "cursor:pointer; background:linear-gradient(135deg,#1565c0,#1976d2); color:white; padding:8px 12px; border-radius:6px; font-weight:bold; font-size:13px; user-select:none;",
                          onclick = "var c=document.getElementById('pca-info-body'); c.style.display=(c.style.display==='none'?'block':'none');",
                          icon("info-circle"), " Principes, objectifs & conditions de l'ACP ",
                          icon("chevron-down")
                        ),
                        div(id = "pca-info-body", style = "display:none; background:#e8f4f8; border:1px solid #90caf9; border-radius:0 0 6px 6px; padding:12px; font-size:12px;",
                            fluidRow(
                              column(6,
                                     tags$b(style="color:#1565c0;", icon("drafting-compass"), " Principes :"),
                                     tags$p(style="margin:2px 0 6px 0; color:#333;", "Réduction dimensionnelle par rotation orthogonale maximisant la variance. Transforme p variables corrélées en composantes principales indépendantes (valeurs propres de la matrice de corrélation)."),
                                     tags$b(style="color:#1565c0;", icon("bullseye"), " Objectifs :"),
                                     tags$p(style="margin:2px 0 0 0; color:#333;", "Explorer la structure, identifier des patterns, visualiser des données multivariées, réduire la dimensionnalité avant classification.")
                              ),
                              column(6,
                                     tags$b(style="color:#1565c0;", icon("ruler"), " Taille nécessaire :"),
                                     tags$ul(style="margin:2px 0 6px 12px; padding:0; color:#333;",
                                             tags$li("Minimum absolu : n", icon("arrow-right"), "30"),
                                             tags$li("Recommandé : n", icon("arrow-right"), "max(50, 5xp)"),
                                             tags$li("Idéal : n", icon("arrow-right"), "100 pour stabilité")
                                     ),
                                     tags$b(style="color:#1565c0;", icon("hashtag"), " Variables minimales :"),
                                     tags$ul(style="margin:2px 0 0 12px; padding:0; color:#333;",
                                             tags$li("Minimum absolu : p", icon("arrow-right"), "2"),
                                             tags$li("Recommandé : p", icon("arrow-right"), "3-5"),
                                             tags$li("Toutes doivent être numériques")
                                     )
                              )
                            ),
                            uiOutput("pcaConditionsCheck")
                        )
                    ),
                    
                    uiOutput("pcaVarSelect"),
                    
                    # Panel colinéarité (affiché dynamiquement si colinéarité détectée)
                    uiOutput("pcaCollinearityPanel"),
                    
                    checkboxInput("pcaScale", "Standardiser les variables", TRUE),
                    checkboxInput("pcaUseMeans", "Utiliser les moyennes par groupe", FALSE),
                    conditionalPanel(
                      condition = "input.pcaUseMeans == true",
                      uiOutput("pcaMeansGroupSelect"),
                      actionButton("pcaRefresh", "Actualiser l'ACP", 
                                   icon = icon("sync"), 
                                   class = "btn-info btn-sm",
                                   style = "margin-bottom: 10px;")
                    ),
                    uiOutput("pcaQualiSupSelect"),
                    uiOutput("pcaIndSupSelect"),
                    uiOutput("pcaLabelSourceSelect"),
                    hr(),
                    radioButtons("pcaPlotType", "Type de visualisation:",
                                 choices = c("Variables" = "var", "Individus" = "ind", "Biplot" = "biplot"),
                                 selected = "var", inline = TRUE),
                    
                    # - Coloration -
                    div(style = "background:#f0f7ff; border-left:3px solid #2196f3; padding:8px 12px; margin:6px 0 10px 0; border-radius:0 4px 4px 0;",
                        selectInput("pcaColorBy", 
                                    tagList(icon("palette"), " Colorer les éléments par :"),
                                    choices = c(
                                      "Contribution (% à la composante)"       = "contrib",
                                      "Cos² (qualité de représentation)"       = "cos2",
                                      "Indice de saturation (|corrélation|)"   = "sat"
                                    ),
                                    selected = "contrib"),
                        uiOutput("pcaColorByLegend")
                    ),
                    numericInput("pcaComponents", "Nombre de composantes:", value = 5, min = 2, max = 10),
                    
                    # Options rotation orthogonale
                    div(style = "background-color: #f8f9fa; border-left: 4px solid #495057; padding: 10px; margin: 10px 0;",
                        h5(style = "margin-top: 0; color: #343a40;", icon("sync-alt"), " Rotation orthogonale"),
                        p(style = "font-size: 11px; color: #555; margin-bottom: 8px;",
                          "La rotation simplifie la structure factorielle pour faciliter l'interprétation des composantes. Elle ne modifie pas la variance totale expliquée."),
                        fluidRow(
                          column(6,
                                 selectInput("pcaRotationMethod", "Méthode de rotation:",
                                             choices = c("Varimax" = "varimax",
                                                         "Quartimax" = "quartimax",
                                                         "Oblimin" = "oblimin",
                                                         "Aucune" = "none"),
                                             selected = "varimax")
                          ),
                          column(6,
                                 numericInput("pcaRotationNFactors", "Nombre de facteurs à rotationner:", 
                                              value = 2, min = 2, max = 10)
                          )
                        )
                    ),
                    
                    # Sélection des axes de l'ACP
                    div(style = "background-color: #f8f9fa; border-left: 3px solid #6c757d; padding: 10px; margin: 10px 0;",
                        h5(style = "margin-top: 0; color: #495057;", icon("chart-line"), " Sélection des axes à représenter"),
                        fluidRow(
                          column(6,
                                 uiOutput("pcaAxisXSelect")
                          ),
                          column(6,
                                 uiOutput("pcaAxisYSelect")
                          )
                        ),
                        p(style = "margin: 5px 0 0 0; font-size: 11px; color: #495057; font-style: italic;",
                          icon("info-circle"), " Choisissez les axes à afficher sur votre graphique")
                    ),
                    
                    hr(),
                    # Option d'arrondi pour les résultats ACP
                    div(style = "background-color: #f8f9fa; border-left: 4px solid #6c757d; padding: 10px; margin: 10px 0;",
                        fluidRow(
                          column(6,
                                 checkboxInput("pcaRoundResults", "Arrondir les résultats", value = FALSE)
                          ),
                          column(6,
                                 conditionalPanel(
                                   condition = "input.pcaRoundResults == true",
                                   numericInput("pcaDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                 )
                          )
                        )
                    ),
                    hr(),
                    h5("Personnalisation graphique:", style = "font-weight: bold; color: #495057;"),
                    textInput("pcaPlotTitle", "Titre du graphique:", 
                              value = "ACP - Analyse en Composantes Principales"),
                    textInput("pcaXLabel", "Label axe X:", value = ""),
                    textInput("pcaYLabel", "Label axe Y:", value = ""),
                    checkboxInput("pcaCenterAxes", "Centrer sur (0,0)", TRUE),
                    hr(),
                    h5("Options de Téléchargement graphique:", style = "font-weight: bold; color: #495057;"),
                    fluidRow(
                      column(6,
                             selectInput("pcaPlot_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")
                      ),
                      column(6,
                             numericInput("pcaPlot_dpi", "Resolution (DPI):", value = 300, min = 72, max = 1200)
                      )
                    ),
                    fluidRow(
                      column(6,
                             numericInput("pcaPlot_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                      ),
                      column(6,
                             numericInput("pcaPlot_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                      )
                    ),
                    hr(),
                    div(style = "text-align: center;",
                        downloadButton("downloadPcaPlot", "Télécharger graphique", class = "btn-info", style = "margin: 5px;"),
                        br(), br(),
                        downloadButton("downloadPcaDataXlsx", "Télécharger données (Excel)", class = "btn-success", style = "margin: 5px;"),
                        downloadButton("downloadPcaDataCsv", "Télécharger données (CSV)", class = "btn-success", style = "margin: 5px;")
                    )
                ),
                box(title = "Visualisation ACP", status = "info", width = 6, solidHeader = TRUE,
                    plotlyOutput("pcaPlot", height = "550px"),
                    hr(),
                    
                    # Métriques de validation ACP en ordre logique
                    div(style = "background-color: #f8f9fa; border-left: 5px solid #495057; padding: 12px; margin: 10px 0;",
                        h5(style = "color: #343a40; font-weight: bold; margin-top: 0;",
                           icon("clipboard-check"), " Métriques de validation de l'ACP"),
                        p(style = "font-size: 11px; color: #555; margin-bottom: 0;",
                          "Les métriques suivantes permettent d'évaluer la qualité et la robustesse de l'analyse. Elles doivent être analysées dans l'ordre présenté.")
                    ),
                    
                    # 1. Bartlett + KMO
                    h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                       icon("check-circle"), " Adéquation des données à l'ACP"),
                    uiOutput("pcaBartlettKMO"),
                    
                    # 2. Scree plot
                    h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                       icon("chart-line"), " Graphique des éboulis (Scree Plot)"),
                    p(style = "font-size: 11px; color: #666; font-style: italic;",
                      "Critère de Kaiser (valeur propre min. 1) : les composantes en vert sont retenues. Cherchez le 'coude' de la courbe."),
                    plotOutput("pcaScreePlot", height = "320px"),
                    fluidRow(
                      column(6, selectInput("pcaScree_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")),
                      column(6, numericInput("pcaScree_dpi", "DPI:", value = 300, min = 72, max = 1200))
                    ),
                    div(style = "text-align: center; margin-bottom: 10px;",
                        downloadButton("downloadPcaScreePlot", "Télécharger Scree Plot", class = "btn-info btn-sm")
                    ),
                    
                    # 3. Analyse parallèle
                    h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                       icon("random"), " Analyse parallèle de Horn"),
                    p(style = "font-size: 11px; color: #666; font-style: italic;",
                      "Méthode plus rigoureuse que Kaiser : retenir les composantes dont la valeur propre observée dépasse le percentile 95 des simulations aléatoires."),
                    plotOutput("pcaParallelPlot", height = "320px"),
                    fluidRow(
                      column(6, selectInput("pcaParallel_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")),
                      column(6, numericInput("pcaParallel_dpi", "DPI:", value = 300, min = 72, max = 1200))
                    ),
                    div(style = "text-align: center; margin-bottom: 10px;",
                        downloadButton("downloadPcaParallelPlot", "Télécharger Analyse Parallèle", class = "btn-info btn-sm")
                    ),
                    
                    # 4. CTR
                    h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                       icon("percentage"), " Contributions absolues (CTR) des variables"),
                    p(style = "font-size: 11px; color: #666; font-style: italic;",
                      "Seuil théorique = 100% / nb variables. Les variables au-dessus du seuil (en vert) structurent principalement l'axe."),
                    uiOutput("pcaCTRAxisSelect"),
                    plotOutput("pcaCTRPlot", height = "300px"),
                    fluidRow(
                      column(6, selectInput("pcaCTR_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")),
                      column(6, numericInput("pcaCTR_dpi", "DPI:", value = 300, min = 72, max = 1200))
                    ),
                    div(style = "text-align: center; margin-bottom: 10px;",
                        downloadButton("downloadPcaCTRPlot", "Télécharger Graphique CTR", class = "btn-info btn-sm")
                    ),
                    
                    # 5. Rotation
                    h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                       icon("sync-alt"), " Résultats de la rotation orthogonale"),
                    p(style = "font-size: 11px; color: #666; font-style: italic;",
                      "La rotation simplifie la structure : un loading |x| min. 0,70 indique une contribution forte, |x| de 0,40 à 0,70 une contribution modérée."),
                    div(style = "max-height: 350px; overflow-y: auto; font-size: 11px;",
                        verbatimTextOutput("pcaRotationResult")),
                    
                    hr(),
                    div(style = "max-height: 300px; overflow-y: auto; font-size: 12px;",
                        verbatimTextOutput("pcaSummary")),
                    
                    hr(),
                    # ---- Export métriques ACP 
                    div(style = "background: linear-gradient(135deg, #343a40 0%, #495057 100%); border-radius: 10px; padding: 18px; margin-top: 10px;",
                        h4(style = "color: white; font-weight: bold; margin-top: 0; text-align: center;",
                           icon("file-export"), " Export des métriques ACP"),
                        p(style = "color: #aed6f1; font-size: 12px; text-align: center; margin-bottom: 12px;",
                          "Valeurs propres, Bartlett/KMO, Contributions absolues (CTR), Qualité de représentation (cos²)"),
                        fluidRow(
                          column(12, style = "text-align: center;",
                                 downloadButton("downloadPcaMetricsXlsx",
                                                HTML(paste0(as.character(icon("file-excel")), " <strong>Métriques ACP (Excel)</strong>")),
                                                class = "btn-success",
                                                style = "margin: 4px; padding: 7px 16px;"),
                                 downloadButton("downloadPcaMetricsCsv",
                                                HTML(paste0(as.character(icon("file-csv")), " <strong>Métriques ACP (CSV)</strong>")),
                                                class = "btn-warning",
                                                style = "margin: 4px; padding: 7px 16px;")
                          )
                        )
                    )
                )
              ),
              
              # HCPC
              fluidRow(
                box(title = "Classification Hierarchique sur Composantes Principales (HCPC)", 
                    status = "success", width = 12, solidHeader = TRUE,
                    p("Cette analyse combine l'ACP avec une classification hierarchique automatique."),
                    
                    # - Panneau Info HCPC -
                    div(style = "margin-bottom:10px;",
                        div(
                          style = "cursor:pointer; background:linear-gradient(135deg,#2e7d32,#388e3c); color:white; padding:8px 12px; border-radius:6px; font-weight:bold; font-size:13px; user-select:none;",
                          onclick = "var c=document.getElementById('hcpc-info-body'); c.style.display=(c.style.display==='none'?'block':'none');",
                          icon("info-circle"), " Principes, objectifs & conditions de la HCPC ",
                          icon("chevron-down")
                        ),
                        div(id = "hcpc-info-body", style = "display:none; background:#e8f5e9; border:1px solid #a5d6a7; border-radius:0 0 6px 6px; padding:12px; font-size:12px;",
                            fluidRow(
                              column(6,
                                     tags$b(style="color:#2e7d32;", icon("drafting-compass"), " Principes :"),
                                     tags$p(style="margin:2px 0 6px 0; color:#333;", "Classification ascendante hiérarchique (critère de Ward) appliquée aux coordonnées ACP. Minimise la variance intra-cluster à chaque fusion. La coupure optimale est déterminée par le saut maximal des hauteurs."),
                                     tags$b(style="color:#2e7d32;", icon("bullseye"), " Objectifs :"),
                                     tags$p(style="margin:2px 0 0 0; color:#333;", "Identifier des groupes naturels (typologies), profiler les individus similaires, réduire les données individuelles en groupes interprétables.")
                              ),
                              column(6,
                                     tags$b(style="color:#2e7d32;", icon("ruler"), " Taille nécessaire :"),
                                     tags$ul(style="margin:2px 0 6px 12px; padding:0; color:#333;",
                                             tags$li("Minimum absolu : n", icon("arrow-right"), "2xk (k=clusters)"),
                                             tags$li("Recommandé : n", icon("arrow-right"), "10xk pour stabilité"),
                                             tags$li("Hérite des conditions de l'ACP")
                                     ),
                                     tags$b(style="color:#2e7d32;", icon("hashtag"), " Composantes nécessaires :"),
                                     tags$ul(style="margin:2px 0 0 12px; padding:0; color:#333;",
                                             tags$li("Minimum : ", icon("arrow-right"), "1 composante ACP"),
                                             tags$li("Recommandé : ", icon("arrow-right"), "2 composantes retenues (", tags$em("\u03bb"), " ", icon("arrow-right"), "1)"),
                                             tags$li("Utilise les composantes de l'ACP précédente")
                                     )
                              )
                            ),
                            uiOutput("hcpcConditionsCheck")
                        )
                    ),
                    
                    fluidRow(
                      column(4,
                             numericInput("hcpcClusters", "Nombre de clusters:", value = 3, min = 2, max = 10)
                      ),
                      column(8,
                             div(style = "text-align: center; margin-top: 25px;",
                                 downloadButton("downloadHcpcDataXlsx", "Télécharger données (Excel)", 
                                                class = "btn-success", style = "margin: 5px;"),
                                 downloadButton("downloadHcpcDataCsv", "Télécharger données (CSV)", 
                                                class = "btn-success", style = "margin: 5px;")
                             )
                      )
                    ),
                    
                    # Sélection des axes pour HCPC
                    div(style = "background-color: #e8f5e9; border-left: 4px solid #4caf50; padding: 10px; margin: 10px 0;",
                        h5(style = "margin-top: 0; color: #2e7d32;", icon("chart-line"), " Sélection des axes à représenter"),
                        fluidRow(
                          column(6,
                                 uiOutput("hcpcAxisXSelect")
                          ),
                          column(6,
                                 uiOutput("hcpcAxisYSelect")
                          )
                        ),
                        p(style = "margin: 5px 0 0 0; font-size: 11px; color: #1b5e20; font-style: italic;",
                          icon("info-circle"), " Ces axes s'appliquent à la carte des clusters")
                    ),
                    
                    hr(),
                    # Option d'arrondi pour les résultats HCPC
                    div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin: 10px 0;",
                        fluidRow(
                          column(6,
                                 checkboxInput("hcpcRoundResults", "Arrondir les résultats", value = FALSE)
                          ),
                          column(6,
                                 conditionalPanel(
                                   condition = "input.hcpcRoundResults == true",
                                   numericInput("hcpcDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                 )
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
                             h5("Options Téléchargement carte clusters:"),
                             p(style = "font-size: 11px; color: #5cb85c; font-style: italic;",
                               icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                             fluidRow(
                               column(6,
                                      selectInput("hcpcCluster_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")
                               ),
                               column(6,
                                      numericInput("hcpcCluster_dpi", "DPI:", value = 300, min = 72, max = 2000)
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("hcpcCluster_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                               ),
                               column(6,
                                      numericInput("hcpcCluster_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                               )
                             )
                      ),
                      column(6,
                             textInput("hcpcDendTitle", "Titre dendrogramme:", 
                                       value = "Dendrogramme HCPC"),
                             p(style = "font-style: italic; color: #666;", 
                               "Le dendrogramme n'est pas centré sur (0,0)"),
                             hr(),
                             h5("Options Téléchargement dendrogramme:"),
                             p(style = "font-size: 11px; color: #5cb85c; font-style: italic;",
                               icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                             fluidRow(
                               column(6,
                                      selectInput("hcpcDend_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")
                               ),
                               column(6,
                                      numericInput("hcpcDend_dpi", "DPI:", value = 300, min = 72, max = 2000)
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("hcpcDend_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                               ),
                               column(6,
                                      numericInput("hcpcDend_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
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
                                     downloadButton("downloadHcpcClusterPlot", "Télécharger carte")
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
                            h4(class = "box-title", "Resultats detailles HCPC", style = "color: white; font-weight: bold;")
                        ),
                        div(class = "box-body", style = "background-color: #f9f9f9;",
                            div(style = "max-height: 500px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                verbatimTextOutput("hcpcSummary"))
                        )
                    ),
                    
                    # ---- MÉTRIQUES DE VALIDATION HCPC 
                    div(style = "background-color: #eafaf1; border-left: 5px solid #27ae60; padding: 12px; margin: 15px 0 10px 0;",
                        h4(style = "color: #1e8449; font-weight: bold; margin-top: 0;",
                           icon("microscope"), " Métriques de validation de la classification"),
                        p(style = "font-size: 12px; color: #555; margin-bottom: 0;",
                          "Les métriques suivantes permettent d'évaluer la qualité et la robustesse de la partition obtenue. Elles doivent être analysées dans l'ordre présenté.")
                    ),
                    
                    # 1. Hauteurs de fusion
                    div(class = "box box-solid",
                        div(class = "box-header with-border", style = "background-color: #2980b9; color: white;",
                            h4(class = "box-title", style = "color: white;",
                               icon("chart-area"), " Graphique des hauteurs de fusion")
                        ),
                        div(class = "box-body",
                            p(style = "font-size: 12px; color: #555; font-style: italic;",
                              "Un saut important entre deux fusions consécutives suggère la coupure optimale du dendrogramme (règle du coude). Ce graphique complète la lecture visuelle du dendrogramme."),
                            plotOutput("hcpcHeightsPlot", height = "320px"),
                            fluidRow(
                              column(6, selectInput("hcpcHeights_format", "Format:",
                                                    choices = c("png", "svg", "pdf", "tiff"), selected = "png")),
                              column(6, numericInput("hcpcHeights_dpi", "DPI:", value = 300, min = 72, max = 1200))
                            ),
                            div(style = "text-align: center; margin-top: 4px;",
                                downloadButton("downloadHcpcHeightsPlot",
                                               "Télécharger hauteurs de fusion",
                                               class = "btn-info btn-sm")
                            )
                        )
                    ),
                    
                    # 2. CH, DB, Silhouette, Cophénétique
                    div(class = "box box-solid",
                        div(class = "box-header with-border", style = "background-color: #8e44ad; color: white;",
                            h4(class = "box-title", style = "color: white;",
                               icon("ruler-combined"), " Indices de validation des clusters")
                        ),
                        div(class = "box-body",
                            p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 12px;",
                              "Ces quatre indices évaluent respectivement la séparation inter-classes (CH), la compacité relative (DB), la cohérence individuelle (Silhouette) et la fidélité du dendrogramme (Cophénétique)."),
                            uiOutput("hcpcMetricsUI")
                        )
                    ),
                    
                    # 3. Stabilité
                    div(class = "box box-solid",
                        div(class = "box-header with-border", style = "background-color: #16a085; color: white;",
                            h4(class = "box-title", style = "color: white;",
                               icon("shield-alt"), " Stabilité par sous-échantillonnage")
                        ),
                        div(class = "box-body",
                            p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 12px;",
                              "Évalue si la structure de clusters est reproductible sur des sous-échantillons aléatoires des données. Un indice de Rand proche de 1 confirme la robustesse de la partition."),
                            uiOutput("hcpcStabilityUI")
                        )
                    ),
                    
                    # ---- Export métriques HCPC 
                    div(style = "background: linear-gradient(135deg, #0e6655 0%, #117a65 100%); border-radius: 10px; padding: 18px; margin-top: 20px;",
                        h4(style = "color: white; font-weight: bold; margin-top: 0; text-align: center;",
                           icon("file-export"), " Export des métriques HCPC / CAH"),
                        p(style = "color: #a9dfbf; font-size: 12px; text-align: center; margin-bottom: 12px;",
                          "Indices CH, Davies-Bouldin, Silhouette, corrélation cophénétique, affectation des individus aux clusters"),
                        fluidRow(
                          column(12, style = "text-align: center;",
                                 downloadButton("downloadHcpcMetricsXlsx",
                                                HTML(paste0(as.character(icon("file-excel")), " <strong>Métriques HCPC (Excel)</strong>")),
                                                class = "btn-success",
                                                style = "margin: 4px; padding: 7px 16px;"),
                                 downloadButton("downloadHcpcMetricsCsv",
                                                HTML(paste0(as.character(icon("file-csv")), " <strong>Métriques HCPC (CSV)</strong>")),
                                                class = "btn-warning",
                                                style = "margin: 4px; padding: 7px 16px;")
                          )
                        )
                    )
                )
              ),
              
              # AFD
              fluidRow(
                box(title = "Analyse Factorielle Discriminante (AFD)", 
                    status = "primary", width = 12, solidHeader = TRUE,
                    
                    # - Panneau Info AFD -
                    div(style = "margin-bottom:12px;",
                        div(
                          style = "cursor:pointer; background:linear-gradient(135deg,#1a237e,#283593); color:white; padding:8px 12px; border-radius:6px; font-weight:bold; font-size:13px; user-select:none;",
                          onclick = "var c=document.getElementById('afd-info-body'); c.style.display=(c.style.display==='none'?'block':'none');",
                          icon("info-circle"), " Principes, objectifs & conditions de l'AFD ",
                          icon("chevron-down")
                        ),
                        div(id = "afd-info-body", style = "display:none; background:#e8eaf6; border:1px solid #9fa8da; border-radius:0 0 6px 6px; padding:12px; font-size:12px;",
                            fluidRow(
                              column(6,
                                     tags$b(style="color:#1a237e;", icon("drafting-compass"), " Principes :"),
                                     tags$p(style="margin:2px 0 6px 0; color:#333;", "Recherche les combinaisons linéaires de variables (fonctions discriminantes) maximisant le ratio variance inter-groupes / variance intra-groupes (critère de Fisher-Rao). Généralisation multivariée de l'ANOVA."),
                                     tags$b(style="color:#1a237e;", icon("bullseye"), " Objectifs :"),
                                     tags$p(style="margin:2px 0 0 0; color:#333;", "Discriminer des individus dans des groupes prédéfinis, identifier les variables les plus discriminantes, prédire l'appartenance à un groupe pour de nouveaux individus.")
                              ),
                              column(6,
                                     tags$b(style="color:#1a237e;", icon("ruler"), " Taille nécessaire :"),
                                     tags$ul(style="margin:2px 0 6px 12px; padding:0; color:#333;",
                                             tags$li("Minimum absolu : n > p + g - 1"),
                                             tags$li("Recommandé : min. 20 obs. par groupe"),
                                             tags$li("Idéal : ratio n/p min. 10"),
                                             tags$li(tags$em("p = nb variables, g = nb groupes"))
                                     ),
                                     tags$b(style="color:#1a237e;", icon("hashtag"), " Variables minimales :"),
                                     tags$ul(style="margin:2px 0 0 12px; padding:0; color:#333;",
                                             tags$li("Minimum absolu : p min. 1"),
                                             tags$li("Recommandé : p min. 2"),
                                             tags$li("Maximum : p < n - g"),
                                             tags$li("Groupes minimum : g min. 2")
                                     )
                              )
                            ),
                            uiOutput("afdConditionsCheck")
                        )
                    ),
                    
                    # Sélection du facteur discriminant 
                    div(style = "background-color: #f8f9fa; border-left: 4px solid #343a40; padding: 15px; margin-bottom: 15px;",
                        h4(style = "color: #343a40; margin-top: 0;",
                           icon("bullseye"), " Variable à discriminer (OBLIGATOIRE)"),
                        p(style = "margin: 5px 0; font-size: 13px; color: #555;",
                          "Sélectionnez la variable catégorielle que vous souhaitez discriminer (prédire). Cette variable doit contenir au moins 2 groupes différents."),
                        uiOutput("afdFactorSelect"),
                        p(style = "margin: 5px 0 0 0; font-size: 11px; color: #e74c3c; font-weight: bold;",
                          icon("exclamation-circle"), " Si aucune variable n'apparaît, vérifiez que vos données contiennent des variables catégorielles (facteurs).")
                    ),
                    
                    # Sélection des variables quantitatives
                    div(style = "background-color: #f8f9fa; border-left: 4px solid #495057; padding: 15px; margin-bottom: 15px;",
                        h4(style = "color: #495057; margin-top: 0;",
                           icon("chart-line"), " Variables quantitatives (OBLIGATOIRE)"),
                        p(style = "margin: 5px 0; font-size: 13px; color: #555;",
                          "Sélectionnez les variables numériques qui serviront à discriminer les groupes. Plus il y a de variables pertinentes, meilleure sera la discrimination."),
                        uiOutput("afdVarSelect"),
                        p(style = "margin: 5px 0 0 0; font-size: 11px; color: #27ae60; font-style: italic;",
                          icon("check-circle"), " Conseil : Sélectionnez au moins 2-3 variables pour obtenir de bons résultats.")
                    ),
                    
                    # Panel colinéarité AFD (affiché dynamiquement si colinéarité détectée)
                    uiOutput("afdCollinearityPanel"),
                    
                    hr(),
                    h4(style = "color: #6c757d; margin-top: 10px;", icon("cogs"), " Options avancées (optionnel)"),
                    checkboxInput("afdUseMeans", "Utiliser les moyennes par groupe", FALSE),
                    conditionalPanel(
                      condition = "input.afdUseMeans == true",
                      uiOutput("afdMeansGroupSelect"),
                      p(style = "margin: 5px 0 10px 0; font-size: 11px; color: #6c757d;",
                        icon("lightbulb"), 
                        " Conseil: Utilisez la même variable que le facteur de discrimination pour une AFD sur moyennes de groupes."),
                      actionButton("afdRefresh", "Actualiser l'AFD", 
                                   icon = icon("sync"), 
                                   class = "btn-info btn-sm",
                                   style = "margin-bottom: 10px;")
                    ),
                    
                    # Sélection des axes pour AFD
                    div(style = "background-color: #e3f2fd; border-left: 4px solid #2196f3; padding: 10px; margin: 10px 0;",
                        h5(style = "margin-top: 0; color: #495057;", icon("chart-line"), " Sélection des axes à représenter"),
                        fluidRow(
                          column(6,
                                 uiOutput("afdAxisXSelect")
                          ),
                          column(6,
                                 uiOutput("afdAxisYSelect")
                          )
                        ),
                        p(style = "margin: 5px 0 0 0; font-size: 11px; color: #495057; font-style: italic;",
                          icon("info-circle"), " Choisissez les fonctions discriminantes à afficher")
                    ),
                    
                    uiOutput("afdPredictVarsSelect"),
                    div(style = "background-color: #d1ecf1; border-left: 4px solid #17a2b8; padding: 10px; margin: 10px 0;",
                        p(style = "margin: 0; font-size: 12px; color: #0c5460;",
                          icon("info-circle"), 
                          HTML(" <strong>Variables de prédiction:</strong> Sélectionnez des variables catégorielles supplémentaires pour enrichir la prédiction du modèle."))
                    ),
                    uiOutput("afdQualiSupSelect"),
                    conditionalPanel(
                      condition = "input.afdUseMeans == false || input.afdUseMeans == null",
                      div(style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin: 15px 0;",
                          checkboxInput("afdCrossValidation", 
                                        HTML("<strong>Activer la validation croisée (Leave-One-Out)</strong>"), 
                                        FALSE),
                          p(style = "margin: 5px 0 0 25px; font-size: 12px; color: #856404;",
                            icon("exclamation-triangle"), 
                            " ATTENTION: La validation croisée peut être très longue sur de grands jeux de données.")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.afdUseMeans == true",
                      div(style = "background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 15px 0;",
                          p(style = "margin: 0; font-size: 12px; color: #721c24;",
                            icon("info-circle"), 
                            HTML(" <strong>Note:</strong> La validation croisée Leave-One-Out n'est pas disponible avec les moyennes par groupe (nombre d'observations insuffisant)."))
                      )
                    ),
                    hr(),
                    # Option d'arrondi pour les résultats AFD
                    div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin: 10px 0;",
                        fluidRow(
                          column(6,
                                 checkboxInput("afdRoundResults", "Arrondir les résultats", value = FALSE)
                          ),
                          column(6,
                                 conditionalPanel(
                                   condition = "input.afdRoundResults == true",
                                   numericInput("afdDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                 )
                          )
                        )
                    ),
                    hr(),
                    h5("Personnalisation graphique:", style = "font-weight: bold; color: #495057;"),
                    fluidRow(
                      column(6,
                             textInput("afdIndTitle", "Titre projection individus:", 
                                       value = "AFD - Projection des individus"),
                             textInput("afdIndXLabel", "Label axe X:", value = ""),
                             textInput("afdIndYLabel", "Label axe Y:", value = ""),
                             checkboxInput("afdIndCenterAxes", "Centrer sur (0,0)", TRUE),
                             hr(),
                             h5("Options Téléchargement projection individus:"),
                             p(style = "font-size: 11px; color: #495057; font-style: italic;",
                               icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                             fluidRow(
                               column(6,
                                      selectInput("afdInd_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")
                               ),
                               column(6,
                                      numericInput("afdInd_dpi", "DPI:", value = 300, min = 72, max = 2000)
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("afdInd_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                               ),
                               column(6,
                                      numericInput("afdInd_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
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
                             h5("Options Téléchargement contribution variables:"),
                             p(style = "font-size: 11px; color: #495057; font-style: italic;",
                               icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                             fluidRow(
                               column(6,
                                      selectInput("afdVar_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")
                               ),
                               column(6,
                                      numericInput("afdVar_dpi", "DPI:", value = 300, min = 72, max = 2000)
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("afdVar_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                               ),
                               column(6,
                                      numericInput("afdVar_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                               )
                             )
                      )
                    ),
                    hr(),
                    div(style = "text-align: center;",
                        downloadButton("downloadAfdDataXlsx", "Télécharger données (Excel)", 
                                       class = "btn-success", style = "margin: 5px;"),
                        downloadButton("downloadAfdDataCsv", "Télécharger données (CSV)", 
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
                                     downloadButton("downloadAfdIndPlot", "Télécharger projection")
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
                                     downloadButton("downloadAfdVarPlot", "Télécharger contribution")
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
                            # Guide d'interprétation en entête
                            div(style = "background-color: #fdf2f8; border-left: 5px solid #d9534f; padding: 12px; margin-bottom: 15px;",
                                h5(style = "color: #922b21; font-weight: bold; margin-top: 0;",
                                   icon("microscope"), " Métriques de validation de la classification"),
                                p(style = "font-size: 12px; color: #555; margin-bottom: 0;",
                                  "Les métriques suivantes permettent d'évaluer la qualité et la robustesse de la partition obtenue. Elles doivent être analysées dans l'ordre présenté.")
                            ),
                            div(style = "max-height: 700px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                uiOutput("afdSummary"))
                        )
                    ),
                    
                    # ---- Export métriques AFD 
                    div(style = "background: linear-gradient(135deg, #641e16 0%, #922b21 100%); border-radius: 10px; padding: 18px; margin-top: 10px;",
                        h4(style = "color: white; font-weight: bold; margin-top: 0; text-align: center;",
                           icon("file-export"), " Export des métriques AFD"),
                        p(style = "color: #f1948a; font-size: 12px; text-align: center; margin-bottom: 12px;",
                          "Variance expliquée, corrélations canoniques, eta², accuracy, Kappa de Cohen, matrice de confusion, taux par groupe"),
                        fluidRow(
                          column(12, style = "text-align: center;",
                                 downloadButton("downloadAfdMetricsXlsx",
                                                HTML(paste0(as.character(icon("file-excel")), " <strong>Métriques AFD (Excel)</strong>")),
                                                class = "btn-success",
                                                style = "margin: 4px; padding: 7px 16px;"),
                                 downloadButton("downloadAfdMetricsCsv",
                                                HTML(paste0(as.character(icon("file-csv")), " <strong>Métriques AFD (CSV)</strong>")),
                                                class = "btn-warning",
                                                style = "margin: 4px; padding: 7px 16px;")
                          )
                        )
                    )
                )
              ),

              # ---- Analyses multivariees etendues ----
  div(
    style = "margin-top:18px;",

    # -- Selecteur de categorie : une seule categorie visible a la fois --
    div(style = "background:#3c8dbc; border-radius:4px; padding:16px 20px; margin-bottom:18px;",
      h4(style = "color:white; margin:0 0 8px 0; font-weight:600;",
         icon("layer-group"), " Analyses multivariées complémentaires"),
      p(style = "color:#dbeaf5; font-size:12px; margin:0 0 14px 0;",
        "Choisissez une catégorie d'analyse. Seules les analyses de la catégorie sélectionnée sont affichées."),
      radioGroupButtons(
        inputId = "mv_category",
        label = NULL,
        choices = c(
          "Quantitatives" = "quanti",
          "Qualitatives / categorielles" = "quali",
          "Mixtes (quanti + quali)" = "mixte"
        ),
        selected = "quanti",
        justified = TRUE,
        size = "normal",
        status = "primary",
        checkIcon = list(yes = icon("check"))
      )
    ),

    # =================== CATEGORIE QUANTITATIVES ===================
    conditionalPanel(
      condition = "input.mv_category == 'quanti'",
      .mv_category_header("Analyses multivariees QUANTITATIVES",
                          "ruler-combined", "#3c8dbc"),
      fluidRow(.mv_analysis_box(
        "kmeans", "Classification k-means (partitionnement)", "quanti",
        principes  = "Partitionne n individus en k groupes en minimisant iterativement l'inertie intra-classe (somme des carres aux centroides). Algorithme de Lloyd/Hartigan-Wong.",
        objectifs  = "Construire une typologie d'individus, segmenter une population, identifier des profils homogenes sur variables quantitatives.",
        taille     = c("Minimum : n &ge; 2&times;k", "Recommande : n &ge; 10&times;k",
                       "Ideal : n &ge; 30&times;k pour des centroides stables"),
        variables  = c("Minimum : p &ge; 2 variables numeriques", "Recommande : p &ge; 3",
                       "Standardisation conseillee si echelles heterogenes"),
        intro = "Partitionnement non hierarchique : le nombre de clusters est fixe a priori."
      )),
      fluidRow(.mv_analysis_box(
        "efa", "Analyse Factorielle Exploratoire (AFE)", "quanti",
        principes  = "Modele a facteurs communs separant variance commune et variance specifique. Extraction (ML, axes principaux) puis rotation (varimax/oblimin) pour simplifier la structure.",
        objectifs  = "Decouvrir les facteurs latents sous-jacents a un ensemble de variables, valider la structure d'un questionnaire, reduire la dimension.",
        taille     = c("Minimum : n &ge; 5&times;p", "Recommande : n &ge; 100",
                       "Ideal : n &ge; 200 et &ge; 10 individus / variable"),
        variables  = c("Minimum : p &ge; 3 variables numeriques", "KMO &ge; 0,60 requis",
                       "Test de Bartlett significatif (p &lt; 0,05)"),
        intro = "Cherche une structure latente sans hypothese imposee (exploratoire)."
      )),
      fluidRow(.mv_analysis_box(
        "cfa", "Analyse Factorielle Confirmatoire (AFC-c)", "quanti",
        principes  = "Modele d'equations structurelles : la structure facteurs <-> items est imposee a priori, puis estimee et evaluee par des indices d'ajustement.",
        objectifs  = "Tester un modele de mesure theorique, confirmer la validite convergente et discriminante d'un instrument.",
        taille     = c("Minimum : n &ge; 100", "Recommande : n &ge; 200",
                       "Ideal : &ge; 10 individus par parametre estime"),
        variables  = c("Modele specifie en syntaxe lavaan", "&ge; 3 indicateurs par facteur conseille",
                       "Variables numeriques (estimateurs robustes sinon)"),
        intro = "Confirme un modele de mesure pre-specifie. Renseignez la syntaxe du modele."
      )),
      fluidRow(.mv_analysis_box(
        "pls", "Regression PLS / PLS-DA", "quanti",
        principes  = "Construit des composantes latentes maximisant la covariance entre les predicteurs X et la reponse Y. Adaptee aux cas p &gt;&gt; n et forte multicolinearite.",
        objectifs  = "Predire une reponse (quantitative = PLS, categorielle = PLS-DA) en grande dimension, identifier les variables influentes (VIP).",
        taille     = c("Fonctionne meme si n &lt; p", "Recommande : n &ge; 20",
                       "Validation croisee conseillee"),
        variables  = c("1 variable reponse Y", "p &ge; 2 predicteurs numeriques X",
                       "Predicteurs correles : aucun probleme"),
        intro = "Regression sur composantes latentes, robuste a la colinearite."
      )),
      fluidRow(.mv_analysis_box(
        "regmult", "Regression lineaire multiple", "quanti",
        principes  = "Estime par moindres carres ordinaires une reponse quantitative comme combinaison lineaire de plusieurs predicteurs.",
        objectifs  = "Expliquer et predire une variable continue, quantifier l'effet de chaque predicteur, controler des facteurs de confusion.",
        taille     = c("Minimum : n &ge; 10&times;p", "Recommande : n &ge; 15&times;p",
                       "Ideal : n &ge; 20&times;p"),
        variables  = c("1 reponse Y numerique", "p &ge; 1 predicteur (numerique ou facteur)",
                       "Residus : normalite, homoscedasticite, independance"),
        intro = "Modele explicatif/predictif de reference pour une reponse continue."
      ))
    ),

    # =================== CATEGORIE QUALITATIVES ===================
    conditionalPanel(
      condition = "input.mv_category == 'quali'",
      .mv_category_header("Analyses multivariees QUALITATIVES / CATEGORIELLES",
                          "shapes", "#605ca8"),
      fluidRow(.mv_analysis_box(
        "afc", "Analyse Factorielle des Correspondances (AFC)", "quali",
        principes  = "Decompose l'inertie du khi-deux d'une table de contingence ; compare les profils-lignes et profils-colonnes via la distance du khi-deux.",
        objectifs  = "Analyser et visualiser l'association entre DEUX variables qualitatives, reperer les modalites attractives ou repulsives.",
        taille     = c("Effectifs theoriques &ge; 5 par case conseille",
                       "Recommande : n &ge; 50", "Eviter cases vides"),
        variables  = c("Exactement 2 variables qualitatives", "Variable-ligne + variable-colonne",
                       "Modalites a effectif suffisant"),
        intro = "Association entre deux variables categorielles (table croisee)."
      )),
      fluidRow(.mv_analysis_box(
        "mca", "Analyse des Correspondances Multiples (ACM)", "quali",
        principes  = "Generalise l'AFC a plus de deux variables qualitatives via le tableau disjonctif complet (ou tableau de Burt).",
        objectifs  = "Explorer la structure d'associations entre plusieurs variables qualitatives, positionner individus et modalites.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Regrouper les modalites rares (&lt; 5 %)"),
        variables  = c("Minimum : p &ge; 2 variables qualitatives", "Recommande : p &ge; 3",
                       "Variables nominales"),
        intro = "Structure d'associations entre plusieurs variables categorielles."
      )),
      fluidRow(.mv_analysis_box(
        "kmodes", "Classification k-modes (partitionnement)", "quali",
        principes  = "Equivalent du k-means pour donnees qualitatives : dissimilarite d'appariement simple (Hamming), les centres sont des modes.",
        objectifs  = "Segmenter une population decrite par des variables categorielles, construire une typologie qualitative.",
        taille     = c("Minimum : n &ge; 2&times;k", "Recommande : n &ge; 10&times;k",
                       "Ideal : n &ge; 30&times;k"),
        variables  = c("Minimum : p &ge; 2 variables qualitatives", "Recommande : p &ge; 3",
                       "Modalites a effectif suffisant"),
        intro = "Partitionnement non hierarchique pour variables categorielles."
      )),
      fluidRow(.mv_analysis_box(
        "lca", "Analyse en Classes Latentes (LCA)", "quali",
        principes  = "Modele de melange probabiliste : sous hypothese d'independance locale conditionnelle, estime des classes latentes par maximum de vraisemblance (EM).",
        objectifs  = "Identifier des sous-populations non observees a partir de variables categorielles, clustering base sur un modele.",
        taille     = c("Minimum : n &ge; 100", "Recommande : n &ge; 300",
                       "Plus de classes => plus d'effectif"),
        variables  = c("Minimum : p &ge; 3 variables qualitatives", "Variables categorielles",
                       "Independance locale conditionnelle"),
        intro = "Clustering probabiliste : classes latentes derriere des reponses categorielles."
      )),
      fluidRow(.mv_analysis_box(
        "logit", "Regression logistique / multinomiale", "quali",
        principes  = "Modele lineaire generalise a lien logit estime par maximum de vraisemblance ; produit des rapports de cotes (odds ratios).",
        objectifs  = "Predire une reponse categorielle (binaire ou multinomiale), quantifier l'effet des predicteurs.",
        taille     = c("Regle : &ge; 10 evenements par predicteur",
                       "Recommande : n &ge; 100", "Eviter la separation parfaite"),
        variables  = c("1 reponse Y categorielle", "p &ge; 1 predicteur (numerique ou facteur)",
                       "Independance des observations"),
        intro = "Modele explicatif/predictif pour une reponse categorielle."
      ))
    ),

    # =================== CATEGORIE MIXTES ===================
    conditionalPanel(
      condition = "input.mv_category == 'mixte'",
      .mv_category_header("Analyses multivariees MIXTES (quanti + quali)",
                          "layer-group", "#00a65a"),
      fluidRow(.mv_analysis_box(
        "famd", "Analyse Factorielle de Donnees Mixtes (AFDM)", "mixte",
        principes  = "Combine ACP (variables quantitatives standardisees) et ACM (variables qualitatives), avec une ponderation equilibrant les deux types.",
        objectifs  = "Reduire la dimension d'un tableau melant variables quantitatives et qualitatives, visualiser individus et modalites.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Ideal : n &ge; 5&times;p"),
        variables  = c("Au moins 1 variable quantitative", "Au moins 1 variable qualitative",
                       "p &ge; 3 au total conseille"),
        intro = "Reduction de dimension pour un tableau de variables mixtes."
      )),
      fluidRow(.mv_analysis_box(
        "mfa", "Analyse Factorielle Multiple (AFM)", "mixte",
        principes  = "Analyse des donnees structurees en groupes de variables ; chaque groupe est equilibre par sa premiere valeur propre afin qu'aucun ne domine.",
        objectifs  = "Comparer et integrer plusieurs groupes de variables (bloc quantitatif et bloc qualitatif), etudier leur coherence.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Ideal : n &ge; 5&times;p"),
        variables  = c("Bloc quantitatif : &ge; 1 variable", "Bloc qualitatif : &ge; 1 variable",
                       "Definir explicitement les deux blocs"),
        intro = "Integration de blocs de variables (bloc quanti + bloc quali)."
      )),
      fluidRow(.mv_analysis_box(
        "kproto", "Classification k-prototypes (partitionnement mixte)", "mixte",
        principes  = "Combine k-means (distance euclidienne sur le quantitatif) et k-modes (appariement sur le qualitatif), ponderes par un parametre gamma.",
        objectifs  = "Segmenter une population decrite par des variables a la fois quantitatives et qualitatives.",
        taille     = c("Minimum : n &ge; 2&times;k", "Recommande : n &ge; 10&times;k",
                       "Ideal : n &ge; 30&times;k"),
        variables  = c("Au moins 1 variable quantitative", "Au moins 1 variable qualitative",
                       "Standardisation du quantitatif appliquee"),
        intro = "Partitionnement non hierarchique pour donnees mixtes."
      ))
    )
  )
      ),
      # ---- Seuils d'efficacité ----
      tabItem(tabName = "threshold",
              mod_threshold_ui("threshold")
      )
      
    )
  )
)