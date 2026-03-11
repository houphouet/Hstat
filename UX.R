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

        /* Suppression des indicateurs visuels de focus */
        *:focus { outline: none !important; box-shadow: none !important; }
        .form-control:focus { border-color: #ccc !important; box-shadow: none !important; }
        .btn:focus, .btn:active:focus { outline: none !important; box-shadow: none !important; }
        .selectize-input.focus { box-shadow: none !important; border-color: #ccc !important; }
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
                              accept = c(".csv", ".xlsx", ".xls", ".txt", ".sav", ".dta", ".rds")),
                    uiOutput("sheetUI"),
                    radioButtons("sep", "Séparateur (CSV/TXT)",
                                 choices = c(Virgule = ",", `Point-virgule` = ";", Tab = "\t"), selected = ","),
                    checkboxInput("header", "Avec en-têtes", TRUE),
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
                                   sliderInput("distTitleSize", "Taille titre:", 
                                               min = 8, max = 24, value = 14, ticks = FALSE),
                                   sliderInput("distAxisTitleSize", "Taille titres axes:", 
                                               min = 8, max = 20, value = 12, ticks = FALSE),
                                   sliderInput("distAxisTextSize", "Taille texte axes:", 
                                               min = 6, max = 16, value = 10, ticks = FALSE),
                                   sliderInput("distLegendTextSize", "Taille texte légende:",
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
                    column(9,
                           div(
                             style = "max-height: 400px; overflow-y: auto; background-color: #fafafa; padding: 15px; border-radius: 5px;",
                             uiOutput("varTypeUI")
                           )
                    ),
                    column(3,
                           div(
                             style = "text-align: center; padding: 20px;",
                             actionButton(
                               "applyTypes",
                               tagList(icon("check-circle"), " Appliquer les Types"),
                               class = "btn-warning btn-lg btn-block",
                               style = "font-size: 16px; padding: 12px 20px;"
                             )
                           )
                    )
                  )
                )
              ),
              
              # Étape 2: Gestion des variables
              fluidRow(
                # Gestion Variables + Lignes 
                box(
                  title = tagList(
                    tags$span(
                      class = "badge bg-red",
                      style = "font-size: 14px; margin-right: 10px;",
                      "2"
                    ),
                    icon("edit"),
                    "Gestion des Variables et Lignes"
                  ),
                  status = "danger",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  tabsetPanel(
                    type = "tabs",
                    
                    # Onglet 1 : Supprimer variable
                    tabPanel(
                      title = tagList(icon("columns"), " Supprimer Variable"),
                      br(),
                      div(
                        style = "background-color: #ffebee; padding: 15px; border-radius: 5px;",
                        h5(icon("trash-alt"), "Supprimer une Variable", style = "color: #e74c3c; margin-top: 0;"),
                        uiOutput("removeVarUI"),
                        actionButton(
                          "removeVar",
                          tagList(icon("trash"), " Supprimer"),
                          class = "btn-danger btn-block",
                          style = "margin-top: 10px;"
                        )
                      )
                    ),
                    
                    # Onglet 2 : Supprimer lignes
                    tabPanel(
                      title = tagList(icon("trash-alt"), " Supprimer Lignes"),
                      br(),
                      tabsetPanel(
                        type = "pills",
                        
                        tabPanel(
                          title = tagList(icon("keyboard"), " Saisie"),
                          br(),
                          div(
                            style = "background-color: #ffebee; padding: 12px; border-radius: 5px;",
                            tags$p(style = "font-size: 11px; color: #7f8c8d; margin-bottom: 8px;",
                              "Formats : ", tags$code("1,3,5"), " — ", tags$code("10 à 20"), " — ", tags$code("1,3,10 à 15")),
                            textAreaInput("deleteRowsInput", NULL,
                              placeholder = "1,3,5
10 à 20
1,3,5,10 à 15",
                              rows = 3, width = "100%"),
                            uiOutput("deleteRowsPreview"),
                            actionButton("applyDeleteRows",
                              tagList(icon("trash"), " Supprimer"),
                              class = "btn-danger btn-block", style = "margin-top: 8px; font-weight: bold;")
                          )
                        ),
                        
                        tabPanel(
                          title = tagList(icon("mouse-pointer"), " Interactif"),
                          br(),
                          tags$p(style = "font-size: 11px; color: #555; margin-bottom: 8px;",
                            icon("hand-pointer"), " Cliquez sur les lignes (Ctrl = multiple)"),
                          div(
                            style = "border: 1px solid #dee2e6; border-radius: 5px; overflow: hidden;",
                            DT::dataTableOutput("deleteRowsTable", height = "260px")
                          ),
                          br(),
                          uiOutput("deleteRowsInteractivePreview"),
                          br(),
                          actionButton("applyDeleteRowsInteractive",
                            tagList(icon("trash"), " Supprimer la sélection"),
                            class = "btn-danger btn-block", style = "font-weight: bold;")
                        )
                      )
                    ),
                    
                    # Onglet 3 : Ajouter variable constante
                    tabPanel(
                      title = tagList(icon("plus-circle"), " Ajouter Variable"),
                      br(),
                      div(
                        style = "background-color: #e8f5e9; padding: 15px; border-radius: 5px;",
                        h5(icon("plus-circle"), "Ajouter une Variable Constante", style = "color: #27ae60; margin-top: 0;"),
                        tags$p(style = "font-size: 12px; color: #7f8c8d;",
                          "Crée une nouvelle colonne avec une valeur identique pour toutes les lignes"),
                        textInput("newVarName", "Nom:", placeholder = "ex: Categorie"),
                        numericInput("newVarValue", "Valeur par défaut:", 0),
                        actionButton("addVar",
                          tagList(icon("plus"), " Ajouter"),
                          class = "btn-success btn-block")
                      )
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
                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                    
                    h5(icon("info-circle"), "Assistant de Formule", style = "color: #3498db; margin-top: 0;"),
                    p(style = "font-size: 12px; color: #7f8c8d;", 
                      "Créez des variables basées sur des calculs. Exemple: (Var1 + Var2) / 2"),
                    
                    textInput(
                      "calcVarName", 
                      "Nom de la variable calculée:",
                      placeholder = "ex: Moyenne_Score"
                    ),
                    
                    # ── Sélection de colonnes et lignes
                    fluidRow(
                      column(6,
                        div(
                          style = "background-color: #eef7ff; padding: 10px; border-radius: 5px;",
                          h6(icon("columns"), " Colonnes", style = "margin-bottom: 6px; color: #2c5aa0;"),
                          uiOutput("colPicker"),
                          tags$small(style = "color: #6c757d;",
                            icon("info-circle"), " Cliquez pour insérer dans la formule")
                        )
                      ),
                      column(6,
                        div(
                          style = "background-color: #f0fff4; padding: 10px; border-radius: 5px;",
                          h6(icon("filter"), " Filtrer sur lignes (optionnel)", style = "margin-bottom: 6px; color: #1a6e2e;"),
                          uiOutput("rowCondPicker"),
                          tags$small(style = "color: #6c757d;",
                            icon("info-circle"), " Génère ifelse() dans la formule")
                        )
                      )
                    ),
                    
                    br(),
                    
                    # ── Opérateurs et fonctions
                    fluidRow(
                      column(12,
                        div(
                          style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                          fluidRow(
                            column(6,
                              h6("Opérateurs :", style = "margin-bottom: 5px; font-size: 11px; color: #555;"),
                              div(style = "display: flex; flex-wrap: wrap; gap: 4px;",
                                actionButton("insertPlus",  "+",  class = "btn-outline-secondary btn-sm"),
                                actionButton("insertMoins", "-",  class = "btn-outline-secondary btn-sm"),
                                actionButton("insertMult",  "×",  class = "btn-outline-secondary btn-sm"),
                                actionButton("insertDiv",   "÷",  class = "btn-outline-secondary btn-sm"),
                                actionButton("insertPow",   "^",  class = "btn-outline-secondary btn-sm"),
                                actionButton("insertParen", "()", class = "btn-outline-secondary btn-sm")
                              )
                            ),
                            column(6,
                              h6("Fonctions :", style = "margin-bottom: 5px; font-size: 11px; color: #555;"),
                              div(style = "display: flex; flex-wrap: wrap; gap: 4px;",
                                actionButton("insertLog",   "log()",    class = "btn-outline-info btn-sm"),
                                actionButton("insertLog10", "log10()",  class = "btn-outline-info btn-sm"),
                                actionButton("insertSqrt",  "sqrt()",   class = "btn-outline-info btn-sm"),
                                actionButton("insertAbs",   "abs()",    class = "btn-outline-info btn-sm"),
                                actionButton("insertRound", "round()",  class = "btn-outline-info btn-sm"),
                                actionButton("insertExp",   "exp()",    class = "btn-outline-info btn-sm"),
                                actionButton("insertMean",  "mean()",   class = "btn-outline-info btn-sm"),
                                actionButton("insertSum",   "sum()",    class = "btn-outline-info btn-sm"),
                                actionButton("insertIfelse","ifelse()", class = "btn-outline-warning btn-sm"),
                                actionButton("insertIsNA",  "is.na()",  class = "btn-outline-warning btn-sm")
                              )
                            )
                          )
                        )
                      )
                    ),
                    
                    textInput(
                      "calcFormula", 
                      "Formule de calcul:",
                      placeholder = "ex: (Rendement + Biomasse) / 2   |   sum(Poids, Hauteur)   |   sqrt(Var1 * Var2)"
                    ),
                    
                    # ── Exemples d'application des fonctions
                    tags$div(
                      style = "margin-top:4px; margin-bottom:12px; padding:10px 12px; background:#f0f7ff; border:1px solid #bee3f8; border-radius:6px; font-size:11.5px;",
                      tags$b(style="color:#1a56db; font-size:12px;", icon("calculator"), " Exemples d'utilisation :"),
                      tags$table(
                        style = "width:100%; margin-top:6px; border-collapse:collapse;",
                        # ── En-tête opérations multi-variables ──
                        tags$tr(style="background:#dbeafe;",
                          tags$td(colspan="2",style="padding:4px 8px;color:#1e40af;font-size:11px;font-weight:bold;",
                            icon("layer-group")," Opérations sur plusieurs variables")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;color:#555;width:52%;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "(Rendement + Biomasse) / 2")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Moyenne de 2 variables")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "mean(c(Rendement, Biomasse, Poids))")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Moyenne (n variables)")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "Rendement + Biomasse + Poids")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Somme de variables")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "Rendement / (Biomasse + Poids)")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Ratio entre variables")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "sqrt(Rendement * Biomasse)")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Moyenne géométrique")
                        ),
                        # ── En-tête transformations ──
                        tags$tr(style="background:#dcfce7;",
                          tags$td(colspan="2",style="padding:4px 8px;color:#166534;font-size:11px;font-weight:bold;",
                            icon("calculator")," Transformations classiques")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "log(Rendement)")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Logarithme naturel")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "log10(Rendement + 1)")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Log10 (si zéros présents)")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "sqrt(Rendement)")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Racine carrée")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "Rendement^2")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Mise au carré")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "round(Rendement / Biomasse, 3)")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Ratio arrondi à 3 déc.")
                        ),
                        tags$tr(
                          tags$td(style="padding:3px 6px;",
                            tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                              "(Rendement - mean(Rendement)) / sd(Rendement)")),
                          tags$td(style="padding:3px 6px;color:#666;","→ Z-score")
                        )
                      )
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
                      "3"
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
              #  En-tête avec boutons d'action globaux 
              fluidRow(
                box(title = "Actions globales", status = "warning", width = 12, solidHeader = TRUE,
                    icon = icon("cog"),
                    fluidRow(
                      column(6,
                             actionButton("resetFilter", "Réinitialiser tous les filtres", 
                                          class = "btn-warning btn-lg btn-block", 
                                          icon = icon("redo"))
                      ),
                      column(6,
                             downloadButton("downloadFilteredData", "Télécharger les données filtrées", 
                                            class = "btn-success btn-lg btn-block")
                      )
                    )
                )
              ),
              
              #  Indicateurs de performance 
              fluidRow(
                valueBoxOutput("originalRows", width = 3),
                valueBoxOutput("filteredRows", width = 3),
                valueBoxOutput("removedRows", width = 3),
                valueBoxOutput("columnsCount", width = 3)
              ),
              
              #  Section 1: Filtres basiques (lignes et valeurs) 
              fluidRow(
                box(title = "Filtre par sélection de lignes", status = "primary", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE,
                    icon = icon("list-ol"),
                    uiOutput("rowRangeUI"),
                    hr(),
                    actionButton("applyRowRange", "Appliquer la sélection de lignes", 
                                 class = "btn-primary btn-block", icon = icon("filter"))
                ),
                
                box(title = "Filtre par valeur(s)", status = "primary", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE,
                    icon = icon("search"),
                    uiOutput("valueFilterUI"),
                    hr(),
                    actionButton("applyValueFilter", "Appliquer le filtre par valeur", 
                                 class = "btn-primary btn-block", icon = icon("filter"))
                )
              ),
              
              #  Section 2: Filtre par colonnes 
              fluidRow(
                box(title = "Sélection des colonnes", status = "info", width = 12, 
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    icon = icon("columns"),
                    fluidRow(
                      column(12,
                             uiOutput("columnSelectUI")
                      )
                    ),
                    hr(),
                    actionButton("applyColumnFilter", "Appliquer la sélection de colonnes", 
                                 class = "btn-info btn-block", icon = icon("check"))
                )
              ),
              
              #  Section 3: Filtres avancés  
              fluidRow(
                box(title = "Filtre croisement complet (2 facteurs)", status = "success", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    icon = icon("th"),
                    uiOutput("filterFactorA"),
                    uiOutput("filterFactorB"),
                    checkboxInput("requireA", "Garder niveaux de A présents pour tous les niveaux de B", TRUE),
                    checkboxInput("requireB", "Garder niveaux de B présents pour tous les niveaux de A", FALSE),
                    hr(),
                    actionButton("applyCrossFilter", "Appliquer (2 facteurs)", 
                                 class = "btn-success btn-block", icon = icon("filter")),
                    helpText("Filtre les données pour ne garder que les combinaisons complètes entre deux facteurs.")
                ),
                
                box(title = "Filtre croisement complet (N facteurs)", status = "success", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    icon = icon("project-diagram"),
                    uiOutput("filterFactorsN"),
                    helpText("Garde uniquement les niveaux qui forment un croisement complet entre tous les facteurs sélectionnés."),
                    hr(),
                    actionButton("applyCrossFilterN", "Appliquer (N facteurs)", 
                                 class = "btn-success btn-block", icon = icon("filter"))
                )
              ),
              
              #  Tableau des données filtrées 
              fluidRow(
                box(title = "Aperçu des données filtrées", status = "info", width = 12, 
                    solidHeader = TRUE, collapsible = TRUE,
                    icon = icon("table"),
                    DTOutput("filteredData"),
                    br(),
                    helpText("Ce tableau affiche les données après application des filtres.")
                )
              )
      ),
      # ---- Analyse descriptives ----
      tabItem(tabName = "descriptive",
              fluidRow(
                box(title = tags$span(icon("chart-bar"), " Sélection des Variables"), 
                    status = "success", width = 4, solidHeader = TRUE,
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
                    hr(style = "border-color: #ddd; margin: 15px 0;"),
                    # Option d'arrondi pour les analyses descriptives
                    div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; border-radius: 4px;",
                        fluidRow(
                          column(6,
                                 checkboxInput("descRoundResults", "Arrondir les résultats", value = FALSE)
                          ),
                          column(6,
                                 conditionalPanel(
                                   condition = "input.descRoundResults == true",
                                   numericInput("descDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                 )
                          )
                        )
                    ),
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
                      tags$p(tagList(icon("info-circle"), " Les résultats s'afficheront ici après le calcul. Vous pouvez trier, filtrer et rechercher dans le tableau."),
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
                             
                             # OPTIONS BOXPLOT 
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
                               style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #6c757d; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                               h6(icon("expand-arrows-alt"), " Dimensions du Graphique", 
                                  style = "color: #343a40; font-weight: bold; margin-bottom: 12px;"),
                               sliderInput("descPlotWidth", "Largeur (pixels):", 
                                           min = 400, max = 2000, value = 900, step = 50, width = "100%"),
                               sliderInput("descPlotHeight", "Hauteur (pixels):", 
                                           min = 400, max = 2000, value = 600, step = 50, width = "100%")
                             ),
                             
                             # TÉLÉCHARGEMENT DU GRAPHIQUE 
                             fluidRow(
                               column(6, selectInput("descPlot_format", "Format:",
                                                     choices = c("png", "svg", "pdf", "tiff"),
                                                     selected = "png")),
                               column(6, numericInput("descPlot_dpi", "DPI:",
                                                      value = 300, min = 72, max = 1200))
                             ),
                             downloadButton("downloadDescPlot", "Télécharger le Graphique",
                                            class = "btn-info btn-sm")
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
                                    style = "font-weight: bold; color: #495057; margin-bottom: 15px; font-size: 15px;"),
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
                          style = "font-weight: bold; color: #495057; margin-top: 5px;"
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
                                 style = "font-weight: bold; font-size: 16px; padding: 15px; margin-top: 10px; margin-bottom: 10px; background: linear-gradient(to right, #495057, #343a40); box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
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
                    
                    # Paramètres d'export - Rendu conditionnel
                    uiOutput("plotDownloadSection")
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
                    
                    # Paramètres d'export - Rendu conditionnel
                    uiOutput("pieDownloadSection")
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
                
                # PANNEAU DE CONTRÔLE 
                box(
                  title = tagList(icon("sliders-h"), " Paramètres de Visualisation"),
                  status = "primary",
                  width = 4,
                  solidHeader = TRUE,
                  
                  #  Variables de base
                  div(
                    class = "well",
                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(icon("database"), " Sélection des Variables", 
                       style = "color: #007bff; margin-top: 0; font-size: 16px;"),
                    
                    # Variable X avec actualisation auto
                    uiOutput("vizXVarSelect"),
                    
                    # Iddentiquer le type de variable X avec détection automatique
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
                  
                  #  Personnalisation des étiquettes de l'axe X 
                  conditionalPanel(
                    condition = "true",
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
                  
                  #  Ordre des catégories/valeurs X (tous types de données)
                  conditionalPanel(
                    condition = "input.vizType != 'histogram' && input.vizType != 'density' && input.vizType != 'pie' && input.vizType != 'donut' && input.vizType != 'treemap'",
                    div(
                      class = "well",
                      style = "background-color: #e8f5e9; border-left: 4px solid #28a745; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      h4(
                        icon("sort"), " Ordre des Catégories (Axe X)",
                        style = "color: #28a745; font-weight: bold; margin-top: 0; font-size: 15px;"
                      ),
                      p("Glissez-déposez pour réorganiser l'ordre d'affichage sur l'axe X.",
                        style = "font-size: 13px; color: #666; margin-bottom: 10px;"),
                      uiOutput("xOrderEditor"),
                      
                    )
                  ),
                  
                  #  Formatage des dates
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
                        tagList(icon("file-import"), " Format des données source :"),
                        choices = c(
                          "AAAA-MM-JJ (ISO 8601)" = "%Y-%m-%d",
                          "JJ/MM/AAAA (France)"   = "%d/%m/%Y",
                          "MM/JJ/AAAA (US)"       = "%m/%d/%Y",
                          "AAAA/MM/JJ"            = "%Y/%m/%d",
                          "JJ-Mois-AAAA"          = "%d-%b-%Y",
                          "Mois JJ, AAAA"         = "%B %d, %Y"
                        ),
                        selected = "%Y-%m-%d"
                      ),
                      helpText(
                        icon("exclamation-triangle", style = "color: #ffc107;"),
                        "Format des dates dans vos données brutes (pour la conversion)."
                      ),
                      
                      hr(style = "margin: 10px 0;"),
                      
                      selectInput(
                        "xDateDisplayFormat",
                        tagList(icon("eye"), " Format d'affichage sur l'axe :"),
                        choices = c(
                          "── Formats chiffres ──────────────────" = "",
                          "JJ-MM-AAAA (ex: 25-03-2024)"  = "%d-%m-%Y",
                          "MM-JJ-AAAA (ex: 03-25-2024)"  = "%m-%d-%Y",
                          "AAAA-MM-JJ (ex: 2024-03-25)"  = "%Y-%m-%d",
                          "AAAA-JJ-MM (ex: 2024-25-03)"  = "%Y-%d-%m",
                          "JJ/MM/AAAA (ex: 25/03/2024)"  = "%d/%m/%Y",
                          "MM/JJ/AAAA (ex: 03/25/2024)"  = "%m/%d/%Y",
                          "JJ-MM (ex: 25-03)"            = "%d-%m",
                          "MM-JJ (ex: 03-25)"            = "%m-%d",
                          "MM-AAAA (ex: 03-2024)"        = "%m-%Y",
                          "AAAA-MM (ex: 2024-03)"        = "%Y-%m",
                          "── Mois abrégé ───────────────────────" = "",
                          "JJ-Mois-AAAA (ex: 25-Mar-2024)"        = "%d-%b-%Y",
                          "Mois-AAAA (ex: Mar-2024)"              = "%b-%Y",
                          "JJ-Mois (ex: 25-Mar)"                  = "%d-%b",
                          "Mois-JJ (ex: Mar-25)"                  = "%b-%d",
                          "AAAA-Mois-JJ (ex: 2024-Mar-25)"        = "%Y-%b-%d",
                          "── Mois entier ───────────────────────" = "",
                          "JJ Mois AAAA (ex: 25 Mars 2024)"       = "%d %B %Y",
                          "Mois AAAA (ex: Mars 2024)"             = "%B %Y",
                          "JJ Mois (ex: 25 Mars)"                 = "%d %B",
                          "Mois JJ (ex: Mars 25)"                 = "%B %d",
                          "AAAA Mois (ex: 2024 Mars)"             = "%Y %B"
                        ),
                        selected = "%d-%m-%Y"
                      ),
                      helpText(
                        icon("info-circle", style = "color: #2196F3;"),
                        "Format d'affichage des dates sur l'axe X du graphique.",
                        tags$br(),
                        tags$small(
                          style = "color: #888;",
                          "Mois abrégé = Jan, Fév, Mar... | Mois entier = Janvier, Février..."
                        )
                      )
                    )
                  ),
                  
                  #  Selection multiples des Variable(s) Y avec support multi-sélection
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
                    ),
                  ),
                  
                  #  Variables supplémentaires (Couleur et Facette)
                  div(
                    class = "well",
                    style = "background-color: #fce4ec; border-left: 4px solid #e91e63; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(
                      icon("palette"), " Variables Supplémentaires",
                      style = "color: #c2185b; font-weight: bold; margin-top: 0; font-size: 15px;"
                    ),
                    
                    # Variable de couleur
                    uiOutput("vizColorVarSelect"),
                    
                    # Couleur fixe de la courbe (quand pas de variable couleur)
                    conditionalPanel(
                      condition = "input.vizColorVar == 'Aucun' || input.vizColorVar == null",
                      div(
                        style = "margin-top:8px;padding:10px;background:#f3e5f5;border-radius:6px;border-left:3px solid #9c27b0;",
                        h6(tagList(icon("paint-brush")," Couleur de la courbe / points"),
                           style="color:#6a1b9a;font-weight:bold;margin-top:0;margin-bottom:6px;font-size:12px;"),
                        colourInput("lineFixedColor", NULL, value="#2196F3", showColour="background"),
                        helpText(style="font-size:11px;color:#666;margin-top:4px;",
                                 icon("info-circle"),
                                 " Couleur appliquée à la courbe, aux points et au lissage.")
                      )
                    ),
                    
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
                  
                  #  Type de visualisation
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
                  
                  #  Options d'agrégation
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
                      # Variables de regroupement
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
                  
                  #  Bouton de mise à jour 
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton(
                      "refreshPlot",
                      tagList(icon("sync-alt"), " Actualiser le Graphique"),
                      class = "btn-success btn-lg",
                      style = "width: 100%; font-weight: bold;"
                    )
                  )
                ),
                
                # PANNEAU D'AFFICHAGE DU GRAPHIQUE
                box(
                  title = tagList(icon("chart-area"), " Graphique "),
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
                )
              ),
              
              # SECTION EXPORT DU GRAPHIQUE
              fluidRow(
                box(
                  title = tagList(icon("download"), " Exporter le graphique"),
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  
                  # Ligne 1: Format et DPI
                  fluidRow(
                    column(4,
                           selectInput("exportFormat", 
                                       label = tagList(icon("file-image"), " Format:"),
                                       choices = c(
                                         "PNG" = "png",
                                         "JPEG" = "jpeg",
                                         "TIFF" = "tiff",
                                         "BMP" = "bmp",
                                         "PDF" = "pdf",
                                         "SVG" = "svg",
                                         "EPS" = "eps"
                                       ),
                                       selected = "png")
                    ),
                    column(4,
                           numericInput("exportDPI", 
                                        label = tagList(icon("sliders-h"), " DPI:"),
                                        value = 300, min = 300, max = 20000, step = 50)
                    ),
                    column(4,
                           tags$label(tagList(icon("ruler-combined"), " Dimensions:")),
                           uiOutput("calculatedDimensions")
                    )
                  ),
                  
                  # Ligne 2: Options JPEG/TIFF
                  conditionalPanel(
                    condition = "input.exportFormat == 'jpeg'",
                    fluidRow(
                      column(6,
                             sliderInput("jpegQuality", "Qualité JPEG:", 
                                         min = 50, max = 100, value = 95, step = 5)
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.exportFormat == 'tiff'",
                    fluidRow(
                      column(6,
                             selectInput("tiffCompression", "Compression TIFF:",
                                         choices = c("Aucune" = "none", "LZW" = "lzw", "ZIP" = "zip"),
                                         selected = "lzw")
                      )
                    )
                  ),
                  
                  # Bouton de téléchargement
                  fluidRow(
                    column(12,
                           div(style = "margin-top: 15px; text-align: center;",
                               actionButton("downloadPlotBtn", 
                                            label = tagList(icon("download"), " Télécharger le graphique"),
                                            class = "btn-success btn-lg",
                                            style = "padding: 15px 50px; font-size: 16px;")
                           )
                    )
                  )
                )
              ),
              
              # PANNEAU D'OPTIONS AVANCÉES
              fluidRow(
                box(
                  title = tagList(icon("cog"), " Options Avancées de Personnalisation"),
                  status = "warning",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  fluidRow(
                    
                    # COLONNE 1 : TEXTES, LABELS ET LEGENDE 
                    column(
                      width = 4,
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px;",
                        
                        h4(icon("font"), " Textes, Labels et Légende",
                           style = "color: #9c27b0; font-weight: bold; border-bottom: 3px solid #9c27b0; padding-bottom: 10px; margin-top: 0;"),
                        
                        # Titre principal
                        div(
                          style = "margin-bottom: 18px; padding: 12px; background-color: #f3e5f5; border-radius: 6px;",
                          h5(icon("heading"), " Titre",
                             style = "color: #6a1b9a; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          textInput(
                            "plotTitle",
                            "Titre principal:",
                            value = "",
                            placeholder = "Titre du graphique..."
                          )
                        ),
                        
                        # Tailles de police globales
                        div(
                          style = "margin-bottom: 18px; padding: 12px; background-color: #ede7f6; border-radius: 6px;",
                          h5(icon("text-width"), " Tailles de police",
                             style = "color: #4a148c; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          sliderInput(
                            "baseFontSize",
                            "Police de base:",
                            min = 8, max = 20, value = 12, step = 1
                          ),
                          sliderInput(
                            "titleSize",
                            "Titre du graphique:",
                            min = 10, max = 24, value = 14, step = 1
                          ),
                          sliderInput(
                            "axisLabelSize",
                            "Labels des axes:",
                            min = 8, max = 18, value = 11, step = 1
                          )
                        ),
                        
                        # Labels des axes
                        div(
                          style = "margin-bottom: 18px; padding: 12px; background-color: #e8eaf6; border-radius: 6px;",
                          h5(icon("arrows-alt-h"), " Labels des axes",
                             style = "color: #283593; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
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
                          style = "padding: 12px; background-color: #fce4ec; border-radius: 6px;",
                          h5(icon("list"), " Légende",
                             style = "color: #880e4f; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          textInput(
                            "legendTitle",
                            "Titre de la légende:",
                            value = "",
                            placeholder = "Auto"
                          ),
                          sliderInput(
                            "legendTitleSize",
                            "Taille du titre:",
                            min = 8, max = 22, value = 12, step = 1
                          ),
                          sliderInput(
                            "legendTextSize",
                            "Taille des entrées:",
                            min = 6, max = 20, value = 10, step = 1
                          ),
                          sliderInput(
                            "legendKeySize",
                            "Taille des symboles:",
                            min = 0.3, max = 3, value = 1, step = 0.1
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
                          ),
                          div(
                            style = "margin-top: 8px;",
                            # Bouton toujours visible (fonctionne pour color var ET multi-Y)
                            actionButton(
                              "customizeLegendLabels",
                              tagList(icon("tags"), " Modifier les labels de légende"),
                              class = "btn-sm btn-warning btn-block",
                              style = "font-weight:600; letter-spacing:0.3px;",
                              title = "Ouvre un éditeur pour renommer chaque niveau de la légende"
                            ),
                            div(
                              style = "margin-top:5px; padding:5px 8px; background:#fff8e1; border-radius:4px; font-size:11px; color:#7f6000; border-left:3px solid #ff9800;",
                              icon("lightbulb", style="color:#ff9800;"),
                              " Cliquez pour renommer A→Groupe 1, Ctrl→Traitement...",
                              br(),
                              tags$small(style="color:#aaa;", "Fonctionne pour variable couleur et Y multiples.")
                            ),
                            uiOutput("legendLabelsStatus")
                          )
                        )
                      )
                    ),
                    
                    #  COLONNE 2 : AXES - FORMATAGE ET ECHELLE 
                    column(
                      width = 4,
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px;",
                        
                        h4(icon("ruler-combined"), " Axes : Formatage et Échelle",
                           style = "color: #ff5722; font-weight: bold; border-bottom: 3px solid #ff5722; padding-bottom: 10px; margin-top: 0;"),
                        
                        # Style label axe X
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #e3f2fd; border-radius: 6px;",
                          h5(icon("long-arrow-alt-right"), " Style label axe X",
                             style = "color: #495057; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput("xAxisBold",   tagList(icon("bold"),   " Gras"),    value = FALSE),
                            checkboxInput("xAxisItalic", tagList(icon("italic"), " Italique"), value = FALSE)
                          )
                        ),
                        
                        # Style label axe Y
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #f3e5f5; border-radius: 6px;",
                          h5(icon("long-arrow-alt-up"), " Style label axe Y",
                             style = "color: #7b1fa2; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput("yAxisBold",   tagList(icon("bold"),   " Gras"),    value = FALSE),
                            checkboxInput("yAxisItalic", tagList(icon("italic"), " Italique"), value = FALSE)
                          )
                        ),
                        
                        # Niveaux axe X
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #fff3e0; border-radius: 6px;",
                          h5(icon("tag"), " Niveaux axe X",
                             style = "color: #f57c00; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput("xTickBold",   tagList(icon("bold"),   " Gras"),    value = FALSE),
                            checkboxInput("xTickItalic", tagList(icon("italic"), " Italique"), value = FALSE)
                          ),
                          sliderInput("xTickSize",  "Taille:",           min = 6, max = 20, value = 10, step = 1),
                          sliderInput("xAxisAngle", "Angle (°):", min = 0, max = 90, value = 0, step = 15)
                        ),
                        
                        # Niveaux axe Y
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #ede7f6; border-radius: 6px;",
                          h5(icon("tag"), " Niveaux axe Y",
                             style = "color: #6a1b9a; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput("yTickBold",   tagList(icon("bold"),   " Gras"),    value = FALSE),
                            checkboxInput("yTickItalic", tagList(icon("italic"), " Italique"), value = FALSE)
                          ),
                          sliderInput("yTickSize", "Taille:", min = 6, max = 20, value = 10, step = 1)
                        ),
                        
                        # Graduations des axes
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #e8f5e9; border-radius: 6px;",
                          h5(icon("ruler"), " Graduations des axes",
                             style = "color: #2e7d32; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          checkboxInput(
                            "customAxisBreaks",
                            tagList(icon("sliders-h"), " Personnaliser les graduations"),
                            value = FALSE
                          ),
                          conditionalPanel(
                            condition = "input.customAxisBreaks == true",
                            div(
                              style = "margin-top: 8px;",
                              numericInput(
                                "yAxisBreakStep",
                                tagList(icon("long-arrow-alt-up"), " Pas axe Y:"),
                                value = NA, min = 0.001, step = 1
                              ),
                              helpText(icon("info-circle"), "Ex: 10 -> graduations 0, 10, 20...",
                                       style = "font-size: 11px; color: #555;"),
                              numericInput(
                                "xAxisBreakStep",
                                tagList(icon("long-arrow-alt-right"), " Pas axe X (numérique):"),
                                value = NA, min = 0.001, step = 1
                              ),
                              helpText(icon("info-circle"), "Uniquement si l'axe X est numérique.",
                                       style = "font-size: 11px; color: #555;")
                            )
                          )
                        ),
                        
                        # Traits des axes (couleur + épaisseur)
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #fce4ec; border-radius: 6px;",
                          h5(icon("minus"), " Traits des axes",
                             style = "color: #c62828; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          sliderInput(
                            "axisLineSize",
                            tagList(icon("ruler-horizontal"), " Epaisseur des axes:"),
                            min = 0, max = 3, value = 0.8, step = 0.1
                          ),
                          helpText(icon("info-circle"), "0 = axes invisibles. Les traits sont toujours noirs.",
                                   style = "font-size: 11px; color: #555;")
                        ),
                        
                        # Limites des axes (min/max)
                        div(
                          style = "padding: 12px; background-color: #e0f2f1; border-radius: 6px;",
                          h5(icon("compress-arrows-alt"), " Limites des axes (min / max)",
                             style = "color: #00695c; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          helpText(icon("info-circle"), "Laissez vide pour les limites automatiques.",
                                   style = "font-size: 11px; color: #555; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 8px;",
                            numericInput("yAxisMin", tagList(icon("long-arrow-alt-up"),    " Y min:"), value = NA),
                            numericInput("yAxisMax", tagList(icon("long-arrow-alt-up"),    " Y max:"), value = NA)
                          ),
                          div(
                            style = "display: flex; gap: 8px; margin-top: 8px;",
                            numericInput("xAxisMin", tagList(icon("long-arrow-alt-right"), " X min:"), value = NA),
                            numericInput("xAxisMax", tagList(icon("long-arrow-alt-right"), " X max:"), value = NA)
                          ),
                          helpText(icon("info-circle"), "Axe X uniquement pour variables numériques.",
                                   style = "font-size: 11px; color: #555; margin-top: 6px;")
                        ),
                        
                        # ── AXE Y SECONDAIRE (droite) 
                        conditionalPanel(
                          condition = "output.multiYIndicator == true",
                          div(
                            style = "margin-top:15px; padding:15px; background:linear-gradient(135deg,#fff8e1,#fffde7); border:2px solid #ff9800; border-radius:8px;",
                            h5(
                              icon("chart-line", style="color:#e65100;"),
                              tags$b(style="color:#e65100; margin-left:4px;", " Axe Y Secondaire (droite)"),
                              style = "margin:0 0 12px 0; font-size:13px; font-weight:bold;"
                            ),
                            
                            # Sélection des variables Y2
                            uiOutput("vizY2VarSelect"),
                            
                            hr(style="border-color:#ffe0b2; margin:10px 0;"),
                            
                            # ── Note miroir Y1 ──
                            div(
                              style = "margin-bottom:12px; padding:10px; background:#fff8e1; border-radius:6px; border-left:3px solid #ff9800;",
                              tags$p(
                                icon("sync-alt", style="color:#e65100;"),
                                tags$b(style="color:#e65100;", " Mise en forme automatique"),
                                style = "font-size:12px; margin:0 0 4px 0;"
                              ),
                              tags$p(
                                "Taille, police (gras/italique) et épaisseur de l'axe Y2 suivent automatiquement les paramètres de l'axe Y principal.",
                                style = "font-size:11px; color:#555; margin:0;"
                              )
                            ),
                            
                            # ── Nom du label axe Y2 ──
                            div(
                              style = "margin-bottom:12px; padding:10px; background:#fff3e0; border-radius:6px;",
                              h6(icon("font"), " Nom du label axe Y2",
                                 style = "color:#e65100; font-size:12px; font-weight:bold; margin:0 0 8px 0;"),
                              textInput("y2AxisLabel", "Nom de l'axe Y2:", placeholder = "ex: Température (°C)")
                            ),
                            
                            # ── Épaisseur de la courbe Y2 ──
                            div(
                              style = "margin-bottom:12px; padding:10px; background:#fff3e0; border-radius:6px;",
                              h6(icon("chart-line"), " Épaisseur courbe Y2",
                                 style = "color:#e65100; font-size:12px; font-weight:bold; margin:0 0 8px 0;"),
                              sliderInput(
                                "y2CurveWidth",
                                tagList(icon("minus"), " Épaisseur courbe :"),
                                min = 0.5, max = 5, value = 1.2, step = 0.5
                              ),
                              helpText(icon("info-circle"), "Indépendant de 'Épaisseur des lignes' Y1.",
                                       style = "font-size:11px; color:#888;")
                            ),
                            
                            # ── Graduations axe Y2 ──
                            div(
                              style = "margin-bottom:12px; padding:10px; background:#fff3e0; border-radius:6px;",
                              h6(icon("ruler"), " Graduations axe Y2",
                                 style = "color:#e65100; font-size:12px; font-weight:bold; margin:0 0 8px 0;"),
                              numericInput(
                                "y2AxisBreakStep",
                                tagList(icon("long-arrow-alt-up"), " Pas (intervalle):"),
                                value = NA, min = 0.001, step = 1
                              ),
                              helpText(icon("info-circle"), "Ex: 5 → graduations 0, 5, 10...",
                                       style = "font-size:11px; color:#888;")
                            ),
                            
                            # ── Limites axe Y2 ──
                            div(
                              style = "padding:10px; background:#fff3e0; border-radius:6px;",
                              h6(icon("compress-arrows-alt"), " Limites axe Y2 (min / max)",
                                 style = "color:#e65100; font-size:12px; font-weight:bold; margin:0 0 8px 0;"),
                              helpText(icon("info-circle"), "Vide = automatique.",
                                       style = "font-size:11px; color:#888; margin-bottom:6px;"),
                              div(
                                style = "display:flex; gap:8px;",
                                numericInput("y2AxisMin", tagList(icon("long-arrow-alt-down"), " Y2 min:"), value = NA),
                                numericInput("y2AxisMax", tagList(icon("long-arrow-alt-up"),   " Y2 max:"), value = NA)
                              )
                            )
                          )
                        )
                      )
                    ),
                    
                    # COLONNE 3 : APPARENCE ET ELEMENTS DU GRAPHIQUE 
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
                          min = 1, max = 10, value = 3, step = 0.5
                        ),
                        sliderInput(
                          "pointAlpha",
                          tagList(icon("adjust"), " Transparence des points:"),
                          min = 0, max = 1, value = 0.7, step = 0.1
                        ),
                        sliderInput(
                          "lineWidth",
                          tagList(icon("minus"), " Épaisseur des lignes:"),
                          min = 0.5, max = 5, value = 1, step = 0.5
                        ),
                        
                        # ── Marges internes du graphique ──
                        div(
                          style = "margin-bottom:15px; padding:12px; background-color:#e8f5e9; border-radius:6px;",
                          h5(icon("expand-arrows-alt"), " Marges du graphique",
                             style = "color:#2e7d32; font-size:13px; font-weight:bold; margin-top:0; margin-bottom:10px;"),
                          helpText(icon("info-circle"), "Espace entre le graphique et ses bordures (en px).",
                                   style = "font-size:11px; color:#555; margin-bottom:8px;"),
                          div(
                            style = "display:grid; grid-template-columns:1fr 1fr; gap:6px;",
                            numericInput("plotMarginTop",    tagList(icon("arrow-up"),    " Haut:"),   value = 10, min = 0, max = 120, step = 5),
                            numericInput("plotMarginRight",  tagList(icon("arrow-right"), " Droite:"), value = 30, min = 0, max = 120, step = 5),
                            numericInput("plotMarginBottom", tagList(icon("arrow-down"),  " Bas:"),    value = 10, min = 0, max = 120, step = 5),
                            numericInput("plotMarginLeft",   tagList(icon("arrow-left"),  " Gauche:"), value = 10, min = 0, max = 120, step = 5)
                          )
                        ),
                        
                        # Arrière-plan / Thème ggplot2
                        div(
                          style = "margin-top: 15px; padding: 10px; background-color: #e8eaf6; border-radius: 6px; border-left: 3px solid #3f51b5;",
                          h6(
                            tagList(icon("fill-drip"), " Arrière-plan du graphique"),
                            style = "color: #283593; font-weight: bold; margin-top: 0; margin-bottom: 8px; font-size: 12px;"
                          ),
                          selectInput(
                            "plotTheme",
                            NULL,
                            choices = c(
                              "Minimal (blanc, grille grise)"   = "minimal",
                              "Classique (blanc, sans grille)"  = "classic",
                              "Blanc & grille complète"         = "bw",
                              "Fond blanc lumineux"             = "light",
                              "Fond gris doux"                  = "gray",
                              "Fond sombre (dark)"              = "dark",
                              "Fond noir total"                 = "void",
                              "Ligne de base seulement"         = "linedraw"
                            ),
                            selected = "minimal"
                          ),
                          div(
                            style = "margin-top: 6px; display: flex; gap: 6px; flex-wrap: wrap;",
                            actionButton("previewThemeBtn", tagList(icon("eye"), " Aperçu"),
                                         class = "btn-xs btn-info")
                          )
                        ),
                        
                        # Options barres
                        conditionalPanel(
                          condition = "input.vizType == 'bar'",
                          div(
                            style = "margin-top: 15px; padding: 10px; background-color: #e8f5e9; border-radius: 6px;",
                            h5(icon("chart-bar"), " Options Barres",
                               style = "color: #388e3c; font-size: 13px; font-weight: bold; margin-top: 0;"),
                            sliderInput("barWidth",  "Largeur:", min = 0.3, max = 1, value = 0.8, step = 0.1),
                            selectInput(
                              "barPosition", "Position:",
                              choices = c("Côte à côte" = "dodge", "Empilées" = "stack", "Remplissage" = "fill"),
                              selected = "dodge"
                            )
                          )
                        ),
                        
                        # Options aires (area chart)
                        conditionalPanel(
                          condition = "input.vizType == 'area'",
                          div(
                            style = "margin-top: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 6px;",
                            h5(icon("chart-area"), " Options Aires",
                               style = "color: #495057; font-size: 13px; font-weight: bold; margin-top: 0;"),
                            selectInput(
                              "areaPosition", "Position des aires :",
                              choices = c(
                                "Empilées" = "stack",
                                "Côte à côte" = "dodge",
                                "Remplissage (100%)" = "fill",
                                "Identité (superposées)" = "identity"
                              ),
                              selected = "stack"
                            )
                          )
                        ),
                        
                        # Options histogramme
                        conditionalPanel(
                          condition = "input.vizType == 'histogram'",
                          div(
                            style = "margin-top: 15px; padding: 10px; background-color: #e1f5fe; border-radius: 6px;",
                            h5(icon("chart-area"), " Options Histogramme",
                               style = "color: #0277bd; font-size: 13px; font-weight: bold; margin-top: 0;"),
                            sliderInput("histBins", "Nombre de bins:", min = 10, max = 100, value = 30, step = 5),
                            colourInput("histColor", "Couleur:", value = "steelblue")
                          )
                        )
                      ),
                      
                      # Éléments du graphique
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px;",
                        
                        h4(icon("layer-group"), " Éléments du Graphique",
                           style = "color: #2196F3; font-weight: bold; border-bottom: 3px solid #2196F3; padding-bottom: 10px; margin-top: 0;"),
                        
                        checkboxInput("showPoints", tagList(icon("circle"),  " Afficher les points"), value = TRUE),
                        
                        checkboxInput("showValues", tagList(icon("hashtag"), " Afficher les valeurs"), value = FALSE),
                        
                        conditionalPanel(
                          condition = "input.showValues == true",
                          div(
                            style = "margin-top: 8px; padding: 12px; background-color: #e8f4fd; border-radius: 6px; border-left: 3px solid #3498db;",
                            h6(icon("sliders-h"), " Options des valeurs",
                               style = "color: #343a40; font-weight: bold; margin-top: 0;"),
                            sliderInput("valueLabelSize", "Taille:", min = 2, max = 10, value = 3, step = 0.5),
                            selectInput(
                              "valueLabelPosition", "Position:",
                              choices = c("Au-dessus" = "above", "En-dessous" = "below", "Au centre" = "center",
                                          "A droite" = "right", "A gauche" = "left"),
                              selected = "above"
                            ),
                            colourInput("valueLabelColor", "Couleur:", value = "#333333"),
                            div(
                              style = "display: flex; gap: 10px;",
                              checkboxInput("valueLabelBold",   tagList(icon("bold"),   " Gras"),    value = FALSE),
                              checkboxInput("valueLabelItalic", tagList(icon("italic"), " Italique"), value = FALSE)
                            ),
                            numericInput("valueLabelDigits", "Décimales:", value = 2, min = 0, max = 6, step = 1)
                          )
                        ),
                        
                        # Options NA pour courbes (line / area)
                        conditionalPanel(
                          condition = "input.vizType == 'line' || input.vizType == 'area'",
                          div(
                            style = "margin-top: 10px; padding: 12px; background-color: #fff3e0; border-radius: 6px; border-left: 3px solid #ff9800;",
                            h6(tagList(icon("exclamation-triangle"), " Gestion des valeurs manquantes (NA)"),
                               style = "color: #e65100; font-weight: bold; margin-top: 0; font-size: 12px;"),
                            checkboxInput("lineConnectNA",
                                          tagList(icon("link"), " Connecter par-dessus les NA"),
                                          value = FALSE),
                            helpText(style = "font-size: 11px; color: #666;",
                                     icon("info-circle"),
                                     " Coché : la courbe relie les points valides en ignorant les NA.",
                                     " Décoché : la courbe est interrompue aux NA (trou visible)."),
                            checkboxInput("lineShowNAMarker",
                                          tagList(icon("times"), " Marquer les positions NA (×)"),
                                          value = FALSE),
                            helpText(style = "font-size: 11px; color: #666;",
                                     "Affiche une croix grise à chaque position NA en bas du graphique.")
                          )
                        ),
                        
                        # Ligne de tendance (scatter / line)
                        conditionalPanel(
                          condition = "input.vizType == 'scatter' || input.vizType == 'line'",
                          checkboxInput("showTrendLine", tagList(icon("chart-line"), " Ligne de tendance"), value = FALSE),
                          conditionalPanel(
                            condition = "input.showTrendLine == true",
                            selectInput(
                              "trendMethod", "Méthode:",
                              choices = c("Linéaire" = "lm", "LOESS" = "loess", "GAM" = "gam"),
                              selected = "lm"
                            )
                          )
                        ),
                        
                        # Lissage (seasonal_smooth)
                        conditionalPanel(
                          condition = "input.vizType == 'seasonal_smooth'",
                          checkboxInput("showSmoothLine", tagList(icon("bezier-curve"), " Ligne de lissage"), value = TRUE),
                          conditionalPanel(
                            condition = "input.showSmoothLine == true",
                            selectInput(
                              "smoothMethod", "Méthode:",
                              choices = c("LOESS" = "loess", "Linéaire" = "lm", "GAM" = "gam"),
                              selected = "loess"
                            ),
                            sliderInput("smoothSpan", "Degré de lissage:", min = 0.1, max = 2, value = 0.75, step = 0.05)
                          )
                        ),
                        
                        checkboxInput("showConfidenceInterval", tagList(icon("area-chart"), " Intervalle de confiance"), value = TRUE),
                        
                        # Outliers (boxplot)
                        conditionalPanel(
                          condition = "input.vizType == 'box'",
                          checkboxInput("showOutliers", tagList(icon("circle-notch"), " Afficher les outliers"), value = TRUE)
                        ),
                        
                        # Boîte intérieure (violin)
                        conditionalPanel(
                          condition = "input.vizType == 'violin'",
                          checkboxInput("showBoxInsideViolin", tagList(icon("box"), " Boîte à l'intérieur"), value = FALSE)
                        )
                      )
                    )
                  )
                )
              ),
              
              # PANNEAU DES STATISTIQUES SAISONNIÈRES
              fluidRow(
                # Informations saisonnières (conditionnelles)
                conditionalPanel(
                  condition = "input.vizType == 'seasonal_smooth' || input.vizType == 'seasonal_evolution'",
                  box(
                    title = tagList(icon("calendar-alt"), " Analyse Saisonnière"),
                    status = "success",
                    width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    div(
                      style = "padding: 15px;",
                      uiOutput("seasonalDuplicateWarning"),
                      verbatimTextOutput("seasonalAnalysisSummary")
                    )
                  )
                )
              ),
              
              # SCRIPTS JAVASCRIPT ET CSS 
              tags$head(
                # Script SortableJS pour le drag-and-drop
                tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"),
                
                # Script d'initialisation Sortable
                tags$script(HTML("
    $(document).ready(function() {
      // Fonction pour initialiser Sortable (recrée toujours une instance fraiche)
      function initSortable() {
        var el = document.getElementById('xOrderSortable');
        if (!el) return;
        // Détruire l'ancienne instance si elle existe
        if (el.sortableInstance) {
          el.sortableInstance.destroy();
          el.sortableInstance = null;
        }
        el.sortableInstance = Sortable.create(el, {
          animation: 150,
          ghostClass: 'sortable-ghost',
          handle: '.sortable-item',
          onEnd: function(evt) {
            var items = el.querySelectorAll('.sortable-item');
            var order = [];
            items.forEach(function(item) {
              order.push(item.getAttribute('data-value'));
            });
            Shiny.setInputValue('xLevelOrder', order, {priority: 'event'});
          }
        });
      }
      
      // Initialiser au chargement
      setTimeout(initSortable, 500);
      
      // Réinitialiser chaque fois que xOrderEditor ou xLevelsEditor est mis à jour
      $(document).on('shiny:value', function(event) {
        if (event.target.id === 'xOrderEditor' || event.target.id === 'xLevelsEditor') {
          setTimeout(initSortable, 150);
        }
      });
      
      // Réinitialiser aussi après shiny:recalculated (au cas où le DOM est reconstruit)
      $(document).on('shiny:recalculated', function() {
        if (document.getElementById('xOrderSortable')) {
          setTimeout(initSortable, 200);
        }
      });
    });
  ")),
                
                # Styles CSS
                tags$style(HTML("
    /* Styles pour le drag-and-drop */
    .sortable-ghost {
      opacity: 0.4;
      background-color: #e3f2fd;
    }
    
    /* BOUTON TÉLÉCHARGEMENT */
    #downloadPlotBtn {
      pointer-events: auto !important;
      cursor: pointer !important;
      background-color: #28a745 !important;
      border-color: #28a745 !important;
      color: white !important;
    }
    
    #downloadPlotBtn:hover {
      background-color: #218838 !important;
      border-color: #1e7e34 !important;
      transform: scale(1.02);
    }
    
    #downloadPlotBtn:active {
      transform: scale(0.98);
    }
    
    #downloadPlotBtn:disabled {
      background-color: #6c757d !important;
      border-color: #6c757d !important;
      cursor: wait !important;
    }
    
    .sortable-item:hover {
      box-shadow: 0 4px 8px rgba(0,0,0,0.2) !important;
      border-color: #28a745 !important;
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
                             checkboxInput("interaction", "Inclure les interactions (ANOVA/Scheirer-Ray-Hare)", FALSE),
                             hr(),
                             # Option d'arrondi pour les tests
                             div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px;",
                                 fluidRow(
                                   column(6,
                                          checkboxInput("testsRoundResults", "Arrondir les résultats", value = FALSE)
                                   ),
                                   column(6,
                                          conditionalPanel(
                                            condition = "input.testsRoundResults == true",
                                            numericInput("testsDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                          )
                                   )
                                 )
                             )
                      ),
                      column(4,
                             h4("Tests sur données brutes", style = "color: #3c8dbc;"),
                             div(style="display:flex; flex-direction:column; gap:8px; margin-bottom:12px;",
                               actionButton("testNormalityRaw",   "Test de normalité",     class = "btn-warning btn-block", icon = icon("chart-line")),
                               actionButton("testHomogeneityRaw", "Test d'homogénéité",    class = "btn-warning btn-block", icon = icon("balance-scale"))
                             ),
                             h4("Tests paramétriques", style = "color: #00a65a;"),
                             div(style="display:flex; flex-direction:column; gap:8px;",
                               actionButton("testT",    "Test t de Student",           class = "btn-success btn-block", icon = icon("check")),
                               actionButton("testANOVA","ANOVA",                        class = "btn-success btn-block", icon = icon("check")),
                               actionButton("testLM",   "Régression linéaire",          class = "btn-success btn-block", icon = icon("check")),
                               actionButton("testGLM",  "Modèle linéaire généralisé",   class = "btn-success btn-block", icon = icon("check"))
                             )
                      ),
                      column(4,
                             h4("Tests non-paramétriques", style = "color: #f39c12;"),
                             div(style="display:flex; flex-direction:column; gap:8px;",
                               actionButton("testWilcox",          "Test de Wilcoxon",          class = "btn-warning btn-block", icon = icon("check")),
                               actionButton("testKruskal",         "Test de Kruskal-Wallis",     class = "btn-warning btn-block", icon = icon("check")),
                               actionButton("testScheirerRayHare", "Test de Scheirer-Ray-Hare",  class = "btn-warning btn-block", icon = icon("check"))
                             )
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
                
                # PANEL GAUCHE - Configuration 
                
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
                                        "Dunn" = "dunn",
                                        "Conover" = "conover",
                                        "Nemenyi" = "nemenyi"
                                      ),
                                      selected = "dunn"
                          )
                        )
                    ),
                    
                    
                    # Analyse des interactions 
                    
                    div(style = "border: 3px solid #e74c3c; border-radius: 8px; padding: 15px; margin-bottom: 15px; background: linear-gradient(135deg, #fff5f5 0%, #ffe8e8 100%);",
                        h4(style = "color: #c0392b; margin-top: 0;", 
                           icon("project-diagram"), " Analyse des interactions"),
                        checkboxInput("posthocInteraction", 
                                      HTML("<strong style='color: #c0392b;'>Activer l'analyse des interactions</strong>"), 
                                      value = FALSE),
                        conditionalPanel(
                          condition = "input.posthocInteraction == true",
                          div(style = "margin-top: 8px; padding: 8px 10px; background:#fff3e0; border-left:3px solid #ff9800; border-radius:4px;",
                              tags$small(style="color:#e65100;",
                                icon("info-circle"), " Sélectionnez ≥ 2 facteurs. Les effets simples s'affichent dans l'onglet 'Effets simples'."
                              )
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
                    
                    
                ),
                
                
                # PANEL DROIT - Résultats 
                
                box(title = div(icon("table"), " Résultats et visualisations"), 
                    status = "primary", width = 8, solidHeader = TRUE,
                    
                    # Organiser les résultats en onglets
                    tabsetPanel(id = "resultsTabs", type = "tabs",
                                
                                
                                # ONGLET 1 : Effets principaux 
                                
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
                                
                                
                                # ONGLET 2 : Effets simples 
                                
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
                                             → Compare les traitements <u>au temps T0 uniquement</u></li>
                                         <li><code style='background:#fff;padding:2px 6px;'>Temps | Traitement=Ctrl</code><br/>
                                             → Compare les temps <u>pour le contrôle uniquement</u></li>
                                       </ul>
                                       
                                       <p><b>Utilité :</b> Identifier <i>où précisément</i> les facteurs diffèrent lorsqu'ils interagissent.</p>
                                       </div>")
                                    ),
                                    
                                    # Filtres
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
                                
                                
                                # ONGLET 3 : Visualisations 
                                
                                tabPanel(
                                  title = div(icon("chart-bar"), " Graphiques"),
                                  value = "plots",
                                  br(),
                                  
                                  # Navigation des variables 
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
                                      plotlyOutput("multiPlot", height = "600px")
                                  ),
                                  
                                  br(),
                                  
                                  # ── Options du graphique
                                  div(style = "border: 1px solid #dee2e6; border-radius: 8px; overflow: hidden;",
                                    div(
                                      class = "panel-heading",
                                      style = "background-color: #343a40; color: white; padding: 12px 18px; cursor: pointer; display: flex; align-items: center;",
                                      `data-toggle` = "collapse",
                                      `data-target` = "#graphOptionsPanel",
                                      icon("sliders-h", style = "margin-right: 8px;"),
                                      tags$strong("Options du graphique"),
                                      tags$span(style = "margin-left: auto; font-size: 12px; opacity: 0.75;",
                                                icon("chevron-down"), " Développer / Réduire")
                                    ),
                                    div(id = "graphOptionsPanel", class = "collapse",
                                      div(style = "padding: 20px; background-color: #fdfdfd;",
                                        
                                        fluidRow(
                                          # COL 1 : Type + Couleurs
                                          column(4,
                                            div(style = "padding-right: 15px; border-right: 1px solid #e9ecef;",
                                              h6(icon("palette"), " Type et couleurs",
                                                 style = "font-weight: bold; color: #343a40; border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 12px;"),
                                              selectInput("boxColor", "Palette",
                                                choices = c("Défaut" = "default", "Bleu" = "Blues",
                                                            "Vert" = "Greens", "Rouge" = "Reds",
                                                            "Set1" = "Set1", "Pastel" = "Pastel1",
                                                            "Paired" = "Paired"),
                                                selected = "Set1"),
                                              radioButtons("plotType", "Type de graphique",
                                                choices = c("Boxplot" = "box", "Violon" = "violin",
                                                            "Points + barres" = "point", "Barres" = "hist"),
                                                selected = "box", inline = TRUE),
                                              radioButtons("errorType", "Barres d'erreur",
                                                choices = c("SE" = "se", "SD" = "sd",
                                                            "IC 95%" = "ci", "Aucune" = "none"),
                                                selected = "se", inline = TRUE),
                                              checkboxInput("colorByGroups",
                                                HTML("Colorer par groupes statistiques <small style='color:#6c757d;'>(a, b, c...)</small>"),
                                                value = FALSE)
                                            )
                                          ),
                                          
                                          # COL 2 : Titres + Tailles
                                          column(4,
                                            div(style = "padding-left: 15px; padding-right: 15px; border-right: 1px solid #e9ecef;",
                                              h6(icon("heading"), " Titres et tailles",
                                                 style = "font-weight: bold; color: #343a40; border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 12px;"),
                                              textInput("customTitle", "Titre", placeholder = "Auto"),
                                              textInput("customSubtitle", "Sous-titre", placeholder = "Optionnel"),
                                              fluidRow(
                                                column(6, textInput("customXLabel", "Label X", placeholder = "Auto")),
                                                column(6, textInput("customYLabel", "Label Y", placeholder = "Auto"))
                                              ),
                                              textInput("customLegendTitle", "Titre légende", placeholder = "Auto"),
                                              fluidRow(
                                                column(6, sliderInput("titleSize", "Titre", min = 8, max = 32, value = 16, step = 1, ticks = FALSE)),
                                                column(6, sliderInput("axisTitleSize", "Axes titres", min = 8, max = 28, value = 14, step = 1, ticks = FALSE))
                                              ),
                                              fluidRow(
                                                column(6, sliderInput("axisTextSize", "Texte axes", min = 6, max = 24, value = 12, step = 1, ticks = FALSE)),
                                                column(6, sliderInput("graphValueSize", "Lettres (a,b,c)", min = 2, max = 20, value = 5, step = 0.5, ticks = FALSE))
                                              ),
                                              sliderInput("meanValueSize", "Taille moyennes dans barres",
                                                min = 2, max = 12, value = 4, step = 0.5, ticks = FALSE),
                                              # Styles de police
                                              fluidRow(
                                                column(6,
                                                  selectInput("titleFontStyle", "Style titre",
                                                    choices = c("Normal" = "plain", "Gras" = "bold",
                                                                "Italique" = "italic", "Gras+Italique" = "bold.italic"),
                                                    selected = "bold")
                                                ),
                                                column(6,
                                                  selectInput("axisTitleFontStyle", "Style titres axes",
                                                    choices = c("Normal" = "plain", "Gras" = "bold",
                                                                "Italique" = "italic", "Gras+Italique" = "bold.italic"),
                                                    selected = "plain")
                                                )
                                              ),
                                              fluidRow(
                                                column(6,
                                                  selectInput("graphValueFontStyle", "Style lettres (a,b,c)",
                                                    choices = c("Normal" = "plain", "Gras" = "bold",
                                                                "Italique" = "italic", "Gras+Italique" = "bold.italic"),
                                                    selected = "bold")
                                                ),
                                                column(6,
                                                  checkboxInput("rotateXLabels", "Labels X à 45°", value = TRUE)
                                                )
                                              )
                                            )
                                          ),
                                          
                                          # COL 3 : Axes + Ordre
                                          column(4,
                                            div(style = "padding-left: 15px;",
                                              h6(icon("ruler-combined"), " Axes et ordre",
                                                 style = "font-weight: bold; color: #343a40; border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 12px;"),
                                              checkboxInput("customAxisLimits", "Personnaliser les limites des axes", value = FALSE),
                                              conditionalPanel(
                                                condition = "input.customAxisLimits == true",
                                                fluidRow(
                                                  column(6, numericInput("yAxisMin", "Y min", value = NULL, step = 0.1)),
                                                  column(6, numericInput("yAxisMax", "Y max", value = NULL, step = 0.1))
                                                )
                                              ),
                                              checkboxInput("customAxisBreaks", "Personnaliser les graduations", value = FALSE),
                                              conditionalPanel(
                                                condition = "input.customAxisBreaks == true",
                                                fluidRow(
                                                  column(6, numericInput("yAxisBreakStep", "Pas Y", value = NULL, step = 0.1, min = 0.01)),
                                                  column(6, numericInput("xAxisBreakStep", "Pas X", value = NULL, step = 0.1, min = 0.01))
                                                )
                                              ),
                                              hr(style = "margin: 10px 0;"),
                                              checkboxInput("customXOrder", "Personnaliser l'ordre axe X", value = FALSE),
                                              conditionalPanel(
                                                condition = "input.customXOrder == true",
                                                uiOutput("xAxisOrderUI")
                                              ),
                                              hr(style = "margin: 10px 0;"),
                                              # Inputs légende
                                              fluidRow(
                                                column(6, sliderInput("legendTitleSize", "Titre légende", min = 6, max = 24, value = 12, step = 1, ticks = FALSE)),
                                                column(6, sliderInput("legendTextSize", "Texte légende", min = 6, max = 20, value = 10, step = 1, ticks = FALSE))
                                              ),
                                              sliderInput("legendSpacing", "Espacement légende",
                                                min = 0, max = 6, value = 0, step = 0.1, ticks = FALSE),
                                              # Inputs masqués pour export + styles supplémentaires
                                              tags$div(style = "display:none;",
                                                numericInput("plotWidth",  "Largeur", value = 8,   min = 3, max = 20),
                                                numericInput("plotHeight", "Hauteur", value = 6,   min = 3, max = 20),
                                                numericInput("plotDPI",    "DPI",     value = 300, min = 72, max = 600),
                                                numericInput("xAxisMin", "X min", value = NULL, step = 0.1),
                                                numericInput("xAxisMax", "X max", value = NULL, step = 0.1),
                                                sliderInput("subtitleSize", "Sous-titre", min = 6, max = 28, value = 12, step = 1),
                                                selectInput("subtitleFontStyle", "Style sous-titre",
                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                  selected = "italic"),
                                                selectInput("axisTextXFontStyle", "Style axe X",
                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                  selected = "plain"),
                                                selectInput("axisTextYFontStyle", "Style axe Y",
                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                  selected = "plain"),
                                                selectInput("legendTitleFontStyle", "Style titre légende",
                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                  selected = "bold"),
                                                selectInput("legendTextFontStyle", "Style texte légende",
                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                  selected = "plain"),
                                                selectInput("subtitlePosition", "Position sous-titre",
                                                  choices = list("Centré"="0.5","Gauche"="0","Droite"="1"), selected="0.5"),
                                                numericInput("legendKeySize", "Icône légende", value=0.5, min=0.1, max=3, step=0.1)
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  ),
                                  
                                  br(),
                                  
                                  # ── Téléchargement
                                  div(style = "text-align: center;",
                                      downloadButton("downloadMultiPlot",
                                                     tagList(icon("image"), " Télécharger le graphique (PNG)"),
                                                     class = "btn-success",
                                                     style = "width: 100%; max-width: 400px; height: 50px; font-weight: bold;")
                                  )
                                ),
                                
                                
                                # ONGLET 4 : Rapport complet 
                                
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
      # ---- Analyses multivariees ----
      
      tabItem(tabName = "multivariate",
              # ACP
              fluidRow(
                box(title = "Analyse en Composantes Principales (ACP)", status = "info", width = 6, solidHeader = TRUE,
                    
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
                           icon("clipboard-check"), " Validation de l'ACP — Ordre d'analyse recommandé"),
                        p(style = "font-size: 11px; color: #555; margin-bottom: 0;",
                          "1. Vérifiez l'adéquation des données (Bartlett + KMO) → 2. Choisissez le nombre de composantes (Scree plot + Analyse parallèle) → 3. Examinez les contributions (CTR) → 4. Appliquez la rotation pour affiner l'interprétation.")
                    ),
                    
                    # 1. Bartlett + KMO
                    h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                       icon("check-circle"), " 1. Adéquation des données à l'ACP"),
                    uiOutput("pcaBartlettKMO"),
                    
                    # 2. Scree plot
                    h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                       icon("chart-line"), " 2. Graphique des éboulis (Scree Plot)"),
                    p(style = "font-size: 11px; color: #666; font-style: italic;",
                      "Critère de Kaiser (λ ≥ 1) : les composantes en vert sont retenues. Cherchez le 'coude' de la courbe."),
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
                       icon("random"), " 3. Analyse parallèle de Horn"),
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
                       icon("percentage"), " 4. Contributions absolues (CTR) des variables"),
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
                       icon("sync-alt"), " 5. Résultats de la rotation orthogonale"),
                    p(style = "font-size: 11px; color: #666; font-style: italic;",
                      "La rotation simplifie la structure : un loading |x| ≥ 0,70 indique une contribution forte, |x| de 0,40 à 0,70 une contribution modérée."),
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
                               icon("chart-area"), " 1. Graphique des hauteurs de fusion")
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
                               icon("ruler-combined"), " 2. Indices de validation des clusters")
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
                               icon("shield-alt"), " 3. Stabilité par sous-échantillonnage")
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
                           icon("chart-line"), " ÉTAPE 2 : Variables quantitatives (OBLIGATOIRE)"),
                        p(style = "margin: 5px 0; font-size: 13px; color: #555;",
                          "Sélectionnez les variables numériques qui serviront à discriminer les groupes. Plus il y a de variables pertinentes, meilleure sera la discrimination."),
                        uiOutput("afdVarSelect"),
                        p(style = "margin: 5px 0 0 0; font-size: 11px; color: #27ae60; font-style: italic;",
                          icon("check-circle"), " Conseil : Sélectionnez au moins 2-3 variables pour obtenir de bons résultats.")
                    ),
                    
                    # Panel colinéarité AFD (affiché dynamiquement si colinéarité détectée)
                    uiOutput("afdCollinearityPanel"),
                    
                    hr(),
                    h4(style = "color: #6c757d; margin-top: 10px;", icon("cogs"), " ÉTAPE 3 : Options avancées (optionnel)"),
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
                                   icon("list-ol"), " Ordre d'analyse recommandé"),
                                p(style = "font-size: 12px; color: #555; margin-bottom: 0;",
                                  "1. Vérifiez la variance expliquée et les corrélations canoniques (Section 1) → 2. Évaluez la force de discrimination via eta² (Section 2b) → 3. Analysez la qualité de classification avec l'accuracy et le Kappa (Section 3) → 4. Interprétez les coefficients et la matrice de structure (Section 2) → 5. Vérifiez la validation croisée (Section 4).")
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
                             style = "font-weight: bold; color: #495057; margin-bottom: 10px;"),
                          
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
                    
                    # Exportation 
                    div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                        h4(icon("download"), " Options d'exportation haute qualité", 
                           style = "color: white; font-weight: bold; margin: 0;")
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
      )
      
    )
  )
)
