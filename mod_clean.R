################################################################################
#
#  Module Shiny : Nettoyage des donnees
#
#  Onglet migre en module Shiny (4e migration). Modifie les donnees partagees :
#  ecrit values$cleanData et values$filteredData (consommees par les autres
#  onglets). 'values' etant un reactiveValues passe par reference, ces ecritures
#  se propagent a toute l'application.
#  - mod_clean_ui(id)             : contenu de l'onglet (dans un tabItem)
#  - mod_clean_server(id, values) : 'values' = reactiveValues GLOBAL
################################################################################

mod_clean_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
                             uiOutput(ns("varTypeUI"))
                           )
                    ),
                    column(3,
                           div(
                             style = "text-align: center; padding: 20px;",
                             actionButton(ns("applyTypes"),
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
                        uiOutput(ns("removeVarUI")),
                        actionButton(ns("removeVar"),
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
                                   "Formats : ", tags$code("1,3,5"), " -- ", tags$code("10 à 20"), " -- ", tags$code("1,3,10 à 15")),
                            textAreaInput(ns("deleteRowsInput"), NULL,
                                          placeholder = "1,3,5
10 à 20
1,3,5,10 à 15",
                                          rows = 3, width = "100%"),
                            uiOutput(ns("deleteRowsPreview")),
                            actionButton(ns("applyDeleteRows"),
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
                            DT::dataTableOutput(ns("deleteRowsTable"), height = "260px")
                          ),
                          br(),
                          uiOutput(ns("deleteRowsInteractivePreview")),
                          br(),
                          actionButton(ns("applyDeleteRowsInteractive"),
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
                        textInput(ns("newVarName"), "Nom:", placeholder = "ex: Categorie"),
                        numericInput(ns("newVarValue"), "Valeur par défaut:", 0),
                        actionButton(ns("addVar"),
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
                    
                    textInput(ns("calcVarName"), 
                      "Nom de la variable calculée:",
                      placeholder = "ex: Moyenne_Score"
                    ),
                    
                    # - Sélection de colonnes et lignes
                    fluidRow(
                      column(6,
                             div(
                               style = "background-color: #eef7ff; padding: 10px; border-radius: 5px;",
                               h6(icon("columns"), " Colonnes", style = "margin-bottom: 6px; color: #2c5aa0;"),
                               uiOutput(ns("colPicker")),
                               tags$small(style = "color: #6c757d;",
                                          icon("info-circle"), " Cliquez pour insérer dans la formule")
                             )
                      ),
                      column(6,
                             div(
                               style = "background-color: #f0fff4; padding: 10px; border-radius: 5px;",
                               h6(icon("filter"), " Filtrer sur lignes (optionnel)", style = "margin-bottom: 6px; color: #1a6e2e;"),
                               uiOutput(ns("rowCondPicker")),
                               tags$small(style = "color: #6c757d;",
                                          icon("info-circle"), " Génère ifelse() dans la formule")
                             )
                      )
                    ),
                    
                    br(),
                    
                    # - Opérateurs et fonctions
                    fluidRow(
                      column(12,
                             div(
                               style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                               fluidRow(
                                 column(6,
                                        h6("Opérateurs :", style = "margin-bottom: 5px; font-size: 11px; color: #555;"),
                                        div(style = "display: flex; flex-wrap: wrap; gap: 4px;",
                                            actionButton(ns("insertPlus"),  "+",  class = "btn-outline-secondary btn-sm"),
                                            actionButton(ns("insertMoins"), "-",  class = "btn-outline-secondary btn-sm"),
                                            actionButton(ns("insertMult"),  "x",  class = "btn-outline-secondary btn-sm"),
                                            actionButton(ns("insertDiv"),   "÷",  class = "btn-outline-secondary btn-sm"),
                                            actionButton(ns("insertPow"),   "^",  class = "btn-outline-secondary btn-sm"),
                                            actionButton(ns("insertParen"), "()", class = "btn-outline-secondary btn-sm")
                                        )
                                 ),
                                 column(6,
                                        h6("Fonctions :", style = "margin-bottom: 5px; font-size: 11px; color: #555;"),
                                        div(style = "display: flex; flex-wrap: wrap; gap: 4px;",
                                            actionButton(ns("insertLog"),   "log()",    class = "btn-outline-info btn-sm"),
                                            actionButton(ns("insertLog10"), "log10()",  class = "btn-outline-info btn-sm"),
                                            actionButton(ns("insertSqrt"),  "sqrt()",   class = "btn-outline-info btn-sm"),
                                            actionButton(ns("insertAbs"),   "abs()",    class = "btn-outline-info btn-sm"),
                                            actionButton(ns("insertRound"), "round()",  class = "btn-outline-info btn-sm"),
                                            actionButton(ns("insertExp"),   "exp()",    class = "btn-outline-info btn-sm"),
                                            actionButton(ns("insertMean"),  "mean()",   class = "btn-outline-info btn-sm"),
                                            actionButton(ns("insertSum"),   "sum()",    class = "btn-outline-info btn-sm"),
                                            actionButton(ns("insertIfelse"),"ifelse()", class = "btn-outline-warning btn-sm"),
                                            actionButton(ns("insertIsNA"),  "is.na()",  class = "btn-outline-warning btn-sm")
                                        )
                                 )
                               )
                             )
                      )
                    ),
                    
                    textInput(ns("calcFormula"), 
                      "Formule de calcul:",
                      placeholder = "ex: (Rendement + Biomasse) / 2   |   sum(Poids, Hauteur)   |   sqrt(Var1 * Var2)"
                    ),
                    
                    # - Exemples d'application des fonctions
                    tags$div(
                      style = "margin-top:4px; margin-bottom:12px; padding:10px 12px; background:#f0f7ff; border:1px solid #bee3f8; border-radius:6px; font-size:11.5px;",
                      tags$b(style="color:#1a56db; font-size:12px;", icon("calculator"), " Exemples d'utilisation :"),
                      tags$table(
                        style = "width:100%; margin-top:6px; border-collapse:collapse;",
                        # - En-tête opérations multi-variables -
                        tags$tr(style="background:#dbeafe;",
                                tags$td(colspan="2",style="padding:4px 8px;color:#1e40af;font-size:11px;font-weight:bold;",
                                        icon("layer-group")," Opérations sur plusieurs variables")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;color:#555;width:52%;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "(Rendement + Biomasse) / 2")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Moyenne de 2 variables")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "mean(c(Rendement, Biomasse, Poids))")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Moyenne (n variables)")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "Rendement + Biomasse + Poids")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Somme de variables")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "Rendement / (Biomasse + Poids)")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Ratio entre variables")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "sqrt(Rendement * Biomasse)")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Moyenne géométrique")
                        ),
                        # - En-tête transformations -
                        tags$tr(style="background:#dcfce7;",
                                tags$td(colspan="2",style="padding:4px 8px;color:#166534;font-size:11px;font-weight:bold;",
                                        icon("calculator")," Transformations classiques")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "log(Rendement)")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Logarithme naturel")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "log10(Rendement + 1)")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Log10 (si zéros présents)")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "sqrt(Rendement)")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Racine carrée")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "Rendement^2")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Mise au carré")
                        ),
                        tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                tags$td(style="padding:3px 6px;",
                                        tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "round(Rendement / Biomasse, 3)")),
                                tags$td(style="padding:3px 6px;color:#666;","-> Ratio arrondi à 3 déc.")
                        ),
                        tags$tr(
                          tags$td(style="padding:3px 6px;",
                                  tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                            "(Rendement - mean(Rendement)) / sd(Rendement)")),
                          tags$td(style="padding:3px 6px;color:#666;","-> Z-score")
                        )
                      )
                    ),
                    
                    actionButton(ns("addCalcVar"), 
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
                             
                             uiOutput(ns("naVarSelect")),
                             
                             h5(icon("tools"), "Méthode de Traitement", style = "color: #3498db; margin-top: 20px;"),
                             
                             radioButtons(ns("naMethod"), 
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
              ns = ns,
                               condition = "input.naMethod == 'value'",
                               div(
                                 style = "margin-top: 15px; padding: 10px; background-color: white; border-radius: 5px;",
                                 numericInput(ns("naValue"), 
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
                             actionButton(ns("applyNA"), 
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
                    DTOutput(ns("cleanedData")),
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
  )
}

mod_clean_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # -- Helper : parser une selection de lignes (ex. "1-5, 8, 10-12") --
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
                  "Format invalide -- vérifiez votre saisie.")
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
  
  # - Tableau interactif pour sélection des lignes à supprimer
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
                 icon("info-circle"), " Aucune ligne sélectionnée -- cliquez sur des lignes dans le tableau."))
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
    selectInput(ns("removeVarName"), "Supprimer variable :", 
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
    selectInput(ns("colInsert"), "Insérer colonne :", 
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
      selectInput(ns("rowCondCol"), "Colonne :", choices = c("", names(values$cleanData))),
      conditionalPanel(
        ns = ns,
        condition = "input.rowCondCol != ''",
        fluidRow(
          column(6, selectInput(ns("rowCondOp"), "Opérateur :",
                                choices = c("==" = "==", "!=" = "!=", ">" = ">", ">=" = ">=", "<" = "<", "<=" = "<=", "is.na" = "is.na"),
                                selected = "==")),
          column(6, textInput(ns("rowCondVal"), "Valeur :", placeholder = "ex: 'A' ou 10"))
        ),
        actionButton(ns("insertRowCond"), tagList(icon("filter"), " Insérer condition"),
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
      inputId = ns("naVars"),
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
  })
}
