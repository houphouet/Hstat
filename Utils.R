################################################################################
#
#             Encodage de l'application
#
################################################################################


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
# Fonctions utilitaires
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
  "RColorBrewer", "colourpicker", "ggrepel",  "openxlsx", "rmarkdown", "haven", "base64enc",
  "dplyr", "knitr", "stringr", "scales", "ggplot2", "ggdendro", "reshape2", "sortable",
  "tibble", "plotrix", "plotly",  "qqplotr", "tidyr",  "report", "see", "corrplot",
  "car", "agricolae","forcats", "bslib", "factoextra",  "FactoMineR","questionr",  "digest",
  "MASS", "cluster", "GGally", "psych", "nortest", "lmtest", "multcomp","FSA", "treemapify", "ggtext",
  "stats",  "emmeans", "performance","purrr", "PMCMRplus","multcompView", "rcompanion"
)

install_and_load(required_packages)

# Petit utilitaire
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Supprimer les colonnes
remove_zero_var_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(df)
  keep <- sapply(df, function(x) {
    if (!is.numeric(x)) return(TRUE)
    sd_val <- sd(x, na.rm = TRUE)
    !is.na(sd_val) && sd_val > 0
  })
  df[, keep, drop = FALSE]
}

# Filtrer les colonnes non-numériques
safe_cor <- function(df, use = "pairwise.complete.obs") {
  if (is.null(df)) return(NULL)
  df <- df[, sapply(df, is.numeric), drop = FALSE]
  df <- remove_zero_var_cols(df)
  if (is.null(df) || ncol(df) < 2) return(NULL)
  tryCatch(suppressWarnings(cor(df, use = use)), error = function(e) NULL)
}

# Fonction pour détecter les variables catégorielles 
is_categorical <- function(x) {
  is.factor(x) || is.character(x) || inherits(x, "Date") || inherits(x, "POSIXt")
}

# Fonction pour obtenir les colonnes catégorielles d'un dataframe
get_categorical_cols <- function(df) {
  names(df)[sapply(df, is_categorical)]
}

# Selection des colonnes candidates pour être des facteurs (catégorielles)
get_all_factor_candidates <- function(df, max_numeric_levels = 30) {
  nms <- names(df)
  keep <- sapply(nms, function(col) {
    x <- df[[col]]
    if (is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
    if (inherits(x, "Date") || inherits(x, "POSIXt"))    return(TRUE)
    if (is.numeric(x)) return(length(unique(na.omit(x))) <= max_numeric_levels)
    FALSE
  })
  nms[keep]
}

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
#  Fonctions utilitaires — Visualisation des données
################################################################################

# Formats de dates valides pour l'affichage sur l'axe X
VIZ_DATE_FORMATS_VALID <- c(
  "%d-%m-%Y", "%m-%d-%Y", "%Y-%m-%d", "%Y-%d-%m",
  "%d/%m/%Y", "%m/%d/%Y",
  "%d-%m",    "%m-%d",    "%m-%Y",    "%Y-%m",
  "%d-%b-%Y", "%b-%Y",    "%d-%b",    "%b-%d",    "%Y-%b-%d",
  "%d %B %Y", "%B %Y",    "%d %B",    "%B %d",    "%Y %B"
)

# Valider un format de date
#' @param fmt character
#' @return logical
viz_valid_date_fmt <- function(fmt) {
  !is.null(fmt) && nzchar(trimws(fmt)) && fmt %in% VIZ_DATE_FORMATS_VALID
}

# Détecter le type d'une variable X
#' @param x vector
#' @return character : "date" | "numeric" | "factor" | "categorical" | "text"
viz_detect_x_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("date")
  if (is.factor(x))    return("factor")
  if (is.numeric(x))   return("numeric")
  if (is.character(x))
    return(if (length(unique(x)) < length(x) / 2) "categorical" else "text")
  "text"
}

# Choisir un thème ggplot2
#' @param theme_name character (clé parmi les thèmes ggplot2 disponibles)
#' @param base_size  numeric
#' @return ggplot2 theme object
viz_get_theme <- function(theme_name = "minimal", base_size = 12) {
  switch(theme_name,
    "minimal"  = ggplot2::theme_minimal( base_size = base_size),
    "classic"  = ggplot2::theme_classic( base_size = base_size),
    "bw"       = ggplot2::theme_bw(      base_size = base_size),
    "light"    = ggplot2::theme_light(   base_size = base_size),
    "gray"     = ggplot2::theme_gray(    base_size = base_size),
    "dark"     = ggplot2::theme_dark(    base_size = base_size),
    "void"     = ggplot2::theme_void(    base_size = base_size),
    "linedraw" = ggplot2::theme_linedraw(base_size = base_size),
    ggplot2::theme_minimal(base_size = base_size)          
  )
}

# Construire scale_x_* adapté au type
#' Unique point d'entrée pour l'axe X.
#' Lit le format, le mapping de labels et l'ordre directement en paramètres
#' (jamais via attr() qui est perdu lors des manipulations de data.frame).
#'
#' @param x_col      vector   : colonne X (Date, numeric, factor, character)
#' @param disp_fmt   character : format strftime pour les dates
#' @param label_map  named vector/list : original -> étiquette affichée
#'                   NULL = pas de renommage
#' @param custom_ord character vector : ordre souhaité des valeurs X
#'                   NULL = ordre naturel
#' @return ggplot2 scale object
viz_get_x_scale <- function(x_col,
                             disp_fmt   = "%d-%m-%Y",
                             label_map  = NULL,
                             custom_ord = NULL) {
  # Dé-wrapper AsIs (produit par I() dans data.frame) tout en préservant le type
  if (inherits(x_col, "AsIs")) {
    cls <- class(x_col)
    cls <- cls[cls != "AsIs"]
    x_col <- unclass(x_col)
    class(x_col) <- cls
  }
  x_is_date    <- inherits(x_col, c("Date", "POSIXct", "POSIXlt"))
  x_is_numeric <- is.numeric(x_col) && !x_is_date

  # Déterminer si le mapping contient de vrais changements
  has_lm <- !is.null(label_map) && length(label_map) > 0 &&
             any(as.character(label_map) != names(label_map))

  if (x_is_date) {
    # ── Dates
    all_x <- sort(unique(x_col))

    # Ordre personnalisé (valeurs converties en Date)
    if (!is.null(custom_ord) && length(custom_ord) > 0) {
      co_dates <- tryCatch(as.Date(custom_ord), error = function(e) NULL)
      if (!is.null(co_dates)) {
        valid_ord <- co_dates[!is.na(co_dates) & co_dates %in% all_x]
        if (length(valid_ord) > 0) all_x <- valid_ord
      }
    }

    # Labels : mapping personnalisé sinon format strftime
    labels_vec <- sapply(as.character(all_x), function(v) {
      if (has_lm && v %in% names(label_map) &&
          as.character(label_map[[v]]) != v)
        as.character(label_map[[v]])
      else
        tryCatch(format(as.Date(v), disp_fmt), error = function(e) v)
    }, USE.NAMES = FALSE)

    return(ggplot2::scale_x_date(
      breaks = all_x,
      labels = labels_vec,
      guide  = ggplot2::guide_axis(check.overlap = TRUE)
    ))

  } else if (x_is_numeric) {
    # ── Numérique
    all_x <- sort(unique(x_col))

    # Ordre personnalisé
    if (!is.null(custom_ord) && length(custom_ord) > 0) {
      co_num <- suppressWarnings(as.numeric(custom_ord))
      valid  <- co_num[!is.na(co_num) & co_num %in% all_x]
      if (length(valid) > 0) all_x <- valid
    }

    # Labels personnalisés pour numériques
    if (has_lm) {
      labels_vec <- sapply(as.character(all_x), function(v) {
        if (v %in% names(label_map) && as.character(label_map[[v]]) != v)
          as.character(label_map[[v]])
        else v
      }, USE.NAMES = FALSE)
    } else {
      labels_vec <- ggplot2::waiver()
    }

    return(ggplot2::scale_x_continuous(
      breaks = all_x,
      labels = labels_vec,
      guide  = ggplot2::guide_axis(check.overlap = TRUE)
    ))

  } else {
    # ── Facteur / texte
    if (is.factor(x_col)) {
      all_x <- levels(x_col)
    } else {
      all_x <- sort(unique(as.character(x_col)))
    }

    # Ordre personnalisé
    if (!is.null(custom_ord) && length(custom_ord) > 0) {
      ord_valid <- custom_ord[custom_ord %in% all_x]
      if (length(ord_valid) > 0) all_x <- ord_valid
    }

    return(ggplot2::scale_x_discrete(limits = all_x, drop = FALSE))
  }
}

# Paramètres pour les étiquettes de valeurs
#' Fonction pure : construire la liste de paramètres d'étiquettes.
#' @param size     numeric
#' @param color    character (hex)
#' @param bold     logical
#' @param italic   logical
#' @param digits   integer
#' @param position character : "above"|"below"|"center"|"right"|"left"
#' @return list(size, color, fontface, digits, vjust, hjust, plotly_textpos)
viz_label_params <- function(size     = 3,
                              color    = "#333333",
                              bold     = FALSE,
                              italic   = FALSE,
                              digits   = 2,
                              position = "above") {
  fontface <- if (bold && italic) "bold.italic"
              else if (bold)   "bold"
              else if (italic) "italic"
              else             "plain"

  vjust <- switch(position,
    "above"  = -0.5, "below"  =  1.5,
    "center" =  0.5, "right"  =  0.5, "left" = 0.5, -0.5)
  hjust <- switch(position, "right" = -0.2, "left" = 1.2, 0.5)
  plotly_pos <- switch(position,
    "above"  = "top center",    "below"  = "bottom center",
    "center" = "middle center", "right"  = "middle right",
    "left"   = "middle left",   "top center")

  list(size = size, color = color, fontface = fontface,
       digits = digits, vjust = vjust, hjust = hjust,
       plotly_textpos = plotly_pos)
}
