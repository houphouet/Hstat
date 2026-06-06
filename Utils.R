################################################################################
#
#             Encodage de l'application
#
################################################################################

Sys.setlocale("LC_ALL", "C")
options(encoding = "UTF-8")

# Limite de taille des fichiers televerses : 2 Go (defaut Shiny = 5 Mo)
options(shiny.maxRequestSize = 2 * 1024^3)

if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_CTYPE", "French_France.UTF-8")
} else {
  Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")
}


################################################################################
#
# Gestion des packages
#
################################################################################

install_and_load <- function(packages) {
  installed_packages <- rownames(installed.packages())
  to_install <- packages[!packages %in% installed_packages]

  if (length(to_install) > 0) {
    # Verifier la disponibilite d'une connexion avant de tenter l'installation
    online <- tryCatch({
      con <- url("https://cran.r-project.org", open = "rb")
      on.exit(close(con), add = TRUE)
      TRUE
    }, error = function(e) FALSE, warning = function(e) FALSE)

    if (online) {
      tryCatch(
        install.packages(to_install, repos = "https://cran.r-project.org"),
        error = function(e) message("Echec d'installation : ", conditionMessage(e)))
    } else {
      message("\n", strrep("=", 70),
              "\n  HStat -- mode hors-ligne detecte",
              "\n  Packages manquants : ", paste(to_install, collapse = ", "),
              "\n  Connectez-vous une fois a Internet pour les installer,",
              "\n  ou installez-les manuellement, puis relancez l'application.",
              "\n", strrep("=", 70), "\n")
    }
  }

  # Chargement : on n'interrompt pas l'application pour un package optionnel manquant
  missing_after <- character(0)
  for (pkg in packages) {
    ok <- suppressWarnings(suppressPackageStartupMessages(
      requireNamespace(pkg, quietly = TRUE)))
    if (ok) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    } else {
      missing_after <- c(missing_after, pkg)
    }
  }
  if (length(missing_after) > 0)
    message("HStat : packages indisponibles (certaines fonctions seront limitees) : ",
            paste(missing_after, collapse = ", "))
}

required_packages <- c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets", "shinyalert", "DT", "shinycssloaders",
  "RColorBrewer", "colourpicker", "ggrepel",  "openxlsx", "rmarkdown", "haven", "base64enc",
  "dplyr", "knitr", "stringr", "scales", "ggplot2", "ggdendro", "reshape2", "sortable",
  "tibble", "plotrix", "plotly",  "qqplotr", "tidyr",  "report", "see", "corrplot",
  "car", "agricolae","forcats", "bslib", "factoextra",  "FactoMineR","questionr",  "digest",
  "MASS", "cluster", "GGally", "psych", "nortest", "lmtest", "multcomp","FSA", "treemapify", "ggtext",
  "stats",  "emmeans", "performance","purrr", "PMCMRplus","multcompView", "rcompanion",
  "bestNormalize",
  "EMT",            # Test multinomial exact
  "lme4",           # Modeles lineaires (generalises) mixtes -- glmer/lmer
  "lmerTest",       # p-values et ddl (Satterthwaite) pour les LMM gaussiens
  "afex",           # ANOVA a mesures repetees (aov_ez / aov_car)
  "ARTool",         # ANOVA sur rangs alignes (art) -- non parametrique factoriel
  "glmmTMB",        # Modeles mixtes flexibles (zero-inflation, familles etendues)
  "vegan",          # PERMANOVA (adonis2) + betadisper
  "heplots",        # Box's M test
  "data.table",     # Lecture rapide de CSV (fread)
  "DBI", "duckdb"   # Moteur hors-memoire (out-of-core) pour gros fichiers
)


install_and_load(required_packages)

# Petit utilitaire
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Supprimer les colonnes à variance nulle
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

# Détecter les variables catégorielles
is_categorical <- function(x) {
  is.factor(x) || is.character(x) || inherits(x, "Date") || inherits(x, "POSIXt")
}

# Obtenir les colonnes catégorielles d'un dataframe
get_categorical_cols <- function(df) {
  names(df)[sapply(df, is_categorical)]
}

# Sélection des colonnes candidates pour être des facteurs
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

# Interprétation des p-values
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

# Interprétation des résultats statistiques
interpret_test_results <- function(test_type, p_value, test_object = NULL) {
  if (is.na(p_value)) return("Résultat non disponible")
  significance <- ifelse(p_value < 0.05, "significative", "non significative")
  switch(test_type,
         "t.test"           = paste0("Le test t montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "wilcox.test"      = paste0("Le test de Wilcoxon montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "anova"            = paste0("L'ANOVA montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "kruskal.test"     = paste0("Le test de Kruskal-Wallis montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "scheirerRayHare"  = paste0("Le test de Scheirer-Ray-Hare montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "manova"           = paste0("La MANOVA montre une différence multivariée ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "permanova"        = paste0("La PERMANOVA montre une différence multivariée ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "chisq.test"       = paste0("Le test du chi² montre une association ", significance, " entre les variables (p = ", round(p_value, 8), ")"),
         "cor.test"         = paste0("La corrélation est ", significance, " (p = ", round(p_value, 8), ")"),
         paste0("Le test ", test_type, " montre un résultat ", significance, " (p = ", round(p_value, 8), ")")
  )
}

# Interprétation de la normalité et l'homogénéité
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

interpret_normality_resid <- function(p_value) {
  if (is.na(p_value)) return("Test non applicable")
  if (p_value > 0.05) {
    return("Les résidus suivent une distribution normale (p > 0.05). Les conditions pour les tests paramétriques sont respectées.")
  } else {
    return("Les résidus ne suivent pas une distribution normale (p < 0.05). Considérez l'utilisation de tests non-paramétriques.")
  }
}

interpret_homogeneity_resid <- function(p_value) {
  if (is.na(p_value)) return("Test non applicable")
  if (p_value > 0.05) {
    return("Les variances sont homogènes (p > 0.05). Les conditions pour les tests paramétriques sont respectées.")
  } else {
    return("Les variances ne sont pas homogènes (p < 0.05). Utilisez des tests robustes à l'hétérogénéité des variances.")
  }
}

# Filtrage croisé complet (2 facteurs)
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

# Filtrage croisé complet (N facteurs)
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

# Coefficient de variation
calc_cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100


################################################################################
#
#  TRANSFORMATIONS DE VARIABLES
#  Applicable pour corriger les violations des hypothèses paramétriques
#
################################################################################

#' Retourne le libellé lisible d'une méthode de transformation
#' @param method character : code de la méthode
#' @return character : libellé affiché
get_transformation_label <- function(method) {
  switch(method,
         "log"        = "log(x) — Logarithme naturel",
         "log1p"      = "log(x+1) — Log avec zéros",
         "log10"      = "log10(x) — Log base 10",
         "sqrt"       = "sqrt(x) — Racine carrée",
         "cuberoot"   = "x^(1/3) — Racine cubique",
         "boxcox"     = "Box-Cox (λ optimal)",
         "yeojohnson" = "Yeo-Johnson (bestNormalize)",
         "arcsin"     = "asin(sqrt(x)) — Arcsinus",
         "logit"      = "log(p/(1-p)) — Logit",
         method
  )
}

#' Retourne le code mathématique affiché de la transformation
#' @param method character
#' @return character
get_transformation_formula <- function(method) {
  switch(method,
         "log"        = "log(x)",
         "log1p"      = "log(x + 1)",
         "log10"      = "log10(x)",
         "sqrt"       = "sqrt(x)",
         "cuberoot"   = "x^(1/3)",
         "boxcox"     = "(x^λ - 1) / λ",
         "yeojohnson" = "Yeo-Johnson(x)",
         "arcsin"     = "asin(sqrt(x))",
         "logit"      = "log(x / (1-x))",
         method
  )
}

#' Retourne les conditions d'application d'une transformation
#' @param method character
#' @return character
get_transformation_condition <- function(method) {
  switch(method,
         "log"        = "x > 0 (strictement positif)",
         "log1p"      = "x >= 0 (positif ou nul)",
         "log10"      = "x > 0 (strictement positif)",
         "sqrt"       = "x >= 0 (positif ou nul)",
         "cuberoot"   = "Toutes valeurs (accepte les négatifs)",
         "boxcox"     = "x > 0 (strictement positif) — λ estimé par MV",
         "yeojohnson" = "Toutes valeurs (accepte les négatifs)",
         "arcsin"     = "0 <= x <= 1 (proportions)",
         "logit"      = "0 < x < 1 (taux stricts)",
         method
  )
}

#' Applique une transformation à un vecteur numérique
#'
#' @param x        numeric vector — données originales (peut contenir des NA)
#' @param method   character — code de la méthode de transformation
#' @return         numeric vector de même longueur que x, avec attributs supplémentaires :
#'                   - attr(result, "lambda")    : λ Box-Cox estimé (si method = "boxcox")
#'                   - attr(result, "yj_object") : objet Yeo-Johnson (si method = "yeojohnson")
#'
apply_variable_transformation <- function(x, method) {
  x_nona <- x[!is.na(x)]
  
  result <- switch(method,
                   
                   # ── 1. Logarithme naturel ──────────────────────────────────────────────────
                   "log" = {
                     if (any(x_nona <= 0))
                       stop("log(x) requiert des valeurs strictement positives (x > 0).\n",
                            "Valeurs problématiques : ", sum(x_nona <= 0, na.rm = TRUE), " observation(s) <= 0.\n",
                            "Conseil : utilisez log(x+1) si vous avez des zéros.")
                     log(x)
                   },
                   
                   # ── 2. log(x+1) ───────────────────────────────────────────────────────────
                   "log1p" = {
                     if (any(x_nona < 0))
                       stop("log(x+1) requiert des valeurs >= 0 (x >= 0).\n",
                            "Valeurs négatives détectées : ", sum(x_nona < 0, na.rm = TRUE), " observation(s).")
                     log1p(x)
                   },
                   
                   # ── 3. Log base 10 ────────────────────────────────────────────────────────
                   "log10" = {
                     if (any(x_nona <= 0))
                       stop("log10(x) requiert des valeurs strictement positives (x > 0).\n",
                            "Valeurs problématiques : ", sum(x_nona <= 0, na.rm = TRUE), " observation(s) <= 0.")
                     log10(x)
                   },
                   
                   # ── 4. Racine carrée ──────────────────────────────────────────────────────
                   "sqrt" = {
                     if (any(x_nona < 0))
                       stop("sqrt(x) requiert des valeurs >= 0.\n",
                            "Valeurs négatives : ", sum(x_nona < 0, na.rm = TRUE), " observation(s).\n",
                            "Conseil : utilisez x^(1/3) si vous avez des valeurs négatives.")
                     sqrt(x)
                   },
                   
                   # ── 5. Racine cubique ─────────────────────────────────────────────────────
                   "cuberoot" = {
                     # sign() préserve le signe pour les négatifs
                     sign(x) * abs(x)^(1/3)
                   },
                   
                   # ── 6. Box-Cox ────────────────────────────────────────────────────────────
                   "boxcox" = {
                     if (any(x_nona <= 0))
                       stop("Box-Cox requiert des valeurs strictement positives (x > 0).\n",
                            sum(x_nona <= 0, na.rm = TRUE), " valeur(s) <= 0 détectée(s).\n",
                            "Conseil : si vous avez des zéros, utilisez Yeo-Johnson.")
                     # Estimation du λ optimal par maximum de vraisemblance
                     bc_fit  <- MASS::boxcox(x_nona ~ 1, plotit = FALSE, lambda = seq(-3, 3, by = 0.01))
                     lambda  <- bc_fit$x[which.max(bc_fit$y)]
                     # Transformation proprement dite
                     x_trans <- rep(NA_real_, length(x))
                     if (abs(lambda) < 1e-6) {
                       x_trans[!is.na(x)] <- log(x_nona)
                     } else {
                       x_trans[!is.na(x)] <- (x_nona^lambda - 1) / lambda
                     }
                     attr(x_trans, "lambda") <- round(lambda, 4)
                     x_trans
                   },
                   
                   # ── 7. Yeo-Johnson ────────────────────────────────────────────────────────
                   "yeojohnson" = {
                     yj_obj  <- bestNormalize::yeojohnson(x_nona, standardize = FALSE)
                     x_trans <- rep(NA_real_, length(x))
                     x_trans[!is.na(x)] <- predict(yj_obj, newdata = x_nona)
                     attr(x_trans, "yj_object") <- yj_obj
                     attr(x_trans, "lambda")    <- round(yj_obj$lambda, 4)
                     x_trans
                   },
                   
                   # ── 8. Arcsinus ───────────────────────────────────────────────────────────
                   "arcsin" = {
                     if (any(x_nona < 0 | x_nona > 1))
                       stop("asin(sqrt(x)) requiert des valeurs entre 0 et 1 (proportions/pourcentages en décimal).\n",
                            sum(x_nona < 0 | x_nona > 1, na.rm = TRUE), " valeur(s) hors [0,1].\n",
                            "Si vos données sont en %, divisez par 100 avant la transformation.")
                     asin(sqrt(x))
                   },
                   
                   # ── 9. Logit ──────────────────────────────────────────────────────────────
                   "logit" = {
                     if (any(x_nona <= 0 | x_nona >= 1))
                       stop("logit requiert des valeurs strictement entre 0 et 1 (0 et 1 exclus).\n",
                            sum(x_nona <= 0 | x_nona >= 1, na.rm = TRUE), " valeur(s) hors ]0,1[.\n",
                            "Conseil : si vos données incluent 0 ou 1, appliquez une correction : (x*(n-1)+0.5)/n.")
                     log(x / (1 - x))
                   },
                   
                   stop("Méthode de transformation inconnue : '", method, "'")
  )
  
  return(result)
}

#' Retro-transforme des valeurs vers l'échelle originale (pour l'affichage des moyennes PostHoc)
#'
#' @param x         numeric vector — valeurs sur l'échelle transformée
#' @param method    character — code de la méthode
#' @param lambda    numeric — λ Box-Cox ou Yeo-Johnson (si disponible)
#' @param yj_object objet yeojohnson de bestNormalize (pour inversion exacte)
#' @return          numeric vector sur l'échelle originale
#'
back_transform_values <- function(x, method, lambda = NULL, yj_object = NULL) {
  tryCatch({
    switch(method,
           "log"      = exp(x),
           "log1p"    = expm1(x),
           "log10"    = 10^x,
           "sqrt"     = x^2,
           "cuberoot" = x^3,
           "arcsin"   = sin(x)^2,
           "logit"    = exp(x) / (1 + exp(x)),
           "boxcox" = {
             if (is.null(lambda)) return(x)
             if (abs(lambda) < 1e-6) exp(x) else (lambda * x + 1)^(1 / lambda)
           },
           "yeojohnson" = {
             if (!is.null(yj_object)) {
               predict(yj_object, newdata = x, inverse = TRUE)
             } else {
               x  # fallback si l'objet n'est pas disponible
             }
           },
           x  # default : pas de retro-transformation
    )
  }, error = function(e) x)
}

#' Vérifie si une transformation est applicable sur un vecteur
#' Retourne une liste : list(ok = TRUE/FALSE, message = "...")
#'
check_transformation_feasibility <- function(x, method) {
  x_nona <- x[!is.na(x)]
  n <- length(x_nona)
  if (n == 0) return(list(ok = FALSE, message = "Aucune valeur non-NA disponible."))
  
  issues <- switch(method,
                   "log"     = if (any(x_nona <= 0)) paste(sum(x_nona <= 0), "valeur(s) <= 0 détectée(s)") else NULL,
                   "log1p"   = if (any(x_nona < 0))  paste(sum(x_nona < 0),  "valeur(s) < 0 détectée(s)")  else NULL,
                   "log10"   = if (any(x_nona <= 0)) paste(sum(x_nona <= 0), "valeur(s) <= 0 détectée(s)") else NULL,
                   "sqrt"    = if (any(x_nona < 0))  paste(sum(x_nona < 0),  "valeur(s) < 0 détectée(s)")  else NULL,
                   "cuberoot" = NULL,  # toujours applicable
                   "boxcox"  = if (any(x_nona <= 0)) paste(sum(x_nona <= 0), "valeur(s) <= 0 (Box-Cox nécessite x > 0)") else NULL,
                   "yeojohnson" = NULL,  # toujours applicable
                   "arcsin"  = if (any(x_nona < 0 | x_nona > 1)) paste(sum(x_nona < 0 | x_nona > 1), "valeur(s) hors [0,1]") else NULL,
                   "logit"   = if (any(x_nona <= 0 | x_nona >= 1)) paste(sum(x_nona <= 0 | x_nona >= 1), "valeur(s) hors ]0,1[") else NULL,
                   NULL
  )
  
  if (!is.null(issues)) {
    list(ok = FALSE, message = issues)
  } else {
    list(ok = TRUE, message = paste0("Applicable (n = ", n, ")"))
  }
}


################################################################################
#  Fonctions utilitaires — Visualisation des données
################################################################################

VIZ_DATE_FORMATS_VALID <- c(
  "%d-%m-%Y", "%m-%d-%Y", "%Y-%m-%d", "%Y-%d-%m",
  "%d/%m/%Y", "%m/%d/%Y",
  "%d-%m",    "%m-%d",    "%m-%Y",    "%Y-%m",
  "%d-%b-%Y", "%b-%Y",    "%d-%b",    "%b-%d",    "%Y-%b-%d",
  "%d %B %Y", "%B %Y",    "%d %B",    "%B %d",    "%Y %B"
)

viz_valid_date_fmt <- function(fmt) {
  !is.null(fmt) && nzchar(trimws(fmt)) && fmt %in% VIZ_DATE_FORMATS_VALID
}

viz_detect_x_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("date")
  if (is.factor(x))    return("factor")
  if (is.numeric(x))   return("numeric")
  if (is.character(x))
    return(if (length(unique(x)) < length(x) / 2) "categorical" else "text")
  "text"
}

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

viz_get_x_scale <- function(x_col, disp_fmt = "%d-%m-%Y", label_map = NULL, custom_ord = NULL) {
  if (inherits(x_col, "AsIs")) {
    cls <- class(x_col)
    cls <- cls[cls != "AsIs"]
    x_col <- unclass(x_col)
    class(x_col) <- cls
  }
  x_is_date    <- inherits(x_col, c("Date", "POSIXct", "POSIXlt"))
  x_is_numeric <- is.numeric(x_col) && !x_is_date
  has_lm <- !is.null(label_map) && length(label_map) > 0 &&
    any(as.character(label_map) != names(label_map))
  
  if (x_is_date) {
    all_x <- sort(unique(x_col))
    if (!is.null(custom_ord) && length(custom_ord) > 0) {
      co_dates <- tryCatch(as.Date(custom_ord), error = function(e) NULL)
      if (!is.null(co_dates)) {
        valid_ord <- co_dates[!is.na(co_dates) & co_dates %in% all_x]
        if (length(valid_ord) > 0) all_x <- valid_ord
      }
    }
    labels_vec <- sapply(as.character(all_x), function(v) {
      if (has_lm && v %in% names(label_map) && as.character(label_map[[v]]) != v)
        as.character(label_map[[v]])
      else
        tryCatch(format(as.Date(v), disp_fmt), error = function(e) v)
    }, USE.NAMES = FALSE)
    return(ggplot2::scale_x_date(
      breaks = all_x, labels = labels_vec,
      guide  = ggplot2::guide_axis(check.overlap = TRUE)
    ))
  } else if (x_is_numeric) {
    all_x <- sort(unique(x_col))
    if (!is.null(custom_ord) && length(custom_ord) > 0) {
      co_num <- suppressWarnings(as.numeric(custom_ord))
      valid  <- co_num[!is.na(co_num) & co_num %in% all_x]
      if (length(valid) > 0) all_x <- valid
    }
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
      breaks = all_x, labels = labels_vec,
      guide  = ggplot2::guide_axis(check.overlap = TRUE)
    ))
  } else {
    if (is.factor(x_col)) {
      all_x <- levels(x_col)
    } else {
      all_x <- sort(unique(as.character(x_col)))
    }
    if (!is.null(custom_ord) && length(custom_ord) > 0) {
      ord_valid <- custom_ord[custom_ord %in% all_x]
      if (length(ord_valid) > 0) all_x <- ord_valid
    }
    return(ggplot2::scale_x_discrete(limits = all_x, drop = FALSE))
  }
}

viz_label_params <- function(size = 3, color = "#333333", bold = FALSE,
                             italic = FALSE, digits = 2, position = "above") {
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

################################################################################
#
#  MANOVA — Fonctions utilitaires (paramétrique + non paramétrique)
#
#  Couvre :
#    - Vérification des prérequis multivariés (n, p, completeness)
#    - Normalité multivariée (Mardia via psych)
#    - Homogénéité des matrices de covariance (Box's M via heplots)
#    - Homogénéité multivariée des dispersions (PERMDISP via vegan::betadisper)
#    - Mise en forme des résultats parametric (4 statistiques)
#    - PERMANOVA (vegan::adonis2) + pairwise PERMANOVA
#    - Décomposition univariée post-hoc (ANOVA / Kruskal) avec ajustement
#    - Interprétations textuelles
#
################################################################################

#' Vérifie que les données sont utilisables pour une MANOVA
#' @param df         data.frame
#' @param response   character — variables réponses (>= 2)
#' @param factors    character — facteurs (>= 1)
#' @return           list(ok, message, df_clean, n, p, k)
check_manova_data <- function(df, response, factors) {
  if (length(response) < 2)
    return(list(ok = FALSE, message = "MANOVA nécessite au moins 2 variables réponses."))
  if (length(factors) < 1)
    return(list(ok = FALSE, message = "MANOVA nécessite au moins 1 facteur."))
  
  keep <- c(response, factors)
  df2  <- df[, keep, drop = FALSE]
  
  # Toutes les réponses doivent être numériques
  for (v in response) {
    if (!is.numeric(df2[[v]])) df2[[v]] <- suppressWarnings(as.numeric(df2[[v]]))
  }
  
  # Conversion facteurs (texte/date/numérique <= 30 niveaux)
  for (f in factors) {
    if (!is.factor(df2[[f]])) {
      df2[[f]] <- tryCatch({
        if (inherits(df2[[f]], c("Date","POSIXct","POSIXlt")))
          factor(format(df2[[f]], "%Y-%m-%d"))
        else
          factor(as.character(df2[[f]]))
      }, error = function(e) factor(df2[[f]]))
    }
    df2[[f]] <- droplevels(df2[[f]])
  }
  
  df2 <- df2[stats::complete.cases(df2), , drop = FALSE]
  
  n <- nrow(df2)
  p <- length(response)
  if (n < (p + 3))
    return(list(ok = FALSE,
                message = paste0("Trop peu d'observations complètes (n=", n,
                                 ") pour ", p, " variables réponses.")))
  
  # Variance non nulle pour chaque réponse globalement et par groupe
  zero_var <- vapply(response, function(v) {
    stats::sd(df2[[v]], na.rm = TRUE) == 0 ||
      is.na(stats::sd(df2[[v]], na.rm = TRUE))
  }, logical(1))
  if (any(zero_var))
    return(list(ok = FALSE,
                message = paste0("Variance nulle pour : ",
                                 paste(response[zero_var], collapse = ", "))))
  
  # Au moins 2 niveaux par facteur, et chaque cellule >= 2 obs
  for (f in factors) {
    if (nlevels(df2[[f]]) < 2)
      return(list(ok = FALSE,
                  message = paste0("Le facteur '", f, "' a moins de 2 niveaux après nettoyage.")))
  }
  
  list(ok = TRUE, message = "Données valides",
       df_clean = df2, n = n, p = p, k = length(factors))
}


#' Test de normalité multivariée de Mardia (skewness + kurtosis)
#' @param Y matrix/data.frame numérique
#' @return list(skewness, p.skewness, kurtosis, p.kurtosis, n, p, conclusion)
multivariate_normality_mardia <- function(Y) {
  Y <- as.matrix(Y)
  Y <- Y[stats::complete.cases(Y), , drop = FALSE]
  n <- nrow(Y); p <- ncol(Y)
  
  if (n < 8 || p < 2) {
    return(list(method = "Mardia",
                skewness = NA_real_, p.skewness = NA_real_,
                kurtosis = NA_real_, p.kurtosis = NA_real_,
                n = n, p = p,
                conclusion = "Échantillon trop petit pour Mardia (n < 8 ou p < 2)"))
  }
  
  res <- tryCatch(suppressWarnings(psych::mardia(Y, plot = FALSE)),
                  error = function(e) NULL)
  if (is.null(res))
    return(list(method = "Mardia",
                skewness = NA_real_, p.skewness = NA_real_,
                kurtosis = NA_real_, p.kurtosis = NA_real_,
                n = n, p = p,
                conclusion = "Test de Mardia indisponible"))
  
  ok_skew <- isTRUE(res$p.skew >= 0.05)
  ok_kurt <- isTRUE(res$p.kurt >= 0.05)
  concl <- if (ok_skew && ok_kurt)
    "Normalité multivariée plausible (Mardia : p.skew >= 0.05 et p.kurt >= 0.05)"
  else if (!ok_skew && !ok_kurt)
    "Violation de normalité multivariée (skewness ET kurtosis significatifs)"
  else if (!ok_skew)
    "Violation par asymétrie multivariée (Mardia p.skew < 0.05)"
  else
    "Violation par aplatissement multivarié (Mardia p.kurt < 0.05)"
  
  list(method = "Mardia",
       skewness   = as.numeric(res$skew),
       p.skewness = as.numeric(res$p.skew),
       kurtosis   = as.numeric(res$kurtosis),
       p.kurtosis = as.numeric(res$p.kurt),
       n = n, p = p,
       conclusion = concl)
}


#' Test de Box's M (homogénéité des matrices de covariance entre groupes)
#' @param Y matrix/data.frame numérique
#' @param group facteur (1 seul facteur)
#' @return list(chi2, df, p.value, conclusion)
box_m_test <- function(Y, group) {
  Y <- as.matrix(Y); group <- as.factor(group)
  ok <- stats::complete.cases(Y) & !is.na(group)
  Y <- Y[ok, , drop = FALSE]; group <- droplevels(group[ok])
  
  if (nlevels(group) < 2 || nrow(Y) < 5 || ncol(Y) < 2)
    return(list(chi2 = NA_real_, df = NA_real_, p.value = NA_real_,
                conclusion = "Test impossible (< 2 groupes / trop peu d'obs / p < 2)"))
  
  min_per_group <- min(table(group))
  if (min_per_group < ncol(Y) + 1)
    return(list(chi2 = NA_real_, df = NA_real_, p.value = NA_real_,
                conclusion = paste0("Groupes trop petits pour Box's M (min n=",
                                    min_per_group, " < p+1=", ncol(Y) + 1, ")")))
  
  # Vérifier que chaque matrice de covariance de groupe est de rang plein.
  # On teste le RANG (via qr) et non le déterminant brut : le déterminant
  # dépend de l'échelle des variables et peut être légitimement très petit.
  p <- ncol(Y)
  rank_ok <- vapply(split(seq_len(nrow(Y)), group), function(idx) {
    sub <- Y[idx, , drop = FALSE]
    cov_sub <- tryCatch(stats::cov(sub), error = function(e) NULL)
    if (is.null(cov_sub) || any(!is.finite(cov_sub))) return(FALSE)
    rg <- tryCatch(qr(cov_sub)$rank, error = function(e) NA_integer_)
    !is.na(rg) && rg >= p
  }, logical(1))
  
  if (any(!rank_ok))
    return(list(chi2 = NA_real_, df = NA_real_, p.value = NA_real_,
                conclusion = "Matrice de covariance singulière dans au moins un groupe (variables colinéaires ou n trop petit) -- Box's M non applicable"))
  
  res <- tryCatch(
    withCallingHandlers(
      heplots::boxM(Y, group),
      warning = function(w) {
        if (grepl("NaN|log|det", conditionMessage(w), ignore.case = TRUE))
          invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  if (is.null(res) || is.na(res$p.value))
    return(list(chi2 = NA_real_, df = NA_real_, p.value = NA_real_,
                conclusion = "Test de Box's M indisponible (matrices mal conditionnées)"))
  
  concl <- if (isTRUE(res$p.value >= 0.05))
    "Homogénéité des matrices de covariance OK (Box's M p >= 0.05)"
  else
    "Violation d'homogénéité (Box's M p < 0.05) — privilégier Pillai (robuste)"
  
  list(chi2 = unname(res$statistic),
       df = unname(res$parameter),
       p.value = unname(res$p.value),
       conclusion = concl)
}


#' PERMDISP — homogénéité multivariée des dispersions (vegan::betadisper)
#' Équivalent multivarié non paramétrique du test de Levene
#' @param Y matrix de réponses
#' @param group facteur
#' @param dist_method "euclidean" (par défaut) ou autre
#' @return list(F, df1, df2, p.value, conclusion)
permdisp_test <- function(Y, group, dist_method = "euclidean") {
  Y <- as.matrix(Y); group <- as.factor(group)
  ok <- stats::complete.cases(Y) & !is.na(group)
  Y <- Y[ok, , drop = FALSE]; group <- droplevels(group[ok])
  
  if (nlevels(group) < 2 || nrow(Y) < 5)
    return(list(F = NA_real_, df1 = NA_real_, df2 = NA_real_,
                p.value = NA_real_, conclusion = "Test impossible"))
  
  res <- tryCatch({
    d  <- vegan::vegdist(Y, method = dist_method)
    bd <- vegan::betadisper(d, group)
    pa <- vegan::permutest(bd, permutations = 999)
    list(F   = pa$tab[1, "F"],
         df1 = pa$tab[1, "Df"],
         df2 = pa$tab[2, "Df"],
         p   = pa$tab[1, "Pr(>F)"])
  }, error = function(e) NULL)
  
  if (is.null(res))
    return(list(F = NA_real_, df1 = NA_real_, df2 = NA_real_,
                p.value = NA_real_, conclusion = "PERMDISP indisponible"))
  
  concl <- if (isTRUE(res$p >= 0.05))
    "Dispersions multivariées homogènes (PERMDISP p >= 0.05)"
  else
    "Dispersions multivariées hétérogènes (PERMDISP p < 0.05) — interpréter PERMANOVA avec prudence"
  
  list(F = res$F, df1 = res$df1, df2 = res$df2,
       p.value = res$p, conclusion = concl)
}


#' Format les 4 statistiques MANOVA en data.frame "wide"
#' @param fit modèle manova
#' @return data.frame avec colonnes Effet, ddl_num, ddl_den + 4 blocs (stat/F/p)
manova_format_all_stats <- function(fit) {
  pillai <- summary(fit, test = "Pillai")$stats
  wilks  <- summary(fit, test = "Wilks")$stats
  hotell <- summary(fit, test = "Hotelling-Lawley")$stats
  roy    <- summary(fit, test = "Roy")$stats
  
  effects <- rownames(pillai)
  effects <- effects[effects != "Residuals"]
  
  do.call(rbind, lapply(effects, function(eff) {
    data.frame(
      Effet         = eff,
      ddl_num       = pillai[eff, "Df"],
      ddl_den       = pillai["Residuals", "Df"],
      Pillai        = pillai[eff, "Pillai"],
      F_Pillai      = pillai[eff, "approx F"],
      p_Pillai      = pillai[eff, "Pr(>F)"],
      Wilks         = wilks[eff,  "Wilks"],
      F_Wilks       = wilks[eff,  "approx F"],
      p_Wilks       = wilks[eff,  "Pr(>F)"],
      Hotelling     = hotell[eff, "Hotelling-Lawley"],
      F_Hotelling   = hotell[eff, "approx F"],
      p_Hotelling   = hotell[eff, "Pr(>F)"],
      Roy           = roy[eff,    "Roy"],
      F_Roy         = roy[eff,    "approx F"],
      p_Roy         = roy[eff,    "Pr(>F)"],
      stringsAsFactors = FALSE
    )
  }))
}


#' Tailles d'effet multivariées (eta² partiel) à partir de Wilks
#' Formule : eta²_partiel = 1 - Wilks^(1/s)  où s = min(p, ddl_effet)
#' @param df data.frame produit par manova_format_all_stats
#' @param p nombre de réponses
manova_effect_sizes <- function(df, p) {
  s <- pmin(p, df$ddl_num)
  df$eta2_partial <- 1 - df$Wilks^(1 / s)
  df$eta2_pillai  <- df$Pillai / s   # eta² partiel basé sur Pillai
  df
}


#' Interprétation textuelle d'un effet MANOVA paramétrique
#' @param p_pillai  p-value de Pillai
#' @param eta2      eta² partiel (optionnel)
#' @return character
interpret_manova_effect <- function(p_pillai, eta2 = NA) {
  if (is.na(p_pillai)) return("Résultat non disponible")
  sig <- if (p_pillai < 0.05) "significatif" else "non significatif"
  base <- paste0("Effet multivarié ", sig, " (Pillai, p = ", round(p_pillai, 6), ")")
  if (!is.na(eta2)) {
    mag <- if (eta2 < 0.01) "négligeable"
    else if (eta2 < 0.06) "faible"
    else if (eta2 < 0.14) "modéré"
    else "important"
    base <- paste0(base, " — taille d'effet ", mag, " (eta² = ", round(eta2, 3), ")")
  }
  base
}


#' Interprétation textuelle d'un effet PERMANOVA
interpret_permanova_effect <- function(p_value, R2 = NA) {
  if (is.na(p_value)) return("Résultat non disponible")
  sig <- if (p_value < 0.05) "significatif" else "non significatif"
  base <- paste0("Effet multivarié ", sig, " (PERMANOVA, p = ", round(p_value, 6), ")")
  if (!is.na(R2)) {
    mag <- if (R2 < 0.01) "négligeable"
    else if (R2 < 0.06) "faible"
    else if (R2 < 0.14) "modéré"
    else "important"
    base <- paste0(base, " — R² = ", round(R2, 3), " (", mag, ")")
  }
  base
}


#' PERMANOVA pairwise sur les niveaux d'un facteur
#' Implémentation manuelle (adonis2 sur chaque paire) avec correction de p-values
#' @param Y matrice de réponses
#' @param group facteur (1 seul)
#' @param permutations nombre de permutations
#' @param dist_method  méthode de distance (par défaut "euclidean")
#' @param p_adjust     "bonferroni" (défaut), "holm", "BH", "fdr"...
#' @return data.frame avec colonnes : Niveau1, Niveau2, n1, n2, F, R2, p_value, p_adj
pairwise_permanova <- function(Y, group, permutations = 999,
                               dist_method = "euclidean",
                               p_adjust = "bonferroni") {
  Y <- as.matrix(Y); group <- as.factor(group)
  ok <- stats::complete.cases(Y) & !is.na(group)
  Y <- Y[ok, , drop = FALSE]; group <- droplevels(group[ok])
  
  levs  <- levels(group)
  pairs <- utils::combn(levs, 2, simplify = FALSE)
  
  out <- do.call(rbind, lapply(pairs, function(pr) {
    idx  <- group %in% pr
    Yp   <- Y[idx, , drop = FALSE]
    gp   <- droplevels(group[idx])
    if (length(unique(gp)) < 2 || nrow(Yp) < 4) {
      return(data.frame(Niveau1 = pr[1], Niveau2 = pr[2],
                        n1 = sum(group == pr[1]),
                        n2 = sum(group == pr[2]),
                        F  = NA_real_, R2 = NA_real_, p_value = NA_real_))
    }
    res <- tryCatch({
      d  <- vegan::vegdist(Yp, method = dist_method)
      a  <- vegan::adonis2(d ~ gp, permutations = permutations, by = "terms")
      data.frame(Niveau1 = pr[1], Niveau2 = pr[2],
                 n1 = sum(gp == pr[1]), n2 = sum(gp == pr[2]),
                 F  = a$F[1], R2 = a$R2[1], p_value = a$`Pr(>F)`[1])
    }, error = function(e) {
      data.frame(Niveau1 = pr[1], Niveau2 = pr[2],
                 n1 = sum(group == pr[1]), n2 = sum(group == pr[2]),
                 F = NA_real_, R2 = NA_real_, p_value = NA_real_)
    })
    res
  }))
  
  out$p_adj <- stats::p.adjust(out$p_value, method = p_adjust)
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


#' Décomposition univariée d'une MANOVA paramétrique
#' Lance une ANOVA sur chaque Y (avec mêmes facteurs/interaction)
#' Applique un ajustement Bonferroni cross-réponses sur les p-values
#' @param df data.frame (déjà nettoyé)
#' @param response character — variables réponses
#' @param factors  character — facteurs
#' @param interaction logical
#' @param p_adjust "bonferroni" (défaut)
#' @return data.frame : Reponse, Effet, ddl, F, p_value, p_adj, eta2_partial, Significatif
manova_univariate_followup <- function(df, response, factors, interaction = FALSE,
                                       p_adjust = "bonferroni") {
  rhs <- paste(sapply(factors, function(x) paste0("`", x, "`")),
               collapse = ifelse(isTRUE(interaction), "*", "+"))
  res_all <- list()
  for (v in response) {
    fml <- stats::as.formula(paste0("`", v, "` ~ ", rhs))
    fit <- tryCatch(stats::aov(fml, data = df), error = function(e) NULL)
    if (is.null(fit)) next
    tab <- summary(fit)[[1]]
    eff <- rownames(tab); eff <- trimws(eff)
    is_resid <- eff == "Residuals"
    if (!any(is_resid)) next
    ss_resid <- tab[is_resid, "Sum Sq"]
    df_resid <- tab[is_resid, "Df"]
    
    for (i in which(!is_resid)) {
      eta2 <- tab[i, "Sum Sq"] / (tab[i, "Sum Sq"] + ss_resid)
      res_all[[paste(v, eff[i], sep = "_")]] <- data.frame(
        Reponse  = v,
        Effet    = eff[i],
        ddl      = paste0(tab[i, "Df"], ", ", df_resid),
        F_stat   = tab[i, "F value"],
        p_value  = tab[i, "Pr(>F)"],
        eta2_partial = eta2,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(res_all) == 0) return(NULL)
  out <- do.call(rbind, res_all); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = p_adjust)
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


#' Décomposition univariée non paramétrique (Kruskal-Wallis) pour PERMANOVA
#' Lance KW sur chaque Y x facteur (effets simples, un facteur à la fois)
#' Ajustement Bonferroni cross-réponses x facteurs
manova_univariate_followup_np <- function(df, response, factors,
                                          p_adjust = "bonferroni") {
  res_all <- list()
  for (v in response) {
    for (f in factors) {
      fml <- stats::as.formula(paste0("`", v, "` ~ `", f, "`"))
      kw  <- tryCatch(stats::kruskal.test(fml, data = df), error = function(e) NULL)
      if (is.null(kw)) next
      # eta² estimé via H / (n-1)
      n   <- nrow(df[stats::complete.cases(df[, c(v, f)]), , drop = FALSE])
      eta2_kw <- (kw$statistic - length(unique(df[[f]])) + 1) / (n - length(unique(df[[f]])))
      eta2_kw <- max(0, as.numeric(eta2_kw))
      res_all[[paste(v, f, sep = "_")]] <- data.frame(
        Reponse  = v,
        Facteur  = f,
        H_stat   = as.numeric(kw$statistic),
        ddl      = as.numeric(kw$parameter),
        p_value  = kw$p.value,
        eta2_KW  = eta2_kw,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(res_all) == 0) return(NULL)
  out <- do.call(rbind, res_all); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = p_adjust)
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


#' Comparaisons par paires sur les niveaux d'un facteur — univarié paramétrique
#' Tukey HSD appliqué à chaque variable réponse, ajustement Bonferroni cross-réponses
#' @param df       data.frame nettoyé
#' @param response character — variables réponses
#' @param factor   character — UN facteur
#' @return data.frame : Reponse, Comparaison, Diff, IC_inf, IC_sup, p_value, p_adj
manova_pairwise_univariate <- function(df, response, factor_name,
                                       p_adjust = "bonferroni") {
  if (length(factor_name) != 1) return(NULL)
  fvar <- factor_name
  res_all <- list()
  for (v in response) {
    fml <- stats::as.formula(paste0("`", v, "` ~ `", fvar, "`"))
    fit <- tryCatch(stats::aov(fml, data = df), error = function(e) NULL)
    if (is.null(fit)) next
    tk <- tryCatch(stats::TukeyHSD(fit, fvar), error = function(e) NULL)
    if (is.null(tk)) next
    tab <- tk[[1]]
    res_all[[v]] <- data.frame(
      Reponse     = v,
      Comparaison = rownames(tab),
      Diff        = tab[, "diff"],
      IC_inf      = tab[, "lwr"],
      IC_sup      = tab[, "upr"],
      p_value     = tab[, "p adj"],
      stringsAsFactors = FALSE
    )
  }
  if (length(res_all) == 0) return(NULL)
  out <- do.call(rbind, res_all); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = p_adjust)
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


#' Comparaisons par paires univariées non paramétriques (Dunn)
manova_pairwise_univariate_np <- function(df, response, factor_name,
                                          p_adjust = "bonferroni") {
  if (length(factor_name) != 1) return(NULL)
  fvar <- factor_name
  res_all <- list()
  for (v in response) {
    sub <- df[stats::complete.cases(df[, c(v, fvar)]), c(v, fvar), drop = FALSE]
    if (nrow(sub) < 4 || nlevels(droplevels(as.factor(sub[[fvar]]))) < 2) next
    sub[[fvar]] <- droplevels(as.factor(sub[[fvar]]))
    dn <- tryCatch(
      FSA::dunnTest(sub[[v]], sub[[fvar]], method = "bonferroni"),
      error = function(e) NULL
    )
    if (is.null(dn)) next
    tab <- dn$res
    res_all[[v]] <- data.frame(
      Reponse     = v,
      Comparaison = as.character(tab$Comparison),
      Z_stat      = as.numeric(tab$Z),
      p_value     = as.numeric(tab$P.unadj),
      p_dunn_bonf = as.numeric(tab$P.adj),
      stringsAsFactors = FALSE
    )
  }
  if (length(res_all) == 0) return(NULL)
  out <- do.call(rbind, res_all); rownames(out) <- NULL
  # Ajustement cross-réponses (en plus du Dunn intra-réponse déjà bonferroni)
  out$p_adj        <- stats::p.adjust(out$p_value, method = p_adjust)
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


#' Box's M par facteur (applique box_m_test à chaque facteur d'un design)
#' @return data.frame : Facteur, Chi2, ddl, p_value, Conclusion (NULL si aucun)
boxm_per_factor <- function(Y, df, factors) {
  rows <- lapply(factors, function(f) {
    bm <- box_m_test(Y, df[[f]])
    data.frame(
      Facteur    = f,
      Chi2       = bm$chi2,
      ddl        = bm$df,
      p_value    = bm$p.value,
      Conclusion = bm$conclusion,
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}


#' PERMDISP par facteur (applique permdisp_test à chaque facteur d'un design)
#' @return data.frame : Facteur, F_stat, ddl1, ddl2, p_value, Conclusion (NULL si aucun)
permdisp_per_factor <- function(Y, df, factors, dist_method = "euclidean") {
  rows <- lapply(factors, function(f) {
    pd <- permdisp_test(Y, df[[f]], dist_method = dist_method)
    data.frame(
      Facteur    = f,
      F_stat     = pd$F,
      ddl1       = pd$df1,
      ddl2       = pd$df2,
      p_value    = pd$p.value,
      Conclusion = pd$conclusion,
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}


#' Construit la matrice de p-values à partir d'un data.frame de paires
#' (utilisé pour générer les lettres CLD via multcompView)
#' @param pairs_df data.frame avec colonnes Niveau1, Niveau2, p_adj
#' @param levels   vecteur des niveaux du facteur
#' @return matrix carrée de p-values (diagonale = 1)
build_pvalue_matrix <- function(pairs_df, levels) {
  pmat <- matrix(NA_real_, length(levels), length(levels),
                 dimnames = list(levels, levels))
  for (i in seq_len(nrow(pairs_df))) {
    a <- pairs_df$Niveau1[i]; b <- pairs_df$Niveau2[i]
    pmat[a, b] <- pairs_df$p_adj[i]
    pmat[b, a] <- pairs_df$p_adj[i]
  }
  diag(pmat) <- 1
  pmat
}


#' Construit le data.frame des lettres CLD pour un facteur
#' Si Y est fourni, ajoute des colonnes Moyenne ± Ecart-type et Moyenne ± Erreur-type
#' (calculees sur la norme multivariee : sqrt(sum(Yi^2)) pour chaque observation).
#' @param pairs_df output de pairwise_permanova
#' @param group    facteur
#' @param Y        (optionnel) matrice des reponses pour calculer moyennes par groupe
#' @param digits   nombre de decimales pour le formatage (defaut : 3)
#' @return data.frame : Niveau, N, Groupes [, Moyenne_pm_SD, Moyenne_pm_SE]
build_letters_df <- function(pairs_df, group, Y = NULL, digits = 3) {
  group <- as.factor(group)
  levs  <- levels(group)
  if (is.null(pairs_df) || nrow(pairs_df) == 0) return(NULL)
  pmat <- build_pvalue_matrix(pairs_df, levs)
  cld <- tryCatch(multcompView::multcompLetters(pmat, threshold = 0.05)$Letters,
                  error = function(e) stats::setNames(rep("a", length(levs)), levs))
  n_per <- as.numeric(table(group))[match(levs, names(table(group)))]
  out <- data.frame(
    Niveau   = levs,
    N        = n_per,
    Groupes  = as.character(cld[levs]),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(Y)) {
    Y <- as.matrix(Y)
    score <- sqrt(rowSums(Y^2))
    means <- vapply(levs, function(lv) mean(score[group == lv], na.rm = TRUE), numeric(1))
    sds   <- vapply(levs, function(lv) stats::sd(score[group == lv], na.rm = TRUE), numeric(1))
    ses   <- sds / sqrt(pmax(n_per, 1))
    fmt   <- function(m, s) ifelse(is.na(m) | is.na(s), "NA",
                                   paste0(formatC(m, digits = digits, format = "f"),
                                          " \u00b1 ",
                                          formatC(s, digits = digits, format = "f")))
    out$`Moyenne_pm_SD` <- paste0(fmt(means, sds), " ", out$Groupes)
    out$`Moyenne_pm_SE` <- paste0(fmt(means, ses), " ", out$Groupes)
  }
  out
}


#' PostHoc univarie par variable reponse (decomposition d'un posthoc multivarie)
#'
#' Pour chaque variable reponse, lance un posthoc adapte :
#' - methode parametrique  -> ANOVA + Tukey HSD
#' - methode non parametrique -> Kruskal-Wallis + Dunn (Bonferroni)
#' puis derive les lettres CLD propres a cette variable.
#'
#' @param df       data.frame nettoye
#' @param response variables reponses (vecteur)
#' @param factor_name un seul facteur
#' @param parametric TRUE = ANOVA/Tukey, FALSE = Kruskal/Dunn
#' @param digits   decimales d'affichage
#' @return data.frame long : Variable, Niveau, N, Moyenne, Ecart_type, Erreur_type,
#'         Groupes, Moyenne_pm_SD, Moyenne_pm_SE
build_letters_per_variable <- function(df, response, factor_name,
                                       parametric = TRUE, digits = 3) {
  if (length(factor_name) != 1) return(NULL)
  fvar <- factor_name
  df <- df[stats::complete.cases(df[, c(response, fvar), drop = FALSE]), , drop = FALSE]
  if (!is.factor(df[[fvar]])) df[[fvar]] <- factor(as.character(df[[fvar]]))
  df[[fvar]] <- droplevels(df[[fvar]])
  levs <- levels(df[[fvar]])
  if (length(levs) < 2) return(NULL)
  
  fmt_val <- function(m, s) ifelse(is.na(m) | is.na(s), "NA",
                                   paste0(formatC(m, digits = digits, format = "f"),
                                          " \u00b1 ",
                                          formatC(s, digits = digits, format = "f")))
  
  rows <- list()
  for (v in response) {
    y  <- df[[v]]
    g  <- df[[fvar]]
    n_per   <- as.numeric(table(g))[match(levs, levels(g))]
    means   <- vapply(levs, function(lv) mean(y[g == lv], na.rm = TRUE), numeric(1))
    sds     <- vapply(levs, function(lv) stats::sd(y[g == lv], na.rm = TRUE), numeric(1))
    ses     <- sds / sqrt(pmax(n_per, 1))
    
    # Lettres CLD pour cette variable
    letters_v <- tryCatch({
      if (parametric) {
        fit  <- stats::aov(y ~ g)
        tuk  <- stats::TukeyHSD(fit)[[1]]
        pv   <- tuk[, "p adj"]
        nm   <- rownames(tuk)
        pmat <- matrix(1, length(levs), length(levs), dimnames = list(levs, levs))
        for (i in seq_along(nm)) {
          pair <- strsplit(nm[i], "-", fixed = TRUE)[[1]]
          if (length(pair) == 2 && all(pair %in% levs)) {
            pmat[pair[1], pair[2]] <- pv[i]
            pmat[pair[2], pair[1]] <- pv[i]
          }
        }
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      } else {
        kt <- stats::kruskal.test(y ~ g)
        dn <- FSA::dunnTest(y, g, method = "bonferroni")$res
        pmat <- matrix(1, length(levs), length(levs), dimnames = list(levs, levs))
        for (i in seq_len(nrow(dn))) {
          pair <- trimws(strsplit(as.character(dn$Comparison[i]), "-", fixed = TRUE)[[1]])
          if (length(pair) == 2 && all(pair %in% levs)) {
            pmat[pair[1], pair[2]] <- dn$P.adj[i]
            pmat[pair[2], pair[1]] <- dn$P.adj[i]
          }
        }
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      }
    }, error = function(e) stats::setNames(rep("a", length(levs)), levs))
    
    rows[[v]] <- data.frame(
      Variable      = v,
      Niveau        = levs,
      N             = n_per,
      Moyenne       = means,
      Ecart_type    = sds,
      Erreur_type   = ses,
      Groupes       = as.character(letters_v),
      Moyenne_pm_SD = paste0(fmt_val(means, sds), " ", as.character(letters_v)),
      Moyenne_pm_SE = paste0(fmt_val(means, ses), " ", as.character(letters_v)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out
}


#' PostHoc par variable sur les cellules d'une interaction (facteurs croises)
#'
#' Quand une interaction est presente, compare les combinaisons de niveaux
#' (ex. periode1.zoneA, periode1.zoneB, periode2.zoneA...) afin d'apprecier
#' simultanement l'effet du facteur fixe et du facteur evalue.
#'
#' @param df        data.frame nettoye
#' @param response  variables reponses
#' @param factors   vecteur de >= 2 facteurs (croises)
#' @param parametric TRUE = ANOVA/Tukey, FALSE = Kruskal/Dunn
#' @param digits    decimales d'affichage
#' @return data.frame long : Variable, Cellule, <facteurs>, N, Moyenne,
#'         Ecart_type, Erreur_type, Groupes, Moyenne_pm_SD, Moyenne_pm_SE
build_letters_interaction <- function(df, response, factors,
                                      parametric = TRUE, digits = 3) {
  if (length(factors) < 2) return(NULL)
  df <- df[stats::complete.cases(df[, c(response, factors), drop = FALSE]), , drop = FALSE]
  for (f in factors) {
    if (!is.factor(df[[f]])) df[[f]] <- factor(as.character(df[[f]]))
    df[[f]] <- droplevels(df[[f]])
  }
  
  # Facteur croise : une cellule par combinaison de niveaux
  cell <- interaction(df[factors], sep = " . ", drop = TRUE)
  cell <- droplevels(cell)
  levs <- levels(cell)
  if (length(levs) < 2) return(NULL)
  
  # Table de correspondance cellule -> niveaux d'origine
  cell_map <- unique(data.frame(
    Cellule = as.character(cell),
    df[factors],
    stringsAsFactors = FALSE
  ))
  cell_map <- cell_map[match(levs, cell_map$Cellule), , drop = FALSE]
  
  fmt_val <- function(m, s) ifelse(is.na(m) | is.na(s), "NA",
                                   paste0(formatC(m, digits = digits, format = "f"),
                                          " \u00b1 ",
                                          formatC(s, digits = digits, format = "f")))
  
  rows <- list()
  for (v in response) {
    y <- df[[v]]
    n_per <- as.numeric(table(cell))[match(levs, levels(cell))]
    means <- vapply(levs, function(lv) mean(y[cell == lv], na.rm = TRUE), numeric(1))
    sds   <- vapply(levs, function(lv) stats::sd(y[cell == lv], na.rm = TRUE), numeric(1))
    ses   <- sds / sqrt(pmax(n_per, 1))
    
    letters_v <- tryCatch({
      if (parametric) {
        fit  <- stats::aov(y ~ cell)
        tuk  <- stats::TukeyHSD(fit)[[1]]
        pv   <- tuk[, "p adj"]; nm <- rownames(tuk)
        pmat <- matrix(1, length(levs), length(levs), dimnames = list(levs, levs))
        for (i in seq_along(nm)) {
          pair <- strsplit(nm[i], "-", fixed = TRUE)[[1]]
          if (length(pair) == 2 && all(pair %in% levs)) {
            pmat[pair[1], pair[2]] <- pv[i]; pmat[pair[2], pair[1]] <- pv[i]
          }
        }
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      } else {
        dn <- FSA::dunnTest(y, cell, method = "bonferroni")$res
        pmat <- matrix(1, length(levs), length(levs), dimnames = list(levs, levs))
        for (i in seq_len(nrow(dn))) {
          pair <- trimws(strsplit(as.character(dn$Comparison[i]), "-", fixed = TRUE)[[1]])
          if (length(pair) == 2 && all(pair %in% levs)) {
            pmat[pair[1], pair[2]] <- dn$P.adj[i]; pmat[pair[2], pair[1]] <- dn$P.adj[i]
          }
        }
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      }
    }, error = function(e) stats::setNames(rep("a", length(levs)), levs))
    
    block <- data.frame(
      Variable      = v,
      Cellule       = levs,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    for (f in factors) block[[f]] <- cell_map[[f]]
    block$N             <- n_per
    block$Moyenne       <- means
    block$Ecart_type    <- sds
    block$Erreur_type   <- ses
    block$Groupes       <- as.character(letters_v)
    block$Moyenne_pm_SD <- paste0(fmt_val(means, sds), " ", as.character(letters_v))
    block$Moyenne_pm_SE <- paste0(fmt_val(means, ses), " ", as.character(letters_v))
    rows[[v]] <- block
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out
}

################################################################################
# MANOVA - Assistant decisionnel et diagnostics avances
################################################################################

#' Distance de Mahalanobis et detection d'outliers multivaries
#' Un point est considere comme outlier si D2 depasse le quantile chi2(p) a alpha.
#' @return list(d2, threshold, n_outliers, idx_outliers, conclusion)
detect_multivariate_outliers <- function(Y, alpha = 0.001) {
  Y <- as.matrix(Y)
  Y <- Y[stats::complete.cases(Y), , drop = FALSE]
  n <- nrow(Y); p <- ncol(Y)
  if (n < p + 2)
    return(list(d2 = NULL, threshold = NA, n_outliers = NA,
                idx_outliers = integer(0),
                conclusion = "Echantillon trop petit pour Mahalanobis"))
  
  centre <- colMeans(Y)
  cov_mat <- tryCatch(stats::cov(Y), error = function(e) NULL)
  if (is.null(cov_mat) || any(is.na(cov_mat)) ||
      tryCatch(det(cov_mat) < .Machine$double.eps, error = function(e) TRUE)) {
    return(list(d2 = NULL, threshold = NA, n_outliers = NA,
                idx_outliers = integer(0),
                conclusion = "Matrice de covariance singuliere -- impossible de calculer Mahalanobis"))
  }
  
  d2 <- stats::mahalanobis(Y, centre, cov_mat)
  threshold <- stats::qchisq(1 - alpha, df = p)
  idx <- which(d2 > threshold)
  pct <- round(100 * length(idx) / n, 1)
  
  concl <- if (length(idx) == 0)
    paste0("Aucun outlier multivarie detecte (seuil chi2(", p, ") a alpha = ", alpha, ").")
  else if (pct < 5)
    paste0(length(idx), " outlier(s) multivarie(s) detecte(s) (", pct,
           "% des observations). Inspectez-les avant d'analyser.")
  else
    paste0(length(idx), " outliers (", pct,
           "% des observations) -- proportion elevee, verifiez la qualite des donnees.")
  
  list(d2 = d2, threshold = threshold, n_outliers = length(idx),
       idx_outliers = idx, conclusion = concl, alpha = alpha)
}


#' Assistant decisionnel : recommande MANOVA parametrique ou PERMANOVA
#'
#' Combine les resultats de Mardia (normalite multivariee), Box's M
#' (homogeneite des covariances) et PERMDISP (homogeneite des dispersions
#' multivariees) pour produire une recommandation argumentee.
#'
#' Logique :
#' - Mardia OK + Box's M OK   -> MANOVA parametrique (Wilks, plus puissant)
#' - Mardia OK + Box's M KO   -> MANOVA parametrique avec Pillai (robuste)
#' - Mardia KO + n grand      -> MANOVA Pillai (theoreme central limite)
#' - Mardia KO + n petit      -> PERMANOVA (aucune hypothese distributionnelle)
#' - PERMDISP KO (dispersions inegales) -> alerte sur l'interpretation
#'
#' @param mardia  output de multivariate_normality_mardia
#' @param boxm    output de boxm_per_factor (data.frame)
#' @param permdisp output de permdisp_per_factor (data.frame)
#' @param n       nombre d'observations
#' @return list(test_recommande, statistique_recommandee, score, justifications, alertes, niveau_confiance)
recommend_manova_test <- function(mardia, boxm, permdisp, n) {
  justifications <- character()
  alertes        <- character()
  score_param    <- 0L  # positif = parametrique recommande, negatif = non parametrique
  
  # 1. Normalite multivariee (Mardia)
  mardia_ok <- !is.null(mardia) &&
    !is.na(mardia$p.skewness) && !is.na(mardia$p.kurtosis) &&
    mardia$p.skewness >= 0.05 && mardia$p.kurtosis >= 0.05
  if (mardia_ok) {
    justifications <- c(justifications,
                        paste0("Normalite multivariee respectee (Mardia : p.skew = ",
                               round(mardia$p.skewness, 3), ", p.kurt = ",
                               round(mardia$p.kurtosis, 3), ")."))
    score_param <- score_param + 2L
  } else {
    if (!is.null(mardia) && (isTRUE(mardia$p.skewness < 0.05) || isTRUE(mardia$p.kurtosis < 0.05))) {
      justifications <- c(justifications,
                          "Violation de la normalite multivariee (Mardia significatif).")
      # Mais si n est grand, le theoreme central limite protege
      if (n >= 50) {
        justifications <- c(justifications,
                            paste0("Toutefois, n = ", n, " >= 50 : la MANOVA reste robuste par le ",
                                   "theoreme central limite (preferer la statistique de Pillai)."))
        score_param <- score_param + 0L
      } else {
        justifications <- c(justifications,
                            paste0("Et n = ", n, " < 50 : PERMANOVA est plus sure (pas d'hypothese ",
                                   "distributionnelle)."))
        score_param <- score_param - 2L
      }
    }
  }
  
  # 2. Homogeneite des matrices de covariance (Box's M)
  boxm_violations <- if (!is.null(boxm)) sum(grepl("Violation", boxm$Conclusion), na.rm = TRUE) else 0
  if (!is.null(boxm) && boxm_violations == 0) {
    justifications <- c(justifications,
                        "Homogeneite des matrices de covariance respectee (Box's M non significatif).")
    score_param <- score_param + 1L
  } else if (boxm_violations > 0) {
    justifications <- c(justifications,
                        paste0("Violation d'homogeneite des covariances sur ", boxm_violations,
                               " facteur(s) (Box's M significatif). La statistique de Pillai est ",
                               "recommandee car plus robuste a cette violation."))
    score_param <- score_param + 0L  # neutre car Pillai compense
  }
  
  # 3. Homogeneite multivariee des dispersions (PERMDISP)
  permdisp_violations <- if (!is.null(permdisp)) sum(grepl("heterogenes|hétérogènes", permdisp$Conclusion), na.rm = TRUE) else 0
  if (permdisp_violations > 0) {
    alertes <- c(alertes,
                 paste0("PERMDISP signale des dispersions multivariees inegales sur ",
                        permdisp_violations, " facteur(s). Une PERMANOVA significative pourrait ",
                        "refleter une difference de dispersion plutot qu'une difference de localisation. ",
                        "A interpreter avec prudence."))
  }
  
  # Decision finale
  if (score_param >= 2) {
    test_rec  <- "MANOVA parametrique"
    stat_rec  <- "Wilks (puissance maximale)"
    confiance <- "elevee"
  } else if (score_param >= 0) {
    test_rec  <- "MANOVA parametrique"
    stat_rec  <- "Pillai (robuste aux violations)"
    confiance <- if (score_param == 0) "moderee" else "elevee"
  } else {
    test_rec  <- "PERMANOVA"
    stat_rec  <- "Pseudo-F par permutations (999 permutations recommandees)"
    confiance <- "elevee"
  }
  
  list(
    test_recommande         = test_rec,
    statistique_recommandee = stat_rec,
    score                   = score_param,
    niveau_confiance        = confiance,
    justifications          = justifications,
    alertes                 = alertes
  )
}


#' Test d'effets simples multivaries (MANOVA conditionnelles)
#'
#' Quand l'interaction A:B est significative, decompose en testant l'effet de A
#' separement dans chaque niveau de B (et vice versa).
#'
#' @param df       data.frame nettoye
#' @param response variables reponses
#' @param fixed    facteur fixe (on conditionne dessus)
#' @param tested   facteur teste dans chaque niveau de `fixed`
#' @return data.frame : Niveau_fixe, Effet_teste, ddl_num, ddl_den, Pillai, F, p_value, Significatif
manova_simple_effects <- function(df, response, fixed, tested) {
  df <- df[stats::complete.cases(df[, c(response, fixed, tested), drop = FALSE]), , drop = FALSE]
  if (!is.factor(df[[fixed]]))  df[[fixed]]  <- factor(as.character(df[[fixed]]))
  if (!is.factor(df[[tested]])) df[[tested]] <- factor(as.character(df[[tested]]))
  
  results <- list()
  skipped  <- character()
  for (lev in levels(df[[fixed]])) {
    sub <- df[df[[fixed]] == lev, , drop = FALSE]
    sub[[tested]] <- droplevels(sub[[tested]])
    if (nlevels(sub[[tested]]) < 2) next
    n_sub <- nrow(sub); p <- length(response)
    if (n_sub < p + nlevels(sub[[tested]]) + 1) {
      skipped <- c(skipped, lev); next
    }
    
    # Verifier le rang de la matrice des reponses : variables colineaires -> MANOVA impossible
    Ysub <- as.matrix(sub[, response, drop = FALSE])
    rg <- tryCatch(qr(scale(Ysub, center = TRUE, scale = FALSE))$rank,
                   error = function(e) NA_integer_)
    if (is.na(rg) || rg < p) {
      skipped <- c(skipped, lev); next
    }
    
    fml <- stats::as.formula(paste0(
      "cbind(", paste(sapply(response, function(x) paste0("`", x, "`")), collapse = ", "),
      ") ~ `", tested, "`"
    ))
    fit <- tryCatch(stats::manova(fml, data = sub), error = function(e) NULL)
    if (is.null(fit)) { skipped <- c(skipped, lev); next }
    
    s <- tryCatch(summary(fit, test = "Pillai")$stats, error = function(e) NULL)
    if (is.null(s)) { skipped <- c(skipped, lev); next }
    eff_row <- which(rownames(s) == tested)
    if (length(eff_row) == 0) eff_row <- 1
    results[[lev]] <- data.frame(
      Niveau_fixe = paste0(fixed, " = ", lev),
      Effet_teste = tested,
      ddl_num     = s[eff_row, "Df"],
      ddl_den     = s["Residuals", "Df"],
      Pillai      = s[eff_row, "Pillai"],
      F_stat      = s[eff_row, "approx F"],
      p_value     = s[eff_row, "Pr(>F)"],
      stringsAsFactors = FALSE
    )
  }
  if (length(results) == 0) {
    if (length(skipped) > 0)
      attr(results, "skip_reason") <-
        "Variables réponses colinéaires ou sous-groupes trop petits : effets simples MANOVA non calculables."
    return(NULL)
  }
  out <- do.call(rbind, results); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = "bonferroni")
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


#' Effets simples PERMANOVA (analogue non parametrique)
#' @return data.frame analogue a manova_simple_effects
permanova_simple_effects <- function(df, response, fixed, tested,
                                     permutations = 999, dist_method = "euclidean") {
  df <- df[stats::complete.cases(df[, c(response, fixed, tested), drop = FALSE]), , drop = FALSE]
  if (!is.factor(df[[fixed]]))  df[[fixed]]  <- factor(as.character(df[[fixed]]))
  if (!is.factor(df[[tested]])) df[[tested]] <- factor(as.character(df[[tested]]))
  
  results <- list()
  for (lev in levels(df[[fixed]])) {
    sub <- df[df[[fixed]] == lev, , drop = FALSE]
    sub[[tested]] <- droplevels(sub[[tested]])
    if (nlevels(sub[[tested]]) < 2 || nrow(sub) < 4) next
    
    Y <- as.matrix(sub[, response, drop = FALSE])
    d <- tryCatch(vegan::vegdist(Y, method = dist_method), error = function(e) NULL)
    if (is.null(d)) next
    fml <- stats::as.formula(paste0("d ~ `", tested, "`"))
    ad <- tryCatch(
      vegan::adonis2(fml, data = sub, permutations = permutations, by = "terms"),
      error = function(e) NULL
    )
    if (is.null(ad)) next
    
    results[[lev]] <- data.frame(
      Niveau_fixe  = paste0(fixed, " = ", lev),
      Effet_teste  = tested,
      ddl          = ad$Df[1],
      R2           = ad$R2[1],
      F_pseudo     = ad$F[1],
      p_value      = ad$`Pr(>F)`[1],
      stringsAsFactors = FALSE
    )
  }
  if (length(results) == 0) return(NULL)
  out <- do.call(rbind, results); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = "bonferroni")
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


#' Resume l'etat actuel de l'analyse multivariee pour la frise de workflow
#' @return liste de booleens decrivant chaque etape du workflow
workflow_state <- function(values) {
  detect_interaction <- function(df, effet_col, p_col) {
    if (is.null(df)) return(FALSE)
    inter <- grepl(":", df[[effet_col]])
    any(inter) && any(df[[p_col]][inter] < 0.05, na.rm = TRUE)
  }
  list(
    has_data        = !is.null(values$filteredData),
    has_diagnostic  = !is.null(values$manovaMardia) || !is.null(values$manovaBoxM),
    has_test        = !is.null(values$manovaParamResults) || !is.null(values$manovaPermanovaResults),
    has_posthoc     = !is.null(values$manovaMultiPostHoc),
    is_param        = !is.null(values$manovaParamResults),
    is_nonparam     = !is.null(values$manovaPermanovaResults),
    has_interaction = detect_interaction(values$manovaParamResults, "Effet", "p_Pillai") ||
      detect_interaction(values$manovaPermanovaResults, "Effet", "p_value")
  )
}


################################################################################
#
#  LM / GLM - PostHoc et comparaisons multiples
#
################################################################################

#' Identifie les predicteurs categoriels dans un modele lm/glm
#' @param model modele lm ou glm ajuste
#' @return character vector des noms des predicteurs categoriels
identify_categorical_predictors <- function(model) {
  if (is.null(model) || is.null(model$model)) return(character(0))
  mf <- model$model
  preds <- setdiff(names(mf), names(mf)[1])
  preds[vapply(preds, function(p) {
    v <- mf[[p]]
    is.factor(v) || is.character(v) || is.logical(v)
  }, logical(1))]
}

#' Comparaisons par paires sur un predicteur categoriel (emmeans-based)
#' @param model    modele lm ou glm
#' @param predictor nom du predicteur categoriel
#' @param adjust   methode d'ajustement ("tukey", "bonferroni", "sidak", "holm", "fdr", "none")
#' @return data.frame : Comparaison, Estimate, SE, ddl, t/z, p_value, p_adj, Significatif
lm_pairwise_emmeans <- function(model, predictor, adjust = "tukey") {
  if (!requireNamespace("emmeans", quietly = TRUE))
    return(NULL)
  em <- tryCatch(
    emmeans::emmeans(model, specs = predictor),
    error = function(e) NULL
  )
  if (is.null(em)) return(NULL)
  pr <- tryCatch(
    graphics::pairs(em, adjust = adjust),
    error = function(e) NULL
  )
  if (is.null(pr)) return(NULL)
  
  df_out <- as.data.frame(pr)
  rename_map <- c(contrast    = "Comparaison",
                  estimate    = "Difference",
                  t.ratio     = "t",
                  z.ratio     = "z",
                  p.value     = "p_value")
  for (old in names(rename_map)) {
    if (old %in% names(df_out)) names(df_out)[names(df_out) == old] <- rename_map[[old]]
  }
  if ("p_value" %in% names(df_out)) {
    df_out$p_adj        <- df_out$p_value
    df_out$Significatif <- ifelse(is.na(df_out$p_adj), "NA",
                                  ifelse(df_out$p_adj < 0.05, "Oui", "Non"))
  }
  df_out
}


#' Lettres de groupes (CLD) pour un predicteur categoriel
#' @param model    modele lm ou glm
#' @param predictor nom du predicteur categoriel
#' @param adjust   methode d'ajustement
#' @return data.frame : Niveau, emmean, SE, ddl, Groupes
lm_cld_letters <- function(model, predictor, adjust = "tukey", digits = 3) {
  if (!requireNamespace("emmeans", quietly = TRUE) ||
      !requireNamespace("multcomp", quietly = TRUE))
    return(NULL)
  em <- tryCatch(emmeans::emmeans(model, specs = predictor),
                 error = function(e) NULL)
  if (is.null(em)) return(NULL)
  cld <- tryCatch(
    multcomp::cld(em, adjust = adjust, Letters = letters, decreasing = TRUE),
    error = function(e) {
      tryCatch(emmeans::cld(em, adjust = adjust, Letters = letters, decreasing = TRUE),
               error = function(e2) NULL)
    }
  )
  if (is.null(cld)) return(NULL)
  
  df_out <- as.data.frame(cld)
  if (predictor %in% names(df_out))
    names(df_out)[names(df_out) == predictor] <- "Niveau"
  if (".group" %in% names(df_out))
    names(df_out)[names(df_out) == ".group"] <- "Groupes"
  if ("group" %in% names(df_out))
    names(df_out)[names(df_out) == "group"] <- "Groupes"
  if ("Groupes" %in% names(df_out))
    df_out$Groupes <- trimws(df_out$Groupes)
  
  # Moyennes observees par niveau (depuis les donnees du modele) + SD / SE
  mf <- tryCatch(model$model, error = function(e) NULL)
  if (!is.null(mf) && predictor %in% names(mf)) {
    resp_name <- names(mf)[1]
    y <- mf[[resp_name]]
    g <- factor(mf[[predictor]])
    fmt <- function(m, s) ifelse(is.na(m) | is.na(s), "NA",
                                 paste0(formatC(m, digits = digits, format = "f"),
                                        " \u00b1 ",
                                        formatC(s, digits = digits, format = "f")))
    stats_g <- sapply(as.character(df_out$Niveau), function(lv) {
      yy <- y[g == lv]
      n  <- sum(!is.na(yy))
      m  <- mean(yy, na.rm = TRUE)
      sdv <- stats::sd(yy, na.rm = TRUE)
      sev <- if (n > 0) sdv / sqrt(n) else NA_real_
      c(m = m, sd = sdv, se = sev)
    })
    grp <- df_out$Groupes
    df_out$`Moyenne_pm_SD` <- paste0(fmt(stats_g["m", ], stats_g["sd", ]), " ", grp)
    df_out$`Moyenne_pm_SE` <- paste0(fmt(stats_g["m", ], stats_g["se", ]), " ", grp)
  }
  df_out
}


#' Test de Type II (ou Type III) pour un modele lm/glm
#' @param model modele
#' @param type 2 ou 3
#' @return data.frame avec colonnes : Predicteur, Chi2|F, ddl, p_value
lm_anova_table <- function(model, type = 2) {
  if (!requireNamespace("car", quietly = TRUE)) {
    res <- tryCatch(stats::anova(model), error = function(e) return(NULL))
    if (is.null(res)) return(NULL)
    df_out <- as.data.frame(res)
    df_out$Predicteur <- rownames(df_out)
    return(df_out)
  }
  res <- tryCatch(car::Anova(model, type = type), error = function(e) NULL)
  if (is.null(res)) return(NULL)
  df_out <- as.data.frame(res)
  df_out$Predicteur <- rownames(df_out)
  df_out <- df_out[df_out$Predicteur != "Residuals", , drop = FALSE]
  pcol <- intersect(c("Pr(>F)", "Pr(>Chisq)"), names(df_out))
  if (length(pcol) > 0) {
    df_out$Significatif <- ifelse(is.na(df_out[[pcol[1]]]), "NA",
                                  ifelse(df_out[[pcol[1]]] < 0.05, "Oui", "Non"))
  }
  df_out
}

################################################################################
# Helpers transverses : arrondi global et coloration des groupes posthoc
################################################################################

#' Arrondit toutes les colonnes numeriques d'un data.frame selon les reglages.
#' Centralise la logique d'arrondi pour toutes les analyses (maintenable).
#' @param df       data.frame a arrondir
#' @param round_on TRUE si l'arrondi est active (input$testsRoundResults)
#' @param decimals nombre de decimales (input$testsDecimals)
#' @return data.frame avec colonnes numeriques arrondies (inchange si round_on FALSE)
round_numeric_df <- function(df, round_on, decimals = 2) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  
  # Nombre de decimales : si l'arrondi est actif, valeur choisie ; sinon
  # une precision d'affichage par defaut (3) garantissant la coherence
  # entre colonnes numeriques et colonnes texte "Moyenne ± ...".
  dec <- if (isTRUE(round_on)) {
    if (is.null(decimals) || is.na(decimals)) 2L else as.integer(decimals)
  } else {
    3L
  }
  
  # Noms possibles des colonnes texte (avant ou apres renommage d'affichage)
  sd_names <- c("Moyenne_pm_SD", "Moyenne \u00b1 Ecart-type groupe")
  se_names <- c("Moyenne_pm_SE", "Moyenne \u00b1 Erreur-type groupe")
  has_col  <- function(nms) nms[nms %in% names(df)][1]
  sd_col   <- has_col(sd_names)
  se_col   <- has_col(se_names)
  
  # Reconstruction des colonnes texte a partir des colonnes numeriques sources,
  # afin que "Moyenne ± Ecart-type" affiche EXACTEMENT la valeur de "Moyenne".
  fmt_pair <- function(m, s, grp) {
    out <- ifelse(is.na(m) | is.na(s), "NA",
                  paste0(formatC(round(m, dec), digits = dec, format = "f"),
                         " \u00b1 ",
                         formatC(round(s, dec), digits = dec, format = "f")))
    if (!is.null(grp)) out <- paste0(out, " ", grp)
    out
  }
  grp_vec <- if ("Groupes" %in% names(df)) as.character(df$Groupes) else NULL
  
  if (!is.na(sd_col) && all(c("Moyenne", "Ecart_type") %in% names(df))) {
    df[[sd_col]] <- fmt_pair(df$Moyenne, df$Ecart_type, grp_vec)
  }
  if (!is.na(se_col) && all(c("Moyenne", "Erreur_type") %in% names(df))) {
    df[[se_col]] <- fmt_pair(df$Moyenne, df$Erreur_type, grp_vec)
  }
  
  # Colonnes numeriques : arrondi seulement si l'utilisateur l'a demande.
  if (isTRUE(round_on)) {
    num <- vapply(df, is.numeric, logical(1))
    if (any(num))
      df[, num] <- lapply(df[, num, drop = FALSE], function(x) round(x, dec))
    
    # Colonnes texte "± " restantes (sans colonnes numeriques sources) :
    # re-arrondir les nombres presents dans la chaine.
    is_pm_col <- function(values) {
      v <- values[!is.na(values)]
      length(v) > 0 && all(grepl("\u00b1", v))
    }
    handled <- c(sd_col, se_col)
    for (cn in names(df)) {
      if (cn %in% handled) next
      if (is.character(df[[cn]]) && is_pm_col(df[[cn]])) {
        df[[cn]] <- vapply(df[[cn]], function(s) {
          if (is.na(s)) return(NA_character_)
          m <- gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", s, perl = TRUE)
          regmatches(s, m) <- list(vapply(regmatches(s, m)[[1]], function(num) {
            val <- suppressWarnings(as.numeric(num))
            if (is.na(val)) num
            else formatC(round(val, dec), digits = dec, format = "f")
          }, character(1)))
          s
        }, character(1), USE.NAMES = FALSE)
      }
    }
  }
  df
}

#' Palette de couleurs distinctes pour les lettres de groupes CLD.
#' @param n nombre de couleurs souhaitees
#' @return vecteur de couleurs hexadecimales
group_color_palette <- function(n) {
  base <- c("#E8F5E9", "#FFF3E0", "#E3F2FD", "#F3E5F5", "#FCE4EC",
            "#E0F7FA", "#FFF9C4", "#EFEBE9", "#F1F8E9", "#E1F5FE",
            "#FBE9E7", "#EDE7F6")
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::hcl.colors(n, palette = "Pastel 1")
}

#' Applique une couleur de fond distincte par lettre de groupe a une colonne
#' d'un datatable DT. Chaque groupe unique recoit sa propre couleur.
#' @param dt  objet datatable DT
#' @param df  data.frame source (pour extraire les niveaux de groupe)
#' @param col nom de la colonne contenant les lettres de groupes
#' @return datatable DT avec coloration appliquee
color_groups_dt <- function(dt, df, col = "Groupes") {
  if (!col %in% names(df)) return(dt)
  grps <- sort(unique(stats::na.omit(as.character(df[[col]]))))
  if (length(grps) == 0) return(dt)
  cols <- group_color_palette(length(grps))
  DT::formatStyle(dt, col,
                  backgroundColor = DT::styleEqual(grps, cols),
                  fontWeight = "bold", textAlign = "center")
}


################################################################################
#
#  Moteur de donnees HStat -- chargement en memoire (fread) + hors-memoire (DuckDB)
#
################################################################################

# -- Parametres globaux (modifiables) -----------------------------------------
# Au-dela de ce seuil, les CSV/Parquet basculent en mode hors-memoire (DuckDB).
HSTAT_BIGDATA_THRESHOLD <- 500 * 1024^2      # 500 Mo
# Taille de l'echantillon de travail en mode hors-memoire.
HSTAT_SAMPLE_SIZE       <- 100000L

# -- Detection du type de fichier ---------------------------------------------
hstat_file_kind <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "txt", "tsv"))      return("csv")
  if (ext %in% c("xlsx", "xls"))            return("excel")
  if (ext == "parquet")                     return("parquet")
  if (ext %in% c("duckdb", "ddb"))          return("duckdb")
  if (ext == "sav")                         return("sav")
  if (ext == "dta")                         return("dta")
  if (ext == "rds")                         return("rds")
  "inconnu"
}

# -- Taille du fichier (octets) -----------------------------------------------
hstat_file_size <- function(path) {
  s <- tryCatch(file.info(path)$size, error = function(e) NA_real_)
  if (is.na(s)) 0 else s
}
hstat_format_size <- function(bytes) {
  if (is.na(bytes) || bytes <= 0) return("0 o")
  u <- c("o", "Ko", "Mo", "Go", "To")
  i <- min(floor(log(bytes, 1024)), length(u) - 1)
  paste0(round(bytes / 1024^i, 1), " ", u[i + 1])
}

# -- Chemin compatible SQL (slash avant, apostrophes echappees) ---------------
hstat_sql_path <- function(path) {
  gsub("'", "''", gsub("\\\\", "/", path))
}

# -- Disponibilite des moteurs ------------------------------------------------
hstat_has_duckdb <- function()
  requireNamespace("duckdb", quietly = TRUE) && requireNamespace("DBI", quietly = TRUE)
hstat_has_datatable <- function()
  requireNamespace("data.table", quietly = TRUE)

# -- Lecture rapide d'un CSV en memoire (fread, repli read.csv) ---------------
hstat_read_csv_mem <- function(path, header = TRUE, sep = ",") {
  if (hstat_has_datatable()) {
    df <- data.table::fread(path, header = header, sep = sep,
                            data.table = FALSE, check.names = FALSE,
                            showProgress = FALSE, encoding = "UTF-8")
    return(as.data.frame(df))
  }
  read.csv(path, header = header, sep = sep, check.names = FALSE,
           stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}

# -- Ouverture d'une connexion DuckDB en memoire ------------------------------
hstat_duckdb_connect <- function() {
  if (!hstat_has_duckdb()) stop("Le package 'duckdb' est requis pour le mode hors-memoire.")
  DBI::dbConnect(duckdb::duckdb())
}

# -- Enregistrement d'une source dans DuckDB sous forme de VUE ----------------
# Aucune donnee n'est materialisee : DuckDB interroge le fichier sur disque.
# Retourne le nom de la vue creee.
hstat_duckdb_register <- function(con, path, kind, header = TRUE, sep = ",") {
  p   <- hstat_sql_path(path)
  tbl <- "hstat_source"
  DBI::dbExecute(con, sprintf("DROP VIEW IF EXISTS %s", tbl))
  if (kind == "csv") {
    delim <- if (identical(sep, "\t")) "\\t" else sep
    sql <- sprintf(
      "CREATE VIEW %s AS SELECT * FROM read_csv_auto('%s', header=%s, delim='%s', sample_size=-1)",
      tbl, p, if (isTRUE(header)) "true" else "false", delim)
  } else if (kind == "parquet") {
    sql <- sprintf("CREATE VIEW %s AS SELECT * FROM read_parquet('%s')", tbl, p)
  } else {
    stop("Type non pris en charge par DuckDB : ", kind)
  }
  DBI::dbExecute(con, sql)
  tbl
}

# -- Connexion a un fichier DuckDB natif --------------------------------------
hstat_duckdb_open_file <- function(path) {
  if (!hstat_has_duckdb()) stop("Le package 'duckdb' est requis.")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  tbls <- DBI::dbListTables(con)
  if (length(tbls) == 0) { DBI::dbDisconnect(con, shutdown = TRUE); stop("Aucune table dans ce fichier DuckDB.") }
  list(con = con, table = tbls[1], tables = tbls)
}

# -- Metadonnees d'une table/vue DuckDB ---------------------------------------
hstat_duckdb_nrow <- function(con, tbl) {
  as.numeric(DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", tbl))$n[1])
}
hstat_duckdb_colnames <- function(con, tbl) {
  DBI::dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 0", tbl)) |> names()
}
hstat_duckdb_na_total <- function(con, tbl) {
  cols <- hstat_duckdb_colnames(con, tbl)
  if (length(cols) == 0) return(0)
  expr <- paste(sprintf('SUM(CASE WHEN "%s" IS NULL THEN 1 ELSE 0 END)', cols),
                collapse = " + ")
  as.numeric(DBI::dbGetQuery(con, sprintf("SELECT (%s) AS na FROM %s", expr, tbl))$na[1])
}

# -- Echantillon representatif (reservoir sampling DuckDB) --------------------
hstat_duckdb_sample <- function(con, tbl, n = HSTAT_SAMPLE_SIZE) {
  total <- hstat_duckdb_nrow(con, tbl)
  if (total <= n) {
    df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", tbl))
  } else {
    df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s USING SAMPLE %d ROWS",
                                       tbl, as.integer(n)))
  }
  as.data.frame(df)
}

# -- Materialisation complete d'une source DuckDB (petits fichiers) -----------
hstat_duckdb_collect <- function(con, tbl) {
  as.data.frame(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", tbl)))
}

# -- Fermeture propre d'une connexion DuckDB ----------------------------------
hstat_duckdb_close <- function(con) {
  if (!is.null(con)) tryCatch(DBI::dbDisconnect(con, shutdown = TRUE),
                              error = function(e) NULL)
}

# -- Chargeur unifie ----------------------------------------------------------
# Retourne une liste : data (data.frame de travail), mode ("memory"/"duckdb"),
# con (connexion DuckDB ou NULL), table, full_nrow, full_ncol, full_na,
# is_sampled, kind, size.
hstat_load_data <- function(path, kind, header = TRUE, sep = ",",
                            sheet = 1,
                            threshold = HSTAT_BIGDATA_THRESHOLD,
                            sample_size = HSTAT_SAMPLE_SIZE) {
  size <- hstat_file_size(path)

  # --- Formats toujours charges en memoire ---------------------------------
  if (kind == "excel") {
    df <- as.data.frame(readxl::read_excel(path = path, sheet = sheet %||% 1))
  } else if (kind == "sav") {
    df <- as.data.frame(haven::read_sav(path))
  } else if (kind == "dta") {
    df <- as.data.frame(haven::read_dta(path))
  } else if (kind == "rds") {
    df <- as.data.frame(readRDS(path))

  # --- CSV : memoire si petit, DuckDB si volumineux ------------------------
  } else if (kind == "csv") {
    if (size <= threshold || !hstat_has_duckdb()) {
      df <- hstat_read_csv_mem(path, header = header, sep = sep)
    } else {
      con <- hstat_duckdb_connect()
      tbl <- hstat_duckdb_register(con, path, "csv", header = header, sep = sep)
      total <- hstat_duckdb_nrow(con, tbl)
      smp   <- hstat_duckdb_sample(con, tbl, sample_size)
      return(list(data = smp, mode = "duckdb", con = con, table = tbl,
                  full_nrow = total, full_ncol = ncol(smp),
                  full_na = NA_real_, is_sampled = total > nrow(smp),
                  kind = kind, size = size))
    }

  # --- Parquet : DuckDB (materialise si petit, vue si volumineux) ----------
  } else if (kind == "parquet") {
    if (!hstat_has_duckdb()) stop("Le package 'duckdb' est requis pour lire le Parquet.")
    con <- hstat_duckdb_connect()
    tbl <- hstat_duckdb_register(con, path, "parquet")
    total <- hstat_duckdb_nrow(con, tbl)
    if (size <= threshold) {
      df <- hstat_duckdb_collect(con, tbl)
      hstat_duckdb_close(con)
    } else {
      smp <- hstat_duckdb_sample(con, tbl, sample_size)
      return(list(data = smp, mode = "duckdb", con = con, table = tbl,
                  full_nrow = total, full_ncol = ncol(smp),
                  full_na = NA_real_, is_sampled = total > nrow(smp),
                  kind = kind, size = size))
    }

  # --- DuckDB natif --------------------------------------------------------
  } else if (kind == "duckdb") {
    op <- hstat_duckdb_open_file(path)
    total <- hstat_duckdb_nrow(op$con, op$table)
    smp   <- hstat_duckdb_sample(op$con, op$table, sample_size)
    return(list(data = smp, mode = "duckdb", con = op$con, table = op$table,
                full_nrow = total, full_ncol = ncol(smp),
                full_na = NA_real_, is_sampled = total > nrow(smp),
                kind = kind, size = size))

  } else {
    stop("Format de fichier non pris en charge.")
  }

  # --- Sortie mode memoire -------------------------------------------------
  list(data = df, mode = "memory", con = NULL, table = NULL,
       full_nrow = nrow(df), full_ncol = ncol(df),
       full_na = sum(is.na(df)), is_sampled = FALSE,
       kind = kind, size = size)
}


################################################################################
#
#  Statistiques exactes sur le jeu COMPLET via DuckDB (mode hors-memoire)
#
################################################################################

# -- Construit les expressions SQL d'agregation pour une colonne numerique ----
# Retourne un vecteur nomme statistique -> expression SQL.
.hstat_sql_stat_exprs <- function(col, stats_sel) {
  q <- sprintf('"%s"', col)
  all_ex <- c(
    mean   = sprintf("AVG(%s)", q),
    median = sprintf("MEDIAN(%s)", q),
    sd     = sprintf("STDDEV_SAMP(%s)", q),
    var    = sprintf("VAR_SAMP(%s)", q),
    cv     = sprintf("(CASE WHEN AVG(%s)=0 THEN NULL ELSE 100.0*STDDEV_SAMP(%s)/ABS(AVG(%s)) END)",
                     q, q, q),
    min    = sprintf("MIN(%s)", q),
    max    = sprintf("MAX(%s)", q),
    q1     = sprintf("QUANTILE_CONT(%s, 0.25)", q),
    q3     = sprintf("QUANTILE_CONT(%s, 0.75)", q)
  )
  all_ex[stats_sel[stats_sel %in% names(all_ex)]]
}

# -- Statistiques descriptives GLOBALES exactes sur le jeu complet ------------
# Sortie identique a make_summ_global : Facteurs, Variable, <stats...>
hstat_duckdb_describe_global <- function(con, tbl, num_vars, stats_sel) {
  rows <- lapply(num_vars, function(v) {
    ex <- .hstat_sql_stat_exprs(v, stats_sel)
    if (length(ex) == 0) return(NULL)
    sel <- paste(sprintf("%s AS %s", ex, names(ex)), collapse = ", ")
    r <- DBI::dbGetQuery(con, sprintf("SELECT %s FROM %s", sel, tbl))
    data.frame(Facteurs = "Global", Variable = v, r, check.names = FALSE)
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# -- Statistiques descriptives GROUPEES exactes sur le jeu complet -----------
# Sortie identique a make_summ_grouped : <group_vars...>, Variable, <stats...>
hstat_duckdb_describe_grouped <- function(con, tbl, group_vars, num_vars, stats_sel) {
  gcols <- paste(sprintf('"%s"', group_vars), collapse = ", ")
  rows <- lapply(num_vars, function(v) {
    ex <- .hstat_sql_stat_exprs(v, stats_sel)
    if (length(ex) == 0) return(NULL)
    sel <- paste(sprintf("%s AS %s", ex, names(ex)), collapse = ", ")
    r <- DBI::dbGetQuery(con, sprintf(
      "SELECT %s, %s FROM %s GROUP BY %s ORDER BY %s",
      gcols, sel, tbl, gcols, gcols))
    r$Variable <- v
    r[, c(group_vars, "Variable", names(ex)), drop = FALSE]
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# -- Table de contingence exacte sur le jeu complet --------------------------
hstat_duckdb_crosstab <- function(con, tbl, row_var, col_var) {
  r <- DBI::dbGetQuery(con, sprintf(
    'SELECT "%s" AS rv, "%s" AS cv, COUNT(*) AS n FROM %s GROUP BY 1, 2',
    row_var, col_var, tbl))
  if (nrow(r) == 0) return(NULL)
  stats::xtabs(n ~ rv + cv, data = r)
}

# -- Matrice de correlation exacte sur le jeu complet ------------------------
hstat_duckdb_cor <- function(con, tbl, num_vars) {
  k <- length(num_vars)
  if (k < 2) return(NULL)
  m <- diag(1, k); dimnames(m) <- list(num_vars, num_vars)
  for (i in 1:(k-1)) for (j in (i+1):k) {
    val <- tryCatch(DBI::dbGetQuery(con, sprintf(
      'SELECT CORR("%s","%s") AS r FROM %s', num_vars[i], num_vars[j], tbl))$r[1],
      error = function(e) NA_real_)
    m[i, j] <- m[j, i] <- val
  }
  m
}

# -- Comptage exact de lignes apres filtre SQL (optionnel) -------------------
hstat_duckdb_count <- function(con, tbl, where = NULL) {
  sql <- sprintf("SELECT COUNT(*) AS n FROM %s", tbl)
  if (!is.null(where) && nzchar(where)) sql <- paste0(sql, " WHERE ", where)
  as.numeric(DBI::dbGetQuery(con, sql)$n[1])
}


################################################################################
#
#  Reproductibilite -- graine aleatoire controlable
#
################################################################################

# Graine par defaut de l'application (modifiable par l'utilisateur dans l'UI).
HSTAT_DEFAULT_SEED <- 123L

# Applique une graine de maniere sure juste avant une operation aleatoire.
# 'seed' provient generalement de input$globalSeed ; si NULL/NA, on retombe
# sur la graine par defaut afin de garantir un comportement reproductible.
hstat_set_seed <- function(seed = NULL) {
  s <- suppressWarnings(as.integer(seed))
  if (length(s) == 0 || is.na(s)) s <- HSTAT_DEFAULT_SEED
  set.seed(s)
  invisible(s)
}


################################################################################
#
#  Cache memoire pour les agregations DuckDB (evite de relancer une requete
#  SQL identique sur un tres gros fichier). Cache simple cle -> valeur, vide
#  a chaque nouveau chargement de donnees via hstat_cache_clear().
#
################################################################################

.hstat_cache <- new.env(parent = emptyenv())

# Vide le cache (a appeler au chargement d'un nouveau fichier).
hstat_cache_clear <- function() {
  rm(list = ls(.hstat_cache, all.names = TRUE), envir = .hstat_cache)
  invisible(NULL)
}

# Memoise le resultat de 'fn()' sous une cle. Si la cle existe deja, renvoie
# la valeur en cache sans relancer le calcul.
hstat_cache_get <- function(key, fn) {
  if (exists(key, envir = .hstat_cache, inherits = FALSE))
    return(get(key, envir = .hstat_cache, inherits = FALSE))
  val <- fn()
  assign(key, val, envir = .hstat_cache)
  val
}

# Construit une cle de cache stable a partir d'elements (table + parametres).
hstat_cache_key <- function(...) {
  parts <- vapply(list(...), function(x) paste(as.character(x), collapse = "|"),
                  character(1))
  paste(parts, collapse = "::")
}
