# =============================================================================
#  HStat -- Tests automatises (testthat)
#
#  Comment lancer :
#    1. Installer testthat :  install.packages("testthat")
#    2. Depuis le dossier de l'application :
#         testthat::test_dir("tests")
#    ou pour un seul fichier :
#         testthat::test_file("tests/test-hstat.R")
#
#  Ces tests couvrent les FONCTIONS DE CALCUL et utilitaires de Utils.R :
#  detection de format, formatage, moteur de donnees (chemin memoire),
#  agregations SQL (si duckdb dispo), graine reproductible.
#  Ils ne lancent pas l'application Shiny (pas de UI/serveur).
# =============================================================================

library(testthat)

# -- Charger uniquement les fonctions utilitaires (sans demarrer l'app) -------
# On source Utils.R dans un environnement isole. install_and_load() peut
# tenter de charger des packages ; on neutralise cet effet pour les tests.
local({
  # Resolution robuste du chemin de Utils.R : que les tests soient lances depuis
  # la racine de l'application (sys.source("Utils.R")) ou depuis tests/
  # (testthat::test_dir, qui se place dans le dossier du fichier de test).
  utils_path <- "Utils.R"
  if (!file.exists(utils_path)) utils_path <- file.path("..", "Utils.R")
  if (!file.exists(utils_path)) {
    cand <- normalizePath(file.path(dirname(getwd()), "Utils.R"), mustWork = FALSE)
    if (file.exists(cand)) utils_path <- cand
  }
  # Empeche install_and_load de bloquer si un package manque dans l'env de test
  e <- new.env()
  assign("install_and_load", function(...) invisible(NULL), envir = e)
  suppressWarnings(suppressMessages(
    sys.source(utils_path, envir = e, keep.source = FALSE)))
  # Exporter TOUTES les fonctions (y compris cachees, ex. .hstat_sql_stat_exprs)
  for (nm in ls(e, all.names = TRUE))
    assign(nm, get(nm, envir = e), envir = globalenv())
})

# -- Charger le module d'analyses qualitatives (fonctions de calcul) ---------
local({
  q_path <- "mod_qualitative.R"
  if (!file.exists(q_path)) q_path <- file.path("..", "mod_qualitative.R")
  if (file.exists(q_path)) {
    eq <- new.env()
    suppressWarnings(suppressMessages(sys.source(q_path, envir = eq, keep.source = FALSE)))
    for (nm in ls(eq, all.names = TRUE))
      assign(nm, get(nm, envir = eq), envir = globalenv())
  }
})


# =============================================================================
context("Détection du type de fichier")
# =============================================================================

test_that("hstat_file_kind reconnait les extensions courantes", {
  expect_equal(hstat_file_kind("data.csv"),      "csv")
  expect_equal(hstat_file_kind("data.CSV"),      "csv")   # insensible a la casse
  expect_equal(hstat_file_kind("data.txt"),      "csv")
  expect_equal(hstat_file_kind("data.tsv"),      "csv")
  expect_equal(hstat_file_kind("classeur.xlsx"), "excel")
  expect_equal(hstat_file_kind("classeur.xls"),  "excel")
  expect_equal(hstat_file_kind("tab.parquet"),   "parquet")
  expect_equal(hstat_file_kind("base.duckdb"),   "duckdb")
  expect_equal(hstat_file_kind("enq.sav"),       "sav")
  expect_equal(hstat_file_kind("enq.dta"),       "dta")
  expect_equal(hstat_file_kind("obj.rds"),       "rds")
})

test_that("hstat_file_kind renvoie 'inconnu' pour le reste", {
  expect_equal(hstat_file_kind("image.png"), "inconnu")
  expect_equal(hstat_file_kind("archive.zip"), "inconnu")
  expect_equal(hstat_file_kind("sansextension"), "inconnu")
})


# =============================================================================
context("Formatage des tailles de fichier")
# =============================================================================

test_that("hstat_format_size formate correctement les ordres de grandeur", {
  expect_equal(hstat_format_size(0),          "0 o")
  expect_equal(hstat_format_size(-5),         "0 o")     # garde-fou
  expect_equal(hstat_format_size(NA),         "0 o")
  expect_match(hstat_format_size(1024),       "Ko")
  expect_match(hstat_format_size(1048576),    "Mo")
  expect_match(hstat_format_size(1073741824), "Go")
})

test_that("hstat_format_size donne une valeur numérique plausible", {
  expect_equal(hstat_format_size(1536), "1.5 Ko")   # 1.5 * 1024
})


# =============================================================================
context("Chemin SQL (echappement)")
# =============================================================================

test_that("hstat_sql_path normalise les antislash et echappe les apostrophes", {
  expect_equal(hstat_sql_path("C:\\data\\f.csv"), "C:/data/f.csv")
  expect_equal(hstat_sql_path("a'b.csv"),         "a''b.csv")
  expect_equal(hstat_sql_path("/home/u/f.csv"),   "/home/u/f.csv")
})


# =============================================================================
context("Graine reproductible")
# =============================================================================

test_that("hstat_set_seed rend les tirages reproductibles", {
  hstat_set_seed(42); a <- runif(5)
  hstat_set_seed(42); b <- runif(5)
  expect_identical(a, b)
})

test_that("hstat_set_seed retombe sur la graine par défaut si entree invalide", {
  s1 <- hstat_set_seed(NULL)
  expect_equal(s1, HSTAT_DEFAULT_SEED)
  s2 <- hstat_set_seed(NA)
  expect_equal(s2, HSTAT_DEFAULT_SEED)
})

test_that("deux graines differentes produisent des tirages differents", {
  hstat_set_seed(1); a <- runif(10)
  hstat_set_seed(2); b <- runif(10)
  expect_false(isTRUE(all.equal(a, b)))
})


# =============================================================================
context("Chargeur de données -- chemin en memoire (CSV)")
# =============================================================================

test_that("hstat_load_data lit un CSV en memoire et renseigne les metadonnees", {
  set.seed(1)
  df <- data.frame(
    g = sample(c("A", "B", "C"), 200, replace = TRUE),
    x = rnorm(200),
    y = runif(200)
  )
  df$x[c(3, 50, 120)] <- NA
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  res <- hstat_load_data(tmp, kind = "csv", header = TRUE, sep = ",")

  expect_equal(res$mode, "memory")
  expect_true(is.data.frame(res$data))
  expect_equal(res$full_nrow, 200)
  expect_equal(res$full_ncol, 3)
  expect_equal(res$full_na, 3)          # 3 NA inseres
  expect_false(res$is_sampled)
  expect_null(res$con)
})

test_that("le seuil hors-memoire n'affecte pas un petit CSV", {
  df  <- data.frame(a = 1:50, b = letters[1:50 %% 26 + 1])
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  res <- hstat_load_data(tmp, kind = "csv", threshold = 1e12)
  expect_equal(res$mode, "memory")
  expect_equal(res$full_nrow, 50)
})


# =============================================================================
context("Expressions SQL d'agregation")
# =============================================================================

test_that(".hstat_sql_stat_exprs génère les expressions attendues", {
  ex <- .hstat_sql_stat_exprs("Rendement", c("mean", "sd", "min", "max"))
  expect_true(all(c("mean", "sd", "min", "max") %in% names(ex)))
  expect_match(ex[["mean"]], "AVG")
  expect_match(ex[["sd"]],   "STDDEV_SAMP")
  expect_match(ex[["min"]],  "MIN")
  expect_match(ex[["max"]],  "MAX")
  # Les noms de colonnes sont entre guillemets doubles (securite SQL)
  expect_match(ex[["mean"]], '"Rendement"')
})

test_that(".hstat_sql_stat_exprs ne retourne que les stats demandees", {
  ex <- .hstat_sql_stat_exprs("x", c("median"))
  expect_equal(names(ex), "median")
  expect_match(ex[["median"]], "MEDIAN")
})


# =============================================================================
context("Agregations exactes via DuckDB (si disponible)")
# =============================================================================

test_that("describe_global DuckDB == calcul de référence R", {
  skip_if_not(hstat_has_duckdb(), "duckdb non installe")

  set.seed(7)
  df <- data.frame(grp = sample(c("A", "B"), 5000, replace = TRUE),
                   x = rnorm(5000, 10, 5))
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  con <- hstat_duckdb_connect()
  on.exit(hstat_duckdb_close(con), add = TRUE)
  tbl <- hstat_duckdb_register(con, tmp, "csv", header = TRUE, sep = ",")

  out <- hstat_duckdb_describe_global(con, tbl, "x", c("mean", "sd", "min", "max"))

  expect_equal(out$mean[1], mean(df$x),  tolerance = 1e-6)
  expect_equal(out$sd[1],   sd(df$x),    tolerance = 1e-6)
  expect_equal(out$min[1],  min(df$x),   tolerance = 1e-6)
  expect_equal(out$max[1],  max(df$x),   tolerance = 1e-6)
})

test_that("crosstab DuckDB == table() de référence", {
  skip_if_not(hstat_has_duckdb(), "duckdb non installe")

  set.seed(3)
  df <- data.frame(r = sample(c("A", "B", "C"), 4000, replace = TRUE),
                   c = sample(c("X", "Y"), 4000, replace = TRUE))
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  con <- hstat_duckdb_connect()
  on.exit(hstat_duckdb_close(con), add = TRUE)
  tbl <- hstat_duckdb_register(con, tmp, "csv", header = TRUE, sep = ",")

  ct_duck <- hstat_duckdb_crosstab(con, tbl, "r", "c")
  ct_ref  <- table(df$r, df$c)

  # Memes effectifs (apres alignement des dimensions)
  expect_equal(sum(ct_duck), sum(ct_ref))
  expect_equal(as.numeric(ct_duck["A", "X"]),
               as.numeric(ct_ref["A", "X"]))
})

test_that("corrélation DuckDB == cor() de référence (Pearson)", {
  skip_if_not(hstat_has_duckdb(), "duckdb non installe")

  set.seed(11)
  n <- 5000
  df <- data.frame(a = rnorm(n))
  df$b <- df$a * 0.7 + rnorm(n, 0, 0.5)
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  con <- hstat_duckdb_connect()
  on.exit(hstat_duckdb_close(con), add = TRUE)
  tbl <- hstat_duckdb_register(con, tmp, "csv", header = TRUE, sep = ",")

  m <- hstat_duckdb_cor(con, tbl, c("a", "b"))
  expect_equal(m["a", "b"], cor(df$a, df$b), tolerance = 1e-6)
  expect_equal(diag(m), c(a = 1, b = 1))
})

test_that("le sous-echantillonnage DuckDB respecte la taille demandee", {
  skip_if_not(hstat_has_duckdb(), "duckdb non installe")

  df  <- data.frame(x = 1:20000)
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  con <- hstat_duckdb_connect()
  on.exit(hstat_duckdb_close(con), add = TRUE)
  tbl <- hstat_duckdb_register(con, tmp, "csv", header = TRUE, sep = ",")

  hstat_set_seed(1)
  smp <- hstat_duckdb_sample(con, tbl, n = 1000)
  expect_lte(nrow(smp), 1000)
  expect_gt(nrow(smp), 0)

  # Si l'echantillon demande depasse le total, on recupere tout
  smp_all <- hstat_duckdb_sample(con, tbl, n = 99999)
  expect_equal(nrow(smp_all), 20000)
})

# ---- Tests du module d'analyses qualitatives -----------------------------
test_that("Détection de type qualitatif", {
  expect_equal(hstat_q_detect_type(c("Homme","Femme","Homme")), "nominale")
  expect_equal(hstat_q_detect_type(factor(c("Bas","Moyen","Haut"), ordered=TRUE)), "ordinale")
})

test_that("Analyse nominale univariee", {
  set.seed(1); x <- sample(c("A","B","C"), 60, replace=TRUE)
  r <- hstat_q_nominal_univariate(x, "Var")
  expect_true(r$ok)
  expect_true(any(grepl("Shannon", r$metrics$Metrique)))
  expect_true("Tableau de fréquences" %in% names(r$tables))
})

test_that("Analyse nominale bivariee produit V de Cramér", {
  set.seed(2); x <- sample(c("A","B"), 80, replace=TRUE); y <- sample(c("X","Y","Z"), 80, replace=TRUE)
  r <- hstat_q_nominal_bivariate(x, y)
  expect_true(r$ok)
  expect_true(any(grepl("Cramér", r$metrics$Metrique)))
})

test_that("Choix multiples : formats binaire et séparé", {
  set.seed(3)
  dfb <- as.data.frame(matrix(rbinom(50*3,1,.5), ncol=3)); names(dfb) <- c("O1","O2","O3")
  rb <- hstat_q_multiple_choice(dfb, cols=c("O1","O2","O3"))
  expect_true(rb$ok)
  dfs <- data.frame(c = c("A;B","B;C","A","A;B;C"), stringsAsFactors=FALSE)
  rs <- hstat_q_multiple_choice(dfs, sep_col="c")
  expect_true(rs$ok)
})

test_that("Échelle de Likert et alpha de Cronbach", {
  set.seed(4); lat <- sample(1:5, 100, replace=TRUE)
  items <- as.data.frame(lapply(1:4, function(i) pmax(1,pmin(5, lat + sample(-1:1,100,replace=TRUE)))))
  names(items) <- paste0("Q",1:4)
  r <- hstat_q_likert_scale(items, levels_order=1:5)
  expect_true(r$ok)
  alpha_row <- r$metrics$Valeur[r$metrics$Metrique=="Alpha de Cronbach"]
  expect_true(as.numeric(alpha_row) > 0.7)  # items correles -> alpha eleve
})

test_that("Analyse textuelle et thématique", {
  set.seed(5)
  txt <- sample(c("le service est rapide et efficace personnel competent",
                  "prix trop cher pour la qualite vraiment decevant",
                  "produit de bonne qualite je recommande vivement"), 60, replace=TRUE)
  r <- hstat_q_text_analysis(txt, n_topics=2)
  expect_true(r$ok)
  expect_true("Fréquences des mots" %in% names(r$tables))
  expect_true(any(grepl("TF-IDF|TFIDF", names(r$tables))))
})


# =============================================================================
context("Sécurité -- évaluateur de formules et identifiants SQL")
# =============================================================================

test_that("hstat_safe_eval évalue les formules légitimes", {
  d <- data.frame(`Var A` = c(1, 2, 3), B = c(10, 20, 30), check.names = FALSE)
  expect_equal(hstat_safe_eval("`Var A` + B * 2", d), c(21, 42, 63))
  expect_equal(hstat_safe_eval("ifelse(B > 15, 1, 0)", d), c(0, 1, 1))
  expect_equal(hstat_safe_eval("rowMeans(cbind(`Var A`, B), na.rm = TRUE)", d),
               c(5.5, 11, 16.5))
  expect_equal(hstat_safe_eval("log(B)", d), log(c(10, 20, 30)))
})

test_that("hstat_safe_eval bloque le code arbitraire (RCE)", {
  d <- data.frame(B = 1:3)
  expect_error(hstat_safe_eval('system("id")', d))
  expect_error(hstat_safe_eval('base::system("id")', d))
  expect_error(hstat_safe_eval('file.remove("x")', d))
  expect_error(hstat_safe_eval('eval(parse(text = "1+1"))', d))
  expect_error(hstat_safe_eval('(function(x) x)(1)', d))
  expect_error(hstat_safe_eval('assign("x", 1)', d))
  expect_error(hstat_safe_eval('1 + 1; system("id")', d))
  expect_error(hstat_safe_eval('get("system")("id")', d))
  expect_error(hstat_safe_eval('do.call("system", list("id"))', d))
})

test_that("hstat_sql_ident neutralise les guillemets dans les identifiants", {
  expect_equal(hstat_sql_ident("Rendement"), '"Rendement"')
  expect_equal(hstat_sql_ident('col" ; DROP TABLE x --'),
               '"col"" ; DROP TABLE x --"')
})


# =============================================================================
context("Visualisation -- conversion numérique FR et capuchons de moustache")
# =============================================================================

test_that("hstat_as_numeric_fr convertit les formats français", {
  expect_equal(hstat_as_numeric_fr(c("12,5", "3,25", "1 250,75")),
               c(12.5, 3.25, 1250.75))
  expect_equal(hstat_as_numeric_fr(c("2005", "2010", NA)), c(2005, 2010, NA))
  x <- c(1.5, 2.5); expect_identical(hstat_as_numeric_fr(x), x)
  expect_null(hstat_as_numeric_fr(c("ABENGOUROU", "Divo", "MAN")))
  expect_null(hstat_as_numeric_fr(c("", NA_character_)))
  # tolérance : > 10 % de valeurs non convertibles -> NULL
  expect_null(hstat_as_numeric_fr(c("1", "2", "x", "y")))
  expect_equal(hstat_as_numeric_fr(c(rep("1,5", 19), "abc"))[1], 1.5)
})

test_that("hstat_add_whisker_caps insère des capuchons alignés", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  set.seed(3)
  d <- data.frame(x = rep(c("A", "B"), each = 40),
                  loc = rep(c("g1", "g2", "g3", "g4"), 20), y = rnorm(80))
  align_ok <- function(p2, n) {
    b <- ggplot_build(p2); eb <- b$data[[1]]; bx <- b$data[[2]]
    nrow(eb) == n && max(abs(sort(eb$x) - sort(bx$x))) < 1e-9 &&
      max(abs(sort(eb$ymin) - sort(bx$ymin))) < 1e-9 &&
      max(abs(sort(eb$ymax) - sort(bx$ymax))) < 1e-9
  }
  # boxplot groupé (fill au niveau couche) : 8 boîtes "dodgées"
  p1 <- hstat_add_whisker_caps(ggplot(d, aes(x, y)) +
                                 geom_boxplot(aes(fill = loc), alpha = .7))
  expect_true(inherits(p1$layers[[1]]$geom, "GeomErrorbar"))
  expect_true(align_ok(p1, 8))
  # fill au niveau plot (cas post-hoc)
  p2 <- hstat_add_whisker_caps(ggplot(d, aes(x, y, fill = x)) +
                                 geom_boxplot(alpha = .7))
  expect_true(align_ok(p2, 2))
  # idempotence et non-boxplot inchangé
  expect_length(hstat_add_whisker_caps(p1)$layers, 2)
  expect_length(hstat_add_whisker_caps(ggplot(d, aes(y, y)) + geom_line())$layers, 1)
})


# =============================================================================
context("Types de variables -- facteurs ordinaux")
# =============================================================================

test_that("la conversion en facteur ordinal respecte l'ordre défini", {
  convert_ordered <- function(x, lv_user) {
    vals_chr <- as.character(x)
    uniq <- unique(vals_chr[!is.na(vals_chr)])
    lv <- lv_user[lv_user %in% uniq]
    if (length(lv) == 0) lv <- sort(uniq)
    lv <- c(lv, sort(setdiff(uniq, lv)))
    factor(vals_chr, levels = lv, ordered = TRUE)
  }
  x <- c("0-3 ans", "4-15 ans", "+15 ans", "0-3 ans", NA)
  f <- convert_ordered(x, c("0-3 ans", "4-15 ans", "+15 ans"))
  expect_true(is.ordered(f))
  expect_identical(levels(f), c("0-3 ans", "4-15 ans", "+15 ans"))
  expect_true(f[1] < f[2] && f[2] < f[3])
  # ordre partiel : modalités restantes ajoutées en fin, rien n'est perdu
  f2 <- convert_ordered(x, "0-3 ans")
  expect_identical(levels(f2), c("0-3 ans", "+15 ans", "4-15 ans"))
  expect_equal(sum(is.na(f2)), 1)
  # nominal explicite : la classe ordered doit disparaître
  f3 <- factor(as.character(f), ordered = FALSE)
  expect_false(is.ordered(f3))
  expect_true(is.ordered(as.factor(f)))  # justification du correctif
})


# =============================================================================
context("Analyses qualitatives -- sorties console et IC du V de Cramér")
# =============================================================================

test_that("les tests bivariés reproduisent la présentation console R (Titanic)", {
  skip_if_not_installed("ggplot2")
  x <- c(rep("0", 81), rep("1", 233), rep("0", 468), rep("1", 109))
  y <- c(rep("female", 314), rep("male", 577))
  res <- hstat_q_nominal_bivariate(x, y, "Survived", "Sex")
  expect_true(isTRUE(res$ok))
  txt <- paste(res$console, collapse = "\n")
  expect_true(grepl("Yates", txt))            # khi-deux avec correction
  expect_true(grepl("260.7", txt))            # X-squared = 260.72
  expect_true(grepl("193.4747", txt))         # effectifs attendus
  expect_true(grepl("-8.08617", txt))         # résidus de Pearson
  expect_true(grepl("Fisher's Exact Test", txt))
  expect_true(grepl("Cramer V", txt))
  expect_true("Effectifs théoriques (attendus)" %in% names(res$tables))
})

test_that("OR / RR au format epitools avec les valeurs de référence", {
  x <- c(rep("0", 81), rep("1", 233), rep("0", 468), rep("1", 109))
  y <- c(rep("female", 314), rep("male", 577))
  orr <- hstat_q_or_rr_analysis(y, x, "Sex", "Survived", y_issue = "1")
  t2 <- paste(orr$console, collapse = "\n")
  expect_true(grepl("odds ratio with 95% C.I.", t2))
  expect_true(grepl("risk ratio with 95% C.I.", t2))
  expect_true(grepl("0.2545801", t2))         # RR identique à epitools
  expect_true(grepl("0.2123854", t2))         # IC bas identique
  expect_true(grepl("6.46392e-60", t2))       # p Fisher identique
})

test_that("l'IC du V de Cramér encadre l'estimation", {
  chi2 <- 260.717; n <- 891
  V <- sqrt(chi2 / n)
  ci <- hstat_q_cramer_ci(chi2, 1, n, 2)
  expect_true(ci[1] < V && V < ci[2])
  expect_true(ci[1] > 0)
  expect_equal(hstat_q_cramer_ci(NA, 1, 10, 2), c(NA_real_, NA_real_))
})


# =============================================================================
context("Analyses qualitatives -- OR/RR toutes modalités et interprétation")
# =============================================================================

test_that("le mode 'toutes les modalités' croise chaque X avec chaque Y", {
  set.seed(7)
  x <- sample(c("0", "1"), 600, replace = TRUE)
  y <- sample(c("A", "B", "C"), 600, replace = TRUE)
  res <- hstat_q_or_rr_analysis(x, y, "X", "Y", all_pairs = TRUE)
  expect_true(isTRUE(res$ok))
  tbl <- res$tables[["OR / RR par paire"]]
  expect_equal(nrow(tbl), 2 * 3)              # 2 modalités X x 3 modalités Y
  expect_true("Interpretation" %in% names(tbl))
  expect_true(all(nzchar(tbl$Interpretation)))
  # un bloc console OR + RR par modalité de Y
  expect_equal(sum(grepl("== ODDS RATIO", res$console)), 3)
  expect_equal(sum(grepl("== RISQUE RELATIF", res$console)), 3)
  # synthèse d'interprétation présente
  expect_true(any(grepl("combinaison", res$interpretation)))
})

test_that("le mode 'une issue' et le 2x2 strict restent inchangés", {
  set.seed(8)
  x <- sample(c("0", "1"), 400, replace = TRUE)
  y3 <- sample(c("A", "B", "C"), 400, replace = TRUE)
  r1 <- hstat_q_or_rr_analysis(x, y3, "X", "Y", y_issue = "A", all_pairs = FALSE)
  expect_equal(nrow(r1$tables[["OR / RR par paire"]]), 2)     # 2 X vs issue A
  y2 <- sample(c("H", "F"), 400, replace = TRUE)
  r2 <- hstat_q_or_rr_analysis(x, y2, "X", "Sexe")
  expect_equal(nrow(r2$tables[["OR / RR par paire"]]), 1)     # 2x2 strict
})

test_that("l'interprétation par ligne détecte le sens de l'association", {
  # X fortement associé à Y : X=1 -> presque toujours Y=oui
  x <- c(rep("1", 100), rep("0", 100))
  y <- c(rep("oui", 90), rep("non", 10), rep("oui", 10), rep("non", 90))
  res <- hstat_q_or_rr_analysis(x, y, "Expo", "Issue", all_pairs = TRUE)
  tbl <- res$tables[["OR / RR par paire"]]
  # au moins une association significative détectée
  sig <- !(tbl$OR_IC_bas <= 1 & tbl$OR_IC_haut >= 1)
  expect_true(any(sig))
  expect_true(any(grepl("significatif", tbl$Interpretation)))
})


# =============================================================================
context("Analyses qualitatives -- texte (NLP) et modes")
# =============================================================================

test_that("le stemmer français regroupe les formes fléchies sans sur-découper", {
  st <- hstat_q_stem_fr(c("moustiquaire", "moustiquaires", "enfant", "enfants",
                          "saison", "saisons", "dorment", "dormir"))
  expect_equal(st[1], st[2])         # moustiquaire(s)
  expect_equal(st[3], st[4])         # enfant(s)
  expect_equal(st[3], "enfant")      # PAS "enf" (sur-découpage évité)
  expect_equal(st[5], st[6])         # saison(s)
})

test_that("le pipeline NLP produit toutes les étapes et respecte les options", {
  skip_if_not_installed("ggplot2")
  ph <- c("Les moustiquaires protègent les enfants contre le paludisme.",
          "Il fait chaud, les moustiques donnent le paludisme aux enfants.",
          "Le dispensaire donne des moustiquaires en saison des pluies.",
          "Les enfants dorment sous une moustiquaire chaque nuit.",
          "Sans moustiquaire, les enfants attrapent le paludisme souvent.",
          "Ma famille dort sous moustiquaire depuis deux ans.")
  r <- hstat_q_text_analysis(ph, "texte", stem = TRUE)
  expect_true(isTRUE(r$ok))
  expect_true("Étapes du pipeline NLP" %in% names(r$tables))
  expect_true("Scores TF-IDF" %in% names(r$tables))
  # "qu" (mot outil) doit être filtré par les stopwords étendus
  expect_false("qu" %in% r$tables[["Fréquences des mots"]]$Mot)
  # stopwords personnalisés
  r2 <- hstat_q_text_analysis(ph, "texte", extra_stopwords = c("paludisme"))
  expect_false("paludisme" %in% r2$tables[["Fréquences des mots"]]$Mot)
  # chiffres retirés par défaut
  r3 <- hstat_q_text_analysis(c("test 123 456", "test 789 mot", "mot test valeur"),
                              "t", min_char = 2)
  expect_false(any(grepl("[0-9]", r3$tables[["Fréquences des mots"]]$Mot)))
})

test_that("le mode signale l'unimodalité et la multimodalité", {
  r1 <- hstat_q_nominal_univariate(c(rep("A", 10), rep("B", 5)), "V")
  m1 <- r1$metrics
  expect_equal(m1$Valeur[m1$Metrique == "Mode(s)"], "A")
  expect_equal(m1$Valeur[m1$Metrique == "Nature de la distribution"], "Unimodale")
  r2 <- hstat_q_nominal_univariate(c(rep("A", 5), rep("B", 5), rep("C", 2)), "V")
  m2 <- r2$metrics
  expect_equal(m2$Valeur[m2$Metrique == "Nature de la distribution"], "Bimodale")
  expect_true(grepl("A", m2$Valeur[m2$Metrique == "Mode(s)"]))
  expect_true(grepl("B", m2$Valeur[m2$Metrique == "Mode(s)"]))
})

test_that("hstat_q_apply_palette re-colore sans casser le graphique", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  r <- hstat_q_nominal_univariate(c(rep("A", 6), rep("B", 3), rep("C", 1)), "V")
  pbar <- r$plotfns[["Diagramme en barres"]]()
  for (pal in c("blues", "greens", "viridis", "spectral", "greys", "custom")) {
    p2 <- hstat_q_apply_palette(pbar, pal, "#123456", "#abcdef")
    expect_s3_class(p2, "ggplot")
    expect_silent(ggplot2::ggplot_build(p2))
  }
  # 'default' laisse le graphique inchangé
  expect_identical(hstat_q_apply_palette(pbar, "default"), pbar)
})


# =============================================================================
context("Analyses qualitatives -- couleurs par défaut de ggplot")
# =============================================================================

test_that("les graphiques utilisent les échelles par défaut de ggplot", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  ph <- rep(c("moustiquaire enfants paludisme excellent",
              "moustiques chaud probleme difficile",
              "dispensaire sante dormir nuit"), 4)
  rt <- hstat_q_text_analysis(ph, "t", stem = TRUE)
  # aucune échelle fill/colour déclarée => défauts ggplot
  no_custom_scale <- function(p)
    !any(vapply(p$scales$scales,
                function(sc) any(c("fill", "colour") %in% sc$aesthetics), logical(1)))
  expect_true(no_custom_scale(rt$plotfns[["Sentiments"]]()))
  expect_true(no_custom_scale(rt$plotfns[["Mots fréquents"]]()))
  expect_true(no_custom_scale(rt$plotfns[["Nuage de mots"]]()))
  r <- hstat_q_nominal_univariate(c(rep("A", 6), rep("B", 3)), "V")
  expect_true(no_custom_scale(r$plotfns[["Diagramme en barres"]]()))
  # la personnalisation reste possible par-dessus
  p2 <- hstat_q_apply_palette(rt$plotfns[["Sentiments"]](), "viridis")
  expect_false(no_custom_scale(p2))
})


# =============================================================================
context("Analyses qualitatives -- Chi²/Multinomial et tableaux croisés intégrés")
# =============================================================================

test_that("le Chi² d'ajustement reproduit chisq.test et interprète ses métriques", {
  x <- c(rep("A", 60), rep("B", 25), rep("C", 15))
  r <- hstat_q_gof_analysis(x, "V", method = "chisq")
  expect_true(isTRUE(r$ok))
  ref <- suppressWarnings(chisq.test(c(60, 25, 15)))
  expect_equal(as.numeric(r$metrics$Valeur[r$metrics$Metrique == "Khi-deux"]),
               unname(round(ref$statistic, 3)))
  expect_true("Interpretation" %in% names(r$metrics))
  expect_true(all(nzchar(r$metrics$Interpretation)))
  expect_true(any(grepl("Chi-squared", r$console)))
  # proportions personnalisées conformes -> p ~ 1
  r2 <- hstat_q_gof_analysis(x, "V", expected_props = c(0.6, 0.25, 0.15))
  p2 <- r2$metrics$Valeur[r2$metrics$Metrique == "p-value (Khi-deux)"]
  expect_true(as.numeric(gsub("[^0-9.e-]", "", p2)) > 0.5)
  # nombre de proportions incorrect -> message clair
  r3 <- hstat_q_gof_analysis(x, "V", expected_props = c(0.5, 0.5))
  expect_false(isTRUE(r3$ok))
})

test_that("le test multinomial exact fournit une p-value et sa sortie console", {
  xs <- c(rep("A", 9), rep("B", 3), rep("C", 2))
  set.seed(11)
  r <- hstat_q_gof_analysis(xs, "V", method = "multinomial", B = 2000)
  expect_true(isTRUE(r$ok))
  expect_true("p-value (Multinomial exact)" %in% r$metrics$Metrique)
  p <- as.numeric(gsub("[^0-9.e-]", "",
        r$metrics$Valeur[r$metrics$Metrique == "p-value (Multinomial exact)"]))
  expect_true(p > 0 && p < 1)
  expect_true(any(grepl("multinomial exact", r$console)))
})

test_that("les tableaux croisés intégrés fournissent profils et métriques interprétées", {
  set.seed(2)
  x <- sample(c("H", "F"), 200, TRUE)
  y <- sample(c("Oui", "Non", "NSP"), 200, TRUE)
  r <- hstat_q_nominal_bivariate(x, y, "Sexe", "Reponse")
  expect_true(all(c("Table de contingence", "Profils ligne (%)",
                    "Profils colonne (%)", "Pourcentages du total (%)")
                  %in% names(r$tables)))
  expect_true("Interpretation" %in% names(r$metrics))
  expect_true(all(nzchar(r$metrics$Interpretation)))
  expect_true("Barres groupées (effectifs)" %in% names(r$plotfns))
  pc <- r$tables[["Profils colonne (%)"]]
  expect_true(all(abs(colSums(pc[, -1]) - 100) < 0.5))
})


# =============================================================================
context("Analyses qualitatives -- croisement d'une variable avec elle-même")
# =============================================================================

test_that("les graphiques croisés fonctionnent même si les variables ont le même nom", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  set.seed(1)
  x <- sample(c("A", "B", "C"), 120, TRUE)
  y <- sample(c("Oui", "Non"), 120, TRUE)
  # Cas qui provoquait l'erreur "duplicate columns" : xname == yname
  r <- hstat_q_nominal_bivariate(x, x, "V", "V")
  expect_true(isTRUE(r$ok))
  for (pn in names(r$plotfns)) {
    p <- r$plotfns[[pn]]()
    if (!is.null(p)) expect_silent(ggplot2::ggplot_build(p))
  }
  # Les axes de la carte des résidus restent corrects (x = Y, y = X)
  r2 <- hstat_q_nominal_bivariate(x, y, "GG", "RR")
  ph <- r2$plotfns[["Carte des résidus"]]()
  expect_equal(ph$labels$x, "RR")
  expect_equal(ph$labels$y, "GG")
})
