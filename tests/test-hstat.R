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
  # Empeche install_and_load de bloquer si un package manque dans l'env de test
  e <- new.env()
  assign("install_and_load", function(...) invisible(NULL), envir = e)
  suppressWarnings(suppressMessages(
    sys.source("Utils.R", envir = e, keep.source = FALSE)))
  # Exporter TOUTES les fonctions (y compris cachees, ex. .hstat_sql_stat_exprs)
  for (nm in ls(e, all.names = TRUE))
    assign(nm, get(nm, envir = e), envir = globalenv())
})


# =============================================================================
context("Detection du type de fichier")
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

test_that("hstat_format_size donne une valeur numerique plausible", {
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

test_that("hstat_set_seed retombe sur la graine par defaut si entree invalide", {
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
context("Chargeur de donnees -- chemin en memoire (CSV)")
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

test_that(".hstat_sql_stat_exprs genere les expressions attendues", {
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

test_that("describe_global DuckDB == calcul de reference R", {
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

test_that("crosstab DuckDB == table() de reference", {
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

test_that("correlation DuckDB == cor() de reference (Pearson)", {
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
