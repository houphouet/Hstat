# Guide : transformer HStat en package R installable

## Fichiers fournis dans ce kit
- `DESCRIPTION` — métadonnées du package (à adapter : nom, email, licence, dépendances)
- `R/run_hstat.R` — fonction exportée `run_hstat()` qui lance l'app
- `NAMESPACE` — export minimal (sera régénéré par roxygen2)
- `.Rbuildignore` — fichiers à exclure du build (.Rproj, .RData, etc.)

## Étapes à suivre sur votre dépôt local

### 1. Créer la nouvelle arborescence
```
Hstat/
├── DESCRIPTION
├── NAMESPACE
├── .Rbuildignore
├── R/
│   └── run_hstat.R
└── inst/
    └── app/
        ├── HStat.R          <- votre code actuel (renommé si besoin, ex. app.R)
        ├── Server.R
        ├── UX.R
        ├── Utils.R
        ├── mod_clean.R
        ├── mod_descriptive.R
        ├── mod_design.R
        ├── mod_explore.R
        ├── mod_filter.R
        ├── mod_qualitative.R
        ├── mod_tests.R
        ├── mod_threshold.R
        ├── mod_viz.R
        └── www/
            ├── fonts/
            ├── hstat-theme.css
            └── Sortable.min.js
```

### 2. Commandes à exécuter (dans le dossier du dépôt cloné)

```bash
mkdir -p R inst/app
mv HStat.R Server.R UX.R Utils.R mod_*.R inst/app/
mv www inst/app/
```

Ensuite copiez les fichiers `DESCRIPTION`, `NAMESPACE`, `.Rbuildignore` et
`R/run_hstat.R` fournis dans ce kit à la racine de votre dépôt.

### 3. Adapter DESCRIPTION
- Remplacez `Authors@R` par vos vraies infos
- Dans `Imports:`, ajoutez **tous** les packages utilisés dans votre code
  (par exemple `dplyr`, `ggplot2`, `DT`, `shinydashboard`, `shinyjs`, etc.)
  Vous pouvez lister automatiquement les packages utilisés avec :
  ```r
  # install.packages("renv")
  renv::dependencies("inst/app")
  ```

### 4. Comment HStat.R lance l'app (confirmé via le README)
D'après votre README, l'app se lance avec `shiny::runApp("HStat.R")` :
c'est donc une "single-file Shiny app" qui se termine par un appel du
type `shinyApp(ui, server)`.

`run_hstat()` a été adapté en conséquence : il pointe directement vers
`inst/app/HStat.R` (et non vers le dossier), via :
```r
shiny::runApp(file.path(app_dir, "HStat.R"), ...)
```

**Point de vigilance** : si `HStat.R` contient des `source("Server.R")`,
`source("UX.R")`, `source("Utils.R")`, `source("mod_clean.R")`, etc. avec
des chemins **relatifs**, ça fonctionnera normalement sans rien changer,
car `shiny::runApp()` place automatiquement le répertoire de travail sur
le dossier du fichier lancé (donc `inst/app/`) pendant l'exécution de l'app.
Assurez-vous simplement que tous les fichiers `Server.R`, `UX.R`, `Utils.R`,
`mod_*.R` et le dossier `www/` restent bien **ensemble** dans `inst/app/`.

### 5. Régénérer la documentation et NAMESPACE avec roxygen2
```r
# install.packages("devtools")
devtools::document()
```

### 6. Vérifier que le package est valide
```r
devtools::check()
```
Corrigez les erreurs et warnings affichés.

### 7. Committer et pousser
```bash
git add .
git commit -m "Restructure HStat as an installable R package"
git push origin main
```

### 8. Tester l'installation depuis un autre poste (ou après un check)
```r
# install.packages("remotes")
remotes::install_github("houphouet/Hstat")
library(HStat)
run_hstat()
```

### 9. Tests (testthat)
Le fichier `tests/test-hstat.R` à la racine doit être **remplacé** par :
```
tests/
├── testthat.R
└── testthat/
    └── test-hstat.R
```
Ces deux fichiers sont fournis dans ce kit. Le fichier `test-hstat.R` a été
adapté pour localiser `Utils.R` et `mod_qualitative.R` via `system.file()`
(fiable une fois le package construit ou avec `devtools::load_all()`),
au lieu de chemins relatifs fragiles.

Supprimez l'ancien `tests/test-hstat.R` (à la racine) une fois la nouvelle
structure en place :
```powershell
Remove-Item tests\test-hstat.R
```

Lancez les tests avec :
```r
devtools::test()
```

## Corrections à faire aussi dans le workflow GitHub Actions
Votre fichier `.github/workflows/R.yml` actuel contient deux erreurs :
- `runs-on: macos-latest` seul → ajoutez `ubuntu-latest` et `windows-latest`
  pour tester sur plusieurs OS
- `r-version: ['3.6.3', '4.6.1']` → la version `4.6.1` n'existe pas encore ;
  utilisez par exemple `['4.3.3', '4.4.1']` ou `'release'`/`'devel'`

Exemple corrigé :
```yaml
strategy:
  fail-fast: false
  matrix:
    os: [ubuntu-latest, windows-latest, macos-latest]
    r-version: ['4.3.3', 'release']
runs-on: ${{ matrix.os }}
steps:
  - uses: actions/checkout@v4
  - name: Set up R ${{ matrix.r-version }}
    uses: r-lib/actions/setup-r@v2
    with:
      r-version: ${{ matrix.r-version }}
  - name: Install dependencies
    run: |
      install.packages(c("remotes", "rcmdcheck"))
      remotes::install_deps(dependencies = TRUE)
    shell: Rscript {0}
  - name: Check
    run: rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")
    shell: Rscript {0}
```
