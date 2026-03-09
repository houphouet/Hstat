# HStat — Application Shiny d'Analyse Statistique

HStat est une application web interactive développée avec R Shiny permettant de réaliser un pipeline complet d'analyse de données, de l'importation jusqu'aux analyses multivariées avancées, sans écrire une seule ligne de code.

---

## Prérequis

- **R** ≥ 4.2.0
- **RStudio** (recommandé) ou tout autre environnement R
- Connexion Internet pour l'installation automatique des packages (première exécution)

---


### 1. Lancer l'application

Ouvrez RStudio, puis dans la console :

```r
shiny::runApp("app.R")
```

Les packages s'installent automatiquemment si nécessaire.
---

## Structure du projet

```
HStat/
├── app.R        # Point d'entrée : source les modules et lance shinyApp()
├── Utils.R      # Encodage, installation des packages, fonctions utilitaires
├── UX.R         # Interface utilisateur (objet `ui`)
├── Server.R     # Logique serveur (fonction `server`)
└── README.md    # Ce fichier
```


## Licence

Ce projet est opensource. 

---

*HStat est développé pour faciliter l'analyse statistique sans barrière de programmation.*
