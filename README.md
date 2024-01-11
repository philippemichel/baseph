# baseph

<!-- badges: start --> [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)

<!-- badges: end -->

Les fonctions de base pour un projet simple de recherche clinique en R, particulièrement adapté pour une thèse ou un mémoire de DES. Si vous travaillez sur un PHRC ou une phase III ça va être un peu court !


La [vignette]("vignettes/my-vignette.html")

\## Installation

``` r
> library("remotes")
> remotes::install_github("https://github.com/philippemichel/baseph")
```

Pour les graphiques, possibilité d'avoir les intitulés vrais des variables & non les codes. Il vous faut alors créer une liste des "beaux intitulés". C'est lors de l'import du csv principal par la fonction `debutph` que ces labels seront importés par exemple :

``` r
 > library(labelled)
 > bnom <- read.csv("datas/bnom.csv") # Import des intitulés (tableau à deux colonnes : 'nom' & 'code' par ex.).
 > bnom <- bnom$nom 
 > ttd <- debutph("datas/drepa2.csv", bnom) #Import des données avec insertion des labels.
```

Dans les fichiers d'exemple, les données (fictives) sont dans le fichier "patients" & les beaux noms dans le fichier "bnom", variable "nom".

Les tests ou simplement l'affichage peuvent, pour les variables numériques, être présentés en moyenne ± écart-type ou en médiane (quartiles). Les variables discrètes sont rendues en n (%).

La sortie sera meilleure pour du PDF via $\LaTeX$ si on passe par KableExtra par exemple via la fonction `gexptabph` (pour les tableaux trop larges mais *longtable* n'est pas géré) ou `pexptabph` qui permettent de gérer l'export en .xls au besoin & la sortie en *longtable*. On va donc avoir quelque chose comme :

``` r
 > library(gtsummary)
 > library (kableExtra)
 > patients |> 
      tbl_summary(...) |> 
      pexptabph(exp = FALSE,
                nomfich = "export.xls",
                nomsheet = "demo",
                lg = FALSE)
```

## Autres aides :

-   **debutph** Importation d'un csv avec normalisation des noms de variables, colonnes de type *caractère* converties en *facteur*. Un vecteur doit être présent contenant les labels corrects pour les tableaux & les figures.
-   **listrandph** listes de randomisation multicentre, blocs variables.
-   **beaup** Écrire un *p = 0,05* esthétique avec *p\< 0,001* si nécessaire.
-   **bmiph** Couper en facteurs un BMI numérique, version française & anglaise.

# Graphiques

-   **barconfph** Graphique en barres avec intervalle de confiance pour une variable numérique (y) & une variable factorielle de tri (x).
-   **barsimpleph** Graphique en barre exprimé en % des modalités pour une variable factorielle (x)
-   **bardecph** Même graphique que **barsimpleph** mais les % sont en ordre décroissant
-   **barouiph** Graphique en barre avec barres d'erreur. Une variable binaire (`oui/non`) exprimée en % d'une modalité (`oui` par ex.) pour chaque modalité de l'autre variable.
-   **vioboxph** Graphique en violon avec un box -plot intégré.
-   **pyrph** Pyramide des âges. La fonction *epiDisplay::pyramid()* donne aussi un très bon rendu. (en travaux)
-   **barpcph** Graphique en barre avec une variable en % pour chaque niveau de l'autre variable.
-   **lollipph** Graphique *lollipop* de distribution d'une donnée factorielle avec éventuelle mise en évidence d'un ou plusieurs niveaux.

## Calcul du nombre de sujets nécessaires

-   **nbobsph** Enquète simple

## Jeu de données

Un jeu de données est fourni pour exemple :

-   **Patients** Escarres en réaniamtion chez la personne âgée (données fictives).
-   **bnom** Les labels *propres* des variables:
    -   `code` Code utilisé dans R (ex : `igs`)
    -   `nom` Beau label pour les tableaux, figures etc. (ex : `IGS 2`)

## ÉVOLUTION

### Branche TABLEAUX

Supprimée pour l'instant. J'avais écrit des fonctions pour gérer créer divers tableaux mais le package `gtsummary` est bien plus simple d'emploi avec de très beaux résultats. J'ai juste garder les fonctions `pexptabph` & `gexptabph` pour la sortie via kableExtra & l'export en .xls.

### À FAIRE

Des modèles en Quarto ou RMarkdown pour :

-   des plans d'analyse
-   un rapport pour rendre les résultats, complet
-   un rapport plus simple proche du chapitre *Résultats* qui sera dans l'article ou la thèse.

Tout ça en deux versions :

-   Étude observationnelle simple
-   Étude comparant deux groupes, interventionnelle ou non.
