# Utilitaires pour une petite étude clinique en R

<!-- badges: start -->  
  
[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)

<!-- badges: end -->

Les fonctions de base pour un projet simple de recherche clinique en R, particulièrement adapté pour une thèse ou un mémoire de DES. Si vous travaillez sur un PHRC ou une phase III ça va être un peu court !

Si tout cela vous effraie je peux vous aider. Plus de renseignements sur [https://docteur-michel.fr.latexr](https://docteur-michel.fr.latexr).

Vous pouvez me contacter via un simple mail à <a href="mailto:docphilmstat@gmail.com">docphilmstat@gmail.com</a> ou un message sur [Twitter](https://twitter.com/PhiippeMICHEL) & je vous répondrai dans les plus brefs délais.

## Installation

``` r
> library("remotes")
> remotes::install_github("philippemichel/baseph")
```

### Beaux noms

Pour les graphiques, possibilité d'avoir les intitulés vrais des variables & non les codes. Il vous faut alors créer une liste des *beaux intitulés*. C'est lors de l'import du csv principal par la fonction `debutph` que ces labels seront importés par exemple :

``` r
 > library(labelled)
 > bnom <- read.csv("datas/bnom.csv") # Import des intitulés (tableau à deux colonnes : 'nom' & 'code' par ex.).
 > bnom <- bnom$nom 
 > ttd <- debutph("datas/drepa2.csv", bnom) #Import des données avec insertion des labels.
```

Dans les fichiers d'exemple, les données (fictives) sont dans le fichier "patients" & les beaux noms dans le fichier "bnom", variable "nom".

### Tests statistiques
Les tests ou simplement l'affichage peuvent, pour les variables numériques, être présentés en moyenne ± écart-type ou en médiane (quartiles). Les variables discrètes sont rendues en n (%).

### Tableaux
La sortie sera meilleure pour du PDF via $\LaTeX$ si on passe par KableExtra par exemple via la fonction `gexptabph` (pour les tableaux trop larges mais *longtable* n'est pas géré) ou `pexptabph` qui permettent de gérer l'export en .ods au besoin & la sortie en *longtable*. On va donc avoir quelque chose comme :

``` r
 > library(gtsummary)
 > library (kableExtra)
 > patients |> 
      tbl_summary(...) |> 
      tabph(nomv = "Traitements", normx = TRUE) |>
      pexptabph(exp = FALSE,
                nomfich = "export.ods", 
                nomsheet = "demo",
                lg = FALSE)
```

### Autres aides :

-   **listrandph** listes de randomisation multicentre, blocs variables.
-   **beaup** Écrire un *p = 0,05* esthétique avec *p\< 0,001* si nécessaire.
-   **bmiph** Couper en facteurs un BMI numérique, version française & anglaise.
-   **tabph** Amélioration esthétique d'un tableau généré via `gtsummary` avec variable de tri & tests statistiques.
-   **debph** Chargement des libraries de base, réglage des tableaux pour les variables numériques (normales ou pas).

### Graphiques

-   **barconfph** Graphique en barres avec intervalle de confiance pour une variable numérique (y) & une variable factorielle de tri (x).
-   **barsimpleph** Graphique en barre exprimé en % des modalités pour une variable factorielle (x)
-   **bardecph** Même graphique que **barsimpleph** mais les % sont en ordre décroissant
-   **barouiph** Graphique en barre avec barres d'erreur. Une variable binaire (`oui/non`) exprimée en % d'une modalité (`oui` par ex.) pour chaque modalité de l'autre variable.
-   **vioboxph** Graphique en violon avec un box -plot intégré.
-   **pyrph** Pyramide des âges. La fonction *epiDisplay::pyramid()* donne aussi un très bon rendu. (en travaux)
-   **barpcph** Graphique en barre avec une variable en % pour chaque niveau de l'autre variable.
-   **lollipph** Graphique *lollipop* de distribution d'une donnée factorielle avec éventuelle mise en évidence d'un ou plusieurs niveaux.
-  **raincloud** Graphique en *raincloud* (densité + boxplot + nuage de points) pour une variable numérique & une variable factorielle de tri.

### Calcul du nombre de sujets nécessaires

-   **nbobsph** Enquête simple sans test.

## Jeu de données

Un jeu de données est fourni pour exemple :

-   **Patients** Escarres en réanimation chez la personne âgée (données fictives).
-   **bnom** Les labels *propres* des variables (pour utiliser avec le package `labelled`):
    -   `code` Code utilisé dans R (ex : `igs`)
    -   `nom` Beau label pour les tableaux, figures etc. (ex : `IGS 2`)

# ÉVOLUTION

### Branche TABLEAUX

Supprimée pour l'instant. J'avais écrit des fonctions pour gérer créer divers tableaux mais le package `gtsummary` est bien plus simple d'emploi avec de très beaux résultats. J'ai juste gardé les fonctions `pexptabph` & `gexptabph` pour la sortie via kableExtra & l'export en .ods & `tabph` qui met en forme le tableau généré par `gtsummary`.


