# baseph

  <!-- badges: start -->
  [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  
  [![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)
  
  <!-- badges: end -->
  
Les fonctions de base pour un projet simple de recherche clinique en R, particulièrement adapté pour une thèse ou un mémoire de DES. Si vous travaillez sur un PHRC ou une phase III ça va être un peu court !

 ## Installation
 
 ```R
 > library("remotes")
 > remotes::install_github("https://github.com/philippemichel/baseph")
```

## Plusieurs tableaux : 
- **tab1ph** Descriptif simple
- **tabcph** Comparaison simple
- **tabregph** Toute régression

Pour ces tableaux (ainsi que pour les graphiques), possibilité d'avoir les intitulés vrais des variables & non les codes. Il vous faut alors créer une liste des "beaux intitulés". C'est lors de l'import du csv principal par la fonction `debutph` que ces labels seront importés par exemple :

```R
 > library(labelled)
 > bnom <- read.csv("datas/bnom.csv") # Import des intitulés (tableau à deux colonnes : 'nom' & 'code' par ex.).
 > bnom <- bnom$nom 
 > ttd <- debutph("datas/drepa2.csv", bnom) #Import des données avec insertion des labels.
```

Dans les fichiers d'exemple, les données (fictives) sont dans le fichier "patients" & les beaux noms dans le fichier "bnom", variable "nom".

Les tests ou simplement l'affichage peuvent, pour les variables numériques, être présentés en moyenne ± écart-type ou en médiane (quartiles). Les variables discrètes sont rendues en n (%). 


## Autres aides : 
- **debutph** Importation d'un csv avec normalisation des noms de variables, colonnes de type *caractère* converties en *facteur*. Un vecteur doit être présent contenant les labels corrects pour les tableaux & les figures.
- **listrandph** listes de randomisation multicentre, blocs variables.
- **beaup** Écrire un *p = 0,05* esthétique avec *p< 0,001* si nécessaire.
- **bmiph** Couper en facteurs un BMI numérique, version française & anglaise.

# Graphiques
 - **barconfph** Graphique en barres avec intervalle de confiance pour une variable numérique (y) & une variable factorielle de tri (x).
- **barsimpleph** Graphique en barre exprimé en %  des modalités pour une variable factorielle (x) 
- **bardecph**  Même graphique que **barsimpleph** mais les % sont en ordre décroissant
- **bardeuxph** Graphique en barre. Une variable exprimée en % (y) pour chaque modalité (x) de l'autre.
- **barouiph** Graphique en barre avec barres d'erreur. Une variable
  binaire (`oui/non`) exprimée en % d'une modalité (`oui` par ex.) pour
  chaque modalité de l'autre variable.
- **vioboxph** Graphique en violon avec un box -plot intégré.
- **pyrph** Pyramide des âges. La fonction *epiDisplay::pyramid()* donne aussi un très bon rendu. (en travaux)
- **barpcph** Graphique en barre avec une variiable en % pour chauqe niveau de l'autre variable.

## Calcul du nombre de sujets nécessaires

- **nbobsph** Enquète simple
- **nb.equi.ph** Étude d'équivalence
- **nb.obs.ph** Étude de non-infériorité

## Jeu de données

Un jeu de données est fourni pour exemple : 

- **Patients** Escarres en réniamtion chez la personne âgée (données fictives).
- **bnom** Les labels *propres* des variables: 
    - `code` Code utilisé dans R (ex : `igs`)
    - `nom` Beau label pour les tableaux, figures etc.  (ex : `IGS 2`)
    
## ÉVOLUTION

### Branche TABLEAUX

01/02/2023 Réécriture des fonctions gérant des tableaux sans utiliser `gtsummary`, très bon package mais trop rigide pour moi.


### À faire

Des modèles en Quarto ou RMarkdown pour :

- des plans d'analyse
- un rapport pour rendre les résultats, complet
- un rapport plus simple proche du chapitre *Résultats* qui sera dans l'article ou la thèse. 

Tout ça en deux versions : 

- Étude observationnelle simple
- Étude comparant deux groupes, interventionnelle ou non. 
