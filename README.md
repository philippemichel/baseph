# baseph

  <!-- badges: start -->
  [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  <!-- badges: end -->
  
Les fonctions de base pour un projet simple de recherche clinique en R, particulièrement adapté pour une thèse ou un mémoire de DES. 

# Plusieurs tableaux : 
- **tab1ph** Descriptif simple
- **tabcph** Comparaison simple
- **glmph** Régression logistique

Pour ces tableaux, possibilité d'avoir les intitulés vrais des variables & non les codes. Il vous faut alors créer une liste des "beaux intitulés" en csv qui doit être importé dans un data-frame.
Dans les fichiers d'exemple, les données (fictives) sont dans le fichier "patients" & les beaux noms dans le fichier "nn", variable "nom".

On peut aussi sélectionner les variables à afficher dans les tableaux tab1ph & tabcph. 



# Autres aides : 
- **debutph** Importation d'un csv avec normalisation des noms de variables, colonnes de type *caractère* converties en *facteur* 
- **listrandph** listes de randomisation multicentre, blocs variables.
- **beaup** Écrire un *p = 0,05* esthétique avec *p< 0,001* si nécessaire.
- **bmiph** Couper en facteurs un BMI numérique, version française & anglaise.

# Graphiques
 - **barconfph** Graphique en barres avec intervalle de confiance pour une variable numérique (y) & une variable factorielle de tri (x).
- **barsimpleph** Graphique en barre exprimé en %  des modalités pour une variable factorielle (x) 
- **bardecph**  Même graphique que **barsimpleph** mais les % sont en ordre décroissant
- **bardeuxph** Graphique en barre. Une variable exprimée en % (y) pour chaque modalité (x) de l'autre.
- **barouiph** Graphique en barre avec barres d'erreur. Une variable binaire (oui/non) exprimée en % d'une modalité (oui par ex.) pour chaque modalité de l'autre.
- **pyrph** Pyramide des âges. La fonction *epiDisplay::pyramid()* donne aussi un très bon rendu. (en travaux)

# Calcul du nombre de sujets nécessaires

- **nbobsph** Enquète simple
- **nb.equi.ph** Étude d'équivalence
- **nb.obs.ph** Étude de non-infériorité
