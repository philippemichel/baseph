# baseph

  <!-- badges: start -->
  [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  <!-- badges: end -->
  
Les fonctions de base pour un projet simple de recherche clinique en R, particuièrement adapté pour une thèse ou un mémoire de DES. 

# Plusieurs tableaux : 
- **tab1ph** Descriptif simple
- **tabcph** Comparaison simple
- **glmph** Régression logistique

Pour ces tableaux, possibilité d'avoir les intitulés vrais des variables & non les codes. Il vous faut alors créer une liste des "beaux intitulés" en csv qui doit être importé dans un data-frame.

# Autres aides : 
- **debutph** Importation d'un csv avec normalisation des noms de variables, colonnes de type *caractère* converties en *facteur* 
- **listrandph** listes de randomisation multicentre, blocs variables.
- **beaup** Écrire un *p = 0,05* esthétique avec *p< 0,001* si nécessaire.
- **bmiph** Couper en facteurs un BMI numérique, version française & anglaise.

# Graphiques
 - **barconfph** Graphique en barres avec intervalle de confiance pour une variable numérique & une variable factorielle de tri.
- **barsimpleph** Graphique en barre exprimé en % pour une variable factorielle.
- **bardeuxph** Graphique en barre. Une variable exprimée en % pour chaque modalité de l'autre.
- **bardecph**  Même graphique que bardeuxph mais les % sont en ordre décroissant
- **barouiph** Graphique en barre avec barres d'erreur. Une variable binaire (oui/non) exprimée en % d'une modalité (oui par ex.) pour chaque modalité de l'autre.
