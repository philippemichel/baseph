# Barplot avec les intervalles de confiance pour une valeur donnée d'une variable factorielle selon les classes d'une variable factorielle de classification.

Barplot avec les intervalles de confiance pour une valeur donnée d'une
variable factorielle selon les classes d'une variable factorielle de
classification.

## Usage

``` r
barouiph(
  dfx,
  varx,
  testx,
  valx = "oui",
  titre = "",
  stitre = "",
  titx = "",
  tity = "%",
  capt = ""
)
```

## Arguments

- dfx:

  data.frame

- varx:

  variable à représenter (factoriel) y

- testx:

  Variable de tri (factoriel) x

- valx:

  valeur de test à présenter

- titre:

  Titre du graphique

- stitre:

  Sous-titre du graphique

- titx:

  titre (optionnel) de l'axe x

- tity:

  titre (% par défaut) de l'axe y

- capt:

  légende du graphique

## Value

graphique

## Examples

``` r
data(patients)
barouiph(dfx = patients, varx = escarre, testx = sexe, valx = "oui", titre = "Sexe")
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the baseph package.
#>   Please report the issue to the authors.

```
