# Barplot : une variable factorielle, axe des y en %

Barplot : une variable factorielle, axe des y en %

## Usage

``` r
barsimpleph(dfx, varx, titre = "", stitre = "", capt = "", lab = "", angle = 0)
```

## Arguments

- dfx:

  data.frame

- varx:

  nom de la variable à traiter (factorielle)

- titre:

  Titre du graphique

- stitre:

  Sous-titre du graphique

- capt:

  légende du graphique

- lab:

  label du graphique

- angle:

  angle d'affichage des valeurs de vart sur l'axe des x (0 par defaut)

## Value

un graphique

## Examples

``` r
data(patients)
barsimpleph(dfx = patients, varx= admission,
titre = "Mode d'admission", stitre ="ICU",
capt ="Admission", lab = "aa", angle = 10)
#> Ignoring unknown labels:
#> • label : "aa"

```
