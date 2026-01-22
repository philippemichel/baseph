# Barplot décroissant en %. Trace un barplot d'une variable factorielle, axe des y en %, classement des items en ordre décroissant.

Barplot décroissant en %. Trace un barplot d'une variable factorielle,
axe des y en %, classement des items en ordre décroissant.

## Usage

``` r
bardecph(dfx, varx, titre = "", stitre = "", capt = "", lab = "", angle = 0)
```

## Arguments

- dfx:

  data.frame ou yibble

- varx:

  nom de la variable à traiter (factorielle)

- titre:

  Titre du graphique

- stitre:

  Soustitre du graphique

- capt:

  légende du graphique

- lab:

  label du graphique

- angle:

  angle d'affichage des valeurs de vart sur l'axe des x (0 par défaut)

## Value

un graphique

## Examples

``` r
data("patients")
bardecph(patients, admission, titre = "Mode d'admission",
stitre = "%", capt = "Mode d'admission", lab ="ma", angle = 0)
#> Ignoring unknown labels:
#> • label : "ma"

```
