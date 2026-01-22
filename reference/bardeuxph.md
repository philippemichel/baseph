# Barplot 2 variables factorielles avec une variable de tri & une variable en % par modalité de tri

Barplot 2 variables factorielles avec une variable de tri & une variable
en % par modalité de tri

## Usage

``` r
bardeuxph(
  dfx,
  varp,
  vart,
  titre = "",
  stitre = "",
  xtitre = "",
  ltitre = "",
  capt = "",
  lab = "",
  angle = 0
)
```

## Arguments

- dfx:

  data.frame

- varp:

  variable à exprimer en % (factorielle)

- vart:

  variable factorielle de tri (axe des x)

- titre:

  Titre du graphique

- stitre:

  Sous titre

- xtitre:

  titre de l'axe x (vide par défaut)

- ltitre:

  titre de la légende

- capt:

  légende du graphique

- lab:

  label du graphique

- angle:

  angle affichage des valeurs de vart sur l'axe des x (0 par defaut)

## Value

un graphique

## Examples

``` r
data("patients")
bardeuxph(patients,escarre, admission ,
titre = "Escarre & mode d'admission",
stitre = "%", xtitre = "x", ltitre ="Escarre",
capt ="Escarre", lab = "aa", angle = 20)
#> Ignoring unknown labels:
#> • label : "aa"

```
