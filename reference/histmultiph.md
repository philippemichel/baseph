# Graphique en plusieurs histogrammes superposés, pratique pour bien visualiser les variations d'une distribution selon une modalité.

Graphique en plusieurs histogrammes superposés, pratique pour bien
visualiser les variations d'une distribution selon une modalité.

## Usage

``` r
histmultiph(dfx, varx, varn, tit = "", stit = 0, titx = "", bin = 1)
```

## Arguments

- dfx:

  data-frame ou tibble

- varx:

  Variable factorielle

- varn:

  variable numérique

- tit:

  Titre du graphique

- stit:

  valeur de p

- titx:

  Titre de l'axe horizontal

- bin:

  largeur d'une barre

## Value

un graphique

## Examples

``` r
histmultiph(dfx = iris, varx= Species,
varn = Sepal.Width, tit = "IRIS", stit = 0.002, titx = "Pétales (mm)", bin = 1)
#> Warning: Removed 5 rows containing missing values or values outside the scale range
#> (`geom_bar()`).

```
