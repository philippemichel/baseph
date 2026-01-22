# Graphique violon + boxplot

Graphique violon + boxplot

## Usage

``` r
vioboxph(
  dfx,
  varx,
  varnum,
  titre = "",
  stit = "",
  titx = "",
  tity = "n",
  lab = ""
)
```

## Arguments

- dfx:

  Tibble de référence

- varx:

  Variable catégorielle

- varnum:

  Variable numérique

- titre:

  Titre

- stit:

  Sous-titre

- titx:

  Titre de l'axe x

- tity:

  Titre de l'axe y

- lab:

  label

## Value

graphique

## Examples

``` r
data("patients")
vioboxph(patients, admission, age, titre ="grands", stit = "et grandes", titx = "Mode d'admission")
#> Ignoring unknown labels:
#> • label : ""
```
