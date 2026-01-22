# Tracé d'un graphique en barre représentant la moyenne avec son intervalle de confiance à 95 % d'une valeur numérique découpé selon une variable factorielle.

Tracé d'un graphique en barre représentant la moyenne avec son
intervalle de confiance à 95 % d'une valeur numérique découpé selon une
variable factorielle.

## Usage

``` r
barconfph(
  dfx,
  varnum,
  vartri,
  titre = "",
  stitre = "",
  tx = "",
  ty = "n",
  lab = "",
  cap = "",
  angle = 0
)
```

## Arguments

- dfx:

  dataframe

- varnum:

  Variable numérique à afficher

- vartri:

  Variable de tri, factorielle

- titre:

  Titre du graphique (vide par défaut)

- stitre:

  Sous titre du graphique (vide par défaut)

- tx:

  Titre de l'axe des x (vide par défaut)

- ty:

  Titre de l'axe des x ("n" par défaut)

- lab:

  label (vide par défaut) rarement utile

- cap:

  caption(texte écrit en bas du graphique)

- angle:

  d'affichage des labels de l'axe x s'ils ont trop longs

## Value

un graphique

## Examples

``` r
data("patients")
barconfph(dfx = patients, varnum = igs2, vartri = admission, 
titre = "IGS II vs provenance", stitre = "en ICU", tx = "Mode d'admission", ty = "IGS II", 
cap = "Texte écrit petit",angle = 0)
#> Ignoring unknown labels:
#> • label : ""

```
