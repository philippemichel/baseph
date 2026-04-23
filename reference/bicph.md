# nbic

Affiche un nombre avec son intervalle de confiance

## Usage

``` r
bicph(nn, bb, hh, pc = 3)
```

## Arguments

- nn:

  nb brut

- bb:

  Borne basse de l'IC

- hh:

  Borne haute de l'IC

- pc:

  Nombre de chiffres affiche

## Value

chaine de caractère

## Examples

``` r
bicph(nn = 55, bb = 22, hh = 77, pc = 3)
#> [1] "55 [22 ; 77]"
```
