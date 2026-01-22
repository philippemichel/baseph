# Calcul des bornes sup et inf d'un intervalle de confiance par transformation angulaire.

Calcul des bornes sup et inf d'un intervalle de confiance par
transformation angulaire.

## Usage

``` r
transangph(nb, total, pr = 95, pcs = 3)
```

## Arguments

- nb:

  nb evenement

- total:

  taille echantillon

- pr:

  percent of IC (95 by default)

- pcs:

  nb décimales (2 by default)

## Value

binf = borne inferieur de l'IC, bsup = borne superieure de l'IC, nb =
borne inf ; borne sup\]

## Examples

``` r
transangph(nb = 55, total = 100, pr = 95)
#> $binf
#> [1] 0,4521588
#> 
#> $bsup
#> [1] 0,6459266
#> 
#> $nb
#> [1] "55% [45,2 ; 64,6]"
#> 
```
