# Calcul du nombre de cas nécessaires pour une enquête obsevationelle simple sans aucun test.Si il y a plusieurs questions prendre px = 0.5

Calcul du nombre de cas nécessaires pour une enquête obsevationelle
simple sans aucun test.Si il y a plusieurs questions prendre px = 0.5

## Usage

``` r
nb.obs.ph(px = 0.5, ex = 0.1, np = 1e+05)
```

## Arguments

- px:

  Proportion estimée des réponses (0.5)

- ex:

  Marge d'erreur consideree comme acceptable (0.1)

- np:

  Taille de la population totale (1e5)

## Value

nombre

## Examples

``` r
nb.obs.ph(px = 0.5, ex = 0.1, np = 1e5)
#> [1] 96
```
