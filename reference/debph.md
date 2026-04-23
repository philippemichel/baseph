# Réglages de base pour les tableaux. À éxécuter au début du document Quarto. Chargement de quleques libraries.

Réglages de base pour les tableaux. À éxécuter au début du document
Quarto. Chargement de quleques libraries.

## Usage

``` r
debph(param = FALSE)
```

## Arguments

- param:

  TRUE si variables normales (moyenne et écart-type), FALSE si variables
  non normales (médiane et quartiles)

## Value

affs (affichage des valeurs numériques)

## Examples

``` r
debph(param = FALSE)
#> Setting theme "language: fr"
#> [[1]]
#> all_continuous() ~ affpx
#> <environment: 0x564751424888>
#> 
#> [[2]]
#> all_categorical() ~ "{n} / {N} ({p}%)"
#> <environment: 0x564751424888>
#> 
```
