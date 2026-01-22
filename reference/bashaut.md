# Calcul de l'intervalle de confiance à 95 % de la moyenne d'une variable numérique (utilisé par barconfph)

Calcul de l'intervalle de confiance à 95 % de la moyenne d'une variable
numérique (utilisé par barconfph)

## Usage

``` r
bashaut(xx)
```

## Arguments

- xx:

  Une variable numérique

## Value

deux nombres : la borne inf et la borne sup de l'IC

## Examples

``` r
data("patients")
bashaut(xx = patients$age)
#> $binf
#> [1] 87.06026
#> 
#> $bsup
#> [1] 88.65974
#> 
```
