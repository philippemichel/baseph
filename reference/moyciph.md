# Intervalle de Confiance par Bootstrap pour une moyenne

Intervalle de Confiance par Bootstrap pour une moyenne

## Usage

``` r
moyciph(varx, ci = 95)
```

## Arguments

- varx:

  la variable à étudier

- ci:

  Intervalle de confiance en % (95 par défaut)

## Value

bornes inf & sup de l'IC

## Examples

``` r
moyciph(patients$age, ci = 95)
#>     binf     bsup 
#> 87,10051 88,60000 
```
