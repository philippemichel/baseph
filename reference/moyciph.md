# Intervalle de Confiance par Bootstrap pour une moyenne

Intervalle de Confiance par Bootstrap pour une moyenne

## Usage

``` r
moyciph(varx, ci = 95)
```

## Arguments

- varx:

  numeric variable

- ci:

  Intervalle de confiance en % (95 par défaut)

## Value

bornes inf & sup of confidence interval, mean ci

## Examples

``` r
moyciph(patients$age, ci = 95)
#>                 binf                 bsup                  mic 
#>   "87,1005081014161"               "88,6" "87,9 [87,1 ; 88,6]" 
```
