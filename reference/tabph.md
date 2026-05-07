# Réglages pour un tableau généré par gtsummary (avec une variable de tri & des tests statistiques).

Réglages pour un tableau généré par gtsummary (avec une variable de tri
& des tests statistiques).

## Usage

``` r
tabph(tbx, nn = 2, nomv = "", normx = FALSE)
```

## Arguments

- tbx:

  un tableau gtsummary

- nn:

  nombre de valeurs dans la variable de tri

- nomv:

  nom à afficher pour la variable de tri

- normx:

  si variables numériques normales TRUE

## Value

un tableau fignolé

## Examples

``` r
tabph(gtsummary::tbl_summary(patients[, 1:4], by = sexe), nn = 2, nomv = "Sexe", normx = FALSE)
#> The following warnings were returned during `bold_p()`:
#> ! For variable `admission` (`sexe`) and "statistic", "p.value", and "parameter"
#>   statistics: Chi-squared approximation may be incorrect
#> ! For variable `lieudevie1` (`sexe`) and "statistic", "p.value", and
#>   "parameter" statistics: Chi-squared approximation may be incorrect


  

```
