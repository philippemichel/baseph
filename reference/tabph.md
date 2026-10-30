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
#> Error in order(ux, method = "radix", na.last = NA): Character encoding must be UTF-8, Latin-1 or bytes
```
