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
#> ! For variable `age` (`sexe`) and "estimate", "statistic", "p.value",
#>   "conf.low", and "conf.high" statistics: cannot compute exact p-value with
#>   ties
#> ! For variable `age` (`sexe`) and "estimate", "statistic", "p.value",
#>   "conf.low", and "conf.high" statistics: cannot compute exact confidence
#>   intervals with ties
#> ! For variable `lieudevie1` (`sexe`) and "statistic", "p.value", and
#>   "parameter" statistics: Chi-squared approximation may be incorrect


  

```

**N**

**Total** n/N = 50¹

**Sexe**

**p-valeur**²

**f**  
N = 21¹

**m**  
N = 29¹

age

50

87 (86 – 90)

88 (86 – 91)

86 (85 – 89)

0,2

admission

49

  

  

  

0,2

    court séjour

  

26 (53%)

9 (45%)

17 (59%)

  

    smur

  

8 (16%)

6 (30%)

2 (6,9%)

  

    ssr

  

3 (6,1%)

1 (5,0%)

2 (6,9%)

  

    urgences

  

12 (24%)

4 (20%)

8 (28%)

  

    Manquant

  

1

1

0

  

lieudevie1

46

  

  

  

0,2

    Avec la famille

  

7 (15%)

3 (17%)

4 (14%)

  

    Domicile, seul

  

16 (35%)

5 (28%)

11 (39%)

  

    EHPAD

  

3 (6,5%)

2 (11%)

1 (3,6%)

  

    En couple

  

15 (33%)

4 (22%)

11 (39%)

  

    Maison de retraite

  

5 (11%)

4 (22%)

1 (3,6%)

  

    Manquant

  

4

3

1

  

¹ Médiane (Q1 – Q3); n (%)

² test de Wilcoxon-Mann-Whitney; test du khi-deux d’indépendance
