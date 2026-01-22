# Dessin des crochets avec texte pour les p-value dans les graphiques

Dessin des crochets avec texte pour les p-value dans les graphiques

## Usage

``` r
figp(pp, x1, x2, yy, pval, od = FALSE, h = 1.1)
```

## Arguments

- pp:

  Un objet ggplot2

- x1:

  Numéro d'ordre de la première variable

- x2:

  Numéro d'ordre de la deuxième variable

- yy:

  Niveau vertical de la barre horizontale

- pval:

  p-value à afficher

- od:

  Décalage horizontal des crochets (si plusieurs niveaux)

- h:

  Écart barre horizontale-texte

## Value

graphique

## Examples

``` r
fp <- patients |>
  tidyr::drop_na(typemaladie, igs2) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = typemaladie, y = igs2)
ggplot2::geom_boxplot()
#> geom_boxplot: outliers = TRUE, outlier_gp = list(colour = NULL, fill = NULL, shape = NULL, size = NULL, stroke = 0.5, alpha = NULL), whisker_gp = list(colour = NULL, linetype = NULL, linewidth = NULL), staple_gp = list(colour = NULL, linetype = NULL, linewidth = NULL), median_gp = list(colour = NULL, linetype = NULL, linewidth = NULL), box_gp = list(colour = NULL, linetype = NULL, linewidth = NULL), notch = FALSE, notchwidth = 0.5, staplewidth = 0, varwidth = FALSE, na.rm = FALSE, orientation = NA
#> stat_boxplot: na.rm = FALSE, orientation = NA
#> position_dodge2 
figp(fp, x1 = 1, x2 = 2, yy = 95, pval = 0.01, od = FALSE, h = 2)

```
