# Graphique en "raincloud"

Graphique en "raincloud"

## Usage

``` r
raincloudph(df, vcat, vnum, titre = "", titcat = "", titnum = "", adj = 1)
```

## Arguments

- df:

  dataframe

- vcat:

  variable de tri (catégorielle)

- vnum:

  variable à afficher (numérique)

- titre:

  titre du graphique

- titcat:

  titre de la variable catégorielle

- titnum:

  titre de la variable numérique

- adj:

  facteur d'ajustement de la courbe de densité (1 par défaut)

## Value

graphique

## Examples

``` r
raincloudph(df = patients, vcat = admission, vnum = igs2, 
titre = "IGS2 vs adm", titcat = "Adm", titnum = "IGS 2", adj = 1)
```
