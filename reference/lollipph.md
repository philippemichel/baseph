# Tracé d'un graphique "lollipop" de distribution d'une variable factorielle avec possibilité de mise en évidence d'un ou plusieurs facteurs.

Tracé d'un graphique "lollipop" de distribution d'une variable
factorielle avec possibilité de mise en évidence d'un ou plusieurs
facteurs.

## Usage

``` r
lollipph(dfx, nom, tri = "xx", titre = "", capt = "x")
```

## Arguments

- dfx:

  data-frame

- nom:

  colonne à exposer (factorielle)

- tri:

  niveaux de "nom" à mettre en évidence

- titre:

  titre

- capt:

  légende

## Value

graphique

## Examples

``` r
lollipph(dfx = patients, nom = lieudevie1, 
tri = c("EHPAD","Maison de retraite"), 
titre = "Lieu de vie", capt = "Lieu de vie avant l'hospitalisation")

```
