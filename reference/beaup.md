# Petit p bien presenté

Petit p bien presenté

## Usage

``` r
beaup(varp, affp = 0)
```

## Arguments

- varp:

  résultat du p

- affp:

  si TRUE renvoie "p = 0,005" sinon "0,005"

## Value

beaup , chaine de caractère

## Examples

``` r
pp <- cor.test(iris$Sepal.Length, iris$Sepal.Width)
beaup(pp$p.value)
#> [1] "0.152"

```
