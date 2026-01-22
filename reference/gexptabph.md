# Sortie d'un tableau gtsummary en tableau kableExtra avec adaptation à la page et comme feuille d'un tableur ods.

Sortie d'un tableau gtsummary en tableau kableExtra avec adaptation à la
page et comme feuille d'un tableur ods.

## Usage

``` r
gexptabph(dfk, exp = FALSE, nomfich = "export", nomsheet = "x", lg = FALSE)
```

## Arguments

- dfk:

  objet gtsummary

- exp:

  booleen. TRUE : export vers ods

- nomfich:

  nom du classeur ods

- nomsheet:

  nom de la feuille dans le classeur ods

- lg:

  Booleeen. TRUE si sortie en longtable

## Value

un tableau kbl prévu pour un export LaTeX

## Examples

``` r
zz <- gtsummary::tbl_summary(iris)
gexptabph(dfk = zz, exp = FALSE)
#> <table style="NAborder-bottom: 0; margin-left: auto; margin-right: auto;" class="table">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Caractéristique </th>
#>    <th style="text-align:center;"> N = 150 </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> Sepal.Length </td>
#>    <td style="text-align:center;"> 5,80 (5,10 – 6,40) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Sepal.Width </td>
#>    <td style="text-align:center;"> 3,00 (2,80 – 3,30) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Petal.Length </td>
#>    <td style="text-align:center;"> 4,35 (1,60 – 5,10) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Petal.Width </td>
#>    <td style="text-align:center;"> 1,30 (0,30 – 1,80) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Species </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;padding-left: 2em;" indentlevel="1"> setosa </td>
#>    <td style="text-align:center;"> 50 (33%) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;padding-left: 2em;" indentlevel="1"> versicolor </td>
#>    <td style="text-align:center;"> 50 (33%) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;padding-left: 2em;" indentlevel="1"> virginica </td>
#>    <td style="text-align:center;"> 50 (33%) </td>
#>   </tr>
#> </tbody>
#> <tfoot><tr><td style="padding: 0; " colspan="100%">
#> <sup>1</sup> Médiane (Q1 – Q3); n (%)</td></tr></tfoot>
#> </table>
```
