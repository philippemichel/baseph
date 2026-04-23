# Tableau Odds-Ratio, Risque Relatif & nb à traiter

Tableau Odds-Ratio, Risque Relatif & nb à traiter

## Usage

``` r
tab_oo(df, intervention, evenement)
```

## Arguments

- df:

  a tibble

- intervention:

  variable d'entree

- evenement:

  Variable de sortie (diagnostic)

## Value

a table

## Examples

``` r
tab_oo(patients, "sexe", "escarre")
#> <table class="table" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Mesure d'association </th>
#>    <th style="text-align:right;"> Estimation </th>
#>    <th style="text-align:left;"> IC 95% </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> Odds Ratio </td>
#>    <td style="text-align:right;"> 0,13 </td>
#>    <td style="text-align:left;"> [0,03; 0,66] </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Risque Relatif </td>
#>    <td style="text-align:right;"> 0,21 </td>
#>    <td style="text-align:left;"> [0,05; 0,84] </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Nombre a Traiter </td>
#>    <td style="text-align:right;"> 2,80 </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#> </tbody>
#> </table>
```
