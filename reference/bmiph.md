# Coupe un vecteur BMI ou IMC (numeric) en facteurs avec les limites usuelles, en français ou en anglais (les limites sont différentes)

Coupe un vecteur BMI ou IMC (numeric) en facteurs avec les limites
usuelles, en français ou en anglais (les limites sont différentes)

## Usage

``` r
bmiph(bmi, lang = "fr")
```

## Arguments

- bmi:

  BMI numérique

- lang:

  Langue de sortie (french or english (fr/eng)). default : french

## Value

vecteur factoriel

## Examples

``` r
imc <- c(18,25,40,32)
bmiph(imc, "fr")
#> [1] maigreur        surpoids        obésité morbide obésité modérée
#> 7 Levels: dénutrition maigreur normal surpoids ... obésité morbide
```
