## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(baseph)
data(esoph)

## ----tabd---------------------------------------------------------------------
tab1ph(esoph)

## ----tabc---------------------------------------------------------------------
tabcph(esoph,tobgp)


## ----rlog---------------------------------------------------------------------
mydata <- lm(carb~wt + am, data= mtcars)
tabmmph(ll =  mydata,titre = "Analyse multivariée", lab = "tabmulti", export = FALSE)


