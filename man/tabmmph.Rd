% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabmmph.R
\name{tabmmph}
\alias{tabmmph}
\title{Tableau multivarie}
\usage{
tabmmph(ll, titre = "Analyse multivariée", lab = "tabmulti", export = FALSE)
}
\arguments{
\item{ll}{Regression logistique}

\item{titre}{Titre - "Analyse multivariee" by default}

\item{lab}{Label - "tabmulti" by default}

\item{export}{TRUE : export csv (FALSE by default)}
}
\value{
un tableau LaTeX , tableau csv
}
\description{
Tableau multivarie
}
\examples{
mydata <- lm(carb~wt + am, data= mtcars)
tabmmph(ll =  mydata,titre = "Analyse multivariée", lab = "tabmulti", export = FALSE)

}
