% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moyciph.R
\name{moyciph}
\alias{moyciph}
\title{Intervalle de Confiance par Bootstrap pour une moyenne}
\usage{
moyciph(varx, ci = 95)
}
\arguments{
\item{varx}{la variable à étudier}

\item{ci}{Intervalle de confiance en \% (95 par défaut)}
}
\value{
bornes inf & sup de l'IC
}
\description{
Intervalle de Confiance par Bootstrap pour une moyenne
}
\examples{
moyciph(patients$age, ci = 95)

}
