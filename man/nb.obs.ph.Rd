% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbobsph.R
\name{nb.obs.ph}
\alias{nb.obs.ph}
\title{Calcul du nombre de cas nécessaires pour une enquête obsevationelle
simple sans aucun test.Si il y a plusieurs questions prendre px = 0.5}
\usage{
nb.obs.ph(px = 0.5, ex = 0.1, np = 1e+05)
}
\arguments{
\item{px}{Proportion estimée des réponses (0.5)}

\item{ex}{Marge d'erreur consideree comme acceptable (0.1)}

\item{np}{Taille de la population totale (1e5)}
}
\value{
nombre
}
\description{
Calcul du nombre de cas nécessaires pour une enquête obsevationelle
simple sans aucun test.Si il y a plusieurs questions prendre px = 0.5
}
\examples{
nb.obs.ph(px = 0.5, ex = 0.1, np = 1e5)

}
