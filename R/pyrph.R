#' Pyramide des ages
#' Trace une pyramide des ages simple.
#'
#' @param dfx data.frame contenant les valeurs
#' @param age valeur des ages , idealement divise en classes
#' @param sexe variable factorielle binaire
#' @param ff valeur de la classe 'Feminin"
#' @param mm valeur de la classe "masculin"
#' @param titre Titre du graphique
#'
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples aa <- runif(100,0,100)
#'           aa <- cut(aa,seq(0,100,10))
#'           sx <- c(rep("F",50),rep("M",50))
#'           df <- data.frame(aa,sx)
#'           pyrph(dfx = df, age = aa, sexe = sx, ff = "F", mm = "M", titre = "Pyramide des âges")
#'
)
