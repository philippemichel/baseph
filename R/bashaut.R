#' bashaut
#'
#' Calcul de l'intervalle de confiance à 95 % de la moyenne d'une variable numérique (utilisé par barconfph)
#'
#' @param xx Une variable numérique
#' @param bh 1 : borne basse, 2 : borne haute
#'
#' @return un nombre simple
#' @export
#'
#' @examples data ("patients")
#' bashaut(xx =patients$age, bh = 1)
#'
#'
bashaut <- function(xx,bh){
  zz <- t.test(xx)
  zz <- zz$conf.int[[bh]]
  return(zz)
}
