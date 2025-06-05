#' Calcul de l'intervalle de confiance à 95 % de la moyenne d'une variable numérique (utilisé par barconfph)
#'
#' @param xx Une variable numérique

#'
#' @return deux nombres : la borne inf et la borne sup de l'IC
#' @export
#'
#' @examples data("patients")
#' bashaut(xx = patients$age)
#'
bashaut <- function(xx) {
  zz <- t.test(xx)
  zz <- list(binf = zz$conf.int[[1]], bsup = zz$conf.int[[2]])
  return(zz)
}
