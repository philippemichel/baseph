#' Calcul des bornes sup et inf d'un intervalle de confiance par transformation angulaire.
#' @param nb  nb evenement
#' @param total taille echantillon
#' @param pr percent of IC (95 by default)
#'
#' @return binf = borne inferieur de l'IC,
#'         bsup = borne superieure de l'IC,
#'         nb = borne inf ; borne sup]
#'
#' @import stats
#'
#' @examples transangph(nb = 55, total = 100, pr = 95)
#'
#' @export
#'
transangph <- function(nb, total, pr = 95) {
  pc <- qnorm((100 - (100 - pr) / 2) / 100)
  pz <- asin(sqrt(nb / total))
  pinf <- sin(pz - pc * sqrt(1 / (4 * total)))^2
  psup <- sin(pz + pc * sqrt(1 / (4 * total)))^2
  nb1 <-
    paste0(signif(nb / total * 100, 3), "% [", signif(pinf, 3), " ; ", signif(psup, 3), "]")
  return(list(
    binf = pinf,
    bsup = psup,
    nb = nb1
  ))
}
