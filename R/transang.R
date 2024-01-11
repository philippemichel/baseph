#' TRANSFORMATION ANGULAIRE
#' Calcul des bornes sup et inf d'un intervalle de confiance par transformation angulaire.
#' @param vv  nb evenement
#' @param ld taille echantillon
#' @param pc percent of IC (95 by default)
#'
#' @return binf = borne inferieur de l'IC,
#'         bsup = borne superieure de l'IC,
#'         nb = borne inf ; borne sup]
#'
#' @import stats
#'
#' @examples transangph(vv = 55, ld = 100, pc = 95)
#'
#' @export
#'
transangph <- function(nb , total, pr = 95) {
  pc <- qnorm((100 - (100 - pr) / 2) / 100)
  pz <- asin(sqrt(nb / total))
  pinf <- sin(pz - pc * sqrt((pz * (1 - pz)) / (total))) ^ 2 * 100
  psup <- sin(pz + pc * sqrt((pz * (1 - pz)) / (total))) ^ 2 * 100
  nb1 <-
    paste0(nb / total * 100, " [", signif(pinf, 3), " ; ", signif(psup, 3), "]")
  return(list(binf = pinf,
              bsup = psup,
              nb = nb1))
}
