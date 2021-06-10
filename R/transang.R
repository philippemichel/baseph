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
transangph <- function(vv, ld, pc = 95) {
  pp <- vv / ld
  sp <- 1.959 / (2 * sqrt(ld))
  pinf <- sin(asin(sqrt(pp - 1 / (2 * ld))) - sp) ^ 2 * 100
  psup <- sin(asin(sqrt(pp + 1 / (2 * ld))) + sp) ^ 2 * 100
  nb1 <- paste0(vv, "/", ld, " (", signif(pp * 100, 2), " \\%)")
  nb2 <- paste0(" [", signif(pinf, 3), " ; ", signif(psup, 3), "]")
  return(list(
    binf = pinf,
    bsup = psup,
    nb = nb2
  ))
}
