#' Affiche un nombre avec son intervalle de confiance
#'
#' @param nn nb brut
#' @param bb Borne basse de l'IC
#' @param hh Borne haute de l'IC
#' @param pc Nombre de chiffres affiche
#'
#' @returns chaine de caractère
#' @export
#'
#' @examples bicph(nn = 55, bb = 22, hh = 77, pc = 3)
bicph <- function(nn, bb, hh, pc = 3) {
  nn <- signif(nn, pc)
  bb <- signif(bb, pc)
  hh <- signif(hh, pc)
  ll <- paste0(nn, " [", bb, " ; ", hh, "]")
  return(ll)
}
