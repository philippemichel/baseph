#' TNb de sujets pour un sondage
#' Calcul du nombre de cas necessaires pour une enquète obsevationelle
#' simple snas aucun test.
#' Si il y aplusiuers questions prendre px = 0.5
#'
#' @param px Proportion estimé des réponses (0.5)
#' @param ex Marge d'erreur considere comme acceptable (0.1)
#' @param np Taile de la population totale (1e5)
#'
#' @return
#' @export
#'
#' @examples nb.obs.ph(px = 0.5, ex = 0.1, np = 1e5)
nb.obs.ph <- function(px = 0.5, ex = 0.1, np = 1e5) {
  pp <- px * (1 - px)
  zz <- 1.96^2
  nb <- (zz * (pp) / (ex^2)) / (1 + ((zz * pp) / (ex^2 * np)))
  return(nb)
}
