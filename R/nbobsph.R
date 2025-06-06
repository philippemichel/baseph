#' Calcul du nombre de cas nécessaires pour une enquête obsevationelle
#' simple sans aucun test.Si il y a plusieurs questions prendre px = 0.5
#'
#' @param px Proportion estimée des réponses (0.5)
#' @param ex Marge d'erreur consideree comme acceptable (0.1)
#' @param np Taille de la population totale (1e5)
#'
#' @return nombre
#' @export
#'
#' @examples nb.obs.ph(px = 0.5, ex = 0.1, np = 1e5)
#'
nb.obs.ph <- function(px = 0.5, ex = 0.1, np = 1e5) {
  pp <- px * (1 - px)
  zz <- 1.96^2
  nb <- (zz * (pp) / (ex^2)) / (1 + ((zz * pp) / (ex^2 * np)))
  nb <- floor(nb) + 1
  return(nb)
}
