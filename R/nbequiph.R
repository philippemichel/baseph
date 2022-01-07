#' Nb de sujet  Test d'équivalence proportions
#' Calcul du nb de sujet par groupe pour
#' un test d'équivalence sur des proportions.
#'
#'
#'
#' @param po proportion sur le groupe controle
#' @param dl deviation jugee comme acceptable (0.1)
#' @param zalpha risque alpha (0.05)
#' @param zbeta risque beta (0.2)
#'
#' @return
#' @export
#'
#' @examples nb.equi.ph(po=0.5,dl = 0.1, zalpha = 0.05, zbeta = 0.2)
#'
nb.equi.ph <- function(po,
                       dl = 0.1,
                       zalpha = 0.05,
                       zbeta = 0.2) {
  pe <- po * (1 + dl)
  po <- po * (1 - po)
  pe <- pe * (1 - pe)
  za <- qnorm(1 - zalpha)
  zb <- qnorm(1 - (zbeta / 2))
  nnx <- (1 / dl^2) * (za * sqrt(po + pe) + zb * sqrt(2 * po))^2
  nnx <- floor(nnx) + 1
  return(nnx)
}
