#' Ligne numerique tableau descriptif
#'
#' @param x vecteur de donnees
#' @param nn nom du vecteur
#'
#' @import stats
#'
#' @return une ligne
#' @export
#'
#' @examples lignum (iris$Sepal.Length, "largeur petales")
#'
lignum <- function(x,nn){
  bornes <- moyciph(x, ci = 95)
  tbf <- paste0("[", signif(bornes[1],3), " ; ", signif(bornes[2],3), "]")
  ll <- c(nn,paste0(signif(mean(x),3), " \u00b1 " ,signif(sd(x),3)), tbf)
  return(ll)
}
