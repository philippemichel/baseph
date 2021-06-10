#' Ligne numerique
#'
#' @param x vecteur de donnees
#' @param nn nom du vecteur
#'
#' @import stats
#'
#' @return
#' @export
#'
#' @examples lignum (iris$Sepal.Length, "largeur petales")
#'
lignum <- function(x,nn){
  bornes <- moyciph(x, ci = 95)
  tbf <- paste0("[", bornes[1], " ; ", bornes[2], "]")
  ll <- c(nn,paste0(signif(mean(x),3), " \u00b1 " ,signif(sd(x),3)), tbf)
  return(ll)
}
