#' Présentatioj moyenne  ecart-type
#'
#' @param x variable numerique
#'
#' @import stats
#'
#' @return moyenne +/- ecart-type
#' @export
#'
#' @examples moys(iris$Sepal.Length)
moys <- function(x) {
  mm <- signif(mean(x, na.rm = TRUE), 3)
  ss <- signif(sd(x, na.rm = TRUE), 3)
  ligm <- paste0(mm, " \u00b1 ", ss)
  return(ligm)
}
