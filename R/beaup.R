#' Petit p bien presente
#'
#' @param varp resultat du p
#'
#' @return beaup , chaine de caractère
#'
#' @examples
#' pp <- cor.test(iris$Sepal.Length,iris$Sepal.Width)
#' beaup(pp$p.value)
#'
#' @export
beaup <- function(varp) {
  if (varp < 0.001) {
    beaup <- "p < 0,001"
  }
  else {
    beaup <- paste0("p = ", round(varp, 3))
  }
  return(beaup)
}
