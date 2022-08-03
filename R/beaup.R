#' Petit p bien presente
#'
#' @param varp resultat du p
#' @param affp si 1 renvoie "p = 0,005" sinon "0,005"
#'
#' @return beaup , chaine de caractère
#'
#' @examples
#' pp <- cor.test(iris$Sepal.Length,iris$Sepal.Width)
#' beaup(pp$p.value)
#'
#' @export
beaup <- function(varp, affp = 0) {
  if (varp < 0.001) {
    if (affp == 1) {
      beaup <- "**p < 0,001**"
    }
    else {
      beaup <- "< 0,001"
    }
  }
  else {
    beaup <- round(varp, 2)
    if (affp == 1) {
      beaup <- paste0("p = ", beaup)
    }
  }
  return(beaup)
}
