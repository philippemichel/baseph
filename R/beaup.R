#' Petit p bien presenté
#'
#' @param varp résultat du p
#' @param affp si TRUE renvoie "p = 0,005" sinon "0,005"
#'
#' @return beaup , chaine de caractère
#'
#' @export
#'
#' @examples
#' pp <- cor.test(iris$Sepal.Length, iris$Sepal.Width)
#' beaup(pp$p.value)
#'

#'
beaup <- function(varp, affp = 0) {
  if (varp < 0.0011) {
    if (affp) {
      beaup <- "p < 0,001"
    } else {
      beaup <- "< 0,001"
    }
  } else {
    beaup <- as.character(round(varp, 3))
    if (affp == 1) {
      beaup <- paste0("p = ", beaup)
    }
  }
  return(beaup)
}
