#' Ligne facteurs
#'
#' @param x vecteur de donnees
#' @param nn nom du vecteur
#'
#' @import stats
#'
#' @return
#' @export
#'
#' @examples ligfact (iris$Species, "Especes")
#'
ligfact <- function(x,nn){
  ttz <- c(nn, " "," ")
  zz <- table(x)
  tot <- length(x)
  nl <- length(levels(x))
  for(l in 1:nl){
    nom <- names(zz)[l]
    nz <- zz[[1]]
    pc <- signif(100 * nz/tot,3)
    #
    cf <- transangph(nz, tot)
    #
    ll <- c(nom,paste0(nz,"/",tot, " (",pc," %)"),cf$nb)
    ttz <- rbind(ttz,ll)
  }
  return(list(ttz = ttz,nl = nl))
}
