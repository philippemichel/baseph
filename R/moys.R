#' Présentation moyenne  ecart-type
#'
#' @param x variable numerique
#'
#' @import stats
#'
#' @return moyenne +/- ecart-type
#' @export
#'
#' @examples moys(patients$age)
moys <- function(x) {
  mm <- signif(mean(x, na.rm = TRUE), 3)
  ss <- signif(sd(x, na.rm = TRUE), 3)
  ligm <- paste0(mm, " \u00b1 ", ss)
  return(ligm)
}


#' Présentation mediane  quantiles
#'
#' @param x variable numerique
#'
#' @import stats
#'
#' @return mediane (quartiles)
#' @export
#'
#' @examples meds(patients$age)
meds <- function(x) {
  mm <- quantile(x, na.rm = TRUE)
  medx <- signif(mm[[3]], 3)
  binf <- signif(mm[[2]], 3)
  bsup <- signif(mm[[4]], 3)
  ligm <- paste0(medx, " (", binf, ";", bsup, ")")
  return(ligm)
}
