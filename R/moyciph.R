#' Intervalle de Confiance par Bootstrap pour une moyenne
#'
#' @param varx la variable à étudier
#' @param ci Intervalle de confiance en % (95 par défaut)
#'
#' @return bornes inf & sup de l'IC
#'
#' @import boot
#' @import stats
#'
#' @examples moyciph(patients$age, ci = 95)
#'
#' @export
moyciph <- function(varx, ci = 95) {
  ci <- ci / 100
  moy <- function(data, ind) {
    deb <- data[ind]
    moye <- mean(deb, na.rm = TRUE)
  }
  if (min(varx, na.rm = TRUE) == max(varx, na.rm = TRUE)) {
    bbr <- c(binf = NA, bsup = NA)
  } else {
    set.seed(1234)
    b1 <- boot::boot(varx, statistic = moy, R = 1000)
    bb <- boot::boot.ci(b1, conf = ci, type = "perc")
    binf <- bb$percent[4]
    bsup <- bb$percent[5]
    bbr <- c(binf = binf, bsup = bsup)
  }
  return(bbr)
}
