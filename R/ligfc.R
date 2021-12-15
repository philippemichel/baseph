#' ligne facteurs tableau comparatif
#'
#' @param nom nom de la ligne
#' @param varp variable explicative (facteur)
#' @param trip variable expliquee (facteur)
#'
#' @import stats
#'
#' @return ligne complète avec p
#' @export
#'
#' @examples ligfc("petale",iris$Species,iris$Species)
ligfc <- function(nom, varp, trip) {
  tabp <- table(varp, trip)
  chip <- chisq.test(tabp, correct = TRUE)[[3]]
  chip <- chisq.test(tabp, correct = FALSE)
  if (min(chip$expected)<5){chip <- chisq.test(tabp, correct = TRUE)}
  chip <- chip[[3]]
  if (is.na(chip)){
    chip <- fisher.test(tabp)[[1]]
  }
  chip <- beaup(chip)
  ltri <- length(levels(trip))
  lvar <- length(levels(varp))
  tabg <- c(nom, rep(" ", ltri), chip)
  #
  ss <- colSums(tabp)
  for (lig in 1:lvar) {
    ligt <- levels(varp)[lig]
    for (cas in 1:ltri) {
      casx <- tabp[lig, cas]
      casp <- round(100*casx/ss[cas],1)
      cast <- paste0(casx,"/",ss[cas]," (",casp," %)" )
      ligt <- c(ligt,cast)
    }
    ligt <- c(ligt, " ")
    tabg <- rbind(tabg, ligt)
  }
  return(tabg)
}
