#' Barplot avec barres d'erreur
#' Trace un barplot avec les intervalles de confiance pour un tableau de deux variables numeriques
#'
#' @param varx cas a représenter
#' @param testx Variable de tri
#' @param valx valeur de testx a presenter
#' @param tit titre du graphique
#'
#'@import binom
#'@import ggplot2
#' @import dplyr
#'
#' @return graphique
#' @export
#'
#' @examples xx <- c("A","A","B","A","B","C","A","C")
#'           yy <- c(rep("oui",4),rep("non",4))
#'          barconfph(varx=xx, testx= yy, valx = "non", tit = "essai")
#'
barconfph <- function(varx, testx, valx = "oui", tit = "titre") {
  varx <- as.factor(varx)
  testx <- as.factor(testx)
  ll <- which(valx == levels(testx))
  zz <- table(testx, varx)
  tzz <- colSums(zz)
  szz <- zz[ll, ]
  aa <- binom::binom.confint(szz, tzz, method = "exact")
  aa <- data.frame(aa)
  aa$nom <- levels(varx)
  aa %>%
    ggplot() +
    aes(x = nom, y = mean * 100, fill = nom) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100),
                  width = .2,
                  position = position_dodge(.9)) +
    labs(title = tit,
         y = "%") +
    theme_light() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
