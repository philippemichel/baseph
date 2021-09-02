#' Barplot avec barres d'erreur
#' Trace un barplot avec les intervalles de confiance pour une variable numerique et une variable factorielle de classification
#'
#' @param varx cas a représenter
#' @param testx Variable de tri
#' @param valx valeur de testx a presenter
#' @param tit titre du graphique
#' @param subtit sous titre
#' @param ang Angle d'ecriture des légendes de l'axe x.
#'
#' @import binom
#' @import ggplot2
#' @import dplyr
#' @import ggsci
#'
#' @return graphique
#' @export
#'
#' @examples xx <- factor(c("A","A","B","A","B","C","A","C"))
#'           yy <- factor(c(rep("oui",4),rep("non",4)))
#'          barconfph(varx=xx, testx= yy, valx = "non", tit = "essai", subtit = "", ang = 0)
#'
barconfph <- function(varx, testx, valx = "oui", tit = "titre", subtit = "", ang = 0) {
  ll <- which(valx == levels(testx))
  zz <- table(testx, varx)
  tzz <- colSums(zz)
  szz <- zz[ll,]
  aa <- binom::binom.confint(szz, tzz, method = "exact")
  aa <- tibble(aa)
  aa$nom <- as.factor(levels(varx))
  levels(aa$nom) <- levels(varx)
  aa %>%
    ggplot() +
    aes(x = nom, y = mean * 100, fill = nom) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100),
                  width = .2,
                  position = position_dodge(.9)) +
    labs(title = tit,
         subtitle = subtit,
         y = "%") +
    theme_light() +
    scale_fill_d3() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 12 ,angle = ang, hjust = 1),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
