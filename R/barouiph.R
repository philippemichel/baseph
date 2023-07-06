#' Barplot avec barres d'erreur d'une valeur
#'
#' Trace un barplot avec les intervalles de confiance pour une valeur donnée
#' d'une variable factorielle selon les classes d'une variable factorielle de
#' classification.
#'
#'
#' @param dfx data.frame
#' @param varx variable à représenter (factoriel) y
#' @param testx Variable de tri (factoriel) x
#' @param valx valeur de test à présenter
#' @param titre Titre du graphique
#' @param stitre Sous-titre du graphique
#' @param titx titre (optionnel) de l'axe x
#' @param tity titre (% par défaut) de l'axe y
#' @param capt légende du graphique
#' @param lab label du graphique
#' @param angle angle affichage des valeurs de vart sur l'axe des x (0 par defaut)
#'
#' @import binom
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import colorspace
#'
#' @return graphique
#' @export
#'
#' @examples data(patients)
#' barouiph(dfx = patients, varx =escarre, testx =sexea, valx = "oui", titre = "Sexe",ang = 0)
#'
barouiph <- function(dfx,
                      varx,
                      testx,
                      valx = "oui",
                      titre = "",
                      stitre = "",
                      titx="",
                      tity = "%",
                      capt = "",
                      lab = "",
                      angle = 0) {
  xx <- yy <- mean.yy <- NULL
  mean.Freq <- lower <- upper <- NULL
  if (angle == 0) {
    hj <- 0.5
  } else {
    hj <- 1
  }
  aa <- dfx |>
    drop_na({{varx}}) |>
    transmute(xx = as.factor({{varx}}), yy = as.factor({{testx}}))
  ll <- aa %>%
    summarise(which(valx == levels(xx)))
  ll <- ll[[1]]
  if (length(ll) == 0)
    return("Modalit\u00E9 absente")
  zz <- aa %>%
    summarise(table(xx,yy))
  tzz <- colSums(zz)
  szz <- pull(zz[ll, ])
  aa <- binom::binom.confint(szz, tzz, method = "exact")
  aa <- dplyr::tibble(aa)[, 7:10]
  #
  aa %>%
    ggplot() +
    aes(x = mean.yy, y = mean.Freq * 100, fill = mean.yy) +
    geom_bar(stat = "identity") +
    geom_errorbar(
      aes(ymin = lower * 100, ymax = upper * 100),
      width = .3,
      size = 0.8,
      position = position_dodge(.9)
    ) +
    labs(
      title = titre,
      subtitle = stitre,
      x = titx,
      y = tity,
      caption = capt,
      label = lab
    ) +
    theme_light() +
  scale_fill_discrete_qualitative(palette = "Dynamic") +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_text(
        size = 12,
        angle = 90,
        vjust = .5
      ),
      axis.title.x = element_text(
        size = 12,
        angle = angle,
        hjust = hj
      ),
      axis.text.x = element_text(
        size = 12,
        angle = angle,
        hjust = hj
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
