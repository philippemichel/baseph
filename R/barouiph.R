#' Barplot avec les intervalles de confiance pour une valeur donnée
#' d'une variable factorielle selon les classes d'une variable factorielle de classification.
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
#'
#' @import binom
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggsci
#'
#' @return graphique
#' @export
#'
#' @examples data(patients)
#' barouiph(dfx = patients, varx = escarre, testx = sexe, valx = "oui", titre = "Sexe")
#'
barouiph <- function(dfx,
                     varx,
                     testx,
                     valx = "oui",
                     titre = "",
                     stitre = "",
                     titx = "",
                     tity = "%",
                     capt = "") {
  xx <- nom <- lower <- upper <- NULL
  dfx <- dfx |>
    drop_na({{ varx }}) |>
    mutate(xx = as.factor({{ varx }}), yy = as.factor({{ testx }})) |>
    mutate(xx = xx == valx)
  nn <- levels(dfx$yy)

  if (nrow(dfx) == 0) {
    return("Modalit\u00E9 absente")
  }
  zz <- table(dfx$xx, dfx$yy)
  binom::binom.confint(zz[2, ], colSums(zz), method = "exact") |>
    dplyr::tibble() |>
    mutate(nom = factor(nn, levels = nn)) |>
    ggplot() +
    aes(x = nom, y = mean * 100, fill = nom) +
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
      caption = capt
    ) +
    theme_light() +
    scale_fill_jama() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_text(
        size = 12,
        angle = 90,
        vjust = .5
      ),
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
