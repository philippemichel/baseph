#' Barplot uniquement les "oui" en %
#' Trace un barplot d'une variable factorielle en oui/non, uniquement les "oui" selon les modalités de la variable de tri
#
#' @param varp variable a traiter (factorielle) le plus souvent binaire
#' @param vart variable de tri (afficher sur l'axe des x)
#' @param titre Titre du graphique
#' @param stitre Soustitre du graphique
#' @param oui oui la valeur à afficher dans la variable varp
#' @param capt  légende du graphique
#' @param lb label du graphique
#' @param angle angle affichage des valeurs de vart sur l'axe des x (0 par defaut)
#'
#' @import ggplot2
#' @import see
#' @import dplyr
#' @import binom
#
#'
#' @return un graphique
#' @export
#'
#' @examples aa <- c("a","a","b","c")
#'           bb <- c("oui","non","oui","oui")
#'          barouiph(aa,bb, titre = "essai", oui = "oui", angle = 0)
#'
#'
barouiph <- function(varp,
                     vart,
                     oui = "oui",
                     titre = "",
                     stitre = "",
                     capt = "",
                     lab = "",
                     angle = 0) {
  if (is.factor(varp) == FALSE){varp <- as.factor(varp)}
  if (is.factor(vart) == FALSE){vart <- as.factor(vart)}
  if (oui %in% levels(varp) == FALSE)
  {
    print(paste0("*", oui, "* n'est pas dans la variable \u00e9tudi\u00e9e"))
    print(levels(varp))
    return()
  }
  if (angle == 0) {hj = 0.5} else {hj = 1}
  nlev <- which(oui == levels(varp))
  zz <- table(varp, vart)
  zz <- binom.confint(zz[nlev, ], colSums(zz), method = "exact")
  zz <- as_tibble(zz)
  ymax <- max(zz$upper) * 100 + 10
  if (ymax > 90) {
    ymax = 100
  }
  zz$tri <- as.factor(levels(vart))
  levels(zz$tri) <- levels(vart)
  zz %>%
    ggplot() +
    aes(x = tri, y = mean * 100, fill = tri) +
    geom_bar(stat = "Identity") +
    geom_errorbar(
      ymin = zz$lower * 100,
      ymax = zz$upper * 100,
      width = 0.6
    ) +
    labs(title = titre,
         subtitle = stitre,
         y = "%",
         caption = capt,
         label = lab) +
    theme_light() +
    scale_fill_material() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      axis.title.y = element_text(
        size = 12,
        angle = 0,
        vjust = .5
      ),
      axis.text.x = element_text(
        size = 12 ,
        angle = angle,
        hjust = hj
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    ) +
   scale_y_continuous(limits = c(0, ymax))
}
