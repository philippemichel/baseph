#' Barplot uniquement les "oui" en %
#' Trace un barplot d'une variable factorielle en oui/non, uniquement les "oui" selon les modalités de la variable de tri
#
#' @param varp variable a traiter (factorielle) le plus souvent binaire
#' @param vart variable de tri (afficher sur l'axe des x)
#' @param titre Titre du graphique
#' @param oui oui la valeur à afficher dans la variable varp
#' @param angle angle affichage des valeurs de vart sur l'axe des x (0 par defaut)
#'
#' @import ggplot2
#' @import see
#' @import dplyr
#
#'
#' @return un graphique
#' @export
#'
#' @examples aa <- c("a","a","b","c")
#'           bb <- c("oui","non","oui","oui")
#'          barouiph(bb,aa, titre = "essai", oui = "oui", angle = 0)
#'
#'
barouiph <- function(varp,
                     vart,
                     titre = "",
                     oui = "oui",
                     angle = 0) {
  varp <- as.factor(varp)
  vart <- as.factor(vart)
  if (oui %in% levels(varp) == FALSE)
  {
    print(paste0("*", oui, "* n'est pas dans la variable \u00e9tudi\u00e9e"))
    return()
  }
  nlev <- which(oui == levels(varp))
  zz <- table(varp, vart)
  zz <- binom.confint(zz[nlev, ], colSums(zz), method = "exact")
  zz <- as_tibble(zz)
  ymax <- max(zz$upper) * 100 + 10
  if (ymax > 100) {
    ymax = 100
  }
  zz$tri <- levels(vart)
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
         y = "%") +
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
        hjust = 1
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    ) +
    scale_y_continuous(limits = c(0, ymax))
}