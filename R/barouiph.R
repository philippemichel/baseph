#' Barplot uniquement les "oui" en %
#' Trace un barplot d'une variable factorielle en oui/non, uniquement les "oui" selon les modalites de la variable de tri
#
#' @param varp variable a traiter (factorielle)
#' @param vart variable de tri (afficher sur l'axe des x)
#' @param titre Titre du graphique
#' @param oui oui la valeur à afficher dans la variable varp
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
#'          barouiph(bb,aa, titre = "essai", oui = "oui")
#'
#'
barouiph <- function(varp, vart, titre, oui = "oui") {
  zz <- table(varp, vart)
  zz <- prop.table(zz, 2) * 100
  zz <- as_tibble(zz)
  zz %>%
    filter(varp == oui) %>%
    ggplot() +
    aes(x = vart, y = n, fill = vart) +
    geom_bar(stat = "Identity") +
    geom_text(
      aes(label = paste0(round(n, 0), " %")),
      vjust = 1.6,
      color = "white",
      size = 6
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
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(
        size = 12 ,
        angle = 60,
        hjust = 1
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
