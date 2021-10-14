#' Barplot 2 variables factorielles
#' une variable de tri, une variable en % par modalité de tri
#'
#' @param vart variable factorielle de tri (axe des x)
#' @param varp variable a exprimer en % (factorielle)
#' @param titre Titre du graphique
#'
#' @import ggplot2
#' @import see
#' @import dplyr
#'
#' @return un graphique
#' @export
#'
#' @examples aa <- c("a","a","b","c")
#'           bb <- c("1","2","3","3")
#'          bardeuxph(aa,bb,"essai")
#'
bardeuxph <- function(varp, vart, titre = ""){
  zz <- table(varp, vart)
  zz <- prop.table(zz, 2) * 100
  zz <- as.data.frame(zz)
  zz %>%
    ggplot() +
    aes(x = vart, y = Freq, fill = varp) +
    geom_bar(stat = "Identity") +
    labs(title = titre,
         y = "%") +
    theme_light() +
    scale_fill_material() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
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
      legend.position = "right"
    )
}
