#' Barplot 2 variables factorielles
#' une variable de tri, une variable en % par modalité de tri
#'
#' @param vart variable factorielle de tri (axe des x)
#' @param varp variable a exprimer en % (factorielle)
#' @param titre Titre du graphique
#' @param capt  légende du graphique
#' @param lb label du graphique
#' @param angle angle affichage des valeurs de vart sur l'axe des x (0 par defaut)
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
bardeuxph <- function(varp,
                      vart,
                      titre = "",
                      capt = "",
                      lab = "",
                      angle = 0){
  if (angle == 0) {hj = 0.5} else {hj = 1}
  zz <- table(varp, vart)
  zz <- prop.table(zz, 2) * 100
  zz <- as.data.frame(zz)
  ccs <- chisq.test(varp,vart)
  stit <- beaup(ccs[[3]])
  zz %>%
    ggplot() +
    aes(x = vart, y = Freq, fill = varp) +
    geom_bar(stat = "Identity") +
    labs(title = titre,
         subtitle = stit,
         y = "%",
         caption = capt,
         label = lab) +
    theme_light() +
    scale_fill_material() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
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
      legend.position = "right"
    )
}
