#' Barplot simple en %
#' Trace un barplot d'une variable factorielle, axe des y en %
#' @param varx variable a traiter (factorielle)
#' @param titre Titre du graphique
#' @param stitre Soustitre du graphique
#' @param capt  légende du graphique
#' @param lab label du graphique
#' @param angle angle affichage des valeurs de vart sur l'axe des x (0 par defaut)
#'
#' @import ggplot2
#' @import see
#' @import dplyr
#'
#' @return un graphique
#' @export
#'
#' @examples barsimpleph(iris$Species, "Espece")
barsimpleph <- function(varx,
                        titre = "",
                        stitre = "",
                        capt = "",
                        lab = "",
                        angle = 0){
  if (angle == 0) {hj = 0.5} else {hj = 1}
    aa <- prop.table(table(varx)) * 100
    aa <- as.data.frame(aa)
    names(aa)[1] <- "cause"
    aa %>%
    ggplot() +
    aes(x = cause, y = Freq, fill = cause) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = paste0(round(Freq, 0), " %")),
      vjust = 1.6,
      color = "white",
      size = 6
    ) +
      labs(title = titre,
           subtitle = stitre,
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
      legend.position = "none"
    )
}
