#' Barplot decroissant en %
#' Trace un barplot d'une variable factorielle, axe des y en %, classement des items en décroissant
#'
#' @param varx variable a traiter (factorielle)
#' @param titre Titre du graphique
#' @param stitre Soustitre du graphique
#' @param capt  légende du graphique
#' @param lb label du graphique
#' @param angle angle affichage des valeurs de vart sur l'axe des x (0 par defaut)
#'
#' @import ggplot2
#' @import see
#' @import dplyr
#' @import ggsci
#' @import forcats
#'
#' @return un graphique
#' @export
#'
#' @examples bardecph(iris$Species, "Espece")
bardecph <- function(varx,
                     titre = "",
                     stitre = "",
                     capt = "",
                     lab = "",
                     angle = 0){
  if (angle == 0) {hj = 0.5} else {hj = 1}
    aa <- prop.table(table(varx)) * 100
    aa <- as.data.frame(aa)
    maxy <- floor(max(aa$Freq) / 10 + 2) * 10
    if (maxy > 100) {

    maxy = 100
  }
  names(aa)[1] <- "cause"
  aa %>%
    mutate(name = forcats::fct_reorder(cause, desc(Freq))) %>%
    ggplot() +
    aes(x = name, y = Freq, fill = name) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = paste0(round(Freq, 0), " %")),
      vjust = -0.8,
      color = "black",
      size = 6
    ) +
    labs(title = titre,
         subtitle = stitre,
         y = "%",
         caption = capt,
         label = lab)+
    scale_y_continuous(limits = c(0, maxy)) +
    theme_light() +
    scale_color_material() +
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
