#' Barplot décroissant en %
#' Trace un barplot d'une variable factorielle, axe des y en %,
#' classement des items en décroissant.
#'
#' @param dfx data.frame ou yibble
#' @param varx nom de la variable à traiter (factorielle)
#' @param titre Titre du graphique
#' @param stitre Soustitre du graphique
#' @param capt  légende du graphique
#' @param lab label du graphique
#' @param angle angle d'affichage des valeurs de vart sur l'axe des x (0 par défaut)
#'
#' @import ggplot2
#' @import dplyr
#' @import forcats
#'
#' @return un graphique
#' @export
#'
#' @examples data("patients")
#' bardecph(patients, admission, titre = "Mode d'admission",
#' stitre = "%", capt = "Mode d'admission", lab ="ma", angle = 0)
#'
bardecph <- function(dfx,
                     varx,
                     titre = "",
                     stitre = "",
                     capt = "",
                     lab = "",
                     angle = 0){
  nomx <- fqx <- NULL
  if (angle == 0) {hj <-  0.5} else {hj <-  1}
  #
  aa <- dfx %>%
    group_by({{varx}}) %>%
    count({{varx}})
  names(aa) <- c("nomx","fqx")
  aa <- na.omit(aa)
  aa$fqx <- aa$fqx*100/sum(aa$fqx)
  maxy <- floor(max(aa$fqx) / 10 + 2) * 10
  if (maxy > 100) {

    maxy <-  100
  }
  #
  aa %>%
    ggplot() +
    aes(x = fct_reorder(nomx,fqx, .desc = TRUE), nomx, y = fqx, fill = fct_reorder(nomx,fqx, .desc = TRUE)) +
    geom_bar(stat = "identity") +
 #   geom_text(
#     aes(label = paste0(round(fqx, 0), " %")),
 #     vjust = -0.8,
#      color = "black",
 #     size = 6
 #   ) +
    labs(title = titre,
         subtitle = stitre,
         y = "%",
         caption = capt,
         label = lab)+
    scale_y_continuous(limits = c(0, maxy)) +
    theme_light() +
    scale_fill_discrete_qualitative(palette = "Dark 3") +
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
        size = 11 ,
        angle = angle,
        hjust = hj
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
