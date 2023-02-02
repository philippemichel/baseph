#' Barplot 2 variables factorielles
#' une variable de tri, une variable en % par modalité de tri
#'
#' @param dfx data.frame
#' @param varp variable à exprimer en % (factorielle)
#' @param vart variable factorielle de tri (axe des x)
#' @param titre Titre du graphique
#' @param stitre Sous titre
#' @param xtitre titre de l'axe x (vide par défaut)
#' @param ytitre titre de l'axe y (% par défaut)
#' @param ltitre titre de la légende
#' @param capt  légende du graphique
#' @param lab label du graphique
#' @param angle angle affichage des valeurs de vart sur l'axe des x (0 par defaut)
#'
#' @import ggplot2
#' @import ggsci
#' @import dplyr
#'
#' @return un graphique
#' @export
#'
#' @examples data("patients")
#' bardeuxph(patients,escarre, admission ,
#' titre = "Escarre & mode d'admission",
#' stitre = "%", xtitre = "x", ytitre = "%, ltitre ="Escarre",
#' capt ="Escarre", lab = "aa", angle = 20)
#'
bardeuxph <- function(dfx,
                      varp,
                      vart,
                      titre = "",
                      stitre = "",
                      xtitre ="",
                      ytitre = "%",
                      ltitre = "",
                      capt = "",
                      lab = "",
                      angle = 0){
  if (angle == 0) {hj <-  0.5} else {hj <-  1}
  dfx |>
    dplyr::filter(!is.na({{vart}}) & !is.na({{varp}})) |>
    ggplot2::ggplot() +
    aes(x = {{vart}}, fill = {{varp}}) +
    geom_bar(stat = "count", position = "fill") +
    labs(title = titre,
         subtitle = stitre,
         x = xtitre,
         y = ytitre,
         caption = capt,
         fill = ltitre,
         label = lab) +
    theme_light() +
    colorspace::scale_fill_discrete_qualitative(palette = "Dynamic") +
    scale_y_continuous("%", breaks = seq(0,1,0.2),labels = seq(0,100,20)) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      legend.title = element_text(size = 12),
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
