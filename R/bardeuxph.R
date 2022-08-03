#' Barplot 2 variables factorielles
#' une variable de tri, une variable en % par modalité de tri
#'
#' @param dfx data.frame
#' @param varp variable a exprimer en % (factorielle)
#' @param vart variable factorielle de tri (axe des x)
#' @param titre Titre du graphique
#' @param stitre Sous titre
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
#' @examples aa <- c("a","a","b","b","c")
#'           bb <- c("1","2","2","3","3")
#'           df <- data.frame(aa,bb)
#'           bardeuxph(dfx = df, varp = bb, vart = aa,
#'           titre = "essai", stitre = "bel essais", ltitre = "categories",
#'           capt = "petit texte", lab = "cc", angle = 0)
#'
bardeuxph <- function(dfx,
                      varp,
                      vart,
                      titre = "",
                      stitre = "",
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
         y = "%",
         caption = capt,
         fill = ltitre,
         label = lab) +
    theme_light() +
    ggsci::scale_fill_lancet() +
    scale_y_continuous("%", breaks = seq(0,1,0.2),labels = seq(0,100,20)) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
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
