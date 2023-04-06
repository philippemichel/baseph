#' HISTMULTIPH
#' Graphique en plusieurs histogrammes superposés, pratique pour bien visualiser les variation d'une distribution selon une modalité.
#'
#' @param dfx data-frame ou tibble
#' @param varx Variable factorielle
#' @param varn variable numérique
#' @param tit Titre du graphique
#' @param stit valeur de p
#' @param titx Titre de l'axe horizontal
#' @param bin largeur d'une barre
#' 
#' @import tidyverse
#' @import ggplot2
#' @import colorspace
#' 
#' @return un graphique
#' @export 
#'
#' @examples histmultiph(dfx = iris, varx= Species, varn = Sepal.Width, tit = "IRIS", stit = 0.002, titx = "Pétales (mm)", bin = 5)
#' 
histmultiph <- function(dfx, varx, varn, tit = "", stit= 0, titx = "", bin = 1){
  stit <- paste0("p = ", stit)
  dfx |> 
    drop_na({{varn}}) |> 
    ggplot() +
    aes(x = {{varn}}, fill = {{varx}}, color = {{varx}}) +
    geom_histogram(, binwidth = bin, alpha = 0.9) +
    facet_grid(vars({{varx}})) +
    labs(title = tit,
         subtitle = stit,
         x = titx,
         y = "n",
         caption = tit) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0,10,2)) +
    theme_light() +
    scale_fill_discrete_qualitative(palette = "Dynamic") +
    theme(
      strip.text.y = element_text(size=12),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title.x = element_text(size = 12),
      legend.title = element_blank(),
      axis.title.y = element_text(
        size = 12,
        angle = 0,
        vjust = .5
      ),
      axis.text.x = element_text(
        size = 12
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
