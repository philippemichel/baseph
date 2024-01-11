#' lollipopph
#' 
#' Tracé d'un graphique "lollipop" de distribution d'une variable factorielle avec possibilité de mise en évidence d'un ou plusieurs facteurs.
#'
#' @param dfx data-frame
#' @param nom colonne à exposer (factorielle)
#' @param tri niveaux de "nom" à mettre en évidence 
#' @param titre titre
#' @param capt légende 
#' 
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import colorspace
#' @import forcats
#'
#' @return graphique
#' @export
#'
#' @examples lollipph(dfx = patients, nom = lieudevie1, 
#' tri = c("EHPAD","Maison de retraite"), 
#' titre = "Lieu de vie", capt = "Lieu de vie avant l'hospitalisation")
#' 
 lollipph <- function(dfx,nom, tri= "xx", titre = "", capt = "x"){
  ndfx <- dfx |> 
    count({{nom}})
  names(ndfx) <-  c("nom","n")
  #
  if (capt == "x"){capt = titre}
  #
  cg <- 'darkslategrey'
  ndfx |> 
    mutate(ndfx, trix = (nom %in% tri)) |> 
    drop_na(nom) |> 
    ggplot() +
    aes(x = fct_reorder(nom, n), y = n, fill = trix) +
    geom_segment(
      aes(
        x = fct_reorder(nom, n),
        xend = fct_reorder(nom, n),
        y = 0,
        yend = n
      ),
      linewidth = 1.5,
      color = cg
    ) +
    geom_point(
      size = 5,
      shape = 21,
      color = cg
    ) +
    coord_flip() +
    labs(
      title = titre,
      subtitle = "",
      x = "",
      y = "n",
      caption = capt
    ) +
    theme_light() +
    colorspace::scale_fill_discrete_qualitative(palette = "Dark 3", nmax = 2, order = c(2,1)) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      legend.title = element_text(size = 12),
      axis.title.y = element_text(
        size = 12,
        angle = 0,
        vjust = .5
      ),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
