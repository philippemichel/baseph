#' Barconfph
#'
#'Tracé d'un graphique en barre représentant la moyenne avec son intervalle de confiance à 
#' 95 % d'une valeur numérique découpé selon une variable factorielle.
#'
#' @param dfx dataframe
#' @param varnum Variable numérique à afficher
#' @param vartri Variable de tri, factorielle
#' @param titre Titre du graphique (vide par défaut)
#' @param stitre Sous titre du graphique (vide par défaut)
#' @param tx Titre de l'axe des x (vide par défaut)
#' @param ty Titre de l'axe des x ("n" par défaut)
#' @param lab label  (vide par défaut) rarement utile
#' @param cap caption(texte écrit en bas du graphique)
#' @param angle d'affichage des labels de l'axe x s'ils ont trop longs
#'
#' @import tidyverse
#' @import ggsci
#' @import stringr

#' @return un graphique
#' @export
#'
#' @examples data("patients")
#' barconfph(dfx = patients, varnum = igs2, vartri = admission, 
#' titre = "IGS II vs provenance", stitre = "en ICU", tx = "Mode d'admission", ty = "IGS II", 
#' cap = "Texte écrit petit",angle = 0)
#'
barconfph <-
  function(dfx,
           varnum,
           vartri,
           titre = "",
           stitre = "",
           tx = "",
           ty = "n",
           lab = "",
           cap = "",
           angle = 0)
  {
  if (angle == 0) {
    hj <- 0.5
  } else {
    hj <- 1
  }
  angy <-  (str_length(ty) > 3)*90
  #
  bas <- NULL
  haut <- NULL
  moy <- NULL
dfx |>
    drop_na({{vartri}}) |>
    group_by({{vartri}}) |>
    summarise(
      bas = bashaut({{varnum}})[[1]],
      haut = bashaut({{varnum}})[[2]],
      moy = mean({{varnum}}, na.rm = TRUE)
    ) |>
    ggplot() +
    aes(x = {{vartri}}, y = moy, fill = {{vartri}}) +
    # geom_bar(stat = "identity") +
    geom_col() +
    geom_errorbar(
      aes(ymin = bas, ymax = haut),
      width = .6,
      position = position_dodge(0.5),
      linewidth = 0.6
    ) +
    labs(
      title = titre,
      subtitle = stitre,
      x = tx,
      y = ty,
      caption = cap, 
      label = lab
    ) +
    theme_light() +
    scale_fill_jama() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(
        size = 12,
        angle = angy,
        vjust = .5
      ),
      axis.text.x = element_text(
        size = 12,
        angle = angle,
        hjust = hj
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
