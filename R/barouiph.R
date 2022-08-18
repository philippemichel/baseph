#' Barplot uniquement les "oui" en %
#' Trace un barplot d'une variable factorielle en oui/non, en affichant uniquement les "oui" selon les modalitées de la variable de tri
#
#' @param df data-frame
#' @param varx variable _ traiter (factorielle) le plus souvent binaire
#' @param vartri variable de tri (afficher sur l'axe des x)
#' @param oui oui la valeur _ afficher dans la variable varp
#' @param titre Titre du graphique
#' @param stitre Sous-titre du graphique
#' @param ytitre Titre de l'axe y (%)
#' @param capt  légende du graphique
#' @param lab label du graphique
#' @param angle angle affichage des valeurs de vart sur l'axe des x (0 par defaut)
#'
#' @import ggplot2
#' @import ggsci
#' @import dplyr
#' @import binom
#
#'
#' @return un graphique
#' @export
#'
#' @examples barouiph(df = patients, varx = sexe, vartri = admission,
#' oui = "f", titre = "Mode d'admission",
#' stitre =" Pour les dames",
#' ytitre ="%", capt ="Mode d'admission selon le sexe",
#' lab = "ab", angle = 0)
#'
barouiph <- function(df,
                     varx,
                     vartri,
                     oui = "oui",
                     titre = "",
                     stitre = "",
                     ytitre = "%",
                     capt = "",
                     lab = "",
                     angle = 0) {
  tri <- NULL
  if (angle == 0) {hj <-  0.5} else {hj <-  1}
  #
  nv <- df |>
    dplyr::filter({{varx}} == oui) |>
    group_by({{vartri}}) |>
    summarise(n())
  #
  if (dim(nv)[1] == 0){
    return ("La modalite demandee n'existe pas")
  }
  #
  nt <- df |>
    group_by({{vartri}}) |>
    summarise(n())
  #
  zz <- as_tibble(binom.confint(pull(nv[,2]), pull(nt[,2]), methods = "exact"))
  ymax <- floor(max(zz$upper)*10)*10 + 10
  if (ymax > 89) {
    ymax <-  100
  }
  zz$tri <- pull(nv[,1])
  zz %>%
    ggplot() +
    aes(x = tri, y = mean * 100, fill = tri) +
    geom_bar(stat = "Identity") +
    geom_errorbar(
      ymin = zz$lower * 100,
      ymax = zz$upper * 100,
      width = 0.6,
      size = 0.8
    ) +
    labs(title = titre,
         subtitle = stitre,
         y = ytitre,
         caption = capt,
         label = lab) +
    theme_light() +
    scale_fill_lancet() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      axis.title.y = element_text(
        size = 12,
        angle = 90,
        vjust = .5
      ),
      axis.text.x = element_text(
        size = 12 ,
        angle = angle,
        hjust = hj
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    ) +
    scale_y_continuous(limits = c(0, ymax))
}
