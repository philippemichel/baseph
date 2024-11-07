#' Graphique violon + boxplot
#'
#' @param dfx Tibble de référence
#' @param varx Variable catégorielle
#' @param varnum Variable numérique
#' @param titre Titre
#' @param stit Sous-titre
#' @param titx Titre de l'axe x
#' @param tity Titre de l'axe y
#' @param lab label
#'
#' @import ggplot2
#' @import ggsci
#' @import dplyr
#'
#' @return graphique
#' @export
#'
#' @examples data("patients")
#' vioboxph(patients, admission, age, titre ="grands", stit = "et grandes", titx = "Mode d'admission")

vioboxph <-
  function(dfx,
           varx,
           varnum,
           titre = "",
           stit = "",
           titx = "",
           tity = "n",
           lab = "") {
    dfx |>
      tidyr::drop_na({{varx}}) |>
      ggplot2::ggplot() +
      aes(x = {{varx}}, y = {{varnum}}, fill = {{varx}}) +
      geom_violin() +
      geom_boxplot(fill = "white", width = 0.25) +
      labs(
        title = titre,
        subtitle = stit,
        x = titx,
        y = tity,
        caption = titre,
        label = lab
      ) +
      theme_light() +
      scale_fill_jama() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none"
      )
  }




