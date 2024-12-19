#' raincloudph
#' 
#' @param df dataframe
#' @param vcat variable de tri (catégorielle)
#' @param vnum variable à afficher (numérique)
#' @param titre titre du graphique
#' @param titcat titre de la variable catégorielle
#' @param titnum titre de la variable numérique
#' @param adj facteur d'ajustement de la courbe de densité (1 par défaut)
#'
#' @import ggplot2
#' @import dplyr
#' @import ggsci
#' @import ggdist
#'
#' @returns
#' @export
#'
#' @examples raincloudph(df = patients, vcat = admission, vnum = igs2, titre = "IGS2 vs adm", titcat = "Adm", titnum = "IGS 2", adj = 1)
raincloudph <- function(df,
                        vcat,
                        vnum,
                        titre = "",
                        titcat = "",
                        titnum = "",
                        adj = 1) {
  df |>
    drop_na({{vcat}}) |>
    ggplot() +
    aes (x = {{vcat}}, y = {{vnum}}, fill = {{vcat}}) +
    stat_halfeye(
      adjust = adj,
      width = 0.5,
      justification = -0.2,
      .width = 0,
      point_colour = NA
    ) +
    geom_boxplot(
      width = 0.15,
      fill = "grey90",
      alpha = 0.5,
      outlier.color = "orange"
    ) +
    stat_dots(
      dotsize = 0.08,
      justification = 1.2,
      side = "left",
      binwidth = 10
    ) +
    coord_flip() +
    labs(title = titre, x = titcat, y = titnum) +
    theme_light() +
    scale_fill_jama() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      legend.title = element_text(size = 12),
      axis.title.y = element_text(
        size = 12,
        angle = 90,
        vjust = .5
      ),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
