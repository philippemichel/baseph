#' Barplot en pourcentage
#'
#' Affiche un bar-plot de deux variables discrètes, avec la variable en Y présentée en % pour chaque niveau de la variable X.
#'
#' @param dfx Tibble
#' @param varx Variable de tri, (axe X)
#' @param vary Variable Y (%)
#' @param tit Titre
#' @param stit Sous titre
#' @param cap Texte de légende
#' @param tx Titr de l'axe X
#' @param angle Angle des étiquettes de l'axe X
#'
#' @import ggplot2
#' @import dplyr
#' @import colorspace
#' @return graphique
#' @export
#'
#' @examples barpcph(dfx = patients, varx = sexea, vary = admission, tit="Admission selon le sexe", stit ="Démonstration", cap="Démonstration sur Admission selon le sexe", tx="Sexe", angle = 0)
#'
#'
barpcph <-
  function(dfx,
           varx,
           vary,
           tit = "",
           stit = "",
           cap = "",
           tx = "",
           angle = 0) {
    if (angle > 10) {
      hj = 1
    } else{
      hj = 0.5
    }
    dfx |>
      drop_na({{varx}}, {{vary}}) |>
      ggplot() +
      aes (x = {{varx}},
           fill = {{vary}}) +
      geom_bar(stat = "count", position = "fill") +
      scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20)) +
      labs(
        title = tit,
        subtitle = stit,
        x = tx,
        y = "%",
        caption = cap
      ) +
      theme_light() +
      scale_fill_discrete_qualitative(palette = "Dark 3") +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12,
                                    vjust = .5),
        axis.text.x = element_text(
          size = 12,
          angle = angle,
          hjust = hj
        ),
        axis.text.y = element_text(size = 12),
        legend.position = "right"
      )
  }
