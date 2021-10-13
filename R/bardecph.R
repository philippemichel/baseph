#' Barplot decroissant en %
#' Trace un barplot d'une variable factorielle, axe des y en %, classement des items en décroissant
#' @param varx variable a traiter (factorielle)
#' @param titre Titre du graphique
#'
#' @import ggplot2
#' @import see
#' @import dplyr
#' @import ggsci
#'
#' @return un graphique
#' @export
#'
#' @examples bardecph(iris$Species, "Espece")
bardecph <- function(varx, titre = "") {
  aa <- prop.table(table(varx)) * 100
  aa <- as.data.frame(aa)
  maxy <- floor(max(aa$Freq) / 10 + 2) * 10
  if (maxy > 100) {
    maxy = 100
  }
  names(aa)[1] <- "cause"
  aa %>%
    mutate(name = fct_reorder(cause, desc(Freq))) %>%
    ggplot() +
    aes(x = name, y = Freq, fill = name) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = paste0(round(Freq, 0), " %")),
      vjust = -0.8,
      color = "black",
      size = 6
    ) +
    labs(title = titre,
         y = "%") +
    scale_y_continuous(limits = c(0, maxy)) +
    theme_light() +
    scale_color_material() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(
        size = 12 ,
        angle = 60,
        hjust = 1
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}
