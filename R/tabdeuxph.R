#' barplot deux variables factorielles
#'Barplot de deux variables factorielles representees en % de la deuxieme dans chaque classe de la premiere
#'
#' @param vart variable de tri
#' @param vart variable a decouper en %
#' @param titre titre du graphique
#' @param stit soustitre du graphique
#' @param ang angle des legendes en x
#'
#' @return graphique
#' @export
#'
#' @examples aa <- c("a","a","b","c")
#'           bb <- c("1","2","3","3")
#'           tabdeuxph(aa,bb,"essai","test",ang=80)
tabdeuxph <- function(varx,
                      vart,
                      titre = "",
                      stit = "",
                      ang = 90) {
  zz <- table(varx, vart)
  zz <- prop.table(zz, 1) * 100
  zz <- as.data.frame(zz)
  zz %>%
    ggplot() +
    aes(x = varx, y = Freq, fill = vart) +
    geom_bar(stat = "Identity") +
    labs(title = titre,
         subtitle = stit,
         y = "%") +
    theme_light() +
    scale_fill_d3() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(
        size = 12 ,
        angle = ang,
        hjust = 1
      ),
      axis.text.y = element_text(size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}
