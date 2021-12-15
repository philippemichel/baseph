#' Pyramide des ages
#' Trace une pyramide des ages simple.
#'
#' @param dfx data.frame contenant les valeurs
#' @param age valeur des ages , idealement divise en classes
#' @param sexe variable factorielle binaire
#' @param ff valeur de la classe 'Feminin"
#' @param mm valeur de la classe "masculin"
#' @param titre Titre du graphique
#'
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples aa <- runif(100,0,100)
#'           aa <- cut(aa,seq(0,100,10))
#'           sx <- c(rep("F",50),rep("M",50))
#'           df <- data.frame(aa,sx)
#'           pyrph(dfx = df, age = aa, sexe = sx, ff = "F", mm = "M", titre = "Pyramide des âges")
#'
pyrph <- function(dfx,
           age = age,
           sexe = sexe,
           ff = "F",
           mm = "M",
           titre = "Pyramide des âges") {
    ggplot(dfx) +
      aes(x = {{age}}, fill = {{sexe}}) +
      geom_bar(data = subset(dfx, {{sexe}} == ff),
               aes(y = ..count.. * (-1))) +
      geom_bar(data = subset(dfx, {{sexe}} == mm)) +
      scale_fill_manual(values = c("pink", "light blue")) +
      coord_flip() +
      labs(title = titre) +
      theme_light() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y =  element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
      )
  }
