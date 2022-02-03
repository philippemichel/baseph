#' Barplot avec barres d'erreur d'une valeur
#' Trace un barplot avec les intervalles de confiance pour une valeur donnee
#' d'une variable factorielle
#' selon les classes d'une variable factorielle de classification
#'
#' @param dfx data.frame
#' @param varx variable a représenter (factoriel) y
#' @param testx Variable de tri (factoriel) x
#' @param valx valeur de testx a presenter
#' @param titre Titre du graphique
#' @param stitre Soustitre du graphique
#' @param capt légende du graphique
#' @param lab label du graphique
#' @param angle angle affichage des valeurs de vart sur l'axe des x (0 par defaut)
#'
#' @import binom
#' @import ggplot2
#' @import dplyr
#' @import see
#'
#' @return graphique
#' @export
#'
#' @examples xx <- factor(c("A","A","B","A","B","C","A","C"))
#'           yy <- factor(c(rep("oui",4),rep("non",4)))
#'           dfe <- dplyr::tibble(xx,yy)
#'          barconfph(dfx = dfe,
#'                    varx=xx,
#'                    testx= yy,
#'                    valx = "non",
#'                    tit = "essai",
#'                    stitre = "",
#'                    ang = 0)
#'
barconfph <- function(dfx,
           varx,
           testx,
           valx = "oui",
           titre = "",
           stitre = "",
           capt = "",
           lab = "",
           angle = 0){
    if (angle == 0) {hj <-  0.5} else {hj <-  1}
    ll <- dfx %>%
      summarise(which(valx == levels({{testx}})))
    ll <- ll[[1]]
    print(ll)
    zz <- dfx %>%
      summarise(table({{testx}}, {{varx}}))
    tzz <- colSums(zz)
    szz <- zz[ll,][[1]]
    aa <- binom::binom.confint(szz, tzz, method = "exact")
    aa <- dplyr::tibble(aa)[,7:10]
    names(aa) <- c("nom","mean","lower","upper")
    #
    aa %>%
      ggplot() +
      aes(x = nom, y = mean * 100, fill = nom) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100),
                    width = .2,
                    position = position_dodge(.9)) +
      labs(title = titre,
           subtitle = stitre,
           y = "%",
           caption = capt,
           label = lab) +
      theme_light() +
      scale_fill_material() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(
          size = 12,
          angle = 0,
          vjust = .5
        ),
        axis.text.x = element_text(
          size = 12 ,
          angle = angle,
          hjust = hj
        ),
        axis.text.y = element_text(size = 12),
        legend.position = "none"
      )
  }

