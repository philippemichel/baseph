#' ligne numerique  test parametrique tableau comparatif
#'
#' @param nom nom de la ligne
#' @param varp variable explicative (numerique)
#' @param trip variable expliquee (facteur)
#'
#' @import dplyr
#' @import rlang
#'
#' @return ligne complète avec p
#' @export
#'
#' @examples ligfc("petale",iris$Species,iris$Species)
lignumc <- function(nom, varp, trip) {
  dfp <- data.frame(varp, trip)
  tano <- anova(lm(varp ~ trip))

  zz <- dfp %>%
    group_by(trip) %>%
    summarise(moys(varp), .groups = "drop")
  zz <- zz[2]
  lig <- c(nom, zz[[1]], beaup(tano$`Pr(>F)`[1]))
  return(lig)
}


#' ligne numerique  test non parametrique tableau comparatif
#'
#' @param nom nom de la ligne
#' @param varp variable explicative (numerique)
#' @param trip variable expliquee (facteur)
#'
#' @import dplyr
#' @import rlang
#'
#' @return ligne complète avec p
#' @export
#'
#' @examples ligmedc("tt A", runif(88),c(rep("A",44),rep("B",44)))
 ligmedc <- function(nom, varp, trip) {
  dfp <- data.frame(varp, trip)
  tano <- wilcox.test(varp ~ trip)
  dfp <- na.omit(dfp)
  zz <- dfp %>%
    group_by(trip) %>%
    summarise(meds(varp),.groups = "drop" )
  zz <- zz[2]
  lig <- c(nom, zz[[1]], beaup(tano$p.value[1]))
  return(lig)
}
