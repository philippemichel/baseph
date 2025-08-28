#' Tables de randomisation multicentre avec taille des blocs aléatoire
#'
#' @param nbcent Nombre de centres
#' @param nbtrait Nombre de classes
#' @param nbcas Nombre total de cas souhaité
#'
#' @import blockrand
#' @import utils
#'
#' @return 1 fichier csv global & un fichier par centre
#' @export
#'
#' @examples listrandph(nbcent = 1, nbtrait = 2, nbcas = 100)
#'
listrandph <- function(nbcent = 1, nbtrait = 2, nbcas) {
  listr <- NULL
  tca <- ceiling(nbcas / nbtrait)
  for (l in 1:nbcent) {
    strat <- paste0("centre ", l)
    nn <- paste0("table_random_centre", l, ".csv")
    listp <- blockrand::blockrand(
      n = nbcas,
      stratum = strat,
      num.levels = nbtrait,
      id.prefix = "patient_",
      block.sizes = sample(1:6, tca, replace = TRUE),
      levels = c("A", "B")
    )
    utils::write.csv2(listp, nn, row.names = FALSE)
    listr <- rbind(listr, listp)
  }
  utils::write.csv2(listr, "randomisation_total.csv", row.names = FALSE)
}
