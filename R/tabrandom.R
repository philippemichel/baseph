##' @title Table de randomisation
##' Plusieurs centres, blocs de taille variable
##' @param nbcent nombre de centres (default : 1)
##' @param nbtrait nombre de groupes (default : 2)
##' @param nbcas  Nombre cas souhaites
##' @return un csv global  + un csv par centre
##' @author Philippe MICHEL
##'
##' @import stats
##' @import utils
##' @import blockrand
##'
##' @example listrandph(nbcent  = 2, nbtrait = 2, nbcas = 30)
##'
listrandph <- function(nbcent  = 1, nbtrait = 2, nbcas){
    listr <- NULL
    tca <- ceiling(nbcas/nbtrait)
for (l in 1:nbcent){
  strat <- paste0("centre ",l)
  nn <- paste0("table_random_centre",l,".csv")
  listp <- blockrand::blockrand(
  n = nbcas,
    stratum = strat,
  num.levels = nbtrait,
  id.prefix='patient_',
  block.sizes = sample(1:6,tca, replace = TRUE) ,
  levels=c("A", "B"))
  write.csv2(listp,nn, row.names = FALSE)
  listr <- rbind(listr,listp)
}
write.csv2(listr,"randomisation_total.csv", row.names = FALSE)
}
