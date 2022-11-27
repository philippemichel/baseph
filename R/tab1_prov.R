#' Tableau comparatif (test)
#'
#' Génération d'un tableau comparatif pour une étude clinique avec tests unitaires pour les deux groupes. Choix du type de test (paramétriques ou non).
#'
#' @param dfx Tibble
#' @param test Affichage des valuers numériques : si "moy " : moyenne ± écart-type, si "med" : médiane (quartiles)
#'
#' @return
#' @export
#'
#' @examples tab1prov(patients, escarre)
tab1prov <- function(dfx, test = "moy"){
  tabf <- NULL
  tlig <- NULL
  nlig <- 1
  for (l in 1:ncol(dfx)){
  aa <- pull(na.omit(dfx[,l]))
  nom <- var_label(dfx[,l])[[1]]
  if (!is.numeric(aa)){
  lig <- c(nom,length(aa)," ")
  tlig <- c(tlig,nlig)
  nlig <- nlig + 1
  zz <- tab1(aa, decimal = 1, graph = FALSE, cum.percent = FALSE)
  zz <- zz[[2]]
  for (i in 1:(nrow(zz) - 1)){
    l2 <- c( paste0("     ", row.names(zz)[i]), " ",paste(zz[i,1]," (",zz[i,2],"%)"))
    lig <- rbind(lig,l2)
    nlig <- nlig + 1
  }
  }
  else{
    if (test =="moy"){
      mm <- paste(signif(mean(aa),4), " ± ", signif(sd(aa),4))
    }
    else{
      mm <- paste0(median(aa), " (", quantile(aa)[2],";",quantile(aa)[4],")")
    }
    lig <- c(nom,length(aa),mm)
    nlig <- nlig + 1
  }
  # print(lig)
  tabf <- rbind(tabf,lig)
  }
  return(tabf)
}