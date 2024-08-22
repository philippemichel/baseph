#' DEBPH
#' 
#' Réglages de base pour les tableaux. À éxécuter au début du document Quarto. Chargement de quleques libraries. 
#'
#' @param param TRUE si variables normales (moyenne et écart-type), FALSE si variables non normales (médiane et quartiles)
#'
#' @return affs (affichage des valeurs numériques)
#' @export
#'
#' @examples debph(param = FALSE)
#' 
debph <- function(param = FALSE) {
  library(tidyverse)
  library(gtsummary)
  library(xlsx)
  library(baseph)
  #
  affpx <- "{median} [{p25} - {p75}]"
  
  if (paramx) {
    affpx <-  "{mean} ({sd})"
  }
  
  affs <- list(all_continuous() ~ affpx, 
               all_categorical() ~ "{n} / {N} ({p}%)")
  #
  theme_gtsummary_language(language = "fr", decimal.mark = ",")
  options(OutDec = ",")
  #
  return(affs)
}
