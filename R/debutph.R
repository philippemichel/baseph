#' Début du travail
#'
#' Lit un csv contenant les données, simplifie & normalise les titres des variables.
#'
#'Le csv doit avoir les réglages par défaut (UTF-8, séparateur de champs : ",") soit les réglages par défaut de LibreOffice
#'.
#' Toutes les variables "caractère" sont transformées en facteur.
#'
#' @param fich Fichier csv contenant les données
#' @param nax liste des valeurs à considérer comme NA
#' 
#' @import utils
#' @import stats
#' @import janitor
#' @import readr

#' @return data.frame
#'
#' @example debutph("data/patients.csv")
#' @export
debutph <- function(fich, nax =  c("","na","NA"," ")) {
dfx <-  readr::read_delim(
    fich,
    delim = ";",
    show_col_types = FALSE,
    na = nax) |> 
    mutate_if(is.character, as.factor) |>
    janitor::clean_names()
  #  janitor::remove_constant() |>
  #  janitor::remove_empty()
  return(dfx)
}
