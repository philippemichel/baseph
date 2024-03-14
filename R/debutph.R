#' Début du travail
#'
#' Lit un csv contenant les données, simplifie & normalise les titres des variables.
#'
#'Le csv doit avoir les réglages par défaut (UTF-8, séparateur de champs : ",") soit les réglages par défaut de LibreOffice
#'.
#' Toutes les variables "caractère" sont transformées en facteur.
#'
#' @param fich Fichier csv contenant les données
#' @param bnom Fichier csv avec une colonne "noms" contenant les intitulés corrects pour les variables
#' @param nax liste des valeurs à considérer comme NA
#' @param delim Délimitateur utilisés dans les deux fichiers csv
#' 
#' @import utils
#' @import stats
#' @import janitor
#' @import readr
#' @import labelled

#' @return data.frame
#'
#' @example debutph("data/patients.csv")
#' @export
debutph <- function(fich, bnom, nax =  c("","na","NA"," "), delim = ";") {
#
bnomx <- readr::read_delim(
    bnom,
    delim = delim,
    show_col_types = FALSE)
#
dfx <-  readr::read_delim(
    fich,
    delim = delim,
    show_col_types = FALSE,
    na = nax) |> 
    mutate_if(is.character, as.factor) |>
    janitor::clean_names()
#
var_label(dfx) <- bnomx$noms
  return(dfx)
}
