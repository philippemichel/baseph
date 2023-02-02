#' Début du travail
#'
#' Lit un csv contenant les données, simplifie & normalise les titres des variables.
#' le vecteur `nom` contenant les beaux labels des variables pour les tableaux, graphiques etc. sera incorporé dans le tableau principal.
#'
#'Le csv doit avoir les réglages par défaut (UTF-8, séparateur de champs : ",") soit les réglages par défaut de LibreOffice
#'.
#' Toutes les variables "caractère" sont transformées en facteur.
#'
#' @param fich Fichier csv contenant les données
#' @param fich Fichier csv contenant les labels corrects des variables (colonne 'nom')
#' @import utils
#' @import stats
#' @import labelled
#' @import janitor
#' @import readr
#' @import stringr
#' @return data.frame
#'
#' @example debutph(patients,bnom)
#' @export
debutph <- function(fich, nom) {
  dfx <- read_csv(
    fich,
    col_names = TRUE,
    na = c("", "NA", " "),
    lazy = FALSE,
    show_col_types = FALSE
  ) |>
    mutate_if(is.character, as.factor) |>
    janitor::clean_names()
  #  janitor::remove_constant() |>
  #  janitor::remove_empty()

  names(dfx) <- stringr::str_replace_all(names(dfx), "_", ".")
  var_label(dfx) <- as.character(nom)

  return(dfx)
}
