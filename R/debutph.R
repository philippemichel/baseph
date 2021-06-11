#' Debut du travail
#'
#' Lit un csv, simplifie & normalise les titres des variables. Le csv a ete cree par libreOffice avec les reglages by default.
#'
#' @param fich Fichier csv
#' @import utils
#' @import stats
#' @import janitor
#' @return data.frame
#'
#' @example print("pas d'exemple possible")
#'
#' @export
debutph <- function(fich){
  df <- read.csv(fich, header = TRUE, na.strings = c("NA",""), dec = ",", as.is=FALSE) %>%
     janitor::clean_names()
  df <- as_tibble(df)
  return(df)
}
