#' Debut du travail
#'
#' Lit un csv, simplifie & normalise les titres des variables. Le csv a ete cree par libreOffice avec les reglages by default.
#'
#' @param fich Fichier csv
#' @import utils
#' @import stats
#' @import janitor
#' @import readr
#' @return data.frame
#'
#' @example debutph("https://github.com/tidyverse/readr/raw/master/inst/extdata/mtcars.csv")
#' @export
debutph <- function(fich){
  df <- read_csv(fich,
                 col_names = TRUE,
                 na = c("", "NA"),
                 lazy = FALSE
                 ) %>%
    janitor::clean_names()
  return(df)
}
