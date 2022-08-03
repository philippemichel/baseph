#' Debut du travail
#'
#' Lit un csv, simplifie & normalise les titres des variables. Le csv a ete cree par libreOffice avec les reglages by default.
#' Toutes les variables "caractere" sont transformees en facteur.
#' @param fich Fichier csv
#' @import utils
#' @import stats
#' @import janitor
#' @import readr
#' @import stringr
#' @return data.frame
#'
#' @example debutph("https://github.com/tidyverse/readr/raw/master/inst/extdata/mtcars.csv")
#' @export
debutph <- function(fich) {
  df <- read_csv(
    fich,
    col_names = TRUE,
    na = c("", "NA"),
    lazy = FALSE,
    show_col_types = FALSE
  ) %>%
    mutate_if(is.character, as.factor) %>%
    janitor::clean_names() |>
    janitor::remove_constant() |>
    janitor::remove_empty()
  names(df) <- stringr::str_replace_all(names(df), "_", ".")
  return(df)
}
