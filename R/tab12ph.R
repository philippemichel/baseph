#' Tableau 1
#'
#' Génération d'un tableau simple pour une étude clinique avec tests unitaires pour les deux groupes. Aucun test.
#' 
#' Il n'y a pas de titre au tableau car cela interfère avec le titre du chunck.
#'
#' @param dfx Tibble
#' @param test Valeurs numériques en moyenne ± écart-type (`moy`) ou médiane (quartiles) (`med`)
#' @param note Titre du graphique
#' @param lt logique. TRUE crée un *longtable*
#' @param export logique. TRUE crée un export en xls
#'
#' @import dplyr
#' @import gtsummary
#' @import labelled
#' 
#' @return tableau
#' @export
#'
#' @examples tab1ph(dfx = patients, colx = 2:5, test = "zz")
#'
tab1ph <- function(dfx, colx = NULL, test = "med") {
  if (test == "med"){
    pp <- list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")
  } else {
    pp <- list(all_continuous() ~ "{mean} ± {sd}", all_categorical() ~ "{n} ({p}%)")
  }
  if (is.null(colx)) {colx = 1:ncol(dfx)}
  dfx |>
    dplyr::select(colx) |>
    tbl_summary(missing = "no",
                statistic = pp) |>
    modify_header(label ~ " ") |>
    bold_labels() |>
    add_n()
}

