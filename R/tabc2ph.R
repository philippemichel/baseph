#' Tableau comparatif
#'
#' Génération d'un tableau comparatif pour une étude clinique avec tests unitaires pour les deux groupes. Choix du type de test (paramétriques ou non).
#'
#' @param dfx Tibble
#' @param trix Variable de tri, comparaison
#' @param test Test paramétrique & chi2 si = moy, sinon, test de Wilcoxon + fisher
#' @param tit Nom à afficher au dessus des colonnes de comparaison
#' @param note Titre du graphique
#' @param lt logique. TRUE crée un *longtable*
#' @param export logique. TRUE crée un export en xls
#'
#' @import dplyr
#' @import gtsummary
#' @import labelled
#' @import WriteXLS
#' @import kableExtra
#' @return tableau
#
#' @export
#'
#' @examples tabcph(dfx = patients, trix = escarre, test = "moy", tit = "Escarre", note = "Tableau 1", lt = FALSE, export = FALSE)
#'
tabcph  <- function(dfx, trix,  colx = NULL, test = "med") {
    if (test == "med"){
      pp <- list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")
      ptest <- list(all_continuous() ~ "wilcox.test", all_categorical() ~ "fisher.test")
    } else {
      pp <- list(all_continuous() ~ "{mean} ± {sd}", all_categorical() ~ "{n} ({p}%)")
      ptest <- list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test")
    }
    if (is.null(colx)) {colx = 1:ncol(dfx)}
    dfx |>
      dplyr::select(colx) |>
      tbl_summary(missing = "no",
                  by = {{trix}},
                  statistic = pp) |>
      modify_header(label ~ " ") |>
      bold_labels() |>
      add_n() |> 
      add_p(test = ptest) |> 
      bold_p()
  }
