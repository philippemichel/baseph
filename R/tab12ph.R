#' Tableau 1
#'
#' Génération d'un tableau simple pour une étude clinique avec tests unitaires pour les deux groupes. Aucun test.
#'
#' @param dfx Tibble
#' @param test Valeurs numériques en moyenne ± écart-type (`moy`) ou médiane (quartiles) (`med`)
#' @param titre Nom à afficher au desu des colonnes de comparaison
#' @param capt Titre du graphique
#'
#' @import dplyr
#' @import gtsummary
#' @import labelled
#' @return tableau
#' @export
#'
#' @examples tabdph(dfx = patients, test = "med", titre = "Escarre", capt = "Tableau 1")
#'
tab1ph <-
  function(dfx,
           titre = "",
           test = "med",
           capt = ""){
    if (test == "moy") {
      vtest <- list(all_continuous() ~ "{mean} ± {sd}")
    } else {
      vtest <- list(all_continuous() ~ "{median} ({p25} ; {p75})")
    }
    dfx |>
      tbl_summary(
        statistic = vtest,
        missing = "no"
      ) |>
      modify_header(label ~ " ") %>%
      modify_caption(paste0("**", capt, "**")) %>%
      bold_labels() |>
      add_n()

  }
