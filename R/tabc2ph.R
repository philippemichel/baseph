#' Tableau comparatif
#'
#' Génération d'un tableau comparatif pour une étude clinique avec tests unitaires pour les deux groupes. Choix du type de test (paramétriques ou non).
#'
#' @param dfx Tibble
#' @param tri Variable de tri, comparaison
#' @param test Test paramétrique & chi2 si = moy, sinon, test de Wilcoxon + fisher
#' @param titre Nom à afficher au dessus des colonnes de comparaison
#' @param capt Titre du graphique
#'
#' @import dplyr
#' @import gtsummary
#' @import labelled
#' @return tableau
#' @export
#'
#' @examples tabcph(dfx = patients, tri = escarre, test = "moy", titre = "Escarre", capt = "Tableau 1")
#'
tabcph <-
  function(dfx,
           tri,
           test = "moy",
           titre = "",
           capt = "") {
    if (test == "moy") {
      vtest <- list(all_continuous() ~ "{mean} ± {sd}")
      ptest <- "t.test"
      ctest <- "chisq.test"
    } else {
      vtest <- list(all_continuous() ~ "{median} ({p25} ; {p75})")
      ptest <- "wilcox.test"
      ctest <- "fisher.test"
    }
    #
    dfx |>
      tbl_summary(
        by = {{ tri }},
        statistic = vtest,
        missing = "no"
      ) |>
      modify_header(label ~ " ") %>%
      modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**", titre, "**")) %>%
      modify_caption(paste0("**", capt, "**")) %>%
      bold_labels() |>
      add_p(test = list(all_continuous() ~ ptest, all_categorical() ~ ctest)) |>
      bold_p() |>
      add_n() |>
      add_overall(col_label = "**Total**")
  }
