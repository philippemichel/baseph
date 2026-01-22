#' Tableau Odds-Ratio, Risque Relatif & nb à traiter
#'
#' @param df un tibble
#' @param intervention variable d'entree
#' @param nomv Variable de sortie
#' @import biostats
#' @import kableExtra
#' @import tidyverse
#'
#' @return un tableau
#' @export
#'
#'
#' @examples tab_oo(patients, "sexe", "escarre")
tab_oo <- function(df, intervention, evenement) {
  zz <- table(df[[intervention]], df[[evenement]])
  oo <- biostats::effect_measures(zz[1, 2], zz[1, 1], zz[2, 2], zz[2, 1], correction = TRUE)
  or1 <- paste0("[", round(oo$or_ci[1], 2), "; ", round(oo$or_ci[2], 2), "]")
  rr1 <- paste0("[", round(oo$rr_ci[1], 2), "; ", round(oo$rr_ci[2], 2), "]")
  ot <- tibble(
    Mesure = c("Odds Ratio", "Risque Relatif", "Nombre a Traiter"),
    Estimation = c(round(oo$odds_ratio, 2), round(oo$risk_ratio, 2), round(oo$nnt_nnh, 1)),
    IC_95 = c(or1, rr1, "")
  ) |>
    kableExtra::kable(
      booktabs = TRUE, escape = FALSE,
      col.names = c("Mesure d'association", "Estimation", "IC 95%")
    ) |>
    kableExtra::kable_styling(latex_options = c("hold_position"))

  return(ot)
}
