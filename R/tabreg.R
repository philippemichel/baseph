#' Tableau de régression
#'
#' @param ll Résultat une analyse en régression
#' @param titre Titre du tableau
#'
#' @return tableau
#' @export
#'
#' @examples ll <- glm(escarre~., data = patients, family = "binomial")
#' tabregph(reg = ll, titre = "Facteurs de risque d'escarre" )
#'
tabregph <- function(reg, titre = "") {
  tbl_regression(reg, exponentiate = TRUE) |>
    bold_labels() |>
    bold_p() |>
    modify_caption(paste0("**", titre, "**"))
}
