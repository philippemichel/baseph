#' Sortie d'un tableau gtsummary en tableau kableExtra (pour export pdf via LaTeX) sans adaptation à la page ou comme feuille d'un tableur ods. Longtable possible
#'
#' @param dfk objet gtsummary 
#' @param exp  booleen. FALSE : vers kableExtra, TRUE : vers ods
#' @param nomfich nom du classeur ods
#' @param nomsheet nom de la feuille dans le classeur ods
#' @param lg Booleeen. TRUE si sortie en longtable
#' 
#' @import readODS
#' @import kableExtra
#' @import dplyr
#' @import gtsummary
#'
#' @export
#' @return un tableau kbl prévu pour un export LaTeX
#'
#' @examples zz <- gtsummary::tbl_summary(iris)
#'           pexptabph(dfk = zz, exp = FALSE)
#' 
pexptabph <-
  function(dfk,
           exp = FALSE,
           nomfich = "export",
           nomsheet = "x",
           lg = FALSE) {
    zz <- dfk |>
      gtsummary::as_kable_extra(longtable = lg, booktabs = TRUE) |>
      kable_styling(latex_options = c("repeat_header"))
    if (exp) {
      dfk |>
        as_tibble() |>
        write_ods(path = nomfich, sheet = nomsheet, append = TRUE)
    }
    return(zz)
  }
