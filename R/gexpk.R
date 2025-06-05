#' Sortie d'un tableau gtsummary en tableau kableExtra avec adaptation à la page et comme feuille d'un tableur ods. 
#' 
#' @param dfk objet gtsummary 
#' @param exp  booleen.  TRUE : export vers ods
#' @param nomfich nom du classeur ods
#' @param nomsheet nom de la feuille dans le classeur ods
#' @param lg Booleeen. TRUE si sortie en longtable
#' 
#' @import readODS
#' @import kableExtra
#' @import dplyr
#' @import gtsummary
#'
#' @return un tableau kbl prévu pour un export LaTeX
#' @export
#'
#' @examples zz <- gtsummary::tbl_summary(iris)
#'           gexptabph(dfk = zz, exp = FALSE)
#' 
gexptabph <-
  function(dfk,
           exp = FALSE,
           nomfich = "export",
           nomsheet = "x",
           lg = FALSE) {
    zz <- dfk |>
      as_kable_extra(longtable = lg, booktabs = TRUE) |>
      kable_styling(latex_options = c("scale_down", "repeat_header"))
    if (exp) {
      dfk |>
        as_tibble() |>
        write_ods(path = nomfich, sheet = nomsheet, append = TRUE)
    }
    return(zz)
  }
