#' Export de tableau 
#' 
#' Sortie d'un tableau gtsummary en tableau kableExtra avec adaptation à la page ou comme feuille d'un tableur xls.  
#'
#' @param dfk objet gtsummary 
#' @param exp  booleen. FALSE : vers kableExtra, TRUE : vers xls
#' @param nomfich nom du classeur xls
#' @param nomsheet nom de la feuille dans le classeur xls
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
      kable_styling(latex_options = c("HOLD_position", "scale_down", "repeat_header"))
    if (exp) {
      dfk |>
        as_tibble() |>
<<<<<<< HEAD
        write_ods(path = nomfich, sheet = nomsheet, append = TRUE)
=======
        xlsx::write.xlsx(nomfich, sheetName = nomsheet, append = TRUE)
>>>>>>> 818a468d053708ebadf5e9fd123b951494ef472c
    }
    return(zz)
  }
