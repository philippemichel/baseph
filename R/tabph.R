#' TABPH
#' 
#' Réglages pour un tableau généré par gtsummary, le plus souvent tbl_summary (avec une variable de tri & des tests statistiques).
#'
#' @param tbx un tableau gtsummary
#' @param nn nombre de valeurs dans la variable de tri
#' @param nomv nom à afficher pour la variable de tri
#' @param normx si variables numériques normales TRUE
#' @import gtsummary
#' @import tidyverse
#'
#' @return un tableau fignolé 
#' @export
#' 
#'
#' @examples tabph(gtsummary::tbl_summary(patients[,1:4],by =sexe),  nn = 2, nomv = "Sexe", normx = FALSE)
#' 
tabph <- function(tbx,
                  nn = 2,
                  nomv = "",
                  normx = FALSE) {
  if (normx) {
    ccx <-  "t.test"
  } else {
    ccx <- "wilcox.test"
  }
  nom <- paste0("**", nomv, "**")
  if (nn > 2) {
    cc <- "aov"
  } else {
    cc <- ccx
  }
  nn <- paste0("stat_", 1:nn)
  tbx |>
    modify_header(label ~ " ") |>
    modify_spanning_header(nn ~ nom) |>
    add_p(test = list(all_continuous() ~ cc, all_categorical() ~ "chisq.test")) |>
    add_n(col_label = "**N**") |> 
    add_overall(col_label = "**Total**  \nN = {style_number(N)}") |>
    bold_labels()
}
