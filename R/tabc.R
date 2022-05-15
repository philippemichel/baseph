#' Tableau comparatif (univarie)
#'
#' @param dfx table a esplorer
#' @param tri variable expliquee
#' @param nomv liste des vrais noms des variables
#' @param titre titre du tableau
#' @param lab label
#' @param export si TRUE, export en CSV
#' @param fnote note en bas de tableau
#'
#' @import knitr
#' @import kableExtra
#' @import rlang
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples tabcph(iris, Species, levels(iris$Species), fnote = "demonstration")
tabcph <- function(dfx,
                   tri,
                   nomv,
                   titre = "Tableau comparatif",
                   lab = "tabcomp",
                   fnote = "",
                   export = FALSE) {
  # On supprime les données manquantes dans la variable de tri
  dfx <- as_tibble(dfx)
  dfx <- dfx %>%
    dplyr::filter(!is.na({{tri}}))
  #
  tabx <- NULL
  nlig <- 0
  ligd <- NULL
  trix <- enquo(tri)
  vv <- quo_name(trix)
  triz <- dfx[vv]
  triz <- as.factor(triz[[1]])
  for (ll in seq_len(length(dfx))) {
    varx <- dfx[, ll]
    varx <- varx[[1]]
    if (names(dfx)[ll] != vv) {
      nom <- paste(text_spec(nomv[ll],bold = TRUE))
      if (is.numeric(varx)) {
        # Variables numériques
        lig <- lignumc(nom, varx, triz)
        tabx <- rbind(tabx, lig)
        nlig <- nlig + 1
      } else {
        # Variables factorielles
        if(is.factor(varx)==FALSE){
          varx <- as.factor(varx)
        }
        lig <- ligfc(nom, varx, triz)
        tabx <- rbind(tabx, lig)
        llx <- length(levels(varx))
        nlig <- nlig + 1 + llx
        ligd <- c(ligd, seq(nlig - llx + 1, nlig))
      }
    }
  }
  # Export
  if (export) {
    nomcsv <- paste0(titre, "_export_comparatif.csv")
    write.csv(tabx, nomcsv)
  }
  # Création tableaux
  ltit <- c(" ", levels(triz), "p")
 # cs_dt$mpg = cell_spec(cs_dt$mpg, color = ifelse(cs_dt$mpg > 20, "red", "blue"))
kable(
    tabx,
    row.names = FALSE,
    col.names = ltit,
    caption = titre,
    label = lab,
    escape = FALSE,
    booktabs = TRUE,
    longtable = TRUE
  ) %>%
    kable_styling(
      latex_options = c("striped","repeat_header", "hold_position"),
      bootstrap_options = "striped",
      full_width = FALSE,
      position = "center",
      fixed_thead = TRUE
    ) %>%
  footnote(general = fnote) |>
 add_indent(ligd) |>
  scroll_box(width = "100%", height = "850px")
}
