#' Tableau comparatif (univarie)
#'
#' @param dfx table a esplorer
#' @param tri variable expliquee
#' @param nomv liste des vrais noms des variables
#' @param titre titre du tableau
#' @param lab label
#' @param export si TRUE, export en CSV
#'
#' @import knitr
#' @import kableExtra
#' @import rlang
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples tabcph(iris, Species, levels(iris$Species))
tabcph <- function(dfx,
                   tri,
                   nomv,
                   titre = "Tableau comparatif",
                   lab = "tabcomp",
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
  triz <- triz[[1]]
  for (ll in 1:length(dfx)) {
    varx <- dfx[, ll]
    varx <- varx[[1]]
    if (names(dfx)[ll] != vv) {
      nom <- paste0("<b>", nomv[ll], "</b>")
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
tabf <-   kable(
    tabx,
    row.names = FALSE,
    col.names = ltit,
    caption = titre,
    label = lab,
    escape = FALSE
  ) %>%
    kable_styling(
      bootstrap_options = "striped",
      full_width = FALSE,
      position = "center"
    )

  if (!is.null(ligd)) {add_indent(tabf, ligd)}
tabf %>%
  scroll_box(width = "100%", height = "850px")
}
