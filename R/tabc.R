#' Tableau comparatif (univarie)
#'
#' @param dfx table à explorer
#' @param tri variable expliquée
#' @param nlignes numéro des lignes à inclure dans le tableau
#' @param nomv liste des vrais noms des variables
#' @param titre titre du tableau
#' @param nomvar nom de la variable de tri (nom des variables par défaut)
#' @param lab label
#' @param export si TRUE, export en CSV
#' @param fnote note en bas de tableau
#' @param test paramétrique : test = "moy"; non "paramétrique : tet = "med"
#'
#' @import knitr
#' @import kableExtra
#' @import rlang
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples tabcph(dfx = patients, tri = alite.7.j.av,
#' nomv= nn$noms,
#' titre = "Tableau comparatif", nlignes = c(2:8),
#' nomvar = "Alité avant",
#' lab = "tabcomp",
#' test = "moy",
#' fnote = "Résultats selon l'alitement avant", export = FALSE)
#'
tabcph <- function(dfx,
                   tri,
                   nlignes= NULL,
                   nomv = NULL,
                   titre = "Tableau comparatif",
                   nomvar = "Traitement",
                   lab = "tabcomp",
                   fnote = "",
                   test = "moy",
                   export = FALSE) {
 # Tableau restreint aux colonnes désirées, sans NA
  if (is.null(nlignes)){
    nlignes <- 1:ncol(dfx)
  }
  dfx <- dfx |>
    select(nlignes) |>
    dplyr::filter(!is.na({{tri}}))
  #
  if (is.null(nomv)){nomv = names(dfx)}
  else {nomv <- nomv[nlignes]}
  #
  if (test == "moy"){
    fnote <- paste0 ( "moyenne \u00b1 ecart-type - nb/total (%) ", fnote)
  } else {
    fnote <- paste0 ( "m\u00E9diane (quartiles) - nb/total (%) ", fnote)
  }
  #
  tabx <- NULL
  nlig <- 0
  ligd <- c()
  trix <- enquo(tri)
  vv <- quo_name(trix)
  triz <- dfx[vv]
  triz <- as.factor(triz[[1]])
  for (ll in seq_len(length(dfx))) {
    varx <- dfx[, ll]
    varx <- varx[[1]]
    if (names(dfx)[ll] != vv) {
      nom <- paste(text_spec(nomv[ll], bold = TRUE))
      if (is.numeric(varx)) {
        # Variables numériques
        if (test == "moy"){
        lig <- lignumc(nom, varx, triz)
        } else {
          lig <- ligmedc(nom, varx, triz)
        }
        tabx <- rbind(tabx, lig)
        nlig <- nlig + 1
      } else {
        # Variables factorielles
        if (is.factor(varx) == FALSE) {
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
  aa <- c("",length(levels(triz)), "")
  names(aa) <- c("", nomvar, "")
  ntabx <- 1:nrow(tabx)
kbl(
    tabx,
    row.names = FALSE,
    col.names = ltit,
    caption = titre,
    label = lab,
    escape = FALSE,
    booktabs = TRUE,
    longtable = TRUE,
    linesep = ""
  ) |>
  add_header_above(aa) %>%
    kable_styling(
      latex_options = c("repeat_header", "hold_position", "striped"),
     stripe_index = ntabx[!(ntabx %in% ligd)],
      full_width = FALSE,
      position = "center",
      fixed_thead = TRUE
    ) %>%
  footnote(general = fnote) |>
  add_indent(ligd) |>

  scroll_box(width = "100%", height = "850px")
}
