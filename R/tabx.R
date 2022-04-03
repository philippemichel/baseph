#' Tableau descriptif
#' Avec les intitules des variables en clair
#' @param dfx Data.frame to explore
#' @param titre title of the table
#' @param label label of the table for RMarkdown or LaTeX
#' @param nomv liste des vrais noms des variables (names(dfx by default))
#' @param export si TRUE, export en CSV
#'
#' @import stats
#' @import dplyr
#' @import kableExtra
#' @import knitr
#'
#' @return
#' @export
#'
#' @examples tab1ph(iris, nomv = names(iris), titre = "Table 1", label = "tabiiris")
tabx <-
  function(dfx,
           nomv = "**",
           titre = "Tableau descriptif",
           label = "tabd",
           export = FALSE) {
    tabp <- NULL
    if (length(nomv) == 1) {
      nomv <- names(dfx)
    }
    for (i in 1:ncol(dfx)) {
      #nom <- paste0("<b>", nomv[i], "</b>")
      nom <- nomv[i]
      varx <- select(dfx, i)
      varx <- na.omit(varx[[1]])
      # Variables numeriques
      if (is.numeric(varx)) {
        bornes <- moyciph(varx, ci = 95)
        tbf <-
          paste0("[", signif(bornes[1], 3), " ; ", signif(bornes[2],
                                                          3), "]")
        ll <- c(nom, paste0(signif(mean(varx), 3), " \u00b1 ",
                            signif(sd(varx), 3)), tbf, "")
        # variables non numeriques
      } else {
        if (is.factor(varx) == FALSE) {
          varx <- as.factor(varx)
        }
        # Calcul & ecriture de la ligne factorielle
        zz <- table(varx)
        tot <- length(varx)
        nl <- length(levels(varx))
        ll <- NULL
        for (l in 1:nl) {
          noml <- names(zz)[l]
          nz <- zz[[l]]
          pc <- signif(100 * nz / tot, 3)
          #
          cf <- transangph(nz, tot)
          #
          llf <- c(noml, paste0(nz, "/", tot, " (", pc,
                                " %)"), cf$nb, nom)
          ll <- rbind(ll, llf)
        }
        #
      }
      tabp <- rbind(tabp, ll)
    }
    tabx <- as_tibble(tabp)
    names(tabx) <- c("nom", "Moyenne", "IC 95 %", "groupe")
    print(tabx)
    # Export
    if (export == TRUE) {
      nomcsv <- paste0(titre, "_table1.csv")
      write.csv(tabp, nomcsv)
    }
    # Ecriture du tableau
    gt(
      tabx,
      rowname_col = "nom",
      groupname_col = "groupe",
      locale = "fr_FR"
    ) |>
      tab_header(title = md("**Description de la population**"),
                 subtitle = "") |>
      tab_options(row_group.font.weight = "bold",
                  row_group.as_column = FALSE) |>
      tab_footnote(
        footnote = "Données numériques : moyenne ± écart-type, Données discrètes : nombre/total (%)",
        locations = cells_column_labels(columns = "Moyenne")
      ) |>
      tab_footnote(
        footnote = "Intervalle de confiance à 95 % de la moyenne ou du pourcentage",
        locations = cells_column_labels(columns = "IC 95 %")
      )
  }
