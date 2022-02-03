#' Tableau descriptif
#' Averc les intitules des variables en clair
#' @param dfx Data.frame to explore
#' @param titre title of the table
#' @param label label of the table for RMarkdown or LaTeX
#' @param nomv liste des vrais noms des variables
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
#' @examples tab1ph(iris,nomv =names(iris),titre = "Table 1", label = "tabiiris")
tab1ph <- function(dfx,nomv, titre = "Tableau descriptif", label = "tabd", export = FALSE){
    tabp <- NULL
    ligd <- NULL
    nlig <- 0
    for (i in 1:ncol(dfx)) {
      nom <- paste0("<b>", nomv[i], "</b>")
      varx <- select(dfx, i)
      varx <- na.omit(varx[[1]])
      # Variables numeriques
      if (is.numeric(varx)) {
        bornes <- moyciph(varx, ci = 95)
        tbf <- paste0("[", signif(bornes[1], 3), " ; ", signif(bornes[2],
                                                               3), "]")
        ll <- c(nom, paste0(signif(mean(varx), 3), " ± ",
                            signif(sd(varx), 3)), tbf)
        nlig <- nlig + 1
        # variables non numeriques
      } else {
        if (is.factor(varx) == FALSE) {
          varx <- as.factor(varx)
        }
        # Calcul & ecriture de la ligne factorielle
        ll <- c(nom, " ", " ")
        zz <- table(varx)
        tot <- length(varx)
        nl <- length(levels(varx))
        for (l in 1:nl) {
          nom <- names(zz)[l]
          nz <- zz[[l]]
          pc <- signif(100 * nz/tot, 3)
          #
          cf <- transangph(nz, tot)
          #
          llf <- c(nom, paste0(nz, "/", tot, " (", pc,
                               " %)"), cf$nb)
          ll <- rbind(ll, llf)
        }
        #
        nlig <- nlig + nl + 1
        ligd <- c(ligd, seq(nlig - nl + 1, nlig))
      }
      tabp <- rbind(tabp, ll)
    }
    # Export
    if (export == TRUE) {
      nomcsv <- paste0(titre, "_export_table1.csv")
      write.csv(tabp, nomcsv)
    }
    # Ecriture du tableau
    kable(tabp, row.names = FALSE, col.names = c("", "moy ± \u00E9cart-type <br> N/total (%)",
                                                 "IC 95 %"), caption = titre, escape = FALSE) %>%
      kableExtra::kable_styling(bootstrap_options = "striped",
                                full_width = FALSE, position = "center", fixed_thead = TRUE) %>%
      add_indent(ligd)
  }
