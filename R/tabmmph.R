#' Tableau multivarie
#'
#' @param ll Regression logistique
#' @param titre Titre - "Analyse multivariee" by default
#' @param lab  Label - "tabmulti" by default
#' @param export TRUE : export csv (FALSE by default)
#'
#' @return un tableau LaTeX , tableau csv
#'
#' @import epiDisplay
#' @import stats
#' @import knitr
#' @import kableExtra
#' @import utils
#' @import rlang
#' @import dplyr
#'
#' @examples mydata <- lm(carb~wt + am, data= mtcars)
#' tabmmph(ll =  mydata,titre = "Analyse multivari\u00e9e", lab = "tabmulti", export = FALSE)
#'
#' @export
tabmmph <-
  function(ll ,
           titre = "Analyse multivari\u00e9e",
           lab = "tabmulti",
           export = FALSE) {
    tbf <- c()
    llx <- logistic.display(ll, simplified = TRUE)$table
    rn <- rownames(llx)
    llx <- signif(llx, 3)
    for (nl in 1:length(rn)) {
      ic <- paste0("[", llx[nl, 2], " ; ", llx[nl, 3], "]")
      lf <- c(rn[nl], llx[nl, 1], ic, llx[nl, 4])
      tbf <- rbind(tbf, lf)
    }
    # Export
    if (export == 1) {
      write.csv(tbf, "export_comparatif_multi.csv")
    }
    tbf[, 1] <- chartr("_", " ", tbf[, 1])
    #
    ltit <- c(" ", "Odd-Ratio", "IC 95 %", "p")
    kable(
      tbf,
      caption = titre,
      label = lab,
      col.names = ltit,
      row.names = FALSE,
      digits = 2,
      escape  = FALSE
    ) %>%
      kable_styling(
        bootstrap_options = "striped",
        full_width = FALSE,
        position = "center"
      )
  }
