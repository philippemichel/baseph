#' Tableau descriptif
#' Averc les intitules des variables en clair
#' @param dfx Data.frame to explore
#' @param nom vecteur avec les vrais noms des variables
#' @param titre title of the table
#' @param label label of the table for RMarkdown or LaTeX
#'
#' @import stats
#' @import dplyr
#' @import kableExtra
#' @import knitr
#'
#' @return
#' @export
#'
#' @examples tab1bph(iris,nom =names(iris),titre = "Table 1", label = "tabiiris")
tab1ph <- function(dfx,nomv, titre = "Tableau descriptif", label = "tabd"){
  tabp <- NULL
  ligd <- NULL
  nlig <- 0
  for (i in 1:ncol(dfx)){
    nom <- paste0("<b>",nomv[i],"</b>")
    varx <- na.omit(dfx[,i])
    varx <- varx[[1]]
    if(is.numeric(varx)){ # Variables numeriques
      ll <- lignum(varx, nom)
      nlig <- nlig + 1
    }else{ # variables non numeriques
      if(is.factor(varx)==FALSE){
        varx <- as.factor(varx)
      }
      llx <- ligfact(varx, nom)
      ll <- llx$ttz
      nlig <- nlig + llx$nl + 1
      ligd <- c(ligd, seq(nlig-llx$nl+1,nlig))
    }
    tabp <- rbind(tabp,ll)
  }
  # Ecriture du tableau
  kable(
    tabp,
    row.names = FALSE,
    col.names = c("", "moy \u00b1 et  N/total (%)", "IC 95 %"),
    caption = titre,
    escape = FALSE
  ) %>%
    kableExtra::kable_styling(
      bootstrap_options = "striped",
      full_width = FALSE,
      position = "center",
      fixed_thead = TRUE
    ) %>%
    add_indent(ligd)
}
