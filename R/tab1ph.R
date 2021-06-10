#' Tableau descriptif
#'
#' @param dfx Data.frame to explore
#' @param titre title of the table
#' @param label label of the table for RMarkdown or LaTeX
#'
#' @import stats
#' @import kableExtra
#' @import knitr
#'
#' @return
#' @export
#'
#' @examples tab1ph(iris, "Table 1")
tab1ph <- function(dfx, titre = "Tableau descriptif", label = "tabd"){
  tabp <- NULL
  ligd <- NULL
  nlig <- 0
  for(i in 1:ncol(dfx)){
    nom <- paste0("<b>",names(dfx)[i],"</b>")
    varx <- na.omit(dfx[,i])
    varx <- varx[[1]]
    if(is.numeric(varx)){
      ll <- lignum(varx, nom)
      nlig <- nlig + 1
    }else{
      llx <- ligfact(varx, nom)
      ll <- llx$ttz
      nlig <- nlig + llx$nl + 1
      ligd <- c(ligd, seq(nlig-llx$nl+1,nlig))
    }
    tabp <- rbind(tabp,ll)
  }
  kable(
    tabp,
    row.names = FALSE,
    col.names = c("", "moy \u00b1 et  N/total (%)", "IC 95 %"),
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
