#' Tableau de regresion logistique
#'
#' Trace le tableau d'une regression logistique avec les intitules vrais des variables.
#'
#' @param dfx nom du data-frame
#' @param vart nom de la variable dependante
#' @param vars vecteur des variables explicatives
#' @param bnom liste des intitules corrects des variables
#' @param titre titre du tableau
#' @param lab label
#'
#' @import knitr
#' @import dplyr
#' @import kableExtra
#' @import questionr
#'
#' @return tableau
#' @export
#'
#' @examples irisz <- iris
#'           irisz$zz <- as.factor(rep(c("a","b"),75))
#'           glmph(dfx = "irisz",vart = "zz",vars = c("Species","Sepal.Width"),bnom = names(iris))
#'
glmph <- function(dfx,
                  vart,
                  vars,
                  bnom ,
                  titre = "",
                  lab = "") {
  qq <- names(eval(parse(text = dfx)))
  znom <- which(qq %in% vars)
  bnom <- bnom[znom]
  zz <- paste(vars, collapse = "+")
  aaf <-
    parse(text = paste("glm(", vart, " ~ ", zz, ", data = ", dfx, ", family = binomial)"))
  ll <- eval(aaf)
  lod <- odds.ratio(ll)
  lod <- lod[-1, ]
  lodd <-
    paste0(signif(lod[, 1], 3), " [", signif(lod[, 2], 3), " : ", signif(lod[, 3], 3), "]")
  bpo <- sapply(lod[, 4], beaup)
  #
  tabfin <- NULL
  lzz <- 1
  #
  for (l in 1:length(vars)) {
    xx <- eval(parse(text = paste(dfx, "$", vars[l])))
    if (is.numeric(xx)) {
      lfin <- c(paste0("<b>", bnom[l], "<b>"), lodd[lzz], bpo[lzz])
      tabfin <- rbind(tabfin, lfin)
      lzz <- lzz + 1
    }
    else{
      lfin <- c(paste0("<b>", bnom[l], "<b>"), "", "")
      tabfin <- rbind(tabfin, lfin)
      lfin <- c(levels(xx)[1], "", "1")
      tabfin <- rbind(tabfin, lfin)
      for (i in(2:length(levels(xx)))) {
        lfin <- c(levels(xx)[i], lodd[lzz], bpo[lzz])
        lzz <- lzz + 1
        tabfin <- rbind(tabfin, lfin)
      }
    }
  }
  ltit <- c(" ", "Odd -Ratio [IC 95 %]", "p")
  kable(
    tabfin,
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
}
