#' Tableau de regresion logistique
#'
#' Trace le tableau d'une regression logistique avec les intitules vrais des variables.
#'
#' @param dfx nom du data-frame
#' @param vart nom de la variable dependante
#' @param vars vecteur des variables explicatives (garder l'ordre du data-frame)
#' @param bnom vecteur contenant les intitules corrects des variables (l'extraction des valeurs utiles est automatique)
#' @param titre titre du tableau
#' @param lab label
#' @param export si TRUE : exporte la table en csv?
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
#'           glmph(dfx = "irisz",vart = "zz",vars = c("Sepal.Width","Species"),bnom = names(irisz))
#'
glmph <-
  function(dfx,
           vart,
           vars,
           bnom ,
           titre = "Analyse multivari\u00e9e",
           lab = "",
           export = FALSE) {
    qq <- names(eval(parse(text = dfx)))
    znom <- which(qq %in% vars)
    bnom <- bnom[znom]
    zz <- paste(vars, collapse = "+")
    aaf <-
      parse(text = paste("glm(", vart, " ~ ", zz, ", data = ", dfx, ", family = binomial)"))
    ll <- eval(aaf)
    lod <- odds.ratio(ll)
    lod <- lod[-1,]
    lodd <-
      paste0(signif(lod[, 1], 3), " [", signif(lod[, 2], 3), " : ", signif(lod[, 3], 3), "]")
    bpo <- sapply(lod[, 4], beaup,affp = 0)
    #
    tabfin <- NULL
    lzz <- 1
    #
    for (l in seq_len(length(vars))) {
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
    if (export == TRUE) {
      nomcsv <- paste0(titre, ".csv")
      write.csv(tabfin, nomcsv)
    }
    ltit <- c(" ", "Odd-Ratio [IC 95 %]", "p")
    kable(
      tabfin,
      row.names = FALSE,
      col.names = ltit,
      caption = titre,
      label = lab,
      escape = FALSE
    ) %>%
      kable_styling(
        latex_options = c("repeat_header", "hold_position", "striped"),
        full_width = FALSE,
        position = "center",
        fixed_thead = TRUE,
        bootstrap_options = "striped"
      )
  }
