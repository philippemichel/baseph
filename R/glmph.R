#' Regression logistique, sortie en un tableau.
#'
#' @param dfx Data-frame (chaine de caracteres)
#' @param vart Variable de tri (chaine de caracteres)
#' @param vars Variables a explorer (un vecteur)
#' @param titre Titre du tableau
#' @param lab Label du tableau
#'
#' @import questionr
#' @import knitr
#' @import kableExtra
#' @import dplyr
#'
#' @return tableau
#' @export
#'
#' @examples counts <- c(18,17,15,20,10,20,25,13,12)
#'           outcome <- gl(3,1,9)
#' treatment <- as.factor(c(rep("o",4),rep("n",5)))
#' dff <- data.frame(treatment, outcome, counts)
#' glmph(dfx = "dff", vart =  "treatment", vars = c("counts", "outcome" ), titre = "test", lab = "tabtest")
#'
#'
glmph <- function(dfx, vart, vars, titre ="", lab = ""){
  zz <- paste(vars, collapse = "+")
  aaf <- parse(text = paste("glm(",vart," ~ ", zz, ", data = ",dfx, ", family = binomial)"))
  ll <- eval(aaf)
  lod <- odds.ratio(ll)
  lod <- lod[-1,]
  lodd <- paste0(signif(lod[,1],3), " [",signif(lod[,2],3), " : ",signif(lod[,3],3),"]")
  bpo <- sapply(lod[,4],beaup)
  #
  tabfin <- NULL
  lzz <- 1
  #
  for (l in 1:length(vars)) {
    xx <- eval(parse(text = paste(dfx,"$",vars[l])))
    if (is.numeric(xx)) {
      lfin <- c(paste0("<b>",vars[l],"<b>"), lodd[lzz], bpo[lzz])
      tabfin <- rbind(tabfin,lfin)
      lzz <- lzz + 1
    }
    else{
      lfin <- c(paste0("<b>",vars[l],"<b>"),"","")
      tabfin <- rbind(tabfin,lfin)
      lfin <- c(levels(xx)[1],"","1")
      tabfin <- rbind(tabfin,lfin)
      for(i in(2 : length(levels(xx)))){
        lfin <- c(levels(xx)[i], lodd[lzz], bpo[lzz])
        lzz <- lzz + 1
        tabfin <- rbind(tabfin,lfin)
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
