#' Tableau descriptif
#' Avec les intitules des variables en clair
#' @param dfx Data.frame to explore
#' @param nlignes numéro des lignes a inclure dans le tableau
#' @param titre title of the table
#' @param label label of the table for RMarkdown or LaTeX
#' @param nomv liste des vrais noms des variables (names(dfx by default))
#' @param test présentation des résultats en moy/écart-type si ="moy", en médiane quartiles si = "med"
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
#' @examples tab1ph(patients, nomv = names(patients), nlignes = c(2:8),
#'  titre = "Table 1", label = "tab1", test = "moy")
#'
tab1ph <-
  function(dfx,
           nomv = NULL,
           nlignes= NULL,
           titre = "Tableau descriptif",
           label = "tabd",
           test = "moy",
           export = FALSE) {
    tabp <- NULL
    ligd <- NULL
    nlig <- 0
    #
    if (is.null(nlignes)){
      nlignes <- 1:ncol(dfx)

    }
    dfx <- select(dfx,nlignes)
    #
    if (is.null(nomv)){nomv = names(dfx)}
    else {nomv <- nomv[nlignes]}
    #
    for (i in 1:ncol(dfx)) {
      #nom <- paste0("<b>", nomv[i], "</b>")
      nom <- (paste(text_spec(nomv[i],bold = TRUE)))
      varx <- select(dfx, i)
      varx <- na.omit(varx[[1]])
      #
      # Variables numeriques
      if (is.numeric(varx)) {
        if (test == "moy"){
        ll <- c(nom, paste0(signif(mean(varx), 3), " \u00b1 ",
                            signif(sd(varx), 3)))}
      else{
        ll <- c(nom, paste0(signif(median(varx), 3), " (",quantile(varx)[2]," ; ",quantile(varx)[5],")"))
      }
        nlig <- nlig + 1
      }
        #
        # variables non numeriques
       else {
        if (is.factor(varx) == FALSE) {
          varx <- as.factor(varx)
        }
        #
        # Calcul & écriture de la ligne factorielle
        ll <- c(nom, " ")
        zz <- table(varx)
        tot <- length(varx)
        nl <- length(levels(varx))
        for (l in 1:nl) {
          nom <- names(zz)[l]
          nz <- zz[[l]]
          pc <- signif(100 * nz / tot, 3)
          #
          llf <- c(nom, paste0(nz, "/", tot, " (", pc,
                               " \\%)"))
          ll <- rbind(ll, llf)
        }
        #
        nlig <- nlig + nl + 1
        ligd <- c(ligd, seq(nlig - nl + 1, nlig))
      }
      tabp <- rbind(tabp, ll)
    }
    #
    # Export
    if (export == TRUE) {
      nomcsv <- paste0(titre, "_table1.csv")
      write.csv(tabp, nomcsv)
    }
    #
    # Ecriture du tableau
    kable(
      caption = titre,
      escape = FALSE,
      tabp,
      row.names = FALSE,
      col.names = linebreak(c("", "moy \u00b1 \u00E9cart-type \n N/total (\\%)")),
      booktabs = TRUE,
      longtable = TRUE
    ) %>%
      kableExtra::kable_styling(
        latex_options = c("striped","repeat_header", "hold_position"),
        bootstrap_options = "striped",
        full_width = FALSE,
        position = "center",
        fixed_thead = TRUE
      ) %>%
      add_indent(ligd) |>
      scroll_box(width = "100%", height = "850px")
  }
