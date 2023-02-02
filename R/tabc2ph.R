#' Tableau comparatif
#'
#' Génération d'un tableau comparatif pour une étude clinique avec tests unitaires pour les deux groupes. Choix du type de test (paramétriques ou non).
#'
#' @param dfx Tibble
#' @param tri Variable de tri, comparaison
#' @param test Test paramétrique & chi2 si = moy, sinon, test de Wilcoxon + fisher
#' @param titre Nom à afficher au dessus des colonnes de comparaison
#' @param note Titre du graphique
#' @param lt logique. TRUE crée un *longtable*
#' @param export logique. TRUE crée un export en xls
#'
#' @import dplyr
#' @import gtsummary
#' @import labelled
#' @import WriteXLS
#' @import kableExtra
#' @return tableau
#
#' @export
#'
#' @examples tabcph(dfx = patients, trix = escarre, test = "moy", tit = "Escarre", note = "Tableau 1", lt = FALSE, export = FALSE)
#'
tabcph  <- function(dfx, trix, test = "moy", tit = "", note ="", lt = FALSE, export = FALSE){
  aa <- enquo(trix)
  ntrix <- as.character(aa[2])
  if (test == "moy"){
    note <- "n (%) - moyenne ± écart type"
  }
  else{
    note <- "n (%) - médiane (quartiles)"
  }
  dfx <- dfx |>
    drop_na({{trix}})
  trix <- dfx |>
    pull({{trix}})
  #  print(length(trix))
  tabf <- NULL
  tlig <- 1
  nl <- 1
  #
  for (l in 1:ncol(dfx)) {
    varx <- pull(dfx[,l])
    ltri <- length(na.omit(varx))
    nom <- paste0("",var_label(varx),"")
    if (names(dfx[,l]) != ntrix) {
      # Factoriel
      if (!is.numeric(varx)) {
        zz <- chisq.test(varx,trix, correct = FALSE)
        if (min(zz$expected) < 5) {
          zz <- chisq.test(varx,trix, correct = TRUE)
        }
        pp <- beaup(zz$p.value)
        tabp <- c(nom,ltri,"","",pp)
        bb <- table(varx,trix)
        aa <- prop.table(bb,2)*100
        for (l in 1:length(levels(varx))) {
          ll <- c(levels(varx)[l],
                  "",
                  paste0(bb[l,1]," (",round(aa[l,1],1),"%)"),
                  paste0(bb[l,2]," (",round(aa[l,2],1),"%)"),"")
          tabp <- rbind(tabp,ll)
        }
        nl <- nl + length(levels(varx))
      }
      else{
        # Numérique
        dfz <- tibble(varx,trix)
        if (test == "moy"){
          zz <- t.test(varx~trix, var.equal = TRUE)
          pp <- beaup(zz$p.value)
          aa <- dfz |>
            group_by(trix) |>
            summarise(mmx = mean(varx, na.rm = TRUE), dd = sd(varx, na.rm = TRUE))
          aa1 <- paste0(round(aa[[1,2]],2), " ± ", round(aa[[1,3]],2))
          aa2 <- paste0(round(aa[[2,2]],2), " ± ", round(aa[[2,3]],2))
          tabp <- c(nom,ltri,aa1,aa2,pp)
        }
        else{
          zz <- wilcox.test(varx~trix)
          pp <- beaup(zz$p.value)
          aad <- dfz |>
            group_by(trix) |>
            summarise(mmx = median(varx, na.rm = TRUE),
                      d1 = quantile(varx, na.rm = TRUE)[[2]],
                      d2 = quantile(varx, na.rm = TRUE)[[4]])
          aa1 <- paste0(round(aad[[1,2]],2), " ( ",
                        round(aad[[1,3]],2),";",
                        round(aad[[1,4]],2),")")
          aa2 <- paste0(round(aad[[2,2]],2), " ( ",
                        round(aad[[2,3]],2),";",
                        round(aad[[2,4]],2),")")
          tabp <- c(nom,ltri,aa1,aa2,pp)
        }
      }
      tabf <- rbind(tabf,tabp)
      nl <- nl + 1
      tlig <- c(tlig,nl)
    }}
  #
  titn = c(" ","n", levels(trix), "p.value")
  #
  # Export
  #
  if (export == TRUE) {
    txls <-  as.data.frame(rbind(titn, tabf) )
    extit <- paste0("tables/", tit, ".xls")
    WriteXLS(
      txls, extit
    )
  }
  #
  # Tracé
  #
  tlig <- tlig[-length(tlig)]
  nind <- which(!(1:nl %in% tlig))
  trait <- tlig[-1] - 1
  # cell_spec(1, bold = T)
  kbl(
    tabf,
    booktabs = TRUE,
    longtable = lt,
    row.names = FALSE,
    col.names = titn,
    escape = TRUE
  ) |>
    kable_styling(
      latex_options = c("HOLD_position", "scale_down", "repeat_header"),
      position = "center"
    ) |>
    footnote(general = note) |>
    row_spec(tlig, bold = T) |>
    row_spec(trait, hline_after = TRUE)
  #add_indent(1, level_of_indent = 2)
}
