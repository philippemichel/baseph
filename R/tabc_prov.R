#' Tableau comparatif (test)
#'
#' Génération d'un tableau comparatif pour une étude clinique avec tests unitaires pour les deux groupes. Choix du type de test (paramétriques ou non).
#'
#' @param dfx Tibble
#' @param trix Variable de tri
#' @param test Valeurs numériques en moyenne ± écart-type (`moy`) ou médiane (quartiles) (`med`)
#' @param titre Titre du graphique
#' @aram caption  texte explicatif sous le graphique
#'
#' @return
#' @export
#'
#' @examples tabcprov(patients, escarres, "moy")
#'
tabc_prov <- function(dfx, trix, test = "moy", titre = "", caption =""){
  if (test == "moy"){
    note <- "n (%) - moyenne ± écart-type"
  }
  else{
    note <- "n (%) - médiane (quartiles)"
  }
  dfx <- dfx |>
    drop_na({{trix}})
  trix <- dfx |>
    pull({{trix}})
  tabf <- NULL
  tlig <- 1
  nl <- 1
  #
  for (l in 1:ncol(dfx)) {
    varx <- pull(dfx[,l])
    nom <- paste0("\\textbf{",var_label(varx),"}")
    if (!setequal(trix,varx)) {
      # Numérique
      if (!is.numeric(varx)) {
        zz <- chisq.test(varx,trix, correct = FALSE)
        if (min(zz$expected) < 5) {
          zz <- chisq.test(varx,trix, correct = TRUE)
        }
        pp <- beaup(zz$p.value)
        tabp <- c(nom,"","",pp)
        bb <- table(varx,trix)
        aa <- prop.table(bb,2)*100
        for (l in 1:length(levels(varx))) {
          ll <- c(levels(varx)[l],
                  paste0(bb[l,1]," (",round(aa[l,1],1),"\\%)"),
                  paste0(bb[l,2]," (",round(aa[l,2],1),"\\%)"),"")
          tabp <- rbind(tabp,ll)
        }
        nl <- nl+length(levels(varx))
      }

      else{
        # Factoriel
        dfz <- tibble(varx,trix)
        if (test == "moy"){
          zz <- t.test(varx~trix, var.equal = TRUE)
          pp <- beaup(zz$p.value)
          aa <- dfz |>
            group_by(trix) |>
            summarise(mmx = mean(varx, na.rm = TRUE), dd = sd(varx, na.rm = TRUE))
          aa1 <- paste0(round(aa[[1,2]],2), " ± ", round(aa[[1,3]],2))
          aa2 <- paste0(round(aa[[2,2]],2), " ± ", round(aa[[2,3]],2))
          tabp <- c(nom,aa1,aa2,pp)
        }
        else{
          zz <- wilcox.test(varx~trix)
          pp <- beaup(zz$p.value)
          print("ssss")
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
          tabp <- c(nom,aa1,aa2,pp)
        }
      }
      tabf <- rbind(tabf,tabp)
      nl <- nl + 1
      tlig <- c(tlig,nl)

    }}
  #
  # Tracé
  #
  nind <- which(!(1:22 %in% tlig))
  trait <- tlig[-c(1, length(tlig))] - 1
  tit = c(" ", levels(trix), "p.value")
  cell_spec(1, bold = T)
  kbl(tabf,
      booktabs = T,
      row.names = FALSE,
      col.names = tit,
      escape = FALSE) |>
    kable_styling(latex_options = c("hold_position", "scale_down"),
                  position = "center") |>
    footnote(general = note) |>
    #  row_spec(tlig,
    #   bold = T) |>
    row_spec(trait, hline_after = TRUE) |>
    add_indent(nind, level_of_indent = 2)
}
