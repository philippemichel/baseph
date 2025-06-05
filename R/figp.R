#' Dessin des crochets avec texte pour les p-value dans les graphiques
#'
#' @param pp Un objet ggplot2
#' @param x1 Numéro d'ordre de la première variable
#' @param x2 Numéro d'ordre de la deuxième variable
#' @param yy Niveau vertical de la barre horizontale
#' @param pval p-value à afficher
#' @param od Décalage horizontal des crochets (si plusieurs niveaux)
#' @param h Écart barre horizontale-texte
#'
#' @returns graphique
#' @export
#'
#' @examplesfp <- patients |>
#' drop_na(typemaladie, igs2) |>
#'  ggplot(aes(x = typemaladie, y = igs2)) +
#'  geom_boxplot()
#'  figp(fp, x1 =1, x2 = 2, yy =95, pval = 0.01, od = FALSE, h = 2)

figp <- function(pp, x1, x2, yy, pval, od = FALSE, h = 1.1) {
  od <- ifelse(od, 0, 0.1)
  txt <- beaup(pval, affp = 1)
  pp +
    geom_segment(x = x1 + od, xend = x1 + od, y = yy, yend = yy - 2, color = "grey10", linewidth = 0.4) +
    geom_segment(x = x2 - od, xend = x2 - od, y = yy, yend = yy - 2, color = "grey10", linewidth = 0.4) +
    geom_segment(x = x1 + od, xend = x2 - od, y = yy, yend = yy, color = "grey10", linewidth = 0.4) +
    annotate("text", x = ((x1 + x2) / 2), y = yy + h, label = txt)
}
