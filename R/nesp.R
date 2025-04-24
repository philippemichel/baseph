#' Espaces superflus
#' 
#' Élimine les espaces en début et fin d'une chaîne de caractères.
#'
#' @param x chaine de caractères
#'
#' @returns chaine de caractères sans espaces superflus
#' @export
#'
#' @examples  nesp("  Bonjour à   vous ")
#' 
nesp <- function(x){
  x <- gsub("\\s{2,}"," ",x)
  x <-   gsub("^\\s+|\\s$", "", x)
  return(x)
}
