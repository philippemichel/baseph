#' Espaces superflus
#' 
#' Élimine les espaces en début et fin d'une chaîne de caractères.
#'
#' @param x chaine de caractères
#'
#' @returns chaine de caractères sans espaces superflus
#' @export
#'
#' @examples  nesp("  Bonjour  ")
#' 
nesp <- function(x){
  zz <-   gsub("^\\s+|\\s", "", x)
  return(zz)
}
