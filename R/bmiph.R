#' BMIPH
#'Coupe un vecteur BMI(numeric) (ou IMC) en facteurs avec les limites usuelles, en francais ou en anglais
#' @param bmi BMI numérique
#' @param lang Langue de sortie (french or english (fr/eng)). default : french
#'
#' @return vecteur factoriel
#' @export
#'
#' @examples imc <- c(18,25,40,32)
#' bmiph(imc, "fr")
bmiph <- function(bmi, lang = "fr") {
  if (lang == "fr") {
    limf <- c(0, 16.5, 18.5, 25, 30, 35, 40, 100)
    labf <-
      c(
        "d\u00e9nutrition",
        "maigreur",
        "normal",
        "surpoids",
        "ob\u00e9sit\u00e9 mod\u00e9r\u00e9e",
        "ob\u00e9sit\u00e9 s\u00e9v\u00e8re",
        "ob\u00e9sit\u00e9 morbide"
      )
  }
  else{
    limf <- c(0, 18.5, 25, 30, 100)
    labf = c("Underweight", "Normal weight", "Overweight", "Obese")
}
  bmif <-
    cut(
      bmi,
      include.lowest = FALSE,
      right = FALSE,
      dig.lab = 4,
      breaks = limf,
      labels = labf
    )
  return(as.factor(bmif))
  }
