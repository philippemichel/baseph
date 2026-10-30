#' Multiple responses
#' For an item with multiple answers(typically one question, several checkboxes). Summarizes the responses in a vector and a frequency table (gtsummary)
#'
#' @param df : data_frame or tibble
#' @param aa ; names of the column
#' @param deli :delimitator (";" by default)
#'
#' @returns dfx : a tibble with one colomn; tzz : gtsummary tabble
#' @export
#'
#' @examples df <- tibble(rep = c("a;b", "b;c", "a"))
#' multirep(df = df, rep = rep, deli = ";")
#'
multirep <- function(df, rep, deli = ";") {
  zz <- df |>
    dplyr::select({{ rep }})
  aa <- as.vector(zz[, 1])
  zz <- str_split(aa[[1]], deli, simplify = TRUE)
  zz <- tibble(nn = as.vector(zz)) |>
    dplyr::filter(nn != "")


  tzz <- tbl_summary(zz,
    sort = all_categorical(FALSE) ~ "frequency"
  )
  return(list(dfx = zz, tzz = tzz))
}
