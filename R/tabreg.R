
ll <- glm(escarre~ ., data = patients, family = "binomial")

tabregph <- function(ll, titre = ""){
tbl_regression(ll, exponentiate = TRUE) |>
  bold_labels() |>
  bold_p() |>
  modify_caption(paste0("**", titre, "**"))
}
