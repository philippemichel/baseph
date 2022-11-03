
barconfph <- function(dfx, varnum, vartri, titre = "", stitre = "", tx= "", ty = "n",capt ="x", lab = "", angle = 0) {
  if (angle == 0) {
    hj <- 0.5
  } else {
    hj <- 1
  }
  if(capt == "x"){capt <- titre}
  angy <-  (str_length(ty) > 3)*90
  #
  dfx |>
    drop_na({{vartri}}) |>
    group_by({{vartri}}) |>
    summarise(
      bas = bashaut({{varnum}}, 1),
      haut = bashaut({{varnum}}, 2),
      moy = mean({{varnum}}, na.rm = TRUE)
    ) |>
    ggplot() +
    aes(x = {{vartri}}, y = moy, fill = {{vartri}}) +
   # geom_bar(stat = "identity") +
    geom_col() +
    geom_errorbar(
      aes(ymin = bas, ymax = haut),
      width = .6,
      position = position_dodge(0.5),
      size = 0.6
    ) +
  labs(
    title = titre,
    subtitle = stitre,
    x = tx,
    y = ty,
    caption = capt,
    label = lab
  ) +
    theme_light() +
    scale_fill_discrete_qualitative(palette = "Dynamic") +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(
        size = 12,
        angle = angy,
        vjust = .5
      ),
      axis.text.x = element_text(
        size = 12,
        angle = angle,
        hjust = hj
      ),
      axis.text.y = element_text(size = 12),
      legend.position = "none"
    )
}





bashaut <- function(xx,bh){
  zz <- t.test(xx)
  zz <- zz$conf.int[[bh]]
  return(zz)
}
