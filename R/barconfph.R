
#  ------------------------------------------------------------------------
#
# Title : barconfph
#    By :
#  Date : 2022-11-03
#
#  ------------------------------------------------------------------------

barconfph <- function(dfx, varnum, varf, titre = "", stitre= "",lab = "", capt = "x", angle = 0){
  if (angle == 0) {
    hj <- 0.5
  } else {
    hj <- 1
  }
  if(capt == "x"){capt <- titre}
  #
  dfx |>
    drop_na({{varf}}) |>
    ggplot() +
    aes(x = {{varf}} , y = {{varnum}}, fill = {{varf}}) +
    geom_bar(stat = "identity") +
    labs(
      title = titre,
      subtitle = stitre,
      y = "%",
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
        angle = 0,
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
