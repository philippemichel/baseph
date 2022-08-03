zz <- function(df,var, varp, valx = "Drug A") {

 aa <- df |>
    transmute(xx = as.factor({{var}}), yy = as.factor({{varp}}))
 ll <- aa %>%
 summarise(which(valx == levels(xx)))
 ll <- ll[[1]]
 zz <- aa %>%
   summarise(table(xx,yy))
 tzz <- colSums(zz)
 szz <- zz[ll, ][[1]]
 aa <- binom::binom.confint(szz, tzz, method = "exact")
 return(aa)
           }
