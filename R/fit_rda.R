fit_rda <- function(data) {
  nutr <- as.matrix(select(data, c(n, ca, k, mg, p, al, b, cu, fe, mn, zn)))
  
  crop_rda <- rda(nutr ~ historical*current, data = data)
  return(crop_rda)
}
