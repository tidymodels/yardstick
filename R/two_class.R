#' Other Metrics for 2x2 Tables
#' 
#' 
#' 
#' 
#' 

mcc <- function(data) {
  positive <- pos_val(data)
  negative <- neg_val(data)
  
  tp <- data[positive, positive]
  tn <- data[negative, negative]
  fp <- data[positive, negative]
  fn <- data[negative, positive]
  d1 <- tp + fp
  d2 <- tp + fn
  d3 <- tn + fp
  d4 <- tn + fn
  if (d1 == 0 | d2 == 0 | d3 == 0 | d4 == 0)
    return(NA)
  ((tp * tn) - (fp * fn)) / sqrt(d1 * d2 * d3 * d4)
}
