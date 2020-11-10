#' Calculates the IEEE 754 remainder to use in the bearing function
#' 
#' Utility function to calculate IEEE 754 remainder
#' @export




IEEERemainder <- function(x, y) {
  
  multiplier <- 1000000000000
  
  rem1 <- ((x * multiplier) %% (y * multiplier)) / multiplier
  
  
  if(rem1 == 0 & x < 0) {remainder <- -0}
  else {
    rem2 <- rem1 - (abs(y) * ifelse(x < 0, -1, ifelse(x > 0, 1, 0)))
    quot <- x / y
    if(abs(rem2) == abs(rem1)) {
      if(abs(round(quot, 0)) > abs(quot)) {
        remainder <- rem2
      } else {
        remainder <- rem1
      }
    } else if(abs(rem2) < abs(rem1)) {
      remainder <- rem2
    } else {
      remainder <- rem1
    }
  }
  
  return(remainder)
  
}
