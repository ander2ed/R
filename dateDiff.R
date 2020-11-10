#' Calculates difference between two dates
#' 
#' Utility function to calculate the difference between two dates. Can return days, weeks, months, quarters or years.
#' @param startDate This is the first date
#' @param endDate This is the end date
#' @param units Units to be returned. One of:
#' \code{c("d", "day", "days")} for days
#' #' \code{c("w", "week", "weeks")} for weeks
#' #' \code{c("m", "month", "months")} for months
#' #' \code{c("q", "quarter", "quarters")} for quarters
#' #' \code{c("y", "year", "years")} for years
#' @keywords dateDiff
#' @seealso difftime
#' @export
#' @examples
#' start <- as.Date("2018-01-01", "%Y-%m-%d")
#' end <- format(Sys.time(), %Y-%m-%d)
#' dateDiff(start, end, "m")

dateDiff <- function(startDate, endDate, units = "days") {
  days <- as.numeric(difftime(endDate, startDate, units = "days"))
  
  if(tolower(units) %in% c("d", "day", "days")) {
    diff <- days
  }
  if(tolower(units) %in% c("w", "week", "weeks")) {
    diff <- days / 7
  }
  if(tolower(units) %in% c("m", "month", "months")) {
    diff <- (days / 365.25) * 12
  }
  if(tolower(units) %in% c("q", "quarter", "quarters")) {
    diff <- (days / 365.25) * 4
  }
  if(tolower(units) %in% c("y", "year", "years")) {
    diff <- days / 365.25
  }
  
  return(diff)

}

