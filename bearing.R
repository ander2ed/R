#' Calculates the bearing between two points.
#' 
#' Utility function to calculate the bearing between two points identified by their X/Y coordinates.
#' @param x.start The longitude of the starting point.
#' @param y.start The latitude of the starting point.
#' @param x.end The longitude coordinate of the end point.
#' @param y.end The latitude coordinate of the end point.
#' @details Because a bearing is being calculated (in degress) between two points, the order in which those 
#' points are passed to the function matter. For example, two points along the equator may have a bearing of 270
#' or 90, depending on which point is passed as the start point.
#' @keywords boundingBox
#' @export
#' @examples
#' points <- data.frame(x.start = c(-83.913975), y.start = c(43.597764),
#' x.end = c(-83.914224), y.end = c(43.623795))
#' 
#' bearing(points[1, 1], points[1, 2], points[1,3], points[1,4])
#' 
#' bearing(-80, 0, -81, 0)
#' bearing(-81, 0, -80, 0)



bearing <- function(x.start, y.start, x.end, y.end) {
  
  x.start.1 <- x.start * (pi / 180)
  y.start.1 <- y.start * (pi / 180)
  x.end.1 <- x.end * (pi / 180)
  y.end.1 <- y.end * (pi / 180)
  
  p1 <- sin(x.end.1 - x.start.1) * cos(y.end.1)
  p2 <- cos(y.start.1) * sin(y.end.1) - sin(y.start.1) * cos(y.end.1) * cos(x.end.1 - x.start.1)
  
  bearing <- (180 / pi * IEEERemainder(
    ifelse(p1 == 0 & p2 == 0, 0, atan2(p1, p2)), 2 * pi)
  )
  
  bearing <- ifelse(bearing < 0, 360 + bearing, bearing)
  
  return(bearing)
  
}


