#' Calculates the distance (miles) between longitude lines and latitude lines at any given latitude.
#' 
#' Utility function to calculate the corrected distance between longitude and latitude lines
#' at any North/South latitude. Uses the WGS84 ellipsoid model of earth.
#' @param phi Latitude where distance between degrees of latitude and longitude are to be calculated. 
#' Can be a vector or single value or data frame column.
#' @details This function uses the WGS84 ellipsoid model of earth. The returned distance between meridians and circles of latitude
#' will therefore vary based on the north/south position. That is, the distance between meridians will approach zero as one nears the poles. Additionally,
#' circles of latitude will have different distances corresponding to a one degree change in latitude at different north/south positions as well.
#' 
#' The calculation uses four fundamental values relating to the WGS84 model of Earth. These metrics are the length of the Semi-major axis (a) and Semi-minor axis (b),
#' the flattening metric of the ellipsoid (f = (a - b) / a), and the squared eccentricity (e2 = (2 - f) * f).
#' 
#'  See the Wikipedia article on \href{https://en.wikipedia.org/wiki/World_Geodetic_System}{World_Geodetic_System} and \href{https://en.wikipedia.org/wiki/Flattening}{Flattening}
#' @keywords coordDist
#' @export
#' @examples
#' latitudes <- seq(0, 90, 5)
#' names(latitudes) <- seq(0, 90, 5)
#' coordDist(latitudes)


coordDist <- function(phi) {
  # Calculations use metrics relating to the WGS84 ellipsoid model of Earth.
  # a is Semi-major axis in meters
  # b is the Semi-minor axis in meters
  # e2 is a function of the flattening metric (f = (a - b) / a). This is the squared eccentricity
  # e2 = (2 - f) * f
  # See Wikipedia article on "World_Geodetic_System" and "Flattening"
  phi <- pi / 180 * phi
  a <- 6378137
  b <- 6356752.314245
  f <- (a - b) / a
  e2 <- (2 - f) * f
  u <- 1 - e2 * sin(phi) ^ 2
  
  return(
    data.frame(cbind(
      LatDist=(1-e2)/u, 
      LonDist=cos(phi)) * (a / sqrt(u)) 
    * (pi / 180) # convert back to meters
    / 1609.34) # convert to miles
  ) 
  
}