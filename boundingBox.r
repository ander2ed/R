#' Calculates a bounding box data frame around a point.
#' 
#' Utility function to calculate a bounding box around a point or set of points identified by their X/Y coordinates.
#' @param lon Longitude
#' Can be a vector or single value or data frame column.
#' @param lat Latitude
#' Can be a vector or single value or data frame column.
#' @param dist The distance (radius) of the box.
#' @param returnType The return type of the object created by calling boundingBox. See Details.
#' @details The returnType argument is used if the returned object should be of the spatial polygon class.
#' If not specified, the returned object is a data frame containing the left, bottom, top and right latitude/longitude which creates the box.
#' If returnType = 'sp' is specified, a SpatialPolygon is returned.
#' @keywords boundingBox
#' @export
#' @examples
#' none created yet




boundingBox <- function(lon, lat, dist, returnType = "sf") {
  bufferDist <- dist * 1.1
  expandLat <- bufferDist / PBmdl::coordDist(lat)[1]
  expandLon <- bufferDist / PBmdl::coordDist(lat)[2]
  
  ll.lat <- as.numeric(lat - expandLat)
  ll.lon <- as.numeric(lon - expandLon)
  ur.lat <- as.numeric(lat + expandLat)
  ur.lon <- as.numeric(lon + expandLon)
  
  if(is.null(returnType)) {
    bbox <- data.frame(cbind(ll.lon, ll.lat, ur.lon, ur.lat))
    colnames(bbox) <- c("left", "bottom", "right", "top")
  } else if(returnType == "sf") {
    bbox <- sf::st_as_sf(sf::st_geometry(sf::st_polygon(x = list(
      matrix(
        c(
          ll.lon,
          ll.lat,
          ur.lon,
          ll.lat,
          ur.lon,
          ur.lat,
          ll.lon,
          ur.lat,
          ll.lon,
          ll.lat
        ),
        byrow = T,
        ncol = 2
      )
    ))))
    sf::st_crs(bbox) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  } else {
    stop(paste("invalid return type given: ", returnType, sep = ""))
  }
  
  return(bbox)
  
}