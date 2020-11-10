#' Calculate straight line distance (miles) between two points
#' 
#' Utility function to calculate the distance between two points identified by their x/y coordinates
#' @param x.1 Longitude of the first point
#' @param y.1 Latitude of the first point
#' @param x.2 Longitude of the second point
#' @param y.2 Latitude of the second point
#' @keywords dbo.distance
#' @export
#' @examples
#' Same usage as SQL function: dbo.distance(x1, y1, x2, y2)

dbo.distance <- function(x.1, y.1, x.2, y.2) {

	DtoR <- pi / 180
	X.1 <- x.1 * DtoR
	Y.1 <- y.1 * DtoR
	X.2 <- x.2 * DtoR
	Y.2 <- y.2 * DtoR

	d.x <- X.2 - X.1
	d.y <- Y.2 - Y.1

	a <- (sin(d.y / 2)^2) + cos(Y.1) * cos(Y.2) * (sin(d.x / 2) ^ 2)
	c <- 2 * atan2(sqrt(a), sqrt(1 - a))

	return(3956 * c)

}

