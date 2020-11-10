

rm(list = ls())


library(spatial)
library(rgdal)
library(rgeos)
library(reshape2)
library(sp)
library(ggplot2)
library(raster)


point <- c(x = -83.913985, y = 43.597761)

coords <- data.frame(
                     rbind(
                      c(point["x"] + .5, point["y"] + .5), 
                      c(point["x"] + .5, point["y"] - .5), 
                      c(point["x"] - .5, point["y"] - .5), 
                      c(point["x"] - .5, point["y"] + .5)
                      ))
coordsSP <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)), 1)))



make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0.5, 0.5))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}


grid <- make_grid(coordsSP, .05, clip = F)

grid.points <- fortify(grid, region = "id")
grid.points$dataElement <- runif(nrow(grid.points), 0, 1)


ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = dataElement), data = grid.points) +
  geom_polygon(aes(x = x, y = y, alpha = .2, col = "red"), data = coords) +
  
  geom_point(aes(point["x"], point["y"]))




