

library(sf)
library(dplyr)
library(ggplot2)

rm(list = ls())

p <- st_sfc(st_point(c(1, 1))) 
circle <- st_buffer(p, dist = 1)


st_wedge <- function(x,y,r,start,width,n=20){
  theta = seq(start, start+width, length=n)
  xarc = x + r*sin(theta)
  yarc = y + r*cos(theta)
  xc = c(x, xarc, x)
  yc = c(y, yarc, y)
  st_polygon(list(cbind(xc,yc)))   
}

st_wedges <- function(x, y, r, nsegs){
  width = (2*pi)/nsegs
  starts = (1:nsegs)*width
  polys = lapply(starts, function(s){st_wedge(x,y,r,s,width)})
  mpoly = st_cast(do.call(st_sfc, polys), "MULTIPOLYGON")
  mpoly
}

w5 = st_wedges(5,1,10,5)

slices.sf <- st_wedges(st_coordinates(st_centroid(p))[, 1],
          st_coordinates(st_centroid(p))[, 2],
          1,
          6)







ggplot() +
  geom_sf(data = circle, fill = "transparent") +
  geom_sf(data = slices.sf, fill = "transparent")


