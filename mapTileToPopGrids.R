


library(jpeg)
library(sf)
# library(RgoogleMaps)
library(ggplot2)
library(ggmap)

rm(list = ls())

source("Z:/E_Anderson/Research/R/getBingMap.R")

# dsn <- paste0("MSSQL:server=pasql2;",
#               "database=Shell;",
#               "trusted_connection=yes")
# 
# rgdal::ogrListLayers(dsn)
# 
# spdf <- rgdal::readOGR(dsn = "Shell", layer = "MBI_MY_DataAndBoundary")

## Read WKT of Boundary 
bndy <- read.table(file = "Z:/E_Anderson/_Projects/Shell/2018/Malaysia/LabuanGridIssue/LabuanWKT.txt",
                   stringsAsFactors = FALSE,
                   header = TRUE,
                   sep = "|")

bndy$Geom <- st_as_sfc(bndy[, "Geom"], crs = 3376)

bndy.sf <- st_sf(bndy)

rm(bndy)


## Read the image
img <- readJPEG("C:/users/ed008an/desktop/screenshots/Labuan.jpg")

dim <- dim(img)

img.df <- data.frame(
  x = rep(1:dim[2], each = dim[1]),
  y = rep(dim[1]:1, dim[2]),
  R = ceiling(as.vector(img[, , 1]) * 255),
  G = ceiling(as.vector(img[, , 2]) * 255),
  B = ceiling(as.vector(img[, , 3]) * 255)
)


## Need to translate img.sf to be on same coords as grid.
#, or get map tile with specified bounding box, extract grids within same bounding box.
# or, just get map image so it has coord properties and can overlay ootb.



img.sf <- st_as_sf(img.df, coords = c("x", "y"), crs = 3376)

img.nowater.sf <- img.sf[img.sf$R > img.sf$B & img.sf$G > img.sf$B, ]


img.nowater.sf$col <- rgb(img.nowater.sf$R,
                          img.nowater.sf$G,
                          img.nowater.sf$B,
                          maxColorValue = 255)


ggplot(data = img.nowater.sf) +
  geom_sf(col = img.nowater.sf$col, pch = 19, cex = .1)


## Grids

grids <- st_read(dsn = "Z:/E_Anderson/_Projects/Shell/2018/Malaysia/LabuanGridIssue/Labaun_250m_Pop.tab",
                 layer = "Labaun_250m_Pop")




pal <- colorRampPalette(c("blue", "red"))
breaks <- cut(grids$Population, 10)
cols <- pal(10)[breaks]

ggplot(data = grids) +
  geom_sf(aes(fill = log(grids$Population))) +
  scale_fill_gradient2(low = "blue", high = "red")



# ll.X = 115.120318
# ll.y = 5.185367
# ur.x = 115.329426
# ur.y = 5.393180

ll.x <- st_bbox(grids)[1]
ll.y <- st_bbox(grids)[2]
ur.x <- st_bbox(grids)[3]
ur.y <- st_bbox(grids)[4]

map <- getBingMap(mapArea = c(ll.y, ll.x, ur.y, ur.x),
           maptype = c("Aerial"),
           zoom = 11,
           apiKey = "AtZt1KSMMSaoPcbt85RPIer5r9gkJm33cdrrkhYeIBSibj34dqXm9cDa0BgEq3Lu",
           verbose = TRUE,
           NEWMAP = TRUE,
           destfile = 'C:/users/ed008an/desktop/maps/Labuan.png'
           )



min(
  map$myTile[, 1]
)

max(
  map$myTile[, 1]
)

map2 <- map$myTile



