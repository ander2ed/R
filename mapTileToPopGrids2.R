


# library(jpeg)
library(sf)
library(RgoogleMaps)
library(ggplot2)
library(ggmap)
library(rjson)
library(randomForest)

rm(list = ls())

source("Z:/E_Anderson/Research/R/getBingMap.R")


## Grids;

grids <- st_read(dsn = "Z:/E_Anderson/_Projects/Shell/2018/Malaysia/LabuanGridIssue/Labaun_250m_Pop.tab",
                 layer = "Labaun_250m_Pop")




# ggplot(data = grids) +
#   geom_sf(aes(fill = log(grids$Population))) +
#   scale_fill_gradient2(low = "blue", high = "red")

## Map;

ll.x <- st_bbox(grids)[1]
ll.y <- st_bbox(grids)[2]
ur.x <- st_bbox(grids)[3]
ur.y <- st_bbox(grids)[4]

## get map metadata for bbox/center to use for transforming image raster coords to lat/long;
map <- getBingMap(mapArea = c(ll.y, ll.x, ur.y, ur.x),
                  maptype = c("Aerial"),
                  apiKey = "AtZt1KSMMSaoPcbt85RPIer5r9gkJm33cdrrkhYeIBSibj34dqXm9cDa0BgEq3Lu",
                  verbose = TRUE,
                  NEWMAP = TRUE,
                  destfile = 'C:/users/ed008an/desktop/maps/Labuan.png',
                  RETURNIMAGE = FALSE
)
map.tile <- ReadMapTile('C:/users/ed008an/desktop/maps/Labuan.png') # read the actual map

metaData_json <- fromJSON(file = paste0(map, "&mapMetadata=1"))

map.bbox <- metaData_json$resourceSets[[1]]$resources[[1]]$bbox
map.center <- as.numeric(metaData_json$resourceSets[[1]]$resources[[1]]$mapCenter$coordinates)

names(map.bbox) <- c("ll.y", "ll.x", "ur.y", "ur.x")
names(map.center) <- c("y", "x")


rm(ll.x, ll.y, ur.x, ur.y, metaData_json, map)
## Read the image
img <- png::readPNG("C:/users/ed008an/desktop/maps/Labuan.png", native = FALSE)

dim <- dim(img)

img.df <- data.frame(
  x = rep(1:dim[2], each = dim[1]),
  y = rep(dim[1]:1, dim[2]),
  R = ceiling(as.vector(img[, , 1]) * 255),
  G = ceiling(as.vector(img[, , 2]) * 255),
  B = ceiling(as.vector(img[, , 3]) * 255)
)

head(img.df)


# translate img.df x/y to lat/long;

latRange <- map.bbox[["ur.y"]] - map.bbox[["ll.y"]]
lonRange <- map.bbox[["ur.x"]] - map.bbox[["ll.x"]]

height <- dim[1]
width <- dim[2]

img.df$lat <- ((latRange / height) * img.df$y) + map.bbox[["ll.y"]]
img.df$lon <- ((lonRange / width)  * img.df$x) + map.bbox[["ll.x"]]


head(img.df)


img.sf <- st_as_sf(img.df, coords = c("lon", "lat"), crs = 4326)

st_crs(grids) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
st_crs(img.sf)<- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

img.nowater.sf <- img.sf[img.sf$R > img.sf$B & img.sf$G > img.sf$B, ]

img.sf$col <- rgb(img.sf$R,
                  img.sf$G,
                  img.sf$B,
                  maxColorValue = 255)

img.nowater.sf$col <- rgb(img.nowater.sf$R,
                          img.nowater.sf$G,
                          img.nowater.sf$B,
                          maxColorValue = 255)



# ggplot() +
#   geom_sf(data = img.nowater.sf,
#           col = img.nowater.sf$col, pch = 19, cex = .1) +
#   geom_sf(data = grids, fill = "transparent")








gridIntersection <- st_intersects(grids, img.sf, sparse = TRUE)


gridInt <- data.frame(
  Grid_i = as.integer(NULL),
  Img_j = as.integer(NULL),
  R = as.integer(NULL),
  G = as.integer(NULL),
  B = as.integer(NULL)
)


for(i in 1:length(gridIntersection)) {
  for(j in 1:length(gridIntersection[[i]])) {
    
    img_j <- gridIntersection[[i]][[j]]
    
    gridInt <- rbind(
      gridInt,
      data.frame(
        Grid_i = c(i),
        Img_j = c(img_j),
        R = img.df[img_j, c("R")],
        G = img.df[img_j, c("G")],
        B = img.df[img_j, c("B")]
        )
    )
  }
}

gridInt.dt <- data.table(gridInt)

gridInt.dt.agg <- gridInt.dt[, .(R = mean(R),
                                 G = mean(G),
                                 B = mean(B),
                                 n = .N),
                             by = .(Grid_i)]


grids$rowId <- as.integer(rownames(grids))

grids.dt <- data.table(grids)

setkey(gridInt.dt.agg, "Grid_i");setkey(grids.dt, "rowId")

grids.mdl <- gridInt.dt.agg[grids.dt[, .(rowId, Population)]]

grids.mdl[, ZeroPop := ifelse(Population == 0, TRUE, FALSE)]

## predict for 0 pop grids first
zp.mdl <- randomForest(ZeroPop ~ R + G + B, type = 'classification', data = grids.mdl)
grids.mdl$zp_pred <- predict(zp.mdl, grids.mdl)

# summary(linear.model <- lm(Population ~ R + G + B, data = grids.mdl2, weights = n))

rf.model <- randomForest(Population ~ R + G + B, data = grids.mdl)
grids.mdl$pred <- predict(rf.model, grids.mdl)

summary(grids.mdl$zp_pred)

grids.mdl[, final_pred := ifelse(zp_pred > .65, 0, pred)]

grids.mdl[, `:=`(error = (Population - final_pred) / Population,
                 absError = abs((Population - final_pred) / Population))]

summary(grids.mdl[, absError], na.rm = T)


grids.mdl.sf <- base::merge(grids, grids.mdl[, .(Grid_i, final_pred)], by.x = "rowId", by.y = "Grid_i")



dev.new()
ggplot(data = grids.mdl.sf) +
  geom_sf(aes(fill = log(grids.mdl.sf$final_pred))) +
  scale_fill_gradient2(low = "blue", high = "red") +
  ggtitle("Predicted")

dev.new()
ggplot(data = grids.mdl.sf) +
  geom_sf(aes(fill = log(grids.mdl.sf$Population))) +
  scale_fill_gradient2(low = "blue", high = "red") +
  ggtitle("Actual")

str(grids.mdl.sf)
