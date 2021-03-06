getBingMap <- function (center = c(lat = 42, lon = -76), mapArea = c(45.219, 
                                                                     -122.325, 47.61, -122.107), size = c(640, 640), destfile, 
                        zoom = 12, markers, path = "", maptype = c("Road", "Aerial ", 
                                                                   "AerialWithLabels")[1], format = c("png", "gif", "jpg", 
                                                                                                      "jpg-baseline", "png8", "png32")[1], extraURL = "", RETURNIMAGE = TRUE, 
                        GRAYSCALE = FALSE, NEWMAP = TRUE, SCALE = 1, apiKey = NULL, 
                        verbose = 0) 
{
  if (!(maptype %in% c("Road", "Aerial", "AerialWithLabels"))) 
    maptype = "Road"
  if (missing(destfile)) 
    destfile = file.path(tempdir(), "mapTile.png")
  if (is.character(center)) {
    if (verbose) 
      cat("geocoding ", center, "\n")
    center = getGeoCode(center, verbose)
  }
  if (all(c("lat", "lon") %in% names(center))) 
    center = center[c("lat", "lon")]
  stopifnot(all(size <= 640))
  fileBase <- substring(destfile, 1, nchar(destfile) - 4)
  fileExt <- substring(destfile, nchar(destfile) - 2, nchar(destfile))
  if (is.null(center)) {
    if (verbose) 
      print("Note that when center and zoom are not specified, no meta information on the map tile can be stored. This basically means that R cannot compute proper coordinates. You can still download the map tile and view it in R but overlays are not possible.")
    MetaInfo <- list(lat.center = NULL, lon.center = NULL, 
                     zoom = zoom, url = "bing", BBOX = NULL, size = size, 
                     SCALE = SCALE)
    save(MetaInfo, file = paste(destfile, "rda", sep = "."))
  }
  else if (is.numeric(center) & !missing(zoom)) {
    MyMap <- list(lat.center = center[1], lon.center = center[2], 
                  zoom = zoom, SCALE = SCALE)
    BBOX <- list(ll = XY2LatLon(MyMap, -size[1]/2 + 0.5, 
                                -size[2]/2 - 0.5), ur = XY2LatLon(MyMap, size[1]/2 + 
                                                                    0.5, size[2]/2 - 0.5))
    MetaInfo <- list(lat.center = center[1], lon.center = center[2], 
                     zoom = zoom, url = "bing", BBOX = BBOX, size = size, 
                     SCALE = SCALE)
    save(MetaInfo, file = paste(destfile, "rda", sep = "."))
  }
  if (length(size) < 2) {
    s <- paste(size, size, sep = ",")
  }
  else {
    s <- paste(size, collapse = ",")
  }
  if (!is.null(center)) 
    center <- paste(center, collapse = ",")
  bingURL = paste0("http://dev.virtualearth.net/REST/v1/Imagery/Map/", 
                   maptype, "/")
  if (is.null(center) & missing(zoom)) {
    stopifnot(!missing(markers) | path != "")
    url <- paste0(bingURL, "size=", s, "&maptype=", maptype, 
                  "&format=", format)
  }
  else if (missing(mapArea)) {
    stopifnot(!is.null(center), !missing(zoom))
    url <- paste0(bingURL, center, "/", zoom, "?mapSize=", 
                  s, "&format=", format)
  }
  else if (!missing(mapArea)) {
    latR = range(mapArea[c(1, 3)])
    lonR = range(mapArea[c(2, 4)])
    zoom <- min(MaxZoom(latR, lonR, size))
    lat.center <- mean(latR)
    lon.center <- mean(lonR)
    center = c(lat.center, lon.center)
    BBOX = list(ll = mapArea[1:2], ur = mapArea[3:4])
    names(BBOX$ll) = c("lat", "lon")
    names(BBOX$ur) = c("lat", "lon")
    MetaInfo <- list(lat.center = center[1], lon.center = center[2], 
                     zoom = zoom, url = "bing", BBOX = BBOX, size = size, 
                     SCALE = SCALE)
    save(MetaInfo, file = paste(destfile, "rda", sep = "."))
    bingURL = paste0("http://dev.virtualearth.net/REST/v1/Imagery/Map/", 
                     maptype, "?mapArea=")
    url <- paste0(bingURL, paste0(mapArea, collapse = ","), 
                  "&mapSize=", s, "&format=", format)
  }
  url <- paste(url, path, sep = "")
  url <- paste(url, extraURL, sep = "")
  if (!missing(markers)) {
    if (is.matrix(markers) | is.data.frame(markers)) {
      stopifnot(all(c("lat", "lon") %in% colnames(markers)))
      latlon = which(colnames(markers) %in% c("lat", "lon"))
      for (i in 1:nrow(markers)) {
        m1 <- paste(markers[i, c("lat", "lon")], collapse = ",")
        if (any(c("size", "color", "label") %in% colnames(markers))) {
          m2 <- paste(colnames(markers)[-latlon], markers[i, 
                                                          -latlon], collapse = "|", sep = ":")
          m <- paste(m2, m1, sep = "|")
        }
        else {
          m <- m1
        }
        if (i == 1) {
          markers.string <- paste0("&markers=", m)
        }
        else {
          markers.string <- paste(markers.string, paste0("&markers=", 
                                                         m), sep = "")
        }
      }
    }
    else if (is.character(markers)) {
      markers.string <- markers
    }
    url <- paste0(url, markers.string)
  }
  url <- paste0(url, "&key=", apiKey)
  if (verbose) 
    print(url)
  if (verbose == -1) 
    browser()
  if (verbose < 2 & NEWMAP) 
    suppressWarnings(download.file(url, destfile, mode = "wb", 
                                   quiet = TRUE))
  if (GRAYSCALE) {
    myTile <- readPNG(destfile, native = FALSE)
    myTile <- RGB2GRAY(myTile)
    writePNG(myTile, destfile)
  }
  if (RETURNIMAGE) {
    myMap <- ReadMapTile(destfile)
    return(myMap)
  }
  invisible(url)
}
