
#' Returns size of folders on disk
#'
#' This function produces a data.table object containing sizes of directories and files.
#' @param path Path of directory to get size of.
#' @param detail How much detail do you want? Allowed values are \code{c(1, 2, 3, 4)}. See details below.
#' @details This function will return a \code{data.table} with size of all files at \code{path}.
#'
#' The \code{detail} argument cant take values 1, 2, 3, or 4. \code{detail == 1} will result in a single record returned with
#' the total size of all files at \code{path} and its subdirectories.
#'
#' \code{detail == 2} (default) will return the size of all files and directories immediately found in \code{path}.
#' This is equivalent to getting the size for every result returned by \code{list.files(path, recursive = F)}.
#'
#' \code{detail == 3} will return a size for all files within all subdirectories as a separate record, except for those files
#' at the very lowest level in the file tree. These are reported under the size of their parent directory. This isn't very useful.
#'
#' \code{detail == 4} will return the size for all individual files under \code{path}.
#'
#' Note that empty folders will not show up in the output as they don't contain any files to contribute to the overall size.
#' @seealso list.files file.info dir
#' @export
#' @examples
#' ## Lets look at R home;
#' path <- paste(R.home(), "/bin", sep = "")
#' ## list the files there;
#' list.files(path, recursive = T, full.names = T)
#'
#' ## Get size of those files with different detail levels;
#' size1 <- getDirSize(path, detail = 1)
#' size2 <- getDirSize(path, detail = 2)
#' size3 <- getDirSize(path, detail = 3)
#' size4 <- getDirSize(path, detail = 4)


getDirSize <- function (path = getwd(), detail = c(1, 2, 3, 4)) 
{
  if (substr(path, nchar(path), nchar(path)) == "/") {
    path <- substr(path, 1, nchar(path) - 1)
  }
  if(assertthat::are_equal(x, c(1, 2, 3, 4))) {
    detail <- 2
  } else {
    detail <- min(detail)
  }
  if (!detail %in% c(1, 2, 3, 4)) {
    stop("Detail must be one of 1, 2, 3, or 4")
  }
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  df <- list()
  i <- 1
  for (file in files) {
    size <- file.info(file)$size
    parts <- unlist(strsplit(file, "/"))
    path.parts <- unlist(strsplit(path, "/"))
    parent <- setdiff(parts, path.parts)[1]
    directory <- paste0(parts[1:(length(parts) - 1)], sep = "", 
                        collapse = "/")
    name <- paste0(parts[length(parts)], sep = "", 
                   collapse = "")
    df[[i]] <- data.table::data.table(data.frame(directory = directory, 
                                                 parent = parent, name = name, size = size))
    i <- i + 1
  }
  dt <- do.call(rbind, df)
  if (detail == 1) {
    return <- dt[, .(size = sum(size, na.rm = T))]
  }
  else if (detail == 2) {
    return <- dt[, .(size = sum(size, na.rm = T)), by = .(parent)]
  }
  else if (detail == 3) {
    return <- dt[, .(size = sum(size, na.rm = T)), by = .(parent, directory)]
  }
  else if (detail == 4) {
    return <- dt
  }
  return[, `:=`(Size, ifelse(size < 1000, paste(size, 
                                                " B", sep = ""), ifelse(size < 1000^2, paste(round(size/(1024), 
                                                                                                   2), " KB", sep = ""), ifelse(size < 1000^3, 
                                                                                                                                paste(round(size/(1024^2), 2), " MB", sep = ""), 
                                                                                                                                paste(round(size/(1024^3), 2), " GB", sep = "")))))]
  return[]
}
