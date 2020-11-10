#' Describes the data passed to the function
#'
#' Utility function to describe the data file by returning count missing (or blank) and unique values of each character or factor variable,
#' or summary statistics for numeric variables.
#' @param data The data to describe
#' @param save Should the output be written to csv? If yes, the file path where you want to write the results
#' @param maxLevels For character and factor data, how many levels should be shown? Default is 5.
#' @details \code{describe.data} describes all variables passed as part of the dataframe in the \code{data} argument.
#' For numeric or integer types, \code{summary} is called on the column, returning:
#' \describe{
#' \item{\code{NA} (or blank)}{NA or blank values}
#' \item{\code{min}}{minimum value}
#' \item{\code{q1}}{first quantile (25th percentile)}
#' \item{\code{median}}{median value}
#' \item{\code{mean}}{average value}
#' \item{\code{q3}}{3rd quantile (75th percentile)}
#' \item{\code{max}}{maximum value}
#' }
#' For character or factor types, if there are fewer than 10 unique values those values are returned
#' with counts alongside the count of missing (\code{NA} or blank) values. If there are more than 10 unique values
#' only count missing is returned.
#' @keywords describe.data
#' @examples
#' describe.data(datasets::iris)
#' describe.data(datasets::iris, save = "/some/file.csv")
#' @import data.table
#' @export


describe.data <- function(data, save = "", maxLevels = 5) {


  # Initialize list to push data into
  result <- list()
  for(i in colnames(data)) {
    if(class(data[[i]]) == "factor") {
      data[[i]] <- as.character(data[[i]])
    }
    if(class(data[[i]]) == "character") {
      if(length(unique(data[[i]])) > maxLevels) {
        ## only use first 10 values;
        x <- head(broom::tidy(table(Var1 = data[[i]])), maxLevels)
        result[[i]] <- paste(sum(is.na(data[[i]]) | data[[i]] == ""),
                             paste0(ifelse(x$Var1 == "", "<blank>(", paste(x$Var1, "(", sep = "")),
                                    x$n, ")",
                                    sep = "",
                                    collapse = ","),
                             sep = ";"
        )

      } else {
        x <- broom::tidy(table(Var1 = data[[i]]))
        result[[i]] <- paste(sum(is.na(data[[i]]) | data[[i]] == ""),
                             paste0(ifelse(x$Var1 == "", "<blank>(", paste(x$Var1, "(", sep = "")),
                                    x$n, ")",
                                    sep = "",
                                    collapse = ","),
                             sep = ";"
        )

      }
    } else {
      result[[i]] <- broom::tidy(summary(data[[i]]))
      result[[i]]  <- paste(sum(is.na(data[[i]] | data[[i]] == "")),
        paste(
        paste("min:",  result[[i]]$minimum, sep = ""),
        paste("q1:",   result[[i]]$q1, sep = ""),
        paste("med:",  result[[i]]$median, sep = ""),
        paste("mean:", result[[i]]$mean, sep = ""),
        paste("q3:",   result[[i]]$q3, sep = ""),
        paste("max:",  result[[i]]$maximum, sep = ""),
        sep = ","
      ), sep = ";")
    }
  }

  result <- data.frame(t(data.frame(result)))
  colnames(result) <- c("x")
  result$variable <- row.names(result)
  data.table::setDT(result)[, c("missing", "values") := tstrsplit(x, ";")]

  if(save == "") {
    return(result[, c("variable", "missing", "values")])
  } else {
    tryCatch(
      write.table(result[, c("variable", "missing", "values")],
                  file = save,
                  append = FALSE,
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE,
                  na = "",
                  sep = ","
      ),
      error = function(e) {print("Could not write file. Check if it is open and that you specified the location correctly")},
      warning = function(w) {print("Could not write file. Check if it is open and that you specified the location correctly")}
    )
    ## Return result anyway, even if cannot write the results
    return(result[,  c("variable", "missing", "values")])

  }


}


