#' Used to fit curves
#' 
#' Utility function to fit linaer, quadratic and cubic polynomial models and plot fits against data
#' @param x RHS (independent) variable 
#' @param y LHS (dependent) variable
#' @param data Data frame containing X, Y and weight (if used)
#' @param weight Variable used as weight in regression
#' @param col Variable used to color the points. Coerced to factor. User can specify colors in a variable in the data frame being plotted
#' @keywords fitCurve
#' @export
#' @examples
#' None created yet.

fitCurve <- function(x, y, data, weight=NULL, col=NULL, baseline = T) {
  
  # Create list to hold output. return from function.
  return.vals <- list()
  
  ## unweighted regression / plot
  if(missing(weight)) {
    for(i in 1:3) { 
      # build regression models and store estimates
      model <- lm(data[[y]] ~ poly(data[[x]], i, raw = TRUE))
      return.vals[[i]] <- summary(model)
    }
    
    # plot regression fits
      return.vals[[4]] <- ggplot2::ggplot(data = data,
                                          aes(x = data[[x]], y = data[[y]])) +
        ggplot2::stat_smooth(method = lm, se=FALSE,
                           formula = y ~ poly(x, 1, raw = TRUE), col = "red") +
        ggplot2::stat_smooth(method = lm, se=FALSE,
                             formula = y ~ poly(x, 2, raw = TRUE), col = "blue") +
        ggplot2::stat_smooth(method = lm, se=FALSE,
                             formula = y ~ poly(x, 3, raw = TRUE), col = "green") +
        ggplot2::xlab(x) +
        ggplot2::ylab(y)
        
      if(!is.null(col)) {
        legend.name <- deparse(substitute(col))
        if(regexpr('$', legend.name, fixed = T)[1]) {
          legend.name <- substr(legend.name,
                                regexpr('$', legend.name, fixed = T)[1] + 1,
                                nchar(legend.name))
        }
        return.vals[[4]] <- return.vals[[4]] + ggplot2::geom_point(aes(x = data[[x]], y = data[[y]], col = as.factor(col))) +
          ggplot2::scale_color_manual(values = 1:length(unique(col)), 
                                      name = legend.name
                                      
          )
      } else {
        return.vals[[4]] <- return.vals[[4]] + ggplot2::geom_point()
      }
      
      if(baseline) {
        return.vals[[4]] <- return.vals[[4]] + ggplot2::geom_abline(slope = 0, intercept = 1)
                                      
      }
      
  ## weighted regression / plot
  } else { 
    for(i in 1:3) {
      # build regression models and store estimates
     model <- lm(data[[y]] ~ poly(data[[x]], i, raw = TRUE), weight = data[[weight]])
     return.vals[[i]] <- summary(model)
    }
    # plot regression fits
    return.vals[[4]] <- ggplot2::ggplot(aes(x = data[[x]], y = data[[y]], weight = data[[weight]]), data = data) +
      ggplot2::geom_point() +
      ggplot2::stat_smooth(method = lm, se=FALSE,
                  formula = y ~ poly(x, 1, raw = TRUE), col = "red") +
      ggplot2::stat_smooth(method = lm, se=FALSE,
                  formula = y ~ poly(x, 2, raw = TRUE), col = "blue") +
      ggplot2::stat_smooth(method = lm, se=FALSE,
                  formula = y ~ poly(x, 3, raw = TRUE), col = "green") +
      ggplot2::xlab(x) +
      ggplot2::ylab(y)
  
  }
  
  # Name the elements of the returned results list
  names(return.vals) <- c("Linaer", "Quadratic", "Cubic", "plot")
  return.vals
  
}