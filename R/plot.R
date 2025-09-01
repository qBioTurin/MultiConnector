#' @title plot
#'
#' create  plot of the basic data or the data already analyzed
#'
#' @param data CONNECTORData. (see \code{\link{ConnectorData}})
#' 
#' @export
#' 


setGeneric("plot", function(data, ...) {
  standardGeneric("plot")
})

setMethod("plot", 
          signature(data = "CONNECTORData"), 
          function(data, ...) {
            args <- list(...)
            
            if (!"ConfigChosen" %in% names(args)) {
              
              
              return(PlotTimeSeries(
                data = data,
                feature = args$feature,
                labels = args$labels
              ))
            }
            
            if (!"feature" %in% names(args)) {
              stop("The 'feature' parameter is required when 'ConfigChosen' is specified")
            }
            
            return(IndexPlotExtrapolation2(
              CONNECTORData = data,
              ConfigChosen = args$ConfigChosen,
              feature = args$feature
            ))
          })
