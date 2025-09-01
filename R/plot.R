#' @title plot
#'
#' create plot of the basic data or the data already clustered.
#'
#' @param data CONNECTORData or CONNECTORDataClustered object
#' @param ... Additional arguments passed to specific plotting functions
#' 
#' @export
#' @import methods

setGeneric("plot", function(data, ...) {
  standardGeneric("plot")
})

#' @rdname plot
#' @export
setMethod("plot", 
          signature(data = "CONNECTORData"), 
          function(data, ...) {
            # For CONNECTORData objects, always use PlotTimeSeries
            args <- list(...)
            return(PlotTimeSeries(
              data = data,
              feature = args$feature,
              labels = args$labels
            ))
          })

#' @rdname plot
#' @export
setMethod("plot", 
          signature(data = "CONNECTORDataClustered"), 
          function(data, ...) {
            # For CONNECTORDataClustered objects, always use ClusterPlot
            args <- list(...)
            
            return(ClusterPlot(
              CONNECTORDataClustered = data,
              feature = args$feature
            ))
          })

#' @rdname plot
#' @export
setMethod("plot", 
          signature(data = "list"), 
          function(data, ...) {
            
            # Check if this is a valid estimateCluster output
            if (!all(c("Clusterings", "KData", "Seed") %in% names(data))) {
              stop("This list does not appear to be output from estimateCluster function")
            }
            
            return(IndexPlotExtrapolation(data))
          })
