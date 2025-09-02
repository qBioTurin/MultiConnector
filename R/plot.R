#' @title Intelligent Plot Dispatch for MultiConnector Objects
#'
#' @description
#' Provides a unified plotting interface that automatically selects the appropriate
#' visualization based on the input object type. This function serves as the main
#' entry point for visualizing data and clustering results in the MultiConnector package,
#' dispatching to specialized plotting functions based on object class.
#'
#' @param data Either a CONNECTORData object (for raw time series visualization) or
#'   a CONNECTORDataClustered object (for cluster visualization), or a list object
#'   (for clustering quality metrics visualization).
#' @param ... Additional arguments passed to the specific plotting functions:
#'   \itemize{
#'     \item \code{feature}: Feature name for coloring (used in all plot types)
#'     \item \code{labels}: Custom labels for the plots
#'     \item \code{measurementColName}: Specific measurement to plot (for time series)
#'     \item Other plot-specific parameters
#'   }
#' 
#' @return The appropriate plot object based on input type:
#'   \itemize{
#'     \item For CONNECTORData: ggplot2 time series plots via \code{\link{PlotTimeSeries}}
#'     \item For CONNECTORDataClustered: ggplot2 cluster plots via \code{\link{PlotCluster}}
#'     \item For list objects: ggplot2 quality metrics plots via \code{\link{IndexPlotExtrapolation}}
#'   }
#'
#' @details
#' This function implements intelligent method dispatch to provide a consistent interface
#' for visualization across different stages of the analysis workflow:
#' 
#' \strong{Workflow Integration:}
#' \itemize{
#'   \item \strong{Data Exploration}: \code{plot(data)} shows raw time series
#'   \item \strong{Cluster Visualization}: \code{plot(clustered_data)} shows cluster results
#'   \item \strong{Quality Assessment}: \code{plot(cluster_metrics)} shows validation plots
#' }
#' 
#' The function automatically determines the most appropriate visualization while
#' maintaining a simple, consistent user interface throughout the analysis pipeline.
#'
#' @examples
#' \dontrun{
#' # Plot raw time series data
#' plot(my_connector_data, feature = "treatment_group")
#' 
#' # Plot clustering results
#' plot(my_clustered_data, feature = "gender")
#' 
#' # Plot clustering quality metrics
#' plot(clustering_results)
#' 
#' # With custom parameters
#' plot(my_data, feature = "age", measurementColName = "biomarker1")
#' }
#'
#' @seealso 
#' \code{\link{PlotTimeSeries}} for time series visualization,
#' \code{\link{PlotCluster}} for cluster visualization,
#' \code{\link{IndexPlotExtrapolation}} for quality metrics,
#' \code{\link{DiscriminantPlot}} for advanced cluster analysis plots
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
