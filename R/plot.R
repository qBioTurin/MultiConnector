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
#'     \item For CONNECTORDataClassified: ggplot2 classification plots (overview or specific subjects)  \code{\link{ClassificationCurves}}
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
#' \code{\link{IndexPlotExtrapolation}} for quality metrics
#'
#' @export
#' @import methods

setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' @rdname plot
#' @export
setMethod("plot", signature(x = "CONNECTORData", y = "missing"), function(x, y, ...) {
  # For CONNECTORData objects, always use PlotTimeSeries
  args <- list(...)
  return(PlotTimeSeries(
    data = x,
    feature = args$feature,
    labels = args$labels
  ))
})

#' @rdname plot
#' @export
setMethod("plot", signature(x = "CONNECTORDataClustered", y = "missing"), function(x, y, ...) {
  # For CONNECTORDataClustered objects, always use ClusterPlot
  args <- list(...)

  return(ClusterPlot(
    CONNECTORDataClustered = x,
    feature = args$feature
  ))
})

#' @rdname plot
#' @export
setMethod("plot", signature(x = "list", y = "missing"), function(x, y, ...) {
  # Check if this is a valid estimateCluster output
  return(IndexPlotExtrapolation(x))
})

#' @rdname plot
#' @export
setMethod("plot", signature(x = "CONNECTORDataClassified", y = "missing"), function(x, y, subjID = NULL, ...) {
  
  # Actually, let's implement a version that can show 1 or more IDs
  IDcurves <- subjID
  CData <- x@ClassificationData@curves
  CONNECTORDataClustered <- x@ClusteredData
  
  resClust <- getClusters(CONNECTORDataClustered)
  df_train <- CONNECTORDataClustered@KData$CData
  df_train$cluster <- resClust$cluster[match(df_train$subjID, resClust$subjID)]
  
  # Get mean curves
  MeanC <- getClustersCentroids(CONNECTORDataClustered)
  
  
  if (!is.null(subjID)) {
    # Plot specific subject(s)
      if (! all(subjID %in% x@ClassificationData@curves$subjID) ) {
        warning(paste("Subject ID", subjID[!(subjID %in% x@ClassificationData@curves$subjID)], "not found in classification data."))
        return(NULL)
      }
    # Filter new data
    CData <- CData %>% filter(subjID %in% IDcurves)
  }

   
    
    # Get predicted clusters for these new IDs
    new_assignments <- x@ClassMatrix_entropy %>% filter(ID %in% IDcurves)
    CData$cluster <- new_assignments$Cluster[match(CData$subjID, new_assignments$ID)]
    
    # Annotation for probs
    df_probs <- x@ClassMatrix %>%
      filter(ID %in% IDcurves) %>%
      tidyr::gather(key = "cluster", value = "Prob", -ID) %>%
      mutate(cluster = factor(cluster, levels = levels(MeanC$cluster)))
    
    # Plot
    pl <- ggplot() +
      geom_line(data = df_train, aes(x = time, y = value, group = subjID), color = "grey", alpha = 0.3) +
      geom_line(data = MeanC, aes(x = time, y = value), linewidth = 0.8, linetype = "dashed") +
      geom_line(data = CData, aes(x = time, y = value, group = subjID), color = "red", linewidth = 1) +
      facet_grid(measureID ~ cluster) +
      theme_bw() +
      labs(
        title = "Classification Result",
        subtitle = "Grey: Training curves, Dashed: Cluster means, Red: New classified curves"
      )
    
    return(pl)
})

