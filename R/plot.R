#' Plot MultiConnector objects
#'
#' Provides a common plotting interface for raw data, selected clustering models,
#' clustering searches, and classified curves.
#'
#' @param x A [CONNECTORData-class], [CONNECTORDataClustered-class],
#'   [CONNECTORDataClassified-class], or the list returned by
#'   [estimateCluster()].
#' @param ... Additional arguments for the selected method. Raw-data plots accept
#'   `feature` and `labels`; clustered plots accept `feature`;
#'   classified-data plots accept `subjID`.
#' @param subjID Optional character vector of subjects to highlight when `x` is a
#'   [CONNECTORDataClassified-class] object. `NULL` plots every classified
#'   subject.
#'
#' @return A `ggplot` or patchwork object. The exact plot depends on the class of
#'   `x`. If a requested classified subject does not exist, the method warns and
#'   returns `NULL`.
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
#' # Plot selected classified subjects
#' plot(classified_data, subjID = c("S1", "S2"))
#' }
#'
#' @seealso [PlotTimeSeries()], [ClusterPlot()], [DiscriminantPlot()],
#'   [validateCluster()]
#'
#' @export
#' @import methods

setGeneric("plot", function(x, ...) standardGeneric("plot"))

#' @rdname plot
#' @export
setMethod("plot", signature(x = "CONNECTORData"), function(x,  ...) {
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
setMethod("plot", signature(x = "CONNECTORDataClustered"), function(x,  ...) {
  # For CONNECTORDataClustered objects, always use ClusterPlot
  args <- list(...)

  return(ClusterPlot(
    CONNECTORDataClustered = x,
    feature = args$feature
  ))
})

#' @rdname plot
#' @export
setMethod("plot", signature(x = "list"), function(x,  ...) {
  # Check if this is a valid estimateCluster output
  return(IndexPlotExtrapolation(x))
})

#' @rdname plot
#' @export
setMethod("plot", signature(x = "CONNECTORDataClassified"), function(x,  subjID = NULL, ...) {
  # Actually, let's implement a version that can show 1 or more subjID
  IDcurves <- subjID
  CData <- x@ClassificationData@curves
  CONNECTORDataClustered <- x@ClusteredData

  resClust <- getClusters(CONNECTORDataClustered)
  df_train <- CONNECTORDataClustered@KData$CData
  df_train$cluster <- resClust$cluster[match(df_train$subjID, resClust$subjID)]

  # Get mean curves
  MeanC <- getClustersCentroids(CONNECTORDataClustered)
  # Get predicted clusters for these new subjID
  new_assignments <- x@ClassMatrix_entropy
  df_probs <- x@ClassMatrix
  if (!is.null(subjID)) {
    # Plot specific subject(s)
    if (!all(subjID %in% x@ClassificationData@curves$subjID)) {
      warning(paste("Subject ID", subjID[!(subjID %in% x@ClassificationData@curves$subjID)], "not found in classification data."))
      return(NULL)
    }
    # Filter new data
    CData <- CData %>% filter(subjID %in% IDcurves)
    new_assignments <- new_assignments %>% filter(ID %in% IDcurves)
    df_probs <- df_probs %>% filter(subjID %in% IDcurves)
  }
  CData$cluster <- new_assignments$Cluster[match(CData$subjID, new_assignments$ID)]

  # Annotation for probs
  df_probs <- df_probs %>%
    tidyr::gather(key = "cluster", value = "Prob", -subjID) %>%
    mutate(cluster = factor(cluster, levels = levels(MeanC$cluster)))

  # Plot
  pl <- ggplot() +
    geom_line(data = df_train, aes(x = time, y = value, group = subjID), color = "grey", alpha = 0.3) +
    geom_line(data = MeanC, aes(x = time, y = value), linewidth = 0.8, linetype = "dashed") +
    geom_line(data = CData, aes(x = time, y = value, group = subjID, color = subjID), linewidth = 1) +
    facet_grid(measureID ~ cluster) +
    theme_bw() +
    labs(
      title = "Classification Result",
      subtitle = "Grey: Training curves, Dashed: Cluster means, Colored: New classified curves"
    )

  if (is.null(subjID)) pl <- pl + theme(legend.position = "none")

  return(pl)
})
