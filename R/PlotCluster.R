#' ClusterPlot
#'
#'@description
#'
#'  plot ??
#'
#' @param CONNECTORDataClustered Connector object created with ConnectorData
#' @param feature dunno
#'
#' @return ...
#'

#' @seealso CONNECTORDataClustered ConfigSelection ClusterAnalysis
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom dplyr select filter mutate
#' @importFrom tidyr gather
#' @importFrom rlang sym
#' @export

setGeneric("ClusterPlot", function(CONNECTORDataClustered,
                                   feature = NULL) {
  standardGeneric("ClusterPlot")
})

setMethod("ClusterPlot", signature(CONNECTORDataClustered = "CONNECTORDataClustered"),
          function(CONNECTORDataClustered,
                                               feature= NULL) {
  # Get number of clusters from CONNECTORDataClustered
  G = CONNECTORDataClustered@TTandfDBandSil$G[1]
  
  # Get predicted clusters
  resClust = CONNECTORDataClustered@CfitandParameters$pred$class.pred
  df = CONNECTORDataClustered@KData$CData
  
  # Get number of features per measure
  q <- sapply(CONNECTORDataClustered@KData$FullS, function(x)
    dim(x)[2])
  
  # Merge data
  combined_df = merge(CONNECTORDataClustered@KData$annotations, df)
  combined_df$cluster = resClust[combined_df$jamesID]
  
  TimeGrids = CONNECTORDataClustered@KData$TimeGrids
  
  # Compute curve predictions
  curvepred = fclust.curvepred(
    CONNECTORDataClustered@CfitandParameters,
    CONNECTORDataClustered@KData,
    tau = 0.95,
    tau1 = 0.975,
    q = q
  )
  
  MeanC = do.call(rbind, lapply(names(curvepred), function(x) {
    as.data.frame(curvepred[[x]]$meancurves) -> Mean
    
    # Ensure column names match number of clusters
    colnames(Mean) = as.character(1:G)
    
    Mean$measureID = x
    Mean$time = TimeGrids[[x]]
    return(Mean)
  })) %>%
    tidyr::gather(-time, -measureID, value = "value", key = "cluster")
  combined_df$cluster <- factor(combined_df$cluster)
  MeanC$cluster <- factor(MeanC$cluster)
  
  # Plot with cluster-specific mean curves
  if (is.null(feature)) {
    # When feature is NULL, use grey color for all lines
    p <- combined_df %>%
      ggplot() +
      geom_line(aes(
        x = time,
        y = value,
        group = subjID
      ), color = "grey") +
      geom_line(
        data = MeanC,
        aes(x = time, y = value),
        linewidth = .9,
        linetype = "dashed"
      ) +
      facet_grid(measureID ~ cluster, scales = "free_y") +
      labs(y = "", x = "Time") 
  } else {
    # When feature is provided, use it for coloring
    p <- combined_df %>%
      ggplot() +
      geom_line(aes(
        x = time,
        y = value,
        color = !!sym(feature),
        group = subjID
      )) +
      geom_line(
        data = MeanC,
        aes(x = time, y = value),
        linewidth = .9,
        linetype = "dashed"
      ) +
      facet_grid(measureID ~ cluster, scales = "free_y") +
      scale_color_brewer(palette = "Set1") +
      labs(y = "", x = "Time", col = feature) 
  }
  
  
  return(p +
           theme_bw() +
           theme(
             axis.title.x = element_text(
               size = 14,
               face = "bold",
               family = "Times"
             ),
             axis.text.x = element_text(size = 14, family = "Times"),
             axis.title.y = element_text(
               size = 14,
               face = "bold",
               family = "Times"
             ),
             axis.text.y = element_text(size = 14, family = "Times"),
             strip.text = element_text(
               color = "black",
               size = 14,
               face = "bold",
               family = "Times"
             ),
             plot.margin = unit(c(0, 0, 0, 0), "cm")
           ))
})
