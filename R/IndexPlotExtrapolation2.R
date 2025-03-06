#' IndexPlotExtrapolation2
#'
#'@description
#'
#'  plot ??
#'
#' @param CONNECTORData Connector object created with DataImport
#' @param ConfigChoosen configuration choosen using ConfigSelection
#' @param KData Last element returned by ClusterAnalysis
#' @param feature dunno
#'
#'
#'
#' @return Boh
#'

#' @seealso DataImport, ConfigSelection, ClusterAnalysis
#'
#' @import
#' @export
#'



setGeneric("IndexPlotExtrapolation2", function(CONNECTORData,
                                               ConfigChoosen,
                                               KData,
                                               feature) {
  standardGeneric("IndexPlotExtrapolation2")
})

setMethod("IndexPlotExtrapolation2", signature(), function(CONNECTORData,
                                                           ConfigChoosen,
                                                           KData,
                                                           feature) {
  # Get number of clusters from ConfigChoosen
  G = ConfigChoosen$TTandfDBandSil$G[1]
  
  # Get predicted clusters
  resClust = ConfigChoosen$CfitandParameters$pred$class.pred
  df = KData@CData
  
  # Get number of features per measure
  q <- sapply(KData@FullS, function(x)
    dim(x)[2])
  
  # Merge data
  combined_df = merge(CONNECTORData@annotations, df)
  combined_df$cluster = resClust[combined_df$jamesID]
  
  TimeGrids = CONNECTORData@TimeGrids
  
  # Compute curve predictions
  curvepred = fclust.curvepred(
    ConfigChoosen$CfitandParameters,
    KData,
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
  
  # Plot with cluster-specific mean curves
  combined_df %>%
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
    facet_grid(measureID ~ cluster) +
    scale_color_brewer(palette = "Set1") +
    labs(y = "", x = "Time", col = feature) +
    theme_bw() +
    theme(
      legend.position = "bottom",
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
    )
})

#Faccio l'1, poi un metodo per scegliere il migliore (most freq o minima) e poi questo che ha come input il migliore
#se alpha son 2 dimensioni grafico normale, se son 3 uso plotly, se son di più faccio PCA
#implementare anche countingsamples
#prendo prob dall'output di clusteranalysis e calcolo l'entropia come in classification di connector vecchio
