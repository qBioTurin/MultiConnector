#' @title splinePlot
#'
#' @description plot splines
#'
#' @param CONNECTORDataClustered data created with selectCluster()
#'
#' @return a plot of splines for each data
#'

#' @importFrom dplyr filter
#' @import ggplot2 patchwork rlist
#' @export
#'
setGeneric("splinePlot", function(CONNECTORDataClustered) standardGeneric("splinePlot"))

setMethod("splinePlot", signature(CONNECTORDataClustered = "CONNECTORDataClustered"), function(CONNECTORDataClustered) {
  if (!inherits(CONNECTORDataClustered, "CONNECTORDataClustered")) {
    stop("Input must be of class 'CONNECTORDataClustered'. Current class: ", class(CONNECTORDataClustered))
  }

  data <- CONNECTORDataClustered@KData$CData
  cluster <- CONNECTORDataClustered@CfitandParameters$pred$class.pred

  params = getParameters(CONNECTORDataClustered)
  q = params$p

  objects <- fclust.curvepred(
    data = CONNECTORDataClustered@CfitandParameters,
    q = q,
    KData = CONNECTORDataClustered@KData
  )

  M <- sort(as.character(unique(data$measureID)))
  grid <- CONNECTORDataClustered@KData$TimeGrids

  ####
  plot_list <- list()

  unique_james_ids <- unique(data$jamesID)

  for (james_id in unique_james_ids) {
    james_plots <- list()

    for (m in M) {
      object <- objects[[m]]

      filtered_data <- data %>% filter(measureID == m, jamesID == james_id)

      if (nrow(filtered_data) == 0) next

      i <- which(unique_james_ids == james_id)
      cl <- cluster[i]

      upci <- object$upci[i, ]
      uppi <- object$uppi[i, ]
      lowci <- object$lowci[i, ]
      lowpi <- object$lowpi[i, ]
      gpred <- object$gpred[i, ]
      meancurves <- (object$meancurves)[, cl]

      data.ggplot <- data.frame(
        grid = grid[[m]],
        upci = upci,
        uppi = uppi,
        lowci = lowci,
        lowpi = lowpi,
        gpred = gpred,
        meancurves = meancurves
      )

      data.real <- data.frame(
        time = filtered_data$time,
        vol = filtered_data$value
      )

      gpl <- ggplot() +
        geom_ribbon(data = data.ggplot, aes(x = grid, ymin = lowci, ymax = upci), alpha = 0.1) +
        geom_line(data = data.ggplot, aes(x = grid, y = gpred, linetype = "Spline estimated", col = "Spline estimated")) +
        geom_line(data = data.ggplot, aes(x = grid, y = meancurves, linetype = "Cluster mean", col = "Cluster mean")) +
        geom_line(data = data.real, aes(x = time, y = vol, col = "Real points", linetype = "Real points")) +
        geom_point(data = data.real, aes(x = time, y = vol), col = "blue") +
        labs(
          title = paste("Sample", james_id, "-", m),
          x = "Time",
          y = "Growth value"
        ) +
        scale_colour_manual(
          values = c("black", "red", "blue"),
          limits = c("Cluster mean", "Spline estimated", "Real points"),
          breaks = c("Cluster mean", "Spline estimated", "Real points"),
          name = " "
        ) +
        guides(
          linetype = "none",
          colour = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))
        ) +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          legend.key.width = unit(1, "cm")
        )

      james_plots[[m]] <- gpl
    }

    # Combine plots for this jamesID using patchwork
    combined_plot <- wrap_plots(james_plots, ncol = 2) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")

    plot_list[[as.character(james_id)]] <- combined_plot
  }

  return(plot_list)
})
