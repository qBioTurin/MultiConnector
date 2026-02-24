#' ClusterPlot
#'
#' @description
#'
#'  plot ??
#'
#' @param CONNECTORDataClustered Connector object created with ConnectorData
#' @param feature dunno
#' @param feature_type Character string specifying how to treat the feature: "auto" (default), "discrete", or "continuous"
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
                                   feature = NULL,
                                   feature_type = "auto") {
  standardGeneric("ClusterPlot")
})

setMethod(
  "ClusterPlot", signature(CONNECTORDataClustered = "CONNECTORDataClustered"),
  function(CONNECTORDataClustered,
           feature = NULL,
           feature_type = "auto") {
    # Get predicted clusters
    df <- CONNECTORDataClustered@KData$CData
    resClust <- getClusters(CONNECTORDataClustered)

    # Merge data
    combined_df <- merge(CONNECTORDataClustered@KData$annotations, df)
    combined_df <- merge(combined_df, resClust)

    meas_levels <- getMeasures(CONNECTORDataClustered)
    clust_levels <- getClusterNames(CONNECTORDataClustered)

    combined_df$measureID <- factor(combined_df$measureID, levels = meas_levels)
    combined_df$cluster <- factor(combined_df$cluster, levels = clust_levels)

    MeanC <- getClustersCentroids(CONNECTORDataClustered)
    MeanC$measureID <- factor(MeanC$measureID, levels = meas_levels)
    MeanC$cluster <- factor(MeanC$cluster, levels = clust_levels)

    TimeGrids <- CONNECTORDataClustered@KData$TimeGrids

    # Plot with cluster-specific mean curves
    if (is.null(feature)) {
      # When feature is NULL, use grey color for all lines
      p_plot <- combined_df %>%
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
      # Determine if the feature should be treated as discrete or continuous
      feature_values <- combined_df[[feature]]

      if (feature_type == "auto") {
        # Automatic detection
        is_discrete <- is.factor(feature_values) || is.character(feature_values) ||
          (is.numeric(feature_values) && length(unique(feature_values)) <= 10)
      } else if (feature_type == "discrete") {
        is_discrete <- TRUE
      } else if (feature_type == "continuous") {
        is_discrete <- FALSE
      } else {
        stop("feature_type must be 'auto', 'discrete', or 'continuous'")
      }

      # Apply appropriate color scale based on feature type
      if (is_discrete) {
        p_plot <- combined_df %>%
          ggplot() +
          geom_line(aes(
            x = time,
            y = value,
            color = as.factor(!!sym(feature)),
            group = subjID
          )) +
          geom_line(
            data = MeanC,
            aes(x = time, y = value),
            linewidth = .9,
            linetype = "dashed"
          ) +
          facet_grid(measureID ~ cluster, scales = "free_y") +
          labs(y = "", x = "Time", col = feature)

        # Check if there are too many categories for scale_color_brewer
        n_categories <- length(unique(feature_values))
        if (n_categories <= 8) {
          p_plot <- p_plot + scale_color_brewer(palette = "Set1")
        } else if (n_categories <= 12) {
          p_plot <- p_plot + scale_color_brewer(palette = "Set3")
        } else {
          # Too many categories for brewer palettes, use viridis instead
          p_plot <- p_plot + scale_color_viridis_d()
        }
      } else {
        p_plot <- combined_df %>%
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
          labs(y = "", x = "Time", col = feature) +
          scale_color_gradient(low = "blue", high = "red")
      }
    }


    return(p_plot +
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
  }
)
