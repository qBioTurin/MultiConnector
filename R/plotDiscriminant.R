#' @title Discriminant Analysis Plot
#'
#' @description
#' Creates discriminant analysis plots to visualize clustering results in reduced dimensional space.
#' This function generates both cluster-colored and feature-colored visualizations, automatically
#' adapting the plotting approach based on the dimensionality of the data (1D, 2D, or 3D with PCA).
#' For high-dimensional cases, PCA is applied to reduce dimensionality for visualization.
#'
#' @param CONNECTORDataClustered A CONNECTORDataClustered object created with \code{selectCluster()}.
#'   This contains the clustered data along with cluster assignments and parameters.
#' @param feature Optional character string specifying a feature from the annotations to use for 
#'   coloring points. If NULL, points will be colored uniformly. Must match a column name 
#'   in the annotations data.
#'
#' @return A list containing ggplot2 or plotly objects:
#'   \itemize{
#'     \item \code{ColCluster}: Plot colored by cluster assignments
#'     \item \code{ColFeature}: Plot colored by the specified feature (or uniform if feature=NULL)
#'   }
#'   For 3D cases, returns interactive plotly objects. For 1D and 2D cases, returns ggplot2 objects.
#'
#' @details
#' The function automatically determines the appropriate visualization based on data dimensionality:
#' \itemize{
#'   \item \strong{1D}: Scatter plot with alpha values vs standard deviation
#'   \item \strong{2D}: 2D scatter plot with convex hulls around clusters
#'   \item \strong{3D+}: PCA reduction, then either 2D plot (if first 2 PCs explain >99% variance) 
#'         or interactive 3D plot
#' }
#' 
#' Each plot includes cluster centers and appropriate legends. The feature coloring allows
#' exploration of how clinical/experimental features relate to the discovered clusters.
#'
#' @examples
#' \dontrun{
#' # Basic discriminant plot
#' plots <- DiscriminantPlot(clustered_data)
#' plots$ColCluster  # View cluster-colored plot
#' 
#' # Plot colored by a specific feature
#' plots <- DiscriminantPlot(clustered_data, feature = "treatment_group")
#' plots$ColFeature  # View feature-colored plot
#' }
#'
#' @seealso 
#' \code{\link{selectCluster}} for creating CONNECTORDataClustered objects,
#' \code{\link{plot}} for the main plotting dispatch function
#'
#' @importFrom MASS ginv
#' @importFrom plotly plot_ly layout add_trace
#' @import ggplot2 geometry grDevices RColorBrewer
#' @importFrom dplyr select filter mutate group_by arrange
#' @importFrom magrittr %>%
#' @export

setGeneric("DiscriminantPlot", function(CONNECTORDataClustered,
                                        feature = NULL) {
  standardGeneric("DiscriminantPlot")
})

setMethod("DiscriminantPlot", signature(CONNECTORDataClustered = "CONNECTORDataClustered"),
          function(CONNECTORDataClustered, feature = NULL) {
            if (!inherits(CONNECTORDataClustered, "CONNECTORDataClustered")) {
              stop("Input must be of class 'CONNECTORDataClustered'. Current class: ", class(CONNECTORDataClustered))
            }
            
            data = CONNECTORDataClustered@KData$CData
            G <- CONNECTORDataClustered@TTandfDBandSil$G
            h <- CONNECTORDataClustered@h
            discrplot <- list()
            
            p <- sapply(1:length(CONNECTORDataClustered@KData$FullS), function(x)
              ncol(CONNECTORDataClustered@KData$FullS[[x]]))
            
            outpred <- CONNECTORDataClustered@CfitandParameters$pred
            
            projectedcurve = outpred$alpha.hat
            projectedclustcenters = CONNECTORDataClustered@CfitandParameters$cfit$parameters$alpha
            outpred$Calpha -> stdevalpha
            outpred$class.pred -> classes
            outpred$distance -> distanze
            
            if(!is.null(feature)){
              Feature <- CONNECTORDataClustered@KData$annotations[paste(feature)]
              colFeature <- get_robust_colors(unlist(Feature))
              col <- as.character(unlist(unique(Feature)))
            }else {
              Feature <- as.factor(rep(1, length(CONNECTORDataClustered@KData$annotations[,1])))
              colFeature <- "black"
              col <- "1"
              feature <- "NoFeature"
            }
            
            symbols <- CONNECTORDataClustered@cluster.names
            
            if (h == 1)
            {
              DataFrame <- data.frame(
                PrjCurv = projectedcurve,
                stdaplh = stdevalpha,
                Feature = unlist(Feature),
                Cluster = symbols[classes]
              )
              
              cl.names <- 1:G - 1
              names(cl.names) <- symbols
              
              discrplot[["ColCluster"]] <- ggplot(data = DataFrame) +
                geom_point(aes(
                  x = PrjCurv,
                  y = stdaplh,
                  colour = Cluster,
                  pch = Cluster
                ),
                size = 4) +
                geom_vline(xintercept = projectedclustcenters) +
                scale_shape_manual("Cluster", values = cl.names) +
                labs(title = "Discriminant plot") +
                xlab("Alpha") + ylab('Standard Deviation') +
                theme(
                  plot.title = element_text(hjust = 0.5),
                  axis.line = element_line(colour = "black"),
                  panel.background = element_blank()
                )
              
              
              discrplot[["ColFeature"]] <- ggplot(data = DataFrame) +
                geom_point(aes(
                  x = PrjCurv,
                  y = stdaplh,
                  colour = as.factor(Feature),
                  shape = Cluster
                ),
                size = 4) +
                geom_vline(xintercept = projectedclustcenters) +
                xlab("Alpha") + ylab('Standard Deviation') +
                labs(title = "Discriminant plot") +
                scale_colour_manual(
                  values = colFeature,
                  limits = col,
                  breaks = col,
                  name = feature
                ) +
                scale_shape_manual("Cluster", values = cl.names) +
                theme(
                  plot.title = element_text(hjust = 0.5),
                  axis.line = element_line(colour = "black"),
                  panel.background = element_blank()
                )
            }
            else if (h == 2) {
              DataFrameSamples <- data.frame(
                PrjCurv1 = projectedcurve[, 1],
                PrjCurv2 = projectedcurve[, 2],
                Feature = unlist(Feature),
                Cluster = symbols[classes]
              )
              DataFrameCluster <- data.frame(
                projectedclustcenters1 = projectedclustcenters[, 1],
                projectedclustcenters2 = projectedclustcenters[, 2],
                Cluster = symbols,
                Center = paste("c.", symbols, sep = "")
              )
              
              cl.names <- 1:G - 1
              names(cl.names) <- symbols
              hull_data <- DataFrameSamples %>%
                group_by(Cluster) %>%
                slice(chull(PrjCurv1, PrjCurv2))
              
              
              discrplot[["ColCluster"]] <- ggplot() +
                geom_polygon(data = hull_data,
                             aes(x = PrjCurv1, y = PrjCurv2, fill = Cluster),
                             alpha = 0.2, colour="black") +
                geom_point(
                  data = DataFrameSamples,
                  aes(
                    x = PrjCurv1,
                    y = PrjCurv2,
                    colour = Cluster,
                    pch = Cluster
                  ),
                  size = 4
                ) +
                geom_text(
                  data = DataFrameCluster,
                  aes(
                    x = projectedclustcenters1,
                    y = projectedclustcenters2,
                    label = paste("c", symbols, sep = ""),
                    colour = Cluster
                  ),
                  size = 5,
                  show.legend = F
                ) +
                scale_shape_manual(
                  "Cluster",
                  values = cl.names,
                  labels = paste(names(cl.names), " (c", symbols, ")", sep = ""),
                  breaks = names(cl.names)
                ) +
                scale_colour_discrete(
                  "Cluster",
                  limits = names(cl.names),
                  breaks = names(cl.names),
                  labels = paste(names(cl.names), " (c", symbols, ")", sep = "")
                ) +
                xlab("Alpha 1") + ylab('Alpha 2') +
                labs(title = "Discriminant plot") +
                theme(
                  plot.title = element_text(hjust = 0.5),
                  axis.line = element_line(colour = "black"),
                  panel.background = element_blank()
                )
              
              
              
              discrplot[["ColFeature"]] <- ggplot() +
                geom_polygon(data = hull_data,
                             aes(x = PrjCurv1, y = PrjCurv2, fill = Cluster),
                             alpha = 0.2, colour="black") +
                geom_point(
                  data = DataFrameSamples,
                  aes(
                    x = PrjCurv1,
                    y = PrjCurv2,
                    colour = as.factor(Feature),
                    pch = Cluster
                  ),
                  size = 4
                ) +
                geom_text(
                  data = DataFrameCluster,
                  aes(x = projectedclustcenters1, y = projectedclustcenters2, label = Center),
                  size = 5,
                  show.legend = F
                ) +
                xlab("Alpha 1") + ylab('Alpha 2') +
                labs(title = "Discriminant plot") +
                scale_colour_manual(
                  values = colFeature,
                  limits = col,
                  breaks = col,
                  name = feature
                ) +
                scale_shape_manual(
                  "Cluster",
                  values = cl.names,
                  labels = paste(names(cl.names), ", c.", symbols, sep = ""),
                  breaks = names(cl.names)
                ) +
                theme(
                  plot.title = element_text(hjust = 0.5),
                  axis.line = element_line(colour = "black"),
                  panel.background = element_blank()
                )
              
            }
            else{
              colnames(projectedcurve) = paste0("Alpha", 1:length(projectedcurve[1, ]))
              prcomp(projectedcurve) -> pca
              # Principal components variances
              eigs <- pca$sdev ^ 2
              # Percentage of variances explained by each component
              percentage <- eigs / sum(eigs) * 100
              print("Percentage of variance explained:")
              print(percentage)
              print(paste("Sum of first two components:", round(percentage[1] + percentage[2], 2)))
              
              projectedclustcentersPCA =
                (projectedclustcenters -
                   matrix(
                     pca$center,
                     ncol = h,
                     nrow = length(projectedclustcenters[, 1]),
                     byrow = T
                   )) %*%
                pca$rotation
              
              # Controllo se le prime due componenti spiegano oltre il 99% della varianza
              if (round(percentage[1] + percentage[2], 2) > 99) {
                # Plot 2D se le prime due componenti spiegano sostanzialmente il 100%
                DataFrameSamples <- data.frame(
                  PrjCurv1 = pca$x[, "PC1"],
                  PrjCurv2 = pca$x[, "PC2"],
                  Feature = unlist(Feature),
                  Cluster = symbols[classes]
                )
                
                DataFrameCluster <- data.frame(
                  projectedclustcenters1 = projectedclustcentersPCA[, "PC1"],
                  projectedclustcenters2 = projectedclustcentersPCA[, "PC2"],
                  Cluster = symbols,
                  Center = paste("c.", symbols, sep = "")
                )
                
                # Definisci un mapping delle forme per i cluster
                unique_clusters <- unique(DataFrameSamples$Cluster)
                cluster_shapes <- setNames(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)[1:length(unique_clusters)], unique_clusters)
                cl.names <- 1:G - 1
                names(cl.names) <- symbols
                hull_data <- DataFrameSamples %>%
                  group_by(Cluster) %>%
                  slice(chull(PrjCurv1, PrjCurv2))
                
                
                discrplot[["ColCluster"]] <- ggplot() +
                  geom_polygon(data = hull_data,
                               aes(x = PrjCurv1, y = PrjCurv2, fill = Cluster),
                               alpha = 0.2, colour="black") +
                  geom_point(
                    data = DataFrameSamples,
                    aes(
                      x = PrjCurv1,
                      y = PrjCurv2,
                      colour = Cluster,
                      pch = Cluster
                    ),
                    size = 4
                  ) +
                  geom_text(
                    data = DataFrameCluster,
                    aes(
                      x = projectedclustcenters1,
                      y = projectedclustcenters2,
                      label = paste("c", symbols, sep = ""),
                      colour = Cluster
                    ),
                    size = 5,
                    show.legend = F
                  ) +
                  scale_shape_manual(
                    "Cluster",
                    values = cl.names,
                    labels = paste(names(cl.names), " (c", symbols, ")", sep = ""),
                    breaks = names(cl.names)
                  ) +
                  scale_colour_discrete(
                    "Cluster",
                    limits = names(cl.names),
                    breaks = names(cl.names),
                    labels = paste(names(cl.names), " (c", symbols, ")", sep = "")
                  ) +
                  xlab("Alpha 1") + ylab('Alpha 2') +
                  labs(title = "Discriminant plot") +
                  theme(
                    plot.title = element_text(hjust = 0.5),
                    axis.line = element_line(colour = "black"),
                    panel.background = element_blank()
                  )
                
                
                discrplot[["ColFeature"]] <- ggplot() +
                  geom_polygon(data = hull_data,
                               aes(x = PrjCurv1, y = PrjCurv2, fill = Cluster),
                               alpha = 0.2, colour="black") +
                  geom_point(
                    data = DataFrameSamples,
                    aes(
                      x = PrjCurv1,
                      y = PrjCurv2,
                      colour = as.factor(Feature),
                      shape = Cluster
                    ),
                    size = 4
                  ) +
                  geom_text(
                    data = DataFrameCluster,
                    aes(
                      x = projectedclustcenters1,
                      y = projectedclustcenters2,
                      label = Center,
                      colour = Cluster
                    ),
                    size = 5,
                    show.legend = FALSE
                  ) +
                  scale_colour_manual(
                    values = colFeature,
                    limits = col,
                    breaks = col,
                    name = feature
                  ) +
                  scale_shape_manual(
                    "Cluster",
                    values = cluster_shapes,
                    labels = paste(names(cluster_shapes), ", c.", symbols, sep = ""),
                    breaks = names(cluster_shapes)
                  ) +
                  xlab(paste0("PC1 (", round(percentage[1]), "%)")) +
                  ylab(paste0("PC2 (", round(percentage[2]), "%)")) +
                  labs(title = "2D PCA Plot by Feature with Cluster Shapes") +
                  theme(
                    plot.title = element_text(hjust = 0.5),
                    axis.line = element_line(colour = "black"),
                    panel.background = element_blank()
                  )
              } else {
                # Mantieni il plot 3D come prima se le prime due componenti non spiegano sostanzialmente il 100%
                DataFrameSamples <- data.frame(
                  PrjCurv1 = pca$x[, "PC1"],
                  PrjCurv2 = pca$x[, "PC2"],
                  PrjCurv3 = pca$x[, "PC3"],
                  Feature = unlist(Feature),
                  Cluster = symbols[classes]
                )
                
                DataFrameCluster <- data.frame(
                  projectedclustcenters1 = projectedclustcentersPCA[, "PC1"],
                  projectedclustcenters2 = projectedclustcentersPCA[, "PC2"],
                  projectedclustcenters3 = projectedclustcentersPCA[, "PC3"],
                  Cluster = symbols,
                  Center = paste("c.", symbols, sep = "")
                )
                
                # Definisci un mapping delle forme per i cluster
                unique_clusters <- unique(DataFrameSamples$Cluster)
                cluster_shapes <- setNames(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)[1:length(unique_clusters)], unique_clusters)
                
                # Plot colored by cluster
                plot_cluster <- plot_ly(
                  data = DataFrameSamples,
                  x = ~ PrjCurv1,
                  y = ~ PrjCurv2,
                  z = ~ PrjCurv3,
                  color = ~ Cluster,
                  symbol = ~ Cluster,
                  symbols = cluster_shapes,
                  type = "scatter3d",
                  mode = "markers",
                  marker = list(size = 5),
                  text = ~ paste(
                    "Cluster:", Cluster,
                    "<br>PC1:", round(PrjCurv1, 2),
                    "<br>PC2:", round(PrjCurv2, 2),
                    "<br>PC3:", round(PrjCurv3, 2)
                  ),
                  hoverinfo = "text"
                )
                
                plot_cluster_final <- plot_cluster %>%
                  add_trace(
                    data = DataFrameCluster,
                    x = ~ projectedclustcenters1,
                    y = ~ projectedclustcenters2,
                    z = ~ projectedclustcenters3,
                    type = "scatter3d",
                    mode = "text",
                    text = ~ Center,
                    textposition = "top center",
                    showlegend = FALSE,
                    inherit = FALSE
                  ) %>%
                  layout(
                    title = "3D PCA Plot by Cluster",
                    scene = list(
                      xaxis = list(title = paste0("PC1 (", round(percentage[1]), "%)")),
                      yaxis = list(title = paste0("PC2 (", round(percentage[2]), "%)")),
                      zaxis = list(title = paste0("PC3 (", round(percentage[3]), "%)"))
                    )
                  )
                
                discrplot[["ColCluster"]] <- plot_cluster_final
                
                # Plot colored by feature
                plot_feature <- plot_ly(
                  data = DataFrameSamples,
                  x = ~ PrjCurv1,
                  y = ~ PrjCurv2,
                  z = ~ PrjCurv3,
                  color = ~ Feature,
                  colors = colFeature,
                  symbol = ~ Cluster,
                  symbols = cluster_shapes,
                  type = "scatter3d",
                  mode = "markers",
                  marker = list(size = 5),
                  text = ~ paste(
                    "Feature:", Feature,
                    "<br>Cluster:", Cluster,
                    "<br>PC1:", round(PrjCurv1, 2),
                    "<br>PC2:", round(PrjCurv2, 2),
                    "<br>PC3:", round(PrjCurv3, 2)
                  ),
                  hoverinfo = "text"
                )
                
                plot_feature_final <- plot_feature %>%
                  add_trace(
                    data = DataFrameCluster,
                    x = ~ projectedclustcenters1,
                    y = ~ projectedclustcenters2,
                    z = ~ projectedclustcenters3,
                    type = "scatter3d",
                    mode = "text",
                    text = ~ Center,
                    textposition = "top center",
                    showlegend = FALSE,
                    inherit = FALSE
                  ) %>%
                  layout(
                    title = "3D PCA Plot by Feature with Cluster Shapes",
                    scene = list(
                      xaxis = list(title = paste0("PC1 (", round(percentage[1]), "%)")),
                      yaxis = list(title = paste0("PC2 (", round(percentage[2]), "%)")),
                      zaxis = list(title = paste0("PC3 (", round(percentage[3]), "%)"))
                    )
                  )
                
                discrplot[["ColFeature"]] <- plot_feature_final
                
              }
            }
            
            return(discrplot)
          })


get_robust_colors <- function(Feature) {
  n_categories <- length(unique(Feature))
  if (n_categories <= 15) {
    # Palette predefinite con alta distintività
    palettes <- list(brewer.pal(9, "Set1"),
                     brewer.pal(8, "Set2"),
                     brewer.pal(12, "Paired"))
    
    # Seleziona la palette più adatta
    selected_palette <- palettes[which.min(abs(sapply(palettes, length) - n_categories))][[1]]
    return(selected_palette[1:n_categories])
  } else {
    # Per molte categorie, usa un metodo di generazione di colori distintivi
    color_wheel_hues <- seq(15, 360, length.out = n_categories)
    colors <- hcl.colors(n_categories,
                         palette = "Zissou 1",
                         alpha = 1,
                         rev = FALSE)
    return(colors)
  }
}