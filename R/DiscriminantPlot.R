#' @title DiscrimintantPlot
#'
#'@description
#'
#'  ...
#'
#' @param CONNECTORData data created by ConnectorData()
#' @param ConfigChosen data created with configSelection()
#' @param feature feature selected for coloring
#'
#' @return ...
#'

#' @seealso ...
#'
#' @import ggplot2 plotly geometry grDevices RColorBrewer
#' @export

setGeneric("DiscriminantPlot", function(CONNECTORData,
                                               ConfigChosen,
                                               feature) {
  standardGeneric("DiscriminantPlot")
})

setMethod("DiscriminantPlot", signature(), function(CONNECTORData,
                                                           ConfigChosen,
                                                           feature) {
  data = ConfigChosen@KData$CData
  G <- ConfigChosen@TTandfDBandSil$G
  h <- ConfigChosen@h
  discrplot <- list()
  
  p <- sapply(1:length(ConfigChosen@KData$FullS), function(x)
    ncol(ConfigChosen@KData$FullS[[x]]))
  # FCM <- fclust.curvepred(data = ConfigChosen@CfitandParameters,
  #                                     q = p,
  #                                    ConfigChosen@KData = ConfigChosen@KData)
  outpred <- ConfigChosen@CfitandParameters$pred
  
  projectedcurve = outpred$alpha.hat
  projectedclustcenters = ConfigChosen@CfitandParameters$cfit$parameters$alpha
  outpred$Calpha -> stdevalpha
  outpred$class.pred -> classes
  outpred$distance -> distanze
  Feature <- CONNECTORData@annotations[paste(feature)]
  
  col <- as.character(unlist(unique(Feature)))
  colFeature <- get_robust_colors(unlist(Feature))
  
  symbols <- ConfigChosen@cluster.names
  
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
  else if (h == 3)
  {  colnames(projectedcurve) = paste0("Alpha", 1:length(projectedcurve[1, ]))
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
  } else{
    
    # Create DataFrame for samples
    DataFrameSamples <- data.frame(
      PrjCurv1 = projectedcurve[, 1],
      PrjCurv2 = projectedcurve[, 2],
      PrjCurv3 = projectedcurve[, 3],
      Feature = unlist(Feature),
      Cluster = symbols[classes]
    )
    
    # Create DataFrame for cluster centers
    DataFrameCluster <- data.frame(
      projectedclustcenters1 = projectedclustcenters[, 1],
      projectedclustcenters2 = projectedclustcenters[, 2],
      projectedclustcenters3 = projectedclustcenters[, 3],
      Cluster = symbols,
      Center = paste("c.", symbols, sep = "")
    )
    
    
    unique_clusters <- unique(DataFrameSamples$Cluster)
    plotly_symbol_map <- c(
      "circle", "square", "diamond", "cross", "x", 
      "triangle-up", "triangle-down", "triangle-left", "triangle-right", "hexagon"
    )
    
    # Assegna simboli validi da plotly ai cluster
    cluster_shapes <- setNames(
      plotly_symbol_map[1:length(unique_clusters)],
      sort(unique_clusters)
    )
    
    discrplot[["ColFeature3DWithClusterShapes"]] <- plot_ly(
      data = DataFrameSamples,
      x = ~PrjCurv1,
      y = ~PrjCurv2,
      z = ~PrjCurv3,
      color = ~Feature,
      colors = colFeature,
      symbol = ~Cluster,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 5),
      text = ~paste(
        "Feature:", Feature, 
        "<br>Cluster:", Cluster
      ),
      hoverinfo = "text"
    ) %>%
      add_markers(
        data = DataFrameCluster,
        x = ~projectedclustcenters1,
        y = ~projectedclustcenters2,
        z = ~projectedclustcenters3,
        type = "scatter3d",
        mode = "text",
        text = ~Center,
        textposition = "top center",
        showlegend = FALSE
      ) %>%
      plotly::layout(
        title = "3D Discriminant Plot",
        scene = list(
          xaxis = list(title = "Alpha 1"),
          yaxis = list(title = "Alpha 2"),
          zaxis = list(title = "Alpha 3")
        )
      )
  }
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
      
      # Primo: grafico principale con i punti
      plot_main <- plot_ly(
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
      
      # Secondo: aggiunta dei centri come secondo livello
      plot_final <- plot_main %>%
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
          inherit = FALSE  # fondamentale: non eredita dati precedenti
        ) %>%
        layout(
          title = "3D PCA Plot by Feature with Cluster Shapes",
          scene = list(
            xaxis = list(title = paste0("PC1 (", round(percentage[1]), "%)")),
            yaxis = list(title = paste0("PC2 (", round(percentage[2]), "%)")),
            zaxis = list(title = paste0("PC3 (", round(percentage[3]), "%)"))
          )
        )
      
      # Salva nel tuo oggetto
      discrplot[["ColFeature3DWithClusterShapes"]] <- plot_final
      
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