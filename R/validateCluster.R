#' @title Validate Cluster Quality
#'
#' @description 
#' Validates the quality of clustering results by calculating and visualizing key clustering metrics.
#' This function computes silhouette scores and entropy measures to assess cluster coherence,
#' separation, and uncertainty. The results are presented both as numerical summaries and
#' comprehensive visualizations to help users evaluate clustering quality.
#'
#' @param CONNECTORDataClustered A CONNECTORDataClustered object created with \code{selectCluster()}.
#'   This should contain the final selected clustering configuration with cluster assignments
#'   and membership probabilities.
#'
#' @return A list containing validation results:
#'   \itemize{
#'     \item \code{plot}: A combined ggplot2 visualization showing silhouette scores and entropy
#'       distributions across clusters
#'     \item \code{metrics}: A data frame with detailed clustering metrics including:
#'       \itemize{
#'         \item Silhouette scores (average and per-cluster)
#'         \item Entropy measures (indicating cluster assignment uncertainty)
#'         \item Cluster sizes and proportions
#'       }
#'   }
#'
#' @details
#' This function provides comprehensive cluster validation through multiple metrics:
#' 
#' \strong{Silhouette Analysis:}
#' \itemize{
#'   \item Measures how well each curve fits within its assigned cluster
#'   \item Values range from -1 to 1 (higher is better)
#'   \item Identifies potential misclassified curves
#' }
#' 
#' \strong{Entropy Analysis:}
#' \itemize{
#'   \item Quantifies uncertainty in cluster assignments
#'   \item Lower entropy indicates more confident assignments
#'   \item Helps identify curves with ambiguous cluster membership
#' }
#' 
#' The visualization includes boxplots, density plots, and summary statistics to provide
#' a comprehensive view of clustering quality.
#'
#' @examples
#' \dontrun{
#' # Validate clustering results
#' validation <- validateCluster(selected_clusters)
#' 
#' # View the validation plot
#' print(validation$plot)
#' 
#' # Examine numerical metrics
#' print(validation$metrics)
#' 
#' # Check average silhouette score
#' mean_silhouette <- mean(validation$metrics$silhouette)
#' }
#'
#' @seealso 
#' \code{\link{selectCluster}} for selecting cluster configurations,
#' \code{\link{estimateCluster}} for the initial clustering analysis,
#' \code{\link{DiscriminantPlot}} for visualizing cluster separation
#'
#' @import ggplot2 tibble dplyr tidyr
#' @export
setGeneric("validateCluster", function(CONNECTORDataClustered) {
  standardGeneric("validateCluster")
})

setMethod("validateCluster", signature(CONNECTORDataClustered = "CONNECTORDataClustered"), function(CONNECTORDataClustered) {
  
  # Check input class at the beginning
  if (!inherits(CONNECTORDataClustered, "CONNECTORDataClustered")) {
    stop("Input must be of class 'CONNECTORDataClustered'. Current class: ", class(CONNECTORDataClustered))
  }
  
  # Use the correct parameter name throughout the function
  probs = CONNECTORDataClustered@CfitandParameters$pred$probs
  colnames(probs) = CONNECTORDataClustered@cluster.names
  
  MatrixClass = as.data.frame(probs)
  MatrixClass$ClusterType <- colnames(MatrixClass)[apply(MatrixClass, MARGIN = 1, FUN = which.max)]
  MatrixClass <- MatrixClass %>%
    mutate(MajorClusterValue = do.call(pmax, c(dplyr::select(., -ClusterType))))
  
  df1 = 
    MatrixClass %>%
    mutate(ID = 1:length(ClusterType)) %>%
    tidyr::gather(-ID, -MajorClusterValue, -ClusterType, key = "Cluster", value = "Prob") %>%
    group_by(ID) %>%
    mutate(Entropy = -sum(ifelse(Prob == 0, 0, Prob * log2(Prob)))) %>%
    ungroup() %>%
    tidyr::spread(key = "Cluster", value = "Prob")
  
  q <- sapply(1:length(CONNECTORDataClustered@KData$FullS), function(x)
    ncol(CONNECTORDataClustered@KData$FullS[[x]]))
  
  cluster_assignments <- CONNECTORDataClustered@CfitandParameters$pred$class.pred
  curvepred <- fclust.curvepred(data = CONNECTORDataClustered@CfitandParameters,
                                q = q,
                                KData = CONNECTORDataClustered@KData)
  
  all_distances = DistAllSubjCurves2Curves.sapl(CONNECTORDataClustered@KData, curvepred)
  
  silCoeff = do.call(rbind, 
                     lapply(1:max(CONNECTORDataClustered@KData$CData$jamesID), function(jID) {
                       current_cluster = cluster_assignments[jID]
                       in_cluster_indices <- which(cluster_assignments == current_cluster)
                       out_cluster_indices <- cluster_assignments[cluster_assignments != current_cluster]
                       
                       denomin = table(cluster_assignments)[current_cluster] - 1
                       denomin = ifelse(denomin == 0, 1, denomin)
                       ai = sum(all_distances[jID, in_cluster_indices] + all_distances[in_cluster_indices, jID]) * 1 / denomin
                       
                       bi_all = sapply(unique(out_cluster_indices), function(cl) {
                         cluster_indices <- which(cluster_assignments == cl)
                         denomin = table(cluster_assignments)[cl]
                         sum(all_distances[jID, cluster_indices] + all_distances[cluster_indices, jID]) * 1 / denomin
                       })
                       bi = min(bi_all)
                       si = if (table(cluster_assignments)[current_cluster] > 1) {
                         (bi - ai) / max(ai, bi)
                       } else 0
                       
                       return(data.frame(jamesID = jID, ai = ai, bi = bi, si = si, cluster = current_cluster))
                     }))
  
  tbl_entropy_silhouette <- silCoeff %>%
    left_join(df1, by = c("jamesID" = "ID")) %>%
    mutate(curvesID = CONNECTORDataClustered@KData$CData$subjID[match(jamesID, CONNECTORDataClustered@KData$CData$jamesID)]) %>%
    group_by(cluster) %>%
    mutate(max_si = max(si)) %>%
    ungroup() %>%
    arrange(max_si, cluster, si) %>%
    dplyr::select(-max_si, -jamesID) %>%
    mutate(Order = factor(1:n(), levels = 1:n()))
  
  p1 <- ggplot(tbl_entropy_silhouette, aes(x = Order, y = si, fill = as.factor(cluster))) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Silhouette Plot",
         x = "Subject",
         y = "Silhouette Score",
         fill = "Cluster") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    coord_flip()
  
  p2 <- ggplot(tbl_entropy_silhouette, aes(x = Order)) +
    geom_segment(aes(yend = Entropy, y = 0)) +
    geom_point(aes(y = Entropy, color = as.factor(ClusterType), fill = as.factor(ClusterType)),
               alpha = 0.4, shape = 21, stroke = 2, size = 4) +
    theme_minimal() +
    labs(title = "Entropy Plot",
         x = "Subject",
         y = "Entropy",
         fill = "Cluster Type") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "none") +
    coord_flip()
  
  combined_plot <- p1 + p2
  return(list(
    plot = combined_plot,
    entropy_silhouette_table = tbl_entropy_silhouette
  ))
})
