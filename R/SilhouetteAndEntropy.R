
setGeneric("SilEntropy", function(best) {
  standardGeneric("SilEntropy")
})

setMethod("SilEntropy", signature(), function(best) {
  probs = best@CfitandParameters$pred$probs
  colnames(probs) = best@cluster.names
  
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
  
  q <- sapply(1:length(best@KData$FullS), function(x)
    ncol(best@KData$FullS[[x]]))
  
  cluster_assignments <- best@CfitandParameters$pred$class.pred
  curvepred <- fclust.curvepred(data = best@CfitandParameters,
                                q = q,
                                KData = best@KData)
  
  all_distances = DistAllSubjCurves2Curves.sapl(best@KData, curvepred)
  
  silCoeff = do.call(rbind, 
                     lapply(1:max(best@KData$CData$jamesID), function(jID) {
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
    mutate(curvesID = best@KData$CData$subjID[match(jamesID, best@KData$CData$jamesID)]) %>%
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
