#' @title Select Optimal Cluster Configuration
#'
#' @description
#' Selects the best cluster configuration from clustering analysis results based on quality metrics.
#' This function evaluates different clustering solutions using functional Data Depth (fDB) and 
#' Total Time (TT) metrics, providing visualization through violin plots to aid in selection.
#' Users can choose between selecting the most frequent configuration or the one with minimum fDB.
#'
#' @param results A list object returned by \code{estimateCluster()} containing multiple clustering
#'   solutions with different parameters and random initializations.
#' @param G Integer specifying the number of clusters to select. Must be one of the values
#'   used in the original clustering analysis.
#' @param best Character string specifying the selection criterion:
#'   \itemize{
#'     \item \code{"MaxFreq"}: Select the most frequently occurring configuration across runs
#'     \item \code{"MinfDB"}: Select the configuration with minimum functional Data Depth
#'   }
#'
#' @return A CONNECTORDataClustered object containing:
#'   \itemize{
#'     \item Selected cluster configuration and parameters
#'     \item Cluster assignments for each curve
#'     \item Quality metrics (silhouette, entropy, fDB, TT)
#'     \item All necessary information for downstream analysis and visualization
#'   }
#'
#' @details
#' This function is a crucial step in the clustering workflow. It helps identify the most
#' stable and high-quality clustering solution from multiple runs. The function displays
#' violin plots showing the distribution of fDB and TT values across different runs,
#' highlighting the selected configuration.
#' 
#' \strong{Selection criteria:}
#' \itemize{
#'   \item \strong{MaxFreq}: Chooses the configuration that appears most often across runs,
#'     indicating stability and reproducibility
#'   \item \strong{MinfDB}: Chooses the configuration with the lowest functional Data Depth,
#'     indicating the most compact and well-separated clusters
#' }
#'
#' @examples
#' \dontrun{
#' # Select most frequent 3-cluster configuration
#' selected_config <- selectCluster(cluster_results, G = 3, best = "MaxFreq")
#' 
#' # Select 4-cluster configuration with minimum fDB
#' selected_config <- selectCluster(cluster_results, G = 4, best = "MinfDB")
#' 
#' # View the selected configuration
#' print(selected_config)
#' }
#'
#' @seealso 
#' \code{\link{estimateCluster}} for performing the initial clustering analysis,
#' \code{\link{validateCluster}} for validating the selected configuration,
#' \code{\link{plot}} for visualizing the selected clusters
#'
#' @export
#'
setGeneric("selectCluster", function(results, G, best) {
  standardGeneric("selectCluster")
})
setMethod("selectCluster", signature(), function(results, G, best) {
  
  error_indices <- sapply(results$Clusterings, function(res) {
    is.list(res) && "Error" %in% names(res$TTandfDBandSil)
  })
  if(length(which(error_indices)) > 0)
    results$Clusterings <- results$Clusterings[-which(error_indices)]
  
  
  indexes =
    do.call(rbind, lapply(seq_along(results$Clusterings), function(x) {
        xx = results$Clusterings[[x]]
        df = data.frame(xx$TTandfDBandSil)
        df$freq = xx$freq
        df$which = x
        return(df)
    }))
  if (best == "MaxFreq") {
    indexesfiltered <- indexes %>%
      tidyr::gather(-G, -freq, -which, value = "IndexesV", key = "Indexes") %>%
      group_by(G, Indexes) %>%
      filter(freq == max(freq)) %>%
      arrange(G, Indexes, IndexesV) %>%
      slice(1) %>%
      ungroup()
  }
  else if (best == "MinfDB") {
    indexesfiltered = indexes %>%
      group_by(G) %>%
      filter(fDB == min(fDB)) %>%
      tidyr::gather(-G, -freq, -which, value = "IndexesV", key = "Indexes") %>%
      group_by(G, Indexes) %>%
      arrange(G, Indexes, IndexesV) %>%
      slice(1) %>%
      ungroup()
  }
  else if (best == "MaxSilhouette") {
    indexesfiltered = indexes %>%
      group_by(G) %>%
      filter(Sil == max(Sil)) %>%
      tidyr::gather(-G, -freq, -which, value = "IndexesV", key = "Indexes") %>%
      group_by(G, Indexes) %>%
      arrange(G, Indexes, IndexesV) %>%
      slice(1) %>%
      ungroup()
  }
  else{
    stop("Error: 'best' must be 'MaxFeq' or 'MinfDB' or 'maxSilhouette")
  }
  pos = indexesfiltered %>% filter(.data[[sym("G")]] == !!G, Indexes == "fDB") %>% pull(which)
  res = results$Clusterings[[pos]]
  
  return(
    new(
      "CONNECTORDataClustered",
      TTandfDBandSil = as_tibble(res$TTandfDBandSil),
      CfitandParameters = res$CfitandParameters,
      h = res$h,
      freq = res$freq,
      cluster.names = LETTERS[1:G],
      KData = results$KData
    )
  )
  
})