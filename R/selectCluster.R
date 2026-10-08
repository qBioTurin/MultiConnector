#' Select a fitted clustering solution
#'
#' Selects one candidate partition from the output of [estimateCluster()] for a
#' fixed number of clusters.
#'
#' @param results A list returned by [estimateCluster()].
#' @param G A single integer number of clusters fitted in `results`.
#' @param best Selection criterion:
#'   \itemize{
#'     \item `"MaxFreq"`: most frequent initial partition;
#'     \item `"MinfDB"`: minimum functional Davies--Bouldin index;
#'     \item `"MaxSilhouette"`: maximum mean silhouette width.
#'   }
#'
#' @return A [CONNECTORDataClustered-class] object containing the selected fit,
#'   cluster assignments, quality metrics, labels, and preprocessed data.
#'
#' @details
#' Candidate fits containing errors are excluded before selection. Ties are
#' resolved deterministically by their metric ordering.
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
#' @rdname selectCluster
setMethod("selectCluster", signature(), function(results, G, best) {
  error_indices <- sapply(results$Clusterings, function(res) {
    is.list(res) && "Error" %in% names(res$TTandfDBandSil)
  })
  results$Clusterings <- results$Clusterings[!error_indices]
  indexes <-
    do.call(rbind, lapply(seq_along(results$Clusterings), function(x) {
      xx <- results$Clusterings[[x]]
      df <- data.frame(xx$TTandfDBandSil)
      df$freq <- xx$freq
      df$which <- x
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
  } else if (best == "MinfDB") {
    indexesfiltered <- indexes %>%
      group_by(G) %>%
      filter(fDB == min(fDB)) %>%
      tidyr::gather(-G, -freq, -which, value = "IndexesV", key = "Indexes") %>%
      group_by(G, Indexes) %>%
      arrange(G, Indexes, IndexesV) %>%
      slice(1) %>%
      ungroup()
  } else if (best == "MaxSilhouette") {
    indexesfiltered <- indexes %>%
      group_by(G) %>%
      filter(Sil == max(Sil)) %>%
      tidyr::gather(-G, -freq, -which, value = "IndexesV", key = "Indexes") %>%
      group_by(G, Indexes) %>%
      arrange(G, Indexes, IndexesV) %>%
      slice(1) %>%
      ungroup()
  } else {
    stop("Error: 'best' must be 'MaxFeq' or 'MinfDB' or 'MaxSilhouette'")
  }
  pos <- indexesfiltered %>%
    filter(.data[[sym("G")]] == !!G, Indexes == "fDB") %>%
    pull(which)
  res <- results$Clusterings[[pos]]

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
