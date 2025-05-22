#' ConfigSelection
#'
#'@description
#'
#'  Print fDB and TT violin plots with best and most frequent value
#'
#' @param results ClusterAnalysis output
#' @param G Number of cluster selected
#' @param best Setup choosen. "MaxFreq" or "MinfDB"
#'
#' @return an object with the cofiguration selected
#'

#' @seealso ClusterAnalysis
#'
#' @import
#' @export
#'
setGeneric("ConfigSelection", function(results, G, best) {
  standardGeneric("ConfigSelection")
})
setMethod("ConfigSelection", signature(), function(results, G, best) {
  indexes =
    do.call(rbind, lapply(seq_along(results), function(x) {
      if (x != (length(results))) {
        xx = results[[x]]
        df = data.frame(xx$TTandfDBandSil)
        df$freq = xx$freq
        df$which = x
        return(df)
      }
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
  res = results[[pos]]
  
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