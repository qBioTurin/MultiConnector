setGeneric("show", function(object, ...) standardGeneric("show"))

#' @title show
#' @description show method for MultiConnector objects.
#' @param object The object to display.
#' @name show
#' @rdname show
#' @export
setMethod("show", signature(object = "CONNECTORData"), function(object) {
  cat("CONNECTORData object with:\n")
  cat("- Subjects:", length(unique(object@curves$subjID)), "\n")
  cat("- Measures:", length(unique(object@curves$measureID)), "\n")
  cat("\nLengths summary:")
  summcounts <- summary(object@curves %>% count(.data$curvesID) %>% pull(.data$n))
  cat("\n", names(summcounts), "\n")
  cat(summcounts, "\n")
  return(invisible(NULL))
})

#' @rdname show
#' @export

setMethod("show", signature(object = "list"), function(object) {
  # Check if this is likely an estimateCluster output
  if (!all(c("Clusterings", "KData") %in% names(object))) {
    methods::showDefault(object)
    return(invisible(NULL))
  }

  results <- object
  error_indices <- sapply(results$Clusterings, function(res) {
    is.list(res) && "Error" %in% names(res$TTandfDBandSil)
  })
  if (length(which(error_indices)) > 0) {
    results$Clusterings <- results$Clusterings[-which(error_indices)]
  }

  if (length(results$Clusterings) == 0) {
    cat("Clustering Estimate object (no valid runs)\n")
    return(invisible(NULL))
  }

  indexes <- do.call(rbind, lapply(seq_along(results$Clusterings), function(x) {
    xx <- results$Clusterings[[x]]
    df <- data.frame(xx$TTandfDBandSil)
    df$freq <- xx$freq
    df$which <- x
    return(df)
  }))

  indexesfilteredMaxFreq <- indexes %>%
    group_by(.data$G) %>%
    filter(.data$freq == max(.data$freq)) %>%
    select(-.data$freq, -.data$which) %>%
    mutate(Index = "maxFreq") %>%
    select(.data$G, .data$Index, .data$fDB, .data$Sil, .data$TT)
  indexesfilteredMinfDB <- indexes %>%
    group_by(.data$G) %>%
    filter(.data$fDB == min(.data$fDB)) %>%
    select(-.data$freq, -.data$which) %>%
    mutate(Index = "minfDB") %>%
    select(.data$G, .data$Index, .data$fDB, .data$Sil, .data$TT)
  indexesfilteredMaxSil <- indexes %>%
    group_by(.data$G) %>%
    filter(.data$Sil == max(.data$Sil)) %>%
    select(-.data$freq, -.data$which) %>%
    mutate(Index = "maxSilhouette") %>%
    select(.data$G, .data$Index, .data$fDB, .data$Sil, .data$TT)
  rbind(indexesfilteredMaxSil, indexesfilteredMinfDB, indexesfilteredMaxFreq) %>%
    arrange(.data$G, .data$Index) %>%
    ungroup() -> indexesfiltered


  return(indexesfiltered)
})
