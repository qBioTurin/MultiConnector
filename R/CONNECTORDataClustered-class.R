#' Selected functional clustering model
#'
#' An S4 container for a clustering solution selected from the candidates fitted
#' by [estimateCluster()]. Objects are created with [selectCluster()].
#'
#' @slot TTandfDBandSil A tibble containing the total tightness (`TT`),
#'   functional Davies--Bouldin index (`fDB`), silhouette index (`Sil`), and
#'   selected number of clusters (`G`).
#' @slot CfitandParameters A list containing the fitted model parameters,
#'   posterior probabilities, and predicted cluster assignments.
#' @slot h Numeric projection dimension used by the fitted model.
#' @slot freq Numeric frequency with which this partition occurred among the
#'   random initialisations.
#' @slot cluster.names Character vector used to label clusters.
#' @slot KData A list of preprocessed inputs, including the long-format data
#'   (`CData`), time grids, initial spline coefficients, number of subjects,
#'   design matrix (`S`), and full spline bases (`FullS`).
#'
#' @seealso [estimateCluster()], [selectCluster()], [getClusters()], and [plot()].
#' @export

setClass(
  "CONNECTORDataClustered",
  slots = list(
    TTandfDBandSil = "tbl_df",
    CfitandParameters = "list",
    h = "numeric",
    freq = "numeric",
    cluster.names = "character",
    KData = "list"
  )
)

# Method to extract annotations for both classes
#' List available annotation columns
#'
#' @param object A [CONNECTORData-class] or
#'   [CONNECTORDataClustered-class] object.
#' @return A character vector of annotation column names. For clustered objects,
#'   identifier columns (`subjID` and `measureID`) are excluded.
#' @examples
#' \dontrun{
#' # For CONNECTORData
#' getAnnotations(my_connector_data)
#'
#' # For CONNECTORDataClustered
#' getAnnotations(my_clustered_data)
#' }
#' @import dplyr
#' @export
setGeneric("getAnnotations", function(object) {
  standardGeneric("getAnnotations")
})

#' @rdname getAnnotations
setMethod("getAnnotations", signature(object = "CONNECTORDataClustered"), function(object) {
  # Get annotations from KData
  annotations <- object@KData$annotations
  feature_cols <- colnames(annotations)[!colnames(annotations) %in% c("subjID", "measureID")]

  return(feature_cols)
})

#' @rdname getAnnotations
setMethod("getAnnotations", signature(object = "CONNECTORData"), function(object) {
  # Get annotations directly from CONNECTORData object
  annotations <- names(object@annotations)

  return(annotations)
})

# Method to extract measures
#' List measures in a MultiConnector object
#'
#' @param object A [CONNECTORData-class] or
#'   [CONNECTORDataClustered-class] object.
#' @return A character vector containing the measure names in time-grid order.
#' @examples
#' \dontrun{
#' getMeasures(data)
#' getMeasures(clustered_data)
#' }
#' @export
setGeneric("getMeasures", function(object) {
  standardGeneric("getMeasures")
})

#' @rdname getMeasures
setMethod("getMeasures", signature(object = "CONNECTORData"), function(object) {
  measures <- names(object@TimeGrids)
  return(measures)
})

#' @rdname getMeasures
setMethod("getMeasures", signature(object = "CONNECTORDataClustered"), function(object) {
  measures <- names(object@KData$TimeGrids)
  return(measures)
})


# Method to extract annotations for both classes
#' Extract cluster assignments
#'
#' @param object A [CONNECTORDataClustered-class] object.
#' @return A data frame with one row per subject and columns `subjID` and
#'   `cluster`. Cluster labels are returned as a factor using the names supplied
#'   by [setClusterNames()].
#' @examples
#' \dontrun{
#'
#' # For CONNECTORDataClustered
#' getClusters(my_clustered_data)
#' }
#' @import dplyr
#' @export
setGeneric("getClusters", function(object) {
  standardGeneric("getClusters")
})

#' @rdname getClusters
setMethod("getClusters", signature(object = "CONNECTORDataClustered"), function(object) {
  # Get annotations from KData
  # Get predicted clusters
  resClust <- object@CfitandParameters$pred$class.pred
  df <- object@KData$CData
  # Merge data
  combined_df <- merge(object@KData$annotations, df)
  combined_df$cluster <- resClust[combined_df$jamesID]

  result <- combined_df %>%
    select(subjID, cluster) %>%
    distinct()

  # Apply cluster names if set
  if (length(object@cluster.names) > 0) {
    result$cluster <- factor(result$cluster,
      levels = seq_along(object@cluster.names),
      labels = object@cluster.names
    )
  }

  return(result)
})


# Method to set cluster names
#' Set cluster labels
#'
#' @param object A [CONNECTORDataClustered-class] object.
#' @param names A unique character vector with one label for each cluster, in
#'   numeric cluster order.
#' @return A copy of `object` with updated cluster labels.
#' @details
#' This method allows you to assign meaningful names to clusters instead of using
#' numeric identifiers. The names are stored in the object and used by other methods
#' like getClusters, getClustersCentroids, and clusterDistribution.
#' @examples
#' \dontrun{
#' # Set names for a 3-cluster solution
#' clustered_data <- setClusterNames(clustered_data, c("Low", "Medium", "High"))
#'
#' # Now getClusters will return named clusters
#' getClusters(clustered_data)
#' }
#' @export
setGeneric("setClusterNames", function(object, names) {
  standardGeneric("setClusterNames")
})

#' @rdname setClusterNames
#' @export
setMethod(
  "setClusterNames", signature(object = "CONNECTORDataClustered"),
  function(object, names) {
    # Get number of clusters
    G <- object@TTandfDBandSil$G[1]

    if (length(names) != G) {
      stop(paste("Number of names (", length(names), ") must match number of clusters (", G, ")", sep = ""))
    }

    if (any(duplicated(names))) {
      stop("Cluster names must be unique")
    }

    object@cluster.names <- as.character(names)
    return(object)
  }
)


# Method to get cluster names
#' Get cluster labels
#'
#' @param object A [CONNECTORDataClustered-class] object.
#' @return A character vector with one label per cluster. Numeric labels are
#'   generated when no custom names have been stored.
#' @examples
#' \dontrun{
#' # Get cluster names
#' getClusterNames(clustered_data)
#' }
#' @export
setGeneric("getClusterNames", function(object) {
  standardGeneric("getClusterNames")
})

#' @rdname getClusterNames
#' @export
setMethod(
  "getClusterNames", signature(object = "CONNECTORDataClustered"),
  function(object) {
    if (length(object@cluster.names) == 0) {
      G <- object@TTandfDBandSil$G[1]
      return(as.character(1:G))
    }

    return(object@cluster.names)
  }
)


# Method to extract Clusters Centroids
#' Extract fitted cluster-centroid curves
#'
#' Computes the fitted mean trajectory for every cluster and measure on the
#' corresponding full time grid.
#'
#' @param object A [CONNECTORDataClustered-class] object.
#' @return A data frame with columns `time`, `measureID`, `cluster`, and `value`.
#' @examples
#' \dontrun{
#'
#' # For CONNECTORDataClustered
#' getClustersCentroids(my_clustered_data)
#' }
#' @import dplyr
#' @export
setGeneric("getClustersCentroids", function(object) {
  standardGeneric("getClustersCentroids")
})

#' @rdname getClustersCentroids
setMethod("getClustersCentroids", signature(object = "CONNECTORDataClustered"), function(object) {
  TimeGrids <- object@KData$TimeGrids
  # Get number of features per measure
  getParameters(object) -> params
  q <- params$p
  G <- params$G

  # Compute curve predictions
  curvepred <- fclust.curvepred(
    object@CfitandParameters,
    KData = object@KData,
    tau = 0.95,
    tau1 = 0.975,
    q = q
  )

  # Get cluster names
  cluster_names <- getClusterNames(object)

  MeanC <- do.call(rbind, lapply(names(curvepred), function(x) {
    as.data.frame(curvepred[[x]]$meancurves) -> Mean

    # Ensure column names match number of clusters
    colnames(Mean) <- as.character(1:G)

    Mean$measureID <- x
    Mean$time <- TimeGrids[[x]]
    return(Mean)
  })) %>%
    tidyr::gather(-time, -measureID, value = "value", key = "cluster")

  # Apply cluster names
  MeanC$cluster <- factor(MeanC$cluster, levels = as.character(1:G), labels = cluster_names)
  return(MeanC)
})


#' Tabulate annotation values across clusters
#'
#' Cross-tabulates one or more subject-level annotations against the selected
#' cluster assignments. Rows with missing values in any requested feature are
#' excluded.
#'
#' @param object A [CONNECTORDataClustered-class] object.
#' @param feature Character vector naming annotation columns to tabulate.
#' @param include_percentages Logical placeholder retained for API
#'   compatibility. The current implementation returns counts.
#' @param include_totals Logical; if `TRUE`, append marginal totals.
#' @return A data frame whose leading columns are the requested features,
#'   followed by one count column per cluster and, when `include_totals = TRUE`,
#'   a `Total` column. Metadata about missing and complete cases is stored as
#'   attributes.
#' @details
#' This method creates a cross-tabulation showing how subjects with different
#' feature values are distributed across clusters. Can handle multiple features
#' simultaneously for multi-dimensional analysis. Useful for understanding
#' cluster composition and feature associations.
#' @examples
#' \dontrun{
#' # Single feature distribution
#' clusterDistribution(clustered_data, "treatment")
#'
#' # Multiple features distribution
#' clusterDistribution(clustered_data, c("treatment", "age_group"))
#'
#' # With totals
#' clusterDistribution(clustered_data, "age_group", include_totals = TRUE)
#' }
#' @import dplyr
#' @import tibble
#' @export
setGeneric("clusterDistribution", function(object, feature,
                                           include_percentages = FALSE,
                                           include_totals = TRUE) {
  standardGeneric("clusterDistribution")
})

#' @rdname clusterDistribution
setMethod(
  "clusterDistribution", signature(object = "CONNECTORDataClustered"),
  function(object, feature,
           include_totals = TRUE) {
    # Get annotations and cluster assignments
    annotations <- object@KData$annotations

    # Validate that all features exist
    for (f in feature) {
      if (!f %in% colnames(annotations)) {
        available_features <- colnames(annotations)[!colnames(annotations) %in% c("subjID", "measureID", "jamesID")]
        stop(paste("Feature '", f, "' not found in annotations.\n",
          "Available features: ", paste(available_features, collapse = ", "),
          sep = ""
        ))
      }
    }

    # Get cluster assignments
    clusters_df <- getClusters(object)
    combined_data <- merge(annotations, clusters_df, by = "subjID")

    # Remove rows with missing values for any of the requested features
    combined_data_clean <- combined_data[complete.cases(combined_data[, feature, drop = FALSE]), ]

    if (nrow(combined_data_clean) == 0) {
      stop(paste("No valid data found after removing missing values for feature(s):", paste(feature, collapse = ", ")))
    }

    # Create contingency table using plyr::count
    cont_table <- plyr::count(combined_data_clean[, c("cluster", feature)])

    # Pivot wider to get clusters as columns
    cont_table <- tidyr::pivot_wider(cont_table,
      names_from = cluster,
      values_from = freq,
      values_fill = 0
    )


    # Identify cluster columns (all columns except the feature columns)
    cluster_cols <- colnames(cont_table)[!colnames(cont_table) %in% feature]

    # Get cluster names for labeling
    cluster_names <- getClusterNames(object)

    # Rename cluster columns with "cluster" prefix and custom names
    for (i in seq_along(cluster_cols)) {
      old_name <- cluster_cols[i]
      cluster_idx <- as.integer(old_name)
      if (!is.na(cluster_idx) && cluster_idx <= length(cluster_names)) {
        new_name <- paste0(cluster_names[cluster_idx])
      } else {
        new_name <- paste0(old_name)
      }
      colnames(cont_table)[colnames(cont_table) == old_name] <- new_name
    }

    # Update cluster_cols with new names
    cluster_cols <- colnames(cont_table)[!colnames(cont_table) %in% feature]

    # Start with cont_table as result
    result_df <- cont_table

    # Add totals if requested
    if (include_totals) {
      # Calculate total row: sum across all cluster columns
      total_values <- result_df %>%
        select(all_of(cluster_cols)) %>%
        summarise(across(everything(), sum))

      # Create total row with "TOTAL" for feature columns
      total_row <- data.frame(matrix("TOTAL", nrow = 1, ncol = length(feature)))
      colnames(total_row) <- feature
      total_row <- cbind(total_row, total_values)

      for (f in feature) {
        result_df[[f]] <- as.character(result_df[[f]])
        total_row[[f]] <- as.character(total_row[[f]])
      }
      result_df <- bind_rows(result_df, total_row)

      # Bind the total row
      result_df <- bind_rows(result_df, total_row)

      # Add total column: sum across all cluster columns for each row
      result_df <- result_df %>%
        mutate(Total = rowSums(select(., all_of(cluster_cols)), na.rm = TRUE))
    }

    # Calculate missing values count for each feature
    missing_count <- sapply(feature, function(f) sum(is.na(annotations[[f]])))

    # Add metadata as attributes
    attr(result_df, "feature") <- feature
    attr(result_df, "n_clusters") <- length(cluster_cols)
    attr(result_df, "total_subjects") <- sum(result_df[nrow(result_df), cluster_cols], na.rm = TRUE)
    attr(result_df, "missing_values") <- missing_count
    attr(result_df, "n_complete_cases") <- nrow(combined_data_clean)

    return(result_df)
  }
)

# Method to get selected parameters
#' Extract selected clustering dimensions
#'
#' @param object A [CONNECTORDataClustered-class] object.
#' @return A list with `G` (number of clusters), `p` (named number of spline
#'   basis functions per measure), and `h` (projection dimension).
#' @examples
#' \dontrun{
#' # Get parameters
#' getParameters(clustered_data)
#' }
#' @export
setGeneric("getParameters", function(object) {
  standardGeneric("getParameters")
})

#' @rdname getParameters
#' @export
setMethod(
  "getParameters", signature(object = "CONNECTORDataClustered"),
  function(object) {
    # G is stored in TTandfDBandSil
    G <- object@TTandfDBandSil$G[1]

    # h is stored in h slot
    h <- object@h

    # p is the number of basis functions for each measure,
    # stored as columns in the FullS matrices in KData
    p <- sapply(object@KData$FullS, ncol)

    return(list(G = G, p = p, h = h))
  }
)


# Method to get membership probabilities
#' Extract posterior cluster-membership probabilities
#'
#' @param object A [CONNECTORDataClustered-class] object.
#' @return A tibble with a `subjID` column followed by one probability column
#'   per cluster. Probability columns use the current cluster labels.
#' @examples
#' \dontrun{
#' # Get membership probabilities
#' getMembershipProbs(clustered_data)
#' }
#' @importFrom tibble as_tibble
#' @export
setGeneric("getMembershipProbs", function(object) {
  standardGeneric("getMembershipProbs")
})

#' @rdname getMembershipProbs
#' @export
setMethod(
  "getMembershipProbs", signature(object = "CONNECTORDataClustered"),
  function(object) {
    # Probabilities are stored in CfitandParameters$pred$probs
    probs <- object@CfitandParameters$pred$probs

    # Get cluster names
    cluster_names <- getClusterNames(object)
    colnames(probs) <- cluster_names

    # Get subject IDs
    # Subject IDs correspond to rows in jamesID order (1 to N)
    # presetKmeans: CData$jamesID <- as.integer(factor(CData$subjID, levels = unique(CData$subjID)))
    subjIDs <- unique(object@KData$CData$subjID)

    result <- as_tibble(cbind(subjID = subjIDs, as.data.frame(probs)))

    return(result)
  }
)
