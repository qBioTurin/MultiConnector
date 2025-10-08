#' @title CONNECTORDataClustered
#' @description CONNECTORDataClustered is a class with the set choosen and with KData that is a list with all output of PRESETKMEANS that are necessary for call JUSTKMEANS and INTFCLUST
#' @slot TTandfDBandSil Contains TT, fDB, Silhouette and G choosen
#' @slot CfitandParameteres Contains values calculated during clusteranalysis
#' @slot h h value choosen
#' @slot freq frequency of the clusterization
#' @slot cluster.names names of the clusters
#' @slot KData contains: CData Contains timeseriefile datas, TimeGrids TimeGrids present in CONNECTORData, points Initial coefficent spline points, N subject number, S block diagonal matrix, FullS full block diagonal matrix

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
#' @title getAnnotations
#' @description Extract and display annotations from CONNECTORData or CONNECTORDataClustered object.
#' Shows all available features (annotation columns) in both cases.
#' @param object CONNECTORData or CONNECTORDataClustered object
#' @return A vector of annotation names for CONNECTORData objects.
#' @details 
#' This method provides the features available in the annotations of the provided object.
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

setMethod("getAnnotations", signature(object = "CONNECTORDataClustered"), function(object) {
  # Get annotations from KData
  annotations <- object@KData$annotations
  feature_cols <- colnames(annotations)[!colnames(annotations) %in% c("subjID", "measureID")]

  return(feature_cols)
})

setMethod("getAnnotations", signature(object = "CONNECTORData"), function(object) {
  # Get annotations directly from CONNECTORData object
  annotations <- names(object@annotations)
  
  return(annotations)
})


# Method to extract annotations for both classes
#' @title getClusters
#' @description Extract and display clusters from  CONNECTORDataClustered object.
#' Shows all available features (annotation columns) in both cases.
#' @param object CONNECTORDataClustered object
#' @return A dataframe with the cluster association for each subjID.
#' @details 
#' This method provides the features available in the annotations of the provided object.
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

setMethod("getClusters", signature(object = "CONNECTORDataClustered"), function(object) {
  # Get annotations from KData
  # Get predicted clusters
  resClust = object@CfitandParameters$pred$class.pred
  df = object@KData$CData
  # Merge data
  combined_df = merge(object@KData$annotations, df)
  combined_df$cluster = resClust[combined_df$jamesID]
  
  return(combined_df %>% select(subjID, cluster) %>% distinct() )
})


# Method to extract Clusters Centroids
#' @title getClustersCentroids
#' @description Extract and display clusters centroids from  CONNECTORDataClustered object.
#' Shows all available features (annotation columns) in both cases.
#' @param object CONNECTORDataClustered object
#' @return A dataframe with the cluster association for each subjID.
#' @details 
#' This method provides the features available in the annotations of the provided object.
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

setMethod("getClustersCentroids", signature(object = "CONNECTORDataClustered"), function(object) {

  TimeGrids = object@KData$TimeGrids
  # Get number of features per measure
  q <- sapply(object@KData$FullS, function(x)
    dim(x)[2])
  # Get number of clusters from CONNECTORDataClustered
  G = object@TTandfDBandSil$G[1]
  
  # Compute curve predictions
  curvepred = fclust.curvepred(
    object@CfitandParameters,
    object@KData,
    tau = 0.95,
    tau1 = 0.975,
    q = q
  )
  
  MeanC = do.call(rbind, lapply(names(curvepred), function(x) {
    as.data.frame(curvepred[[x]]$meancurves) -> Mean
    
    # Ensure column names match number of clusters
    colnames(Mean) = as.character(1:G)
    
    Mean$measureID = x
    Mean$time = TimeGrids[[x]]
    return(Mean)
  })) %>%
    tidyr::gather(-time, -measureID, value = "value", key = "cluster")
  
  MeanC$cluster <- factor(MeanC$cluster)
  return(MeanC )
})




#' @title clusterDistribution
#' @description Generate a table showing the distribution of subjects across clusters 
#' based on one or more features
#' @param object CONNECTORDataClustered object
#' @param feature Feature name(s) to analyze - can be a single feature or vector of features (must be present in annotations)
#' @param include_totals Include total row and column (default: TRUE)
#' @return A contingency table showing feature values vs clusters with subject counts
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
#' @import dplyr plyr
#' @import tibble
#' @export
setGeneric("clusterDistribution", function(object, feature, 
                                          include_percentages = FALSE,
                                          include_totals = TRUE) {
  standardGeneric("clusterDistribution")
})

setMethod("clusterDistribution", signature(object = "CONNECTORDataClustered"), 
          function(object, feature, 
                   include_totals = TRUE) {
            
            # Get annotations and cluster assignments
            annotations <- object@KData$annotations
            
            # Validate that all features exist
            for(f in feature){
              if (!f %in% colnames(annotations)) {
                available_features <- colnames(annotations)[!colnames(annotations) %in% c("subjID", "measureID", "jamesID")]
                stop(paste("Feature '", f, "' not found in annotations.\n",
                          "Available features: ", paste(available_features, collapse = ", "), sep = ""))
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
                                            values_fill = 0)
            
            # Identify cluster columns (all columns except the feature columns)
            cluster_cols <- colnames(cont_table)[!colnames(cont_table) %in% feature]
            
            # Rename cluster columns with "cluster" prefix
            for (i in seq_along(cluster_cols)) {
              old_name <- cluster_cols[i]
              new_name <- paste0("cluster", old_name)
              colnames(cont_table)[colnames(cont_table) == old_name] <- new_name
            }
            
            # Update cluster_cols with new names
            cluster_cols <- paste0("cluster", cluster_cols)
            
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
          })
            