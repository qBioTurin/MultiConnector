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
  combined_df$Cluster = resClust[combined_df$jamesID]
  
  return(combined_df %>% select(subjID, Cluster) %>% distinct() )
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
  MeanC = MeanC %>% rename(Cluster = cluster)
  return(MeanC )
})




#' @title clusterDistribution
#' @description Generate a table showing the distribution of subjects across clusters 
#' based on a specified feature
#' @param object CONNECTORDataClustered object
#' @param feature Feature name to analyze (must be present in annotations)
#' @param include_percentages Include percentage columns (default: TRUE)
#' @param include_totals Include total row and column (default: TRUE)
#' @return A contingency table showing feature values vs clusters with subject counts
#' @details 
#' This method creates a cross-tabulation showing how subjects with different 
#' feature values are distributed across clusters. Useful for understanding 
#' cluster composition and feature associations.
#' @examples
#' \dontrun{
#' # Basic distribution table
#' clusterDistribution(clustered_data, "treatment")
#' 
#' # With percentages and totals
#' clusterDistribution(clustered_data, "age_group", 
#'                    include_percentages = TRUE, include_totals = TRUE)
#' }
#' @import dplyr
#' @import tibble
#' @export
setGeneric("clusterDistribution", function(object, feature, 
                                          include_percentages = FALSE,
                                          include_totals = TRUE) {
  standardGeneric("clusterDistribution")
})

setMethod("clusterDistribution", signature(object = "CONNECTORDataClustered"), 
          function(object, feature, 
                   include_percentages = FALSE,
                   include_totals = TRUE) {
            
            # Get annotations and cluster assignments
            annotations <- object@KData$annotations
            
            # Check if feature exists
            if (!feature %in% colnames(annotations)) {
              available_features <- colnames(annotations)[!colnames(annotations) %in% c("subjID", "measureID", "jamesID")]
              stop(paste("Feature '", feature, "' not found in annotations.\n",
                        "Available features: ", paste(available_features, collapse = ", "), sep = ""))
            }
            
            getClusters(object) -> clusters_df
            combined_data <- merge(annotations, clusters_df, by = "subjID")
            # Remove rows with missing values for the feature
            combined_data <- combined_data[!is.na(combined_data[[feature]]), ]
            
            if (nrow(combined_data) == 0) {
              stop(paste("No valid data found for feature '", feature, "'"))
            }
            
            # Create contingency table
            cont_table <- table(combined_data[[feature]], combined_data$Cluster)
            
            # Convert to data frame for better formatting
            result_df <- as.data.frame.matrix(cont_table)
            
            # Add feature values as a column
            result_df <- result_df %>%
              tibble::rownames_to_column(var = feature) %>%
              as_tibble()
            
            # Ensure cluster columns are properly named
            cluster_cols <- paste("Cluster", 1:ncol(cont_table))
            colnames(result_df)[-1] <- cluster_cols
            
            # Add totals if requested
            if (include_totals) {
              # Add total row
              total_row <- result_df %>%
                select(-1) %>%
                summarise(across(everything(), sum)) %>%
                mutate(!!feature := "TOTAL", .before = 1)
              
              result_df <- bind_rows(result_df, total_row)
              
              # Add total column
              result_df <- result_df %>%
                mutate(Total = rowSums(select(., -1)))
            }
            
            # Add percentages if requested
            if (include_percentages) {
              total_subjects <- sum(cont_table)
              
              # Create percentage table
              perc_df <- result_df
              
              if (include_totals) {
                # Calculate percentages excluding total row/column
                for (i in 2:(ncol(result_df) - 1)) {  # Exclude feature column and Total column
                  perc_df[[paste0(names(result_df)[i], "_pct")]] <- 
                    round((result_df[[i]] / total_subjects) * 100, 2)
                }
              } else {
                for (i in 2:ncol(result_df)) {  # Exclude feature column
                  perc_df[[paste0(names(result_df)[i], "_pct")]] <- 
                    round((result_df[[i]] / total_subjects) * 100, 2)
                }
              }
              
              # Reorder columns to alternate between count and percentage
              cluster_names <- cluster_cols
              new_order <- c(feature)
              
              for (cluster in cluster_names) {
                new_order <- c(new_order, cluster, paste0(cluster, "_pct"))
              }
              
              if (include_totals) {
                new_order <- c(new_order, "Total")
              }
              
              # Select and reorder columns
              existing_cols <- new_order[new_order %in% names(perc_df)]
              result_df <- perc_df %>% select(all_of(existing_cols))
            }
            
            # Add metadata as attributes
            attr(result_df, "feature") <- feature
            attr(result_df, "n_clusters") <- ncol(cont_table)
            attr(result_df, "total_subjects") <- sum(cont_table)
            attr(result_df, "missing_values") <- sum(is.na(annotations[[feature]]))
            
            return(result_df)
          })