#' SubjectInfo
#'
#'@description
#'
#'  Get detailed information about specific subjects including cluster assignments,
#'  highlighted plot, and quality metrics
#'
#' @param CONNECTORDataClustered Connector object created with ConnectorData
#' @param subjIDs The subject ID(s) to analyze - can be a single ID or vector of IDs
#'
#' @return A list containing: cluster assignments, highlighted plot, quality metrics, and subject data
#'

#' @seealso CONNECTORDataClustered ClusterPlot
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom dplyr select filter mutate
#' @importFrom tidyr gather
#' @importFrom rlang sym
#' @importFrom viridis scale_color_viridis_d
#' @export

setGeneric("SubjectInfo", function(CONNECTORDataClustered,
                                   subjIDs) {
  standardGeneric("SubjectInfo")
})

setMethod("SubjectInfo", signature(CONNECTORDataClustered = "CONNECTORDataClustered"),
          function(CONNECTORDataClustered,
                   subjIDs) {
            
            # Get basic information
            G = CONNECTORDataClustered@TTandfDBandSil$G[1]
            resClust = CONNECTORDataClustered@CfitandParameters$pred$class.pred
            df = CONNECTORDataClustered@KData$CData
            
            # Get number of features per measure
            q <- sapply(CONNECTORDataClustered@KData$FullS, function(x) dim(x)[2])
            
            # Merge data
            combined_df = merge(CONNECTORDataClustered@KData$annotations, df)
            combined_df$cluster = resClust[combined_df$jamesID]
            
            TimeGrids = CONNECTORDataClustered@KData$TimeGrids
            
            # Ensure subjIDs is a vector
            subjIDs <- as.character(subjIDs)
            
            # Check if all subjIDs exist
            missing_subjects <- subjIDs[!subjIDs %in% combined_df$subjID]
            if (length(missing_subjects) > 0) {
              stop(paste("Subject ID(s) not found:", paste(missing_subjects, collapse = ", ")))
            }
            
            # Get cluster assignments for the specific subjects
            subject_clusters <- combined_df[combined_df$subjID %in% subjIDs, c("subjID", "cluster")]
            subject_clusters <- unique(subject_clusters)
            
            # Get entropy and silhouette information
            quality_metrics <- CONNECTORDataClustered@TTandfDBandSil
            
            # Compute curve predictions for mean curves
            curvepred = fclust.curvepred(
              CONNECTORDataClustered@CfitandParameters,
              CONNECTORDataClustered@KData,
              tau = 0.95,
              tau1 = 0.975,
              q = q
            )
            
            MeanC = do.call(rbind, lapply(names(curvepred), function(x) {
              as.data.frame(curvepred[[x]]$meancurves) -> Mean
              colnames(Mean) = as.character(1:G)
              Mean$measureID = x
              Mean$time = TimeGrids[[x]]
              return(Mean)
            })) %>%
              tidyr::gather(-time, -measureID, value = "value", key = "cluster")
            
            combined_df$cluster <- factor(combined_df$cluster)
            MeanC$cluster <- factor(MeanC$cluster)
            
            # Create highlighted plot
            # Add columns to identify the target subjects
            combined_df$is_target_subject <- combined_df$subjID %in% subjIDs
            combined_df$subject_type <- ifelse(combined_df$is_target_subject, 
                                               combined_df$subjID, 
                                               "Other")
            
            # Generate colors for multiple subjects
            n_subjects <- length(subjIDs)
            if (n_subjects == 1) {
              target_colors <- "red"
              names(target_colors) <- subjIDs[1]
            } else {
              # Use different colors for each target subject
              target_colors <- rainbow(n_subjects)
              names(target_colors) <- subjIDs
            }
            all_colors <- c(target_colors, "Other" = ggplot2::alpha("grey",0.5))
            
            # Split data for proper layering
            grey_data <- combined_df[!combined_df$is_target_subject, ]
            colored_data <- combined_df[combined_df$is_target_subject, ]
            
              p <- ggplot() +
                # Draw grey lines first (background)
                geom_line(data = grey_data,
                         aes(x = time,
                             y = value,
                             group = subjID),
                         color = "grey",
                         alpha = 0.5,
                         size = 0.5) +
                # Draw colored lines on top
                geom_line(data = colored_data,
                         aes(x = time,
                             y = value,
                             group = subjID,
                             color = subject_type),
                         size = 1.2) +
                scale_color_manual(values = target_colors) +
                geom_line(
                  data = MeanC,
                  aes(x = time, y = value),
                  linewidth = .9,
                  linetype = "dashed",
                  color = "black"
                ) +
                facet_grid(measureID ~ cluster, scales = "free_y") +
                labs(y = "", x = "Time", 
                     title = paste("Highlighted subjects:", paste(subjIDs, collapse = ", "))) +
                guides(color = guide_legend(title = "Subjects"))+ 
                theme_bw() +
              theme(
                axis.title.x = element_text(size = 14, face = "bold", family = "Times"),
                axis.text.x = element_text(size = 14, family = "Times"),
                axis.title.y = element_text(size = 14, face = "bold", family = "Times"),
                axis.text.y = element_text(size = 14, family = "Times"),
                strip.text = element_text(color = "black", size = 14, face = "bold", family = "Times"),
                plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                plot.margin = unit(c(0, 0, 0, 0), "cm")
              )
              
              # Create cluster assignment summary
              if (length(subjIDs) == 1) {
                cluster_summary <- paste("Subject", subjIDs[1], "belongs to Cluster", subject_clusters$cluster[1])
              } else {
                cluster_assignments <- paste(subject_clusters$subjID, "-> Cluster", subject_clusters$cluster)
                cluster_summary <- paste("Cluster assignments:", paste(cluster_assignments, collapse = "; "))
              }
            
            # Return comprehensive information
            return(list(
              cluster_assignments = cluster_summary,
              cluster_table = subject_clusters,
              highlighted_plot = p,
              quality_metrics = quality_metrics,
              subjects_data = combined_df[combined_df$subjID %in% subjIDs, ],
              n_subjects = length(subjIDs)
            ))
          })
