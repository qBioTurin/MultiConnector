#' Generate Analysis Report
#'
#'@description
#'
#'  Generate a comprehensive analysis report including all base plots and analysis choices
#'  for both CONNECTORData and CONNECTORDataClustered objects
#'
#' @param data CONNECTORData object (optional if clustered_data is provided)
#' @param clustered_data CONNECTORDataClustered object (optional if data is provided)
#' @param report_title Title for the report
#' @param include_dimension_analysis Include dimension estimation analysis (default: TRUE)
#' @param include_cluster_analysis Include clustering analysis (default: TRUE)
#' @param features Vector of feature names to analyze (optional)
#' @param output_format Output format: "list" (default), "html", "pdf" (future implementation)
#'
#' @return A comprehensive report containing all plots and analysis summaries
#'

#' @seealso CONNECTORData CONNECTORDataClustered
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom dplyr select filter mutate summarise group_by
#' @importFrom tidyr gather
#' @importFrom patchwork wrap_plots
#' @export

setGeneric("generateReport", function(data = NULL,
                                      clustered_data = NULL,
                                      report_title = "MultiConnector Analysis Report",
                                      include_dimension_analysis = TRUE,
                                      include_cluster_analysis = TRUE,
                                      features = NULL,
                                      output_format = "list") {
  standardGeneric("generateReport")
})

setMethod("generateReport", signature(),
          function(data = NULL,
                   clustered_data = NULL,
                   report_title = "MultiConnector Analysis Report",
                   include_dimension_analysis = TRUE,
                   include_cluster_analysis = TRUE,
                   features = NULL,
                   output_format = "list") {
            
            # Validation
            if (is.null(data) && is.null(clustered_data)) {
              stop("At least one of 'data' or 'clustered_data' must be provided")
            }
            
            # Extract data from clustered object if needed
            if (!is.null(clustered_data) && is.null(data)) {
              # Reconstruct CONNECTORData from clustered data
              df <- clustered_data@KData$CData
              annotations <- clustered_data@KData$annotations
              TimeGrids <- clustered_data@KData$TimeGrids
              
              # Create dimensions summary
              dimensions <- df %>%
                group_by(subjID, measureID) %>%
                summarise(nTimePoints = n(), .groups = "drop") %>%
                rename(curvesID = subjID)
            }
            
            # Initialize report
            report <- list(
              title = report_title,
              generated_on = Sys.time(),
              summary = list(),
              plots = list(),
              tables = list(),
              analysis_choices = list()
            )
            
            cat("Generating comprehensive analysis report...\n")
            
            # ===== DATA SUMMARY SECTION =====
            if (!is.null(data)) {
              cat("- Analyzing base data characteristics...\n")
              
              # Data overview
              n_subjects <- length(unique(data@curves$curvesID))
              n_measures <- length(unique(data@curves$measureID))
              n_timepoints <- nrow(data@curves)
              time_range <- range(data@curves$time)
              
              report$summary$data_overview <- list(
                n_subjects = n_subjects,
                n_measures = n_measures,
                n_timepoints = n_timepoints,
                time_range = time_range,
                measures = unique(data@curves$measureID)
              )
              
              # Time series plots
              report$plots$timeseries_plot <- PlotTimeSeries(data)
              
              # Time grid visualization
              if (exists("plotTimes")) {
                report$plots$time_grid_plot <- plotTimes(data)
              }
              
            } else {
              # Extract summary from clustered data
              df <- clustered_data@KData$CData
              report$summary$data_overview <- list(
                n_subjects = length(unique(df$subjID)),
                n_measures = length(unique(df$measureID)),
                n_timepoints = nrow(df),
                time_range = range(df$time),
                measures = unique(df$measureID)
              )
            }
            
            # ===== DIMENSION ANALYSIS SECTION =====
            if (include_dimension_analysis && !is.null(data)) {
              cat("- Performing dimension estimation analysis...\n")
              
              # Dimension estimation
              tryCatch({
                dim_analysis <- estimatepDimension(data, p = 3:15)
                report$plots$dimension_analysis <- dim_analysis
                report$analysis_choices$recommended_dimensions <- "See dimension analysis plot"
              }, error = function(e) {
                cat("Warning: Dimension analysis failed:", e$message, "\n")
                report$analysis_choices$recommended_dimensions <- "Analysis failed"
              })
            }
            
            # ===== CLUSTERING ANALYSIS SECTION =====
            if (include_cluster_analysis && !is.null(clustered_data)) {
              cat("- Analyzing clustering results...\n")
              
              # Clustering summary
              G <- clustered_data@TTandfDBandSil$G[1]
              h_value <- clustered_data@h
              quality_metrics <- clustered_data@TTandfDBandSil
              
              report$summary$clustering_overview <- list(
                n_clusters = G,
                h_parameter = h_value,
                cluster_names = clustered_data@cluster.names,
                quality_metrics = quality_metrics
              )
              
              # Cluster assignments
              resClust <- clustered_data@CfitandParameters$pred$class.pred
              cluster_sizes <- table(resClust)
              
              report$tables$cluster_assignments <- data.frame(
                Cluster = names(cluster_sizes),
                Size = as.numeric(cluster_sizes),
                Percentage = round(as.numeric(cluster_sizes) / sum(cluster_sizes) * 100, 2)
              )
              
              # Basic cluster plot
              report$plots$cluster_plot_basic <- ClusterPlot(clustered_data)
              
              # Feature-based cluster plots
              if (!is.null(features)) {
                cat("- Generating feature-based visualizations...\n")
                report$plots$cluster_plots_by_feature <- list()
                
                for (feature in features) {
                  tryCatch({
                    # Check if feature exists in annotations
                    annotations <- clustered_data@KData$annotations
                    if (feature %in% colnames(annotations)) {
                      report$plots$cluster_plots_by_feature[[feature]] <- 
                        ClusterPlot(clustered_data, feature = feature)
                    } else {
                      cat(paste("Warning: Feature", feature, "not found in annotations\n"))
                    }
                  }, error = function(e) {
                    cat(paste("Warning: Failed to create plot for feature", feature, ":", e$message, "\n"))
                  })
                }
              }
              
              # Discriminant analysis if available
              tryCatch({
                report$plots$discriminant_plot <- DiscriminantPlot(clustered_data)
              }, error = function(e) {
                cat("Warning: Discriminant plot failed:", e$message, "\n")
              })
              
              # Quality metrics table
              report$tables$quality_metrics <- clustered_data@TTandfDBandSil
              
              # Analysis choices summary
              report$analysis_choices$clustering <- list(
                number_of_clusters = G,
                h_parameter = h_value,
                selection_criteria = "Based on quality metrics (Silhouette, Entropy)"
              )
            }
            
            # ===== ADVANCED ANALYSIS SECTION =====
            if (!is.null(clustered_data)) {
              cat("- Generating advanced analysis summaries...\n")
              
              # Cluster characteristics
              df <- clustered_data@KData$CData
              annotations <- clustered_data@KData$annotations
              resClust <- clustered_data@CfitandParameters$pred$class.pred
              
              # Merge for analysis
              combined_df <- merge(annotations, df)
              combined_df$cluster <- resClust[combined_df$jamesID]
              
              # Summary statistics by cluster
              cluster_stats <- combined_df %>%
                group_by(cluster, measureID) %>%
                summarise(
                  mean_value = mean(value, na.rm = TRUE),
                  sd_value = sd(value, na.rm = TRUE),
                  min_value = min(value, na.rm = TRUE),
                  max_value = max(value, na.rm = TRUE),
                  n_observations = n(),
                  .groups = "drop"
                )
              
              report$tables$cluster_statistics <- cluster_stats
              
              # Feature distribution by cluster (if features provided)
              if (!is.null(features) && length(features) > 0) {
                feature_summaries <- list()
                
                for (feature in features) {
                  if (feature %in% colnames(annotations)) {
                    feature_by_cluster <- annotations %>%
                      mutate(cluster = resClust[jamesID]) %>%
                      group_by(cluster) %>%
                      summarise(
                        mean_feature = mean(!!sym(feature), na.rm = TRUE),
                        sd_feature = sd(!!sym(feature), na.rm = TRUE),
                        .groups = "drop"
                      )
                    feature_summaries[[feature]] <- feature_by_cluster
                  }
                }
                
                if (length(feature_summaries) > 0) {
                  report$tables$feature_by_cluster <- feature_summaries
                }
              }
            }
            
            # ===== REPORT METADATA =====
            report$metadata <- list(
              r_version = R.version.string,
              package_version = "MultiConnector 1.0", # Update as needed
              analysis_parameters = list(
                include_dimension_analysis = include_dimension_analysis,
                include_cluster_analysis = include_cluster_analysis,
                features_analyzed = features
              )
            )
            
            cat("Report generation completed!\n")
            cat(paste("Report contains:", length(report$plots), "plots and", length(report$tables), "tables\n"))
            
            # Return based on output format
            if (output_format == "list") {
              return(report)
            } else {
              # Future implementation for HTML/PDF export
              warning("HTML and PDF output formats not yet implemented. Returning list format.")
              return(report)
            }
          })

#' Print method for analysis report
#'
#' @param report Report object generated by generateReport
#' @export
printReportSummary <- function(report) {
  cat("=== MULTICONNECTOR ANALYSIS REPORT ===\n")
  cat("Title:", report$title, "\n")
  cat("Generated on:", as.character(report$generated_on), "\n\n")
  
  # Data summary
  if (!is.null(report$summary$data_overview)) {
    cat("DATA OVERVIEW:\n")
    cat("- Subjects:", report$summary$data_overview$n_subjects, "\n")
    cat("- Measures:", report$summary$data_overview$n_measures, "\n")
    cat("- Time points:", report$summary$data_overview$n_timepoints, "\n")
    cat("- Time range:", paste(report$summary$data_overview$time_range, collapse = " to "), "\n")
    cat("- Measures:", paste(report$summary$data_overview$measures, collapse = ", "), "\n\n")
  }
  
  # Clustering summary
  if (!is.null(report$summary$clustering_overview)) {
    cat("CLUSTERING RESULTS:\n")
    cat("- Number of clusters:", report$summary$clustering_overview$n_clusters, "\n")
    cat("- H parameter:", report$summary$clustering_overview$h_parameter, "\n")
    cat("- Cluster names:", paste(report$summary$clustering_overview$cluster_names, collapse = ", "), "\n\n")
  }
  
  # Content summary
  cat("REPORT CONTENTS:\n")
  cat("- Plots:", length(report$plots), "\n")
  cat("- Tables:", length(report$tables), "\n")
  cat("- Analysis sections:", length(report$analysis_choices), "\n\n")
  
  # Available plots
  if (length(report$plots) > 0) {
    cat("AVAILABLE PLOTS:\n")
    for (plot_name in names(report$plots)) {
      cat("-", plot_name, "\n")
    }
    cat("\n")
  }
  
  # Available tables
  if (length(report$tables) > 0) {
    cat("AVAILABLE TABLES:\n")
    for (table_name in names(report$tables)) {
      cat("-", table_name, "\n")
    }
    cat("\n")
  }
  
  cat("Use report$plots$<plot_name> or report$tables$<table_name> to access specific elements.\n")
}
