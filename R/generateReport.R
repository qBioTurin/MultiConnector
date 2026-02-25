#' Generate Analysis Report
#'
#' @description
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
#' @param output_format Output format: "list" (default), "html"
#' @param output_file Output file path for HTML report (optional)
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
                                      output_format = "list",
                                      output_file = NULL) {
  standardGeneric("generateReport")
})

setMethod(
  "generateReport", signature(),
  function(data = NULL,
           clustered_data = NULL,
           report_title = "MultiConnector Analysis Report",
           include_dimension_analysis = TRUE,
           include_cluster_analysis = TRUE,
           features = NULL,
           output_format = "list",
           output_file = NULL) {
    # Validation
    if (is.null(data) && is.null(clustered_data)) {
      stop("At least one of 'data' or 'clustered_data' must be provided")
    }

    # Extract data from clustered object if needed
    if (!is.null(clustered_data)) {
      params <- getParameters(clustered_data)
      measures <- getMeasures(clustered_data)
      cluster_names <- getClusterNames(clustered_data)
    } else if (!is.null(data)) {
      measures <- getMeasures(data)
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
      n_subjects <- length(unique(data@curves$subjID))
      n_measures <- length(measures)
      n_timepoints <- nrow(data@curves)
      time_range <- range(data@curves$time)

      report$summary$data_overview <- list(
        n_subjects = n_subjects,
        n_measures = n_measures,
        n_timepoints = n_timepoints,
        time_range = time_range,
        measures = measures
      )

      # Time series plots
      report$plots$timeseries_plot <- plot(data)

      # Time grid visualization
      report$plots$time_grid_plot <- plotTimes(data)
    } else if (!is.null(clustered_data)) {
      # Extract summary from clustered data
      df <- clustered_data@KData$CData
      report$summary$data_overview <- list(
        n_subjects = length(unique(df$subjID)),
        n_measures = length(measures),
        n_timepoints = nrow(df),
        time_range = range(df$time),
        measures = measures
      )
    }

    # ===== DIMENSION ANALYSIS SECTION =====
    if (include_dimension_analysis && !is.null(data)) {
      cat("- Performing dimension estimation analysis...\n")

      # Dimension estimation
      tryCatch(
        {
          dim_analysis <- estimatepDimension(data, p = 3:10)
          report$plots$dimension_analysis <- dim_analysis
          report$analysis_choices$recommended_dimensions <- "See dimension analysis plot"
        },
        error = function(e) {
          cat("Warning: Dimension analysis failed:", e$message, "\n")
          report$analysis_choices$recommended_dimensions <- "Analysis failed"
        }
      )
    }

    # ===== CLUSTERING ANALYSIS SECTION =====
    if (include_cluster_analysis && !is.null(clustered_data)) {
      cat("- Analyzing clustering results...\n")

      # Clustering summary
      report$summary$clustering_overview <- list(
        n_clusters = params$G,
        h_parameter = params$h,
        cluster_names = cluster_names,
        quality_metrics = clustered_data@TTandfDBandSil
      )

      # Cluster assignments
      clusters_df <- getClusters(clustered_data)
      cluster_sizes <- table(clusters_df$cluster)

      report$tables$cluster_assignments <- data.frame(
        Cluster = names(cluster_sizes),
        Size = as.numeric(cluster_sizes),
        Percentage = round(as.numeric(cluster_sizes) / sum(cluster_sizes) * 100, 2)
      )

      # Basic cluster plot
      report$plots$cluster_plot_basic <- plot(clustered_data)

      # Feature-based cluster plots
      available_features <- getAnnotations(clustered_data)
      if (!is.null(features)) {
        cat("- Generating feature-based visualizations...\n")
        report$plots$cluster_plots_by_feature <- list()

        for (feature in features) {
          if (feature %in% available_features) {
            tryCatch(
              {
                report$plots$cluster_plots_by_feature[[feature]] <-
                  plot(clustered_data, feature = feature)
              },
              error = function(e) {
                cat(paste("Warning: Failed to create plot for feature", feature, ":", e$message, "\n"))
              }
            )
          } else {
            cat(paste("Warning: Feature", feature, "not found in annotations\n"))
          }
        }
      }

      # Discriminant analysis
      tryCatch(
        {
          report$plots$discriminant_plot <- DiscriminantPlot(clustered_data)
        },
        error = function(e) {
          cat("Warning: Discriminant plot failed:", e$message, "\n")
        }
      )

      # Spline plots
      tryCatch(
        {
          cat("- Generating spline plots...\n")
          report$plots$spline_plots <- splinePlot(clustered_data)
        },
        error = function(e) {
          cat("Warning: Spline plots failed:", e$message, "\n")
        }
      )

      # Quality metrics table
      report$tables$quality_metrics <- clustered_data@TTandfDBandSil

      # Analysis choices summary
      report$analysis_choices$clustering <- list(
        number_of_clusters = params$G,
        h_parameter = params$h,
        p_parameter = params$p,
        selection_criteria = "Based on quality metrics"
      )
    }

    # ===== REPORT METADATA =====
    report$metadata <- list(
      r_version = R.version.string,
      package_version = "MultiConnector 1.0",
      analysis_parameters = list(
        include_dimension_analysis = include_dimension_analysis,
        include_cluster_analysis = include_cluster_analysis,
        features_analyzed = features
      )
    )

    cat("Report generation completed!\n")

    # Return based on output format
    if (output_format == "list") {
      return(report)
    } else if (output_format == "html") {
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        stop("The 'rmarkdown' package is required for HTML output. Please install it or use output_format = 'list'.")
      }

      # Find template
      template <- system.file("templates/report_template.Rmd", package = "MultiConnector")

      # Fallback for development mode
      if (template == "") {
        template <- "inst/templates/report_template.Rmd"
      }

      if (!file.exists(template)) {
        stop("Report template not found. Please ensure the package is correctly installed.")
      }

      if (is.null(output_file)) {
        output_file <- paste0(gsub(" ", "_", report_title), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      }

      cat(paste("- Exporting to HTML:", output_file, "...\n"))

      rmarkdown::render(
        input = template,
        output_file = output_file,
        params = list(
          title = report_title,
          generated_on = report$generated_on,
          report_data = report
        ),
        quiet = TRUE
      )

      cat("Download your report at:", normalizePath(output_file), "\n")

      return(report)
    } else {
      warning("Selected output format not fully implemented. Returning list format.")
      return(report)
    }
  }
)

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
