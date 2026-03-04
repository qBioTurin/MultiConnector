#' Generate Analysis Report
#'
#' @description
#'
#'  Generate a comprehensive analysis report including all base plots and analysis choices
#'  for both CONNECTORData and CONNECTORDataClustered objects
#'
#' @param data CONNECTORData object (optional if clustered_data is provided)
#' @param clustered_data CONNECTORDataClustered object (optional if data is provided, output from selectCluster)
#' @param p_analysis List of dimension analysis plots (output from estimatepDimension)
#' @param G_analysis Cluster estimation results (output from estimateCluster)
#' @param report_title Title for the report
#' @param include_spline Include spline plots for each subjID (default: FALSE)
#' @param features Vector of feature names to analyze (optional)
#' @param save_list = T, whether to save the list of data passed as input for the report (default: TRUE)
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
                                      p_analysis = NULL,
                                      G_analysis = NULL,
                                      features = NULL,
                                      include_spline = FALSE,
                                      save_list = T,
                                      output_file = "report.html") {
  standardGeneric("generateReport")
})

#' @rdname generateReport
#' @export
setMethod(
  "generateReport", signature(),
  function(data = NULL,
           clustered_data = NULL,
           report_title = "MultiConnector Analysis Report",
           p_analysis = NULL,
           G_analysis = NULL,
           features = NULL,
           include_spline = FALSE,
           save_list = T,
           output_file = "report.html") {
    # Ensure features is a character vector
    if (!is.null(features)) {
      features <- as.character(features)
    }

    # Handle clustered_data as a list for multiple models
    clust_list <- clustered_data
    if (!is.null(clust_list)) {
      if (!is.list(clust_list) || is(clust_list, "CONNECTORDataClustered")) {
        names(clust_list) <- "Model 1"
      }else{
        names(clust_list) <- paste("Model", seq_along(clust_list))
      }
    }

    # Extract data from clustered object if needed to initialize measures
    if (!is.null(data)) {
      measures <- getMeasures(data)
    } else if (!is.null(clust_list)) {
      measures <- getMeasures(clust_list[[1]])
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

    cat("Generating MultiConnector analysis report...\n")

    # ===== DATA SUMMARY SECTION =====
    if (!is.null(data)) {
      cat("- Processing base data characteristics...\n")

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

      # Base data feature-based plots
      if (!is.null(features)) {
        report$plots$timeseries_plots_by_feature <- list()
        available_features_base <- getAnnotations(data)

        for (feature in features) {
          if (feature %in% available_features_base) {
            tryCatch(
              {
                report$plots$timeseries_plots_by_feature[[feature]] <-
                  plot(data, feature = feature)
              },
              error = function(e) {
                cat(paste("Warning: Failed to create base plot for feature", feature, ":", e$message, "\n"))
              }
            )
          }
        }
      }
    } else if (!is.null(clust_list)) {
      # Extract summary from the first clustered data if global data not provided
      df <- clust_list[[1]]@KData$CData
      report$summary$data_overview <- list(
        n_subjects = length(unique(df$subjID)),
        n_measures = length(measures),
        n_timepoints = nrow(df),
        time_range = range(df$time),
        measures = measures
      )
    }

    # ===== DIMENSION ANALYSIS SECTION =====
    if (!is.null(p_analysis)) {
      cat("- Including provided dimension analysis results...\n")
      report$plots$dimension_analysis <- p_analysis
    }

    # ===== CLUSTERING ANALYSIS SECTION =====
    if (!is.null(G_analysis)) {
      cat("- Including provided cluster estimation results...\n")
      # Use IndexPlotExtrapolation to show quality metrics
      report$plots$cluster_estimation_plot <- plot(G_analysis)
    }

    # ===== CLUSTERING ANALYSIS SECTION (MULTIPLE MODELS) =====
    if (!is.null(clust_list)) {
      report$clustering_results <- list()

      for (model_name in names(clust_list)) {
        cat(paste("- Analyzing clustering results for", model_name, "...\n"))

        obj <- clust_list[[model_name]]
        model_params <- getParameters(obj)
        cluster_names <- getClusterNames(obj)
        available_features <- getAnnotations(obj)

        model_report <- list(
          name = model_name,
          summary = list(
            n_clusters = model_params$G,
            h_parameter = model_params$h,
            p_parameter = model_params$p,
            cluster_names = cluster_names,
            quality_metrics = obj@TTandfDBandSil
          ),
          tables = list(),
          plots = list()
        )

        # Cluster assignments
        clusters_df <- getClusters(obj)
        cluster_sizes <- table(clusters_df$cluster)

        model_report$tables$cluster_assignments <- data.frame(
          Cluster = names(cluster_sizes),
          Size = as.numeric(cluster_sizes),
          Percentage = round(as.numeric(cluster_sizes) / sum(cluster_sizes) * 100, 2)
        )

        # Basic cluster plot
        model_report$plots$cluster_plot_basic <- plot(obj)

        # Feature-based cluster plots
        if (!is.null(features)) {
          model_report$plots$cluster_plots_by_feature <- list()
          for (feature in features) {
            if (feature %in% available_features) {
              tryCatch(
                {
                  model_report$plots$cluster_plots_by_feature[[feature]] <- plot(obj, feature = feature)
                  model_report$tables[[feature]] <- clusterDistribution(obj, feature = feature)
                },
                error = function(e) {
                  cat(paste("  Warning: Failed to create plot for feature", feature, "in", model_name, ":", e$message, "\n"))
                }
              )
            }
          }
        }

        # Validation analysis
        tryCatch(
          {
            model_report$plots$validation_plot <- validateCluster(obj)$plot
          },
          error = function(e) {
            cat(paste("  Warning: Validation plot failed for", model_name, ":", e$message, "\n"))
          }
        )

        # Discriminant analysis
        tryCatch(
          {
            model_report$plots$discriminant_plot <- DiscriminantPlot(obj)
          },
          error = function(e) {
            cat(paste("  Warning: Discriminant plot failed for", model_name, ":", e$message, "\n"))
          }
        )

        # Maximum discrimination analysis
        tryCatch(
          {
            MaximumD <- MaximumDiscriminationFunction(obj)
            model_report$plots$maxdiscriminant_plot <- MaximumD$Separated
            model_report$tables$discriminant_areas <- MaximumD$measure_areas
          },
          error = function(e) {
            cat(paste("  Warning: Maximum discrimination analysis failed for", model_name, ":", e$message, "\n"))
          }
        )

        # Spline plots
        if (include_spline) {
          tryCatch(
            {
              full_spline_list <- splinePlot(obj)
              model_report$plots$spline_plots <- full_spline_list
            },
            error = function(e) {
              cat(paste("  Warning: Spline plots failed for", model_name, ":", e$message, "\n"))
            }
          )
        }

        # Save results to main report
        report$clustering_results[[model_name]] <- model_report
      }

      # For backward compatibility with older templates (optional, can be removed if template updated)
      # report$summary$clustering_overview <- report$clustering_results[[1]]$summary
      # report$tables$cluster_assignments <- report$clustering_results[[1]]$tables$cluster_assignments
      # report$plots$cluster_plot_basic <- report$clustering_results[[1]]$plots$cluster_plot_basic
      # report$plots$cluster_plots_by_feature <- report$clustering_results[[1]]$plots$cluster_plots_by_feature
    }

    # ===== REPORT METADATA =====
    report$metadata <- list(
      r_version = R.version.string,
      package_version = "MultiConnector 1.0",
      analysis_parameters = list(
        features_analyzed = features
      )
    )

    cat("Report generation completed!\n")


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

    # Ensure output_file is absolute to avoid rmarkdown::render saving in the template directory
    if (!grepl("^/", output_file) && !grepl("^[A-Za-z]:", output_file)) {
      output_file <- file.path(getwd(), output_file)
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
    if (save_list) {
      saveRDS(
        list(
          report = report,
          inputdata = list(
            data = data,
            clustered_data = clustered_data,
            p_analysis = p_analysis,
            G_analysis = G_analysis
          )
        ),
        file = sub("\\.html$", ".rds", output_file)
      )
    }

    cat("Download your report at:", normalizePath(output_file), "\n")
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
  if (!is.null(report$clustering_results) && length(report$clustering_results) > 0) {
    cat("CLUSTERING ANALYSIS:\n")
    for (name in names(report$clustering_results)) {
      res <- report$clustering_results[[name]]
      cat("- Model:", name, "\n")
      cat("  * Clusters (G):", res$summary$n_clusters, "\n")
      cat("  * H parameter:", res$summary$h_parameter, "\n")
      cat("  * Cluster names:", paste(res$summary$cluster_names, collapse = ", "), "\n")
    }
    cat("\n")
  }

  # Content summary
  cat("REPORT CONTENTS:\n")
  cat("- Global plots:", length(report$plots), "\n")
  cat("- Global tables:", length(report$tables), "\n")
  if (!is.null(report$clustering_results)) {
    cat("- Clustering models:", length(report$clustering_results), "\n")
  }
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
