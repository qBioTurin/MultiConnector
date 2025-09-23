## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  cache = FALSE,
  comment = "#>"
)

# Set up better figure output for PDF
if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(
    fig.pos = "H",
    out.extra = ""
  )
}

## ----libraries, message=FALSE, warning=FALSE----------------------------------
# Load required libraries
library(dplyr)           # Data manipulation
library(parallel)        # Parallel computing
library(MultiConnector)  # Main clustering package
library(ggplot2)         # Enhanced plotting
library(knitr)           # Table formatting
library(kableExtra)      # Enhanced table styling

# Set up parallel processing
n_cores <- detectCores()
workers <- max(1, n_cores - 1)  # Leave one core free

cat("System Information:\n")
cat("- Available CPU cores:", n_cores, "\n")
cat("- Cores used for analysis:", workers, "\n")

## ----create-data-object-------------------------------------------------------
system.file("Data/OvarianCancer/Ovarian_TimeSeries.xlsx", package="MultiConnector") -> time_series_path
system.file("Data/OvarianCancer/Ovarian_Annotations.txt", package="MultiConnector") -> annotations_path
# Create the main data object
Data <- ConnectorData(time_series_path,annotations_path)


## ----initial-plots, fig.cap="Initial data exploration: (A) All growth curves overlaid, (B) Curves colored by progeny type.", fig.width=12, fig.height=8----
# Plot 1: Basic time series overview
p1 <- plot(Data) + 
  ggtitle("A) All Growth Curves") +
  theme_minimal()

# Plot 2: Colored by progeny feature
p2 <- plot(Data, feature = "Progeny") + 
  ggtitle("B) Curves by Progeny Type") +
  theme_minimal()

# Combine plots if possible (requires gridExtra or patchwork)
if (requireNamespace("gridExtra", quietly = TRUE)) {
  gridExtra::grid.arrange(p1, p2, ncol = 2)
} else {
  print(p1)
  print(p2)
}

## ----time-analysis, fig.cap="Time point distribution analysis showing data density across the measurement period."----
# Analyze time point distributions
plotTimes(Data, large = TRUE)

## ----truncation-analysis, fig.cap="Truncation analysis helping to identify optimal cutoff time for maintaining data quality."----
# Analyze truncation effects
truncatePlot(Data, measure = "Ovarian", truncTime = 70)

## ----apply-truncation, fig.cap="Data after truncation at time = 70, showing improved data density."----
# Apply truncation based on analysis
DataTrunc <- truncate(Data, measure = "Ovarian", truncTime = 70)

# Visualize truncated data
plot(DataTrunc) + 
  ggtitle("Growth Curves After Truncation (t ≤ 70)") +
  theme_minimal()

## ----spline-estimation, fig.cap="Cross-validation results for spline dimension selection showing optimal p value."----
# Estimate optimal spline dimension
CrossLogLikePlot <- estimatepDimension(DataTrunc, p = 2:6, cores = workers)

# Display results
print(CrossLogLikePlot)

# Set optimal value (typically where CV error is minimized)
optimal_p <- 3
cat("Selected optimal p =", optimal_p, "\n")

## ----cluster-selection--------------------------------------------------------
# Select optimal clustering (G=4 based on quality metrics)
ClusterData <- selectCluster(clusters, G = 4, "MinfDB")


## ----cluster-plots, fig.cap="Cluster visualization: (A) Growth curves colored by cluster assignment, (B) Curves colored by progeny type to examine biological associations.", fig.width=12, fig.height=8----
# Plot clusters
p1 <- plot(ClusterData) + 
  ggtitle("A) Clusters by Assignment") +
  theme_minimal()

# Plot by progeny feature
p2 <- plot(ClusterData, feature = "Progeny") + 
  ggtitle("B) Clusters by Progeny Type") +
  theme_minimal()

# Display plots
if (requireNamespace("gridExtra", quietly = TRUE)) {
  gridExtra::grid.arrange(p1, p2, ncol = 2)
} else {
  print(p1)
  print(p2)
}

## ----cluster-annotations------------------------------------------------------
# Examine cluster-annotation relationships
annotations_summary <- getAnnotations(ClusterData)
print(annotations_summary)

# Create summary table if annotations exist
if (exists("annotations_summary") && length(annotations_summary) > 0) {
  kable(annotations_summary, 
        caption = "Cluster-annotation summary showing the distribution of features across clusters.") %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
}

## ----validation, fig.cap="Cluster validation metrics: (A) Silhouette analysis showing how well samples fit their clusters, (B) Entropy analysis measuring assignment uncertainty."----
# Perform validation analysis
Metrics <- validateCluster(ClusterData)

# Display validation plots
print(Metrics$plot)

## ----discriminant-plots, fig.cap="Discriminant analysis plots showing cluster separation in reduced dimensional space."----
# Generate discriminant plots
Discr <- DiscriminantPlot(ClusterData)

# Display cluster-colored plot
print(Discr$ColCluster)

# Display feature-colored plot (if features exist)
if ("ColFeature" %in% names(Discr)) {
  print(Discr$ColFeature)
}

## ----spline-plots, fig.cap="Spline-based visualization showing the characteristic curve shape for each cluster."----
# Generate spline plots
splinePlots <- splinePlot(ClusterData)

# Display the main spline plot
if (length(splinePlots) > 0) {
  print(splinePlots[[1]])
}

## ----discrimination-analysis--------------------------------------------------
# Identify most discriminative features
MaximumDiscriminationFunction(ClusterData)

## ----subject-analysis, fig.width=12, fig.height=8-----------------------------
# Select some subjects for detailed analysis
selected_subjects <- unique(DataTrunc@annotations$subjID)[1:3]

# Single subject analysis
subject_info <- SubjectInfo(ClusterData, subjIDs = selected_subjects[1])

# Display cluster assignment
cat("Cluster Assignment:")
cat(subject_info$cluster_assignments)

# Show the highlighted plot
subject_info$highlighted_plot

# Multiple subjects comparison
multi_subject_info <- SubjectInfo(ClusterData, subjIDs = selected_subjects)
multi_subject_info$highlighted_plot


## ----cluster-distribution-----------------------------------------------------

progeny_dist <- clusterDistribution(ClusterData, "Progeny")
  
cat("Progeny Distribution Across Clusters:")
print(progeny_dist)

# If other features are available, analyze them too
available_features <- getAnnotations(ClusterData)
cat("\nAvailable features for distribution analysis:")
print(available_features)

# Example with any numeric feature converted to categories
if (length(available_features) > 1) {
  # Take the first available feature for demonstration
  demo_feature <- available_features[1]
  if (demo_feature != "Progeny") {  # Avoid duplicate analysis
    tryCatch({
      feature_dist <- clusterDistribution(ClusterData, demo_feature)
      cat("\nDistribution of", demo_feature, "across clusters:")
      print(feature_dist)
    }, error = function(e) {
      cat("\nCannot analyze feature", demo_feature, ":", e$message)
    })
  }
}

## ----session-info-------------------------------------------------------------
sessionInfo()

