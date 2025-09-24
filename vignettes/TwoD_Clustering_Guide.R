## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  cache = FALSE,
  comment = "#>",
  fig.width = 12,
  fig.height = 8
)

# Set up better figure output for PDF
if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(
    fig.pos = "H",
    out.extra = ""
  )
}

## ----package-setup------------------------------------------------------------
# Load required packages
library(MultiConnector)
library(tibble)
library(dplyr)
library(ggplot2)

# Load the MCL dataset
# This dataset contains two-dimensional time series data
TimeSeries <- readRDS(system.file("Data/MCL/TimeSeries.rds", package="MultiConnector"))
Annotations <- readRDS(system.file("Data/MCL/Annotations.rds", package="MultiConnector"))

# Display basic dataset information
cat("Dataset Overview:")
cat("\n- Time series data dimensions:", dim(TimeSeries))
cat("\n- Annotations dimensions:", dim(Annotations))
cat("\n- Unique subjects:", length(unique(TimeSeries$subjID)))
cat("\n- Measurement types:", paste(unique(TimeSeries$measureID), collapse = ", "))
cat("\n- Time range:", range(TimeSeries$time))

## ----data-creation------------------------------------------------------------
# Create the main data object for two-dimensional analysis
Data <- ConnectorData(tibble(TimeSeries), tibble(Annotations))

# Display comprehensive summary
summary(Data)
show(Data)


## ----basic-visualization, fig.width=14, fig.height=8--------------------------
# Plot 2.1: Overview of all time series (both measurements)
plot(Data)

## ----feature-exploration, fig.width=14, fig.height=10-------------------------
# Examine available features
available_features <- getAnnotations(Data)
cat("Available features for analysis:")
print(available_features)

# Plot 2.2: Time series colored by key features
# This helps identify potential relationships between features and temporal patterns

# TTP (Time To Progression) analysis
plot(Data, feature="TTP")

# Treatment arm analysis  
plot(Data, feature="Arm")

## ----temporal-analysis, fig.width=14, fig.height=6----------------------------
# Plot 2.3: Detailed time distribution analysis
# This is crucial for two-dimensional data to understand sampling patterns

# Comprehensive time analysis
plotTimes(Data, large=TRUE)

# Summary time analysis
plotTimes(Data, large=FALSE)

## ----dimension-estimation, fig.width=14, fig.height=10------------------------
# Cross-validation for optimal spline basis dimensions
# This step is crucial as each measurement type may require different flexibility

cat("Estimating optimal spline dimensions...")
cat("\nThis process may take several minutes for two-dimensional data...")

# Estimate dimensions for both measurements
# Testing p values from 2 to 6 for both PB and BM
file_crosslog = system.file("Data/MCL/MCLTwoD_CrossLog.rds", package="MultiConnector")
if(file.exists(file_crosslog)){
  CrossLogLikePlot = readRDS(file_crosslog)
} else {
CrossLogLikePlot <- estimatepDimension(Data, p=2:6, cores=5)
saveRDS(CrossLogLikePlot, paste0(system.file("Data/MCL/", package="MultiConnector"), "MCLTwoD_CrossLog.rds" ) )
}

# Display the results
CrossLogLikePlot

## ----optimal-dimensions-------------------------------------------------------
# Based on the cross-validation results, select optimal dimensions
# These values would typically be chosen based on the plotted results
optimal_p <- c("PB" = 4, "BM" = 4)


## ----clustering-analysis------------------------------------------------------

file_cluster = system.file("Data/MCL/MCLTwoD_Clustering.rds", package="MultiConnector")
if(file.exists(file_cluster)){
  clusters = readRDS(file_cluster)
} else {
  # Perform clustering with multiple cluster numbers
# This is computationally intensive for two-dimensional data
clusters <- estimateCluster(Data, 
                           G = 2:6,           # Test 2-6 clusters
                           p = optimal_p,     # Use optimal spline dimensions for both measurements
                           runs = 20,         # Multiple runs for stability
                           cores = 5)         # Parallel processing

saveRDS(clusters, paste0(system.file("Data/MCL/", package="MultiConnector"), "MCLTwoD_Clustering.rds" ) )
}


## ----clustering-results, fig.width=14, fig.height=8---------------------------
# Plot clustering quality metrics
plot(clusters)

## ----cluster-selection--------------------------------------------------------
# Select optimal configuration based on quality metrics
# For demonstration, using G=3 clusters with MinfDB criterion

optimal_G <- 3
selection_criterion <- "MinfDB"

cat("Cluster selection:")
cat("\n- Number of clusters (G):", optimal_G)
cat("\n- Selection criterion:", selection_criterion)
cat("\n- Rationale: Minimum functional Davies-Bouldin index")

# Create the final clustered data object
ClusterData <- selectCluster(clusters, G=optimal_G, selection_criterion)

cat("\n\nFinal clustering solution:")
cat("\n- Clusters identified:", length(unique(ClusterData@cluster.names)))
cat("\n- Cluster labels:", paste(ClusterData@cluster.names, collapse = ", "))

## ----basic-cluster-viz, fig.width=14, fig.height=10---------------------------
# Plot 6.1: Basic cluster visualization across both measurements
plot(ClusterData)

## ----feature-cluster-viz, fig.width=14, fig.height=10-------------------------
# Plot 6.2: Clusters colored by clinical features
# This reveals associations between clusters and clinical characteristics

# TTP (Time To Progression) patterns
plot(ClusterData, feature="TTP")

# Treatment arm associations
plot(ClusterData, feature="Arm")

## ----cluster-composition------------------------------------------------------
# Analyze cluster composition across features
available_features <- getAnnotations(ClusterData)
cat("Features available for composition analysis:")
print(available_features)

# Detailed composition analysis for key features
cat("\n=== CLUSTER COMPOSITION ANALYSIS ===")

# TTP distribution across clusters
if ("TTP" %in% available_features) {
  cat("\n\n1. TTP Distribution Across Clusters:")
  ttp_dist <- clusterDistribution(ClusterData, feature="TTP")
  print(ttp_dist)
}

# Treatment arm distribution
if ("Arm" %in% available_features) {
  cat("\n\n2. Treatment Arm Distribution:")
  arm_dist <- clusterDistribution(ClusterData, feature="Arm")
  print(arm_dist)
}

# Display cluster assignments
cluster_assignments <- getClusters(ClusterData)
cat("\n\n3. Cluster Assignment Summary:")
cat("\n- Total subjects clustered:", nrow(cluster_assignments))
cluster_summary <- table(cluster_assignments$Cluster)
for (i in names(cluster_summary)) {
  cat("\n- Cluster", i, ":", cluster_summary[i], "subjects")
}

## ----single-subject-analysis, fig.width=14, fig.height=10---------------------
# Select a representative subject for detailed analysis
example_subjects <- unique(cluster_assignments$subjID)[1:3]
selected_subject <- example_subjects[1]

cat("Detailed analysis for subject:", selected_subject)

# Comprehensive subject information
subject_info <- SubjectInfo(ClusterData, subjIDs = selected_subject)

cat("\nSubject cluster assignment:")
cat("\n", subject_info$cluster_assignments)

# Display highlighted visualization
subject_info$highlighted_plot

## ----multi-subject-analysis, fig.width=14, fig.height=10----------------------
# Compare multiple subjects across clusters
multi_subject_info <- SubjectInfo(ClusterData, subjIDs = example_subjects)

cat("Multi-subject comparison:")
cat("\nSubjects analyzed:", paste(example_subjects, collapse = ", "))

# Show cluster assignments
for (i in 1:nrow(multi_subject_info$cluster_table)) {
  cat("\n- Subject", multi_subject_info$cluster_table$subjID[i], 
      "-> Cluster", multi_subject_info$cluster_table$cluster[i])
}

# Display comparative visualization
multi_subject_info$highlighted_plot

## ----cluster-validation, fig.width=14, fig.height=10--------------------------
# Comprehensive cluster validation
cat("Performing cluster validation...")

validation_metrics <- validateCluster(ClusterData)

# Display validation plot
validation_metrics$plot


## ----validation-summary-------------------------------------------------------
# Extract and summarize validation metrics
if (!is.null(validation_metrics$metrics)) {
  cat("\n\nValidation Summary:")
  print(validation_metrics$metrics)
}

# Additional quality assessment
quality_metrics <- ClusterData@TTandfDBandSil
cat("\n\nClustering Quality Metrics:")
print(quality_metrics)

## ----discriminant-analysis, fig.width=14, fig.height=10-----------------------
# Discriminant analysis visualization
# This projects the high-dimensional functional data into lower-dimensional space

cat("Generating discriminant analysis plots...")

# Basic discriminant plot
discr_basic <- DiscriminantPlot(ClusterData)
discr_basic$ColCluster

# Feature-enhanced discriminant plot
if ("TTP" %in% available_features) {
  discr_feature <- DiscriminantPlot(ClusterData, feature = "TTP")
  discr_feature$ColFeature
}

## ----max-discrimination, fig.width=14, fig.height=8---------------------------
# Maximum discrimination function analysis
# Identifies the most discriminative aspects of the functional data

max_discr <- MaximumDiscriminationFunction(ClusterData)
max_discr

## ----subclustering-setup------------------------------------------------------

# Get cluster assignments
cluster_assignments <- getClusters(ClusterData)

# Select subjects from cluster 3
cluster3_subjects <- cluster_assignments %>% 
  filter(Cluster == 3) %>% 
  pull(subjID)

cat("\n- Subjects in Cluster 3:", length(cluster3_subjects))
cat("\n- First few subjects:", paste(head(cluster3_subjects), collapse = ", "))

# Create subset data for subclustering
subData <- ConnectorData(
  tibble(TimeSeries) %>% filter(subjID %in% cluster3_subjects), 
  tibble(Annotations) %>% filter(subjID %in% cluster3_subjects)
)

cat("\n\nSubset data for subclustering:")
show(subData)

## ----subclustering-analysis---------------------------------------------------

file_cluster = system.file("Data/MCL/MCLTwoD_SubClustering.rds", package="MultiConnector")
if(file.exists(file_cluster)){
  subClusters = readRDS(file_cluster)
} else {
# Subcluster analysis within Cluster 3
subClusters <- estimateCluster(subData, 
                              G = 2:5,           # Test fewer clusters for subset
                              p = optimal_p,     # Use same spline dimensions
                              runs = 20,         # Multiple runs for stability
                              cores = 5)

saveRDS(clusters, paste0(system.file("Data/MCL/", package="MultiConnector"), "MCLTwoD_SubClustering.rds" ) )
}

# Plot subclustering results
plot(subClusters)

# Select optimal subclustering
subClusterData <- selectCluster(subClusters, G=3, "MinfDB")


## ----subclustering-visualization, fig.width=14, fig.height=10-----------------


# Basic subcluster visualization
plot(subClusterData, feature = "TTP")

# Subcluster composition
if ("TTP" %in% getAnnotations(subClusterData)) {
  subcluster_dist <- clusterDistribution(subClusterData, feature="TTP")
  cat("\n\nSubcluster composition (TTP):")
  print(subcluster_dist)
}

## ----session-info-------------------------------------------------------------
sessionInfo()

