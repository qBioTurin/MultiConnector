# ==============================================================================
# MultiConnector Package - Complete Demo: One-Dimensional Clustering Analysis
# ==============================================================================
# 
# This demo provides a comprehensive step-by-step guide for performing functional
# clustering analysis on one-dimensional time series data using the MultiConnector
# package. We'll use ovarian cancer data as an example to demonstrate the complete
# workflow from data import to final visualization and interpretation.
#
# Dataset: Ovarian cancer cell line growth curves over time
# Goal: Identify distinct growth patterns and relate them to progeny information
#
# ==============================================================================

# ------------------------------------------------------------------------------
# STEP 0: SETUP AND LIBRARY LOADING
# ------------------------------------------------------------------------------

library(dplyr)        # Data manipulation
library(parallel)     # Parallel computing
library(MultiConnector)  # Main clustering package
library(ggplot2)      # Additional plotting (if needed)

# Set up parallel processing
detectCores() -> nworkers
cat("Detected", nworkers, "CPU cores. Will use", nworkers-1, "for parallel processing.\n\n")

# ------------------------------------------------------------------------------
# STEP 1: CREATE CONNECTOR DATA OBJECT
# ------------------------------------------------------------------------------

# Create the main data object for analysis
Data <- ConnectorData("./inst/Data/OvarianCancer/Ovarian_TimeSeries.xlsx",
                      "./inst/Data/OvarianCancer/Ovarian_Annotations.txt")

# ------------------------------------------------------------------------------
# STEP 2: INITIAL DATA EXPLORATION
# ------------------------------------------------------------------------------

# Plot 2.1: Basic time series overview
plot(Data)

# Plot 2.2: Time series colored by progeny feature
plot(Data, feature="Progeny")

# Plot 2.3: Time distribution analysis
plotTimes(Data, large=TRUE)   # Detailed time analysis
plotTimes(Data, large=FALSE)  # Summary time analysis

# ------------------------------------------------------------------------------
# STEP 3: DATA PREPROCESSING (TRUNCATION)
# ------------------------------------------------------------------------------

# This helps identify if we should truncate data at a specific time point
truncatePlot(Data, measure="Ovarian", truncTime=70)

# Based on the truncation plot, applying truncation at time = 70.
# Apply truncation to remove sparse data at later time points
DataTrunc <- truncate(Data, measure="Ovarian", truncTime=70)

# Visualize truncated data
plot(DataTrunc)

# ------------------------------------------------------------------------------
# STEP 4: SPLINE DIMENSION ESTIMATION
# ------------------------------------------------------------------------------

# Estimating optimal spline basis dimension (p parameter)
# This step determines how many spline basis functions to use for curve fitting
# Higher p = more flexible curves, but risk of overfitting
# Lower p = smoother curves, but may miss important features

# Cross-validation to find optimal p
# Test p values from 2 to 6
CrossLogLikePlot <- estimatepDimension(DataTrunc, p=2:6, cores=nworkers-1)

# Display results
print(CrossLogLikePlot)

# Based on cross-validation results:
# - Optimal p = 3 (look for minimum cross-validation error)

# Set optimal p value
optimal_p <- 3

# ------------------------------------------------------------------------------
# STEP 5: CLUSTERING ANALYSIS
# ------------------------------------------------------------------------------

# Estimated time with 7 cores: ~23 seconds

# Perform clustering with multiple G values
# This is the core clustering step - most computationally intensive
clusters <- estimateCluster(DataTrunc, 
                           G = 2:6,           # Test 2-6 clusters
                           p = optimal_p,     # Use optimal spline dimension
                           runs = 50,         # Multiple runs for stability
                           cores = nworkers-1) # Parallel processing

# Plot clustering quality metrics
plot(clusters)

# - fDB (functional Data Depth): Lower is better (more compact clusters)
# - Total Time: Computational cost for each configuration
# - Stability: How consistent results are across runs

# ------------------------------------------------------------------------------
# STEP 6: CLUSTER SELECTION
# ------------------------------------------------------------------------------

# Using G = 4 clusters based on quality metrics
# Selection criterion: MinfDB (minimum functional Data Depth)

# Select the best configuration
# G=4 often provides good balance between complexity and interpretability
ClusterData <- selectCluster(clusters, G=4, "MinfDB")

# ------------------------------------------------------------------------------
# STEP 7: CLUSTER VISUALIZATION AND INTERPRETATION
# ------------------------------------------------------------------------------

# Plot 7.1: Basic cluster visualization
plot(ClusterData)

# Plot 7.2: View cluster assignments with annotations
annotations_summary <- getAnnotations(ClusterData)
print(annotations_summary)

# Plot 7.3: Cluster visualization colored by progeny
plot(ClusterData, feature="Progeny")

# Interpretation notes:
# - Each cluster represents a distinct growth pattern
# - Look for relationships between clusters and progeny information
# - This helps understand biological meaning of discovered patterns

# -----------------------------------------------------------------------------
# STEP 8: CLUSTER VALIDATION
# -----------------------------------------------------------------------------

# Comprehensive cluster validation
Metrics <- validateCluster(ClusterData)

# Display validation plot
# - Silhouette analysis: measures how well samples fit their clusters
# - Entropy analysis: measures uncertainty in cluster assignments
print(Metrics$plot)

# Validation metrics interpretation:
# - High silhouette scores (close to 1): well-separated clusters
# - Low entropy: confident cluster assignments
# - Negative silhouette: potentially misclassified samples

# ------------------------------------------------------------------------------
# STEP 9: ADVANCED VISUALIZATIONS
# ------------------------------------------------------------------------------

# Plot 9.1: Discriminant analysis plots
# This shows clusters in reduced dimensional space
Discr <- DiscriminantPlot(ClusterData)

Discr$ColCluster
Discr$ColFeature

# Plot 9.2: Spline-based cluster representations

splinePlots <- splinePlot(ClusterData)
print(splinePlots$`1`)

# Plot 9.3: Maximum discrimination analysis
MaximumDiscriminationFunction(ClusterData)

