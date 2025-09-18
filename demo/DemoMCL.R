library(MultiConnector)
# Description: This script demonstrates the use of the MultiConnector package for clustering time series data.
TimeSeries = readRDS(system.file("Data/MCL/TimeSeries.rds", package="MultiConnector"))
Annotations = readRDS(system.file("Data/MCL/Annotations.rds", package="MultiConnector"))

# ------------------------------------------------------------------------------
# STEP 1: CREATE CONNECTOR DATA OBJECT
# ------------------------------------------------------------------------------

# Create the main data object for analysis
Data <- ConnectorData( tibble(TimeSeries), tibble(Annotations) )
summary(Data)

# ------------------------------------------------------------------------------
# STEP 2: INITIAL DATA EXPLORATION
# ------------------------------------------------------------------------------

# Plot 2.1: Basic time series overview
plot(Data)
getAnnotations(Data)

# Plot 2.2: TimData# Plot 2.2: Time series colored by progeny feature
plot(Data, feature="TTP")

# Plot 2.3: Time distribution analysis
plotTimes(Data, large=TRUE)   # Detailed time analysis
plotTimes(Data, large=FALSE)  # Summary time analysis


# ------------------------------------------------------------------------------
# STEP 3: SPLINE DIMENSION ESTIMATION
# ------------------------------------------------------------------------------

# Estimating optimal spline basis dimension (p parameter)
# This step determines how many spline basis functions to use for curve fitting
# Higher p = more flexible curves, but risk of overfitting
# Lower p = smoother curves, but may miss important features

# Cross-validation to find optimal p
# Test p values from 2 to 6
CrossLogLikePlot <- estimatepDimension(Data, p=2:6, cores=5)

# Display results
print(CrossLogLikePlot)

# Set optimal p value
optimal_p <- c("PB"= 4, "BM" = 4)

# ------------------------------------------------------------------------------
# STEP 4: CLUSTERING ANALYSIS
# ------------------------------------------------------------------------------

# Estimated time with 5 cores: ~23 seconds

# Perform clustering with multiple G values
# This is the core clustering step - most computationally intensive
clusters <- estimateCluster(Data, 
                            G = 2:6,           # Test 2-6 clusters
                            p = optimal_p,     # Use optimal spline dimension
                            runs = 20,         # Multiple runs for stability
                            cores = 5) # Parallel processing

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

