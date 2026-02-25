#### Loading data to cluster ####

# Use data from the package for portability
TimeSeries <- readRDS(system.file("Data/PDX/TimeSeriesFiltered.RDs", package = "MultiConnector"))
Annotations <- readRDS(system.file("Data/PDX/AnnotationsFiltered.RDs", package = "MultiConnector"))

####

# Initialize ConnectorData using the new constructor
Data <- ConnectorData(TimeSeries, Annotations)

# Initial visualization
plot(Data, feature = "LongID") + ggplot2::theme(legend.position = "none")

# Time point visualization
plotTimes(Data)
plotTimes(Data, large = TRUE)

# Truncation analysis (if needed)
# truncatePlot(Data, measure = "PDX", truncTime = 5)
# DataTrunc <- truncate(Data, measure = "PDX", truncTime = 5)

# Estimate spline dimension (formerly BasisDimensionChoice)
# Using 1 core for compatibility, increase as needed
CrossLogLikePlot <- estimatepDimension(Data, p = 2:6, cores = 1)
print(CrossLogLikePlot)

# Clustering analysis (formerly ClusterAnalysis)
clusters <- estimateCluster(Data, G = 2:6, p = 4, runs = 10, cores = 1)
plot(clusters)

# Select optimal cluster (formerly ConfigSelection)
ClusterData <- selectCluster(clusters, G = 2, "MinfDB")
plot(ClusterData)

# Cluster validation (formerly SilEntropy)
Metrics <- validateCluster(ClusterData)
print(Metrics$plot)

# Discriminant analysis
Discr <- DiscriminantPlot(ClusterData, feature = "LongID")
if (!is.null(Discr$ColCluster)) print(Discr$ColCluster)

# Spline plots
splinePlots <- splinePlot(ClusterData)
if (length(splinePlots) > 0) print(splinePlots[[1]])

# Maximum discrimination
MaximumDiscriminationFunction(ClusterData)


###### Classification #######

# Use synthetic data from the package for demonstration
TimeSeriesClassif <- readRDS(system.file("Data/Synthetic/TimeSeries_Classification.RDs", package = "MultiConnector"))
AnnotationsClassif <- readRDS(system.file("Data/Synthetic/Annotations_Classification.RDs", package = "MultiConnector"))

DataNew <- ConnectorData(TimeSeriesClassif, AnnotationsClassif)

# Classification using the selected cluster configuration
ClassNew <- ClassificationCurves(
    newdata = DataNew,
    ConfigChosen = ClusterData,
    Cores = 1,
    entropyCutoff = 1, probCutoff = 0.6
)

# Access results for a specific subject
if (length(ClassNew$ListClassID) > 0) {
    print(ClassNew$ListClassID[[1]])
}
