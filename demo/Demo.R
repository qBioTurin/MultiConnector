#### Loading data to cluster ####

TimeSeries <- readRDS(system.file("Data/Synthetic/TimeSeries.rds", package = "MultiConnector"))
Annotations <- readRDS(system.file("Data/Synthetic/Annotations.RDs", package = "MultiConnector"))
####

Data <- ConnectorData(TimeSeries, Annotations)

plot(Data, feature = "treatment_group")

plotTimes(Data)
plotTimes(Data, large = T)

truncatePlot(Data, measure = "Parabola", truncTime = 5)
# Note: truncate() returns a new object
DataTrunc <- truncate(Data, measure = "Parabola", truncTime = 5)

CrossLogLikePlot <- estimatepDimension(DataTrunc, p = 2:10, cores = 1)
# Access results for a specific measure
if (!is.null(CrossLogLikePlot$Parabola)) {
    print(CrossLogLikePlot$Parabola)
}

# The p parameter now expects a named vector matching measure names
clusters <- estimateCluster(DataTrunc,
    G = 2:6,
    p = c("Cosine" = 3, "Parabola" = 6, "Hyperbola" = 4, "Sine" = 7),
    runs = 10, cores = 4
)
# "Total time: 2.76 mins"
plot(clusters)

# Use individual measures or a single value for p if desired
# clusters <- estimateCluster(DataTrunc, G=2:6, p=3, runs=10, cores=1)

ClusterData <- selectCluster(clusters, G = 3, "MinfDB")
plot(ClusterData)

Metrics <- validateCluster(ClusterData)
print(Metrics$plot)


generateReport(
    data = DataTrunc,
    clustered_data = ClusterData,
    output_format = "html",
    report_title = "reportDemo"
)

###### Classification #######

# Use data from the package
TimeSeriesClassif <- readRDS(system.file("Data/Synthetic/TimeSeries_Classification.RDs", package = "MultiConnector"))
AnnotationsClassif <- readRDS(system.file("Data/Synthetic/Annotations_Classification.RDs", package = "MultiConnector"))

DataNew <- ConnectorData(TimeSeriesClassif, AnnotationsClassif)

# Classification using the selected cluster configuration
# Note: ConfigChosen should be a CONNECTORDataClustered object from selectCluster
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
