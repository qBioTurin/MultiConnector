library(MultiConnector)
library(tibble)
library(dplyr)
library(ggplot2)

pkg_path <- system.file(package = "MultiConnector")
if (pkg_path == "") {
    # If not installed, try the local dir
    pkg_path <- "/Users/simonepernice/Desktop/GIT/R_packages_project/MultiConnector/inst"
}

TimeSeries <- readRDS(file.path(pkg_path, "Data/MCL/TimeSeries.rds"))
Annotations <- readRDS(file.path(pkg_path, "Data/MCL/Annotations.rds"))

Data <- ConnectorData(tibble(TimeSeries), tibble(Annotations))

# Test initial exploration
cat("Plot(Data)...\n")
p1 <- plot(Data)
ggplot2::ggplot_build(p1)

cat("Plot(Data, feature='TTP')...\n")
p2 <- plot(Data, feature = "TTP")
ggplot2::ggplot_build(p2)

# Load pre-computed clustering
clusters <- readRDS(file.path(pkg_path, "Data/MCL/MCLTwoD_Clustering.rds"))
cat("Plot(clusters)...\n")
p3 <- plot(clusters)
ggplot2::ggplot_build(p3)

# Select clusters
ClusterData <- selectCluster(clusters, G = 3, "MinfDB")
cat("Plot(ClusterData)...\n")
p4 <- plot(ClusterData)
ggplot2::ggplot_build(p4)

cat("Plot(ClusterData, feature='TTP')...\n")
p5 <- plot(ClusterData, feature = "TTP")
ggplot2::ggplot_build(p5)

# Validation
cat("validateCluster(ClusterData)...\n")
val <- validateCluster(ClusterData)
ggplot2::ggplot_build(val$plot)

# Discriminant
cat("DiscriminantPlot(ClusterData)...\n")
disc <- DiscriminantPlot(ClusterData)
# disc can be a plot or a list
if (is.list(disc) && !inherits(disc, "ggplot")) {
    for (i in seq_along(disc)) {
        if (inherits(disc[[i]], "ggplot")) ggplot2::ggplot_build(disc[[i]])
    }
} else {
    ggplot2::ggplot_build(disc)
}

# Max Discrimination
cat("MaximumDiscriminationFunction(ClusterData)...\n")
max_disc <- MaximumDiscriminationFunction(ClusterData)
ggplot2::ggplot_build(max_disc)

cat("ALL PLOTS VERIFIED SUCCESSFULLY\n")
