library(MultiConnector)
library(tibble)
# Description: This script demonstrates the use of the MultiConnector package for clustering time series data.
TimeSeries <- readRDS(system.file("Data/MCL/TimeSeries.rds", package = "MultiConnector"))
Annotations <- readRDS(system.file("Data/MCL/Annotations.rds", package = "MultiConnector"))

# ------------------------------------------------------------------------------
# STEP 1: CREATE CONNECTOR DATA OBJECT
# ------------------------------------------------------------------------------


# Create the main data object for analysis
Data <- ConnectorData(tibble(TimeSeries), tibble(Annotations))
summary(Data)
show(Data)

# ------------------------------------------------------------------------------
# STEP 2: INITIAL DATA EXPLORATION
# ------------------------------------------------------------------------------

# Plot 2.1: Basic time series overview
plot(Data)
getAnnotations(Data)

# Plot 2.2: TimData# Plot 2.2: Time series colored by progeny feature
plot(Data, feature = "TTP")

# Plot 2.3: Time distribution analysis
plotTimes(Data, large = TRUE) # Detailed time analysis
plotTimes(Data, large = FALSE) # Summary time analysis


# ------------------------------------------------------------------------------
# STEP 3: SPLINE DIMENSION ESTIMATION
# ------------------------------------------------------------------------------

# Estimating optimal spline basis dimension (p parameter)
# This step determines how many spline basis functions to use for curve fitting
# Higher p = more flexible curves, but risk of overfitting
# Lower p = smoother curves, but may miss important features

# Cross-validation to find optimal p
# Test p values from 2 to 6
# "Total time: 50.05 secs" PB
# "Total time: 1.29 mins" BM

CrossLogLikePlot <- estimatepDimension(Data, p = 2:6, cores = 5)

# Display results
CrossLogLikePlot

# Set optimal p value
optimal_p <- c("PB" = 4, "BM" = 4)

# ------------------------------------------------------------------------------
# STEP 4: CLUSTERING ANALYSIS
# ------------------------------------------------------------------------------
clusters <- estimateCluster(Data,
  G = 2:6, # Test 2-6 clusters
  p = optimal_p, # Use optimal spline dimension
  runs = 10, # Multiple runs for stability
  cores = 5
) # Parallel processing

# Estimated time with 5 cores: ~4 mins
plot(clusters)
# Perform clustering with multiple G values
# This is the core clustering step - most computationally intensive


# Save results for later use
saveRDS(clusters, file = "MCLTwoD_Clustering.rds")


# - fDB (functional Data Depth): Lower is better (more compact clusters)
# - Total Time: Computational cost for each configuration
# - Stability: How consistent results are across runs

# ------------------------------------------------------------------------------
# STEP 6: CLUSTER SELECTION
# ------------------------------------------------------------------------------

# Using G = 3 clusters based on quality metrics
# Selection criterion: MinfDB (minimum functional Data Depth)

# Select the best configuration
# Select the best configuration
if (file.exists("MCLTwoD_Clustering.rds")) {
  clusters <- readRDS("MCLTwoD_Clustering.rds")
}
ClusterData <- selectCluster(clusters, G = 3, "MinfDB")

# ------------------------------------------------------------------------------
# STEP 7: CLUSTER VISUALIZATION AND INTERPRETATION
# ------------------------------------------------------------------------------

# Plot 7.1: Basic cluster visualization
plot(ClusterData)
getAnnotations(ClusterData)

getClusterNames(ClusterData) # Get cluster names and sizes
ClusterData <- setClusterNames(ClusterData, c("High", "Medium", "Low"))
getClusterNames(ClusterData)
plot(ClusterData)

# Plot 7.2: Cluster visualization colored by progeny
plot(ClusterData, feature = "TTP")
plot(ClusterData, feature = "Arm")

clustersLink <- getClusters(ClusterData)
clusterDistribution(ClusterData, feature = "TTP")

info <- SubjectInfo(ClusterData, subjIDs = "Subject 201")
info$cluster_assignment
info$highlighted_plot
info$quality_metrics # entropy/silhouette table
info$subjects_data # subject's time series data

info <- SubjectInfo(ClusterData, subjIDs = c("Subject 201", "Subject 1105"))
info$highlighted_plot
info$quality_metrics

# Multiple subjects with custom colors
info <- SubjectInfo(ClusterData, c("Subject 201", "Subject 1105"),
  colors = c("red", "blue")
)
info$highlighted_plot

# -----------------------------------------------------------------------------
# STEP 8: CLUSTER VALIDATION
# -----------------------------------------------------------------------------

# Comprehensive cluster validation
Metrics <- validateCluster(ClusterData)

# Display validation plot
# - Silhouette analysis: measures how well samples fit their clusters
# - Entropy analysis: measures uncertainty in cluster assignments
Metrics$plot
Metrics$entropy_silhouette_table

# Validation metrics interpretation:
# - High silhouette scores (close to 1): well-separated clusters
# - Low entropy: confident cluster assignments
# - Negative silhouette: potentially misclassified samples

# ------------------------------------------------------------------------------
# STEP 9: ADVANCED VISUALIZATIONS
# ------------------------------------------------------------------------------

# Plot 9.1: Discriminant analysis plots
# This shows clusters in reduced dimensional space
Discr <- DiscriminantPlot(ClusterData, feature = "TTP")

Discr$ColCluster
Discr$ColFeature
# Plot 9.2: Spline-based cluster representations

splinePlots <- splinePlot(ClusterData)
# Print the first subject plot
if (length(splinePlots) > 0) {
  print(splinePlots[[1]])
}

# Plot 9.3: Maximum discrimination analysis
MaximumDiscriminationFunction(ClusterData)

# ------------------------------------------------------------------------------
# STEP 10: ADVANCED CLUSTER ANALYSIS - subclustering "Total time: 10 mins"
# ------------------------------------------------------------------------------
library(dplyr)
getClusters(ClusterData) -> dfClusters
subjIDCL3 <- dfClusters %>%
  filter(cluster == 3) %>%
  pull(subjID)
subData <- ConnectorData(
  tibble(TimeSeries) %>% filter(subjID %in% subjIDCL3),
  tibble(Annotations) %>% filter(subjID %in% subjIDCL3)
)
show(subData)

subClusters <- estimateCluster(subData,
  G = 2:6, # Test 2-6 clusters
  p = optimal_p, # Use optimal spline dimension
  runs = 20, # Multiple runs for stability
  cores = 5
) # Parallel processing
plot(subClusters)
subClusterData <- selectCluster(subClusters, G = 3, "MinfDB")
plot(subClusterData, feature = "TTP")
clusterDistribution(subClusterData, feature = "TTP")

saveRDS(subClusters, file = "MCLTwoD_SubClustering.rds")

#########
library(latex2exp)
library(ggplot2)
library(dplyr)
library(MultiConnector)
if (file.exists("MCLTwoD_SubClustering.rds")) {
  subClusters <- readRDS("MCLTwoD_SubClustering.rds")
}
subClusterData <- selectCluster(subClusters, G = 3, "MinfDB")
plot(subClusterData, feature = "TTP")

if (file.exists("MCLTwoD_Clustering.rds")) {
  clusters <- readRDS("MCLTwoD_Clustering.rds")
}
ClusterData <- selectCluster(clusters, G = 3, "MinfDB")
plot(ClusterData, feature = "TTP")


getClusters(subClusterData) -> subClusters
subClusters %>% dplyr::mutate(cluster = 2 + cluster) -> subClusters
getClustersCentroids(subClusterData) -> submeanC
submeanC %>% mutate(cluster = 2 + as.double(cluster)) -> submeanC

getClusters(ClusterData) -> Clusters
Clusters <- Clusters %>% dplyr::filter(cluster != "3")
Clusters <- rbind(Clusters, subClusters)


saveRDS(list(ClusterData = ClusterData, subClusterData = subClusterData), file = "DemoMCL_ClusterData.rds")
write.csv(Clusters, file = "MultiConnector_MCL_Clusters.csv", row.names = FALSE, quote = F)

######

newCurves <- merge(merge(
  ClusterData@KData$CData %>% select(-jamesID, -timeindex, -curvesID),
  Clusters
), ClusterData@KData$annotations)
merge(Data@curves, Clusters, by = "subjID") -> newCurves

getClustersCentroids(ClusterData) -> meanC
meanC <- rbind(meanC %>% filter(cluster != "3") %>% mutate(cluster = as.numeric(cluster)), submeanC)

saveRDS(
  list(
    data = merge(merge(
      ClusterData@KData$CData %>% select(-jamesID, -timeindex, -curvesID),
      Clusters
    ), ClusterData@KData$annotations),
    meanCurvesClusters = meanC
  ),
  file = "DatiMCLmulti.RDs"
)

newCurves %>% ggplot() +
  geom_line(aes(x = time, y = value, group = subjID), color = "grey", alpha = 0.3) +
  geom_line(data = newCurves %>% dplyr::filter(subjID == "Subject 201"), aes(x = time, y = value, group = subjID, color = subjID)) +
  geom_line(data = meanC, aes(x = time, y = value)) +
  facet_grid(measureID ~ cluster) +
  labs(x = "Time (days)", y = "MRD quantification") +
  theme_bw() +
  scale_color_manual(values = c("Subject 201" = "blue")) +
  theme(
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.x = element_text(size = 14, family = "Times"),
    axis.title.y = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.y = element_text(size = 14, family = "Times"),
    strip.text = element_text(
      color = "black",
      size = 14,
      face = "bold",
      family = "Times"
    ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  scale_y_continuous(
    limits = c(-1, 8),
    breaks = seq(0, 8, 2),
    labels = c("NEG", "POS", TeX("$10^{-3}$"), TeX("$10^{-2}$"), TeX("$10^{-1}$"))
  )

newCurves %>% ggplot() +
  geom_line(aes(x = time, y = value, group = subjID, color = as.factor(cluster)), alpha = 0.3) +
  geom_line(data = meanC, aes(x = time, y = value)) +
  facet_grid(measureID ~ cluster) +
  labs(x = "Time (days)", y = "MRD quantification") +
  theme_bw() +
  theme(
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.x = element_text(size = 14, family = "Times"),
    axis.title.y = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.y = element_text(size = 14, family = "Times"),
    strip.text = element_text(
      color = "black",
      size = 14,
      face = "bold",
      family = "Times"
    ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  scale_y_continuous(
    limits = c(-1, 8),
    breaks = seq(0, 8, 2),
    labels = c("NEG", "POS", TeX("$10^{-3}$"), TeX("$10^{-2}$"), TeX("$10^{-1}$"))
  )

merge(newCurves, Annotations, by = "subjID") %>% ggplot() +
  geom_line(aes(x = time, y = value, group = subjID, color = as.factor(TTP)), alpha = 0.3) +
  scale_color_manual(values = c("1" = "red", "0" = "blue")) +
  geom_line(data = meanC, aes(x = time, y = value)) +
  facet_grid(measureID ~ cluster) +
  labs(x = "Time (days)", y = "MRD quantification", color = "TTP") +
  theme_bw() +
  theme(
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.x = element_text(size = 14, family = "Times"),
    axis.title.y = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.y = element_text(size = 14, family = "Times"),
    strip.text = element_text(
      color = "black",
      size = 14,
      face = "bold",
      family = "Times"
    ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  scale_y_continuous(
    limits = c(-1, 8),
    breaks = seq(0, 8, 2),
    labels = c("NEG", "POS", TeX("$10^{-3}$"), TeX("$10^{-2}$"), TeX("$10^{-1}$"))
  )

merge(newCurves, Annotations, by = "subjID") %>%
  select(cluster, TTP, subjID) %>%
  distinct() %>%
  group_by(cluster, TTP) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = TTP, values_from = n, values_fill = 0)


Dataset <- merge(newCurves, Annotations, by = "subjID") %>%
  select(cluster, TimeTTPevent, TTP, subjID) %>%
  distinct()

library(survminer)
library(survival)

fit <- eval(parse(text = paste0("survfit(Surv(Dataset$TimeTTPevent,Dataset$TTP) ~ cluster, data = Dataset)")))
ggsurv <- ggsurvplot(
  fit = fit, data = Dataset,
  xlab = "Days", ylab = "TTP",
  size = 1, pval = TRUE, risk.table = TRUE, conf.int = F,
  risk.table.col = "strata", ggtheme = theme_bw(), surv.median.line = "hv"
)

ggsurv$plot


TimeSeries %>% ggplot() +
  geom_line(aes(x = time, y = value, group = subjID), alpha = 0.3, color = "grey") +
  geom_line(data = TimeSeries %>% dplyr::filter(subjID == "Subject 201"), aes(x = time, y = value, group = subjID, color = subjID)) +
  facet_wrap(~measureID, scale = "free") +
  labs(x = "Time (days)", y = "MRD quantification") +
  theme_bw() +
  scale_color_manual(values = c("Subject 201" = "blue")) +
  theme(
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.x = element_text(size = 14, family = "Times"),
    axis.title.y = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.y = element_text(size = 14, family = "Times"),
    strip.text = element_text(
      color = "black",
      size = 14,
      face = "bold",
      family = "Times"
    ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  scale_y_continuous(
    limits = c(-1, 8),
    breaks = seq(0, 8, 2),
    labels = c("NEG", "POS", TeX("$10^{-3}$"), TeX("$10^{-2}$"), TeX("$10^{-1}$"))
  )


merge(newCurves, Annotations, by = "subjID") %>% ggplot() +
  geom_line(aes(x = time, y = value, group = subjID, color = as.factor(Arm))) +
  # scale_color_manual(values = c("1" = "red", "0" = "blue")) +
  geom_line(data = meanC, aes(x = time, y = value)) +
  facet_grid(measureID ~ cluster) +
  labs(x = "Time (days)", y = "MRD quantification", color = "Arm") +
  theme_bw() +
  theme(
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.x = element_text(size = 14, family = "Times"),
    axis.title.y = element_text(
      size = 14,
      face = "bold",
      family = "Times"
    ),
    axis.text.y = element_text(size = 14, family = "Times"),
    strip.text = element_text(
      color = "black",
      size = 14,
      face = "bold",
      family = "Times"
    ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  scale_y_continuous(
    limits = c(-1, 8),
    breaks = seq(0, 8, 2),
    labels = c("NEG", "POS", TeX("$10^{-3}$"), TeX("$10^{-2}$"), TeX("$10^{-1}$"))
  )

merge(newCurves, Annotations, by = "subjID") %>%
  select(cluster, Arm, subjID) %>%
  distinct() %>%
  group_by(cluster, Arm) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = Arm, values_from = n, values_fill = 0)
