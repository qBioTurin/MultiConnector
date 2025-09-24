# MultiConnector


> **⚠️ CONFIDENTIAL - UNPUBLISHED PACKAGE**  
> This package is currently under development and has not been published yet. All content, code, and documentation are confidential and proprietary. Please do not distribute or share without explicit permissresults <- estimateCluster(ovarian_data, G = 2:4, p = 3, runs = 20)
best_model <- selectCluster(results, G = 3, best = "MinfDB")
validation <- validateCluster(best_model)


[![R-CMD-check](https://github.com/qBioTurin/MultiConnector/workflows/R-CMD-check/badge.svg)](https://github.com/qBioTurin/MultiConnector/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/MultiConnector)](https://CRAN.R-project.org/package=MultiConnector)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


## Overview

**MultiConnector** is an R package for functional clustering analysis of multi-dimensional time series data. It implements the James & Sugar (2003) functional clustering model to identify distinct patterns in longitudinal data, making it particularly useful for biomedical research, growth studies, and longitudinal data analysis where curves need to be grouped based on their shape and temporal behavior.

### Key Features

- **Functional Clustering**: Advanced clustering based on curve shapes rather than individual time points
- **Multi-dimensional Support**: Handle multiple measurements per subject simultaneously
- **Spline-based Modeling**: Natural cubic splines capture complex curve patterns
- **Comprehensive Validation**: Built-in quality metrics including silhouette analysis and entropy measures
- **Rich Visualizations**: Specialized plots for cluster exploration, validation, and interpretation
- **Parallel Processing**: Speed up analysis with multi-core support
- **Flexible Data Input**: Support for Excel, CSV files, and R tibbles

## Installation

### From GitHub (Recommended)

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install MultiConnector from GitHub
devtools::install_github("qBioTurin/MultiConnector")
```

### Dependencies

MultiConnector requires R ≥ 4.0.0 and several packages that will be automatically installed:

```r
# Core dependencies
packages <- c("dplyr", "ggplot2", "splines", "Matrix", "parallel", 
              "readxl", "readr", "tibble", "magrittr", "patchwork")
install.packages(packages)
```

## Data Format Requirements

### Time Series File

Your time series data should contain:

- `subjID`: Subject/sample identifier
- `time`: Time points
- `measureID`: Measurement type identifier (for multi-dimensional data)
- `value`: Observed values

### Annotations File
Your annotations should contain:

- `subjID`: Subject identifier (matching time series)

- Additional feature columns (e.g., treatment, gender, outcome)

### Supported Formats

- Excel files (`.xlsx`, `.xls`)
- CSV/text files (`.csv`, `.txt`)
- R tibbles (for programmatic use)

## Terminology and Notation

Understanding the key terms and notation used throughout MultiConnector:

| Term | Definition |
|------|------------|
| **Measure** | A given dimension of the functional data (identified by `measureID`) |
| **Observation** | The discrete sampled curve of a specific measure |
| **Subject** | The connection of the samples among the different measures (identified by `subjID`) |
| **Time** | The time point of a single observation characterizing the set of points in the sample |
| **Functional Data** | The complete set of multiple measures for the same subject (`subjID`) |
| **Feature** | A characteristic or attribute associated with each subject (`subjID`) |
| **Annotations** | The complete set of features associated with each subject |
| **p** | Spline dimension - determines the complexity of curve fitting |
| **G** | Number of clusters to identify in the data |
| **h** | Latent factor dimension - reduces dimensionality in clustering space |

### Data Structure Relationships

```
Subject (subjID) ──┬── Measure 1 (measureID) ──── Observations (time, value)
                   ├── Measure 2 (measureID) ──── Observations (time, value)
                   ├── ...
                   └── Features (annotations)
```

**Example:**
- **Subject**: Patient_001
- **Measures**: Blood pressure, Heart rate, Temperature
- **Observations**: Time series of values for each measure
- **Features**: Age, Gender, Treatment group
- **Functional Data**: All measures combined for Patient_001

## Key Functions

| Function | Purpose |
|----------|---------|
| `ConnectorData()` | Import and prepare data for analysis |
| `estimatepDimension()` | Determine optimal spline dimensions |
| `estimateCluster()` | Perform clustering analysis |
| `selectCluster()` | Choose optimal clustering configuration |
| `validateCluster()` | Assess clustering quality |
| `plot()` | Intelligent plotting dispatch |
| `DiscriminantPlot()` | Discriminant analysis visualization |
| `splinePlot()` | Spline-based curve visualization |
| `SubjectInfo()` | : Detailed subject analysis with cluster highlighting |
| `clusterDistribution()` | : Cross-tabulation of features vs clusters |
| `generateReport()` | : Comprehensive analysis report generation |

## S4 Classes and Methods

MultiConnector is built around two main S4 classes that provide a structured object-oriented approach to functional clustering:

### CONNECTORData Class

The `CONNECTORData` class represents preprocessed time series data ready for clustering analysis.

**Slots:**

- `@curves`: Tibble containing time series data (subjID, measureID, time, value)
- `@dimensions`: Tibble with observation counts per sample
- `@annotations`: Tibble with subject annotations and features
- `@TimeGrids`: List of time grids for each measurement type

**Key Methods:**

- `ConnectorData()`: Constructor method to create objects from files or tibbles
- `plot()`: Visualize time series data, dispatches to `PlotTimeSeries()`
- `show()`: Display basic object summary
- `getAnnotations()`: Extract available feature names

```r
# Create CONNECTORData object
data <- ConnectorData("timeseries.xlsx", "annotations.csv")

# Access slots
head(data@curves)        # Time series data
data@annotations         # Feature annotations
names(data@TimeGrids)    # Available measurements

# Use methods
plot(data, feature = "treatment")
show(data)
getAnnotations(data)     # Lists available features
```

### CONNECTORDataClustered Class

The `CONNECTORDataClustered` class represents the results of clustering analysis with cluster assignments and parameters.

**Slots:**

- `@TTandfDBandSil`: Tibble with quality metrics (TT, fDB, Silhouette, G)
- `@CfitandParameters`: List containing clustering fit and estimated parameters
- `@h`: Latent factor dimension used in clustering
- `@freq`: Frequency of the clustering configuration
- `@cluster.names`: Character vector of cluster labels (e.g., "A", "B", "C")
- `@KData`: List containing original data and preprocessing results

**Key Methods:**

- `plot()`: Visualize clustering results, dispatches to `ClusterPlot()`
- `DiscriminantPlot()`: Create discriminant analysis plots for cluster interpretation
- `validateCluster()`: Compute and plot clustering quality metrics
- `splinePlot()`: Visualize cluster-specific spline representations
- `MaximumDiscriminationFunction()`: Show optimal discrimination weights
- `getAnnotations()`: Extract features with cluster assignments
- `getClusters()`: Extract subjID with cluster assignments
- `SubjectInfo()`: Get detailed information about specific subjects
- `clusterDistribution()`: Analyze feature distribution across clusters
- `getClustersCentroids()`: Get the cluster centroids (mean curves per cluster)

```r
# Create clustered object (from estimateCluster results)
clustered_data <- selectCluster(cluster_results, G = 3, best = "MinfDB")

# Access slots
clustered_data@cluster.names           # Cluster labels
clustered_data@TTandfDBandSil         # Quality metrics
clustered_data@CfitandParameters$pred$class.pred  # Cluster assignments

# Use specialized methods
plot(clustered_data, feature = "treatment")    # Cluster visualization
DiscriminantPlot(clustered_data)               # Discriminant analysis
validateCluster(clustered_data)                # Quality assessment
splinePlot(clustered_data)                     # Spline-based plots
getAnnotations(clustered_data)                 # Features + clusters

# NEW: Advanced analysis methods
SubjectInfo(clustered_data, "subject_123")     # Single subject analysis
SubjectInfo(clustered_data, c("s1", "s2"))     # Multiple subjects
clusterDistribution(clustered_data, "treatment") # Feature distribution table
report <- generateReport(clustered_data = clustered_data) # Comprehensive report
```

### Method Dispatch System

The package uses S4 method dispatch to provide intelligent function behavior based on object class:

| Method | CONNECTORData | CONNECTORDataClustered |
|--------|---------------|------------------------|
| `plot()` | Time series plots via `PlotTimeSeries()` | Cluster plots via `ClusterPlot()` |
| `getAnnotations()` | Lists available feature names | Shows features with cluster assignments |
| `summary()` | Data summary statistics | *(inherited from base)* |

This design ensures that the same function name (`plot()`, `getAnnotations()`) automatically does the right thing based on whether you're working with raw data or clustering results.


## Quick Start

### Basic Workflow

```r
library(MultiConnector)
library(dplyr)

# 1. Load your data
data <- ConnectorData("timeseries.xlsx", "annotations.csv")

# 2. Explore the data
plot(data)
plot(data, feature = "treatment_group")

# 3. Estimate optimal spline dimensions
dimension_results <- estimatepDimension(data, p = 2:8, cores = 2)

# 4. Perform clustering analysis
cluster_results <- estimateCluster(
  data, 
  G = 2:5,           # Test 2-5 clusters
  p = 4,             # Spline dimension from step 3
  runs = 50,         # Multiple runs for stability
  cores = 4          # Parallel processing
)

# 5. Select optimal configuration
final_clusters <- selectCluster(cluster_results, G = 3, best = "MinfDB")

# 6. Visualize results
plot(final_clusters)
plot(final_clusters, feature = "treatment_group")

# 7. Validate clustering quality
validation <- validateCluster(final_clusters)
print(validation$plot)
```

### Example with Built-in Data

```r
# Load example ovarian cancer data
system.file("Data/OvarianCancer/Ovarian_TimeSeries.xlsx", package="MultiConnector") -> time_series_path
system.file("Data/OvarianCancer/Ovarian_Annotations.txt", package="MultiConnector") -> annotations_path

# Create data object
ovarian_data <- ConnectorData(time_series_path, annotations_path)

# Quick visualization
plot(ovarian_data, feature = "Progeny")

# Full analysis pipeline
results <- estimateCluster(ovarian_data, G = 2:4, p = 3, runs = 20)
best_model <- selectCluster(results, G = 3, best = "MinfDB")
validation <- validateCluster(best_model)
```

### Subject-Level Analysis with SubjectInfo()

The `SubjectInfo()` function provides detailed analysis of specific subjects within their clustering context:

```r
# Single subject analysis
subject_analysis <- SubjectInfo(clustered_data, subjIDs = "patient_001")

# Multiple subjects comparison
multi_analysis <- SubjectInfo(clustered_data, 
                             subjIDs = c("patient_001", "patient_045", "patient_123"))

# With feature-based coloring
feature_analysis <- SubjectInfo(clustered_data, 
                               subjIDs = "patient_001",
                               feature = "treatment",
                               feature_type = "discrete")

# Access results
subject_analysis$cluster_assignments    # "Subject patient_001 belongs to Cluster 2"
subject_analysis$highlighted_plot       # Plot with subject highlighted
subject_analysis$quality_metrics        # Silhouette/entropy table
subject_analysis$subjects_data          # Subject's time series data
```

### Cluster Composition Analysis with clusterDistribution()

Analyze how different features are distributed across clusters:

```r
# Basic distribution table
dist_table <- clusterDistribution(clustered_data, "treatment")

# Detailed table with percentages
detailed_table <- clusterDistribution(clustered_data, "age_group",
                                     include_percentages = TRUE,
                                     include_totals = TRUE)

# View results
print(dist_table)
#   treatment  Cluster1  Cluster2  Cluster3  Total
#   Control    25        15        10        50
#   Treatment  20        30        25        75
#   TOTAL      45        45        35        125

# Check table metadata
attr(dist_table, "total_subjects")  # Total number of subjects
attr(dist_table, "missing_values")  # Count of missing values
```

### Comprehensive Reporting with generateReport()

Generate complete analysis reports including all plots and tables:

```r
# Basic report
report <- generateReport(clustered_data = clustered_data)

# Advanced report with features
comprehensive_report <- generateReport(
  data = original_data,                    # Include dimension analysis
  clustered_data = clustered_data,         # Include clustering results
  report_title = "Clinical Trial Analysis",
  features = c("treatment", "age", "gender"),
  include_dimension_analysis = TRUE,
  include_cluster_analysis = TRUE
)

# View report summary
printReportSummary(comprehensive_report)

# Access specific elements
comprehensive_report$plots$cluster_plot_basic      # Basic cluster plot
comprehensive_report$plots$dimension_analysis      # Dimension selection plot
comprehensive_report$tables$cluster_assignments    # Cluster size table
comprehensive_report$tables$quality_metrics        # Silhouette/entropy metrics

# Feature-specific plots
comprehensive_report$plots$cluster_plots_by_feature$treatment
comprehensive_report$plots$cluster_plots_by_feature$age
```

## Advanced Features

### Custom Analysis Parameters

```r
# Advanced clustering with custom parameters
advanced_results <- estimateCluster(
  data = my_data,
  G = 2:6,                    # Cluster range
  p = c("measure1" = 4,       # Different spline dimensions per measure
        "measure2" = 3),
  h = 2,                      # Latent factor dimension
  runs = 100,                 # More runs for stability
  cores = 8,                  # Parallel processing
  seed = 2024                 # Reproducibility
)
```

### Classification of New Samples

```r
# Classify new data using existing model
new_classifications <- ClassificationCurves(
  newdata = new_connector_data,
  CONNECTORDataClustered = final_clusters,
  cores = 4
)
```

### Comprehensive Visualization Suite

```r
# Multiple visualization options
plot(clustered_data)                           # Basic cluster plot
DiscriminantPlot(clustered_data, feature = "treatment")  # Discriminant analysis
splinePlot(clustered_data)                     # Spline visualization
MaximumDiscriminationFunction(clustered_data)  # Discrimination weights
```

## Documentation and Examples

### Comprehensive Demos
- `demo/DemoOvarianCancer.R`: Complete one-dimensional clustering analysis
- `demo/DemoMCL.R`: Two-dimensional clustering example
- `demo/DemoEmoCovid.R`: COVID-19 emotional data analysis

### Vignettes
Access detailed tutorials with:
```r
browseVignettes("MultiConnector")
```

### Help Documentation
```r
# Function-specific help
?ConnectorData
?estimateCluster
?selectCluster

# Package overview
help(package = "MultiConnector")
```

## Citing MultiConnector

If you use MultiConnector in your research, please cite:

```
Soon.....
```

## Authors and Contributors

- **Pernice Simone** - Developer  
- **Sirovich Roberta** - Developer
- **Frattarola Marco** - Developer

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## Interactive Demo Tutorials

Explore comprehensive HTML tutorials with interactive examples and detailed explanations:

### Available Vignettes

**[Complete Guide to One-Dimensional Functional Clustering](vignettes/OneD_Clustering_Guide.html)**
- **Title**: MultiConnector: Complete Guide to One-Dimensional Functional Clustering
- **Subtitle**: Step-by-Step Analysis of Longitudinal Data
- **Dataset**: Ovarian cancer cell growth data analysis

**[Complete Guide to Two-Dimensional Functional Clustering](vignettes/TwoD_Clustering_Guide.html)**
- **Title**: MultiConnector: Complete Guide to Two-Dimensional Functional Clustering
- **Subtitle**: Step-by-Step Analysis of Longitudinal Data
- **Dataset**: MCL MRD data 

### How to Access Demo Tutorials

**Method 1: Direct File Access (Recommended)**
Navigate to the `vignettes/` folder in the GitHub repository and open the HTML files in your browser:
```r
# Clone or download the repository, then open in browser:
# MultiConnector/vignettes/OneD_Clustering_Guide.html
```
or

```r
# Open in browser:
browseVignettes("MultiConnector")
```

**Method 2: R Script Demos**
Run the comprehensive demo scripts available in the `demo/` folder:
```r
# After installing the package, run example demos
demo("DemoOvarianCancer", package = "MultiConnector")  # Complete 1D analysis
```

**Method 3: GitHub Pages (Future)**
Interactive tutorials will be available online once the package is fully published.

## Related Packages

- **cluster**: Classical clustering methods
- **mixtools**: Mixture model clustering
- **fda**: Functional data analysis
- **funclust**: Alternative functional clustering approaches
