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
CrossLogLikePlot <- estimatepDimension(Data, p=2:6, cores=5)

# Display the results
CrossLogLikePlot

## ----optimal-dimensions-------------------------------------------------------
# Based on the cross-validation results, select optimal dimensions
# These values would typically be chosen based on the plotted results
optimal_p <- c("PB" = 4, "BM" = 4)


