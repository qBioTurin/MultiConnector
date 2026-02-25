library(MultiConnector)
library(ggplot2)

# Load synthetic data from inst/Data/Synthetic
# We'll use the local path for now
ts_path <- "inst/Data/Synthetic/TimeSeries.RDs"
ann_path <- "inst/Data/Synthetic/Annotations.RDs"

if (!file.exists(ts_path)) {
    # Fallback for different execution contexts
    ts_path <- system.file("Data/Synthetic/TimeSeries.RDs", package = "MultiConnector")
    ann_path <- system.file("Data/Synthetic/Annotations.RDs", package = "MultiConnector")
}

Measures_TimeSeries <- readRDS(ts_path)
Measures_Annotations <- readRDS(ann_path)

# Create CONNECTORData
Data <- ConnectorData(Measures_TimeSeries, Measures_Annotations)

cat("Original measures:", paste(unique(Data@curves$measureID), collapse = ", "), "\n")

# 1. Truncate ALL measures (default)
cat("\n--- Test 1: Truncate ALL measures (truncTime = 5) ---\n")
DataTruncAll <- truncate(Data, truncTime = 5)
cat("Unique measures in truncated all:", paste(unique(DataTruncAll@curves$measureID), collapse = ", "), "\n")
cat("Max time in truncated all:", max(DataTruncAll@curves$time), "\n")

# 2. Truncate ONLY specific measures
measures_available <- unique(Data@curves$measureID)
if (length(measures_available) >= 2) {
    m_to_trunc <- measures_available[1:2]
    m_to_keep <- measures_available[-(1:2)]

    cat("\n--- Test 2: Truncate ONLY", paste(m_to_trunc, collapse = ", "), "(truncTime = 2) ---\n")
    DataTruncSome <- truncate(Data, measure = m_to_trunc, truncTime = 2)

    cat("Max time for", m_to_trunc[1], ":", max(DataTruncSome@curves$time[DataTruncSome@curves$measureID == m_to_trunc[1]]), "\n")
    if (length(m_to_keep) > 0) {
        cat("Max time for", m_to_keep[1], "(untouched):", max(DataTruncSome@curves$time[DataTruncSome@curves$measureID == m_to_keep[1]]), "\n")
    }
}

# 3. Plot Truncation
cat("\n--- Test 3: Plot Truncation (Visual check) ---\n")
p <- truncatePlot(Data, measure = measures_available[1:2], truncTime = 2)
cat("Is output a ggplot?", inherits(p, "ggplot"), "\n")
