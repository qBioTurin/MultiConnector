# library(MultiConnector)
devtools::load_all()
library(dplyr)

# Load synthetic data
ts_path <- "inst/Data/Synthetic/TimeSeries.RDs"
ann_path <- "inst/Data/Synthetic/Annotations.RDs"
TimeSeries <- readRDS(ts_path)
Annotations <- readRDS(ann_path)

# Create CONNECTORData
Data <- ConnectorData(TimeSeries, Annotations)

cat("Original subjects:", length(unique(Data@curves$subjID)), "\n")
cat("Original data points summary:\n")
print(Data@curves %>% group_by(subjID, measureID) %>% summarise(n = n(), .groups = "drop") %>% summarise(min_n = min(n), max_n = max(n)))

# 1. Test filtering with minPoints = 10 (should remove some subjects)
cat("\n--- Test 1: Filter with minPoints = 10 ---\n")
DataFiltered <- filterData(Data, minPoints = 10)
cat("Remaining subjects:", length(unique(DataFiltered@curves$subjID)), "\n")

# 2. Test filtering with a very high value (should remove everyone if high enough)
cat("\n--- Test 2: Filter with minPoints = 15 ---\n")
DataFiltered2 <- filterData(Data, minPoints = 15)
cat("Remaining subjects:", length(unique(DataFiltered2@curves$subjID)), "\n")

# 3. Verify that the removed subjects were indeed the ones with low points
check_subjects <- Data@curves %>%
    group_by(subjID, measureID) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n < 10) %>%
    pull(subjID) %>%
    unique()

cat("\nManual check: Subjects with < 10 points in at least one measure:", length(check_subjects), "\n")
