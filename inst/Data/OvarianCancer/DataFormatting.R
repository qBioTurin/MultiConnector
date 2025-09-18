
# Define data file paths
# These files contain ovarian cancer cell growth data and sample annotations
TimeSeriesFile <- "./inst/Data/OvarianCancer/475dataset.xlsx"
AnnotationFile <- "./inst/Data/OvarianCancer/475info.txt"

# Import data using legacy connector format (temporary step)
# Note: This is for compatibility with existing data format
dataold = connector::DataImport(TimeSeriesFile, AnnotationFile)

# Transform to MultiConnector format
# The data needs to be in specific column format: subjID, measureID, time, value
TimeSeries = dataold$Dataset %>%
  mutate(measureID = "Ovarian") %>%        # Add measurement type identifier
  rename(subjID = ID,                       # Standardize column names
         time = Time,
         value = Observation)

# Transform annotations to match
Annotations = dataold$LabCurv %>%
  mutate(measureID = "Ovarian") %>%        # Add measurement type identifier  
  rename(subjID = ID)                      # Standardize ID column name

saveRDS(TimeSeries, file="./inst/Data/OvarianCancer/Ovarian_TimeSeries.rds")
saveRDS(Annotations, file="./inst/Data/OvarianCancer/Ovarian_Annotations.rds")
write.csv(Annotations, file="./inst/Data/OvarianCancer/Ovarian_Annotations.txt", row.names=FALSE, quote=FALSE)
writexl::write_xlsx(as.data.frame(TimeSeries), path="./inst/Data/OvarianCancer/Ovarian_TimeSeries.xlsx")