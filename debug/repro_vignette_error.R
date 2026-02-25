library(MultiConnector)
library(ggplot2)
# library(patchwork) # Don't load it to see if it causes issues

# Loading MCL data
ts_path <- "inst/Data/MCL/TimeSeries.rds"
ann_path <- "inst/Data/MCL/Annotations.rds"
TimeSeries <- readRDS(ts_path)
Annotations <- readRDS(ann_path)

Data <- ConnectorData(TimeSeries, Annotations)

# Run estimatepDimension (subsetting p for speed)
# We expect this to return a list of 2 patchwork objects (for PB and BM)
cat("Running estimatepDimension...\n")
CrossLogLikePlot <- estimatepDimension(Data, p = 2:3, cores = 1)

cat("Class of CrossLogLikePlot:", class(CrossLogLikePlot), "\n")
cat("Names of CrossLogLikePlot:", paste(names(CrossLogLikePlot), collapse = ", "), "\n")

# Try to print the result
cat("Attempting to print CrossLogLikePlot...\n")
tryCatch(
    {
        print(CrossLogLikePlot)
        cat("Print successful!\n")
    },
    error = function(e) {
        cat("Print failed with error:", e$message, "\n")
    }
)
