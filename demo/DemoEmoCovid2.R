
# to do just once
# devtools::install_github("qBioTurin/MultiConnector")

rm(list=ls())
library(MultiConnector)
library(dplyr)

getwd()
setwd("~/Documents/MultiConnectorProve/MultiConnector/demo")
SampledCurves = readRDS("../inst/Data/EmoCovid/emocovid_sampleCurves.RDS")
Annotations = readRDS("../inst/Data/EmoCovid/emocovid_annotations.RDS")
db <- ConnectorData(SampledCurves, Annotations) 


length(unique(SampledCurves$subjID))
length(unique(Annotations$subjID))


plot(db)
plot(db, feature = "outcome")
# ok ci siamo

library(parallel)
detectCores()
workers <- 20
spline_dimension <- estimatepDimension(db, p = 3:10, cores =  workers)
spline_dimension
getwd()
save.image(file = "emocovidMultiSplineDim.RData")

table(db@curves$measureID)
optimal_p <- c("Basophils" = 6, "Eosinophils" = 5, "Erythrocytes" = 5,
               "Leukocytes" = 5, "Lymphocytes" = 5, "Monocytes" = 7, "Neutrophils" = 5,
               "Thrombocytes" = 5)
clusters <- estimateCluster(db, 
                            G = 2:6,           # Test 2-6 clusters
                            p = optimal_p,     # Use optimal spline dimensions for both measurements
                            runs = 50,         # Multiple runs for stability
                            cores = workers) 

save.image(file = "emoMultiup2clusters.RData")
