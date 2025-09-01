library(readxl)
library(dplyr)
library(readr)
library(tibble)
library(magrittr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(parallel)
library(MASS)
library(splines)
library(rlist)
library(RhpcBLASctl)
library(RColorBrewer)
library(Matrix)
# install.packages("MetBrewer")
library(MetBrewer)
# install.packages("gghalves")
library(gghalves)
library(statmod)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#RandomGenerationNumber sets as: "Mersenne-Twister" "Inversion"        "Rejection"   (RNGkind())

#### Loading data to cluster ####
TimeSeriesFile<- "./../inst/Data/OvarianCancer/475dataset.xlsx"
AnnotationFile <- "./../inst/Data/OvarianCancer/475info.txt"

####

source("../R/DataImport.R")
source("../R/CONNECTORData.R")
source("../R/PlotTimeSeries.R")
source("../R/DataVisualization.R")
source("../R/GridTimeOfPoints.R")
source("../R/PlotDataTruncation.R")
source("../R/DataTruncation.R")
source("../R/BasisDimensionChoice.R")
source("../R/Clust.R")

dataold = connector::DataImport(TimeSeriesFile,AnnotationFile)

TimeSeries = dataold$Dataset %>%
  mutate(measureID = "Ovarian") %>% rename(subjID = ID,time = Time,value = Observation)

Annotations = dataold$LabCurv  %>%
  mutate(measureID = "Ovarian") %>% rename(subjID = ID)

Data<-DataImport(as_tibble(TimeSeries), as_tibble(Annotations) )

PlotTimeSeries(Data, feature="Progeny")

DataVisualization(Data)
DataVisualization(Data, large=T)

PlotDataTruncation(Data,  measure="Ovarian", truncTime=70)
DataTrunc = DataTruncation(Data,  measure="Ovarian", truncTime=70)
PlotTimeSeries(DataTrunc, feature="Progeny")

source("../R/BasisDimensionChoice.R")
source("../R/Clust.R")
detectCores() -> nworkers
CrossLogLikePlot<-BasisDimensionChoice(DataTrunc, p=2:6, cores=nworkers-1)
CrossLogLikePlot

source("../R/Clust.R")
source("../R/ClusterAnalysis.R")

clusters<-ClusterAnalysis(DataTrunc, G=2:6, p=3, runs=50, cores=5)

source("../R/IndexPlotExtrapolation.R")
IndexPlotExtrapolation(clusters) -> pp
pp
source("../R/CONNECTORDataClustered.R")
source("../R/ConfigSelection.R")
Set<-ConfigSelection(clusters, G=4, "MinfDB")
source("../R/SilhouetteAndEntropy.R")
SilEntropy(Set)

source("../R/IndexPlotExtrapolation2.R")
IndexPlotExtrapolation2(DataTrunc, ConfigChosen=Set, feature="Progeny")

source("../R/DiscriminantPlot.R")
DiscriminantPlot(DataTrunc, ConfigChosen=Set, feature="Progeny")


source("../R/splinePlot.R")
splinePlots = splinePlot(ConfigChosen = Set)
source("../R/MaximumDiscriminationFunction.R")
MaximumDiscriminationFunction(ConfigChosen = Set)

