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
library(MetBrewer)
library(gghalves)
library(statmod)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#RandomGenerationNumber sets as: "Mersenne-Twister" "Inversion"        "Rejection"   (RNGkind())

#### Loading data to cluster ####
TimeSeriesFile<- "./../inst/Data/OvarianCancer/475dataset.xlsx"
AnnotationFile <- "./../inst/Data/OvarianCancer/475info.txt"

####

source("DataImport.R")
source("CONNECTORData.R")
source("PlotTimeSeries.R")
source("DataVisualization.R")
source("GridTimeOfPoints.R")
source("PlotDataTruncation.R")
source("DataTruncation.R")
source("BasisDimensionChoice.R")
source("Clust.R")

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

source("BasisDimensionChoice.R")
source("Clust.R")
CrossLogLikePlot<-BasisDimensionChoice(DataTrunc, p=2:6, cores=10)
CrossLogLikePlot

source("Clust.R")
source("ClusterAnalysis.R")

clusters<-ClusterAnalysis(DataTrunc, G=2:6, p=3, runs=50, cores=5)

source("IndexPlotExtrapolation.R")
IndexPlotExtrapolation(clusters)
source("CONNECTORDataClustered.R")
source("ConfigSelection.R")
Set<-ConfigSelection(clusters, G=4, "MinfDB")
source("SilhouetteAndEntropy.R")
SilEntropy(Set)

source("IndexPlotExtrapolation2.R")
IndexPlotExtrapolation2(DataTrunc, ConfigChosen=Set, feature="Progeny")

source("DiscriminantPlot.R")
DiscriminantPlot(DataTrunc, ConfigChosen=Set, feature="Progeny")


source("splinePlot.R")
splinePlots = splinePlot(ConfigChosen = Set)
source("MaximumDiscriminationFunction.R")
MaximumDiscriminationFunction(ConfigChosen = Set)

