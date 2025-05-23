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


#### Loading data to cluster ####

TimeSeries = readRDS("../inst/Data/PDX/TimeSeriesFiltered.RDs")
Annotations = readRDS("../inst/Data/PDX/AnnotationsFiltered.RDs")

####

source("DataImport.R")
source("CONNECTORData.R")
Data<-DataImport(as_tibble(TimeSeries), as_tibble(Annotations) )
source("PlotTimeSeries.R")
PlotTimeSeries(Data,feature = "LongID") + ggplot2::theme(legend.position = "none")
source("DataVisualization.R")
source("GridTimeOfPoints.R")
DataVisualization(Data)
DataVisualization(Data, large=T)
# source("PlotDataTruncation.R")
# source("DataTruncation.R")
# PlotDataTruncation(Data,  measure="PDX", truncTime=5)
# DataTruncation(Data,  measure="PDX", truncTime=5)
source("BasisDimensionChoice.R")
source("Clust.R")
CrossLogLikePlot<-BasisDimensionChoice(Data, p=2:6, cores=5)
CrossLogLikePlot

source("ClusterAnalysis.R")

clusters<-ClusterAnalysis(Data, G=2:6, p=4, runs=50, cores=5)

saveRDS(clusters, file = "../inst/Data/PDX/clusters.RDs")
clusters = readRDS("../inst/Data/PDX/clusters.RDs")

source("IndexPlotExtrapolation.R")
IndexPlotExtrapolation(clusters)
source("CONNECTORDataClustered.R")
source("ConfigSelection.R")
Set<-ConfigSelection(clusters, G=2, "MinfDB")
source("IndexPlotExtrapolation2.R")
IndexPlotExtrapolation2(Data, ConfigChosen=Set, feature="LongID")

source("SilhouetteAndEntropy.R")
SilEntropy(Set)

source("DiscriminantPlot.R")
DiscriminantPlot(Data, ConfigChosen=Set, feature="LongID")


source("splinePlot.R")
splinePlots = splinePlot(ConfigChosen = Set)
source("MaximumDiscriminationFunction.R")
MaximumDiscriminationFunction(ConfigChosen = Set)


###### Classification #######

TimeSeriesClassif = readRDS("../inst/Data/Synthetic/TimeSeries_Classification.RDs")
AnnotationsClassif = readRDS("../inst/Data/Synthetic/Annotations_Classification.RDs")
clusters = readRDS("../inst/Data/Synthetic/clusters.RDs")
source("ConfigSelection.R")
Set<-ConfigSelection(clusters, G=2, "MinfDB")
source("DataImport.R")
source("CONNECTORData.R")
DataNew<-DataImport(TimeSeriesClassif, AnnotationsClassif)
source("Classification.R")

ClassNew = ClassificationCurves(newdata = DataNew,
                                ConfigChosen = Set,
                                Cores =1,
                                entropyCutoff =1, probCutoff = 0.6 )

ClassNew$ListClassID$ID_1



