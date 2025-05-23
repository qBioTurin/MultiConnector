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
load("./../inst/Data/MCL/MCLcurves.RData")

colnames(BM) = sub("BM_","",colnames(BM))
BM = BM %>% mutate(measureID = "BM") %>% rename(subjID = SampleName,time = Time,value = Observation)

colnames(PB) = sub("PB_","",colnames(PB))
PB = PB %>% mutate(measureID = "PB") %>% rename(subjID = SampleName,time = Time,value = Observation)

TimeSeries =as_tibble(rbind(PB,BM))
Annotations = info %>% rename(subjID = SampleName)

####

source("DataImport.R")
source("CONNECTORData.R")
Data<-DataImport(as_tibble(TimeSeries), as_tibble(Annotations) )
source("PlotTimeSeries.R")
PlotTimeSeries(Data,feature = "TTP")
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
CrossLogLikePlot$PB
CrossLogLikePlot$BM
source("ClusterAnalysis.R")

clusters<-ClusterAnalysis(Data, G=2:6, p=c(4,4), runs=50, cores=5)

saveRDS(clusters, file = "../inst/Data/MCL/clusters.RDs")
clusters = readRDS("../inst/Data/MCL/clusters.RDs")

source("IndexPlotExtrapolation.R")
IndexPlotExtrapolation(clusters)
source("CONNECTORDataClustered.R")
source("ConfigSelection.R")
Set<-ConfigSelection(clusters, G=2, "MinfDB")
source("IndexPlotExtrapolation2.R")
IndexPlotExtrapolation2(Data, ConfigChosen=Set, feature="TTP")

source("SilhouetteAndEntropy.R")
SilEntropy(Set)

source("DiscriminantPlot.R")
DiscriminantPlot(Data, ConfigChosen=Set, feature="TTP")


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



