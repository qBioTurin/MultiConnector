
# https://github.com/qBioTurin/GreatNector-MS/tree/main
# githiub in cui puoi trovare i report fatti sulle singole analisi

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

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#### Loading data to cluster ####
files = list.files("../inst/Data/MultipleSclerosis/")
do.call(rbind,lapply(files[grepl("temporal",x = files)],function(f){
  Data = loadRData(paste0("../inst/Data/MultipleSclerosis/",f))
  f = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(f))
  f = sub(pattern = "temporal", replacement = "", x = f)
  Data$measureID = f
  
  Data %>% 
    rename(subjID = ID,time = Time,value = Observation) %>% na.omit()
  
})) -> TimeSeries

Annotations = loadRData("../inst/Data/MultipleSclerosis/Feature.RData") %>% rename(subjID = ID)

####

source("DataImport.R")
source("CONNECTORData.R")
Data<-DataImport(as_tibble(TimeSeries), as_tibble(Annotations) )
str(Data,max.level = 3)

source("PlotTimeSeries.R")
PlotTimeSeries(Data,feature = "GENDER")

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

# QUI
# da controllare le p dalle analisi fatte prima
CrossLogLikePlot<-BasisDimensionChoice(Data, p=2:6, cores=5)
CrossLogLikePlot$CD4
CrossLogLikePlot$EDSS
CrossLogLikePlot$Th1_prod
CrossLogLikePlot$Th17_prod
CrossLogLikePlot$Th17Treg
CrossLogLikePlot$Treg_prod

source("ClusterAnalysis.R")

clusters<-ClusterAnalysis(Data, G=2:6,
                          p=c("EDSS" = 3, "CD4" = 4,"Th1_prod" = 4,"Th17_prod" = 4, "Th17Treg" = 4, "Treg_prod" = 5),
                          runs=50, cores=5)

saveRDS(clusters, file = "../inst/Data/MCL/clusters.RDs")
clusters = readRDS("../inst/Data/MCL/clusters.RDs")
str(clusters,max.level = 1)


source("IndexPlotExtrapolation.R")
IndexPlotExtrapolation(clusters)
source("CONNECTORDataClustered.R")
source("ConfigSelection.R")
Set<-ConfigSelection(clusters, G=3, "MinfDB")
str(Set,max.level = 3)

source("IndexPlotExtrapolation2.R")
IndexPlotExtrapolation2(Data, ConfigChosen=Set, feature="GENDER")

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



