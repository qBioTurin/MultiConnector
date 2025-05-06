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

TimeSeries = readRDS("../inst/Data/Synthetic/TimeSeries.RDs")
Annotations = readRDS("../inst/Data/Synthetic/Annotations.RDs")

####

source("DataImport.R")
source("CONNECTORData.R")
Data<-DataImport(TimeSeries, Annotations)
source("PlotTimeSeries.R")
PlotTimeSeries(Data, feature="treatment_group")
source("DataVisualization.R")
source("GridTimeOfPoints.R")
DataVisualization(Data)
DataVisualization(Data, large=T)
source("DataTruncation.R")
DataTruncation(Data, feature="gender", measure="Parabola", truncTime=5)
source("BasisDimensionChoice.R")
source("Clust.R")
CrossLogLikePlot<-BasisDimensionChoice(Data, p=2:10, cores=10)
CrossLogLikePlot$Cosine

source("KData.R")
source("ClusterAnalysis.R")

clusters<-ClusterAnalysis(Data, G=2:6, p=c("Cosine"=3,"Parabola"=6,"Hyperbola"=4,"Sine"=7), runs=8, cores=10)

saveRDS(clusters, file = "../inst/Data/Synthetic/clusters.RDs")
clusters = readRDS("../inst/Data/Synthetic/clusters.RDs")

source("IndexPlotExtrapolation.R")
IndexPlotExtrapolation(clusters)
source("ConfigSelection.R")
Set<-ConfigSelection(clusters, G=2, "MinfDB")
source("IndexPlotExtrapolation2.R")
IndexPlotExtrapolation2(Data, ConfigChoosen=Set,KData = clusters$KData, feature="comorbidity")

source("DiscriminantPlot.R")
DiscriminantPlot(Data, ConfigChoosen=Set, KData=clusters$KData, feature="gender")

source("SilhouetteAndEntropy.R")
SilEntropy(Set, clusters)
source("splinePlot.R")
splinePlots = splinePlot(KData=clusters$KData, ConfigChosen = Set)
source("MaximumDiscriminationFunction.R")
MaximumDiscriminationFunction(ConfigChoosen = Set, KData = clusters$KData)


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
                     KData = clusters$KData,
                     ConfigChosen = Set,
                     Cores =1,
                     entropyCutoff =1, probCutoff = 0.6 )

ClassNew$ListClassID$ID_1



