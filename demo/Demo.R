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

source("../R/ConnectorData.R")
source("../R/CONNECTORData.R")
Data<-ConnectorData(TimeSeries, Annotations)
source("../R/PlotTimeSeries.R")
source("../R/plot.R")
plot(Data, feature="treatment_group")
source("../R/plotTimes.R")
source("../R/GridTimeOfPoints.R")
plotTimes(Data)
plotTimes(Data, large=T)
source("../R/truncatePlot.R")
source("../R/truncate.R")
truncatePlot(Data,  measure="Parabola", truncTime=5)
truncate(Data,  measure="Parabola", truncTime=5)
source("../R/estimatepDimension.R")
source("../R/Clust.R")
CrossLogLikePlot<-estimatepDimension(Data, p=2:10, cores=10)
CrossLogLikePlot$Parabola

source("../R/Clust.R")
source("../R/estimateCluster.R")
source("../R/IndexPlotExtrapolation.R")
clusters<-estimateCluster(Data, G=2:6, p=c("Cosine"=3,"Parabola"=6,"Hyperbola"=4,"Sine"=7), runs=8, cores=10)
clusters$plot
saveRDS(clusters, file = "../inst/Data/Synthetic/clusters.RDs")
clusters = readRDS("../inst/Data/Synthetic/clusters.RDs")


source("../R/CONNECTORDataClustered.R")
source("../R/configSelection.R")
Set<-configSelection(clusters, G=2, "MinfDB")
source("../R/IndexPlotExtrapolation2.R")
plot(Data, ConfigChosen=Set, feature="comorbidity")

source("../R/DiscriminantPlot.R")
DiscriminantPlot(Data, ConfigChosen=Set, feature="gender")

source("../R/SilhouetteAndEntropy.R")
SilEnt<-SilEntropy(Set)
source("../R/splinePlot.R")
splinePlots = splinePlot(ConfigChosen = Set)
source("../R/MaximumDiscriminationFunction.R")
MaximumDiscriminationFunction(ConfigChosen = Set)


###### Classification #######

TimeSeriesClassif = readRDS("../inst/Data/Synthetic/TimeSeries_Classification.RDs")
AnnotationsClassif = readRDS("../inst/Data/Synthetic/Annotations_Classification.RDs")
clusters = readRDS("../inst/Data/Synthetic/clusters.RDs")
source("ConfigSelection.R")
Set<-ConfigSelection(clusters, G=2, "MinfDB")
source("DataImport.R")
source("CONNECTORData.R")
DataNew<-ConnectorData(TimeSeriesClassif, AnnotationsClassif)
source("../R/Classification.R")

ClassNew = ClassificationCurves(newdata = DataNew,
                     ConfigChosen = Set,
                     Cores =1,
                     entropyCutoff =1, probCutoff = 0.6 )

ClassNew$ListClassID$ID_1



