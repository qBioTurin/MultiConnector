
library(dplyr)
library(ggplot2)
library(patchwork)
library(parallel)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### Loading data to cluster ####

SampledCurves = readRDS("../inst/Data/EmoCovid/emocovid_sampleCurves.RDS")
Annotations = readRDS("../inst/Data/EmoCovid/emocovid_annotations.RDS")

####

source("../R/ConnectorData.R")
source("../R/CONNData.R")
Data<-ConnectorData(SampledCurves, Annotations)
source("../R/plot.R")
source("../R/PlotTimeSeries.R")
plot(Data, feature="outcome")
source("../R/plotTimes.R")
source("../R/GridTimeOfPoints.R")
plotTimes(Data)
plotTimes(Data, large=T)
source("../R/truncatePlot.R")
source("../R/truncate.R")
truncatePlot(Data,  measure="Basophils", feature="outcome", truncTime=5)
# truncate(Data,  measure="Basophils", truncTime=5)
source("../R/estimatepDimension.R")
source("../R/Clust.R")

nworkers <- detectCores()-1
CrossLogLikePlot<-estimatepDimension(Data, p=3)

CrossLogLikePlot$Parabola

source("../R/Clust.R")
source("../R/ClusterAnalysis.R")
source("../R/IndexPlotExtrapolation.R")
clusters<-ClusterAnalysis(Data, G=2:6, p=c("Cosine"=3,"Parabola"=6,"Hyperbola"=4,"Sine"=7), runs=300, cores=10)
clusters$plot
saveRDS(clusters, file = "../inst/Data/Synthetic/clusters.RDs")
clusters = readRDS("../inst/Data/Synthetic/clusters.RDs")


source("../R/CONNECTORDataClustered.R")
source("../R/ConfigSelection.R")
Set<-ConfigSelection(clusters, G=2, "MinfDB")
source("../R/IndexPlotExtrapolation2.R")
plot(Data, ConfigChosen=Set, feature="comorbidity")

source("../R/DiscriminantPlot.R")
DiscriminantPlot(Data, ConfigChosen=Set, feature="gender")

source("../R/SilhouetteAndEntropy.R")
SilEntropy(Set)
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



