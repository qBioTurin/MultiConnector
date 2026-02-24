
#### Loading data to cluster ####

TimeSeries = readRDS(system.file("Data/Synthetic/TimeSeries.rds", package="MultiConnector")) 
Annotations = readRDS(system.file("Data/Synthetic/Annotations.RDs", package="MultiConnector"))  
####

Data<-ConnectorData(TimeSeries, Annotations)

plot(Data, feature="treatment_group")

plotTimes(Data)
plotTimes(Data, large=T)

truncatePlot(Data,  measure="Parabola", truncTime=5)
truncate(Data,  measure="Parabola", truncTime=5)

CrossLogLikePlot<-estimatepDimension(Data, p=2:10, cores=1)
CrossLogLikePlot$Parabola

clusters<-estimateCluster(Data, G=2:6, p=c("Cosine"=3,"Parabola"=6,"Hyperbola"=4,"Sine"=7), runs=10, cores=5)
plot(clusters)

saveRDS(clusters, file = "../inst/Data/Synthetic/clusters.RDs")
clusters = readRDS("../inst/Data/Synthetic/clusters.RDs")

plot(clusters)


ClusterData <- selectCluster(clusters, G=3, "MinfDB")
plot(ClusterData)

Metrics <- validateCluster(ClusterData)
Metrics



###### Classification #######

TimeSeriesClassif = readRDS("../inst/Data/Synthetic/TimeSeries_Classification.RDs")
AnnotationsClassif = readRDS("../inst/Data/Synthetic/Annotations_Classification.RDs")
clusters = readRDS("../inst/Data/Synthetic/clusters.RDs")
source("selectCluster.R")
Set<-selectCluster(clusters, G=2, "MinfDB")
source("DataImport.R")
source("CONNECTORData.R")
DataNew<-ConnectorData(TimeSeriesClassif, AnnotationsClassif)
source("../R/Classification.R")

ClassNew = ClassificationCurves(newdata = DataNew,
                     ConfigChosen = Set,
                     Cores =1,
                     entropyCutoff =1, probCutoff = 0.6 )

ClassNew$ListClassID$ID_1



