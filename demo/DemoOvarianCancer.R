library(dplyr)
library(parallel)
library(MultiConnector)

#### Loading data to cluster ####
TimeSeriesFile<- "./inst/Data/OvarianCancer/475dataset.xlsx"
AnnotationFile <- "./inst/Data/OvarianCancer/475info.txt"

#### Creating the data object ####
dataold = connector::DataImport(TimeSeriesFile,AnnotationFile)

TimeSeries = dataold$Dataset %>%
  mutate(measureID = "Ovarian") %>% rename(subjID = ID,time = Time,value = Observation)

Annotations = dataold$LabCurv  %>%
  mutate(measureID = "Ovarian") %>% rename(subjID = ID)

#### Starting the analysis ####
Data <- ConnectorData(as_tibble(TimeSeries), as_tibble(Annotations) )

plot(Data)
plot(Data,feature="Progeny")

plotTimes(Data, large=T)
plotTimes(Data, large=F)

truncatePlot(Data, measure="Ovarian", truncTime=70)
truncate(Data, measure="Ovarian", truncTime=70) -> DataTrunc
plot(DataTrunc)

### p estimation
detectCores() -> nworkers
CrossLogLikePlot<-estimatepDimension(DataTrunc, p=2:6, cores=nworkers-1)
CrossLogLikePlot
# p is 3 

### Clustering
# Mean Total time with 8 cores: 23 secs
clusters<-estimateCluster(DataTrunc, G=2:6, p=3, runs=50, cores=nworkers-1)
plot(clusters)


CLusterData<-selectCluster(clusters, G=4, "MinfDB")
plot(CLusterData)
getAnnotations(CLusterData)
plot(CLusterData,feature="Progeny")

Metrics = validateCluster(CLusterData)
Metrics$plot

Discr = MultiConnector::DiscriminantPlot(CLusterData)
Discr$ColCluster
Discr$ColFeature

splinePlots = splinePlot(CLusterData)
splinePlots$`1`

MultiConnector::MaximumDiscriminationFunction(CLusterData)

