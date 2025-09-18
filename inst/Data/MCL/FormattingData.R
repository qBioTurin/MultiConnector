#### Loading data to cluster ####
load(system.file("Data/MCL/MCLcurves.RData", package="MultiConnector"))
colnames(BM) = sub("BM_","",colnames(BM))
BM = BM %>% mutate(measureID = "BM") %>% rename(subjID = SampleName,time = Time,value = Observation)

colnames(PB) = sub("PB_","",colnames(PB))
PB = PB %>% mutate(measureID = "PB") %>% rename(subjID = SampleName,time = Time,value = Observation)

TimeSeries =as_tibble(rbind(PB,BM))
Annotations = info %>% rename(subjID = SampleName)
saveRDS(TimeSeries,"./inst/Data/MCL/TimeSeries.rds")
saveRDS(Annotations,"./inst/Data/MCL/Annotations.rds")