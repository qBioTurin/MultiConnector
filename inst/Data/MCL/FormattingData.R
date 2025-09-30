#### Loading data to cluster ####
WholeData <- readRDS("~/Desktop/GIT/R_packages_project/MCLexplorer/inst/Data/WholeData.RDs")

BM = merge(WholeData$MCL0208$BM$CONNECTORList.FCM$CONNECTORList$Dataset ,
           WholeData$MCL0208$BM$CONNECTORList.FCM$CONNECTORList$LabCurv %>% select(ID,SampleName) ) %>% select(-ID,-Step)
infoBM = merge(WholeData$MCL0208$BM$Dataset, WholeData$MCL0208$BM$CONNECTORList.FCM$CONNECTORList$LabCurv %>% select(ID,SampleName)) %>% select(-ID)


PB = merge(WholeData$MCL0208$PB$CONNECTORList.FCM$CONNECTORList$Dataset ,
           WholeData$MCL0208$PB$CONNECTORList.FCM$CONNECTORList$LabCurv %>% select(ID,SampleName) ) %>% select(-ID,-Step)
infoPB = merge(WholeData$MCL0208$PB$Dataset, WholeData$MCL0208$PB$CONNECTORList.FCM$CONNECTORList$LabCurv %>% select(ID,SampleName)) %>% select(-ID)

BM = BM %>% mutate(measureID = "BM") %>% rename(subjID = SampleName, time = Time,value = Observation)
PB = PB %>% mutate(measureID = "PB") %>% rename(subjID = SampleName,time = Time,value = Observation)

TimeSeries =as_tibble(rbind(PB,BM))
Annotations =as_tibble(rbind(infoPB %>% select(-Cluster),infoBM %>% select(-Cluster))) %>% distinct() %>% rename(subjID = SampleName)

saveRDS(TimeSeries,"./inst/Data/MCL/TimeSeries.rds")
saveRDS(Annotations,"./inst/Data/MCL/Annotations.rds")
