
# to do just once
# devtools::install_github("qBioTurin/MultiConnector")

rm(list=ls())
library(MultiConnector)
library(dplyr)


SampledCurves = readRDS("/Users/susie/Documents/Ricerca/Francesca/connprowerN/ConnectorNtodeploy/MultiConnector/inst/Data/EmoCovid/emocovid_sampleCurves.RDS")
Annotations = readRDS("/Users/susie/Documents/Ricerca/Francesca/connprowerN/ConnectorNtodeploy/MultiConnector/inst/Data/EmoCovid/emocovid_annotations.RDS")
db <- ConnectorData(SampledCurves, Annotations) 
# ok qui mi dice che ci sono dei soggetti con delle curve con meno  di due osservazioni (forse meglio dire una sola?)
# attenzione specificare nel messaggio di errore <= perché meno di due è <

SampledCurves <- SampledCurves %>%
  group_by(measureID, subjID) %>%
  mutate(n = n(), single_point = if_else(n<=2, TRUE, FALSE)) %>%
  ungroup()
  
# ho dovuto reinstallare cli ? non so perché
# install.packages("cli")

SampledCurves <- SampledCurves %>%
  filter(single_point == FALSE)

length(unique(SampledCurves$subjID))
length(unique(Annotations$subjID))

SampledCurves <- SampledCurves %>%
  select(-n, -single_point)

db <- ConnectorData(SampledCurves, Annotations) 
# ok ci siamo

plot(db)
plot(db, feature = "outcome")
# ok ci siamo

library(parallel)
workers <- detectCores() - 2
spline_dimension <- estimatepDimension(db, p = 3:5, cores =  workers)



