#' DataTruncation
#'
#' @description
#'
#' Truncates the functional data (the time series) at a specific time point chosen by the user.
#'
#' @param data CONNECTORData. (see \code{\link{DataImport}})
#' @param feature The column name reported in the AnnotationFile containing the feature  to be investigated.
#' @param truncTime  A two dimension vector of integers corresponding to the time points where the curves will be truncated. If an integer number is passed, than it will be considered as the upper time point by default.
#' @param labels  Vector containing the text for the title of axis names and plot title.
#' @param measure Measure on which to perform the Truncation.
#' @return  DataTruncation returns a list containing the truncated data and the plot of the truncated curves.
#'
#'
#' @examples
#'
#'TimeSeriesFile<-system.file("testdata", "test.xlsx", package = "ConnectorV2.0")
#'AnnotationFile <-system.file("testdata", "testinfo.txt", package = "ConnectorV2.0")
#'
#'CONNECTORData <- DataImport(TimeSeriesFile,AnnotationFile)
#'
#'CONNECTORData<- DataTruncation(CONNECTORData,"Progeny",truncTime=50,labels = c("time","volume","Tumor Growth"), measure="test")
#' @import ggplot2 tibble dplyr tidyr
#' @export


setGeneric("DataTruncation", function(data,
                                      feature,
                                      truncTime = NULL,
                                      labels = NULL,
                                      measure = NULL)
  standardGeneric("DataTruncation"))
#' @rdname DataTruncation
#' @export
setMethod("DataTruncation",
          signature ("CONNECTORData"), function(data,
                                                feature,
                                                truncTime = NULL,
                                                labels = NULL,
                                                measure = NULL)
          {
            select<-dplyr::select
            oldData <- data
            if (length(unique(data@curves$measureID)) > 1) {
              if (is.null(measure) || length(measure) != 1) {
                stop("Indicate one and only one measure on which to perform the Truncation.")
              }
              else{
                data <- filter(data@curves, measureID == measure)
                invisible(capture.output(data <-
                  DataImport(data, oldData@annotations)))
              }
            }
            dimBefore <- oldData@dimensions$nTimePoints
            growthCurveTr <-
              PlotTimeSeries(data, feature, labels = labels)
            if (!is.null(truncTime))
            {
              growthCurveTr <- growthCurveTr + geom_vline(xintercept = truncTime,
                                                          color = "black",
                                                          linewidth = 1)
            }
            if (!is.null(truncTime))
            {
              dataTr <- DataTrunc(data, truncTime = truncTime)
            }
            else
            {
              dataTr <- data
            }
            if (is.double(dataTr))
            {
              stop(
                "Considering this truncation time some curves with just one point are present.\n  A larger truncation time must be chosen.\n"
              )
            }
            if (length(unique(oldData@curves$measureID))>1) {
              oldAnnotations <- oldData@annotations
              oldData <- filter(oldData@curves, measureID != measure)
              dataTr <- bind_rows(oldData, dataTr@curves)
              invisible(capture.output(dataTr <- DataImport(dataTr, oldAnnotations)))
            }
            else{
              cat("############################### \n######## Summary ##############\n")
              cat("\n Number of curves:")
              print(dataTr@dimensions %>%
                      select(-curvesID) %>%
                      summarise_all(function(x)
                        sum(x != 0, na.rm = TRUE)))
              cat(";\n Min curve length: ")
              print(dataTr@dimensions %>%
                      select(-curvesID) %>%
                      summarise_all(function(x)
                        min(x, na.rm = TRUE)))
              
              cat("; Max curve length: ")
              print(dataTr@dimensions %>%
                      select(-curvesID) %>%
                      summarise_all(function(x)
                        max(x, na.rm = TRUE)))
              
              cat("############################### \n")
            }
              
              return(list(dataTr, growthCurveTr))
            })

setGeneric("DataTrunc", function(data,
                                 truncTime = NULL)
  standardGeneric("DataTrunc"))
setMethod("DataTrunc",
          signature = c("CONNECTORData"),
          function(data,
                   truncTime = NULL)
          {
            #### Truncates the growth data at a specific time point chosen by the user.
            # Variables inizialization
            
            select<-dplyr::select
            
            dataset <- data@curves
            
            
            # Data truncation
            
            if (!is.null(truncTime))
            {
              maxTime <- max(dataset$time)
              minTime <- min(dataset$time)
              
              if (length(truncTime) > 1)
              {
                maxTruncTime <- max(truncTime)
                minTruncTime <- min(truncTime)
              } else{
                minTruncTime <- minTime
                maxTruncTime <- truncTime
              }
              
              if (maxTime < maxTruncTime)
                warning("Max truncation time greater than maximum time in the dataset.")
              if (minTime > minTruncTime)
                warning("Min truncation time smaller than minimum time in the dataset.")
              
              datasetTr <-
                dataset[dataset$time <= maxTruncTime &
                          dataset$time >= minTruncTime , ]
              
              dimensionsTr <- datasetTr %>%
                select(-time, -subjID, -measureID) %>%
                group_by(curvesID) %>%
                summarise_all(~ sum(!is.na(.)))
              names(dimensionsTr)[names(dimensionsTr) == "value"] <- "nTimePoints"
              # Ottieni il nome della misura
              measure_name <- names(data@TimeGrids)[1]
              
              # Effettua la truncation mantenendo la struttura di lista
              timegridTr <- list()
              timegridTr[[measure_name]] <- data@TimeGrids[[1]][data@TimeGrids[[1]] <= maxTruncTime &
                                                                  data@TimeGrids[[1]] >= minTruncTime]
              dataTr <- new(
                "CONNECTORData",
                curves = datasetTr,
                dimensions = dimensionsTr,
                annotations = data@annotations,
                TimeGrids = timegridTr
              )
            }
            else
              dataTr <- data
            
            if (length(which(dataTr@dimensions < 2)) != 0)
            {
              warning("Curves with less than 2 points are now present!!!")
            }
            
            return(dataTr)
            ##### The CONNECTORList updated with the following arguments: a data frame with three variables (ID curves, observation and time values truncated at the chosen time), a vector collecting the number of truncated observations collected per sample, a data frame with curves labeled according to target file feature chosen and a vector for overall truncated time grid.
            
          })