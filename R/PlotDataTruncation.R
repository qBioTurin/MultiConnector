setGeneric("PlotDataTruncation", function(data,
                                      feature=NULL,
                                      truncTime = NULL,
                                      labels = NULL,
                                      measure = NULL)
  standardGeneric("PlotDataTruncation"))
#' @rdname DataTruncation
#' @export
setMethod("PlotDataTruncation",
          signature ("CONNECTORData"), function(data,
                                                feature=NULL,
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
                                           ConnectorData(data, oldData@annotations)))
              }
            }
            dimBefore <- oldData@dimensions$nTimePoints
            
            growthCurveTr <- PlotTimeSeries(data, feature = feature, labels = labels)
            
            
            if (!is.null(truncTime))
            {
              growthCurveTr <- growthCurveTr + geom_vline(xintercept = truncTime,
                                                          color = "black",
                                                          linewidth = 1)
              return(growthCurveTr)
            }})