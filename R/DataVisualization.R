#' Data Visualitation
#'
#' creates a barplot of the time presences in various IDs or also a plot that represent time grid density.
#'
#' @param data CONNECTORData. (see \code{\link{DataImport}})
#' @param large logical. If TRUE the function will return a plot that represent time grid density.
#' @return  Data Visualization returns a plot with the density time grid and the line plot of growth data as a ggplot object.
#'  In details, a point $p_{x,y}$ of the time grid density  is defined by a pair of coordinates $p_{x,y}=( x,y) \ $ and by a colour. $p_{x,y}$ is defined if only if exists at least one sample with two observations at time $x\ $ and $y$.
#'  The colour associates with it encodes the frequency of samples in which $p_{x,y}$ is present.
#' 
#' @examples
#'
#'TimeSeriesFile<-system.file("testdata", "test.xlsx", package = "ConnectorV2.0")
#'AnnotationFile <-system.file("testdata", "testinfo.txt", package = "ConnectorV2.0")
#'
#'CONNECTORData <- DataImport(TimeSeriesFile,AnnotationFile)
#'
#'DataVisualization(CONNECTORData)
#'
#' 
#' @seealso  \code{\link{PlotTimeSeries}}, code{\link{GridTimeOfPoints}}.
#' @export
#' 
setGeneric("DataVisualization", function(data, large=FALSE, measureIDs=NULL) standardGeneric("DataVisualization"))
#' @rdname DataVisualization
#' @export
setMethod("DataVisualization", signature = c("CONNECTORData"), function(data, large =FALSE, measureIDs=NULL) {
  if (!is.null(measureIDs) && length(measureIDs) > 0) {
    data@curves <- dplyr::filter(data@curves, measureID %in% measureIDs)
  }
  if(length(data@curves$measureID)==0){
    stop("No data corresponding to the measureID given")
  }
  if(large){
    return(LargeGridTimeOfPoints(data))
  }
  else{
    return(GridTimeOfPoints(data))
  }
})
