#' Visualise observation-time coverage
#'
#' Summarises how many subject-measure curves are observed at each time. The
#' detailed view also displays the frequency of pairs of observation times.
#'
#' @param data A [CONNECTORData-class] object.
#' @param large Logical. If `FALSE`, return the compact coverage summary. If
#'   `TRUE`, add the two-dimensional observation-time density plot.
#' @param measureIDs Optional character vector of measures to include. `NULL`
#'   includes all measures.
#'
#' @return A patchwork plot assembled from `ggplot` objects.
#'
#' @examples
#' \dontrun{
#' plotTimes(data)
#' plotTimes(data, large = TRUE, measureIDs = "marker")
#' }
#'
#' @seealso [plot()], [getMeasures()]
#' @export

setGeneric("plotTimes", function(data, large=FALSE, measureIDs=NULL) standardGeneric("plotTimes"))

#' @rdname plotTimes
setMethod("plotTimes", signature = c("CONNECTORData"), function(data, large =FALSE, measureIDs=NULL) {
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
