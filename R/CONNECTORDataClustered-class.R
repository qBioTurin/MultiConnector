#' @title CONNECTORDataClustered
#' @description CONNECTORDataClustered is a class with the set choosen and with KData that is a list with all output of PRESETKMEANS that are necessary for call JUSTKMEANS and INTFCLUST
#' @slot TTandfDBandSil Contains TT, fDB, Silhouette and G choosen
#' @slot CfitandParameteres Contains values calculated during clusteranalysis
#' @slot h h value choosen
#' @slot freq frequency of the clusterization
#' @slot cluster.names names of the clusters
#' @slot KData contains: CData Contains timeseriefile datas, TimeGrids TimeGrids present in CONNECTORData, points Initial coefficent spline points, N subject number, S block diagonal matrix, FullS full block diagonal matrix

setClass(
  "CONNECTORDataClustered",
  slots = list(
    TTandfDBandSil = "tbl_df",
    CfitandParameters = "list",
    h = "numeric",
    freq = "numeric",
    cluster.names = "character",
    KData = "list"
  )
)

# Method to extract annotations for both classes
#' @title getAnnotations
#' @description Extract and display annotations from CONNECTORData or CONNECTORDataClustered object.
#' Shows all available features (annotation columns) in both cases.
#' @param object CONNECTORData or CONNECTORDataClustered object
#' @return A vector of annotation names for CONNECTORData objects.
#' @details 
#' This method provides the features available in the annotations of the provided object.
#' @examples
#' \dontrun{
#' # For CONNECTORData
#' getAnnotations(my_connector_data)
#' 
#' # For CONNECTORDataClustered 
#' getAnnotations(my_clustered_data)
#' }
#' @import dplyr
#' @export
setGeneric("getAnnotations", function(object) {
  standardGeneric("getAnnotations")
})

setMethod("getAnnotations", signature(object = "CONNECTORDataClustered"), function(object) {
  # Get annotations from KData
  annotations <- object@KData$annotations
  feature_cols <- colnames(annotations)[!colnames(annotations) %in% c("subjID", "measureID")]

  return(feature_cols)
})

setMethod("getAnnotations", signature(object = "CONNECTORData"), function(object) {
  # Get annotations directly from CONNECTORData object
  annotations <- names(object@annotations)
  
  return(annotations)
})