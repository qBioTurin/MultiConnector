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

# Method to print annotations for CONNECTORDataClustered
#' @title getAnnotations
#' @description Extract and display annotations from CONNECTORDataClustered object
#' @param object CONNECTORDataClustered object
#' @return The annotations tibble with cluster assignments
#' @export
setGeneric("getAnnotations", function(object) {
  standardGeneric("getAnnotations")
})

#' @rdname getAnnotations
setMethod("getAnnotations", signature(object = "CONNECTORDataClustered"), function(object) {
  # Get annotations from KData
  annotations <- object@KData$annotations
  
  # Get cluster assignments
  cluster_assignments <- object@CfitandParameters$pred$class.pred
  
  # Add cluster assignments to annotations
  if (!is.null(annotations) && !is.null(cluster_assignments)) {
    # Match cluster assignments with subjects
    annotations$cluster <- cluster_assignments[match(annotations$subjID, object@KData$CData$subjID)]
    
    # Print summary information
    cat("Annotations for", nrow(annotations), "subjects:\n")
    cat("Number of clusters:", length(unique(annotations$cluster, na.rm = TRUE)), "\n")
    cat("Cluster distribution:\n")
    print(table(annotations$cluster, useNA = "ifany"))
    cat("\n")
    
    return(annotations)
  } else {
    cat("No annotations found in the object.\n")
    return(NULL)
  }
})