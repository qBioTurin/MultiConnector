

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