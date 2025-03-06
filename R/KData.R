

#' @title KData
#' @description KData is a class with all output of PRESETKMEANS that are necessary for call JUSTKMEANS and INTFCLUST
#' @slot points Initial coefficent spline points
#' @slot N subject number
#' @slot S block diagonal matrix
#' @slot FullS full block diagonal matrix
#' @exportclass KData


setClass(
  "KData",
  slots = list(
    CData = "tbl_df",
    TimeGrids = "list",
    points = "matrix",
    N = "integer",
    S = "matrix",
    FullS = "list"
  )
)