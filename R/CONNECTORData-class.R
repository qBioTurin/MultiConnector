#' Functional longitudinal data
#'
#' An S4 container for longitudinal observations and subject-level annotations.
#' Objects are normally created with [ConnectorData()] rather than with `new()`.
#'
#' @slot curves A tibble in long format with columns `subjID`, `measureID`,
#'   `time`, `curvesID`, and `value`. Each row is one observation.
#' @slot dimensions A tibble containing the number of observations for each
#'   subject-measure curve.
#' @slot annotations A tibble of subject-level annotations. It must contain a
#'   `subjID` column.
#' @slot TimeGrids A named list. Each element contains the sorted unique time
#'   points observed for one measure.
#'
#' @seealso [ConnectorData()] to construct an object, [filterData()] and
#'   [truncate()] to transform it, and [plot()] to visualise it.
#' @export 
#' @import tibble

setClass(
  "CONNECTORData",
  slots = list(
    curves = "tbl_df",
    dimensions = "tbl_df",
    annotations = "tbl_df",
    TimeGrids = "list"
  )
)

# Validation method for CONNECTORData
setValidity("CONNECTORData", function(object) {
  errors <- character()
  
  # Check that all slots are present and non-empty
  if (nrow(object@curves) == 0) {
    errors <- c(errors, "curves slot cannot be empty")
  }
  
  if (nrow(object@dimensions) == 0) {
    errors <- c(errors, "dimensions slot cannot be empty")
  }
  
  if (nrow(object@annotations) == 0) {
    errors <- c(errors, "annotations slot cannot be empty")
  }
  
  if (length(object@TimeGrids) == 0) {
    errors <- c(errors, "TimeGrids slot cannot be empty")
  }
  
  if (length(errors) == 0) TRUE else errors
})
