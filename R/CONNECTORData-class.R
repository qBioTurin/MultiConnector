#' @title CONNECTORData
#' @description CONNECTORData is a S4 objectwith four arguments: (i) curves: tibble with n columns (i.e. ID, time and n-2 columns of records) encoding the growth data for each sample, (ii) length: the tibble reporting the number of observations collected per sample  (iii) annotation: the tibble matching the samples with their annotations, (iv) TimeGrid: the vector storing all the sample time points (i.e. time grid). Furthermore, it prints a brief summary of the input data, i.e. the total number of curves (samples), the minimum and the maximum curve length.
#' @slot curves tibble with n columns (i.e. ID, time and n-2 columns of records) encoding the growth data for each sample
#' @slot dimensions the tibble reporting the number of observations collected per sample
#' @slot annotations the tibble matching the samples with their annotations
#' @slot TimeGrids the vector storing all the sample time points (i.e. time grid)
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