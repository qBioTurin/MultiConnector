#' MultiConnector: functional clustering of multivariate longitudinal data
#'
#' MultiConnector provides a workflow for importing irregular longitudinal data,
#' selecting spline dimensions, fitting functional clustering models, choosing a
#' clustering solution, and interpreting or validating the resulting groups.
#'
#' The main workflow is:
#'
#' 1. Create a [CONNECTORData-class] object with [ConnectorData()].
#' 2. Inspect candidate spline dimensions with [estimatepDimension()].
#' 3. Fit candidate partitions with [estimateCluster()].
#' 4. Select one model with [selectCluster()].
#' 5. Explore it with [plot()], [validateCluster()], [DiscriminantPlot()], and
#'    [MaximumDiscriminationFunction()].
#'
#' New longitudinal curves can be assigned to an existing model with
#' [ClassificationCurves()].
#'
#' @references
#' James, G. M. and Sugar, C. A. (2003). Clustering for sparsely sampled
#' functional data. *Journal of the American Statistical Association*, 98(462),
#' 397--408. \doi{10.1198/016214503000189}
#'
#' @docType package
#' @name MultiConnector-package
#' @aliases MultiConnector
#' @keywords internal
"_PACKAGE"
