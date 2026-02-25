#' truncatePlot
#'
#' @description
#'
#' Plot the truncation of functional data (the time series) at a specific time point chosen by the user.
#'
#' @param data CONNECTORData. (see \code{\link{ConnectorData}})
#' @param feature The column name reported in the AnnotationFile containing the feature  to be investigated.
#' @param truncTime  A two dimension vector of integers corresponding to the time points where the curves will be truncated. If an integer number is passed, than it will be considered as the upper time point by default.
#' @param labels  Vector containing the text for the title of axis names and plot title.
#' @param measure Measure on which to perform the Truncation.
#' @return  a plot with a bar that indicate the position of truncation. Doesen0t truncate effectively. For truncation check truncate()
#' @import ggplot2 tibble
#' @importFrom dplyr select filter group_by mutate arrange
#' @importFrom tidyr gather spread
#' @export

setGeneric("truncatePlot", function(data,
                                    feature = NULL,
                                    truncTime = NULL,
                                    labels = NULL,
                                    measure = NULL) {
  standardGeneric("truncatePlot")
})

setMethod(
  "truncatePlot",
  signature("CONNECTORData"), function(data,
                                       feature = NULL,
                                       truncTime = NULL,
                                       labels = NULL,
                                       measure = NULL) {
    if (is.null(measure)) {
      measure <- unique(data@curves$measureID)
    }

    # Filter data to selected measures
    curves_to_plot <- data@curves %>% filter(measureID %in% measure)

    if (nrow(curves_to_plot) == 0) {
      stop("None of the specified measures found in data.")
    }

    # Reconstruct temporary object for plotting
    invisible(capture.output(
      data_subset <- ConnectorData(curves_to_plot, data@annotations)
    ))

    # Generate base plot
    growthCurveTr <- plot(data_subset, feature = feature, labels = labels)


    if (!is.null(truncTime)) {
      # Add truncation line (will appear on all relevant facets/measures)
      growthCurveTr <- growthCurveTr + geom_vline(
        xintercept = truncTime,
        color = "black",
        linewidth = 1
      )
    }

    return(growthCurveTr)
  }
)
