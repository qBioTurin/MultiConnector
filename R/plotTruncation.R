#' Preview a time truncation
#'
#' Draws the selected longitudinal curves and overlays vertical lines at the
#' proposed truncation boundaries. This function does not modify the data.
#'
#' @param data A [CONNECTORData-class] object.
#' @param feature Optional annotation column used to colour the curves.
#' @param truncTime Numeric value or vector giving the proposed truncation
#'   boundary or boundaries. `NULL` draws no boundary.
#' @param labels Optional character vector of plot labels passed to [plot()].
#' @param measure Character vector naming the measures to display. `NULL`
#'   displays all measures.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' truncatePlot(data, truncTime = 30)
#' truncatePlot(data, truncTime = c(5, 30), measure = "marker")
#' }
#'
#' @seealso [truncate()], [plot()]
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

#' @rdname truncatePlot
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
