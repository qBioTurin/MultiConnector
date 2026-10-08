#' Plot longitudinal measurements
#'
#' Draws one trajectory per subject and facets the result by measure. Curves can
#' optionally be coloured by a subject-level annotation.
#'
#' @param data A [CONNECTORData-class] object.
#' @param feature Optional single character string naming an annotation column.
#' @param labels Optional character vector of length three containing the x-axis
#'   label, y-axis label, and plot title, in that order.
#'
#' @return A `ggplot` object with free scales for each measure.
#'
#' @examples
#' \dontrun{
#' PlotTimeSeries(data)
#' PlotTimeSeries(data, feature = "group", labels = c("Time", "Value", "Profiles"))
#' }
#'
#' @seealso [plot()], [plotTimes()]
#' @import ggplot2 tibble dplyr
#' @export
#' @rdname plotTimeSeries

setGeneric("PlotTimeSeries", function(data,
                                      feature=NULL,
                                      labels)
  standardGeneric("PlotTimeSeries"))

#' @rdname plotTimeSeries
setMethod("PlotTimeSeries", signature("CONNECTORData"),
          function(data, feature=NULL, labels) {
            if (missing(labels))
            {
              axes.x <- ""
              axes.y <- ""
              title <- ""
            }
            else
            {
              axes.x <- labels[1]
              axes.y <- labels[2]
              title <- labels[3]
            }
            dataplot <- as_tibble(data@curves) %>%
              dplyr::select(subjID, time, measureID, value)
            
            if (!is.null(feature)) {
              # Controllo se la feature è valida
              if (!feature %in% colnames(data@annotations)) {
                stop("The provided feature does not exist in the annotations.")
              }
              
              dataplot <- merge(dataplot, data@annotations[, c("subjID", feature)], by = "subjID")
              dataplot[[feature]] <- factor(dataplot[[feature]])
              
              col <- as.character(unique(dataplot[[feature]]))
              colFeature <- rainbow(length(col))
              
              plt <- ggplot(dataplot, aes(
                x = time,
                y = value,
                group = subjID,
                col = .data[[feature]]
              )) +
                geom_line() +
                geom_point() +
                labs(
                  title = title,
                  x = axes.x,
                  y = axes.y,
                  col = feature
                ) +
                theme(plot.title = element_text(hjust = 0.5),
                      title = element_text(size = 10, face = 'bold')) +
                scale_colour_manual(
                  values = colFeature,
                  limits = col,
                  breaks = sort(col),
                  name = feature
                )
            } else {
              plt <- ggplot(dataplot, aes(
                x = time,
                y = value,
                group = subjID
              )) +
                geom_line() +
                geom_point() +
                labs(
                  title = title,
                  x = axes.x,
                  y = axes.y
                ) +
                theme(plot.title = element_text(hjust = 0.5),
                      title = element_text(size = 10, face = 'bold'))
            }
            
            plt <- plt + facet_wrap(~ measureID, scales = "free") +
              theme_bw()
            
            return(plt)
          })
