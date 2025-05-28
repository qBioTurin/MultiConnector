#' @title PlotTimeSeries
#'
#' @description the line plot of the time series data. The curves are colored with respect to the feature chosen by the user and/or reported in the AnnotationFile.
#'
#' @param data CONNECTORData. See CONNECTORData for details.
#' @param feature The column name reported in the AnnotationFile containing the feature to be investigated.
#' @param labels   The vector containing the text for the axis names and plot title.
#'
#' @return PlotTimeSeries returns a ggplot object.
#'
#' @examples
#'TimeSeriesFile<-system.file("testdata", "test.xlsx", package = "ConnectorV2.0")
#'AnnotationFile <-system.file("testdata", "testinfo.txt", package = "ConnectorV2.0")
#'
#' CONNECTORData <- DataImport(TimeSeriesFile,AnnotationFile)
#'
#' PlotTimeSeries(CONNECTORData,"Progeny",labels=c("Time","Volume","Tumor Growth"))
#'
#' @import ggplot2 tibble dplyr

setGeneric("PlotTimeSeries", function(data,
                                      feature=NULL,
                                      labels)
  standardGeneric("PlotTimeSeries"))

#' @rdname PlotTimeSeries

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
            dataplot <- as_tibble(data@curves)
            
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
