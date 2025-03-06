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
#' @export

setGeneric("PlotTimeSeries", function(data,
                                      feature,
                                      labels)
  standardGeneric("PlotTimeSeries"))

#' @rdname PlotTimeSeries
#' @export

setMethod("PlotTimeSeries", signature("CONNECTORData"),
          function(data, feature, labels) {
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
            #dataframe for ggplot
            dataplot <-
              as_tibble(merge(data@curves, data@annotations[, c("subjID", feature)], by =
                                "subjID"))
            dataplot[[feature]] <-
              factor(as.matrix(dataplot[feature]))
            
            col <- as.character(unique(dataplot[[feature]]))
            colFeature <-
              rainbow(dim(unique(data@annotations[feature])))
            # long_dataplot <- dataplot %>%
            #   pivot_longer(c(-curvesID, -time, -all_of(feature), -subjID, -measureID),
            #                names_to = "curve",
            #                values_to = "value")
            
            #ggplot with a facet-grid on all different from time Id and feature
            PlotTimeSeries <-
              ggplot(data = dataplot, aes(
                x = time,
                y = value,
                group = subjID,
                col = factor(.data[[feature]])
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
              ) +
              facet_wrap(~ measureID, scales = "free") +
              theme_bw()
              
            ### Set growth curve plot with ggplot
            
            
            #data$FeatureColour <- colFeature
            
            
            return(PlotTimeSeries)
          })
