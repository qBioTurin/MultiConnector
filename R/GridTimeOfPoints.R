#' @title GridTimeOfPoints
#' @description creates a barplot of the time presences in various IDs or also a plot that represent time grid density.
#' @param data CONNECTORData. (see \code{\link{DataImport}})
#' @return GridTimeOfPoints returns a plot with the density time grid and the line plot of growth data as a ggplot object.
#'  In details, a point $p_{x,y}$ of the time grid density  is defined by a pair of coordinates $p_{x,y}=( x,y) \ $ and by a colour. $p_{x,y}$ is defined if only if exists at least one sample with two observations at time $x\ $ and $y$.
#'  The colour associates with it encodes the frequency of samples in which $p_{x,y}$ is present.
#' @import ggplot2 tidyr dplyr patchwork
#' @export



setGeneric("GridTimeOfPoints", function(data)
  standardGeneric("GridTimeOfPoints"))

#' @rdname GridTimeOfPoints
#' @export
setMethod("GridTimeOfPoints", signature = c("CONNECTORData"),
          function(data) {
            nTime <- data@curves %>%
              group_by(time, measureID) %>%
              summarise(distinct_IDs = n_distinct(curvesID), .groups = "drop")
          
            p1 <- ggplot(nTime, aes(x = time, y = distinct_IDs, fill = measureID)) +
              geom_bar(stat = "identity", position = "dodge") +
              labs(title = "Number of distinct IDs per time by MeasureID",
                   x = "Time",
                   y = "Number of distinct IDs") +
              theme(plot.title = element_text(hjust = 0.5),
                    title = element_text(size = 10, face = 'bold')) +
              theme_bw()
            #Distribuzione media nei tempi
            p2 <- ggplot(nTime, aes(x = time, fill = as.factor(measureID))) +
              geom_boxplot(orientation = "y")  +
              theme(plot.title = element_text(hjust = 0.5),
                         title = element_text(size = 10, face = 'bold')) +
              theme_bw()
            return(p1 / p2)
          })
setGeneric("LargeGridTimeOfPoints", function(data)
  standardGeneric("LargeGridTimeOfPoints"))
setMethod("LargeGridTimeOfPoints", signature = c("CONNECTORData"),
          function(data) {
            select<-dplyr::select
            timeMeasure <- data@curves[, c("curvesID", "time")]
            timeCombination <- timeMeasure %>%
              group_by(curvesID) %>%
              tidyr::expand(Time1 = time, Time2 = time) %>%
              ungroup() %>%
              select(-curvesID)
            #group timeMeasure by Time1 and Time2 and count the number of elements in each group
            p3 <-  ggplot() +
              geom_point(data = timeCombinationSummarise(timeCombination), aes(Time1, Time2, col =
                                                                                 count / max(count))) +
              stat_density_2d(data = timeCombination,
                              aes(Time1, Time2, fill = after_stat(nlevel)),
                              geom = "polygon") +
              scale_fill_gradientn(colours = c("#baffc9", "#FF0000"),
                                   name = "Freq. of \nobservations") +
              scale_color_gradientn(colours = c("#baffc9", "#FF0000"),
                                    name = "Freq. of \nobservations") +
              theme_bw() +
              labs(title = "Grid of time points", x = "Time", y = "Time") +
              theme(plot.title = element_text(hjust = 0.5),
                    title = element_text(size = 12, face = 'bold')) +
              guides(color = "none")
            nTime <- data@curves %>%
              group_by(time, measureID) %>%
              summarise(distinct_IDs = n_distinct(curvesID), .groups = "drop")
            
            p1 <- ggplot(nTime, aes(x = time, y = distinct_IDs, fill = measureID)) +
              geom_bar(stat = "identity", position = "dodge") +
              labs(title = "Number of distinct IDs per time by MeasureID",
                   x = "Time",
                   y = "Number of distinct IDs") +
              theme(plot.title = element_text(hjust = 0.5),
                    title = element_text(size = 10, face = 'bold')) +
              theme_bw()
            #Distribuzione media nei tempi
            p2 <- ggplot(nTime, aes(x = time, fill = as.factor(measureID))) +
              geom_boxplot(orientation = "y")  +
              theme(plot.title = element_text(hjust = 0.5),
                    title = element_text(size = 10, face = 'bold')) +
              theme_bw()
            p1 <-
              p1 + coord_flip() + theme(
                legend.position = "left",
                legend.key.size = unit(0.3, "cm"),
                legend.title = element_text(size = 8)
              ) + scale_y_reverse()
            
            return(p1 + p3 + plot_spacer() + p2 + plot_layout(
              ncol = 2,
              widths = c(1, 5),
              heights = c(3, 1)
            ))
          })
setGeneric("timeCombinationSummarise", function(data)
  standardGeneric("timeCombinationSummarise"))
setMethod("timeCombinationSummarise", signature = c(), function(data) {
  return(data %>%
           group_by(Time1, Time2) %>%
           summarise(count = n(), .groups = "drop"))
})