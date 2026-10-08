#' Truncate longitudinal curves to a time interval
#'
#' Keeps observations within a user-defined time interval for selected measures.
#' Subjects that have no retained observations in a selected measure are removed
#' consistently from the other measures.
#'
#' @param data A [CONNECTORData-class] object.
#' @param feature,labels Deprecated placeholders retained in the generic for
#'   compatibility. They are not used by the `CONNECTORData` method.
#' @param truncTime A numeric value or a numeric vector of length two. A single
#'   value is interpreted as the upper bound, with the observed minimum time as
#'   the lower bound. Two values define the lower and upper bounds. If `NULL`, no
#'   temporal restriction is applied.
#' @param measure Character vector naming the measures to truncate. `NULL`
#'   applies the operation to all measures.
#'
#' @return A new [CONNECTORData-class] object containing the retained
#'   observations. A truncation summary is printed as a side effect.
#'
#' @examples
#' \dontrun{
#' early <- truncate(data, truncTime = 30)
#' window <- truncate(data, truncTime = c(5, 30), measure = "marker")
#' }
#'
#' @seealso [truncatePlot()], [filterData()]
#'
#' @import ggplot2 tibble dplyr tidyr
#' @export

setGeneric("truncate", function(data,
                                feature = NULL,
                                truncTime = NULL,
                                labels = NULL,
                                measure = NULL) {
  standardGeneric("truncate")
})

#' @rdname truncate
setMethod(
  "truncate",
  signature("CONNECTORData"), function(data,
                                       truncTime = NULL,
                                       measure = NULL) {
    if (is.null(measure)) {
      measure <- getMeasures(data)
    }else{
      if( all(measure %in% getMeasures(data)) == FALSE){
        stop("The measure indicated is not present in the dataset. Please check the measure name and try again.")
      }
    }
    
    # Separate curves to truncate and curves to keep
    curves_to_trunc <- data@curves %>% filter(measureID %in% measure)
    curves_to_keep <- data@curves %>% filter(!(measureID %in% measure))
    
    # Truncate selected curves
    if (nrow(curves_to_trunc) > 0) {
      # Create a temporary CONNECTORData for truncation
      # We use a subset of curves but keep all annotations
      temp_data <- new("CONNECTORData",
                       curves = curves_to_trunc,
                       annotations = data@annotations,
                       dimensions = data@dimensions %>% filter(curvesID %in% curves_to_trunc$curvesID),
                       TimeGrids = data@TimeGrids[names(data@TimeGrids) %in% measure]
      )
      
      dataTr_obj <- DataTrunc(temp_data, truncTime = truncTime)
      curves_truncated <- dataTr_obj@curves
    } else {
      curves_truncated <- curves_to_trunc
    }
    
    # Combine all curves
    curves_to_keep = curves_to_keep %>% filter(subjID %in% unique(curves_truncated$subjID) )
    all_curves <- bind_rows(curves_to_keep, curves_truncated)
    
    # Reconstruct using ConnectorData for robustness (updates dimensions and TimeGrids)
    invisible(capture.output(
      dataTr <- ConnectorData(all_curves, data@annotations %>% filter(subjID %in% unique(all_curves$subjID) )  )
    ))
    
    cat("############################### \n######## Summary ##############\n")
    cat("\n Number of curves cutted:\n")
    # Using data.frame to ensure nice printing of the summary
    summary_df_new <- dataTr@curves %>% 
      group_by(measureID, subjID) %>% 
      summarise(len_curves_after = n(), .groups = "drop") 
    summary_df_old <-data@curves %>% 
      group_by(measureID, subjID) %>% 
      summarise(len_curves_before = n(), .groups = "drop") 
    
    summary_df <- merge(summary_df_old, summary_df_new, by = c("subjID", "measureID") )
    summary_df <- summary_df %>%
      mutate(cutted = len_curves_before - len_curves_after) %>%
      group_by(measureID) %>%
      summarise(Number_subjID_cutted = sum(cutted!=0), .groups = "drop")
    print(as.data.frame(summary_df))
     
    cat("\n Min/Max curve length per measure after cutting:\n")
    len_summary <- dataTr@curves %>%
      group_by(measureID, subjID) %>%
      summarise(len = n(), .groups = "drop_last") %>%
      summarise(min_len = min(len), max_len = max(len), .groups = "drop")
    print(as.data.frame(len_summary))
    
    cat("\n Min/Max curve length per measure before cutting:\n")
    len_summary <- data@curves %>%
      group_by(measureID, subjID) %>%
      summarise(len = n(), .groups = "drop_last") %>%
      summarise(min_len = min(len), max_len = max(len), .groups = "drop")
    print(as.data.frame(len_summary))
    
    cat("############################### \n")
    
    return(dataTr)
  }
)

setGeneric("DataTrunc", function(data,
                                 truncTime = NULL) {
  standardGeneric("DataTrunc")
})
setMethod("DataTrunc",
          signature = c("CONNECTORData"),
          function(data,
                   truncTime = NULL) {
            #### Truncates the growth data at a specific time point chosen by the user.
            # Variables inizialization
            
            select <- dplyr::select
            dataset <- data@curves
            annotations = data@annotations
            # Data truncation
            
            if (!is.null(truncTime)) {
              maxTime <- max(dataset$time)
              minTime <- min(dataset$time)
              
              if (length(truncTime) > 1) {
                maxTruncTime <- max(truncTime)
                minTruncTime <- min(truncTime)
              } else {
                minTruncTime <- minTime
                maxTruncTime <- truncTime
              }
              
              if (maxTime < maxTruncTime) {
                warning("Max truncation time greater than maximum time in the dataset.")
              }
              if (minTime > minTruncTime) {
                warning("Min truncation time smaller than minimum time in the dataset.")
              }
              
              datasetTr <-
                dataset[dataset$time <= maxTruncTime &
                          dataset$time >= minTruncTime, ]
              
              subjIDToRemove <- datasetTr %>%
                select(-time) %>%
                group_by(curvesID) %>%
                mutate(nTimePoints = sum(!is.na(value))) %>% 
                ungroup() %>%
                select(-value)  %>%
                filter( nTimePoints < 2) %>%
                pull(subjID)%>% unique()
              
              if(length(subjIDToRemove) == length(datasetTr %>% pull(subjID) %>% unique() ) ){
                stop("No curves left after truncation. Please check the truncation time and try again.")
              }
              else if (length(subjIDToRemove) != 0) {
                warning(
                  paste0(
                    "subjID with one point after truncation will be removed: ",
                    paste(subjIDToRemove, collapse = ", ")
                  )
                )
                datasetTr <- datasetTr %>% filter(! subjID %in% subjIDToRemove )
                annotations = annotations %>% filter(! subjID %in% subjIDToRemove )
              }
              
              # Update TimeGrids for all measures
              timegridTr <- lapply(data@TimeGrids, function(tg) {
                tg[tg <= maxTruncTime & tg >= minTruncTime]
              })
              
              invisible(capture.output( dataTr <- ConnectorData( datasetTr, annotations ) ))
              
            } else {
              dataTr <- data
            }
            
            
            return(dataTr)
            ##### The CONNECTORList updated with the following arguments: a data frame with three variables (ID curves, observation and time values truncated at the chosen time), a vector collecting the number of truncated observations collected per sample, a data frame with curves labeled according to target file feature chosen and a vector for overall truncated time grid.
          }
)
