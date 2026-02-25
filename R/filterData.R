#' @title filterData
#'
#' @description Filters the CONNECTORData object by removing subjects that have less than a certain number of points in at least one of their measurements.
#'
#' @param data CONNECTORData. (see \code{\link{ConnectorData}})
#' @param minPoints Minimum number of points required for each measure. If a subject has fewer than this value in any of its measurements, it will be removed. Default is 3.
#' @return Returns a new CONNECTORData object with the filtered subjects.
#'
#' @import dplyr
#' @export
#' @rdname filterData

setGeneric("filterData", function(data, minPoints = 3) standardGeneric("filterData"))

#' @rdname filterData
#' @export
setMethod(
    "filterData", signature("CONNECTORData"),
    function(data, minPoints = 3) {
        # Identify subjects to remove
        subjIDToRemove <- data@curves %>%
            group_by(subjID, measureID) %>%
            summarise(nPoints = n(), .groups = "drop") %>%
            filter(nPoints < minPoints) %>%
            pull(subjID) %>%
            unique()

        if (length(subjIDToRemove) > 0) {
            cat("############################### \n")
            cat("####### Filtering Summary ######\n")
            cat("Removing", length(subjIDToRemove), "subjects due to insufficient points (<", minPoints, ") in at least one measure.\n")
            cat("Removed IDs:", paste(subjIDToRemove, collapse = ", "), "\n")
            cat("############################### \n")

            # Filter curves and annotations
            filtered_curves <- data@curves %>% filter(!subjID %in% subjIDToRemove)
            filtered_annotations <- data@annotations %>% filter(!subjID %in% subjIDToRemove)

            # Rebuild the CONNECTORData object
            # Using ConnectorData constructor for consistent updates of TimeGrids and dimensions
            invisible(capture.output(
                data_filtered <- ConnectorData(filtered_curves, filtered_annotations)
            ))

            return(data_filtered)
        } else {
            cat("No subjects found with less than", minPoints, "points. Returning original data.\n")
            return(data)
        }
    }
)
