#' Remove subjects with insufficient longitudinal observations
#'
#' Removes a subject when any of its observed measures contains fewer than the
#' requested number of time points. The resulting dimensions and time grids are
#' rebuilt with [ConnectorData()].
#'
#' @param data A [CONNECTORData-class] object.
#' @param minPoints Positive integer giving the minimum number of observations
#'   required for every subject-measure curve. Defaults to `3`.
#'
#' @return A [CONNECTORData-class] object containing the retained subjects. If
#'   no subject is removed, the original object is returned.
#'
#' @examples
#' \dontrun{
#' filtered <- filterData(data, minPoints = 4)
#' }
#'
#' @seealso [ConnectorData()], [truncate()]
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
