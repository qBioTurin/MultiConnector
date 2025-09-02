#' @title ConnectorData
#' @description ConnectorData is a function that imports the data from the excel file storing the growth evolution data and the annotation file.
#' 
#' @param TimeSeriesFile The name of the excel/csv file storing the  growth evolution data. Alternatively accepts a tibble with the data.
#'
#' @param AnnotationFile The name of a excel/csv file storing  the annotation data. Alternatively accepts a tibble with the annotations.
#'
#' @return ConnectorData returns a S4 object, called ConnectorData, with four arguments: (i) curves: tibble with 5 columns (subjID, measureID, time, curvesID, value) encoding the growth data for each sample, (ii) dimensions: the tibble reporting the number of observations collected per sample  (iii) annotations: the tibble matching the samples with their annotations, (iv) TimeGrid: the vector storing all the sample time points (i.e. time grid). Furthermore, it prints a brief summary of the input data, i.e. the total number of curves (samples), the minimum and the maximum curve length.
#' @examples 
#' TimeSeriesFile<-system.file("testdata", "test.xlsx", package = "ConnectorV2.0")
#' AnnotationFile <-system.file("testdata", "testinfo.txt", package = "ConnectorV2.0")
#' @details
#' create CONNECTORData like an object that contains 4 different slots curves dimensions annotations TimeGrid
#' curves is a tibble that contains subjID, measureID, time, curvesID (concatenation of measureID_subjID), value
#' dimensions is a tibble that contains the number of observations for each ID
#' annotations is a tibble that contains the ID of each curve
#' TimeGrid is a vector that contains the time points of the curves
#' @import readxl dplyr methods readr tibble tidyr
#' @importFrom magrittr %>%
#' @export

# Global variable declarations for R CMD check
if(getRversion() >= "2.15.1") utils::globalVariables(c("subjID", "measureID", "time", "curvesID", "nTimePoints", "."))


# Initialize method for proper S4 constructor
setMethod("initialize", "CONNECTORData",
          function(.Object, curves, dimensions, annotations, TimeGrids) {
            .Object@curves <- curves
            .Object@dimensions <- dimensions
            .Object@annotations <- annotations
            .Object@TimeGrids <- TimeGrids
            
            # Validation will be called automatically
            return(.Object)
          })

# Definition of generic for ConnectorData factory function
setGeneric("ConnectorData", function(TimeSeriesFile, AnnotationFile) {
  standardGeneric("ConnectorData")
})

# show method
setMethod("show", "CONNECTORData", function(object) {
  cat("CONNECTORData object with:\n")
  cat("- Curves:", nrow(object@curves), "\n")
  cat("- Subjects:", length(unique(object@curves$subjID)), "\n")
  cat("- Measures:", length(unique(object@curves$measureID)), "\n")
})

#' @rdname ConnectorData
#' @export
#Definition of method ConnectorData
setMethod("ConnectorData", signature ("character"),
          function(TimeSeriesFile, AnnotationFile) {
            #Read Data File
            
            if (grepl(".xlsx", TimeSeriesFile, ignore.case = TRUE) == TRUE ||
                grepl(".xls", TimeSeriesFile, ignore.case = TRUE) == TRUE) {
              curves <- readxl::read_excel(TimeSeriesFile)
            }
            else if (grepl(".txt", TimeSeriesFile, ignore.case = TRUE) == TRUE) {
              curves <-
                readr::read_csv(TimeSeriesFile,
                                col_names = TRUE,
                                show_col_types = FALSE)
            }
            else {
              (warning("The TimeSeriesFile must be an excel or csv file"))
              return()
            }
            if (grepl(".xlsx", AnnotationFile, ignore.case = TRUE) == TRUE ||
                grepl(".xls", AnnotationFile, ignore.case = TRUE) == TRUE) {
              annotations <- readxl::read_excel(AnnotationFile)
            }
            else if (grepl(".txt", AnnotationFile, ignore.case = TRUE) == TRUE) {
              annotations <-
                readr::read_csv(AnnotationFile,
                                col_names = TRUE,
                                show_col_types = FALSE)
            }
            else {
              (stop("The AnnotationFile must be an excel or csv file"))
            }
            return(ConnectorData(curves, annotations))
            
          })
          
#' @rdname ConnectorData
#' @import readxl dplyr methods readr tibble magrittr tidyr
#' @export
setMethod("ConnectorData", signature("tbl_df"),
          function(TimeSeriesFile, AnnotationFile) {
            select<-dplyr::select
            curves <- TimeSeriesFile
            annotations <- AnnotationFile
            
            
            #Check if column "time" and "ID" are present, but just once. if not throw a error and quit. time could be lowercase or uppercase
            if (sum(grepl("time", tolower(colnames(curves)), ignore.case = TRUE)) != 1) {
              stop(
                "The column name 'time' is not present or is present more than once in the TimeSeriesFile"
              )
              return()
            }
            #trasform the column "time" in lowercase
            colnames(curves)[grepl("time", tolower(colnames(curves)), ignore.case = TRUE)] <-
              "time"
            
            if (sum(colnames(curves) == "subjID") != 1) {
              stop("The column name 'subjID' is not present or is present more than once in the TimeSeriesFile")
              return()
            }
            
            if (sum(colnames(curves) == "value") != 1) {
              stop("The column name 'value' is not present or is present more than once in the TimeSeriesFile")
              return()
            }
            
            if (sum(colnames(curves) == "measureID") != 1) {
              stop("The column name 'measureID' is not present or is present more than once in the TimeSeriesFile")
              return()
            }
            
            #remove rows in curves where ID or time or both are NA
            curves <-
              curves[complete.cases(curves[, c("subjID", "time", "measureID")]),]
            
            if (sum(colnames(annotations) == "subjID") != 1) {
              stop("The column name 'subjID' is not present or is present more than once in the AnnotationFile")
            }
            #check if the ID in the time series file are all present in the annotation file
            if (all(curves$subjID %in% annotations$subjID) == FALSE) {
              IDMISS <- setdiff(unique(curves$subjID), annotations$subjID)
              annotations <- add_row(annotations, subjID = IDMISS)
              warning(
                "The subjID in the TimeSeriesFile are not all present in the AnnotationFile. The missing ID are added to the AnnotationFile"
              )
            }
            #check if the ID in the annotation file are unique
            if (length(unique(annotations$subjID)) != length(annotations$subjID)) {
              stop("The subjIDs in the AnnotationFile are not unique")
            }
            #create a column callec curvesID that is composed by subjID measureId divided by a "_"
            curves$curvesID <- paste(curves$subjID, curves$measureID, sep = "_")
            #check if all rows have a unique combination of curvesID and time
            if (nrow(unique(curves[, c("curvesID", "time")])) != nrow(curves)) {
              stop(
                "Samples with multiple observations at the same time point are present. One observation per time point is required"
              )
            }
            
            #remove lines in annotation if same ID is not present in curves
            annotations <-
              annotations[annotations$subjID %in% curves$subjID,]
            #remove all lanes with less than 2 values
            curves <- curves[rowSums(!is.na(curves)) > 2, ]
            
            ### Inizialize :
            
            ### vector for curves lenghts
            
            dimensions    <- curves %>%
              select(-time, -subjID, -measureID) %>%
              group_by(curvesID) %>%
              summarise_all(~ sum(!is.na(.)))
            if (any(dimensions$value <= 2)) {
              stop(
                "Some curves have less than 2 observations. Please check the data. The curves with less than 2 observations are: ",
                paste(dimensions$curvesID[dimensions$value < 2], collapse = ", ")
              )
            }
            names(dimensions)[names(dimensions) == "value"] <- "nTimePoints"
            #order the column placing ID and time at the beginning
            curves <- curves[, c("subjID", "measureID", "time", "curvesID", setdiff(names(curves), c("subjID", "measureID", "time", "curvesID")))]
            #create a tibble with all columns of curves from the 3rd
            #ObsValue <- as_tibble(curves[, 3:ncol(curves)])
            
            #arrange the tibble by ID and time
            # curves = curves %>%
            #   arrange(ID, time)
            J <- length(unique(curves$measureID))
            #M è il vettore contente le misure, applicando l'unique
            M <- sort(unique(curves$measureID))
            
            grid <- list()
            
            for (j in 1:J) {
              a <- sort(unique(curves$time[curves$measureID == M[j]]))
              grid[[as.character(M[j])]] <- a
            }
            
            #set slots of CONNECTORData
            ConnectorData <-
              new(
                "CONNECTORData",
                curves = curves,
                dimensions = dimensions,
                annotations = annotations,
                TimeGrids = grid
              )
            
            
             if(length(unique(curves$measureID))==1){
               cat("############################### \n######## Summary ##############\n")
               cat("\n Number of curves:")
               print(dimensions %>%
                       select(-curvesID) %>%
                       summarise_all(function(x)
                         sum(x != 0, na.rm = TRUE)))
               cat(";\n Min curve length: ")
               print(dimensions %>%
                       select(-curvesID) %>%
                       summarise_all(function(x)
                         min(x, na.rm = TRUE)))
               
               cat("; Max curve length: ")
               print(dimensions %>%
                       select(-curvesID) %>%
                       summarise_all(function(x)
                         max(x, na.rm = TRUE)))
               
               cat("############################### \n")
             }
            else{
              cat("############################### \n")
              cat("Data loaded...\n")
              cat("Number of curves:")
              cat(sum(dimensions %>%
                      select(-curvesID) %>%
                      unlist()!=0))
              cat("\nNumber of distinct measures:")
              cat(curves$measureID %>% unique() %>% length())
              
              cat("\nAverage length:")
              cat(dimensions %>%
                      select(-curvesID) %>%
                      unlist() %>%
                      mean())
              cat("\nMeasure with highest length:")
              print(dimensions %>%
                      filter(nTimePoints==max(nTimePoints)))
              cat("\nMeasure with lowest length:")
              print(dimensions %>%
                      filter(nTimePoints==min(nTimePoints)))
            }
            
            
            return(ConnectorData)
          })
