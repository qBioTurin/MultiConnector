#' @title Maximum Discrimination Function
#'
#' @description
#' Visualizes the h curve(s) representing the optimal weights to apply to each dimension for determining the cluster membership.
#' The measures in the plots are ordered by the sum of the absolute areas under their respective discrimination curves,
#' meaning the most influential measures from a clustering perspective appear first.
#'
#' @param CONNECTORDataClustered The CONNECTORDataClustered object obtained from selectCluster function.
#' @param absvalue If TRUE, the absolute values of the weights are plotted.
#' @return
#' MaximumDiscriminationFunction returns a list containing:
#' \itemize{
#'   \item \code{DiscrFunctionsPlot}: A ggplot object showing overlapping discriminant functions for each measure in a grid.
#'   \item \code{Separated}: A ggplot object showing separated discriminant functions, ordered by their total area (importance).
#'   \item \code{measure_areas}: A data frame containing the calculated areas for each measure and function, representing their relative importance in clustering.
#' }
#'
#' @author Cordero Francesca, Pernice Simone, Sirovich Roberta
#'
#' @references
#' Gareth M. James and Catherine A. Sugar, (2003). Clustering for Sparsely Sampled Functional Data. Journal of the American Statistical Association.
#'
#'
#'
#' @import ggplot2
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter
#' @export
#'
setGeneric("MaximumDiscriminationFunction", function(CONNECTORDataClustered, absvalue = TRUE) {
  standardGeneric("MaximumDiscriminationFunction")
})

setMethod("MaximumDiscriminationFunction", signature(CONNECTORDataClustered = "CONNECTORDataClustered"), function(CONNECTORDataClustered, absvalue = TRUE) {
  if (!inherits(CONNECTORDataClustered, "CONNECTORDataClustered")) {
    stop("Input must be of class 'CONNECTORDataClustered'. Current class: ", class(CONNECTORDataClustered))
  }

  parameters <- CONNECTORDataClustered@CfitandParameters$cfit$parameters
  DiscriminantResults <- list()
  FullS <- CONNECTORDataClustered@KData$FullS
  sigma <- parameters$sigma
  M <- getMeasures(CONNECTORDataClustered)

  for (j in M) {
    A <- FullS[[j]]
    if (j == M[[1]]) {
      Si <- A
    } else {
      Si <- bdiag(Si, A)
    } # Questo if è necessario poiché bdiag ha bisogno di una matrice iniziale su cui attaccarsi (ovvero quella della prima misura)
  }

  S <- as.matrix(Si)

  nt <- nrow(S)
  Gamma <- parameters$Gamma

  Sigma <- S %*% Gamma %*% t(S) + sigma * diag(nt)
  Lambda <- parameters$Lambda
  discrim <- as.data.frame(solve(Sigma) %*% S %*% Lambda)

  colnames(discrim) <- paste0("DiscrFunc", 1:ncol(discrim))


  if (absvalue) {
    discrim <- abs(discrim)
  }
  n <- ncol(discrim)
  DiscrList <- lapply(1:n, function(x) {
    data.frame(
      Time = unlist(CONNECTORDataClustered@KData$TimeGrids),
      DiscrFunc = discrim[, x],
      DiscrNumber = paste0("DiscrFunc", x)
    )
  })

  q <- lapply(M, function(i) {
    rep(i, length(CONNECTORDataClustered@KData$TimeGrids[[i]]))
  })

  measureID <- unlist(q)

  # Aggiungi measureID a tutti gli elementi di DiscrList (non solo al primo)
  for (i in 1:length(DiscrList)) {
    DiscrList[[i]]$measureID <- measureID
  }
  DiscrFrame <- do.call("rbind", DiscrList)

  # Reorder measureID based on the sum of areas under the curves
  measure_areas <- DiscrFrame %>%
    group_by(measureID, DiscrNumber) %>%
    arrange(Time, .by_group = TRUE) %>%
    summarise(Area = sum(diff(Time) * (abs(DiscrFunc[-1]) + abs(DiscrFunc[-length(DiscrFunc)])) / 2), .groups = "drop") %>%
    group_by(measureID) %>%
    summarise(TotalArea = sum(Area), .groups = "drop") %>%
    arrange(desc(TotalArea))

  DiscrFrame$measureID <- factor(DiscrFrame$measureID, levels = measure_areas$measureID)

  DiscriminantFunctions <- list()
  # TODO: Se più di una discr sovrapporle. DOVREBBE GIA FARLO
  DiscriminantFunctions$DiscrFunctionsPlot <-
    ggplot(
      data = DiscrFrame,
      aes(
        x = Time,
        y = DiscrFunc,
        col = DiscrNumber,
        linetype = DiscrNumber
      )
    ) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(~measureID) +
    xlab("Time") +
    ylab("Discriminant Function value")

  DiscriminantFunctions$Separated <-
    ggplot(data = DiscrFrame, aes(x = Time, y = DiscrFunc)) +
    facet_grid(measureID ~ DiscrNumber) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlab("Time") +
    ylab("Discriminant Function value")+
    theme_bw()

  DiscriminantFunctions$measure_areas <- measure_areas

  return(DiscriminantFunctions)
})
