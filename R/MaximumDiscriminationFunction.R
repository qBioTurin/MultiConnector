#' Maximum Discrimination Function
#'
#'@description
#' Visualizes the h curve(s) representing the optimal weights to apply to each dimension for determining the cluster membership.
#'
#' @param clusterdata The list obtained from extrapolating the most probable clustering from the StabilityAnalysis function output. (see \code{\link{StabilityAnalysis}} and \code{\link{MostProbableClustering.Extrapolation}}).
#' @param absvalue If TRUE, the absolute values of the weights are plotted.
#'  @return
#' MaximumDiscriminationFunction generates two plots as ggplot objects, showing
#' the weights applied to each dimension for determining the cluster membership, plotted as a single curve.
#'
#' @author Cordero Francesca, Pernice Simone, Sirovich Roberta
#'
#' @references
#' Gareth M. James and Catherine A. Sugar, (2003). Clustering for Sparsely Sampled Functional Data. Journal of the American Statistical Association.
#'
#' @examples
#'
#'
#' @import ggplot2 dplyr Matrix
#' @export
#'
setGeneric("MaximumDiscriminationFunction", function(ConfigChosen, absvalue =
                                                       TRUE)
  standardGeneric("MaximumDiscriminationFunction"))
#' @rdname DataTruncation
#' @export
setMethod("MaximumDiscriminationFunction", signature(), function(ConfigChosen, absvalue =
                                                                           TRUE)
{
  parameters <- ConfigChosen@CfitandParameters$cfit$parameters
  DiscriminantResults <- list()
  FullS = ConfigChosen@KData$FullS
  sigma <- parameters$sigma
  J <- length(unique(ConfigChosen@KData$CData$measureID))
  
  
  for (j in 1:J) {
    A <- FullS[[j]]
    if (j == 1) {
      Si <- A
    }
    else{
      Si <- bdiag(Si, A)
    }#Questo if è necessario poiché bdiag ha bisogno di una matrice iniziale su cui attaccarsi (ovvero quella della prima misura)
  }
  
  S <- as.matrix(Si)
  
  nt <- nrow(S)
  Gamma <- parameters$Gamma
  
  Sigma <- S %*% Gamma %*% t(S) + sigma * diag(nt)
  Lambda <- parameters$Lambda
  discrim <- as.data.frame(solve(Sigma) %*% S %*% Lambda)
  
  colnames(discrim) <- paste0("DiscrFunc", ncol(discrim))
  
  
  
  if (absvalue)
    discrim <- abs(discrim)
  n <- ncol(discrim)
  
  DiscrList <- lapply(1:n, function(x)
    data.frame(
      Time = unlist(ConfigChosen@KData$TimeGrids),
      DiscrFunc = discrim[, x],
      DiscrNumber = paste0("DiscrFunc", x)
    ))
  
  q <- sapply(1:length(ConfigChosen@KData$TimeGrids), function(i) {
    rep(names(ConfigChosen@KData$TimeGrids[i]), length(ConfigChosen@KData$TimeGrids[[i]]))
  })
  DiscrList[[1]]$measureID <- unlist(q)
  DiscrFrame <- do.call("rbind", DiscrList)
  
  DiscriminantFunctions <- list()
  #TODO: Se più di una discr sovrapporle. DOVREBBE GIA FARLO
  DiscriminantFunctions$DiscrFunctionsPlot <-
    ggplot(data = DiscrFrame,
           aes(
             x = Time,
             y = DiscrFunc,
             col = DiscrNumber,
             linetype = DiscrNumber
           )) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid( ~ measureID) +
    xlab("Time") + ylab("Discriminant Function value")
  
  DiscriminantFunctions$Separated <-
    ggplot(data = DiscrFrame, aes(x = Time, y = DiscrFunc)) +
    facet_grid(measureID ~ DiscrNumber, scales = "free_y") +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlab("Time") + ylab("Discriminant Function value")
  
  
  
  return(DiscriminantFunctions)
  
})
