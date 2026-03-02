#' ClassificationCurves
#'
#' @description ...
#'
#'
#'
#' @param data CONNECTORData. See CONNECTORData for details.
#' @param CONNECTORDataClustered Configuration choosen by ConfigSelection()
#' @param cores number of cores used
#' @param entropyCutoff ...
#' @param probCutoff ...
#'
#' @return ...
#'
#' @seealso CONNECTORDataClustered()
#'
#' @import parallel ggplot2
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange
#' @importFrom tidyr gather spread
#' @export
#'

#' @seealso CONNECTORDataClustered()
#'
#' CONNECTORDataClassified
#' @description Objects of this class hold the results of classifying new curves into existing clusters.
#' @slot ClassMatrix Data frame with classification probabilities for each subject.
#' @slot ClassMatrix_entropy Data frame with classification results including entropy and certainty thresholds.
#' @slot ClassificationData The CONNECTORData object containing the new curves.
#' @slot ClusteredData The CONNECTORDataClustered model used for classification.
#' @export
setClass(
  "CONNECTORDataClassified",
  slots = list(
    ClassMatrix = "data.frame",
    ClassMatrix_entropy = "data.frame",
    ClassificationData = "CONNECTORData",
    ClusteredData = "CONNECTORDataClustered"
  )
)

setGeneric("ClassificationCurves", function(data,
                                            CONNECTORDataClustered,
                                            cores = 1,
                                            entropyCutoff = 1, probCutoff = 0.6) {
  standardGeneric("ClassificationCurves")
})

#' @export
setMethod(
  "ClassificationCurves", signature(data = "CONNECTORData", CONNECTORDataClustered = "CONNECTORDataClustered"),
  function(data,
           CONNECTORDataClustered,
           cores = 1,
           entropyCutoff = 1, probCutoff = 0.6) {
    if (!inherits(CONNECTORDataClustered, "CONNECTORDataClustered")) {
      stop("CONNECTORDataClustered must be of class 'CONNECTORDataClustered'. Current class: ", class(CONNECTORDataClustered))
    }
    if (!inherits(data, "CONNECTORData")) {
      stop("data must be of class 'CONNECTORData'. Current class: ", class(data))
    }

    CData <- data@curves
    CData$jamesID <- as.integer(factor(CData$subjID, levels = unique(CData$subjID)))
    M <- sort(unique(CData$measureID))

    nworkers <- detectCores()
    if (nworkers < cores) cores <- nworkers

    parameters <- CONNECTORDataClustered@CfitandParameters$cfit$parameters

    grid <- CONNECTORDataClustered@KData$TimeGrids

    Nclusters <- length(CONNECTORDataClustered@cluster.names)

    CONNECTORDataClustered@KData$CData$cluster <- CONNECTORDataClustered@CfitandParameters$pred$class.pred[CONNECTORDataClustered@KData$CData$jamesID]

    FullS <- CONNECTORDataClustered@KData$FullS
    sigma <- parameters$sigma
    J <- length(unique(CONNECTORDataClustered@KData$CData$measureID))

    Gamma <- parameters$Gamma
    Lambda <- parameters$Lambda
    alpha <- parameters$alpha
    lambda.zero <- as.vector(parameters$lambda.zero)
    Lambda.alpha <- lambda.zero + Lambda %*% t(alpha)

    # Lets calculate the new S of the new curves

    newGrid <- data@TimeGrids

    Snew <- lapply(M, function(j) {
      FullSm <- FullS[[j]]
      Gridm <- grid[[j]]
      NewGridm <- newGrid[[j]]
      pm <- dim(FullSm)[2]

      Snew <- matrix(1, ncol = pm, nrow = length(NewGridm))
      Snew[, 1:pm] <- sapply(1:pm, function(i) stats::spline(x = Gridm, y = FullSm[, i], xout = NewGridm)$y)
      Snew
    })
    names(Snew) <- M

    IDcurves <- unique(CData$subjID)
    clusterNames <- getClusterNames(CONNECTORDataClustered)

    cl <- makeCluster(cores)
    clusterCall(cl, function() {
      library(dplyr)
      library(ggplot2)
      library(mvtnorm)
      library(tidyr)
    })
    clusterExport(cl, list(
      "CData", "Lambda.alpha", "Snew", "sigma",
      "Gamma", "ClassificationSingleCurve", "Nclusters", "M"
    ), envir = environment())

    ALL.runs <-
      parLapply(cl, IDcurves, function(x_id) {
        tryCatch(
          {
            do.call(
              rbind,
              lapply(M, function(j) {
                CData_x <- CData %>% filter(subjID == x_id, measureID == j)
                CData_x$timeindex <- match(CData_x$time, newGrid[[j]])
                CData_x
              })
            ) -> CData_i

            ClassificationSingleCurve(CData_i,
              Snew,
              Gamma = Gamma,
              sigma = sigma,
              Lambda.alpha = Lambda.alpha,
              Nclusters = Nclusters,
              CONNECTORDataClustered = CONNECTORDataClustered
            )
          },
          error = function(e) {
            err <- paste("ERROR:", conditionMessage(e), "\n")
            err.list <- list(Error = err)
            return(err.list)
          }
        )
      })

    stopCluster(cl)

    names(ALL.runs) <- paste0("ID_", IDcurves)

    # Extract weights (probabilities)
    df <- as.data.frame(t(sapply(ALL.runs, function(x) x$prob)), row.names = F)
    df$ID <- IDcurves
    df <- df %>% relocate(ID)

    # Entropy calculation
    df_Entrop <- df %>%
      tidyr::gather(-ID, key = "Cluster", value = "Prob") %>%
      group_by(ID) %>%
      mutate(
        Cluster = clusterNames[as.numeric(Cluster)],
        Entropy = -sum(Prob * log2(Prob + 1e-10)), # Added small epsilon for stability
        MajorProb = max(Prob)
      ) %>%
      mutate(
        ClusterOld = Cluster,
        Cluster = ifelse(!is.na(Entropy) & (Entropy < entropyCutoff | MajorProb > probCutoff), Cluster[which(Prob == MajorProb)], "Unclassified")
      ) %>%
      ungroup() %>%
      tidyr::spread(key = "ClusterOld", value = "Prob")

    colnames(df) <- c("subjID", clusterNames)

    # Create the S4 object
    result <- new("CONNECTORDataClassified",
      ClassMatrix = df,
      ClassMatrix_entropy = df_Entrop,
      ClassificationData = data,
      ClusteredData = CONNECTORDataClustered
    )

    return(result)
  }
)

setMethod("show", signature(object = "CONNECTORDataClassified"), function(object) {
  cat("CONNECTORDataClassified Object\n")
  cat("------------------------------\n")
  cat(
    "Classification of", nrow(object@ClassMatrix), "subjects using model with",
    length(getClusterNames(object@ClusteredData)), "clusters.\n\n"
  )

  cat("Cluster Assignment Summary:\n")
  print(table(object@ClassMatrix_entropy$Cluster))

  cat("\nMean major probability:", round(mean(object@ClassMatrix_entropy$MajorProb), 3), "\n")
  cat("Mean entropy:", round(mean(object@ClassMatrix_entropy$Entropy, na.rm = TRUE), 3), "\n")
})

ClassificationSingleCurve <- function(CData_i, Snew, Gamma, sigma, Lambda.alpha, Nclusters, CONNECTORDataClustered) {
  M <- sort(unique(CData_i$measureID))
  J <- length(M)
  pi <- CONNECTORDataClustered@CfitandParameters$cfit$parameters$pi
  for (j in 1:J) {
    A <- Snew[[j]][CData_i$timeindex[(CData_i$measureID == M[j])], ]
    if (j == 1) {
      Si <- A
    } else {
      Si <- bdiag(Si, A)
    }
  }
  Si <- as.matrix(Si)

  ##
  Pcl <- lapply(1:Nclusters, function(i, Snew, sigma, Gamma, Lambda.alpha, CData_i, params_pi) {
    MinL <- CData_i %>%
      group_by(measureID) %>%
      count() %>%
      ungroup() %>%
      summarise(m = min(n)) %>%
      pull(m)

    if (MinL < 2) {
      return(data.frame(log_pi = 0, pi = 0, cluster = i))
    }

    ## defining the new spline basis matrix S

    x <- CData_i$value
    n <- length(x)
    Sigma <- sigma * diag(n) + Snew %*% Gamma %*% t(Snew)

    mu_i <- Snew %*% Lambda.alpha[, i] # (par$lambda.zero + par$Lambda %*% alphai )

    pi_val <- mvtnorm::dmvnorm(
      x = x,
      mean = mu_i,
      sigma = Sigma
    )

    log_pi <- mvtnorm::dmvnorm(
      x = x,
      mean = mu_i,
      sigma = Sigma, log = T
    ) + log(params_pi[i])

    return(data.frame(log_pi = log_pi, pi = pi_val, cluster = i))
  }, Snew = Si, sigma = sigma, Gamma = Gamma, Lambda.alpha = Lambda.alpha, CData_i = CData_i, params_pi = pi)

  Pcl <- do.call(rbind, Pcl)

  ## calculate the probs to belong in the clusters

  Pcl$class <- Pcl$pi * pi / sum(Pcl$pi * pi)

  CData_i$cluster <- Pcl$cluster[which.max(Pcl$log_pi)]
  Pclass <- Pcl$class
  names(Pclass) <- Pcl$cluster

  return(list(weight = Pcl, prob = Pclass))
}
