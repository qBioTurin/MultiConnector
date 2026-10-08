#' Classified longitudinal curves
#'
#' Stores posterior cluster-membership probabilities and assignments obtained by
#' applying a selected clustering model to new longitudinal curves. Objects are
#' created by [ClassificationCurves()].
#'
#' @slot ClassMatrix A data frame with one row per new subject, a `subjID`
#'   column, and one posterior-probability column per cluster.
#' @slot ClassMatrix_entropy A data frame containing cluster probabilities,
#'   entropy, maximum posterior probability, and the final cluster label (or
#'   `"Unclassified"`) for every subject.
#' @slot ClassificationData The [CONNECTORData-class] object containing the new
#'   curves.
#' @slot ClusteredData The [CONNECTORDataClustered-class] model used for
#'   classification.
#'
#' @seealso [ClassificationCurves()] and [plot()].
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

#' Classify new longitudinal curves
#'
#' Projects new curves onto a fitted functional clustering model and computes
#' their posterior probabilities of belonging to each existing cluster.
#'
#' @param data A [CONNECTORData-class] object containing the new curves. Its
#'   measures must be compatible with those in the fitted model.
#' @param CONNECTORDataClustered A [CONNECTORDataClustered-class] object used as
#'   the reference model.
#' @param cores Positive integer number of worker processes used across subjects.
#' @param entropyCutoff Numeric entropy threshold. A subject is assigned when its
#'   entropy is below this value or its largest posterior probability exceeds
#'   `probCutoff`.
#' @param probCutoff Numeric threshold for the largest posterior probability.
#'
#' @return A [CONNECTORDataClassified-class] object.
#'
#' @details
#' Subjects with fewer than two observations in any measure receive zero raw
#' likelihoods and cannot be classified reliably. Entropy is measured in bits.
#'
#' @examples
#' \dontrun{
#' classified <- ClassificationCurves(
#'   new_data,
#'   clustered_data,
#'   cores = 2,
#'   entropyCutoff = 1,
#'   probCutoff = 0.6
#' )
#' classified@ClassMatrix
#' plot(classified)
#' }
#'
#' @seealso [CONNECTORDataClassified-class], [selectCluster()], [plot()]
#'
#' @import parallel ggplot2
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange
#' @importFrom tidyr gather spread
#' @export
setGeneric("ClassificationCurves", function(data,
                                            CONNECTORDataClustered,
                                            cores = 1,
                                            entropyCutoff = 1, probCutoff = 0.6) {
  standardGeneric("ClassificationCurves")
})

#' @rdname ClassificationCurves
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
      tidyr::spread(key = "ClusterOld", value = "Prob") %>%
      rename(subjID = ID)

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

#' @rdname show
#' @aliases show,CONNECTORDataClassified-method
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
    sigma_rows <- unname(sigma[as.character(CData_i$measureID)])

    Sigma <- diag(sigma_rows, nrow = n) + Snew %*% Gamma %*% t(Snew)
    #Sigma <- sigma * diag(n) + Snew %*% Gamma %*% t(Snew)

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
