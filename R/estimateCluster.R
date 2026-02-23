#' @title Estimate Clustering Parameters and Solutions
#'
#' @description
#' Performs comprehensive clustering analysis using the Functional Clustering Model approach.
#' This function fits multiple clustering solutions with different numbers of clusters and
#' parameters, running multiple iterations to ensure stability and robustness. The method
#' is particularly suited for functional/longitudinal data where curves are clustered based
#' on their shape and temporal patterns.
#'
#' @param CONNECTORData A CONNECTORData object created with \code{\link{ConnectorData}}.
#'   Contains the time series data, annotations, and time grids to be clustered.
#' @param G Integer vector specifying the range of cluster numbers to evaluate (e.g., 2:6).
#'   The algorithm will test each specified number of clusters.
#' @param p Named vector or single integer specifying the dimension of the natural cubic
#'   spline basis for each measurement type. Higher values capture more complex curve shapes
#'   but may lead to overfitting. See \code{\link{estimatepDimension}} for guidance.
#' @param h Projection dimension parameter. Must be ≤ min(p, G-1) for identifiability.
#'   Lower values provide simpler representations but may lose important curve features.
#'   Default is typically 2 or 3.
#' @param runs Integer specifying the number of random initializations for each parameter
#'   combination. More runs improve stability but increase computation time. Default: 50.
#' @param seed Integer seed for reproducible results. Ensures consistent random initializations
#'   across runs.
#' @param cores Integer specifying the number of CPU cores for parallel computation.
#'   Speeds up analysis for large datasets or many parameter combinations.
#' @param PercPCA Numeric (0-1) specifying the minimum percentage of variance to retain
#'   in PCA preprocessing. Default: 0.85 (85% of variance retained).
#' @param MinErrFreq Minimum error frequency threshold for stability assessment.
#' @param pert Perturbation parameter for numerical stability in optimization.
#'
#' @return A comprehensive list containing:
#'   \itemize{
#'     \item \code{Clusterings}: List of all clustering solutions for each G and run
#'     \item \code{QualityMetrics}: Data frame with quality metrics (fDB, silhouette, etc.)
#'       for each solution
#'     \item \code{ConsensusInfo}: Consensus matrices and stability measures across runs
#'     \item \code{Parameters}: Summary of all tested parameter combinations
#'     \item \code{seed}: The random seed used for reproducibility
#'   }
#'
#' @details
#' \strong{Clustering Approach:}
#' The function implements functional clustering based on the Sugar & James model, which:
#' \itemize{
#'   \item Projects curves onto a lower-dimensional space using spline coefficients
#'   \item Clusters curves based on their projected representations
#'   \item Accounts for within-curve correlation and measurement error
#' }
#'
#' \strong{Parameter Selection Guidelines:}
#' \itemize{
#'   \item \strong{G}: Start with 2-6 clusters, extend based on domain knowledge
#'   \item \strong{p}: Use \code{estimatepDimension()} to find optimal spline dimensions
#'   \item \strong{h}: Usually 2-3, must be ≤ min(p, G-1)
#'   \item \strong{runs}: 50-100 for final analysis, 10-20 for exploration
#' }
#'
#' \strong{Quality Assessment:}
#' Multiple quality metrics are computed including functional Data Depth (fDB),
#' silhouette scores, and stability measures to help identify optimal solutions.
#'
#' @examples
#' \dontrun{
#' # Basic clustering analysis
#' results <- estimateCluster(
#'   CONNECTORData = my_data,
#'   G = 2:5,
#'   p = c("measure1" = 4, "measure2" = 3),
#'   h = 2,
#'   runs = 50,
#'   cores = 4
#' )
#'
#' # Quick exploration with fewer runs
#' quick_results <- estimateCluster(
#'   CONNECTORData = my_data,
#'   G = 2:4,
#'   p = 3,
#'   h = 2,
#'   runs = 10,
#'   cores = 2
#' )
#' }
#'
#' @author Cordero Francesca, Pernice Simone, Sirovich Roberta
#'
#' @seealso
#' \code{\link{ConnectorData}} for creating input data objects,
#' \code{\link{estimatepDimension}} for selecting spline dimensions,
#' \code{\link{selectCluster}} for choosing optimal solutions,
#' \code{\link{validateCluster}} for validating results
#'
#' @import RColorBrewer statmod parallel splines RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr summarise ungroup pull
#' @export
#'
setGeneric("estimateCluster", function(CONNECTORData,
                                       G,
                                       p,
                                       h = NULL,
                                       runs = 50,
                                       seed = 2404,
                                       cores = 1,
                                       PercPCA = .85,
                                       MinErrFreq = 0,
                                       pert = 0.01) {
  standardGeneric("estimateCluster")
})

setMethod("estimateCluster", signature("CONNECTORData"), function(CONNECTORData,
                                                                  G,
                                                                  p,
                                                                  h = NULL,
                                                                  runs = 50,
                                                                  seed = 2404,
                                                                  cores = 1,
                                                                  PercPCA = .85,
                                                                  MinErrFreq = 0,
                                                                  pert = 0.01) {
  start <- Sys.time()
  p <- process_p(p, CONNECTORData)
  # CONNECTORData deve essere il dataset
  CData <- CONNECTORData@curves
  KmData <- presetKmeans(CData, q = p)

  i <- rep(G, each = runs)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  ALL.runs <- lapply(i, function(i) {
    tryCatch(
      {
        justKmeans(CLUSTData = KmData, K = i)
      },
      error = function(e) {
        err <- paste("ERROR in justKmeans :", conditionMessage(e), "\n")
        err.list <- list(Error = err)
        # print(err)
        return(err.list)
      }
    )
  })

  ALL.runs_grouped <- split(ALL.runs, i)

  groupsFrequency <- lapply(ALL.runs_grouped, function(group) {
    return(kmeansGroup(group))
  })

  all_combinations <- list()
  for (k in as.numeric(names(groupsFrequency))) {
    n_patterns <- length(groupsFrequency[[as.character(k)]]$patterns)
    for (i in 1:n_patterns) {
      all_combinations[[length(all_combinations) + 1]] <- list(
        K = k,
        class = groupsFrequency[[as.character(k)]]$patterns[[i]],
        freq = groupsFrequency[[as.character(k)]]$counts[i]
      )
    }
  }
  if (cores == 1) {
    results <- lapply(all_combinations, function(combo) {
      # browser()
      if (is.null(h)) {
        h <- min(combo$K - 1, p)
      }
      h.found <- F
      tentative <- 1
      while (!h.found) {
        result <- tryCatch(
          {
            intfclust(
              q = p,
              h = h,
              K = combo$K,
              class = combo$class,
              CLUSTData = KmData,
              pert1 = 0.00001
              # freq = combo$freq
            )
          },
          error = function(e) {
            err <- paste("ERROR in intfclust:", conditionMessage(e), "\n")
            return(list(Error = err))
          }
        )

        if (!is.null(result$Error)) {
          h <- h - 1
          tentative <- tentative + 1
        } else {
          fcm.prediction <- fclust.curvepred(
            data = result,
            q = p,
            KData = KmData
          )
          cluster <- result$pred$class.pred
          if (length(unique(cluster)) != combo$K) {
            result <- list(
              Error = paste0(
                "ERROR in prediction: number of clusters obtained is different from ",
                combo$K
              )
            )
          }

          if (is.null(result$Error)) {
            h.found <- T
            h.out <- h
          } else {
            h <- h - 1
            tentative <- tentative + 1
          }
        }

        if (h == 0) {
          h.found <- T
          h.out <- 1
        }

        if (h.found == T && is.null(result$Error)) {
          TTandfDBandSil <- TTandfDBandSilfunction(
            result = result,
            KData = KmData,
            curvepred = fcm.prediction,
            G = combo$K
          )
          result$pred$subjID <- unique(CData$subjID)
        } else if (h.found == T && !is.null(result$Error)) {
          TTandfDBandSil <- list(Error = "Could not compute quality metrics due to previous errors.")
        }
      }
      return(list(
        TTandfDBandSil = TTandfDBandSil,
        CfitandParameters = result,
        h = h.out,
        freq = combo$freq
      ))
    })
  } else {
    type <- if (exists("mcfork", mode = "function")) {
      "FORK"
    } else {
      "PSOCK"
    }
    cl <- makeCluster(cores, type = type)
    clusterSetRNGStream(cl, seed)
    clusterCall(cl, function() {
      library(statmod)
      library(dplyr)
      library(splines)
    })
    clusterExport(
      cl,
      list(
        "intfclust",
        "p",
        "h",
        "KmData",
        "CData",
        # TODO rimuovere se non serve corrispondenza cluster subj
        "groupsFrequency",
        "fclustMstep",
        "fclustEstep",
        "fclustconst",
        "fclust_pred",
        "omp_set_num_threads",
        "fclust.curvepred",
        "nummax",
        "TTandfDBandSilfunction",
        "DistAllSubjCurve2mu",
        "Distmu2mu",
        "DistAllSubjCurves2Curves.sapl"
      ),
      envir = environment()
    )

    results <- parLapply(cl, all_combinations, function(combo) {
      # results <- lapply( all_combinations, function(combo) {
      omp_set_num_threads(1)

      if (is.null(h)) {
        h <- min(combo$K - 1, p)
      }
      h.found <- F
      tentative <- 1
      while (!h.found) {
        result <- tryCatch(
          {
            intfclust(
              q = p,
              h = h,
              K = combo$K,
              class = combo$class,
              CLUSTData = KmData,
              pert1 = 0.00001
              # freq = combo$freq
            )
          },
          error = function(e) {
            err <- paste("ERROR in intfclust:", conditionMessage(e), "\n")
            return(list(Error = err))
          }
        )

        if (!is.null(result$Error)) {
          h <- h - 1
          tentative <- tentative + 1
        } else {
          fcm.prediction <- fclust.curvepred(
            data = result,
            q = p,
            KData = KmData
          )
          cluster <- result$pred$class.pred
          if (length(unique(cluster)) != combo$K) {
            result <- list(
              Error = paste0(
                "ERROR in prediction: number of clusters obtained is different from ",
                combo$K
              )
            )
          }

          if (is.null(result$Error)) {
            h.found <- T
            h.out <- h
          } else {
            h <- h - 1
            tentative <- tentative + 1
          }
        }

        if (h == 0) {
          h.found <- T
          h.out <- 1
        }

        if (h.found == T && is.null(result$Error)) {
          TTandfDBandSil <- TTandfDBandSilfunction(
            result = result,
            KData = KmData,
            curvepred = fcm.prediction,
            G = combo$K
          )
          result$pred$subjID <- unique(CData$subjID)
        } else if (h.found == T && !is.null(result$Error)) {
          TTandfDBandSil <- list(Error = "Could not compute quality metrics due to previous errors.")
        }
      }
      return(list(
        TTandfDBandSil = TTandfDBandSil,
        CfitandParameters = result,
        h = h.out,
        freq = combo$freq
      ))
    })

    stopCluster(cl)
  }

  # Check possible errors in results
  error_indices <- sapply(results, function(res) {
    is.list(res) && "Error" %in% names(res$TTandfDBandSil)
  })

  if (any(error_indices)) {
    warning("Some clustering runs encountered errors. Check the 'Error' messages in the results for details.")
  }

  KmData$annotations <- CONNECTORData@annotations

  output <- list(
    Clusterings = results,
    KData = KmData,
    Seed = seed
  )

  # output$KData = KmData
  # output$plot<- IndexPlotExtrapolation(output)
  time_diff <- Sys.time() - start

  # Estrai il valore numerico e l'unità
  time_value <- round(as.numeric(time_diff), 2)
  time_unit <- attr(time_diff, "units")
  print(paste("Total time:", time_value, time_unit))
  return(output)
})


setGeneric("TTandfDBandSilfunction", function(result, KData, curvepred, G) {
  standardGeneric("TTandfDBandSilfunction")
})

setMethod("TTandfDBandSilfunction", signature(), function(result, KData, curvepred, G) {
  allsubjdist2mu <- DistAllSubjCurve2mu(result, KData, curvepred)
  # sono ordinate per jamesID
  TT <- sum(allsubjdist2mu)
  G <- ncol(result$cfit$vars$piigivej)

  essek <- sapply(1:G, function(g) {
    sqrt(sum(allsubjdist2mu[result$pred$class.pred == g]^2) / sum(result$pred$class.pred == g))
  })

  errek <- sapply(1:G, function(g, result, KData, curvepred) {
    essekprimo <- essek[-g]
    emmekkprimo <- numeric(G - 1)
    indice <- 1
    for (kprimo in (1:G)[-g]) {
      emmekkprimo[indice] <- Distmu2mu(result, KData, curvepred, g, kprimo)
      indice <- indice + 1
    }
    return(max((essekprimo + essek[g]) / emmekkprimo))
  }, result = result, KData = KData, curvepred = curvepred)
  fDB <- sum(errek) / G

  all_distances <- DistAllSubjCurves2Curves.sapl(KData, curvepred)
  cluster_assignments <- result$pred$class.pred

  silCoeff <- do.call(
    rbind,
    lapply(1:max(KData$CData$jamesID), function(jID) {
      current_cluster <- cluster_assignments[jID]
      in_cluster_indices <- which(cluster_assignments == current_cluster)
      out_cluster_indices <- cluster_assignments[cluster_assignments != current_cluster]

      denomin <- table(cluster_assignments)[current_cluster] - 1
      denomin <- ifelse(denomin == 0, 1, denomin)
      # since it is tringular I have to sum both the row and column
      ai <- sum(all_distances[jID, in_cluster_indices] + all_distances[in_cluster_indices, jID]) * 1 / (denomin)

      bi_all <- sapply(unique(out_cluster_indices), function(cl) {
        cluster_indices <- which(cluster_assignments == cl)
        denomin <- table(cluster_assignments)[cl]
        sum(all_distances[jID, cluster_indices] + all_distances[cluster_indices, jID]) * 1 / denomin
      })

      bi <- min(bi_all)

      if (table(cluster_assignments)[current_cluster] > 1) {
        si <- (bi - ai) / max(ai, bi)
      } else {
        si <- 0
      }

      return(data.frame(jamesID = jID, ai = ai, bi = bi, si = si, cluster = current_cluster))
    })
  )
  silhouette_scores <- silCoeff %>%
    summarise(Smean = mean(si)) %>%
    ungroup() %>%
    pull(Smean)


  return(data.frame(fDB = fDB, TT = TT, G = G, Sil = silhouette_scores))
})

setGeneric("DistAllSubjCurve2mu", function(result, KData, curvepred) {
  standardGeneric("DistAllSubjCurve2mu")
})

setMethod("DistAllSubjCurve2mu", signature(), function(result, KData, curvepred) {
  q <- sapply(1:length(KData$FullS), function(x) ncol(KData$FullS[[x]]))
  M <- length(KData$TimeGrids)
  N <- max(KData$CData$jamesID)
  cluster_assignments <- result$pred$class.pred

  a <- min(KData$CData$time)
  b <- max(KData$CData$time)

  # Pre-compute Gauss-Legendre quadrature once
  ptgauss <- gauss.quad(10)
  xk <- (b + a) / 2 + (b - a) / 2 * ptgauss$nodes
  w <- (b - a) / 2 * ptgauss$weights

  # Accumulate per-subject squared distances across measures
  dist2 <- numeric(N)

  for (m in 1:M) {
    x_obs <- KData$TimeGrids[[m]]

    # ns(x, df=q, intercept=TRUE) spans the constant function, so adding a
    # column of 1s (formula intercept) would be redundant and rank-deficient.
    # Using ns_obs directly gives the full-rank q x q system with the same
    # column space as lm(y ~ ns(x, df=q, intercept=TRUE)).
    ns_obs <- ns(x_obs, df = q[m], intercept = TRUE) # [n_grid x q]
    kts <- attr(ns_obs, "knots")
    bkts <- attr(ns_obs, "Boundary.knots")
    ns_xk <- ns(xk,
      knots = kts, Boundary.knots = bkts,
      intercept = TRUE
    ) # [10 x q]

    # Pre-compute projection matrix once; apply to all subjects + means together
    P_m <- ns_xk %*% solve(crossprod(ns_obs), t(ns_obs)) # [10 x n_grid]

    Y_subj <- t(curvepred[[m]]$gpred) # [n_grid x N]
    Y_mean <- curvepred[[m]]$meancurves # [n_grid x K]
    G_subj <- P_m %*% Y_subj # [10 x N]
    G_mean <- P_m %*% Y_mean # [10 x K]

    for (i in 1:N) {
      diff <- G_subj[, i] - G_mean[, cluster_assignments[i]]
      dist2[i] <- dist2[i] + sum(w * diff^2)
    }
  }

  return(sqrt(dist2))
})


setGeneric("Distmu2mu", function(result, KData, curvepred, k, h) {
  standardGeneric("Distmu2mu")
})

setMethod("Distmu2mu", signature(), function(result, KData, curvepred, k, h) {
  q <- sapply(1:length(KData$FullS), function(x) ncol(KData$FullS[[x]]))
  M <- length(KData$TimeGrids)

  a <- min(KData$CData$time)
  b <- max(KData$CData$time)

  # Pre-compute Gauss-Legendre quadrature once
  ptgauss <- gauss.quad(10)
  xk <- (b + a) / 2 + (b - a) / 2 * ptgauss$nodes
  w <- (b - a) / 2 * ptgauss$weights

  dist2 <- 0

  for (m in 1:M) {
    x_obs <- KData$TimeGrids[[m]]

    ns_obs <- ns(x_obs, df = q[m], intercept = TRUE)
    kts <- attr(ns_obs, "knots")
    bkts <- attr(ns_obs, "Boundary.knots")
    ns_xk <- ns(xk, knots = kts, Boundary.knots = bkts, intercept = TRUE)

    P_m <- ns_xk %*% solve(crossprod(ns_obs), t(ns_obs)) # [10 x n_grid]

    mk_xk <- P_m %*% curvepred[[m]]$meancurves[, k]
    mh_xk <- P_m %*% curvepred[[m]]$meancurves[, h]

    diff <- mk_xk - mh_xk
    dist2 <- dist2 + sum(w * diff^2)
  }

  return(sqrt(dist2))
})

setGeneric("DistAllSubjCurves2Curves.sapl", function(KData, curvepred) {
  standardGeneric("DistAllSubjCurves2Curves.sapl")
})

setMethod("DistAllSubjCurves2Curves.sapl", signature(), function(KData, curvepred) {
  q <- sapply(1:length(KData$FullS), function(x) ncol(KData$FullS[[x]]))
  M <- length(KData$TimeGrids)
  n <- max(KData$CData$jamesID)

  a <- min(KData$CData$time)
  b <- max(KData$CData$time)

  # Pre-compute Gauss-Legendre quadrature ONCE
  ptgauss <- gauss.quad(10)
  xk <- (b + a) / 2 + (b - a) / 2 * ptgauss$nodes
  w <- (b - a) / 2 * ptgauss$weights

  # Accumulate the weighted cross-product matrix D across measures:
  #   D[i,j] = sum_m  sum_k  w[k] * G_m[k,i] * G_m[k,j]
  # Pairwise squared distances then follow from:
  #   d(i,j)^2 = D[i,i] + D[j,j] - 2*D[i,j]
  # No pair-loop needed — this reduces to two matrix operations per measure.
  D_total <- matrix(0, n, n)

  for (m in 1:M) {
    x_obs <- KData$TimeGrids[[m]]

    # ns(x, df=q, intercept=TRUE) spans the constant: no extra column of 1s needed.
    # The resulting q-column matrix is full rank and spans the same space as
    # cbind(1, ns(..., intercept=TRUE)), which lm() internally reduces to.
    ns_obs <- ns(x_obs, df = q[m], intercept = TRUE) # [n_grid x q]
    kts <- attr(ns_obs, "knots")
    bkts <- attr(ns_obs, "Boundary.knots")
    ns_xk <- ns(xk,
      knots = kts, Boundary.knots = bkts,
      intercept = TRUE
    ) # [10 x q]

    # Projection matrix computed once per measure [10 x n_grid]
    P_m <- ns_xk %*% solve(crossprod(ns_obs), t(ns_obs))

    # Project all N curves at once: [10 x N]
    G_m <- P_m %*% t(curvepred[[m]]$gpred)

    D_total <- D_total + crossprod(G_m, w * G_m)
  }

  # Build pairwise distance matrix without a pair-loop
  d_diag <- diag(D_total)
  dist_sq <- outer(d_diag, d_diag, FUN = "+") - 2 * D_total
  dist_sq[dist_sq < 0] <- 0 # numerical safety

  # Return upper-triangular to preserve the original interface
  # (callers sum dist[i,j] + dist[j,i] to get the distance for each pair)
  dist_matrix <- matrix(0, n, n)
  dist_matrix[upper.tri(dist_matrix)] <- sqrt(dist_sq)[upper.tri(dist_sq)]

  return(dist_matrix)
})


setGeneric("process_p", function(p, CONNECTORData) {
  standardGeneric("process_p")
})

setMethod("process_p", signature(), function(p, CONNECTORData) {
  if (!is.null(names(p))) {
    valid_names <- names(CONNECTORData@TimeGrids)
    if (!all(names(p) %in% valid_names)) {
      stop(
        "Some of the names provided in 'p' are invalid. Allowed names are: ",
        paste(valid_names, collapse = ", ")
      )
    }
    # Ordina in ordine alfabetico e rimuove i nomi
    p <- p[order(names(p))]
    names(p) <- NULL
  }
  return(p)
})
