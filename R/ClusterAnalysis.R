#' Cluster Analysis
#'
#'@description
#'
#'  Fits and clusters the data with respect to the Functional Clustering Model [Sugar and James]. Multiple runs of the algorithm are necessary since the algorithm is stochastic As explained in [Sugar and James], to have a simple low-dimensional representation of the individual curves and to reduce the number of parameters to be estimated, h value must be equals or lower than \eqn{min(p,G-1)}.
#'
#' @param data CONNECTORList. (see \code{\link{ConnectorData}} or \code{\link{DataTruncation}})
#' @param G The vector/number of possible clusters.
#' @param p The dimension of the natural cubic spline basis. (see \code{\link{BasisDimension.Choice}})
#' @param runs Number of runs.
#' @param seed Seed for the kmeans function.
#' @param save If TRUE then the growth curves plot truncated at the "TruncTime" is saved into a pdf file.
#' @param path The folder path where the plot(s) will be saved. If it is missing, the plot is saved in the current working  directory.
#' @param cores Number of cores to parallelize computations.
#' @param PercPCA=.85
#' @param pert ....
#'
#' @author Cordero Francesca, Pernice Simone, Sirovich Roberta
#'
#' @return StabilityAnalysis returns a list of (i) lists, called ConsensusInfo, reporting for each G and h: the Consensus Matrix, either as a NxN matrix, where N is the number of samples, or plot, and the most probable clustering obtained from running several times the method; (ii) the box plots showing both the Elbow plot considering the total tightness and the box plots of the fDB indexes for each G; and finally, (iii) the seed. See \code{\link{IndexesPlot.Extrapolation}} and \code{\link{MostProbableClustering.Extrapolation}}.
#'
#' @details
#'  Connector provides two different plots to properly guide the choice of the number of clusters:
#'  \itemize{
#'  \item{Elbow Plot:}{ a plot in which the total tightness against the number of clusters is plotted. A proper number of clusters can be inferred as large enough to let the total tightness drop down to relatively little values but as the smallest over which the total tightness does not decrease substantially (we look for the location of an "elbow" in the plot). }
#'  \item{Box Plot:}{ a plot in which for each number of clusters , G, the functional Davies-Buldin (fDB), the cluster separation measure index, is plotted as a boxplot. A proper number of clusters can be associated to the minimum fDB value. }
#'  }
#'
#'    The proximities measures choosen is defined as follow
#'  \deqn{D_q(f,g) = \sqrt( \integral | f^{(q)}(s)-g^{(q)}(s) |^2 ds ), d=0,1,2}
#' where f and g are two curves and f^{(q)} and g^{(q)} are their q-th derivatives. Note that for q=0, the equation becomes the distance induced by the classical L^2-norm.
#' Hence, we can define the following indexes for obtaining a cluster separation measure:
#' \itemize{
#' \item{T:}{ the total tightness representing the dispersion measure given by
#' \deqn{ T = \sum_{k=1}^G \sum_{i=1}^n D_0(\hat{g}_i, \bar{g}^k)}; }
#'  \item{S_k:}{
#'  \deqn{ S_k = \sqrt{\frac{1}{G_k} \sum_{i=1}^{G_k} D_q^2(\hat{g}_i, \bar{g}^k);} }
#'  with G_k the number of curves in the k-th cluster;
#'  }
#'  \item{M_{hk}:}{ the distance between centroids (mean-curves) of h-th and k-th cluster
#'  \deqn{M_{hk} =  D_q(\bar{g}^h, \bar{g}^k);}
#'  }
#'  \item{R_{hk}:}{  a measure of how good the clustering is,
#'  \deqn{R_{hk} =   \frac{S_h + S_k}{M_{hk}};}
#'  }
#'  \item{fDB_q:}{ functional Davies-Bouldin index, the cluster separation measure
#'  \deqn{fDB_q = \frac{1}{G} \sum_{k=1}^G \max_{h \neq k} {  \frac{S_h + S_k}{M_{hk}} } }
#'  }
#' }
#'
#'
#' @seealso MostProbableClustering.Extrapolation, BoxPlot.Extrapolation, ConsMatrix.Extrapolation.
#'
#' @import RColorBrewer statmod parallel Matrix splines RhpcBLASctl dplyr
#' @export
#'
setGeneric("ClusterAnalysis", function(CONNECTORData,
                                       G,
                                       p,
                                       h = NULL,
                                       runs = 50,
                                       seed = 2404,
                                       cores = 1,
                                       PercPCA = .85,
                                       MinErrFreq = 0,
                                       pert = 0.01)
  standardGeneric("ClusterAnalysis"))
#' @rdname ClusterAnalysis
#' @export
setMethod("ClusterAnalysis", signature ("CONNECTORData"), function(CONNECTORData,
                                                                   G,
                                                                   p,
                                                                   h = NULL,
                                                                   runs = 50,
                                                                   seed = 2404,
                                                                   cores = 1,
                                                                   PercPCA = .85,
                                                                   MinErrFreq = 0,
                                                                   pert = 0.01)
{
  start <- Sys.time()
  p<-process_p(p, CONNECTORData)
  #CONNECTORData deve essere il dataset
  CData <- CONNECTORData@curves
  KmData <- presetKmeans(CData, q = p)
  
  i <- rep(G, each = runs)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  ALL.runs <- lapply(i, function(i) {
    tryCatch({
      justKmeans(CLUSTData = KmData, K = i)
    }, error = function(e) {
      err <- paste("ERROR in justKmeans :", conditionMessage(e), "\n")
      err.list <- list(Error = err)
      #print(err)
      return(err.list)
    })
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
      #browser()
      if (is.null(h)) {
        h = min(combo$K - 1, p) 
      }
      h.found = F
      tentative = 1
      while (!h.found) {
        result <- tryCatch({
          intfclust(
            q = p,
            h = h,
            K = combo$K,
            class = combo$class,
            CLUSTData = KmData,
            pert1 = 0.00001
            #freq = combo$freq
          )
        }, error = function(e) {
          err <- paste("ERROR in intfclust:", conditionMessage(e), "\n")
          return(list(Error = err))
        })
        fcm.prediction <- fclust.curvepred(data = result,
                                           q = p,
                                           KData = KmData)
        if (!is.null(result$Error)) {
          h = h - 1
          tentative = tentative + 1
        }
        else{
          cluster <- result$pred$class.pred
          if (length(unique(cluster)) != combo$K) {
            results <- list(
              Error = paste0(
                "ERROR in prediction: number of clusters obtained is different from ",
                combo$K
              )
            )
            
          }
          
          if (is.null(result$Error)) {
            h.found = T
            h.out = h
            
          }
          else{
            h = h - 1
            tentative = tentative + 1
          }
        }
        if (h == 0) {
          h.found = T
          h.out = 1
        }
        if (h.found == T) {
          TTandfDBandSil <- TTandfDBandSilfunction(
            result = result,
            KData = KmData,
            curvepred = fcm.prediction,
            G = combo$K
          )
          result$pred$subjID <- unique(CData$subjID)
        }
      }
      return(list(
        TTandfDBandSil = TTandfDBandSil,
        CfitandParameters = result,
        h = h.out,
        freq = combo$freq
      ))
    })
    
  }
  else{
    type <- if (exists("mcfork", mode = "function"))
      "FORK"
    else
      "PSOCK"
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
        #TODO rimuovere se non serve corrispondenza cluster subj
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
      #results <- lapply( all_combinations, function(combo) {
      omp_set_num_threads(1)
      
     
      
      if (is.null(h)) {
        h = min(combo$K - 1, p) 
      }
      h.found = F
      tentative = 1
      while (!h.found) {
        result <- tryCatch({
          intfclust(
            q = p,
            h = h,
            K = combo$K,
            class = combo$class,
            CLUSTData = KmData,
            pert1 = 0.00001
            #freq = combo$freq
          )
        }, error = function(e) {
          err <- paste("ERROR in intfclust:", conditionMessage(e), "\n")
          return(list(Error = err))
        })
        fcm.prediction <- fclust.curvepred(data = result,
                                           q = p,
                                           KData = KmData)
        if (!is.null(result$Error)) {
          h = h - 1
          tentative = tentative + 1
        }
        else{
          cluster <- result$pred$class.pred
          if (length(unique(cluster)) != combo$K) {
            results <- list(
              Error = paste0(
                "ERROR in prediction: number of clusters obtained is different from ",
                combo$K
              )
            )
            
          }
          
          if (is.null(result$Error)) {
            h.found = T
            h.out = h
            
          }
          else{
            h = h - 1
            tentative = tentative + 1
          }
        }
        if (h == 0) {
          h.found = T
          h.out = 1
        }
        if (h.found == T) {
          TTandfDBandSil <- TTandfDBandSilfunction(
            result = result,
            KData = KmData,
            curvepred = fcm.prediction,
            G = combo$K
          )
          result$pred$subjID <- unique(CData$subjID)
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
  results$KData = KmData
  results$plot<-
    IndexPlotExtrapolation(results)
  time_diff <- Sys.time() - start
  
  # Estrai il valore numerico e l'unità
  time_value <- round(as.numeric(time_diff), 2)
  time_unit <- attr(time_diff, "units")
  print(paste("Total time:", time_value, time_unit))
  return(results)
})




setGeneric("TTandfDBandSilfunction", function(result, KData, curvepred, G)
  standardGeneric("TTandfDBandSilfunction"))

setMethod("TTandfDBandSilfunction", signature(), function(result, KData, curvepred, G) {
  allsubjdist2mu <- DistAllSubjCurve2mu(result, KData, curvepred)
  # sono ordinate per jamesID
  TT <- sum(allsubjdist2mu)
  G <- ncol(result$cfit$vars$piigivej)
  
  essek <- sapply(1:G, function(g) {
    sqrt(sum(allsubjdist2mu[result$pred$class.pred == g] ^ 2) / sum(result$pred$class.pred == g))
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
  #sil
  
  ##HO RIMOSSO RESULT DALLA CHIAMATA SUCCESSIVA, NON SERVE?
  
  
  ##
  all_distances = DistAllSubjCurves2Curves.sapl(KData, curvepred)
  cluster_assignments <- result$pred$class.pred
  
  silCoeff = do.call(rbind, 
                     lapply(1:max(KData$CData$jamesID),function(jID){
                       current_cluster = cluster_assignments[jID]
                       in_cluster_indices <- which(cluster_assignments == current_cluster)
                       out_cluster_indices <- cluster_assignments[cluster_assignments != current_cluster]
                       
                       denomin = table(cluster_assignments)[current_cluster]-1
                       denomin = ifelse(denomin == 0, 1, denomin)
                       # since it is tringular I have to sum both the row and column
                       ai = sum(all_distances[jID, in_cluster_indices] +  all_distances[ in_cluster_indices, jID])* 1/(denomin)
                       
                       bi_all = sapply(unique(out_cluster_indices), function(cl){
                         cluster_indices <- which(cluster_assignments == cl)
                         denomin = table(cluster_assignments)[cl]
                         sum(all_distances[jID, cluster_indices] +  all_distances[ cluster_indices, jID])*1/denomin
                       })
                       
                       bi = min(bi_all)
                       
                       if(table(cluster_assignments)[current_cluster] > 1)
                         si = (bi - ai) / max(ai, bi) 
                       else 
                         si = 0 
                       
                       return(data.frame(jamesID = jID, ai = ai, bi = bi, si = si, cluster = current_cluster))    
                     })
  )
  silhouette_scores <-  silCoeff %>% summarise(Smean = mean(si)) %>% ungroup() %>% pull(Smean)
  
  
  return(data.frame(fDB = fDB, TT = TT, G = G, Sil= silhouette_scores))
})

setGeneric("DistAllSubjCurve2mu", function(result, KData, curvepred)
  standardGeneric("DistAllSubjCurve2mu"))

setMethod("DistAllSubjCurve2mu", signature(), function(result, KData, curvepred) {
  q <- sapply(1:length(KData$FullS), function(x)
    ncol(KData$FullS[[x]]))
  # curvepred <- fclust.curvepred(
  #       data=result,
  #       KData= KmData,
  #       q = q
  #     )
  
  DistSingleSubjCurve2mu <- function(jamesID) {
    # numero delle misure
    M <- length(KData$TimeGrids)
    # bisogna settare gli estremi dell'intervallo di integrazione per ora lo teniamo
    # completo dobbiamo discuterne
    a <- min(KData$CData$time)
    b <- max(KData$CData$time)
    
    library(statmod)
    ptgauss <- gauss.quad(10)
    xk <- (b + a) / 2 + (b - a) / 2 * ptgauss$nodes
    
    # ora bisogna calcolare la effe da integrare che è la somma sulle misure del
    # valore predetto della curva - media del cluster al quale appartiene quadrato
    # approssimo con le spline la previsione di curva e media. La effe va calcolata su xk
    
    # effe_j calcola per ciascuna misura per il subj == jamesID, su tutti i pti di gauss
    # (hatg - meang)^2
    
    effe_j <- function(measure) {
      y <- curvepred[[measure]]$gpred[jamesID, ]
      x <- KData$TimeGrids[[measure]]
      fit.temp <- lm(y ~ ns(x, df = q[measure], intercept = TRUE))
      ypred <- unname(predict(fit.temp, newdata = data.frame(x = xk)))
      m <- curvepred[[measure]]$meancurves[, result$pred$class.pred[jamesID]]
      fit.temp <- lm(m ~ ns(x, df = q[measure], intercept = TRUE))
      mpred <- unname(predict(fit.temp, newdata = data.frame(x = xk)))
      return((ypred - mpred) ^ 2)
    }
    
    effe <- rowSums(sapply(1:M, effe_j))
    
    barint <- (b - a) / 2 * sum(ptgauss$weights * effe)
    return(sqrt(barint))
  }
  
  alldist <- sapply(1:max(KData$CData$jamesID), DistSingleSubjCurve2mu)
  # escono ordinate per jamesID
  return(alldist)
})


setGeneric("Distmu2mu", function(result, KData, curvepred, k, h)
  standardGeneric("Distmu2mu"))

setMethod("Distmu2mu", signature(), function(result, KData, curvepred, k, h) {
  q <- sapply(1:length(KData$FullS), function(x)
    ncol(KData$FullS[[x]]))
  # curvepred <- fclust.curvepred(
  #   outfclust$cfit,
  #   data = outfclust$working_data,
  #   FullS = outfclust$FullS,
  #   q = q)
  # quindi h e k sono le colonne di meancurves che vado a confrontare
  
  # numero delle misure
  M <- length(KData$TimeGrids)
  # bisogna settare gli estremi dell'intervallo di integrazione per ora lo teniamo
  # completo dobbiamo discuterne
  a <- min(KData$CData$time)
  b <- max(KData$CData$time)
  
  library(statmod)
  ptgauss <- gauss.quad(10)
  xk <- (b + a) / 2 + (b - a) / 2 * ptgauss$nodes
  
  # ora bisogna calcolare la effe da integrare che è la somma sulle misure del
  # valore della media_h-media_k quadrato
  # approssimo con le spline le medie. La effe va calcolata su xk
  
  # effe_j calcola per ciascuna misura per il subj == jamesID, su tutti i pti di gauss
  # (meang_h - meang_k)^2
  
  effe_j <- function(measure) {
    mh <- curvepred[[measure]]$meancurves[, h]
    x <- KData$TimeGrids[[measure]]
    fit.temp <- lm(mh ~ ns(x, df = q[measure], intercept = TRUE))
    mhpred <- unname(predict(fit.temp, newdata = data.frame(x = xk)))
    mk <- curvepred[[measure]]$meancurves[, k]
    fit.temp <- lm(mk ~ ns(x, df = q[measure], intercept = TRUE))
    mkpred <- unname(predict(fit.temp, newdata = data.frame(x = xk)))
    return((mhpred - mkpred) ^ 2)
  }
  
  effe <- rowSums(sapply(1:M, effe_j))
  
  barint <- (b - a) / 2 * sum(ptgauss$weights * effe)
  return(sqrt(barint))
  
})

setGeneric("DistAllSubjCurves2Curves.sapl", function(KData, curvepred)
  standardGeneric("DistAllSubjCurves2Curves.sapl"))

setMethod("DistAllSubjCurves2Curves.sapl", signature(), function(KData, curvepred) {

  
  q <- sapply(1:length(KData$FullS), function(x)
    ncol(KData$FullS[[x]]))
  
  # Function to calculate the distance between two curves
  DistBetweenCurves <- function(jamesID1, jamesID2) {
    M <- length(KData$TimeGrids)
    a <- min(KData$CData$time)
    b <- max(KData$CData$time)
    
    ptgauss <- gauss.quad(10)
    xk <- (b + a) / 2 + (b - a) / 2 * ptgauss$nodes
    
    effe <- rowSums(sapply(1:M, function(measure) {
      x <- KData$TimeGrids[[measure]]
      y1 <- curvepred[[measure]]$gpred[jamesID1,]
      y2 <- curvepred[[measure]]$gpred[jamesID2,]
      fit.temp1 <- lm(y1 ~ ns(x, df = q[measure], intercept = TRUE))
      fit.temp2 <- lm(y2 ~ ns(x, df = q[measure], intercept = TRUE))
      ypred1 <- unname(predict(fit.temp1, newdata = data.frame(x = xk)))
      ypred2 <- unname(predict(fit.temp2, newdata = data.frame(x = xk)))
      (ypred1 - ypred2)^2
    }))
    
    barint <- (b - a) / 2 * sum(ptgauss$weights * effe)
    return(sqrt(barint))
  }
  
  # Number of curves
  n <- max(KData$CData$jamesID)
  
  # Generate all pairs of indices for the upper triangular part
  index_pairs <- t(combn(1:n, 2)) # Generate combinations of pairs (i, j) where i < j
  sorted_pairs <- index_pairs[order(index_pairs[, 2], index_pairs[, 1]), ]
  
  # Compute distances for all pairs using apply
  dist_values <- apply(sorted_pairs, 1, function(pair) {
    DistBetweenCurves(pair[1], pair[2])
  })
  
  # Create the triangular distance matrix
  dist_matrix <- matrix(0, n, n)
  dist_matrix[upper.tri(dist_matrix)] <- dist_values
  
  return(dist_matrix)
})


setGeneric("process_p", function(p, CONNECTORData)
  standardGeneric("process_p"))

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