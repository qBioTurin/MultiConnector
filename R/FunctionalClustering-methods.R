#' Prepare longitudinal data for functional clustering
#'
#' @param CData Long-format longitudinal data from a `CONNECTORData` object.
#' @param natural Logical; use natural cubic splines when `TRUE` and B-splines
#'   otherwise.
#' @param q Named integer vector of spline-basis dimensions by measure.
#' @param pert Positive ridge perturbation for the initial coefficient fits.
#'
#' @return A list containing ordered data, time grids, initial spline
#'   coefficients, subject count, design matrix, and full spline bases.
#'
#' @keywords internal
#' @noRd
#' @seealso kmeans()
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


"presetKmeans" <- function(CData,
                           natural = TRUE,
                           q,
                           pert = 0.01) {
  CData$jamesID <- as.integer(factor(CData$subjID, levels = unique(CData$subjID)))

  CData <- CData %>%
    arrange(jamesID, measureID, time)

  # Questa funzione serve ad inizializzare i parametri da usare
  # riceve i dati già ordinati per jamesID, measureID e time
  # TODO capire se serve la grid qua

  # N indica il numero di soggetti studiati
  N <- length(unique(CData$jamesID))
  # M è il vettore contente le misure, applicando l'unique
  M <- sort(as.character(unique(CData$measureID)))

  # Standardize each measure to unit variance for balanced clustering
  # This prevents measures with different scales from dominating the clustering
  # CData$value_original <- CData$value  # Store original values for later inverse transformation
  # scaling_params <- list()  # Store mean and sd for each measure
  # 
  # for (meas in M) {
  #   meas_mask <- CData$measureID == meas
  #   meas_mean <- mean(CData$value[meas_mask], na.rm = TRUE)
  #   meas_sd <- sd(CData$value[meas_mask], na.rm = TRUE)
  #   
  #     # # Only standardize if SD > 0
  #     # if (meas_sd > 1e-10) {
  #     #   CData$value[meas_mask] <- (CData$value[meas_mask] - meas_mean) / meas_sd
  #     #   scaling_params[[meas]] <- list(mean = meas_mean, sd = meas_sd)
  #     # } else {
  #     #   scaling_params[[meas]] <- list(mean = meas_mean, sd = 1)
  #     # }
  # }

  # Definiamo la griglia(faccio una lista i tempi per la j-esima misura)

  grid <- list()

  for (j in M ) {
    a <- sort(unique(CData$time[ CData$measureID == j ]))
    grid <- list.append(grid, a)
  }
  names(grid) <- M

  # Faccio una lista contente le FullS per ogni misura
  FullS <- list()
  # Metto la condizione if, per dare la possibilità di cambiare il tipo di base usata (ns o bs)
  if (natural == TRUE) {
    for (j in M ) {
      b <- cbind(1, ns(grid[[j]], df = q[j] - 1)) # unlist serve poiché se no non mi legge il vettore (ho controllato che mantenga l'ordine e non cambi gli elemnti)
      b <- svd(b)$u
      FullS <- list.append(FullS, b)
    }
  } else {
    for (j in M ) {
      b <- cbind(1, bs(grid[[j]], df = q[j] - 1))
      b <- svd(b)$u
      FullS <- list.append(FullS, b)
    }
  }
  names(FullS) <- M
  
  # Creo i timeindex, poiché nel dataset iniziale io ho solo i tempi ma non come essi siano posizionati nella griglia
  CData$timeindex <- NA

  for (i in 1:N) {
    t <- NULL
    for (j in M) {
      CData$timeindex[CData$measureID == j &
        CData$jamesID == i] <- match(
        CData$time[CData$measureID == j & CData$jamesID == i],
        grid[[j]]
      )
    }
  }

  # I timeindex così definiti sono un vettore, dove prima ci sono gli indici del primo soggetto, poi del secondo, ect...
  # Ora costruisco le S, matrici diagonali a blocchi, come definite in James and Sugar
  S <- NULL

  for (i in 1:N) {
    Si <- NULL
    for (j in M) {
      A <- FullS[[j]][CData$timeindex[(CData$jamesID == i &
        CData$measureID == j )], ]
      if (j == M[1] ) {
        Si <- A
      } else {
        Si <- bdiag(Si, A)
      } # Questo if è necessario poiché bdiag ha bisogno di una matrice iniziale su cui attaccarsi (ovvero quella della prima misura)
    }

    if (is.null(dim(Si))) {
      Si <- matrix(Si, nrow = 1)
    } else {
      Si <- as.matrix(Si)
    }

    S <- rbind(S, Si) # In questo modo attacco le matrice dei vari soggetti una sotto l'altra (tanto il numero di colonne è uguale per ogni soggetto, ovvero sum(q))
  }
  
  # Calcolo i coefficienti iniziali delle spline, usando il metodo dell'errore quadratico (penso qua si usi effetivamente la svd?)
  points <- matrix(0, N, sum(q))
  
  for (i in 1:N) {
    if (length(which(CData$jamesID == i)) == 1) {
      Si <- matrix(S[CData$jamesID == i, ], nrow = 1)
    } else {
      Si <- S[CData$jamesID == i, ]
    }
    yi <- CData$value[CData$jamesID == i]
    points[i, ] <- solve(crossprod(Si) + pert * diag(sum(q))) %*% t(Si) %*%
      yi
  }
  return(list(
    CData = CData,
    TimeGrids = grid,
    points = points,
    N = N,
    S = S,
    FullS = FullS
    # scaling_params = scaling_params  # Store scaling parameters for inverse transformation
  ))
}


#' Initialise clusters with k-means
#'
#' @param CLUSTData Preprocessed output from `presetKmeans()`.
#' @param K Positive integer number of clusters.
#'
#' @return Integer vector of initial cluster assignments.
#'
#' @keywords internal
#' @noRd
#' @seealso kmeans()
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


# Parte randomica dell'algoritmo, è necessario avere la parte di codice per K=1, poiché serve nello stimare gli iper-parametri
"justKmeans" <- function(CLUSTData, K) {
  if (K > 1) {
    class <- kmeans(CLUSTData$points, K, 10)$cluster
  } else {
    class <- rep(1, CLUSTData$N)
  }
  return(class)
}


#' Group equivalent cluster assignments
#'
#' @param vector_list List of integer cluster-assignment vectors.
#' @return A list with canonical assignment `patterns` and their `counts`.
#' @keywords internal
#' @noRd
"kmeansGroup" <- function(vector_list) {
  unic_vectors <- list()
  count <- numeric()

  for (v in vector_list) {
    found <- FALSE
    if (length(unic_vectors) == 0) {
      unic_vectors[[1]] <- v
      count[1] <- 1
      next
    }

    for (i in 1:length(unic_vectors)) {
      if (identical(as.numeric(factor(v, levels = unique(v))), as.numeric(factor(
        unic_vectors[[i]],
        levels = unique(unic_vectors[[i]])
      )))) {
        count[i] <- count[i] + 1
        found <- TRUE
        break
      }
    }

    if (!found) {
      unic_vectors[[length(unic_vectors) + 1]] <- v
      count[length(count) + 1] <- 1
    }
  }

  return(list(patterns = unic_vectors, counts = count))
}


#' Fit one functional clustering model
#'
#' @param q Named spline-basis dimensions by measure.
#' @param h Projection dimension.
#' @param pert Initial ridge perturbation retained for compatibility.
#' @param K Number of clusters.
#' @param class Initial subject-level cluster assignments.
#' @param CLUSTData Preprocessed output from `presetKmeans()`.
#' @param pert1 Ridge perturbation used in M-step linear solves.
#' @param tol Relative convergence tolerance for the measure-specific variances.
#' @param maxit Maximum number of EM iterations.
#' @param hard Logical; use hard rather than posterior-weighted assignments.
#' @param pert2 Ridge perturbation used when estimating projected scores.
#'
#' @return A list containing constrained fitted parameters (`cfit`) and
#'   subject-level predictions (`pred`).
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


"intfclust" <- function(q,
                        h,
                        pert = 0,
                        K,
                        class,
                        CLUSTData,
                        pert1 = 0.01,
                        tol = 0.001,
                        maxit = 20,
                        hard = FALSE,
                        pert2 = 0) {
  piigivej <- matrix(0, CLUSTData$N, K)
  piigivej[col(piigivej) == class] <- 1


  classmean <- matrix(0, K, sum(q))
  # Parte modificata da simone, così non si hanno problemi nel caso in cui in una classe ci sia un solo elemento
  for (l in 1:K)
  {
    if (sum(class == l) > 1) {
      classmean[l, ] <- apply(CLUSTData$points[class == l, ], 2, mean)
    } else {
      classmean[l, ] <- CLUSTData$points[class == l, ]
    }
  }

  # Calcolo i valori che mi servono
  lambda.zero <- apply(classmean, 2, mean)

  Lambda <- as.matrix(svd(scale(classmean, scale = F))$v[, 1:h])
  alpha <- scale(classmean, scale = F) %*% Lambda

  gamma <- t(t(CLUSTData$points) - lambda.zero - (Lambda %*% t(alpha[class, ])))
  gprod <- NULL
  for (i in 1:CLUSTData$N) {
    gprod <- cbind(gprod, (gamma[i, ]) %*% t(gamma[i, ]))
  }
  b <- sum(q)
  gamma <- array(gamma[, rep(1:sum(q), rep(K, sum(q)))], c(CLUSTData$N, K, sum(q)))
  gcov <- matrix(0, b, CLUSTData$N * b)

  # Definisco i dati in uscita

  # data1 = list(
  #   # S = CLUSTData$S,
  #   # FullS = CLUSTData$FullS,
  #   #starter = CData,
  parameters <- list(
    lambda.zero = lambda.zero,
    Lambda = Lambda,
    alpha = alpha
  )
  vars <- list(
    gamma = gamma,
    gprod = gprod,
    gcov = gcov,
    piigivej = piigivej
  )
  #   class=class,
  #   CLUSTData=CLUSTData
  # )


  S <- CLUSTData$S
  FullS <- CLUSTData$FullS
  sigma.old <- rep(0, length(CLUSTData$TimeGrids))
  sigma.new <- rep(1, length(CLUSTData$TimeGrids))
  ind <- 1

  # Iterazioni massime del ciclo EM
  # Main loop. Iterates between M and E steps and stops when
  # sigma  has converged.
  #browser()
  while ( sum( (sigma.new - sigma.old)^2 )/sum(sigma.new^2)  > tol &
    (ind <= maxit)) {
    parameters <- fclustMstepNEW(
      parameters = parameters,
      curve_ok = CLUSTData$CData,
      S = S,
      vars = vars,
      hard = hard,
      p_measures = q,
      p = 5,
      pert1 = pert1,
      tol = tol
    )

    vars <- fclustEstep(
      par = parameters,
      curve_ok = CLUSTData$CData,
      vars = vars,
      S,
      hard = hard
    )
    sigma.old <- sigma.new
    #sigma.new <- mean(parameters$sigma)  # Use mean of measure-specific sigmas for convergence check
    #sigma.new <- max(abs((parameters$sigma - vect.sigma.old) / vect.sigma.old))
    sigma.new <- parameters$sigma  # Store measure-specific sigmas for convergence check
    ind <- ind + 1
  }
  # starter <- initial$starter #Richiamo i valori inziali, poiché mi servono i timeindex
  ########### starter$S<-S

  cfit <- fclustconst(parameters, vars, S, measureID = CLUSTData$CData$measureID)
  pred <- fclust_pred(
    fit = cfit,
    data = CLUSTData,
    reweight = FALSE,
    pert2 = pert2
  )

  return(
    list(
      cfit = cfit,
      pred = pred
    )
  )
  # In uscita, ho le matrice S e FullS (la S è necessaria per tutto il resto del codice)
  # Parameters contiene i parametri inizializzati che effettivamente descrivono le curve
  # Vars
  # starter dà in uscita il dataset originale ma avendo aggiunto i timeindex, fondamentali per altre parti di codice
  # return(data1)
}


#' fclustEstep
#'
#' @description
#' Internal E-step for functional clustering.
#'
#' @param par Current model parameters.
#' @param curve_ok Ordered long-format observations.
#' @param vars Current latent-variable moments and posterior probabilities.
#' @param S Block design matrix.
#' @param hard Logical; replace posterior probabilities by hard assignments.
#'
#' @return Updated variables.
#'
#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


"fclustEstep" <- function(par, curve_ok, vars, S, hard) {
  #Qui richiamo gli oggetti necessari per la function
  N <- dim(vars$gamma)[1]
  K <- dim(vars$gamma)[2]
  q <- dim(vars$gamma)[3]#Così equivale alla sum q
  Gamma <- par$Gamma
  Lambda.alpha <- par$lambda.zero + par$Lambda %*% t(par$alpha)
  vars$gprod <- vars$gcov <- NULL
  for (j in 1:N) {
    # Calculate expected value of gamma.
    if (length(which(curve_ok$jamesID == j)) == 1){
      Sj <- matrix( S[curve_ok$jamesID == j, ],nrow=1)
    }  else {
      Sj <- S[curve_ok$jamesID == j, ]
    }
    
    # Sj <- S[curve_ok$jamesID == j, ]
    nj <- sum(curve_ok$jamesID == j) #determina quanti dati del soggetto j
    # Build measure-specific inverse variance for subject j
    meas_j <- curve_ok$measureID[curve_ok$jamesID == j]
    sigma_vec <- par$sigma[as.character(meas_j)]
    invvar <- diag(1 / sigma_vec)
    # 
    # Vj <- diag(sigma_vec, nrow = nj) + Sj %*% Gamma %*% t(Sj)
    # 
    # Cgamma <- Gamma -
    #   Gamma %*% t(Sj) %*% solve(Vj, Sj %*% Gamma)
    # 
    # 
    Cgamma <- Gamma - Gamma %*% t(Sj) %*% invvar %*% solve(diag(nj) + Sj %*%
                                                  Gamma %*% t(Sj) %*% invvar)  %*% Sj %*% Gamma
    
    centx <- curve_ok$value[curve_ok$jamesID == j] - Sj %*% Lambda.alpha
    vars$gamma[j, , ] <- t(Cgamma %*% t(Sj) %*% invvar %*% centx)
    # Calculate pi i given j.
    covx <- Sj %*% par$Gamma %*% t(Sj) + solve(invvar)
    d <- exp(-diag(t(centx) %*% solve(covx) %*% centx) / 2) * par$pi
    #vars$piigivej[j,  ] <- d/sum(d)
    #Qui ci sono delle aggiunte di simone, servono nel caso in cui sum(d) == 0,
    #così da non avere errori ponendo quel caso uguale a zero per definizione
    if (sum(d) != 0)
      vars$piigivej[j, ] <- d / sum(d)
    else
      vars$piigivej[j, ] <- 0
    
    if (hard) {
      m <- order(-d)[1]
      vars$piigivej[j, ] <- 0
      vars$piigivej[j, m] <- 1
    }
    
    # Calculate expected value of gamma %*% t(gamma).
    vars$gprod <- cbind(vars$gprod,
                        t(matrix(vars$gamma[j, , ], K, q)) %*% (matrix(vars$gamma[j, , ], K, q) * vars$piigivej[j, ]) + Cgamma)
    vars$gcov <- cbind(vars$gcov, Cgamma)
  }
  return(vars)
}

#' fclustMstep
#'
#' @description
#' Internal M-step for functional clustering.
#'
#' @param parameters Current model parameters.
#' @param curve_ok Ordered long-format observations.
#' @param S Block design matrix.
#' @param vars Current latent-variable moments and posterior probabilities.
#' @param p_measures Named spline-basis dimensions by measure.
#' @param hard Logical hard-clustering flag.
#' @param p Rank retained in the random-effect covariance estimate.
#' @param pert1 Ridge perturbation used in linear solves.
#' @param tol Relative convergence tolerance.
#'
#' @return Updated parameters.
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


# M step
"fclustMstep" <- function(parameters,
                          curve_ok,
                          S,
                          vars,
                          p_measures,
                          hard,
                          p,
                          pert1,
                          tol) {
  #Prova del M step
  #Richiamo i paramentri che mi servono

  alpha <- parameters$alpha
  Lambda <- parameters$Lambda
  gamma <- vars$gamma
  gcov <- vars$gcov
  curve <- curve_ok$jamesID #Così faccio coincidere la mia notazione con quella di James and Sugar
  piigivej <- vars$piigivej
  N <- dim(gamma)[1]
  K <- dim(alpha)[1]
  h <- dim(alpha)[2]
  n <- length(curve)
  q <- dim(S)[2]#Così facendo, non devo più usare sum(q)
  sigma.old <- rep(2, length(unique(curve_ok$measureID)) )
  sigma <- rep(1, length(unique(curve_ok$measureID)) )
  measures <- unique(as.character(curve_ok$measureID))
  names(sigma) <- measures
  
  # Compute pi.
  if (hard)
    parameters$pi <- rep(1 / K, K)
  else
    parameters$pi <- apply(vars$piigivej, 2, mean)
  # Compute rank p estimate of Gamma
  ind <- matrix(rep(c(rep(c(
    1, rep(0, q - 1)
  ), N), 0), q)[1:(N * q ^ 2)], N * q, q)
  gsvd <- svd(vars$gprod %*% ind / N)
  gsvd$d[-(1:p)] <- 0
  parameters$Gamma <- gsvd$u %*% diag(gsvd$d) %*% t(gsvd$u)
  # This loop iteratively calculates alpha and then Lambda and stops
  # when they have converged.
  #Nel ciclo sono state aggiunte le perturbazioni pert1, nel caso in cui non funzioni l'inversione per autovalori troppo piccoli
  
  loopnumber <- 1
  while (( sum((sigma.old - sigma)^2) / sum(sigma^2) > tol) &
         (loopnumber < 10)) {
    sigma.old <- sigma
    # Calculate lambda.zero.
    gamma.pi <- diag(S %*% t(apply(
      gamma * as.vector(piigivej), c(1, 3), sum
    )[curve, ]))
    alpha.pi <- t(matrix(apply(alpha, 2, function(x, piigivej, K) {
      rep(1, K) %*% (piigivej * x)
    }, t(piigivej), K), N, h)[curve, ])
    lambda.zero <- solve(crossprod(S) + pert1 * diag(sum(q))) %*% 
      crossprod(S, (curve_ok$value - diag(S %*% Lambda %*% alpha.pi) - gamma.pi) )
    
    x <- curve_ok$value - S %*% lambda.zero
    # Calculate alpha.
    S.Lam <- S %*% Lambda
    for (i in 1:K) {
      S.Lam.pi <- S.Lam * piigivej[curve, i]
      if (sum(piigivej[, i]) > 10 ^ (-4))
        alpha[i, ] <- solve( crossprod(S.Lam.pi,S.Lam) ) %*%
          crossprod(S.Lam.pi, x - diag(S %*% t(gamma[curve, i, ]) ) )
      else
        print("Warning: empty cluster")
    }
    # Calculate Lambda given alpha. This is done by iterating
    # through each column of Lambda holding the other columns fixed.
    for (m in 1:h) {
      pi.alphasq <- apply(t(piigivej) * (alpha ^ 2)[, m], 2, sum)[curve]
      pi.alpha <- apply(t(piigivej) * alpha[, m], 2, sum)[curve]
      S.Lambda <- t(S %*% Lambda)
      
      if (h != 1) {
        temp <- matrix(0, nrow = dim(S)[1], ncol = K)
        for (i in 1:K) {
          # temp <- cbind(temp, as.vector(rep(1, h - 1) %*% matrix((S.Lambda *
          #                                                           alpha[i, ])[-m, ], h - 1, dim(S)[1]
          # )) * alpha[i, m])
          temp[, i] <- as.vector(rep(1, h - 1) %*% matrix((S.Lambda * alpha[i, ])[-m, ], h - 1, dim(S)[1] )) * alpha[i, m]
        }
        otherLambda <- (temp * piigivej[curve, ]) %*% rep(1, K)
      }
      else
        otherLambda <- 0
      
      gamma.pi.alpha <- apply(gamma * as.vector(piigivej) *
                                rep(alpha[, m], rep(N, K)),
                              c(1, 3),
                              sum)[curve, ]
      Lambda[, m] <- solve( crossprod(S * pi.alphasq, S) + pert1 * diag(sum(q))) %*% t(S) %*%
        (x * pi.alpha - otherLambda - (S * gamma.pi.alpha) %*% rep(1, sum(q))) #Aggiunta
    }
    # Calculate sigma per measure
    
    ### DA CONTROLLARE CHE FACCIA LA STESSA COSA: 
    
    # ind <- matrix(rep(c(rep(F, q), rep(T, N * q)), N)
    #               [1:(N * N * q)], N, N * q, byrow = T)[rep(1:N, table(curve)), ]
    # mat1 <- matrix(rep(S, N), n, N * q)
    # mat3 <- t(mat1)
    # mat3[t(ind)] <- 0
    # ind2 <- matrix(rep(c(rep(F, q), rep(T, N * q)), N)[1:(N * N * q)], N, N * q, byrow = T)[rep(1:N, rep(q, N)), ]
    # mat2 <- matrix(rep(t(gcov), N), N * q, N * q, byrow = T)
    # mat2[ind2] <- 0
    # econtrib <- 0
    # for (i2 in 1:K) {
    #   vect1 <- x - S %*% Lambda %*% alpha[i2, ] - (S * gamma[curve, i2, ]) %*% rep(1, q)
    #   econtrib <- econtrib + t(piigivej[curve, i2] * vect1) %*% vect1
    # }
    # sigma <- as.vector((econtrib + sum(diag(
    #   mat1 %*% mat2 %*% mat3
    # ))) / n)
    
    for (meas in measures) {
      meas_idx <- which(curve_ok$measureID == meas)
      n_meas <- length(meas_idx)
      
      # Calculate residuals and variance contributions for this measure
      econtrib_meas <- 0
      for (i2 in 1:K) {
        vect1 <- x[meas_idx] - S[meas_idx, ] %*% Lambda %*% alpha[i2, ] - (S[meas_idx, ] * gamma[curve[meas_idx], i2, ]) %*% rep(1, q)
        econtrib_meas <- econtrib_meas + crossprod(piigivej[curve[meas_idx], i2] * vect1, vect1)
      }
      
      # da inidicizzare al q
      meas_q = p_measures[meas]
      if(meas == measures[1]) {
        meas_q_index = 1:meas_q
      } else {
        meas_q_index = sum(p_measures[measures[1:(which(measures == meas) - 1)]]) + 1:meas_q
      }
      
      meas_q_index_col = rep(0:(N-1)* q,each = meas_q ) + rep(x = meas_q_index,N )
      
      ind <- matrix(rep(c(rep(F, meas_q), rep(T, N * meas_q)), N)[1:(N * N * meas_q)],
                    N, N * meas_q, byrow = T)[rep(1:N, table(curve[meas_idx])), ]
      mat1 <- matrix(rep(S[meas_idx, meas_q_index ], N), n_meas, N * meas_q)
      mat3 <- t(mat1)
      mat3[t(ind)] <- 0
      # mat3 sarebbe una matrice a blocchi fissato la misura in cui le vrie piccole metric sono le S di ongi singlol subject
      
      ind2 <- matrix(rep(c(rep(F, meas_q), rep(T, N * meas_q)), N)[1:(N * N * meas_q)], N, N * meas_q, byrow = T)[rep(1:N, rep(meas_q, N)), ]
      mat2 <- matrix(rep(t(gcov[meas_q_index, meas_q_index_col ]), N), N * meas_q, N * meas_q, byrow = T) # gcov da indicizzare per misura?
      mat2[ind2] <- 0
      
      sigma[meas] <- as.vector((econtrib_meas + sum(diag(
        mat1 %*% mat2 %*% mat3
      ))) / n_meas)
    }
    loopnumber <- loopnumber + 1

  }
  
  parameters$lambda.zero <- as.vector(lambda.zero)
  parameters$alpha <- alpha
  parameters$Lambda <- Lambda
  parameters$sigma <- sigma
  return(parameters)
}

#' Memory-efficient M-step for functional clustering
#'
#' Implements the same parameter update as `fclustMstep()` while computing the
#' random-effect contribution to each measure-specific variance by subject,
#' avoiding large temporary block matrices.
#'
#' @inheritParams fclustMstep
#' @return Updated model parameters, including a named variance vector `sigma`.
#' @keywords internal
#' @noRd
"fclustMstepNEW" <- function(parameters,
                          curve_ok,
                          S,
                          vars,
                          p_measures,
                          hard,
                          p,
                          pert1,
                          tol) {
  #Prova del M step
  #Richiamo i paramentri che mi servono
  
  alpha <- parameters$alpha
  Lambda <- parameters$Lambda
  gamma <- vars$gamma
  gcov <- vars$gcov
  curve <- curve_ok$jamesID #Così faccio coincidere la mia notazione con quella di James and Sugar
  piigivej <- vars$piigivej
  N <- dim(gamma)[1]
  K <- dim(alpha)[1]
  h <- dim(alpha)[2]
  n <- length(curve)
  q <- dim(S)[2]#Così facendo, non devo più usare sum(q)
  sigma.old <- rep(2, length(unique(curve_ok$measureID)) )
  sigma <- rep(1, length(unique(curve_ok$measureID)) )
  measures <- unique(as.character(curve_ok$measureID))
  names(sigma) <- measures
  
  # Compute pi.
  if (hard)
    parameters$pi <- rep(1 / K, K)
  else
    parameters$pi <- apply(vars$piigivej, 2, mean)
  # Compute rank p estimate of Gamma
  ind <- matrix(rep(c(rep(c(
    1, rep(0, q - 1)
  ), N), 0), q)[1:(N * q ^ 2)], N * q, q)
  gsvd <- svd(vars$gprod %*% ind / N)
  gsvd$d[-(1:p)] <- 0
  parameters$Gamma <- gsvd$u %*% diag(gsvd$d) %*% t(gsvd$u)
  
  # Sigma contiene VARIANZE per misura. Il suo numeratore ha due termini:
  # 1) residui al quadrato, pesati con le probabilita dei cluster;
  # 2) incertezza degli effetti casuali: somma_j tr(S_jm C_j S_jm').
  # C_j e la covarianza condizionata restituita dall'ultimo E-step (gcov),
  # non la Gamma appena aggiornata. S e gcov restano fisse nel while:
  # possiamo quindi calcolare il secondo termine una sola volta per M-step.
  measure_rows <- lapply(measures, function(meas) {
    which(curve_ok$measureID == meas)
  })
  names(measure_rows) <- measures
  variance_rows <- numeric(n)
  subject_rows <- split(seq_len(n), curve)
  for (subject in names(subject_rows)) {
    j <- as.integer(subject)
    rows_j <- subject_rows[[subject]]
    Sj <- S[rows_j, , drop = FALSE]
    # gcov = [C_1 | C_2 | ... | C_N], ogni blocco ha q colonne.
    Cj <- gcov[, (j - 1L) * q + seq_len(q), drop = FALSE]
    # diag(Sj %*% Cj %*% t(Sj)) senza costruire la matrice quadrata.
    # Usiamo tutte le q colonne: gli zeri di S selezionano automaticamente
    # il blocco della misura, anche con numeri diversi di basi per misura.
    variance_rows[rows_j] <- rowSums((Sj %*% Cj) * Sj)
  }
  covariance_contrib <- vapply(measure_rows, function(rows) {
    sum(variance_rows[rows])
  }, numeric(1))
  
  # This loop iteratively calculates alpha and then Lambda and stops
  # when they have converged.
  #Nel ciclo sono state aggiunte le perturbazioni pert1, nel caso in cui non funzioni l'inversione per autovalori troppo piccoli
  
  loopnumber <- 1
  while (( sum((sigma.old - sigma)^2) / sum(sigma^2) > tol) &
         (loopnumber < 10)) {
    sigma.old <- sigma
    # Calculate lambda.zero.
    gamma.pi <- diag(S %*% t(apply(
      gamma * as.vector(piigivej), c(1, 3), sum
    )[curve, ]))
    alpha.pi <- t(matrix(apply(alpha, 2, function(x, piigivej, K) {
      rep(1, K) %*% (piigivej * x)
    }, t(piigivej), K), N, h)[curve, ])
    lambda.zero <- solve(crossprod(S) + pert1 * diag(sum(q))) %*% 
      crossprod(S, (curve_ok$value - diag(S %*% Lambda %*% alpha.pi) - gamma.pi) )
    
    x <- curve_ok$value - S %*% lambda.zero
    # Calculate alpha.
    S.Lam <- S %*% Lambda
    for (i in 1:K) {
      S.Lam.pi <- S.Lam * piigivej[curve, i]
      if (sum(piigivej[, i]) > 10 ^ (-4))
        alpha[i, ] <- solve( crossprod(S.Lam.pi,S.Lam) ) %*%
          crossprod(S.Lam.pi, x - diag(S %*% t(gamma[curve, i, ]) ) )
      else
        print("Warning: empty cluster")
    }
    # Calculate Lambda given alpha. This is done by iterating
    # through each column of Lambda holding the other columns fixed.
    for (m in 1:h) {
      pi.alphasq <- apply(t(piigivej) * (alpha ^ 2)[, m], 2, sum)[curve]
      pi.alpha <- apply(t(piigivej) * alpha[, m], 2, sum)[curve]
      S.Lambda <- t(S %*% Lambda)
      
      if (h != 1) {
        temp <- matrix(0, nrow = dim(S)[1], ncol = K)
        for (i in 1:K) {
          # temp <- cbind(temp, as.vector(rep(1, h - 1) %*% matrix((S.Lambda *
          #                                                           alpha[i, ])[-m, ], h - 1, dim(S)[1]
          # )) * alpha[i, m])
          temp[, i] <- as.vector(rep(1, h - 1) %*% matrix((S.Lambda * alpha[i, ])[-m, ], h - 1, dim(S)[1] )) * alpha[i, m]
        }
        otherLambda <- (temp * piigivej[curve, ]) %*% rep(1, K)
      }
      else
        otherLambda <- 0
      
      gamma.pi.alpha <- apply(gamma * as.vector(piigivej) *
                                rep(alpha[, m], rep(N, K)),
                              c(1, 3),
                              sum)[curve, ]
      Lambda[, m] <- solve( crossprod(S * pi.alphasq, S) + pert1 * diag(sum(q))) %*% t(S) %*%
        (x * pi.alpha - otherLambda - (S * gamma.pi.alpha) %*% rep(1, sum(q))) #Aggiunta
    }
    # Aggiorna le varianze con i nuovi lambda.zero, Lambda e alpha.
    # Soltanto i residui cambiano nel while; il contributo di covarianza
    # e gia stato calcolato sopra, senza le grandi mat1, mat2 e mat3.
    for (meas in measures) {
      meas_idx <- measure_rows[[meas]]
      n_meas <- length(meas_idx)
      S_meas <- S[meas_idx, , drop = FALSE]
      
      # E[residuo^2]: per ogni cluster sommiamo pi_jk * residuo_jk^2.
      econtrib_meas <- 0
      for (i2 in 1:K) {
        gamma_meas <- matrix(gamma[curve[meas_idx], i2, ],
                             nrow = n_meas, ncol = q)
        residual <- as.vector(x[meas_idx] -
                                S_meas %*% Lambda %*% alpha[i2, ]) -
          rowSums(S_meas * gamma_meas)
        econtrib_meas <- econtrib_meas +
          sum(piigivej[curve[meas_idx], i2] * residual^2)
      }
      
      # Dividiamo per le osservazioni della misura, non per i soggetti.
      # Nessuna radice quadrata: sigma[meas] rappresenta una varianza.
      sigma[meas] <- (econtrib_meas + covariance_contrib[[meas]]) / n_meas
    }
    loopnumber <- loopnumber + 1
    
  }
  
  parameters$lambda.zero <- as.vector(lambda.zero)
  parameters$alpha <- alpha
  parameters$Lambda <- Lambda
  parameters$sigma <- sigma
  return(parameters)
}

#' fclustconst
#'
#' @description
#' Enforces constraints on functional clustering parameters.
#'
#' @param parameters Fitted model parameters.
#' @param vars Final latent-variable moments and posterior probabilities.
#' @param S Block design matrix.
#' @param measureID Measure associated with each row of `S`.
#'
#' @return List with updated parameters and variables.
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


# Preso senza modifiche, questa function mi calcola i cfit
"fclustconst" <- function(parameters, vars, S, measureID) {
  # This function enforces the constraint (7) from the paper on the
  # parameters. This means that the alphas can be interpreted as the
  # number of standard deviations that the groups are apart etc.
  par <- parameters
  # Use mean of measure-specific sigmas for the global constraint
  sigma_rows <- unname(par$sigma[as.character(measureID)])
  V <- diag(sigma_rows, nrow = nrow(S)) + S %*% par$Gamma %*% t(S)
  A <- crossprod(S, solve(V, S))  
  #A <- t(S) %*% solve(sigma_mean * diag(dim(S)[1]) + S %*% par$Gamma %*% t(S)) %*% S
  svdA <- svd(A)
  sqrtA <- diag(sqrt(svdA$d)) %*% t(svdA$u)
  negsqrtA <- svdA$u %*% diag(1 / sqrt(svdA$d))
  finalsvd <- svd(sqrtA %*% par$Lambda)
  par$Lambda <- negsqrtA %*% finalsvd$u
  if (dim(par$Lambda)[2] > 1) {
    par$alpha <- t(diag(finalsvd$d) %*% t(finalsvd$v) %*% t(par$alpha))
  } else {
    par$alpha <- t(finalsvd$d * t(finalsvd$v) %*% t(par$alpha))
  }
  meanalpha <- apply(par$alpha, 2, mean)
  par$alpha <- t(t(par$alpha) - meanalpha)
  par$lambda.zero <- par$lambda.zero + par$Lambda %*% meanalpha
  return(list(parameters = par, vars = vars))
}


#' Predict clusters from a fitted functional model
#'
#' @param fit Constrained fit returned by `fclustconst()`.
#' @param data Preprocessed clustering data.
#' @param reweight Logical; include fitted mixing proportions in posterior
#'   classification probabilities.
#' @param pert2 Ridge perturbation for inversion of the projected-score metric.
#'
#' @return A list containing score covariance matrices, estimated scores,
#'   predicted classes, distances, maximum probabilities, and full posterior
#'   probabilities.
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


# Questa function mi calcola la predizione finale
# Qui domandona da un milione di dollari, sulla S_ij
"fclust_pred" <- function(fit,
                          data = NULL,
                          reweight = FALSE,
                          pert2 = 0) {
  # FullS <- data1$FullS
  S <- data$S
  M <- data$CData$measureID
  par <- fit$parameters
  curve <- data$CData$jamesID
  time <- data$CData$timeindex
  N <- length(table(curve))
  h <- dim(par$alpha)[2]
  alpha.hat <- matrix(0, N, h)
  K <- dim(fit$par$alpha)[1]
  distance <- matrix(0, N, K)
  Calpha <- array(0, c(N, h, h))
  for (i in 1:N) {
    Si <- S[data$CData$jamesID == i, ]
    yi <- data$CData$value[data$CData$jamesID == i]
    meas_i <- data$CData$measureID[data$CData$jamesID == i]
    n <- length(yi)
    # Build measure-specific variance matrix for subject i
    sigma_vec_i <- par$sigma[as.character(meas_i)]
    Sigma <- diag(sigma_vec_i) + Si %*% par$Gamma %*% t(Si)
    # Calculate covariance for each alpha hat.
    InvCalpha <- t(par$Lambda) %*% t(Si) %*% solve(Sigma) %*% Si %*%
      par$Lambda
    Calpha[i, , ] <- solve(InvCalpha + pert2 * diag(dim(InvCalpha)[1]))
    # Calculate each alpha hat.
    alpha.hat[i, ] <- Calpha[i, , ] %*% t(par$Lambda) %*% t(Si) %*% solve(Sigma) %*% (yi - Si %*% par$lambda.zero)
    # Calculate the matrix of distances, relative to the
    # appropriate metric of each curve from each class centroid.
    # Normalize distance by average sigma per measure to balance across different scales
    n_meas_i <- length(unique(meas_i))
    avg_sigma_i <- mean(par$sigma[as.character(meas_i)])
    for (k in 1:K) {
      y <- as.vector(alpha.hat[i, ]) - fit$par$alpha[k, ]
      distance[i, k] <- (t(y) %*% InvCalpha %*% y)
    }
  }
  # Calculate final class predictions for each curve.
  class.pred <- rep(1, N)
  log.pi <- log(fit$par$pi)
  if (!reweight) {
    log.pi <- rep(0, K)
  }
  # Use log-sum-exp trick to avoid numerical underflow with large distances
  # log_unnorm_probs is N × K where each row k contains log(pi_k) - distance_{i,k}/2
  log_unnorm_probs <- t(log.pi - t(distance) / 2)
  # Subtract max per row to prevent underflow in exp()
  max_per_row <- apply(log_unnorm_probs, 1, max)
  log_unnorm_probs <- log_unnorm_probs - max_per_row
  # Now exp() is numerically stable
  probs <- exp(log_unnorm_probs)
  probs <- probs / apply(probs, 1, sum)
  m <- probs[, 1]
  if (K != 1) {
    for (k in 2:K) {
      test <- (probs[, k] > m)
      class.pred[test] <- k
      m[test] <- probs[test, k]
    }
  }
  pred <- list(
    Calpha = Calpha,
    alpha.hat = alpha.hat,
    class.pred = class.pred,
    distance = distance,
    m = m,
    probs = probs
  )
  return(pred)
}

#' Row-wise maxima and their positions
#'
#' @param X Numeric matrix.
#'
#' @return A list with the index (`ind`) and value (`max`) of each row maximum.
#'

#' @keywords internal
#' @noRd
#'
"nummax" <- function(X) {
  ind <- rep(1, dim(X)[1])
  m <- X[, 1]
  if (dim(X)[2] > 1) {
    for (i in 2:dim(X)[2]) {
      test <- X[, i] > m
      ind[test] <- i
      m[test] <- X[test, i]
    }
  }
  list(ind = ind, max = m)
}
#' Predict fitted curves and uncertainty bands
#'
#' @param data Fit returned by `intfclust()`.
#' @param tau Overall coverage level used to derive prediction quantiles.
#' @param tau1 Posterior mass retained when combining likely clusters.
#' @param q Named spline-basis dimensions by measure.
#' @param KData Preprocessed clustering data.
#'
#' @return A named list by measure containing fitted subject curves, confidence
#'   and prediction bounds, and fitted cluster mean curves.
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange

"fclust.curvepred" <- function(data,
                               tau = 0.95,
                               tau1 = 0.975,
                               q,
                               KData) {
  fit <- data$cfit
  FullS <- KData$FullS
  data <- KData$CData
  # N indica il numero di soggetti studiati
  N <- length(unique(data$jamesID))
  # M è il vettore contente le misure, applicando l'unique
  M <- sort(as.character(unique(data$measureID)))

  tau2 <- tau / tau1
  sigma <- fit$par$sigma # Now measure-specific: sigma is a named vector indexed by measure
  Gamma <- fit$par$Gamma # vanno prese ncol pari al valore di q[j]
  Lambda <- fit$par$Lambda # Prendo le righe pari al valore di q[j]
  alpha <- fit$par$alpha
  lambda.zero <- as.vector(fit$par$lambda.zero)

  # Dopo aver estratto i parametri, li tiro fouri per ogni curva di appartenenza.
  a_old <- 1
  Final_result <- list()


  for (gei in M) {
    a_new <- q[gei] + a_old - 1

    Gamma1 <- Gamma[(a_old:a_new), (a_old:a_new)]

    Lambda1 <- Lambda[a_old:a_new, ]
    if (is.null(dim(Lambda1))) {
      Lambda1 <- matrix(Lambda1, ncol = 1, nrow = q[gei])
    }

    lambda.zero1 <- lambda.zero[a_old:a_new]


    a_old <- a_new + 1


    # Servirà fare un ciclo per ogni j
    # Costruiamo per j fissato
    # Costruisso S già unlistata
    FullSj <- FullS[[gei]]

    upci <- lowci <- uppi <- lowpi <- gpred <- matrix(0, N, nrow(FullSj))
    etapred <- matrix(0, N, ncol(FullSj))
    ind <- 1
    Lambda.alpha <- lambda.zero1 + Lambda1 %*% t(alpha)
    for (i in 1:N) {
      # y sono yij
      y <- data$value[data$jamesID == i & data$measureID == gei]
      Sij <- FullSj[data$timeindex[data$jamesID == i &
        data$measureID == gei], ]
      ni <- dim(Sij)[1]
      # Use measure-specific sigma for this measure
      sigma_j <- sigma[as.character(gei)]
      invvar <- diag(1 / rep(sigma_j, ni))
      covx <- Sij %*% Gamma1 %*% t(Sij) + solve(invvar)
      centx <- data$value[data$jamesID == i &
        data$measureID == gei ] - Sij %*% Lambda.alpha
      d <- exp(-diag(t(centx) %*% solve(covx) %*% centx) / 2) * fit$par$pi
      if (sum(d) != 0) {
        pi <- d / sum(d)
        ord <- order(-pi)
        numb <- min(sum(cumsum(pi[ord]) <= tau1) + 1, nrow(alpha))
      } else {
        # Da controllare: prima il numb era fuori ma dava errore in  sum(cumsum(pi[ord]) <= tau1) + 1 > numero di righe di alfa

        pi <- rep(0, length(d))
        ord <- order(-pi)
        numb <- nrow(alpha)
      }

      K <- length(pi)
      mu <- lambda.zero1 + Lambda1 %*% t(alpha * pi) %*% rep(1, K)
      cov <- (
        Gamma1 - Gamma1 %*% t(Sij) %*% solve(diag(ni) + Sij %*% Gamma1 %*%
          t(Sij) / sigma_j) %*% Sij %*% Gamma1 /
          sigma_j
      ) / sigma_j
      etapred[ind, ] <- mu + cov %*% t(Sij) %*% (y - Sij %*% mu)

      v <- pmax(diag(FullSj %*% (cov * sigma_j) %*% t(FullSj)), 0)
      pse <- sqrt(v + sigma_j)
      se <- sqrt(v)

      lower.p <- upper.p <- lower.c <- upper.c <- matrix(0, nrow(FullSj), numb)
      for (j in 1:numb) {
        #
        mu <- lambda.zero1 + Lambda1 %*% alpha[ord[j], ]
        mean <- FullSj %*% (mu + cov %*% t(Sij) %*% (y - Sij %*% mu))
        upper.p[, j] <- mean + qnorm(tau2) * pse
        lower.p[, j] <- mean - qnorm(tau2) * pse
        upper.c[, j] <- mean + qnorm(tau2) * se
        lower.c[, j] <- mean - qnorm(tau2) * se
      }

      upci[ind, ] <- nummax(upper.c)$max
      lowci[ind, ] <- -nummax(-lower.c)$max
      uppi[ind, ] <- nummax(upper.p)$max
      lowpi[ind, ] <- -nummax(-lower.p)$max
      gpred[ind, ] <- as.vector(FullSj %*% etapred[ind, ])
      ind <- ind + 1
    }
    meancurves <- FullSj %*% Lambda.alpha
    Final_result[[gei]] <- list(
      gpred = gpred,
      upci = upci,
      lowci = lowci,
      uppi = uppi,
      lowpi = lowpi,
      meancurves = meancurves
    )
  }

  return(Final_result)
}
