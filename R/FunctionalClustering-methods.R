#' presetKmeans
#'
#'@description
#'
#'  presets data for the kmeans
#'
#' @param CData data
#' @param natural ...
#' @param q ...
#' @param pert perturbation
#'
#' @return a list with all necessary data
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
  
  
  
  #Questa funzione serve ad inizializzare i parametri da usare
  # riceve i dati già ordinati per jamesID, measureID e time
  #TODO capire se serve la grid qua
  # Definisco J come il numero di misurazioni presenti nel dataset
  J <- length(unique(CData$measureID))
  #N indica il numero di soggetti studiati
  N <- length(unique(CData$jamesID))
  #M è il vettore contente le misure, applicando l'unique
  M <- sort(unique(CData$measureID))
  
  #Definiamo la griglia(faccio una lista i tempi per la j-esima misura)
  
  grid <- list()
  
  for (j in 1:J) {
    a <- sort(unique(CData$time[CData$measureID == M[j]]))
    grid <- list.append(grid, a)
  }
  names(grid)<-M
  
  #Faccio una lista contente le FullS per ogni misura
  FullS <- list()
  #Metto la condizione if, per dare la possibilità di cambiare il tipo di base usata (ns o bs)
  if (natural == TRUE) {
    for (j in 1:J) {
      b <- cbind(1, ns(grid[[j]], df = q[j] - 1)) #unlist serve poiché se no non mi legge il vettore (ho controllato che mantenga l'ordine e non cambi gli elemnti)
      b <- svd(b)$u
      FullS <- list.append(FullS, b)
    }
  }
  else{
    for (j in 1:J) {
      b <- cbind(1, bs(grid[[j]], df = q[j] - 1))
      b <- svd(b)$u
      FullS <- list.append(FullS, b)
    }
  }
  #
  #Creo i timeindex, poiché nel dataset iniziale io ho solo i tempi ma non come essi siano posizionati nella griglia
  timeindex <- NULL
  for (i in 1:N) {
    t <- NULL
    for (j in 1:J) {
      d <- match(CData$time[CData$measureID == M[j] &
                              CData$jamesID == i], grid[[j]])
      #d <- d[!is.na(d)]
      t <- c(t, d)
    }
    timeindex <- c(timeindex, t)
  }
  
  CData$timeindex <- timeindex
  #I timeindex così definiti sono un vettore, dove prima ci sono gli indici del primo soggetto, poi del secondo, ect...
  #Ora costruisco le S, matrici diagonali a blocchi, come definite in James and Sugar
  S <- NULL
  
  for (i in 1:N) {
    Si <- NULL
    for (j in 1:J) {
      A <- FullS[[j]][CData$timeindex[(CData$jamesID == i &
                                         CData$measureID == M[j])], ]
      if (j == 1) {
        Si <- A
      }
      else{
        Si <- bdiag(Si, A)
      }#Questo if è necessario poiché bdiag ha bisogno di una matrice iniziale su cui attaccarsi (ovvero quella della prima misura)
    }

    if (is.null(dim(Si))){
      Si <- matrix(Si,nrow=1)
    }  else {
      Si <- as.matrix(Si)
    }
    
    S <- rbind(S, Si) #In questo modo attacco le matrice dei vari soggetti una sotto l'altra (tanto il numero di colonne è uguale per ogni soggetto, ovvero sum(q))
  }
  
  #S <- as.matrix(S)# è necessario perché rbind, non riempie le matrici con gli zeri
  
  #Calcolo i coefficienti iniziali delle spline, usando il metodo dell'errore quadratico (penso qua si usi effetivamente la svd?)
  points <- matrix(0, N, sum(q))
  #
  for (i in 1:N) {
    if (length(which(CData$jamesID == i)) == 1){
      Si <- matrix( S[CData$jamesID == i, ],nrow=1)
    }  else {
      Si <- S[CData$jamesID == i, ]
    }
    yi <- CData$value[CData$jamesID == i]
    points[i, ]  <- solve(t(Si) %*% Si + pert * diag(sum(q))) %*% t(Si) %*% 
      yi
  }
  return(list(
    CData = CData,
    TimeGrids=grid,
    points = points,
    N = N,
    S = S,
    FullS = FullS
  ))
}


#' justKmeans
#'
#'@description
#'
#'  uses kmeans
#'
#' @param CLUSTData data
#' @param k number of clusters
#'
#' @return results of kmeans
#'
#' @keywords internal
#' @noRd
#' @seealso kmeans()
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


#Parte randomica dell'algoritmo, è necessario avere la parte di codice per K=1, poiché serve nello stimare gli iper-parametri
"justKmeans" <- function(CLUSTData, K) {
  if (K > 1) {
    
    class <- kmeans(CLUSTData$points, K, 10)$cluster
  }
  else{
    class <- rep(1, CLUSTData$N)
  }
  return(class)
}


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
        unic_vectors[[i]], levels = unique(unic_vectors[[i]])
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


#' intfclust
#'
#'@description
#'
#'  ...
#'
#' @param q ...
#' @param h ...
#' @param pert ...
#' @param K ...
#' @param class ... 
#' @param CLUSTData ... 
#' @param pert1 ...
#' @param tol ...
#' @param maxit ...
#' @param hard .....
#' @param pert2 ...
#'
#' @return ...
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
  #Parte modificata da simone, così non si hanno problemi nel caso in cui in una classe ci sia un solo elemento
  for (l in 1:K)
  {
    if (sum(class == l) > 1)
      classmean[l, ] <- apply(CLUSTData$points[class == l, ], 2, mean)
    else
      classmean[l, ] <- CLUSTData$points[class == l, ]
  }
  
  #Calcolo i valori che mi servono
  lambda.zero <- apply(classmean, 2, mean)
 
  Lambda <- as.matrix(svd(scale(classmean, scale = F))$v[, 1:h])
  alpha <- scale(classmean, scale = F) %*% Lambda
  
  gamma <- t(t(CLUSTData$points) - lambda.zero - (Lambda %*% t(alpha[class, ])))
  gprod <- NULL
  for (i in 1:CLUSTData$N) {
    gprod <- cbind(gprod, (gamma[i, ]) %*% t(gamma[i, ]))
  }
  b = sum(q)
  gamma <- array(gamma[, rep(1:sum(q), rep(K, sum(q)))], c(CLUSTData$N, K, sum(q)))
  gcov <- matrix(0, b, CLUSTData$N * b)
  
  #Definisco i dati in uscita
  
  # data1 = list(
  #   # S = CLUSTData$S,
  #   # FullS = CLUSTData$FullS,
  #   #starter = CData,
  parameters = list(lambda.zero = lambda.zero,
                    Lambda = Lambda,
                    alpha = alpha)
  vars = list(
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
  sigma.old <- 0
  sigma.new <- 1
  ind <- 1
  
  #Iterazioni massime del ciclo EM
  # Main loop. Iterates between M and E steps and stops when
  # sigma  has converged.
  while (abs(sigma.old - sigma.new) / sigma.new > tol &
         (ind <= maxit)) {
    
    parameters <- fclustMstep(
      parameters = parameters,
      curve_ok = CLUSTData$CData,
      S = S,
      vars = vars,
      hard = hard,
      p = 5,
      pert1 = pert1,
      tol = tol
    )
    
    vars <- fclustEstep(
      parameters = parameters,
      curve_ok = CLUSTData$CData,
      vars = vars,
      S,
      hard = hard
    )
    sigma.old <- sigma.new
    sigma.new <- parameters$sigma[1]
    ind <- ind + 1
  }
  #starter <- initial$starter #Richiamo i valori inziali, poiché mi servono i timeindex
  ###########starter$S<-S
  
  cfit <- fclustconst(parameters, vars, S)
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
  #In uscita, ho le matrice S e FullS (la S è necessaria per tutto il resto del codice)
  # Parameters contiene i parametri inizializzati che effettivamente descrivono le curve
  #Vars
  #starter dà in uscita il dataset originale ma avendo aggiunto i timeindex, fondamentali per altre parti di codice
  #return(data1)
}



#' fclustEstep
#'
#'@description
#'
#'  ...
#'
#' @param parameters ...
#' @param curve_ok ...
#' @param vars ...
#' @param S ....
#' @param hard ...

#'
#' @return ...
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


# E step
"fclustEstep" <- function(parameters, curve_ok, vars, S, hard) {
  #Qui richiamo gli oggetti necessari per la function
  par <- parameters
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
    invvar <- diag(1 / rep(par$sigma, nj))
    Cgamma <- Gamma - Gamma %*% t(Sj) %*% solve(diag(nj) + Sj %*%
                                                  Gamma %*% t(Sj) %*% invvar) %*% invvar %*% Sj %*% Gamma
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
#'@description
#'
#'  ...
#'
#' @param parameters ...
#' @param curve_ok ...
#' @param S ....
#' @param vars ...
#' @param hard ...
#' @param p ...
#' @param pert1 ...
#' @param tol ...
#'
#' @return ...
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange



#M step
"fclustMstep" <- function(parameters,
                          curve_ok,
                          S,
                          vars,
                          hard,
                          p,
                          pert1,
                          tol) {
  #Prova del M step
  #Richiamo i paramentri che mi servono
  K <- dim(parameters$alpha)[1]
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
  sigma.old <- 2
  sigma <- 1
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
  while ((abs(sigma.old[1] - sigma[1]) / sigma[1] > tol) &
         (loopnumber < 10)) {
    sigma.old <- sigma
    # Calculate lambda.zero.
    gamma.pi <- diag(S %*% t(apply(
      gamma * as.vector(piigivej), c(1, 3), sum
    )[curve, ]))
    alpha.pi <- t(matrix(apply(alpha, 2, function(x, piigivej, K) {
      rep(1, K) %*% (piigivej * x)
    }, t(piigivej), K), N, h)[curve, ])
    lambda.zero <- solve(t(S) %*% S + pert1 * diag(sum(q))) %*% t(S) %*% (curve_ok$value - diag(S %*% Lambda %*% alpha.pi) - gamma.pi)
    x <- curve_ok$value - S %*% lambda.zero
    # Calculate alpha.
    for (i in 1.:K) {
      S.Lam <- S %*% Lambda
      S.Lam.pi <- S.Lam * piigivej[curve, i]
      if (sum(piigivej[, i]) > 10 ^ (-4))
        alpha[i, ] <- solve(t(S.Lam.pi) %*% S.Lam) %*%
        t(S.Lam.pi) %*% (x - diag(S %*% t(gamma[curve, i, ])))
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
        temp <- NULL
        for (i in 1:K) {
          temp <- cbind(temp, as.vector(rep(1, h - 1) %*% matrix((S.Lambda *
                                                                    alpha[i, ])[-m, ], h - 1, dim(S)[1]
          )) * alpha[i, m])
        }
        otherLambda <- (temp * piigivej[curve, ]) %*% rep(1, K)
      }
      else
        otherLambda <- 0
      gamma.pi.alpha <- apply(gamma * as.vector(piigivej) *
                                rep(alpha[, m], rep(N, K)),
                              c(1, 3),
                              sum)[curve, ]
      Lambda[, m] <- solve(t(S * pi.alphasq) %*% S + pert1 * diag(sum(q))) %*% t(S) %*%
        (x * pi.alpha - otherLambda - (S * gamma.pi.alpha) %*% rep(1, sum(q))) #Aggiunta
    }
    # Calculate sigma
    ind <- matrix(rep(c(rep(F, q), rep(T, N * q)), N)
                  [1:(N * N * q)], N, N * q, byrow = T)[rep(1:N, table(curve)), ]
    mat1 <- matrix(rep(S, N), n, N * q)
    mat3 <- t(mat1)
    mat3[t(ind)] <- 0
    ind2 <- matrix(rep(c(rep(F, q), rep(T, N * q)), N)[1:(N * N * q)], N, N * q, byrow = T)[rep(1:N, rep(q, N)), ]
    mat2 <- matrix(rep(t(gcov), N), N * q, N * q, byrow = T)
    mat2[ind2] <- 0
    econtrib <- 0
    for (i2 in 1:K) {
      vect1 <- x - S %*% Lambda %*% alpha[i2, ] - (S * gamma[curve, i2, ]) %*% rep(1, q)
      econtrib <- econtrib + t(piigivej[curve, i2] * vect1) %*% vect1
    }
    sigma <- as.vector((econtrib + sum(diag(
      mat1 %*% mat2 %*% mat3
    ))) / n)
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
#'@description
#'
#'  ...
#'
#' @param parameters ...
#' @param vars ...
#' @param S ...
#'
#' @return ...
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange


#Preso senza modifiche, questa function mi calcola i cfit
"fclustconst" <- function(parameters, vars, S) {
  # This function enforces the constraint (7) from the paper on the
  # parameters. This means that the alphas can be interpreted as the
  # number of standard deviations that the groups are apart etc.
  par <- parameters
  A <- t(S) %*% solve(par$sigma * diag(dim(S)[1]) + S %*% par$Gamma %*%
                        t(S)) %*% S
  svdA <- svd(A)
  sqrtA <- diag(sqrt(svdA$d)) %*% t(svdA$u)
  negsqrtA <- svdA$u %*% diag(1 / sqrt(svdA$d))
  finalsvd <- svd(sqrtA %*% par$Lambda)
  par$Lambda <- negsqrtA %*% finalsvd$u
  if (dim(par$Lambda)[2] > 1)
    par$alpha <- t(diag(finalsvd$d) %*% t(finalsvd$v) %*% t(par$alpha))
  else
    par$alpha <- t(finalsvd$d * t(finalsvd$v) %*% t(par$alpha))
  meanalpha <- apply(par$alpha, 2, mean)
  par$alpha <- t(t(par$alpha) - meanalpha)
  par$lambda.zero <- par$lambda.zero + par$Lambda %*% meanalpha
  return(list(parameters = par, vars = vars))
}


#' fclust_pred
#'
#'@description
#'
#'  ...
#'
#' @param fit ...
#' @param data ....
#' @param rewight ...
#' @param pert2 ...
#'
#' @return ...
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange



#Questa function mi calcola la predizione finale
#Qui domandona da un milione di dollari, sulla S_ij
"fclust_pred" <- function(fit,
                          data = NULL,
                          reweight = FALSE,
                          pert2=0) {
  #FullS <- data1$FullS
  S <- data$S
  M <- data$CData$measureID
  par <- fit$parameters
  curve <- data$CData$jamesID
  time <- data$CData$timeindex
  N <- length(table(curve))
  J <- length(unique(M))
  h <- dim(par$alpha)[2]
  alpha.hat <- matrix(0, N, h)
  K <- dim(fit$par$alpha)[1]
  distance <- matrix(0, N, K)
  Calpha <- array(0, c(N, h, h))
  for (i in 1:N) {
    Si <- S[data$CData$jamesID == i, ]
    yi <- data$CData$value[data$CData$jamesID == i]
    n <- length(yi)
    Sigma <- par$sigma * diag(n) + Si %*% par$Gamma %*% t(Si)
    # Calculate covariance for each alpha hat.
    InvCalpha <- t(par$Lambda) %*% t(Si) %*% solve(Sigma) %*% Si %*%
      par$Lambda
    Calpha[i, , ] <- solve(InvCalpha + pert2 * diag(dim(InvCalpha)[1]))
    # Calculate each alpha hat.
    alpha.hat[i, ] <- Calpha[i, , ] %*% t(par$Lambda) %*% t(Si) %*% solve(Sigma) %*% (yi - Si %*% par$lambda.zero)
    # Calculate the matrix of distances, relative to the
    # appropriate metric of each curve from each class centroid.
    for (k in 1:K) {
      y <- as.vector(alpha.hat[i, ]) - fit$par$alpha[k, ]
      distance[i, k] <- t(y) %*% InvCalpha %*% y
    }
  }
  # Calculate final class predictions for each curve.
  class.pred <- rep(1, N)
  log.pi <- log(fit$par$pi)
  if (!reweight)
    log.pi <- rep(0, K)
  probs <- t(exp(log.pi - t(distance) / 2))
  probs <- probs / apply(probs, 1, sum)
  m <- probs[, 1]
  if (K != 1)
    for (k in 2:K) {
      test <- (probs[, k] > m)
      class.pred[test] <- k
      m[test] <- probs[test, k]
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

#' nummax
#'
#'@description
#'
#'  ...
#'
#' @param X
#'
#' @return ...
#'

#' @keywords internal
#' @noRd
#'
"nummax" <- function(X) {
  ind <- rep(1, dim(X)[1])
  m <- X[, 1]
  if (dim(X)[2] > 1)
    for (i in 2:dim(X)[2]) {
      test <- X[, i] > m
      ind[test] <- i
      m[test] <- X[test, i]
    }
  list(ind = ind, max = m)
}
#' fclust.curvepred
#'
#'@description
#'
#'  ...
#'
#' @param data ...
#' @param tau ...
#' @param tau1 ...
#' @param q ...
#' @param KData ... 
#'
#' @return ...
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
  
  fit=data$cfit
  FullS=KData$FullS
  data=KData$CData
  J <- length(unique(data$measureID))
  #N indica il numero di soggetti studiati
  N <- length(unique(data$jamesID))
  #M è il vettore contente le misure, applicando l'unique
  M <- as.character(unique(data$measureID))
  
  
  
  tau2 <- tau / tau1
  sigma <- fit$par$sigma#Questo non va toccato
  Gamma <- fit$par$Gamma#vanno prese ncol pari al valore di q[j]
  Lambda <- fit$par$Lambda #Prendo le righe pari al valore di q[j]
  alpha <- fit$par$alpha
  lambda.zero <- as.vector(fit$par$lambda.zero)
  
  #Dopo aver estratto i parametri, li tiro fouri per ogni curva di appartenenza.
  a_old <- 1
  Final_result = list()
  #se J e q sono lunghi diversi tira errore con messaggio "numero di q diverso da numero di misure" in inglese
  if (J != length(q)) {
    stop("Number of q different from number of measures")
  }
  
  for (gei in 1:J) {
    a_new <- q[gei] + a_old - 1
    
    Gamma1 <- Gamma[(a_old:a_new), (a_old:a_new)]
    
    Lambda1 <- Lambda[a_old:a_new, ]
    if (is.null(dim(Lambda1)))
      Lambda1 <- matrix(Lambda1, ncol = 1, nrow =  q[gei])
    
    lambda.zero1 <- lambda.zero[a_old:a_new]
    
    
    a_old <- a_new + 1
    
    
    #Servirà fare un ciclo per ogni j
    #Costruiamo per j fissato
    #Costruisso S già unlistata
    FullSj <- FullS[[gei]]
    
    upci <- lowci <- uppi <- lowpi <- gpred <- matrix(0, N, nrow(FullSj))
    etapred <- matrix(0, N, ncol(FullSj))
    ind <- 1
    Lambda.alpha <- lambda.zero1 + Lambda1 %*% t(alpha)
    for (i in 1:N) {
      #y sono yij
      y <- data$value[data$jamesID == i & data$measureID == M[gei]]
      Sij <- FullSj[data$timeindex[data$jamesID == i &
                                     data$measureID == M[gei]], ]
      ni <- dim(Sij)[1]
      invvar <- diag(1 / rep(sigma, ni))
      covx <- Sij %*% Gamma1 %*% t(Sij) + solve(invvar)
      centx <- data$value[data$jamesID == i &
                            data$measureID == M[gei]] - Sij %*% Lambda.alpha
      d <- exp(-diag(t(centx) %*% solve(covx) %*% centx) / 2) * fit$par$pi
      if(sum(d)!=0){
        pi <- d / sum(d)}
      else {
        pi<-rep(0,length(d))
      }
      K <- length(pi)
      mu <- lambda.zero1 + Lambda1 %*% t(alpha * pi) %*% rep(1, K)
      cov <- (
        Gamma1 - Gamma1 %*% t(Sij) %*% solve(diag(ni) + Sij %*% Gamma1 %*%
                                               t(Sij) / sigma) %*% Sij %*% Gamma1 /
          sigma
      ) / sigma
      etapred[ind, ] <- mu + cov %*% t(Sij) %*% (y - Sij %*% mu)
      ord <- order(-pi)
      numb <- sum(cumsum(pi[ord]) <= tau1) + 1
      v <- diag(FullSj %*% (cov * sigma) %*% t(FullSj))
      pse <- sqrt(v + sigma)
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
      lowci[ind, ] <-  -nummax(-lower.c)$max
      uppi[ind, ] <- nummax(upper.p)$max
      lowpi[ind, ] <-  -nummax(-lower.p)$max
      gpred[ind, ] <- as.vector(FullSj %*% etapred[ind, ])
      ind <- ind + 1
    }
    meancurves <- FullSj %*% Lambda.alpha
    Final_result[[M[gei]]] = list(
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



#' fclust
#'
#'@description
#'
#'  ...
#'
#' @param data_input 
#' @param q 
#' @param h 
#' @param K 
#' @param pert 
#' @param pert1 
#' @param natural 
#' @param tol 
#' @param maxit 
#' @param hard 
#' @param pert2 
#' @param seed 

#'
#' @return ...
#'

#' @keywords internal
#' @noRd
#'
#' @import splines rlist ggplot2 RhpcBLASctl
#' @importFrom Matrix bdiag
#' @importFrom dplyr select filter group_by mutate arrange
#Qui inzia il ciclo finale
#initial sono i valori uscenti dall'inizializzazione
#TODO Probabilmente non serve più
"fclust" <- function(data_input,
                     q,
                     h,
                     K,
                     pert = 0.01,
                     pert1 = 0.01,
                     natural = TRUE,
                     tol = 0.001,
                     maxit = 20,
                     hard = FALSE,
                     pert2 = 0,
                     seed = NULL) {
  #limit solve() function
  #omp_set_num_threads(1)
  # initial <- intfclust(
  #   CData = data_input,
  #   q = q,
  #   h = h,
  #   pert = pert,
  #   K = K,
  #   natural = natural
  # )
  #Da qui in avanti passare come dataset intial$starter
  
  parameters <- initial$parameters
  vars <- initial$vars
  S <- initial$S
  FullS <- initial$FullS
  sigma.old <- 0
  sigma.new <- 1
  ind <- 1
  #Iterazioni massime del ciclo EM
  # Main loop. Iterates between M and E steps and stops when
  # sigma  has converged.
  while (abs(sigma.old - sigma.new) / sigma.new > tol &
         (ind <= maxit)) {
    parameters <- fclustMstep(
      parameters = parameters,
      curve_ok = initial$starter,
      S = S,
      vars = vars,
      hard = hard,
      p = 5,
      pert1 = pert1,
      tol = tol
    )
    
    vars <- fclustEstep(
      parameters = parameters,
      curve_ok = initial$starter,
      vars = vars,
      S,
      hard = hard
    )
    sigma.old <- sigma.new
    sigma.new <- parameters$sigma[1]
    ind <- ind + 1
  }
  #starter <- initial$starter #Richiamo i valori inziali, poiché mi servono i timeindex
  ###########starter$S<-S
  
  cfit <- fclustconst(parameters, vars, S)
  pred <- fclust_pred(
    fit = cfit,
    data = initial,
    reweight = FALSE,
    pert2 = pert2
  )
  
  return(
    list(
      cfit = cfit,
      pred = pred,
      FullS = FullS,
      S = S,
      grid = initial$grid,
      working_data = initial$starter
    )
  )
}