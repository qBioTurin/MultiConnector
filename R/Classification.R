
source("Clust.R")
source("ClusterAnalysis.R")
# parallel, ggplot2, dplyr


ClassificationCurves <- function(newdata,
                                 KData,
                                 ConfigChosen,
                                 Cores =1,
                                 entropyCutoff =1,probCutoff = 0.6){

  CData = newdata@curves
  CData$jamesID <- as.integer(factor(CData$subjID, levels = unique(CData$subjID)))
  M <- sort(unique(CData$measureID))

  ######
  # qua è da controllare che abbia le stesse misure dei clustered data
  ######

  nworkers <- detectCores()
  if(nworkers<Cores) Cores <- nworkers

  parameters <- ConfigChosen$CfitandParameters$cfit$parameters


  ### delete points that are outside of the old grid ###
  grid <- KData@TimeGrids
  #
  # for(i in names(grid)){print(i)
  #   gridM = grid[[i]]
  #   DataTruncation(newdata, measure = i, feature = "gender", truncTime = c(min(gridM),max(gridM)))
  # }

  ### Lets obtain the new S of the new curves exploiting the S from FCM
  ## The old S is not the FUllS stored in clusterdata since this one is the U from the svd(old S)

  Nclusters =  length(ConfigChosen$cluster.names)

  KData@CData$cluster = ConfigChosen$CfitandParameters$pred$class.pred[KData@CData$jamesID]

  FullS = KData@FullS
  sigma <- parameters$sigma
  J <- length(unique(KData@CData$measureID))

  Gamma <- parameters$Gamma
  Lambda <- parameters$Lambda
  alpha <- parameters$alpha
  lambda.zero <- as.vector(parameters$lambda.zero)
  Lambda.alpha <- lambda.zero + Lambda %*% t(alpha)

  # Lets calculate the new S of the new curves

  newGrid <- newdata@TimeGrids

  Snew = lapply(1:J, function(j) {
    FullSm <- FullS[[j]]
    Gridm = grid[[j]]
    NewGridm = newGrid[[j]]
    pm = dim(FullSm)[2]

    Snew = matrix(1,ncol = pm , nrow = length(NewGridm))
    Snew[,1:pm] = sapply(1:pm,function(i) stats::spline(x = Gridm, y = FullSm[,i], xout = NewGridm )$y)
    Snew
  })

  ###
  IDcurves = unique(CData$subjID)

  #cl <- makeCluster(Cores)
  #clusterCall(cl, function(){
  #   library(dplyr)
  #   library(ggplot2)
  #   library(mvtnorm)
  #   library(tidyr)
  # })
  #clusterExport(cl,list("CData","Lambda.alpha","Snew","sigma",
  #                      "Gamma","ClassificationSingleCurve","Nclusters"),envir = environment() )

  ALL.runs<-lapply(IDcurves, function(x_id){
    #parLapply(cl,IDcurves, function(x_id){
    tryCatch({
      do.call(rbind,
              lapply(1:J,function(j) {
                CData_x = CData %>% filter(jamesID == x_id,measureID == M[j])
                CData_x$timeindex<- match(CData_x$time, newGrid[[j]])
                CData_x
              })
      ) -> CData_i

      ClassificationSingleCurve(KData,
                                CData_i,
                                Snew,
                                Gamma = Gamma,
                                sigma = sigma,
                                Lambda.alpha = Lambda.alpha,
                                Nclusters =Nclusters,
                                ConfigChosen=ConfigChosen)
    },
    error=function(e) {
      err<-paste("ERROR:",conditionMessage(e), "\n")
      err.list<-list(Error= err)
      #print(err)
      return(err.list)
    })
  })

  #stopCluster(cl)

  names(ALL.runs) = paste0("ID_",IDcurves)
  df = as.data.frame(t(sapply(ALL.runs,"[[",3)),row.names = F)
  df$ID = IDcurves
  df = df %>% relocate(ID)

  # Entropy calculation

  df_Entrop = df %>%
    tidyr::gather(-ID,key = "Cluster",value = "Prob") %>%
    group_by(ID) %>%
    mutate(Entropy = -sum(Prob*log2(Prob)),
           MajorProb = max(Prob) )%>%
    mutate(ClusterOld = Cluster,
           Cluster = ifelse( !is.na(Entropy) & (Entropy<entropyCutoff | MajorProb>probCutoff) , Cluster[which(Prob == MajorProb)], "Unclassified") ) %>%
    ungroup() %>%
    tidyr::spread(key = "ClusterOld",value = "Prob")

  return(list(ClassMatrix = df, ClassMatrix_entropy = df_Entrop, ListClassID =  ALL.runs ) )
}

ClassificationSingleCurve = function(clusterdata, CData_i, Snew, Gamma, sigma, Lambda.alpha, Nclusters, ConfigChosen){
  M <- sort(unique(CData_i$measureID))
  J = length(M)
  pi = ConfigChosen$CfitandParameters$cfit$parameters$pi
  for (j in 1:J) {
    A <- Snew[[j]][CData_i$timeindex[(CData_i$measureID == M[j])], ]
    if (j == 1) {
      Si <- A
    }
    else{
      Si <- bdiag(Si, A)
    }
  }
  Si <- as.matrix(Si)

  ##
  Pcl = lapply(1:Nclusters, function(i,Snew,sigma, Gamma, Lambda.alpha,CData_i,params_pi){

    MinL = CData_i %>% group_by(curvesID) %>% count() %>% ungroup() %>% summarise(m= min(n)) %>% pull(m)

    if(MinL < 2)
      return(data.frame(log_pi = 0, pi = 0, cluster = i) )

    ## defining the new spline basis matrix S

    x = CData_i$value
    n = length(x)
    Sigma <- sigma * diag(n) + Snew %*% Gamma %*% t(Snew)

    mu_i =  Snew %*% Lambda.alpha[,i] #(par$lambda.zero + par$Lambda %*% alphai )

    pi = mvtnorm::dmvnorm(x = x,
                          mean = mu_i,
                          sigma = Sigma )

    log_pi = mvtnorm::dmvnorm(x = x,
                              mean = mu_i,
                              sigma = Sigma, log = T ) + log(params_pi[i])

    return(data.frame(log_pi = log_pi, pi = pi, cluster = i) )
  }, Snew = Si, sigma =sigma, Gamma = Gamma, Lambda.alpha = Lambda.alpha,CData_i=CData_i, params_pi = pi)

  Pcl =  do.call(rbind,Pcl)

  ## calculate the probs to belong in the clusters

  Pcl$class = Pcl$pi * pi / sum(Pcl$pi * pi)

  CData_i$cluster = Pcl$cluster[which.max(Pcl$log_pi)]
  Pclass = Pcl$class
  names(Pclass) = Pcl$cluster

  ### Plotting with the new

  resClust = ConfigChosen$CfitandParameters$pred$class.pred
  df = clusterdata@CData

  # Get number of features per measure
  q <- sapply(clusterdata@FullS, function(x)
    dim(x)[2])

  # Merge data
  df$cluster = resClust[df$jamesID]

  TimeGrids =  clusterdata@TimeGrids

  # Compute curve predictions
  curvepred = fclust.curvepred(
    ConfigChosen$CfitandParameters,
    clusterdata,
    tau = 0.95,
    tau1 = 0.975,
    q = q
  )

  MeanC = do.call(rbind, lapply(names(curvepred), function(x) {
    as.data.frame(curvepred[[x]]$meancurves) -> Mean
    G = dim(Mean)[2]
    # Ensure column names match number of clusters
    colnames(Mean) = as.character(1:G)

    Mean$measureID = x
    Mean$time = TimeGrids[[x]]
    return(Mean)
  })) %>%
    tidyr::gather(-time, -measureID, value = "value", key = "cluster")

  #### plot generation

  ProbAnnot = data.frame(cluster = Pcl$cluster,
                         Prob = round(Pcl$class,digits = 2),
                         x = rep(-Inf,Nclusters),
                         y = rep(Inf,Nclusters))

  pl = ggplot()+
    geom_line(data = MeanC,aes(x = time, y = value,linetype=cluster)) +
    geom_line(data = df, aes(x = time, y = value, group = subjID), col = "grey",alpha = .4) +
    facet_grid(measureID~cluster)+
    geom_line(data = CData_i,aes(x = time, y = value),col = "red") +
    geom_text(data = ProbAnnot, aes(x = x, y = y, label = paste0("Prob: ",Prob)),hjust = 0, vjust = 1)

  return(list(plot = pl, weight = Pcl, prob = Pclass))

}

