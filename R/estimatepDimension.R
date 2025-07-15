#' @title estimatepDimension
#' @description Generates a line plot reporting the cross-validated loglikelihood value for each number of knots. In details, for each number of knots 10% of the curves from the whole data are removed and treated as a test set, then the remaing curves are fitted using the FCM and the loglikelihood on the test set is calculated. The process is then repeated nine more times.
#' @param data CONNECTORData. See CONNECTORData for details.
#' @param i an integer indicating the index of the observation to be used for the analysis. For example, if i=1 the first observation of the data set will be used for the analysis. If i=c(1,2) the first and the second observation of the data set will be used for the analysis. Column ID and time are not considered when indicate the index of the observation. Default value is 1.
#' @param p The vector of the dimensions of the natural cubic spline basis
#' @param cores The number of Cores to be used for parallel computation. Maximum number is 10.
#' @return Returns a list containing for each element a line plot of the cross-validated loglikelihood for each value of p, in grey the result of all ten repetitions of the likelihood calculation and in black the mean of them.
#' @import MASS dplyr parallel ggplot2 splines patchwork rlist
#' @export
setGeneric("estimatepDimension", function(data,
                                            i = NULL,
                                            p,
                                            cores = 1)
  standardGeneric("estimatepDimension"))
#' @rdname estimatepDimension
#' @export
#'

#'
#'
setMethod("estimatepDimension", signature = c("CONNECTORData"),
          function(data,
                   i = NULL,
                   p,
                   cores = 1) {

            res <- list()
            measures <- unique(data@curves$measureID)
            
            if (length(measures) == 1) {
              return(estimatepDimensionPerObs(data, p, cores))
            }
            else if (is.null(i)) {
              res <- lapply(measures, function(j) {
                curve <- filter(data@curves, measureID == j)
                invisible(capture.output(connect <- ConnectorData(curve, data@annotations)))
                result <- estimatepDimensionPerObs(connect, p, cores)
                return(result)
              })
              names(res) <- measures
            }
            else if (i %in% measures) {
              res <- lapply(i, function(j) {
                curve <- filter(data@curves, measureID == j)
                invisible(capture.output(connect <- ConnectorData(curve, data@annotations)))
                result <- estimatepDimensionPerObs(connect, p, cores)
                return(result)
              })
              names(res) <- i
              
            }
            else{
              stop("measures given is not present in the CONNETCTORData object")
            }
            return(res)
          })

setGeneric("estimatepDimensionPerObs", function(data, p, cores)
  standardGeneric("estimatepDimensionPerObs"))

setMethod("estimatepDimensionPerObs", signature = c("CONNECTORData"),
          function(data, p, cores) {
            
            if(length(unique(data@dimensions$curvesID))>9){
              splits=10
            }
            else{splits=length(unique(data@dimensions$curvesID))}
            
            curve<-sample(data@dimensions$curvesID)
            Allsplits <-
              split(curve, cut(seq_along(data@dimensions$curvesID), breaks = splits))
            
            grid <- data@TimeGrids[[1]]
            Lgrid <- length(data@TimeGrids[[1]])
            #pmax <- min(data@dimensions$nTimePoints)
            if (max(p) > Lgrid)
            {
              p <- min(p):Lgrid
              warning(
                paste(
                  "The maximum value of p should not be larger than the length of the grid. Thus the range of p is fixed from",
                  min(p),
                  "to",
                  Lgrid,
                  ".\n"
                )
              )
            }
            if(cores==1){
              crossvalid<-lapply(1:splits, function(step) {
                tryCatch({
                  omp_set_num_threads(1)
                  SampleTestSet <- sort(Allsplits[[step]])
                  SampleTrainSet <- unname(unlist(Allsplits[-step]))
                  TestSet <-
                    data@curves[data@curves$curvesID %in% SampleTestSet, ]
                  TrainingSet <-
                    data@curves[data@curves$curvesID %in% SampleTrainSet, ]
                  #trasforma la colonna ID di TrainingSet in valori numerici
                  TrainingSet$IDnum <-
                    as.numeric(as.factor(TrainingSet$curvesID))
                  TestSet$IDnum <-
                    as.numeric(as.factor(TestSet$curvesID)) #davedere
                  #create data.funcit a tibble composed by TrainingTestSet plus a new column with the value of data@dimensions[2] matching the ID
                  
                  
                  # data.funcit <- TrainingSet #%>%
                  #mutate(timepos = match(TrainingSet$time, grid)) #da vedere
                  
                  Crosslikelihood <-
                    sapply(p, function(p_value){
                      
                      Calclikelihood(
                        p = p_value,
                        data.funcit = TrainingSet,
                        TestSet = TestSet
                      )}
                    )
                  
                  return(tibble(
                    p = p,
                    Crosslikelihood = Crosslikelihood,
                    fold = step
                  ))
                }, error = function(e) {
                  err <-
                    paste("Error in prediction:",
                          conditionMessage(e),
                          TrainingSet$time,
                          grid,
                          "\n")
                  return(err)
                })
              })
            }
            else{
            nworkers <- detectCores()-1
            if (nworkers < cores)
              cores <- nworkers
            if (cores > 10)
              cores <- 10
            variables_to_export <-
              c(
                "data",
                "p",
                "Calclikelihood",
                "fclustMstep",
                "fclustEstep",
                "fclustconst",
                "Likelihood",
                "Allsplits",
                "fclust",
                "fclust_pred",
                "fclustconst",
                "intfclust",
                "presetKmeans",
                "justKmeans"
              )
            cl <- makeCluster(cores)
            clusterExport(cl = cl,
                          varlist = variables_to_export,
                          envir = environment())
            
            
            clusterEvalQ(cl, {
              library(dplyr)
              library(MASS)
              library(splines)
              library(parallel)
              #library(ConnectorV2.0)
              library(Matrix)
              library(rlist)
              library(RhpcBLASctl)
              

            })
            crossvalid <- parLapply(cl, 1:splits, function(step) {
              tryCatch({
                omp_set_num_threads(1)
                SampleTestSet <- sort(Allsplits[[step]])
                SampleTrainSet <- unname(unlist(Allsplits[-step]))
                TestSet <-
                  data@curves[data@curves$curvesID %in% SampleTestSet, ]
                TrainingSet <-
                  data@curves[data@curves$curvesID %in% SampleTrainSet, ]
                #trasforma la colonna ID di TrainingSet in valori numerici
                TrainingSet$IDnum <-
                  as.numeric(as.factor(TrainingSet$curvesID))
                TestSet$IDnum <-
                  as.numeric(as.factor(TestSet$curvesID)) #davedere
                #create data.funcit a tibble composed by TrainingTestSet plus a new column with the value of data@dimensions[2] matching the ID
                
                
                # data.funcit <- TrainingSet #%>%
                #mutate(timepos = match(TrainingSet$time, grid)) #da vedere
                
                Crosslikelihood <-
                  sapply(p, function(p_value){
                  
                    Calclikelihood(
                      p = p_value,
                      data.funcit = TrainingSet,
                      TestSet = TestSet
                    )}
                  )
                
                return(tibble(
                  p = p,
                  Crosslikelihood = Crosslikelihood,
                  fold = step
                ))
              }, error = function(e) {
                err <-
                  paste("Error in prediction:",
                        conditionMessage(e),
                        TrainingSet$time,
                        grid,
                        "\n")
                return(err)
              })
            })
            stopCluster(cl)}
            crossvalid <- crossvalid[!sapply(crossvalid, is.null)]
            Knots.list <- lapply(p, function(p) {
              Spline <- ns(grid, df = (p - 1))
              df <-
                tibble(Knots = c(attr(Spline, "Boundary.knots"), attr(Spline, "knots")),
                       Name = c(rep("Boundary knots", 2), rep("Knots", length(
                         attr(Spline, "knots")
                       ))))
              df$p = paste("p =", p)
              return(df)
            })
            Knots.df <- do.call("rbind", Knots.list)
            Knots.df$p.num <-
              as.numeric(sub("p = ", "", Knots.df$p))
            Knots.Plot <- ggplot(Knots.df) +
              scale_x_continuous(breaks = as.integer(seq(
                min(grid),
                max(grid),
                length.out = max(2, length(grid) /
                                   5)
              ))) +
              geom_hline(aes(yintercept = p.num),
                         linetype = "dashed",
                         color = "grey") +
              geom_point(aes(x = Knots, y = p.num, col = Name), shape = 3) +
              scale_color_manual(values = c(`Boundary knots` = "Orange", Knots = "blue")) +
              geom_boxplot(
                data = data.frame(y = max(Knots.df$p.num) + 1 ,  x = grid),
                aes(x, y),
                width = 0.4,
                col = "black"
              ) +
              scale_y_continuous(
                breaks = c(unique(Knots.df$p.num), max(Knots.df$p.num) + 1),
                labels = c(paste("p = ", unique(Knots.df$p.num)),
                           "Time points \n distribution")
              ) +
              theme(
                axis.text = element_text(size = 15, hjust = 0.5),
                axis.text.x = element_text(angle = +90),
                axis.title = element_text(size = 18, face = "bold"),
                axis.line = element_line(colour = "black"),
                plot.title = element_text(
                  size = 20,
                  face = "bold",
                  vjust = 1,
                  lineheight = 0.6
                ),
                legend.text = element_text(size = 14),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.key = element_blank(),
                legend.key.size = unit(.9, "cm"),
                legend.key.width = unit(.9, "cm"),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                plot.margin = unit(c(10, 5, 5, 5), "mm"),
                strip.background = element_blank(),
                strip.text.x = element_blank()
              ) +
              labs(x = "Time", y = "")
            
            
            GrowthCurve <-
              ggplot(data = data@curves, aes(
                x = time,
                y = data@curves$value,
                group = curvesID
              )) +
              scale_x_continuous(breaks = as.integer(seq(
                min(grid), max(grid), length.out = max(2, length(grid) / 5)
              ))) +
              geom_line() +
              geom_point() +
              labs(x = "", y = "") +
              theme(plot.title = element_text(hjust = 0.5),
                    title = element_text(size = 10, face = 'bold'))
            allCrossvalid <- do.call("bind_rows", crossvalid)
            meanCrossvalid <-
              sapply(p, function(x) {
                mean(allCrossvalid$Crosslikelihood[allCrossvalid$p == x])
              })
            meanCrossvalidData <-
              tibble(p = p, mean = meanCrossvalid)
            measure_id <- data@curves$measureID[[1]]
            title_text <- paste("Cross-LogLikelihood Plot of", measure_id)
            ValidationPlot <- ggplot() +
              geom_line(
                data = meanCrossvalidData,
                aes(
                  x = p,
                  y = mean,
                  linetype = "mean",
                  col = "mean"
                ),
                linewidth = 1.2
              ) +
              geom_line(
                data = allCrossvalid,
                aes(
                  x = p,
                  y = Crosslikelihood,
                  group = fold,
                  linetype = "test",
                  col = "test"
                ),
                linewidth = 1.1
              ) +
              geom_point(data = meanCrossvalidData, aes(x = p, y = mean), size =
                           2) +
              labs(title = title_text , y = "LogLikelihood ", x = "Natural Spline Dimension (p)") +
              scale_color_manual("",
                                 breaks = c("mean", "test"),
                                 values = c("black", "grey")) +
              scale_linetype_manual("",
                                    breaks = c("mean", "test"),
                                    values = c("solid", "dashed")) +
              theme(legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5))
            return((GrowthCurve / Knots.Plot) | ValidationPlot)
          })


setGeneric("Calclikelihood", function(p, data.funcit, TestSet)#, grid)
  standardGeneric("Calclikelihood"))

setMethod("Calclikelihood", signature = c(),
          function(p, data.funcit, TestSet){#, grid) {
            perc <- max(TestSet$IDnum) #TODO
            # points <- data.funcit$value
            # ID <- data.funcit$curvesID
            # timeindex = data.funcit$timepos
            KmData <-
              presetKmeans(
                CData = data.funcit,
                q = p
                # h = 1,
                # K = 1
              )
            runs<-justKmeans(CLUSTData = KmData, K = 1)
            fcm.fit<-intfclust(
              q = p,
              h = 1,
              K = 1,
              class = runs,
              CLUSTData = KmData
            )
            
            #Li <- sapply(1:perc, Likelihood, TestSet, fcm.fit)
            Li <-
              sapply(1:perc, function(per)
                Likelihood(per, TestSet, fcm.fit, p, KmData))
            
            Likelihood <- sum(Li)
            return(Likelihood)
            
          })

setGeneric("Likelihood", function(x, TestSet, fcm.fit, p, KmData)#, grid)
  standardGeneric("Likelihood"))

setMethod("Likelihood", signature = c(),
          function(x, TestSet, fcm.fit, p, KmData){#, grid) {
            
            Gamma <- fcm.fit$cfit$parameters$Gamma
            sigma <- fcm.fit$cfit$parameters$sigma
            Lambda <- fcm.fit$cfit$parameters$Lambda
            alpha <- fcm.fit$cfit$parameters$alpha
            mu <-
              fcm.fit$cfit$parameters$lambda.zero + Lambda * c(alpha)
            
            base <- KmData$FullS[[1]]
            data.temp <- TestSet[TestSet$IDnum == x, ]
            data.temp <- arrange(data.temp, time)
            time.temp <- data.temp$time
            #####
            newGrid = time.temp
            Snew = matrix(1,ncol = p , nrow = length(newGrid))
            Snew[,1:p] = sapply(1:p,function(i) stats::spline(x = KmData$TimeGrids[[1]], y = KmData$FullS[[1]][,i], xout = newGrid )$y)
            #####            
            base.i <- Snew
            Yi <- data.temp$value
            n.i <- length(time.temp)
            Vi <-
              base.i %*% Gamma %*% t(base.i) + sigma ^ 2 * diag(n.i)
            mui <- base.i %*% mu
            invVi <- ginv(Vi)
            result <-
              -1 / 2 * c(determinant(Vi)$modulus) - n.i / 2 * log(2 * pi) - 1 / 2 *
              t(Yi - mui) %*% invVi %*% (Yi - mui)
            
            return(result)
          })
