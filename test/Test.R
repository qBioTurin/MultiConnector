# Load necessary libraries
library(ggplot2)
library(dplyr)
library(microbenchmark)
library(parallel)
library(stats) 
library(plotly)
library(patchwork)
library(MetBrewer)
library(gghalves)
library(statmod)

# Source del codice originale
source("../R/ConnectorData.R")
source("../R/CONNData.R")
source("../R/PlotTimeSeries.R")
source("../R/plot.R")
source("../R/plotTimes.R")
source("../R/GridTimeOfPoints.R")
source("../R/truncatePlot.R")
source("../R/truncate.R")
source("../R/estimatepDimension.R")
source("../R/Clust.R")
source("../R/Clust.R")
source("../R/estimateCluster.R")
source("../R/IndexPlotExtrapolation.R")
source("../R/CONNECTORDataClustered.R")
source("../R/configSelection.R")
source("../R/IndexPlotExtrapolation2.R")
source("../R/DiscriminantPlot.R")
source("../R/SilhouetteAndEntropy.R")
source("../R/splinePlot.R")
source("../R/MaximumDiscriminationFunction.R")


# Funzione per generare curve
generate_curve <- function(n, curve_type, max_points = 30, translation = 5) {
  data <- tibble()
  for (i in 1:n) {
    num_points <- max(5, rbinom(1, max_points, 1/2))
    x <- sample(seq(-10, 10, 0.1), num_points)
    x <- sort(x)
    y <- switch(curve_type,
                "Parabola" = {
                  is_translated <- sample(c(TRUE, FALSE), 1)
                  if (i %% 2 == 1) {
                    if (is_translated) (x - translation)^2 else x^2
                  } else {
                    if (is_translated) -(x - translation)^2 else -x^2
                  }
                },
                "Hyperbola" = {
                  if (i %% 2 == 1) x^3 / 4 else -(x^3 / 4)
                },
                "Sine" = {
                  if (i %% 2 == 1) 150 * sin(x / 3) else -150 * sin(x / 3)
                },
                "Cosine" = {
                  if (i %% 2 == 1) 100 * cos(x / 3) else -100 * cos(x / 3)
                },
                "Logarithm" = { 
                  if (i %% 2 == 1) log(abs(x) + 1) * 50 else -log(abs(x) + 1) * 50
                },
                "Exponential" = { 
                  if (i %% 2 == 1) exp(x / 5) * 10 else -exp(x / 5) * 10
                },
                "Polynomial3" = { 
                  if (i %% 2 == 1) 0.1 * x^3 - x^2 + 2 * x else -(0.1 * x^3 - x^2 + 2 * x)
                },
                "Polynomial4" = { 
                  if (i %% 2 == 1) 0.05 * x^4 - 0.8 * x^2 + 5 * x else -(0.05 * x^4 - 0.8 * x^2 + 5 * x)
                }
    )
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
    
    # Check to make sure x and y have the same length before creating temp
    if (length(x) == length(y) && length(x) > 0) {
      sd_value <- if (curve_type %in% c("Parabola", "Hyperbola")) 20 else 0
      y <- y + rnorm(length(x), sd = sd_value)
      temp <- tibble(
        time = x,
        value = y,
        measureID = curve_type,
        subjID = factor(paste0(i))
      )
      data <- rbind(data, temp)
    } else {
      cat(paste("Skipping curve", i, "of type", curve_type, "due to inconsistent or empty data.\n"))
    }
  }
  return(data)
}

# Funzione per generare il file di annotazioni
generate_annotation_file <- function(curves_data) {
  unique_subjects <- unique(curves_data$subjID)
  annotations <- tibble(
    subjID = unique_subjects,
    gender = sample(c("M", "F"), length(unique_subjects), replace = TRUE),
    age = round(runif(length(unique_subjects), min = 20, max = 80)),
    treatment_group = sample(c("Control", "Treatment A", "Treatment B"), length(unique_subjects), replace = TRUE),
    baseline_weight = round(runif(length(unique_subjects), min = 50, max = 100), 1),
    height = round(runif(length(unique_subjects), min = 150, max = 200), 1),
    comorbidity = sample(c("None", "Diabetes", "Hypertension", "Obesity"), length(unique_subjects), replace = TRUE)
  )
  return(annotations)
}

# Funzione per eseguire il test e misurare il tempo
run_test <- function(num_curves, measures, p_values, G_values, max_points, translation_val) {
  times <- list()
  # Genera le curve per le misure specificate
  all_data <- tibble()
  for (measure in measures) {
    curves <- generate_curve(num_curves, measure, max_points = max_points, translation = translation_val)
    if (nrow(all_data) == 0) {
      all_data <- curves
    } else {
      all_data <- rbind(all_data, curves)
    }
  }
  
  if (nrow(all_data) == 0) {
    cat("Error: No data generated. Skipping this test configuration.\n")
    Sys.sleep(0.1)  # Small pause to ensure output is flushed
    return(times)
  }
  
  Annotations <- generate_annotation_file(all_data)
  
  # Misura il tempo per ConnectorData (era DataImport)
  times[["ConnectorData"]] <- system.time({
    Data <- ConnectorData(all_data, Annotations)
  })[["elapsed"]]
  
  Sys.sleep(0.1)  # Small pause between operations
  
  # Misura il tempo per plot (era PlotTimeSeries)
  times[["plot"]] <- system.time({
    plot(Data, feature = "treatment_group")
  })[["elapsed"]]
  
  Sys.sleep(0.1)
  
  # Misura il tempo per plotTimes (era DataVisualization normale)
  times[["plotTimes_normal"]] <- system.time({
    plotTimes(Data)
  })[["elapsed"]]
  
  Sys.sleep(0.1)
  
  # Misura il tempo per plotTimes (era DataVisualization large)
  times[["plotTimes_large"]] <- system.time({
    plotTimes(Data, large = TRUE)
  })[["elapsed"]]
  
  Sys.sleep(0.1)
  
  # Misura il tempo per truncatePlot (era PlotDataTruncation)
  if (length(measures) > 0) {
    times[["truncatePlot"]] <- system.time({
      truncatePlot(Data, measure = measures[1], truncTime = 5)
    })[["elapsed"]]
    
    Sys.sleep(0.1)
    
    # Misura il tempo per truncate (era DataTruncation)
    times[["truncate"]] <- system.time({
      truncate(Data, measure = measures[1], truncTime = 5)
    })[["elapsed"]]
  }
  
  Sys.sleep(0.1)
  
  # Misura il tempo per estimatepDimension (era BasisDimensionChoice)
  times[["estimatepDimension"]] <- system.time({
    CrossLogLikePlot <- estimatepDimension(Data, p = p_values, cores = min(10, detectCores()))
  })[["elapsed"]]
  
  Sys.sleep(0.1)
  
  # Crea un vettore di p per ogni misura
  p_vector <- setNames(rep(p_values[length(p_values) %/% 2], length(measures)), measures)
  
  # Misura il tempo per estimateCluster (era ClusterAnalysis)
  times[["estimateCluster"]] <- system.time({
    clusters <- estimateCluster(Data, G = G_values, p = p_vector, runs = 100, cores = min(10, detectCores()))
  })[["elapsed"]]
  
 
  
  Sys.sleep(0.1)
  
  for (G_val in G_values) {
    # Misura il tempo per configSelection (era ConfigSelection)
    times[[paste0("configSelection_G", G_val)]] <- system.time({
      ConfigChosen <- configSelection(clusters, G = G_val, "MinfDB")
    })[["elapsed"]]
    
    Sys.sleep(0.1)
    
    # Misura il tempo per plot con ConfigChosen (era IndexPlotExtrapolation2)
    times[[paste0("plot_ConfigChosen_G", G_val)]] <- system.time({
      plot(Data, ConfigChosen = ConfigChosen, feature = "comorbidity")
    })[["elapsed"]]
    
    Sys.sleep(0.1)
    
    # Misura il tempo per DiscriminantPlot
    times[[paste0("DiscriminantPlot_G", G_val)]] <- system.time({
      DiscriminantPlot(Data, ConfigChosen = ConfigChosen, feature = "gender")
    })[["elapsed"]]
    
    Sys.sleep(0.1)
    
    # Misura il tempo per SilEntropy (era SilhouetteAndEntropy)
    times[[paste0("SilEntropy_G", G_val)]] <- system.time({
      SilEntropy(ConfigChosen)
    })[["elapsed"]]
    
    Sys.sleep(0.1)
    
    # Misura il tempo per splinePlot
    times[[paste0("splinePlot_G", G_val)]] <- system.time({
      splinePlot(ConfigChosen = ConfigChosen)
    })[["elapsed"]]
    
    Sys.sleep(0.1)
    
    # Misura il tempo per MaximumDiscriminationFunction
    times[[paste0("MaximumDiscriminationFunction_G", G_val)]] <- system.time({
      MaximumDiscriminationFunction(ConfigChosen = ConfigChosen)
    })[["elapsed"]]
    
    Sys.sleep(0.1)
  }
  
  return(times)
}

# Parametri di test
num_curves_vals <- c(10, 30, 50)  # Vari numeri di curve
measures_sets <- list(
  c("Hyperbola"),
  c("Parabola", "Hyperbola"),
  c("Parabola", "Sine"),
  c("Parabola", "Hyperbola", "Sine", "Cosine"),
  c("Parabola", "Hyperbola", "Sine", "Cosine", "Logarithm")
)
p_values <- 5:8  # Valori di p da testare
G_values <- 2:5  # Valori di G da testare
max_points_vals <- c(10, 30, 50)  # Numero massimo di punti per curva
translation_vals <- c(5)  # Valori di translation per l'irregolarità delle curve

# Crea un file per salvare i risultati
output_file <- "testing_results2.txt"
file.create(output_file)
cat("Starting performance tests...\n")
Sys.sleep(0.1)


max_points_vals <- c(30, 50, 80)
translation_vals <- c(1, 5 , 10)
num_curves_vals <- c(10, 30, 50)

# Esegui i test in modo sistematico
for (num_curves in num_curves_vals) {
  for (measures_idx in seq_along(measures_sets)) {
    measures <- measures_sets[[measures_idx]]
    for (max_points in max_points_vals) {
      for (translation_val in translation_vals) {
        test_config <- paste("Test configuration:",
                             "Curves:", num_curves,
                             "Measures:", paste(measures, collapse = ", "),
                             "Max Points:", max_points,
                             "Translation:", translation_val)
        
        cat("\n", test_config, "\n", sep = "")
        Sys.sleep(0.1)
        
        # Scrivi la configurazione nel file
        write(paste("\n", test_config, "\n", sep = ""), file = output_file, append = TRUE)
        write(paste("Date and time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"), file = output_file, append = TRUE)
        
        # Esegui il test
        tryCatch({
          times_results <- run_test(num_curves, measures, p_values, G_values, max_points, translation_val)
          
          # Scrivi i risultati nel file
          write("Timing results:", file = output_file, append = TRUE)
          for (name in names(times_results)) {
            write(sprintf("%-40s: %10.3f s", name, times_results[[name]]), file = output_file, append = TRUE)
          }
          
          Sys.sleep(0.1)
          
        }, error = function(e) {
          write(paste("ERROR during test:", conditionMessage(e)), file = output_file, append = TRUE)
          cat("ERROR:", conditionMessage(e), "\n")
          Sys.sleep(0.1)
        })
        
        write("\n-------------------------------------------------\n", file = output_file, append = TRUE)
        
        # Pulisci la memoria dopo ogni test
        gc()
        Sys.sleep(0.2)  # Slightly longer pause after cleanup
      }
    }
  }
}

cat("\nTest completati. I risultati sono stati salvati in", output_file, "\n")
Sys.sleep(0.1)