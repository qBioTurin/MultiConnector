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
library(splines)
library(rlist)
library(RhpcBLASctl)
library(RColorBrewer)
library(Matrix)

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

# Funzione per eseguire il test e scrivere direttamente i risultati
run_test <- function(num_curves, measures, p_values, G_values, max_points, translation_val, output_file) {
  set.seed(2404)
  cat("Generating curves...\n")
  flush.console()
  
  # Genera le curve
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
    write("ERROR: No data generated", file = output_file, append = TRUE)
    cat("ERROR: No data generated. Skipping this test configuration.\n")
    flush.console()
    return(FALSE)
  }
  
  cat("Data generated successfully:", nrow(all_data), "rows\n")
  flush.console()
  
  Annotations <- generate_annotation_file(all_data)
  failed_ops <- c()
  
  # Scrivi l'intestazione dei risultati
  write("Timing results:", file = output_file, append = TRUE)
  
  # ConnectorData
  cat("Running ConnectorData...\n")
  flush.console()
  Data <- NULL
  tryCatch({
    elapsed_time <- system.time({
      Data <- ConnectorData(all_data, Annotations)
    })[["elapsed"]]
    write(sprintf("%-40s: %10.3f s", "ConnectorData", elapsed_time), file = output_file, append = TRUE)
  }, error = function(e) {
    write(sprintf("%-40s: FAILED - %s", "ConnectorData", conditionMessage(e)), file = output_file, append = TRUE)
    failed_ops <<- c(failed_ops, "ConnectorData")
  })
  Sys.sleep(0.1)
  
  if (is.null(Data)) {
    write("Cannot continue without Data object", file = output_file, append = TRUE)
    return(FALSE)
  }
  
  # plot
  cat("Running plot...\n")
  flush.console()
  tryCatch({
    elapsed_time <- system.time({
      plot(Data, feature = "treatment_group")
    })[["elapsed"]]
    write(sprintf("%-40s: %10.3f s", "plot", elapsed_time), file = output_file, append = TRUE)
  }, error = function(e) {
    write(sprintf("%-40s: FAILED - %s", "plot", conditionMessage(e)), file = output_file, append = TRUE)
    failed_ops <<- c(failed_ops, "plot")
  })
  Sys.sleep(0.1)
  
  # plotTimes normal
  cat("Running plotTimes_normal...\n")
  flush.console()
  tryCatch({
    elapsed_time <- system.time({
      plotTimes(Data)
    })[["elapsed"]]
    write(sprintf("%-40s: %10.3f s", "plotTimes_normal", elapsed_time), file = output_file, append = TRUE)
  }, error = function(e) {
    write(sprintf("%-40s: FAILED - %s", "plotTimes_normal", conditionMessage(e)), file = output_file, append = TRUE)
    failed_ops <<- c(failed_ops, "plotTimes_normal")
  })
  Sys.sleep(0.1)
  
  # plotTimes large
  cat("Running plotTimes_large...\n")
  flush.console()
  tryCatch({
    elapsed_time <- system.time({
      plotTimes(Data, large = TRUE)
    })[["elapsed"]]
    write(sprintf("%-40s: %10.3f s", "plotTimes_large", elapsed_time), file = output_file, append = TRUE)
  }, error = function(e) {
    write(sprintf("%-40s: FAILED - %s", "plotTimes_large", conditionMessage(e)), file = output_file, append = TRUE)
    failed_ops <<- c(failed_ops, "plotTimes_large")
  })
  Sys.sleep(0.1)
  
  # truncatePlot e truncate
  if (length(measures) > 0) {
    cat("Running truncatePlot...\n")
    flush.console()
    tryCatch({
      elapsed_time <- system.time({
        truncatePlot(Data, measure = measures[1], truncTime = 5)
      })[["elapsed"]]
      write(sprintf("%-40s: %10.3f s", "truncatePlot", elapsed_time), file = output_file, append = TRUE)
    }, error = function(e) {
      write(sprintf("%-40s: FAILED - %s", "truncatePlot", conditionMessage(e)), file = output_file, append = TRUE)
      failed_ops <<- c(failed_ops, "truncatePlot")
    })
    Sys.sleep(0.1)
    
    cat("Running truncate...\n")
    flush.console()
    tryCatch({
      elapsed_time <- system.time({
        truncate(Data, measure = measures[1], truncTime = 5)
      })[["elapsed"]]
      write(sprintf("%-40s: %10.3f s", "truncate", elapsed_time), file = output_file, append = TRUE)
    }, error = function(e) {
      write(sprintf("%-40s: FAILED - %s", "truncate", conditionMessage(e)), file = output_file, append = TRUE)
      failed_ops <<- c(failed_ops, "truncate")
    })
    Sys.sleep(0.1)
  }
  
  # estimatepDimension
  cat("Running estimatepDimension...\n")
  flush.console()
  CrossLogLikePlot <- NULL
  tryCatch({
    elapsed_time <- system.time({
      CrossLogLikePlot <- estimatepDimension(Data, p = p_values, cores = min(10, detectCores()))
    })[["elapsed"]]
    write(sprintf("%-40s: %10.3f s", "estimatepDimension", elapsed_time), file = output_file, append = TRUE)
  }, error = function(e) {
    write(sprintf("%-40s: FAILED - %s", "estimatepDimension", conditionMessage(e)), file = output_file, append = TRUE)
    failed_ops <<- c(failed_ops, "estimatepDimension")
  })
  Sys.sleep(0.1)
  
  # estimateCluster
  p_vector <- setNames(rep(p_values[length(p_values) %/% 2], length(measures)), measures)
  cat("Running estimateCluster...\n")
  flush.console()
  clusters <- NULL
  tryCatch({
    elapsed_time <- system.time({
      clusters <- estimateCluster(Data, G = G_values, p = p_vector, runs = 100, cores = min(10, detectCores()))
    })[["elapsed"]]
    write(sprintf("%-40s: %10.3f s", "estimateCluster", elapsed_time), file = output_file, append = TRUE)
  }, error = function(e) {
    write(sprintf("%-40s: FAILED - %s", "estimateCluster", conditionMessage(e)), file = output_file, append = TRUE)
    failed_ops <<- c(failed_ops, "estimateCluster")
  })
  Sys.sleep(0.1)
  
  if (is.null(clusters)) {
    write("Cannot continue without clusters object", file = output_file, append = TRUE)
    rm(Data, Annotations, all_data, CrossLogLikePlot)
    invisible(gc(verbose = FALSE, full = TRUE))
    return(FALSE)
  }
  
  # Loop per ogni G
  for (G_val in G_values) {
    cat(paste0("Running selectCluster G=", G_val, "...\n"))
    flush.console()
    CONNECTORDataClustered <- NULL
    tryCatch({
      elapsed_time <- system.time({
        CONNECTORDataClustered <- selectCluster(clusters, G = G_val, "MinfDB")
      })[["elapsed"]]
      write(sprintf("%-40s: %10.3f s", paste0("configSelection_G", G_val), elapsed_time), file = output_file, append = TRUE)
    }, error = function(e) {
      write(sprintf("%-40s: FAILED - %s", paste0("configSelection_G", G_val), conditionMessage(e)), file = output_file, append = TRUE)
      failed_ops <<- c(failed_ops, paste0("configSelection_G", G_val))
    })
    Sys.sleep(0.1)
    
    if (is.null(CONNECTORDataClustered)) {
      write(sprintf("Skipping remaining tests for G=%d due to configSelection failure", G_val), file = output_file, append = TRUE)
      next
    }
    
    cat(paste0("Running plot_CONNECTORDataClustered G=", G_val, "...\n"))
    flush.console()
    tryCatch({
      elapsed_time <- system.time({
        plot(Data, CONNECTORDataClustered = CONNECTORDataClustered, feature = "comorbidity")
      })[["elapsed"]]
      write(sprintf("%-40s: %10.3f s", paste0("plot_CONNECTORDataClustered_G", G_val), elapsed_time), file = output_file, append = TRUE)
    }, error = function(e) {
      write(sprintf("%-40s: FAILED - %s", paste0("plot_CONNECTORDataClustered_G", G_val), conditionMessage(e)), file = output_file, append = TRUE)
      failed_ops <<- c(failed_ops, paste0("plot_CONNECTORDataClustered_G", G_val))
    })
    Sys.sleep(0.1)
    
    cat(paste0("Running DiscriminantPlot G=", G_val, "...\n"))
    flush.console()
    tryCatch({
      elapsed_time <- system.time({
        DiscriminantPlot(CONNECTORDataClustered = CONNECTORDataClustered, feature = "gender")
      })[["elapsed"]]
      write(sprintf("%-40s: %10.3f s", paste0("DiscriminantPlot_G", G_val), elapsed_time), file = output_file, append = TRUE)
    }, error = function(e) {
      write(sprintf("%-40s: FAILED - %s", paste0("DiscriminantPlot_G", G_val), conditionMessage(e)), file = output_file, append = TRUE)
      failed_ops <<- c(failed_ops, paste0("DiscriminantPlot_G", G_val))
    })
    Sys.sleep(0.1)
    
    cat(paste0("Running validateCluster G=", G_val, "...\n"))
    flush.console()
    tryCatch({
      elapsed_time <- system.time({
        validateCluster(CONNECTORDataClustered)
      })[["elapsed"]]
      write(sprintf("%-40s: %10.3f s", paste0("validateCluster_G", G_val), elapsed_time), file = output_file, append = TRUE)
    }, error = function(e) {
      write(sprintf("%-40s: FAILED - %s", paste0("validateCluster_G", G_val), conditionMessage(e)), file = output_file, append = TRUE)
      failed_ops <<- c(failed_ops, paste0("validateCluster_G", G_val))
    })
    Sys.sleep(0.1)
    
    cat(paste0("Running splinePlot G=", G_val, "...\n"))
    flush.console()
    tryCatch({
      elapsed_time <- system.time({
        splinePlot(CONNECTORDataClustered = CONNECTORDataClustered)
      })[["elapsed"]]
      write(sprintf("%-40s: %10.3f s", paste0("splinePlot_G", G_val), elapsed_time), file = output_file, append = TRUE)
    }, error = function(e) {
      write(sprintf("%-40s: FAILED - %s", paste0("splinePlot_G", G_val), conditionMessage(e)), file = output_file, append = TRUE)
      failed_ops <<- c(failed_ops, paste0("splinePlot_G", G_val))
    })
    Sys.sleep(0.1)
    
    cat(paste0("Running MaximumDiscriminationFunction G=", G_val, "...\n"))
    flush.console()
    tryCatch({
      elapsed_time <- system.time({
        MaximumDiscriminationFunction(CONNECTORDataClustered = CONNECTORDataClustered)
      })[["elapsed"]]
      write(sprintf("%-40s: %10.3f s", paste0("MaximumDiscriminationFunction_G", G_val), elapsed_time), file = output_file, append = TRUE)
    }, error = function(e) {
      write(sprintf("%-40s: FAILED - %s", paste0("MaximumDiscriminationFunction_G", G_val), conditionMessage(e)), file = output_file, append = TRUE)
      failed_ops <<- c(failed_ops, paste0("MaximumDiscriminationFunction_G", G_val))
    })
    Sys.sleep(0.1)
  }
  
  if (length(failed_ops) > 0) {
    write(sprintf("\nFailed operations: %s", paste(failed_ops, collapse = ", ")), file = output_file, append = TRUE)
  }
  
  cat("Test completed\n")
  flush.console()
  
  # Pulizia memoria
  rm(Data, Annotations, all_data, clusters, CONNECTORDataClustered, CrossLogLikePlot)
  invisible(gc(verbose = FALSE, full = TRUE))
  
  objects_to_remove <- c("Data", "Annotations", "all_data", "clusters", 
                         "CONNECTORDataClustered", "CrossLogLikePlot")
  
  # Rimuovi solo gli oggetti che esistono
  for(obj in objects_to_remove) {
    if(exists(obj)) {
      rm(list = obj)
    }
  }
  
  # Garbage collection più aggressivo
  for(i in 1:3) {
    gc(verbose = FALSE, full = TRUE)
    Sys.sleep(0.5)
  }
  
  return(TRUE)
}


# Parametri di test
num_curves_vals <- c(10, 30, 50)
measures_sets <- list(
  c("Hyperbola"),
  c("Parabola", "Hyperbola"),
  c("Parabola", "Sine"),
  c("Parabola", "Hyperbola", "Sine", "Cosine"),
  c("Parabola", "Hyperbola", "Sine", "Cosine", "Logarithm")
)
p_values <- 5:8
G_values <- 2:5
max_points_vals <- c(30, 50, 80)
translation_vals <- c(1, 5, 10)

# Crea il file di output
output_file <- "testing_results2.txt"
file.create(output_file)
cat("Starting performance tests...\n")
cat("Output will be saved to:", output_file, "\n")
flush.console()

# Conta totale dei test
total_tests <- length(num_curves_vals) * length(measures_sets) * length(max_points_vals) * length(translation_vals)
current_test <- 0
successful_tests <- 0
failed_tests <- 0

# Esegui i test
for (num_curves in num_curves_vals) {
  for (measures_idx in seq_along(measures_sets)) {
    measures <- measures_sets[[measures_idx]]
    for (max_points in max_points_vals) {
      for (translation_val in translation_vals) {
        current_test <- current_test + 1
        
        test_config <- paste("Test configuration:",
                             "Curves:", num_curves,
                             "Measures:", paste(measures, collapse = ", "),
                             "Max Points:", max_points,
                             "Translation:", translation_val)
        
        cat("\n", paste0("[", current_test, "/", total_tests, "] "), test_config, "\n", sep = "")
        flush.console()
        
        # Scrivi la configurazione
        write(paste("\n[", current_test, "/", total_tests, "] ", test_config, sep = ""), 
              file = output_file, append = TRUE)
        write(paste("Date and time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), 
              file = output_file, append = TRUE)
        
        # Pulizia preventiva
        invisible(gc(verbose = FALSE, full = TRUE))
        Sys.sleep(0.5)
        
        # Esegui il test
        test_success <- tryCatch({
          run_test(num_curves, measures, p_values, G_values, max_points, translation_val, output_file)
        }, error = function(e) {
          error_msg <- paste("ERROR during test:", conditionMessage(e))
          write(error_msg, file = output_file, append = TRUE)
          cat(error_msg, "\n")
          flush.console()
          return(FALSE)
        })
        
        if (test_success) {
          successful_tests <- successful_tests + 1
        } else {
          failed_tests <- failed_tests + 1
        }
        
        write("\n-------------------------------------------------\n", file = output_file, append = TRUE)
        
        # Pulizia post-test
        invisible(gc(verbose = FALSE, full = TRUE))
        Sys.sleep(1)
        
        # Checkpoint ogni 5 test
        if (current_test %% 5 == 0) {
          cat("\n--- Memory cleanup checkpoint ---\n")
          cat(paste("Successful:", successful_tests, "| Failed:", failed_tests, "\n"))
          flush.console()
          invisible(gc(verbose = FALSE, full = TRUE))
          Sys.sleep(2)
        }
      }
    }
  }
}

cat("\n\n=== All tests completed ===\n")
cat("Results saved to:", output_file, "\n")
cat("Total tests run:", current_test, "\n")
cat("Successful:", successful_tests, "\n")
cat("Failed:", failed_tests, "\n")
flush.console()
