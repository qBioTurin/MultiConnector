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

# Funzione wrapper per eseguire con retry
safe_execute <- function(funz, func_name, max_retries = 3, wait_time = 2, output_file) {
  for (attempt in 1:max_retries) {
    result <- tryCatch({
      elapsed_time <- system.time({
        eval(funz)  # <-- salva il valore
      })[["elapsed"]]
      value <- eval(funz)
      write(sprintf("%-40s: %10.3f s", func_name, elapsed_time), 
            file = output_file, append = TRUE)
      
      return(list(success = TRUE, time = elapsed_time, value = value))
      
    }, error = function(e) {
      error_msg <- sprintf("  %-40s: Attempt %d/%d FAILED - %s", 
                           func_name, attempt, max_retries, conditionMessage(e))
      cat(error_msg, "\n")
      write(error_msg, file = output_file, append = TRUE)
      flush.console()
      
      if (attempt < max_retries) {
        cat(sprintf("  Waiting %d seconds before retry...\n", wait_time))
        flush.console()
        Sys.sleep(wait_time)
        invisible(gc(verbose = FALSE, full = TRUE))
      }
      
      return(list(success = FALSE, error = conditionMessage(e)))
    })
    
    if (result$success) {
      return(result)
    }
  }
  
  # Tutti i tentativi falliti
  final_msg <- sprintf("%-40s: SKIPPED after %d failed attempts", func_name, max_retries)
  write(final_msg, file = output_file, append = TRUE)
  cat("  ", final_msg, "\n")
  flush.console()
  
  return(list(success = FALSE, skipped = TRUE))
}

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

# Funzione per eseguire il test con gestione robusta degli errori
run_test <- function(num_curves, measures, p_values, G_values, max_points, translation_val, output_file) {
  
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
  
  # Scrivi l'intestazione dei risultati
  write("Timing results:", file = output_file, append = TRUE)
  
  # Variabili per tracciare i risultati
  Data <- NULL
  clusters <- NULL
  ConfigChosen_list <- list()
  
  # ConnectorData - critico, deve funzionare
  cat("Running ConnectorData...\n")
  flush.console()
  result <- safe_execute(
    funz = quote(ConnectorData(all_data, Annotations)),
    func_name = "ConnectorData",
    output_file = output_file
  )
  
  if (!result$success || is.null(result$value)) {
    write("CRITICAL ERROR: ConnectorData failed. Skipping entire test.", file = output_file, append = TRUE)
    return(FALSE)
  }
  Data<-result$Data
  Sys.sleep(0.1)
  
  # plot
  safe_execute(
    funz = quote({ plot(Data, feature = "treatment_group") }),
    func_name = "plot",
    output_file = output_file
  )
  Sys.sleep(0.1)
  
  # plotTimes normal
  safe_execute(
    funz = quote({ plotTimes(Data) }),
    func_name = "plotTimes_normal",
    output_file = output_file
  )
  Sys.sleep(0.1)
  
  # plotTimes large
  safe_execute(
    funz = quote({ plotTimes(Data, large = TRUE) }),
    func_name = "plotTimes_large",
    output_file = output_file
  )
  Sys.sleep(0.1)
  
  # truncatePlot e truncate
  if (length(measures) > 0) {
    safe_execute(
      funz = quote({ truncatePlot(Data, measure = measures[1], truncTime = 5) }),
      func_name = "truncatePlot",
      output_file = output_file
    )
    Sys.sleep(0.1)
    
    safe_execute(
      funz = quote({ truncate(Data, measure = measures[1], truncTime = 5) }),
      func_name = "truncate",
      output_file = output_file
    )
    Sys.sleep(0.1)
  }
  
  # estimatepDimension
  safe_execute(
    funz = quote({ CrossLogLikePlot <<- estimatepDimension(Data, p = p_values, cores = min(10, detectCores())) }),
    func_name = "estimatepDimension",
    output_file = output_file
  )
  Sys.sleep(0.1)
  
  # estimateCluster - critico per i test successivi
  p_vector <- setNames(rep(p_values[length(p_values) %/% 2], length(measures)), measures)
  cat("Running estimateCluster...\n")
  flush.console()
  result <- safe_execute(
    funz = quote({ clusters <<- estimateCluster(Data, G = G_values, p = p_vector, runs = 100, cores = min(10, detectCores())) }),
    func_name = "estimateCluster",
    output_file = output_file
  )
  
  if (!result$success || is.null(result$value)) {
    write("WARNING: estimateCluster failed. Skipping cluster-dependent tests.", file = output_file, append = TRUE)
    cat("WARNING: Skipping cluster-dependent tests due to estimateCluster failure.\n")
    flush.console()
    rm(Data, Annotations, all_data)
    invisible(gc(verbose = FALSE, full = TRUE))
    return(TRUE)  # Non è un fallimento totale
  }
  clusters<-result$value
  Sys.sleep(0.1)
  
  # Loop per ogni G
  for (G_val in G_values) {
    cat(paste0("Running tests for G=", G_val, "...\n"))
    flush.console()
    
    # configSelection
    ConfigChosen <- NULL
    result <- safe_execute(
      funz = quote({ ConfigChosen <<- configSelection(clusters, G = G_val, "MinfDB") }),
      func_name = paste0("configSelection_G", G_val),
      output_file = output_file
    )
    
    if (!result$success || is.null(result$value)) {
      write(paste0("WARNING: Skipping remaining tests for G=", G_val), file = output_file, append = TRUE)
      cat(paste0("WARNING: Skipping remaining tests for G=", G_val, "\n"))
      flush.console()
      next  # Passa al prossimo G
    }
    ConfigChosen <- result$value
    Sys.sleep(0.1)
    
    # plot_ConfigChosen
    safe_execute(
      funz = quote({ plot(Data, ConfigChosen = ConfigChosen, feature = "comorbidity") }),
      func_name = paste0("plot_ConfigChosen_G", G_val),
      output_file = output_file
    )
    Sys.sleep(0.1)
    
    # DiscriminantPlot
    safe_execute(
      funz = quote({ DiscriminantPlot(Data, ConfigChosen = ConfigChosen, feature = "gender") }),
      func_name = paste0("DiscriminantPlot_G", G_val),
      output_file = output_file
    )
    Sys.sleep(0.1)
    
    # SilEntropy
    safe_execute(
      funz = quote({ SilEntropy(ConfigChosen) }),
      func_name = paste0("SilEntropy_G", G_val),
      output_file = output_file
    )
    Sys.sleep(0.1)
    
    # splinePlot
    safe_execute(
      funz = quote({ splinePlot(ConfigChosen = ConfigChosen) }),
      func_name = paste0("splinePlot_G", G_val),
      output_file = output_file
    )
    Sys.sleep(0.1)
    
    # MaximumDiscriminationFunction - la funzione problematica
    safe_execute(
      funz = quote({ MaximumDiscriminationFunction(ConfigChosen = ConfigChosen) }),
      func_name = paste0("MaximumDiscriminationFunction_G", G_val),
      max_retries = 5,  # Più tentativi per questa funzione problematica
      wait_time = 3,    # Più tempo di attesa
      output_file = output_file
    )
    Sys.sleep(0.1)
    
    # Pulizia ConfigChosen per questo G
    rm(ConfigChosen)
    invisible(gc(verbose = FALSE, full = TRUE))
  }
  
  cat("Test completed\n")
  flush.console()
  
  # Pulizia memoria
  rm(Data, Annotations, all_data, clusters)
  if (exists("CrossLogLikePlot")) rm(CrossLogLikePlot)
  invisible(gc(verbose = FALSE, full = TRUE))
  
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
output_file <- "testing_results3.txt"
file.create(output_file)
cat("Starting performance tests with robust error handling...\n")
cat("Output will be saved to:", output_file, "\n")
flush.console()

# Conta totale dei test
total_tests <- length(num_curves_vals) * length(measures_sets) * length(max_points_vals) * length(translation_vals)
current_test <- 0
successful_tests <- 0
failed_tests <- 0
partial_tests <- 0

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
          error_msg <- paste("CRITICAL ERROR during test:", conditionMessage(e))
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