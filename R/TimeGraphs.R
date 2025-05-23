# Analisi Correlazione Grafica tra Setup e Tempi di Esecuzione
library(tidyverse)
library(corrplot)
library(GGally)

# Funzione per parsificare il file di timing (versione semplificata)
parse_timing_file <- function(file_path) {
  lines <- readLines(file_path)
  all_data <- list()
  current_setup <- NULL
  current_timings <- NULL
  
  i <- 1
  while (i <= length(lines)) {
    line <- trimws(lines[i])
    
    # Salta righe vuote o con errori
    if (line == "" || grepl("ERROR|Error", line)) {
      i <- i + 1
      next
    }
    
    # Configurazione test
    if (grepl("^Test configuration:", line)) {
      curves <- as.numeric(str_extract(line, "(?<=Curves: )\\d+"))
      measures <- str_extract(line, "(?<=Measures: )[^M]+(?=Max)")
      n_measures <- length(str_split(str_trim(measures), ",")[[1]])
      max_points <- as.numeric(str_extract(line, "(?<=Max Points: )\\d+"))
      translation <- as.numeric(str_extract(line, "(?<=Translation: )\\d+"))
      
      if (!any(is.na(c(curves, max_points, translation, n_measures)))) {
        current_setup <- data.frame(
          curves = curves,
          n_measures = n_measures,
          max_points = max_points,
          translation = translation
        )
      }
    }
    
    # Timing results
    if (grepl("^Timing results:", line) && !is.null(current_setup)) {
      current_timings <- data.frame()
      i <- i + 1
      
      while (i <= length(lines) && !grepl("^-+$", lines[i]) && 
             !grepl("^Test configuration:", lines[i])) {
        timing_line <- trimws(lines[i])
        
        if (timing_line != "" && grepl(":", timing_line) && 
            !grepl("ERROR|Error", timing_line)) {
          parts <- str_split(timing_line, ":")[[1]]
          if (length(parts) >= 2) {
            method_name <- str_trim(parts[1])
            time_value <- as.numeric(str_extract(str_trim(parts[2]), "[0-9.]+"))
            
            if (!is.na(time_value) && time_value > 0) {
              current_timings <- rbind(current_timings, 
                                       data.frame(method = method_name, time_seconds = time_value))
            }
          }
        }
        i <- i + 1
      }
      
      if (nrow(current_timings) > 0) {
        combined_data <- current_setup[rep(1, nrow(current_timings)), ]
        combined_data <- cbind(combined_data, current_timings)
        all_data <- append(all_data, list(combined_data))
      }
    }
    i <- i + 1
  }
  
  if (length(all_data) > 0) {
    return(do.call(rbind, all_data))
  } else {
    return(data.frame())
  }
}

# Funzione per creare visualizzazioni correlazione
create_correlation_plots <- function(data) {
  
  # Preparazione dati numerici
  numeric_data <- data %>%
    select(curves, n_measures, max_points, translation, time_seconds) %>%
    na.omit()
  
  # 1. Matrice di correlazione
  cat("=== CORRELAZIONI CON TEMPO DI ESECUZIONE ===\n")
  correlations <- cor(numeric_data)
  time_cors <- correlations[, "time_seconds"]
  time_cors <- time_cors[names(time_cors) != "time_seconds"]
  
  for (param in names(time_cors)) {
    cat(sprintf("%s: %.3f\n", param, time_cors[param]))
  }
  cat("\n")
  
  # Setup grafici
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
  
  # 2. Heatmap correlazioni
  corrplot(correlations, method = "color", type = "upper",
           title = "Matrice Correlazioni", mar = c(0,0,2,0),
           tl.cex = 0.8, cl.cex = 0.8)
  
  # 3. Scatter plots per ogni parametro vs tempo
  # Curves vs Time
  plot(numeric_data$curves, numeric_data$time_seconds,
       xlab = "Numero Curve", ylab = "Tempo (sec)",
       main = paste0("Curve vs Tempo\n(r = ", round(cor(numeric_data$curves, numeric_data$time_seconds), 3), ")"),
       pch = 16, col = rgb(0.2, 0.4, 0.8, 0.6))
  abline(lm(time_seconds ~ curves, data = numeric_data), col = "red", lwd = 2)
  
  # Max Points vs Time  
  plot(numeric_data$max_points, numeric_data$time_seconds,
       xlab = "Max Points", ylab = "Tempo (sec)",
       main = paste0("Max Points vs Tempo\n(r = ", round(cor(numeric_data$max_points, numeric_data$time_seconds), 3), ")"),
       pch = 16, col = rgb(0.8, 0.2, 0.4, 0.6))
  abline(lm(time_seconds ~ max_points, data = numeric_data), col = "red", lwd = 2)
  
  # Translation vs Time
  plot(numeric_data$translation, numeric_data$time_seconds,
       xlab = "Translation", ylab = "Tempo (sec)",
       main = paste0("Translation vs Tempo\n(r = ", round(cor(numeric_data$translation, numeric_data$time_seconds), 3), ")"),
       pch = 16, col = rgb(0.2, 0.8, 0.4, 0.6))
  abline(lm(time_seconds ~ translation, data = numeric_data), col = "red", lwd = 2)
  
  # N Measures vs Time
  plot(numeric_data$n_measures, numeric_data$time_seconds,
       xlab = "Numero Misure", ylab = "Tempo (sec)",
       main = paste0("N° Misure vs Tempo\n(r = ", round(cor(numeric_data$n_measures, numeric_data$time_seconds), 3), ")"),
       pch = 16, col = rgb(0.8, 0.4, 0.2, 0.6))
  abline(lm(time_seconds ~ n_measures, data = numeric_data), col = "red", lwd = 2)
  
  # Combinazione parametri (esempio: curves * max_points)
  combined_param <- numeric_data$curves * numeric_data$max_points
  plot(combined_param, numeric_data$time_seconds,
       xlab = "Curves × Max Points", ylab = "Tempo (sec)",
       main = paste0("Complessità vs Tempo\n(r = ", round(cor(combined_param, numeric_data$time_seconds), 3), ")"),
       pch = 16, col = rgb(0.6, 0.2, 0.8, 0.6))
  abline(lm(time_seconds ~ combined_param, data = numeric_data), col = "red", lwd = 2)
  
  par(mfrow = c(1, 1))
  
  # 4. Grafico avanzato con ggplot per setup combinations
  setup_summary <- data %>%
    group_by(curves, max_points, translation) %>%
    summarise(
      avg_time = mean(time_seconds, na.rm = TRUE),
      median_time = median(time_seconds, na.rm = TRUE),
      n_methods = n(),
      .groups = 'drop'
    )
  
  # Heatmap setup combinations
  p1 <- ggplot(setup_summary, aes(x = factor(curves), y = factor(max_points), fill = avg_time)) +
    geom_tile() +
    scale_fill_gradient(low = "lightblue", high = "darkred", name = "Tempo\nMedio (s)") +
    labs(title = "Tempo Medio per Combinazione Setup",
         x = "Numero Curve", y = "Max Points") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p1)
  
  # Grafico a bolle
  p2 <- ggplot(setup_summary, aes(x = curves, y = max_points, size = avg_time, color = factor(translation))) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(name = "Tempo\nMedio (s)", range = c(2, 15)) +
    scale_color_brewer(type = "qual", palette = "Set1", name = "Translation") +
    labs(title = "Correlazione Setup-Performance",
         x = "Numero Curve", y = "Max Points",
         subtitle = "Dimensione bolla = Tempo medio") +
    theme_minimal()
  
  print(p2)
  
  return(list(
    correlations = time_cors,
    numeric_data = numeric_data,
    setup_summary = setup_summary
  ))
}

# Funzione principale semplificata
analyze_timing_correlations <- function(file_path) {
  cat("Caricamento dati da:", file_path, "\n\n")
  
  # Parse dati
  data <- parse_timing_file(file_path)
  
  if (nrow(data) == 0) {
    cat("Nessun dato valido trovato.\n")
    return(NULL)
  }
  
  cat("Dati caricati:", nrow(data), "osservazioni\n")
  cat("Setup unici:", nrow(unique(data[c("curves", "max_points", "translation")])), "\n")
  cat("Metodi:", length(unique(data$method)), "\n\n")
  
  # Analisi correlazioni grafiche
  results <- create_correlation_plots(data)
  
  return(list(
    data = data,
    correlations = results$correlations,
    setup_summary = results$setup_summary
  ))
}

# Utilizzo:
results <- analyze_timing_correlations("C:/Users/marco/OneDrive/Desktop/Ricerca/MultiConnector/R/testing_results_complete.txt")


