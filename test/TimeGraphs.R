library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(gridExtra)

parse_timing_file <- function(file_path) {
  lines <- readLines(file_path)
  
  data_list <- list()
  current_config <- NULL
  current_datetime <- NULL
  record_index <- 1
  
  for (i in 1:length(lines)) {
    line <- trimws(lines[i])
    
    if (grepl("^Test configuration:", line)) {
      curves <- as.numeric(gsub(".*Curves: (\\d+).*", "\\1", line))
      translation <- as.numeric(gsub(".*Translation: (\\d+).*", "\\1", line))
      max_points <- as.numeric(gsub(".*Max Points: (\\d+).*", "\\1", line))
      
      measures_part <- gsub(".*Measures: ([^M]*) Max.*", "\\1", line)
      n_measures <- length(strsplit(trimws(measures_part), "\\s+")[[1]])
      
      current_config <- list(
        curves = curves,
        translation = translation,
        max_points = max_points,
        n_measures = n_measures
      )
    }
    
    if (grepl("^Date and time:", line)) {
      current_datetime <- gsub("Date and time: ", "", line)
    }
    
    if (grepl(":", line) && grepl("\\d+\\.\\d+ s$", line) && !grepl("Date and time", line)) {
      parts <- strsplit(line, ":")[[1]]
      if (length(parts) == 2) {
        algorithm_full <- trimws(parts[1])
        time_str <- trimws(parts[2])
        time_value <- as.numeric(gsub(" s$", "", time_str))
        
        if (grepl("_G\\d+$", algorithm_full)) {
          algorithm_base <- gsub("_G\\d+$", "", algorithm_full)
          g_config <- gsub(".*_(G\\d+)$", "\\1", algorithm_full)
          g_number <- as.numeric(gsub("G", "", g_config))
        } else {
          algorithm_base <- algorithm_full
          g_config <- NA
          g_number <- NA
        }
        
        data_list[[record_index]] <- data.frame(
          test_config_id = paste0("Test_", length(data_list) %/% 50 + 1),
          curves = current_config$curves,
          translation = current_config$translation,
          max_points = current_config$max_points,
          n_measures = current_config$n_measures,
          algorithm_full = algorithm_full,
          algorithm_base = algorithm_base,
          g_config = g_config,
          g_number = g_number,
          time_seconds = time_value,
          datetime = current_datetime,
          stringsAsFactors = FALSE
        )
        record_index <- record_index + 1
      }
    }
  }
  
  df <- do.call(rbind, data_list)
  return(df)
}

df <- parse_timing_file("../demo/testing_results.txt")

print("Struttura dei dati:")
print(head(df, 10))
print(paste("Numero totale di osservazioni:", nrow(df)))
print(paste("Algoritmi unici:", length(unique(df$algorithm_full))))

algorithm_times <- df %>%
  group_by(algorithm_full) %>%
  summarise(
    avg_time = mean(time_seconds, na.rm = TRUE),
    min_time = min(time_seconds, na.rm = TRUE),
    max_time = max(time_seconds, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(avg_time))

print("\nTop 10 algoritmi più lenti:")
print(head(algorithm_times, 10))

slow_algorithms <- algorithm_times %>%
  filter(avg_time > 1) %>%
  pull(algorithm_full)

df_slow <- df %>% filter(algorithm_full %in% slow_algorithms)

estimate_dim <- df %>% 
  filter(grepl("estimatepDimension", algorithm_full, ignore.case = TRUE) | 
           grepl("estimateDimension", algorithm_full, ignore.case = TRUE))

estimate_cluster <- df %>% 
  filter(grepl("estimateCluster", algorithm_full, ignore.case = TRUE))

print(paste("\nOsservazioni estimateDimension:", nrow(estimate_dim)))
print(paste("Osservazioni estimateCluster:", nrow(estimate_cluster)))

setup_summary <- df %>%
  group_by(translation, curves, max_points, n_measures) %>%
  summarise(
    total_time = sum(time_seconds, na.rm = TRUE),
    avg_time = mean(time_seconds, na.rm = TRUE),
    count_obs = n(),
    .groups = 'drop'
  )

numeric_cols <- c("translation", "curves", "max_points", "n_measures", "avg_time")
cor_matrix <- cor(setup_summary[numeric_cols], use = "complete.obs")
print("\nMatrice di correlazione setup:")
print(round(cor_matrix, 3))

p1 <- ggplot(df_slow, aes(x = factor(translation), y = time_seconds, fill = algorithm_full)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Algoritmi Lenti: Distribuzione Tempi per Translation",
       x = "Translation", y = "Tempo (secondi, scala log)") +
  theme_minimal() +
  theme(legend.position = "none")

if(nrow(estimate_dim) > 0) {
  p2 <- ggplot(estimate_dim, aes(x = translation, y = time_seconds)) +
    geom_point(aes(color = factor(curves)), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = "estimateDimension: Translation vs Tempo",
         x = "Translation", y = "Tempo (secondi)",
         color = "Curves") +
    theme_minimal()
} else {
  p2 <- ggplot() + labs(title = "Nessun dato estimateDimension trovato")
}

if(nrow(estimate_cluster) > 0) {
  p3 <- ggplot(estimate_cluster, aes(x = translation, y = time_seconds)) +
    geom_point(aes(color = factor(curves)), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = "estimateCluster: Translation vs Tempo",
         x = "Translation", y = "Tempo (secondi)",
         color = "Curves") +
    theme_minimal()
} else {
  p3 <- ggplot() + labs(title = "Nessun dato estimateCluster trovato")
}

p4 <- ggplot(setup_summary, aes(x = translation, y = avg_time)) +
  geom_point(aes(size = n_measures, color = factor(curves)), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  scale_y_log10() +
  labs(title = "Setup Completo: Translation vs Tempo Medio",
       x = "Translation", y = "Tempo Medio (secondi, scala log)",
       size = "N° Measures", color = "Curves") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

if(nrow(estimate_dim) > 0 && nrow(estimate_cluster) > 0) {
  heavy_algorithms <- rbind(
    estimate_dim %>% mutate(algorithm_type = "estimateDimension"),
    estimate_cluster %>% mutate(algorithm_type = "estimateCluster")
  )
  
  p5 <- ggplot(heavy_algorithms, aes(x = factor(translation), y = time_seconds, fill = algorithm_type)) +
    geom_boxplot(position = "dodge") +
    labs(title = "Confronto Algoritmi Pesanti per Translation",
         x = "Translation", y = "Tempo (secondi)",
         fill = "Algoritmo") +
    theme_minimal()
  
  p6 <- ggplot(heavy_algorithms, aes(x = n_measures, y = time_seconds, color = algorithm_type)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~translation, labeller = labeller(.cols = function(x) paste("Translation:", x))) +
    labs(title = "Algoritmi Pesanti: N° Measures vs Tempo",
         x = "Numero Measures", y = "Tempo (secondi)",
         color = "Algoritmo") +
    theme_minimal()
  
  p7 <- heavy_algorithms %>%
    group_by(translation, algorithm_type) %>%
    summarise(avg_time = mean(time_seconds), .groups = 'drop') %>%
    ggplot(aes(x = factor(translation), y = avg_time, fill = algorithm_type)) +
    geom_col(position = "dodge") +
    labs(title = "Tempo Medio Algoritmi Pesanti per Translation",
         x = "Translation", y = "Tempo Medio (secondi)",
         fill = "Algoritmo") +
    theme_minimal()
  
  p8 <- heavy_algorithms %>%
    ggplot(aes(x = factor(curves), y = time_seconds, fill = algorithm_type)) +
    geom_boxplot(position = "dodge") +
    labs(title = "Algoritmi Pesanti: Effetto del numero di Curves",
         x = "Curves", y = "Tempo (secondi)",
         fill = "Algoritmo") +
    theme_minimal()
  
  grid.arrange(p5, p6, p7, p8, nrow = 2, ncol = 2)
}

cat("\n=== ANALISI CORRELAZIONI ===\n")
for(var in c("translation", "n_measures", "curves", "max_points")) {
  cor_test <- cor.test(setup_summary[[var]], setup_summary$avg_time)
  cat(sprintf("%s vs Tempo: r=%.4f, p=%.4e\n", 
              var, cor_test$estimate, cor_test$p.value))
}

if(nrow(estimate_dim) > 0) {
  cat("\nCorrelazioni estimateDimension:\n")
  for(var in c("translation", "n_measures", "curves", "max_points")) {
    if(var %in% names(estimate_dim) && length(unique(estimate_dim[[var]])) > 1) {
      cor_test <- cor.test(estimate_dim[[var]], estimate_dim$time_seconds)
      cat(sprintf("%s vs Tempo: r=%.4f, p=%.4e\n", 
                  var, cor_test$estimate, cor_test$p.value))
    }
  }
}

if(nrow(estimate_cluster) > 0) {
  cat("\nCorrelazioni estimateCluster:\n")
  for(var in c("translation", "n_measures", "curves", "max_points")) {
    if(var %in% names(estimate_cluster) && length(unique(estimate_cluster[[var]])) > 1) {
      cor_test <- cor.test(estimate_cluster[[var]], estimate_cluster$time_seconds)
      cat(sprintf("%s vs Tempo: r=%.4f, p=%.4e\n", 
                  var, cor_test$estimate, cor_test$p.value))
    }
  }
}

cat("\n=== STATISTICHE ALGORITMI PESANTI ===\n")
if(nrow(estimate_dim) > 0) {
  dim_stats <- estimate_dim %>%
    group_by(translation) %>%
    summarise(
      mean_time = mean(time_seconds),
      sd_time = sd(time_seconds),
      min_time = min(time_seconds),
      max_time = max(time_seconds),
      .groups = 'drop'
    )
  cat("estimateDimension per Translation:\n")
  print(dim_stats)
}

if(nrow(estimate_cluster) > 0) {
  cluster_stats <- estimate_cluster %>%
    group_by(translation) %>%
    summarise(
      mean_time = mean(time_seconds),
      sd_time = sd(time_seconds),
      min_time = min(time_seconds),
      max_time = max(time_seconds),
      .groups = 'drop'
    )
  cat("\nestimateCluster per Translation:\n")
  print(cluster_stats)
}