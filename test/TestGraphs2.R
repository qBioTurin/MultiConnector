library(tidyverse)

# Leggi tutto il file
lines <- readLines("../demo/testing_results.txt")

# Trova gli indici in cui iniziano i blocchi
start_indices <- grep("^Test configuration:", lines)

# Aggiungi anche la fine dell'ultimo blocco
start_indices <- c(start_indices, length(lines) + 1)

# Inizializza lista risultati
results <- list()

# Itera su ogni blocco
for (i in 1:(length(start_indices) - 1)) {
  block_lines <- lines[start_indices[i]:(start_indices[i + 1] - 1)]
  
  # Estrai configurazione
  config_line <- block_lines[1]
  config <- str_match(config_line, "Curves: (\\d+) Measures: ([^ ]+) Max Points: (\\d+) Translation: (\\d+)")
  config <- config[, 2:5]
  names(config) <- c("Curves", "Measures", "MaxPoints", "Translation")
  
  # Trova dove iniziano i timing results
  timing_start <- grep("^Timing results:", block_lines)
  if (length(timing_start) == 0) next
  
  timings <- block_lines[(timing_start + 1):length(block_lines)]
  
  # Pulisci riga per riga
  for (line in timings) {
    line <- str_trim(line)
    if (line == "" | grepl("^-+$", line)) next
    
    # Estrai funzione e tempo
    matched <- str_match(line, "^([A-Za-z0-9_]+(?:_G\\d+)?)\\s*:\\s*([0-9.]+) s$")
    if (is.na(matched[1])) next
    
    fname <- matched[2]
    time <- as.numeric(matched[3])
    
    # Estrai G se esiste
    G_val <- str_match(fname, "_G(\\d+)$")[,2]
    G_val <- ifelse(is.na(G_val), NA, as.integer(G_val))
    
    fname_clean <- sub("_G\\d+$", "", fname)
    
    # Aggiungi alla lista
    results[[length(results) + 1]] <- c(config,
                                        Function = fname_clean,
                                        G = G_val,
                                        Time = time)
  }
}

# Converti in tibble
df <- bind_rows(results) %>%
  mutate(across(c(Curves, MaxPoints, Translation, G), as.integer),
         Time = as.numeric(Time))

# Visualizza un esempio
print(head(df))

# Plot di esempio: funzioni più lente
df %>%
  group_by(Function) %>%
  summarise(mean_time = mean(Time, na.rm = TRUE)) %>%
  arrange(desc(mean_time)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(Function, mean_time), y = mean_time)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 funzioni più lente in media",
       x = "Funzione", y = "Tempo medio (s)") +
  theme_minimal()


#Grafici:
# Funzioni che richiedono più tempo in media
top_funcs <- df %>%
  group_by(Function) %>%
  summarise(avg_time = mean(Time)) %>%
  arrange(desc(avg_time)) %>%
  filter(avg_time > 0.2)

print(top_funcs)

df %>%
  filter(Function %in% top_funcs$Function) %>%
  ggplot(aes(x = Function, y = Time)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Tempi di esecuzione per funzione",
       y = "Tempo (s)", x = "Funzione")

df %>%
  filter(Function == "estimateCluster") %>%
  ggplot(aes(x = Translation, y = Time)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~Function, scales = "free_y") +
  theme_minimal()

df %>%
  filter(!is.na(G), Function %in% c("SilEntropy", "splinePlot", "MaximumDiscriminationFunction")) %>%
  ggplot(aes(x = G, y = Time, color = as.factor(Translation))) +
  geom_line() +
  geom_point() +
  facet_wrap(~Function) +
  theme_minimal() +
  labs(title = "Tempo per G (cluster)", color = "Translation")




