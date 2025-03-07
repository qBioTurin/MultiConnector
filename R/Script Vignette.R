library(readxl)
library(dplyr)
library(readr)
library(tibble)
library(magrittr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(parallel)
library(MASS)
library(splines)
library(rlist)
library(RhpcBLASctl)
library(RColorBrewer)
library(Matrix)

# Set seed for reproducibility
set.seed(9393)
#9393
generate_curve <- function(n, curve_type) {
  data <- data.frame()
  
  translation<-5
  
  for (i in 1:n) {
    # Genera un numero casuale di punti x tra 5 e 30
    num_points <- max(5, rbinom(1, 30, 1/2))
    
    # Campiona casualmente i punti x nell'intervallo -10 a 10
    x <- sample(seq(-10, 10, 0.1), num_points)
    
    # Ordina i punti x per mantenere la forma della curva
    x <- sort(x)
    
    y <- switch(curve_type,
                "Parabola" = {
                  is_translated <- sample(c(TRUE, FALSE), 1)  # Scelta casuale se traslare o no
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
                  if (i %% 2 == 1) 150*sin(x/3) else -150*sin(x/3)
                },
                "Cosine" = {
                  if (i %% 2 == 1) 100*cos(x/3) else -100*cos(x/3)
                })
    
    # Rimuovi i valori NA (se presenti nell'iperbole)
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
    
    # Applica la deviazione standard appropriata in base al tipo di curva
    sd_value <- if (curve_type %in% c("Parabola", "Hyperbola")) 20 else 0
    y <- y + rnorm(length(x), sd = sd_value)
    
    temp <- data.frame(
      time = x,
      value = y,
      measureID = curve_type,
      subjID = factor(paste0(i))
    )
    data <- rbind(data, temp)
  }
  return(data)
}
# Genera parabole

num_curves <- 30
parabolas <- generate_curve(num_curves, "Parabola")

# Genera iperboloidi cubici
hyperbolas <- generate_curve(num_curves, "Hyperbola")

sines <- generate_curve(num_curves, "Sine")

cosines <- generate_curve(num_curves, "Cosine")

TimeSeries <- as_tibble(rbind(parabolas, hyperbolas, sines, cosines))
generate_annotation_file <- function(curves_data) {
  # Estrai i subjID unici dalle curve generate
  unique_subjects <- unique(curves_data$subjID)
  
  # Crea un dataframe di annotazioni
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


Annotations <- generate_annotation_file(iperboltest)
