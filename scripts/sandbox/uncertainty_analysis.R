

# Load functions
files.sources = list.files("../R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(tidyverse)
library(tictoc)

# Data
data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- readRDS("data/processed/nodes.RDS")
adjacency_matrix <- readRDS("data/processed/adjacency_matrix_2024.rds")




models <- readRDS("results/with_and_wo_last_year.RDS")
model <- models$model
model_with_last_year <- models$model_with_last_year


model_data <- model$diagnostics$approval$undirected_data %>% mutate(cv = sd/pred)

model_data$cv[!is.finite(model_data$cv)] <- NA

summary(model_data$cv)

# Må legge til funksjonell vegklasse

ggplot(model_data, aes(x = cv*100, y = stat(density))) +
  geom_histogram(binwidth = 1, color = NA, fill = "lightblue3") +
  xlim(0, 500) +
  ylim(0, 0.1) +
  #facet_wrap(~Vegkategori) +
  facet_wrap(~ is.na(data.2024)) +
  theme_bw()
  #scale_x_log10(labels = scales::comma) +
  #facet_wrap(~Vegkategori)

ggplot(aadt2024, aes(x = as.numeric(Usikkerhet.Estimert), y = stat(density))) +
  geom_histogram(bins = 500) +
  xlim(0, 500) +
  ylim(0, 0.1) +
  theme_bw()


sum(model_data$cv*100 > 100, na.rm = TRUE)/nrow(model_data)
