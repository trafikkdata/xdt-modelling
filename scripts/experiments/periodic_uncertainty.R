# Periodic uncertainty

# Many of the periodic AADT-numbers have uncertainty zero. I don't think that 
# is intended.

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(dplyr)
library(ggplot2)

# Data
data <- readRDS("data/processed/engineered_data.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Examining the uncertainty of periodic registrations ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

periodic <- filter(data, bestDataSourceAadt_registrationFrequency == "PERIODIC")

# How many percent of periodic measurements have zero uncertainty?
sum(periodic$bestDataSourceAadt_correctedStandardError == 0)/nrow(periodic)*100

periodic_zero_sd <- filter(periodic, aadt_sd == 0)

ggplot2::ggplot(periodic_zero_sd, 
                aes(x = bestDataSourceAadt_coverage)) + 
  geom_histogram()

