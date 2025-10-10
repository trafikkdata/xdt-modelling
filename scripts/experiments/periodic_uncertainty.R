library(dplyr)
library(ggplot2)


data <- readRDS("data/processed/engineered_data.rds")

periodic <- filter(data, bestDataSourceAadt_registrationFrequency == "PERIODIC")

# How many percent of periodic measurements have zero uncertainty?
sum(periodic$bestDataSourceAadt_correctedStandardError == 0)/nrow(periodic)*100

periodic_zero_sd <- filter(periodic, aadt_sd == 0)

ggplot2::ggplot(periodic_zero_sd, 
                aes(x = bestDataSourceAadt_coverage)) + 
  geom_histogram()

