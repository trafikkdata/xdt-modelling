
#config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/exploratory_analysis.R")

library(dplyr)
library(ggplot2)
library(purrr)
library(gridExtra)
library(fitdistrplus)

theme_set(theme_bw())

data <- readRDS("data/processed/preprocessed_data.rds")

# Number of AADT observations
(aadt_obs <- sum(!is.na(data$bestDataSourceAadt_trafficVolumeValue)))
cat(round(aadt_obs/nrow(data)*100), "% of traffic links have a registered traffic volume.")

# Distribution of data sources
table(data$bestDataSourceAadt_sourceType, data$bestDataSourceAadt_registrationFrequency)

# Number of derived versus measured values
table(data$bestDataSourceAadt_trafficVolumeType)

# Distribution of heavy weight data sources
heavy_data <- filter(data, !is.na(bestDataSourceAadt_heavyRatio))
table(heavy_data$bestDataSourceAadt_sourceType, heavy_data$bestDataSourceAadt_registrationFrequency)
table(heavy_data$bestDataSourceAadt_trafficVolumeType)

# Markdown table of all the columns
cat(paste0("| Column name |  | \n", "| - | - | \n", 
           paste0("| ", colnames(data), collapse = " | | \n"), " | | "))

# Variables that make sense as covariates
potential_covariates <- c(
  "functionalRoadClass", "functionClass", "highestSpeedLimit", 
  "lowestSpeedLimit", "isNorwegianScenicRoute", "isFerryRoute", "isRamp", 
  "isBlocked", "isInvalid", "yearAppliesTo", "municipalityIds", "countyIds", 
  "roadCategory", "length", "maxLanes", "minLanes", 
  "hasOnlyPublicTransportLanes", "lastYearAadt", "countyIds")

cov_data <- dplyr::select(data, all_of(c(potential_covariates, "aadt")))

# Percentage of missing values in each column
colMeans(is.na(cov_data)) * 100

glimpse(cov_data)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Response variable analysis ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Histogram of AADT measurements
ggplot(cov_data, aes(x = aadt)) +
  geom_histogram()

# Remove the missing values for looking closer at the measurements
aadt_clean <- cov_data$aadt[!is.na(cov_data$aadt)]

cat("Number of AADT values that are zero:", sum(aadt_clean == 0))
cat("Number of values above 40 000:", sum(aadt_clean > 40000))
cat("Max AADT value:", max(aadt_clean))

# Basic summary statistics
cat("Mean:", mean(aadt_clean), "\n", 
    "Variance:", var(aadt_clean), "\n",
    "Variance-to-Mean Ratio:", var(aadt_clean)/mean(aadt_clean), "\n",
    "Standard Deviation:", sd(aadt_clean), "\n")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Covariate analysis ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The main take-away from this is probably which covariate categories can be 
# merged for sparse combinations.


# Select factor columns and make bar charts for all of them
# Get all factor variables
factor_vars <- data %>% 
  select_if(is.factor) %>% 
  names()

factor_vars_to_use <- c("functionalRoadClass", "functionClass", 
                        "highestSpeedLimit", "lowestSpeedLimit", 
                        "roadCategory", "maxLanes", "minLanes", "countyIds")

# Create bar charts for each factor variable
factor_plots <- map(factor_vars_to_use, ~ {
  ggplot(data, aes_string(x = .x)) +
    geom_bar(fill = "steelblue", alpha = 0.7) +
    labs(title = .x,
         x = .x,
         y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Name the plots
names(factor_plots) <- factor_vars_to_use

# Display all plots
patchwork::wrap_plots(factor_plots)

# Candidates to merge categories:
# functionalRoadClass: (6,) 7, 8, 9
table(data$functionalRoadClass)

# highestSpeedLimit: 20, 30 and 90, 100, 110
table(data$highestSpeedLimit)

# lowestSpeedLimit: 5, 20, 30 and 90, 100, 110
table(data$lowestSpeedLimit)

# maxLanes: 4, 5, 6, 7 
table(data$maxLanes)

# minLanes: 3, 4, 7
table(data$minLanes)


# All possible combinations of factor variables
pairwise_combinations <- combn(factor_vars_to_use, 2)

for(pair in 1:ncol(pairwise_combinations)){
  var1 <- pairwise_combinations[1, pair]
  var2 <- pairwise_combinations[2, pair]
  
  formula <- reformulate(c(var1, var2))
  result <- xtabs(formula, data = data)
  
  cat("\n", var1, "vs", var2, "---------------------------------------", "\n\n")
  print(result)
}

# Looking at interactions
# Basic cross-tabs
xtabs(~ roadCategory + maxLanes, data = data)
xtabs(~ roadCategory + functionClass, data = data)
xtabs(~ roadCategory + functionalRoadClass, data = data)

# Count combinations
data %>% count(roadCategory, functionalRoadClass, sort = TRUE)
data %>% count(maxLanes, minLanes, sort = TRUE)

# Identify sparse combinations
data %>% count(functionalRoadClass, maxLanes) %>% filter(n < 50) %>% arrange(n)


crosstab_heatmaps <- plot_all_pairs(data, pairwise_combinations, plot_type = make_crosstab_heatmap)
patchwork::wrap_plots(crosstab_heatmaps)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Response/covariate interactions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Boxplots for factor vs numeric
ggplot(data, aes(x = roadCategory, y = aadt)) + geom_boxplot()

# Faceted plots
ggplot(data, aes(x = aadt)) + 
  geom_histogram() + 
  facet_wrap(~ roadCategory)#, scales = "free")

# Summary by groups
data %>% group_by(roadCategory) %>% 
  summarise(mean_aadt = mean(aadt, na.rm = TRUE),
            median_aadt = median(aadt, na.rm = TRUE),
            n = n())

aadt_heatmaps <- plot_all_pairs(data, pairwise_combinations, plot_type = make_aadt_heatmap, show_counts = FALSE)
patchwork::wrap_plots(aadt_heatmaps)

# AADT vs traffic link length
ggplot(data, aes(x = length, y = aadt)) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(data, aes(x = log(length), y = aadt)) +
  geom_point() +
  geom_smooth(method = "lm")


logical_vars <- cov_data %>% select_if(is.logical) %>% names()

plot_list <- list()
for(i in 1:length(logical_vars)){
  plot_list[[i]] <- ggplot(data, aes_string(x = logical_vars[i], y = "aadt")) +
    geom_jitter(alpha = 0.5) +
    geom_violin(fill = "goldenrod") 
}

patchwork::wrap_plots(plot_list)


logical_vars %>% map(~ data %>% group_by(!!sym(.x)) %>% summarise(mean_aadt = round(mean(aadt, na.rm=T)), median_aadt = round(median(aadt, na.rm=T)), n = n(), .groups = "drop") %>% print())

# Definitely include:
# isFerryRoute - Huge effect! Mean AADT drops from 3,369 to 158. 
# Ferries have capacity constraints. Decent sample size (468 observations)
# hasOnlyPublicTransportLanes - Very strong effect (3,291 vs 212 mean AADT) 
# Conceptually important for urban traffic modeling

# Probably include:
# isNorwegianScenicRoute - Clear effect (3,323 vs 1,184) and good sample size (963). 
# isRamp - Moderate effect but interesting pattern - ramps have lower mean but 
# higher median AADT than regular roads

# Probably exclude:
# isBlocked - Only FALSE values (no variation to model)
# isInvalid - Very small effect (3,277 vs 1,669) and tiny sample (39 obs).


