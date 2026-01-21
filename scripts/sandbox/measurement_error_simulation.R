# Simulering til ITS teknologi presentasjon


set.seed(123)  # for reproducibility

# 1. Generate true traffic volumes (vehicles/hour)
n <- 200
traffic_volume <- runif(n, 100, 2000)  # traffic volume between 100 and 2000 vehicles/hour

# 2. Define the true relationship
# Let's assume noise level (in dB) increases logarithmically with volume,
# plus some random noise around a linear trend in log(volume)
beta0 <- 40      # baseline noise level (dB)
beta1 <- 10      # effect of log(volume)
sigma <- 3       # residual noise standard deviation

# 3. Generate noise levels
noise <- beta0 + beta1 * log10(traffic_volume) + rnorm(n, mean = 0, sd = sigma)

# 4. Combine into a data frame
sim_data <- data.frame(
  traffic_volume = traffic_volume,
  noise = noise
)

# 5. Fit a simple linear model
model <- lm(noise ~ log10(traffic_volume), data = sim_data)
summary(model)

# 6. Plot the data and regression line
plot(noise ~ log10(traffic_volume), data = sim_data,
     pch = 16, col = "steelblue",
     xlab = "log10(Traffic volume [vehicles/hour])",
     ylab = "Noise level [dB]",
     main = "Noise vs. Traffic Volume (no measurement error)")

abline(model, col = "darkred", lwd = 2)


# Adding measurement error
traffic_volume_observed = traffic_volume + rnorm(n, 0, 80)
sim_data$traffic_volume_observed = traffic_volume_observed

model_me = lm(noise ~ log10(traffic_volume_observed), data = sim_data)
summary(model)
summary(model_me)






# --- Demonstration of attenuation from covariate measurement error ---
set.seed(1)

# --- Parameters ---
n <- 200                     # observations
beta0 <- 50                  # intercept (e.g. baseline noise dB)
beta1 <- 0.02                # true effect (dB per vehicle)
sigma_eps <- 3               # residual sd (other noise)
mu_X <- 800                  # mean true traffic volume (vehicles/hour)
sd_X <- 350                  # sd true traffic volume
sd_U <- 200                  # sd of measurement error (sensor noise)

# --- Simulate one dataset ---
X <- rnorm(n, mean = mu_X, sd = sd_X)        # true traffic volume (unobserved)
U <- rnorm(n, mean = 0, sd = sd_U)           # measurement error
W <- X + U                                   # observed (noisy) measurement
Y <- beta0 + beta1 * X + rnorm(n, 0, sigma_eps)  # response: noise (dB)

sim_df <- data.frame(Y = Y, X = X, W = W)

# --- Fit models ---
fit_true <- lm(Y ~ X, data = sim_df)   # regression using true X (not available in practice)
fit_naive <- lm(Y ~ W, data = sim_df)  # naive regression using observed W

summary(fit_true)
summary(fit_naive)

library(ggplot2)

# --- Create ggplot ---
ggplot(sim_df, aes(x = W, y = Y)) +
  geom_point(color = "grey30", alpha = 0.7) +
  geom_abline(
    intercept = coef(fit_naive)[1],
    slope = coef(fit_naive)[2],
    color = "steelblue", size = 1.2
  ) +
  geom_abline(
    intercept = coef(fit_true)[1],
    slope = coef(fit_true)[2],
    color = "firebrick", linetype = "dashed", size = 1.2
  ) +
  labs(
    title = "Attenuation from Measurement Error in Covariate",
    subtitle = paste0(
      "True slope = ", round(coef(fit_true)[2], 3),
      " | Naive slope = ", round(coef(fit_naive)[2], 3)
    ),
    x = "Observed traffic volume",
    y = "Noise (dB)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(values = c("firebrick", "steelblue"))
