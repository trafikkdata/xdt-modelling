# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Basic simulated example ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Only using A1.


mu_v <- MASS::rnegbin(6, mu = 1000, theta = 1)
mu_v
Sigma_v <- diag(rnorm(6)^2)
Sigma_v


A1 <- matrix(c( 1,-1, 0, 0,-1, 1,
               -1, 1, 1,-1, 0, 0,
                0, 0,-1, 1, 1,-1), 
             byrow = TRUE, nrow = 3)
A1

det(A1 %*% t(A1))

Sigma_b <- A1 %*% Sigma_v %*% t(A1)

det(Sigma_b)

b <- rep(0, 3)



# 1. Pseudoinverse
Sigma_b_inv <- MASS::ginv(Sigma_b)
mu_v_given_b_1 <- mu_v + Sigma_v %*% t(A1) %*% Sigma_b_inv %*% (0 - A1 %*% mu_v)
Sigma_v_given_b_1 <- Sigma_v + Sigma_v %*% t(A1) %*% Sigma_b_inv %*% t(Sigma_v %*% t(A1))


# 2. Remove one constraint
A1_red <- A1[-1, ] # Remove first row
Sigma_b_red <- A1_red %*% Sigma_v %*% t(A1_red)
Sigma_b_inv <- solve(Sigma_b_red)
mu_v_given_b_2 <- mu_v + Sigma_v %*% t(A1_red) %*% Sigma_b_inv %*% (0 - A1_red %*% mu_v)
Sigma_v_given_b_2 <- Sigma_v + Sigma_v %*% t(A1_red) %*% Sigma_b_inv %*% t(Sigma_v %*% t(A1_red))


# 3. Regularization
Sigma_b_inv <- solve(Sigma_b + diag(rep(1e-6, 3)))
mu_v_given_b_3 <- mu_v + Sigma_v %*% t(A1) %*% Sigma_b_inv %*% (0 - A1 %*% mu_v)
Sigma_v_given_b_3 <- Sigma_v + Sigma_v %*% t(A1) %*% Sigma_b_inv %*% t(Sigma_v %*% t(A1))


# Compare the three solutions
cbind(mu_v_given_b_1, mu_v_given_b_2, mu_v_given_b_3)

